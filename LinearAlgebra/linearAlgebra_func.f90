!!
!! This module is a frontend for a linear algebra libraries
!! No libraries should be used by other modules directly. If an additional procedure is needed
!! approperiate interface should be added here.
!!
module linearAlgebra_func

  use numPrecision
  use genericProcedures, only : fatalError, numToChar
  use iso_fortran_env,   only : real64, int32

  implicit none
  private

!!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
!! Public Module interface
!!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
  public :: eig
  public :: solveReal
  public :: solveComplex
  public :: solveAdjointProblem
  public :: identityMatrix
  public :: cram
  public :: kill_linearAlgebra


!!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
!! External LAPACK Procedures interfaces
!!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

  !!
  !! lapack_geev
  !! LAPACK General Matrix Eigenvalue Solver (GE- General Matrix; EV - Eigenvalue)
  !! For full documentation of these procedures refer to:
  !! http://www.netlib.org/lapack/explore-html/index.html
  !! (Just search the webpage for dgeev or sgeev and you will find a very clear doc)
  !!
  !! Interface:
  !! jobvl [in]   -> 'V' or 'N'. With 'V' computes left eigenvectors
  !! jobvr [in]   -> 'V' or 'N'. With 'V' computes right eigenvectors
  !! N     [in]   -> Order of the matrix (NxN). N >=0
  !! A     [inout]-> LDA x N Matrix. Will be changed in the algorithm!
  !! LDA   [in]   -> Leading size of A. LDA >= max(1,N). Must be N
  !! WR    [out]  -> Vector of size N. Real Part of eigenvalues
  !! WI    [out]  -> Vector of size N. Imaginary parts of eigenvalues
  !! VL    [out]  -> LDVL x N Matrix of left eigenvalues. If JOBVL='N' it is not referenced
  !! LDVL  [in]   -> Leading dimension of VL
  !! VR    [out]  -> LDVR x N Matrix of left eigenvalues. If JOBVR='N' it is not referenced
  !! LDVR  [in]   -> Leading dimension of VR
  !! WORK  [out]  -> Work space. If LWORK = -1. On exit WORK(1) is size of optimal workspace
  !! LWORK [in]   -> Size of workspace. LWORK >= 3N for pure eigenvalue calculation LWORK >= 4N
  !!                 if any eigenvectors are also requested. If LWORK = -1 optimal size query.
  !! INFO  [out]  -> Error flag. INFO = 0 for succesfull exectution
  !!
  !! Note that the only difference in the dgeev and sgeev is kind of the real arguments. Rest of
  !! the definition is identical.
  !!
  interface lapack_geev

    !!
    !! Double precision. 64-bit float
    !!
    subroutine dgeev(jobvl, jobvr, N, A, LDA, WR, WI, VL, LDVL, VR, LDVR, WORK, LWORK, INFO)
      use iso_fortran_env, only : real64, int32
      implicit none
      character(1), intent(in)                         :: jobvl
      character(1), intent(in)                         :: jobvr
      integer(int32),intent(in)                        :: N
      integer(int32), intent(in)                       :: LDA
      real(real64),dimension(LDA,N),intent(inout)      :: A
      real(real64),dimension(N), intent(out)           :: WR
      real(real64),dimension(N), intent(out)           :: WI
      integer(int32), intent(in)                       :: LDVL
      real(real64),dimension(LDVL,N), intent(out)      :: VL
      integer(int32),intent(in)                        :: LDVR
      real(real64),dimension(LDVR,N),intent(out)       :: VR
      integer(int32),intent(in)                        :: LWORK
      real(real64),dimension(max(1,LWORK)),intent(out) :: WORK
      integer(int32),intent(out)                       :: INFO
    end subroutine dgeev

    !!
    !! Single precision. 32-bit float
    !!
    subroutine sgeev(jobvl, jobvr, N, A, LDA, WR, WI, VL, LDVL, VR, LDVR, WORK, LWORK, INFO)
      use iso_fortran_env, only : real32, int32
      implicit none
      character(1), intent(in)                         :: jobvl
      character(1), intent(in)                         :: jobvr
      integer(int32),intent(in)                        :: N
      integer(int32), intent(in)                       :: LDA
      real(real32),dimension(LDA,N),intent(inout)      :: A
      real(real32),dimension(N), intent(out)           :: WR
      real(real32),dimension(N), intent(out)           :: WI
      integer(int32), intent(in)                       :: LDVL
      real(real32),dimension(LDVL,N), intent(out)      :: VL
      integer(int32),intent(in)                        :: LDVR
      real(real32),dimension(LDVR,N),intent(out)       :: VR
      integer(int32),intent(in)                        :: LWORK
      real(real32),dimension(max(1,LWORK)),intent(out) :: WORK
      integer(int32),intent(out)                       :: INFO
    end subroutine sgeev
  end interface lapack_geev

  !!
  !! lapack_gesv
  !! LAPACK General Matrix Linear Equation solver
  !! For full documentation of these procedures refer to:
  !! http://www.netlib.org/lapack/explore-html/index.html
  !! (Just search the webpage for dgesv or sgesv and you will find a very clear doc)
  !!
  !! Solves AX=B, where each column of X and VB corresponds to Ax=b.
  !! In outher words can solve for a number of vectors b simultaneously
  !! Uses LU decomposition with Partial Pivoting
  !!
  !! Interface:
  !! N    [in]    -> Number of linear equations to solve
  !! NRHS [in]    -> Number of "Right-hand Sides"
  !! A    [inout] -> On entry coefficient matrix A. On exit L and U factorisation: A=P*L*U
  !! LDA  [in]    -> Leading size of A. LDA >= max(1,N). Must be N
  !! IPIV [out]   -> Integer array of size(N). Pivot indices that define permulation matrix P;
  !!                 row i of the matrix was interchanged with row IPIV(i).
  !! B    [inout] -> Real LDB x NRHS array. On entry matrix B. On exit matrix X.
  !! LDB  [in]    -> Leading dimension of matrix B. Must be N.
  !! INFO [out]   -> Error flag. INFO = 0 for succesfull exectution
  !!
  !! Note that the only difference in the dgesv and sgesv is kind of the real arguments. Rest of
  !! the definition is identical.
  !!
  interface lapack_gesv

    !!
    !! Double precision. 64-bit float
    !!
    subroutine dgesv(N, NRHS, A, LDA, IPIV, B, LDB, INFO)
      use iso_fortran_env, only : real64, int32
      implicit none
      integer(int32), intent(in)                      :: N
      integer(int32), intent(in)                      :: NRHS
      integer(int32), intent(in)                      :: LDA
      real(real64),dimension(LDA,N), intent(inout)    :: A
      integer(int32),dimension(:), intent(out)        :: IPIV
      integer(int32), intent(in)                      :: LDB
      real(real64),dimension(LDB,NRHS), intent(inout) :: B
      integer(int32), intent(out)                     :: INFO
    end subroutine dgesv

    !!
    !! Single precision. 32-bit float
    !!
    subroutine sgesv(N, NRHS, A, LDA, IPIV, B, LDB, INFO)
      use iso_fortran_env, only : real32, int32
      implicit none
      integer(int32), intent(in)                      :: N
      integer(int32), intent(in)                      :: NRHS
      integer(int32), intent(in)                      :: LDA
      real(real32),dimension(LDA,N), intent(inout)    :: A
      integer(int32),dimension(:), intent(out)        :: IPIV
      integer(int32), intent(in)                      :: LDB
      real(real32),dimension(LDB,NRHS), intent(inout) :: B
      integer(int32), intent(out)                     :: INFO
    end subroutine sgesv

    !!
    !! For complex matrices, single precision. 32-bit float
    !!
    subroutine cgesv(N, NRHS, A, LDA, IPIV, B, LDB, INFO)
      use iso_fortran_env, only : int32, real32
      implicit none
      integer(int32), intent(in)                         :: N
      integer(int32), intent(in)                         :: NRHS
      integer(int32), intent(in)                         :: LDA
      complex(real32),dimension(LDA,N), intent(inout)    :: A
      integer(int32),dimension(:), intent(out)           :: IPIV
      integer(int32), intent(in)                         :: LDB
      complex(real32),dimension(LDB,NRHS), intent(inout) :: B
      integer(int32), intent(out)                        :: INFO
    end subroutine cgesv

    !!
    !! For complex matrices, double precision. 64-bit float
    !!
    subroutine zgesv(N, NRHS, A, LDA, IPIV, B, LDB, INFO)
      use iso_fortran_env, only : real64, int32
      implicit none
      integer(int32), intent(in)                         :: N
      integer(int32), intent(in)                         :: NRHS
      integer(int32), intent(in)                         :: LDA
      complex(real64),dimension(LDA,N), intent(inout)    :: A
      integer(int32),dimension(:), intent(out)           :: IPIV
      integer(int32), intent(in)                         :: LDB
      complex(real64),dimension(LDB,NRHS), intent(inout) :: B
      integer(int32), intent(out)                        :: INFO
    end subroutine zgesv

  end interface lapack_gesv

!!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
!! External BLAS Procedures interfaces
!!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

  !!
  !! blas_dot
  !! BLAS Dot product procedure
  !! Note that BLAS interface is included in LAPACK documentation
  !! For full documentation of these procedures refer to:
  !! http://www.netlib.org/lapack/explore-html/index.html
  !! (Just search the webpage for dgesv or sgesv and you will find a very clear doc)
  !!
  !! Interface:
  !!   N [in]   -> Number of elements in vector inputs
  !!   DX [in]  -> First array of size (1 + (N-1) * abs(INCX))
  !!   INCX [in]-> Storage spacing between elements of DX
  !!   DY [in]  -> Second array of size (1 + (N-1) * abs(INCX))
  !!   INCY [in]-> Storage spacing between elements of DY
  !!
  !! Result:
  !!   Value of dot product in approperiate precision
  !!
  !! NOTE: Despite the fact it is not explicitly stated in the docs it is prudent to presume
  !!       that effective size of DX and DY, taking storage increpents under account, must be equal
  !!
  interface blas_dot
    !!
    !! Double precision. 64-bit float
    !!
    function ddot(N, DX, INCX, DY, INCY) result(DOT)
      use iso_fortran_env, only : real64, int32
      implicit none
      integer(int32), intent(in)           :: N
      real(real64),dimension(:),intent(in) :: DX
      integer(int32), intent(in)           :: INCX
      real(real64),dimension(:),intent(in) :: DY
      integer(int32), intent(in)           :: INCY
      real(real64)                         :: DOT
    end function ddot

    !!
    !! Single precision. 32-bit float
    !!
    function sdot(N, DX, INCX, DY, INCY) result(DOT)
      use iso_fortran_env, only : real32, int32
      implicit none
      integer(int32), intent(in)           :: N
      real(real32),dimension(:),intent(in) :: DX
      integer(int32), intent(in)           :: INCX
      real(real32),dimension(:),intent(in) :: DY
      integer(int32), intent(in)           :: INCY
      real(real32)                         :: DOT
    end function sdot
  end interface blas_dot

  !!
  !! blas_gemv
  !! BLAS General Matrix-Vector Multiplication
  !! Note that BLAS interface is included in LAPACK documentation
  !! For full documentation of these procedures refer to:
  !! http://www.netlib.org/lapack/explore-html/index.html
  !! (Just search the webpage for dgesv or sgesv and you will find a very clear doc)
  !!
  !! Computes one of the two:
  !!  1) y = alpha*A*x + beta*y
  !!  2) y = alpha*A**T*x + beta*y
  !!
  !! Interface:
  !!   TRANS [in]    -> character for 'N' computes 1). For 'T' computes 2)
  !!   M     [in]    -> Integer. Number of rows of matrix A. M >= 0
  !!   N     [in]    -> Integer. Number of columns of matrix A. N >= 0
  !!   ALPHA [in]    -> Real. Alpha in 1) and 2)
  !!   A     [in]    -> LDA x N Matrix
  !!   LDA   [in]    -> Leading dimension of matrix A. LDA >= max(1,M)
  !!   X     [in]    -> Vector x of size that matches N or M depending on TRANS
  !!   INCX  [in]    -> Increment for the elements of X
  !!   BETA  [in]    -> Real. Beta in 1) and 2)
  !!   Y     [inout] -> Vector y of size that matches N or M depending on TRANS
  !!   INCY  [in]    -> Increment for the elements of Y
  !!
  interface blas_gemv
    !!
    !! Double precision. 64-bit float
    !!
    subroutine dgemv(TRANS, M, N, ALPHA, A, LDA, X, INCX, BETA, Y, INCY)
      use iso_fortran_env, only : real64, int32
      implicit none
      character(1), intent(in)                    :: TRANS
      integer(int32), intent(in)                  :: M
      integer(int32), intent(in)                  :: N
      real(real64), intent(in)                    :: ALPHA
      integer(int32), intent(in)                  :: LDA
      real(real64), dimension(LDA, N), intent(in) :: A
      real(real64), dimension(:), intent(in)      :: X
      integer(int32), intent(in)                  :: INCX
      real(real64), intent(in)                    :: BETA
      real(real64), dimension(:), intent(inout)   :: Y
      integer(int32), intent(in)                  :: INCY
    end subroutine dgemv

    !!
    !! Singe precision. 32-bit float
    !!
    subroutine sgemv(TRANS, M, N, ALPHA, A, LDA, X, INCX, BETA, Y, INCY)
      use iso_fortran_env, only : real32, int32
      implicit none
      character(1), intent(in)                    :: TRANS
      integer(int32), intent(in)                  :: M
      integer(int32), intent(in)                  :: N
      real(real32), intent(in)                    :: ALPHA
      integer(int32), intent(in)                  :: LDA
      real(real32), dimension(LDA, N), intent(in) :: A
      real(real32), dimension(:), intent(in)      :: X
      integer(int32), intent(in)                  :: INCX
      real(real32), intent(in)                    :: BETA
      real(real32), dimension(:), intent(inout)   :: Y
      integer(int32), intent(in)                  :: INCY
    end subroutine sgemv
  end interface blas_gemv

  !!
  !! blas_axpy
  !! BLAS Constant * Vector + Vector Procedure
  !! Note that BLAS interface is included in LAPACK documentation
  !! For full documentation of these procedures refer to:
  !! http://www.netlib.org/lapack/explore-html/index.html
  !! (Just search the webpage for dgesv or sgesv and you will find a very clear doc)
  !!
  !! Computes:
  !!   y = DA * x + y
  !!
  !! Interface:
  !!   N    [in]    -> Size of vectors
  !!   DA   [in]    -> Real. Scalar DA.
  !!   DX   [in]    -> Vector x. Size > ( 1 + ( N - 1 )*abs( INCX )
  !!   INCX [in]    -> Storage spacing between elements of DX
  !!   DY   [inout] -> Vector y. Size > ( 1 + ( N - 1 )*abs( INCY )
  !!   INCY [in]    -> Storage spacing between elements of DY
  !!
  !! NOTE: It should be assumed that effective size of DX and DY, taking increments under account,
  !!       has to match.
  !!
  interface blas_axpy
    !!
    !! Double precision. 64-bit float
    !!
    subroutine daxpy(N, DA, DX, INCX, DY, INCY)
      use iso_fortran_env, only : real64, int32
      implicit none
      integer(int32), intent(in)                :: N
      real(real64), intent(in)                  :: DA
      real(real64), dimension(:), intent(in)    :: DX
      integer(int32), intent(in)                :: INCX
      real(real64), dimension(:), intent(inout) :: DY
      integer(int32), intent(in)                :: INCY
    end subroutine daxpy

    !!
    !! Single precision. 32-bit float
    !!
    subroutine saxpy(N, SA, SX, INCX, SY, INCY)
      use iso_fortran_env, only : real32, int32
      implicit none
      integer(int32), intent(in)                :: N
      real(real32), intent(in)                  :: SA
      real(real32), dimension(:), intent(in)    :: SX
      integer(int32), intent(in)                :: INCX
      real(real32), dimension(:), intent(inout) :: SY
      integer(int32), intent(in)                :: INCY
    end subroutine saxpy

  end interface blas_axpy

  !!
  !! blas_copy
  !! BLAS Procedure to copy vectors
  !! Note that BLAS interface is included in LAPACK documentation
  !! For full documentation of these procedures refer to:
  !! http://www.netlib.org/lapack/explore-html/index.html
  !! (Just search the webpage for dgesv or sgesv and you will find a very clear doc)
  !!
  !! Copies x into y
  !!
  !! Interface:
  !!   N    [in]  -> number of elements in vectors
  !!   DX   [in]  -> Source vector. Size > ( 1 + ( N - 1 )*abs( INCX ) )
  !!   INCX [in]  -> Storage spacing between elements of DX
  !!   DY   [out] -> Target vector. Size > ( 1 + ( N - 1 )*abs( INCY ) )
  !!   INCY [in]  -> Storage spacing between elements of DY
  !!
  !! NOTE: It should be assumed that effective size of DX and DY, taking increments under account,
  !!       has to match.
  !!
  interface blas_copy
    !!
    !! Double precision. 64-bit real
    !!
    subroutine dcopy(N, DX, INCX, DY, INCY)
      use iso_fortran_env, only : real64, int32
      implicit none
      integer(int32), intent(in)              :: N
      real(real64), dimension(:), intent(in)  :: DX
      integer(int32), intent(in)              :: INCX
      real(real64), dimension(:), intent(out) :: DY
      integer(int32), intent(in)              :: INCY
    end subroutine dcopy

    !!
    !! Single precision. 32-bit real
    !!
    subroutine scopy(N, SX, INCX, SY, INCY)
      use iso_fortran_env, only : real32, int32
      implicit none
      integer(int32), intent(in)              :: N
      real(real32), dimension(:), intent(in)  :: SX
      integer(int32), intent(in)              :: INCX
      real(real32), dimension(:), intent(out) :: SY
      integer(int32), intent(in)              :: INCY
    end subroutine scopy

  end interface blas_copy


!!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
!! Module VARIABLES
!!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

  !! This variable must be private to each OpenMP thread
  real(defReal),dimension(:),allocatable,target    :: workspace
  complex(defReal),dimension(:),allocatable,target :: complexWorkspace

contains

  !!
  !! Solves linear system of equations of the form Ax = b
  !!
  !! A - any NxN square real matrix
  !! b - real vector of RHS of size N
  !! x - resul vector of size N
  !!
  !! Gives fatalError if input is invalid or solution A is singular
  !!
  subroutine solveReal(A, x, b)
    real(defReal),dimension(:,:),intent(in) :: A
    real(defReal),dimension(:),intent(out)  :: x
    real(defReal),dimension(:),intent(in)   :: b
    real(defReal),dimension(:,:),pointer    :: A_t, B_t
    integer(shortInt),dimension(size(x))    :: pivot
    integer(shortInt)                       :: N, mem, info
    character(100),parameter :: Here = 'solveReal ( linearAlgebra_func.f90)'

    ! Verify size of the inputs
    N = size(A,1)
    if (size(b) /= N) then
      call fatalError(Here,'Invalid size of RHS vector b. It is not size N')

    else if (size(x) /=N) then
      call fatalError(Here,'Invallid size of result vector x. It is not size N')

    else if (any(shape(A) /= N)) then
      call fatalError(Here,'Invalid shape of array A. Is not NxN')

    end if

    ! Calculate memory required and ensure that memory is avalible
    mem = N*N + N
    call getMem(mem)

    ! Associate workspace memory with different variables
    ! Use pointers to change ranks
    A_t(1:N,1:N) => workspace(1:N*N)
    B_t(1:N,1:1) => workspace(N*N+1 : N*N + N)

    ! Copy input
    A_t      = A
    B_t(:,1) = b

    ! Perform calculation
    call lapack_gesv(N, 1, A_t, N, pivot, B_t, N, info)

    if( info < 0) then
      call fatalError(Here,'LINPACK procedure failed with error: '//numToChar(info))

    else if(info > 0) then
      call fatalError(Here,'LINPACK procedure failed. Matrix A is singular.')
    end if

    ! Copy the results out
    x = B_t(:,1)

  end subroutine solveReal

  !! Solves linear system of equations of the form Ax = b for complex inputs.
  !! Uses zgesv from lapack_gesv
  !!
  !! A - any NxN square real matrix
  !! b - real vector of RHS of size N
  !! x - result vector of size N
  !!
  !! Gives fatalError if input is invalid or solution A is singular
  !!
  subroutine solveComplex(A, x, b)
    complex(defReal),dimension(:,:),intent(in) :: A
    complex(defReal),dimension(:), intent(out) :: x
    complex(defReal),dimension(:), intent(in)  :: b
    complex(defReal),dimension(:,:),pointer    :: A_t, B_t
    integer(shortInt),dimension(size(x))       :: pivot
    integer(shortInt)                          :: N, mem, info
    character(100),parameter                   :: Here = 'solveComplex (linearAlgebra_func.f90)'

    ! Verify size of the inputs
    N = size(A,1)

    if (size(b) /= N) then
      call fatalError(Here,'Invalid size of RHS vector b. It is not size N')

    else if (size(x) /=N) then
      call fatalError(Here,'Invallid size of result vector x. It is not size N')

    else if (any(shape(A) /= N)) then
      call fatalError(Here,'Invalid shape of array A. Is not NxN')

    end if

    ! Calculate memory required and ensure that memory is avalible
    mem = N*N + N
    call getMemComplex(mem)

    ! Associate workspace memory with different variables
    ! Use pointers to change ranks
    A_t(1:N,1:N) => complexWorkspace(1:N*N)
    B_t(1:N,1:1) => complexWorkspace(N*N+1 : N*N + N)

    ! Copy input
    A_t      = A
    B_t(:,1) = b

    ! Perform calculation
    call lapack_gesv(N, 1, A_t, N, pivot, B_t, N, info)

    if( info < 0) then
      call fatalError(Here,'LINPACK procedure failed with error: '//numToChar(info))

    else if(info > 0) then
      call fatalError(Here,'LINPACK procedure failed. Matrix A is singular.')
    end if

    ! Copy the results out
    x = B_t(:,1)

  end subroutine solveComplex

  !!
  !! Solves a generalised adjoint problem for a GPT response
  !! NOTE: We use <.,.> to denote inner product and ` to denote Adjoint
  !!
  !!  x = M`*x + s
  !!  subject to: <f,x> = 0
  !!
  !! Where M` is adjoint of M and M is singular.
  !! Assumes that <flux,s> = 0 where flux is principal eigenvector of M*flux = 0, which is
  !! a condition for existance of solution x due to Fredholm alternative theorem.
  !! Some for of orthogonalisation f is required for interation to converge.
  !!
  !! A -> Adjoint matrix M`
  !! x -> Solution vector
  !! s -> Generalised adjoint source
  !! f -> Orthogonalisation vector
  !!
  !! Matrix must be NxN and all vectors must be of size N
  !! Gives fatalError if input is invalid or solution fails to convergae
  !!
  !! Maximum number iterations is hardcoded at 1024.
  !! Convergance criterion is <x,x>  smaller then 1.0E-10
  !! Convergance is checkes every 8 iterations.
  !!
  !!
  !! NOTE: For some reasons beyond human comprehension BLAS subroutines do not work in when userd in
  !!       this function. They have been rplaced with Fortran intrinsics, that seem much more
  !!       robust. TODO: Investigate why BLAS fails and hopefully repair it!
  !!
  subroutine solveAdjointProblem(A,x,s,f)
    real(defReal),dimension(:,:), intent(in) :: A
    real(defReal),dimension(:), intent(out)  :: x
    real(defReal),dimension(:), intent(in)   :: s
    real(defReal),dimension(:), intent(in)   :: f
    real(defReal),dimension(:,:),pointer     :: A_t
    real(defReal),dimension(:),pointer       :: x_t, s_t, f_t, w_t
    integer(shortInt)                        :: N, mem, maxIter, i
    real(defReal)                            :: dot
    logical(defBool)                         :: converged
    character(100), parameter :: Here = 'solveAdjointProblem ( linearAlgebra_func.f90)'

    ! Verify inputs
    N = size(A,1)
    if(size(s) /= N) then
      call fatalError(Here,'Invalid size of RHS vector s. It is not size N')

    else if(size(x) /=N) then
      call fatalError(Here,'Invalid size of result vector x. It is not size N')

    else if(size(f) /=N) then
      call fatalError(Here,'Invalid size of orthogonalisation vector f. Is is not size N')

    else if ( any(shape(A) /= N)) then
      call fatalError(Here,'Invalid shape of array A. Is not NxN')

    end if

    ! Calculate required work memory and make sure it is available
    mem = N * N + 4 * N
    call getMem(mem)

    ! Associate workspace memory with diffrent wariables
    A_t(1:N,1:N) => workspace(1        : N*N)
    x_t(1:N)     => workspace(N*N+1    : N*N + N)
    w_t(1:N)     => workspace(N*N+N+1  : N*N + 2*N)
    f_t(1:N)     => workspace(N*N+2*N+1: N*N + 3*N)
    s_t(1:N)     => workspace(N*N+3*N+1: N*N + 4*N)

    ! Copy input
    A_t = A
    w_t = s
    s_t = s
    f_t = f
    x_t = ONE

    ! Perform calculation
    maxIter = 1024
    converged = .false.

    do i = 1,maxIter
      ! Calculate w_t = ONE * A_t *x_t + w_t
      w_t = matmul(A_t, x_t) + w_t

      ! Calculate w_t = w_t - <f_t,w_t> * f_t
      dot = dot_product(f_t, w_t) !blas_dot(N, f_t, 1, w_t, 1)
      w_t = w_t - dot * f_t

      ! Normalise solution
      w_t = w_t / norm2(w_t)

      ! Convergance check
      if(iand(i,8) == 0) then
        dot = dot_product(w_t, x_t)
        if (dot < 1.0E-10_defReal) then
          converged = .true.
          exit
        end if
      end if

      ! Move w_t to x_t and s_t to w_t
      x_t = w_t
      w_t = s_t
    end do

    ! Check for failed iteration
    if(.not.converged) then
      call fatalError(Here,'Calculation failed to convarged in: ' // numToChar(maxIter) //' steps. &
      & Final residual is: ' // numToChar(dot))
    end if

    ! Copy out result
    x = w_t

  end subroutine solveAdjointProblem

  !!
  !! Calculates real part of eigenvalues and right eigenvectors of a square matrix A
  !!
  !! A - any square real matrix
  !! k - vector of real part of the eigenvalue
  !! V - matrix of right eigenvectors
  !!
  !! For +ve determinant matrix eigenvalues are in descending order
  !! For -ve determinant matrix eigenvalues are in ascending order
  !! Eigenvectors are normalised to Euclidean norm of 1.0
  !!
  !! Gives fatalError if input is invalid
  !!
  subroutine eig(k, V, A)
    real(defReal),dimension(:),intent(out)    :: k
    real(defReal),dimension(:,:), intent(out) :: V
    real(defReal),dimension(:,:), intent(in)  :: A
    integer(int32)                            :: N, mem, st, info
    real(defReal),dimension(:,:),pointer      :: A_t, VR_t, VL_t
    real(defReal),dimension(:),pointer        :: Re, Im, Work
    character(100),parameter :: Here = 'eig (linearAlgebra_func.f90)'

    ! Verify size of inputs
    N = size(A,1)
    if( any(shape(V) /= N)) then
      call fatalError(Here,'Invalid shape of eigenvector result array. Is not NxN')

    else if(size(k) /= N) then
      call fatalError(Here,'Invalid size of eigenvalue result vectorr. Is not size N')

    else if ( any(shape(A) /= N)) then
      call fatalError(Here,'Invalid shape of array A. Is not NxN')

    end if

    ! Calculate memory required and ensure that memory is avalible
    ! Mem for: A     V    VL   k    Work
    mem    =  N*N + N*N + N + 2*N + 5*N

    ! Ensure that memory is avalible
    call getMem(mem)

    ! Associate workspace memory with different variables
    ! Use pointers to change ranks
    st = 1
    A_t(1:N,1:N)  => workspace(st : st + N*N-1)

    st = st + N*N
    VR_t(1:N,1:N) => workspace(st : st + N*N-1)

    st = st + N*N
    VL_t(1:1,1:N) => workspace(st : st + N-1)

    st = st + N
    Re   => workspace(st : st + N-1)

    st = st + N
    Im   => workspace(st : st + N-1)

    st = st + N
    Work => workspace(st : size(workspace))

    ! Copy Input on the workmemory
    A_t = A

    ! Perform calculation
    call lapack_geev('N','V', N, A_t, N, Re, Im, VL_t, 1, VR_t, N, Work, size(Work), info)

    if( info /= 0) then
      call fatalError(Here,'LINPACK procedure failed with error: '//numToChar(info))
    end if

    ! Copy the results out
    V = VR_t
    k = Re

  end subroutine eig

  !!
  !! Forms a square identity matrix given the desired order as an input
  !!
  !! Args:
  !!   order [input]     -> Order of matrix to be generated
  !!   matrix [output] -> Identity matrix generated
  !!
  subroutine identityMatrix(order, matrix)
    integer(shortInt), intent(in)              :: order
    real(defReal), dimension(:,:), intent(out) :: matrix
    integer(shortInt)                          :: i

    ! Initialise matrix
    matrix = ZERO

    ! Fill the diagonal terms
    do i = 1, order
      matrix(i,i) = ONE
    end do

  end subroutine identityMatrix

  !!
  !! cram: Chebyshev Rational Approximation Method implementation.
  !! Includes 4th, 16th and 48th order approximations.
  !!
  !! Finds action of a matrix exponential on a vector. Used in depletion calculations
  !! to find the action of the exponentiated burnup matrix on the nuclide densities.
  !!
  !! Args:
  !!   A [in]        -> Input matrix. Applied to depletion, it is the burnup
  !!                    matrix multiplied by the timestep
  !!   x [in]        -> Input array. Applied to depletion, this is the nuclide
  !!                    density vector before a timestep
  !!   order [inout] -> Order of desired CRAM solver. Only orders 4, 16 and 48 are supported
  !!   y [out]       -> Output array. Applied to depletion, this is the nuclide
  !!                    density vector after a timestep
  !!
  !! Errors:
  !!   Fatal errors will be called if the input matrix is not square and if the
  !!   input vector doesn't have the same size
  !!
  subroutine cram(A, x, order, y)
    real(defReal), dimension(:,:), intent(in)   :: A
    real(defReal), dimension(:), intent(in)     :: x
    integer(shortInt), intent(inout)            :: order
    real(defReal), dimension(:), intent(out)    :: y
    complex(defReal), dimension(:), allocatable :: v1, v2
    real(defReal), dimension(:,:), allocatable  :: I
    complex(defReal), dimension(:), allocatable :: alpha, theta
    real(defReal)                               :: alpha_zero
    integer(shortInt)                           :: j, N
    character(100),parameter :: Here = 'cram (linearAlgebra_func.f90)'

    ! Verify if the size of the input matrix and vector are ok
    N = size(A, 1)
    if (N /= size(A, 2)) call fatalError(Here,'Input matrix is not square')
    if (N /= size(x))    call fatalError(Here,'Input matrix does not have compatible &
                                                        & dimensions with the input vector')

    ! Allocate and create identity matrix and intermediate vectors
    allocate(I(N,N), v1(N), v2(N))
    call identityMatrix(N, I)

    ! Initialise a complex vector as the real input vector
    v1 = x

   !! All the other required constants for CRAM are defined
   !! Useful documentation: https://www.tandfonline.com/doi/abs/10.13182/NSE15-26
   select case(order)

    case (4)

     allocate(alpha(order/2), theta(order/2))
     alpha(1) = cmplx(1.237660664064637E+2, -7.922222292656543E+2)
     alpha(2) = cmplx(1.156509240520614E+1, -5.582759641139007E+1)
     theta(1) = cmplx(-3.678453861815398E-1, 3.658121298678667E+0)
     theta(2) = cmplx(1.548393223297122E+0, 1.191822946627426E+0)

     alpha_zero = 8.652240695288853E-5

    case (16)

     allocate(alpha(order/2), theta(order/2))
     alpha(1) = cmplx(5.464930576870210E+3, -3.797983575308356E+4)
     alpha(2) = cmplx(9.045112476907548E+1, -1.115537522430261E+3)
     alpha(3) = cmplx(2.344818070467641E+2, -4.228020157070496E+2)
     alpha(4) = cmplx(9.453304067358312E+1, -2.951294291446048E+2)
     alpha(5) = cmplx(7.283792954673409E+2, -1.205646080220011E+5)
     alpha(6) = cmplx(3.648229059594851E+1, -1.155509621409682E+2)
     alpha(7) = cmplx(2.547321630156819E+1, -2.639500283021502E+1)
     alpha(8) = cmplx(2.394538338734709E+1, -5.650522971778156E+0)
     theta(1) = cmplx(3.509103608414918E+0, 8.436198985884374E+0)
     theta(2) = cmplx(5.948152268951177E+0, 3.587457362018322E+0)
     theta(3) = cmplx(-5.264971343442647E+0, 1.622022147316793E+1)
     theta(4) = cmplx(1.419375897185666E+0, 1.092536348449672E+1)
     theta(5) = cmplx(6.416177699099435E+0, 1.194122393370139E+0)
     theta(6) = cmplx(4.993174737717997E+0, 5.996881713603942E+0)
     theta(7) = cmplx(-1.413928462488886E+0, 1.349772569889275E+1)
     theta(8) = cmplx(-1.084391707869699E+1, 1.927744616718165E+1)

     alpha_zero = 2.124853710495224E-16

   case default

     ! Manually set the order to 48 if something different is provided
     if (order /= 48) then
       order = 48
       print*, "The CRAM order has defaulted to 48. Only acceptable inputs are 4, 16, and 48"
     end if

     allocate(alpha(order/2), theta(order/2))
     alpha(1) = cmplx(6.387380733878774E+2, -6.743912502859256E+2)
     alpha(2) = cmplx(1.909896179065730E+2, -3.973203432721332E+2)
     alpha(3) = cmplx(4.236195226571914E+2, -2.041233768918671E+3)
     alpha(4) = cmplx(4.645770595258726E+2, -1.652917287299683E+3)
     alpha(5) = cmplx(7.765163276752433E+2, -1.783617639907328E+4)
     alpha(6) = cmplx(1.907115136768522E+3, -5.887068595142284E+4)
     alpha(7) = cmplx(2.909892685603256E+3, -9.953255345514560E+3)
     alpha(8) = cmplx(1.944772206620450E+2, -1.427131226068449E+3)
     alpha(9) = cmplx(1.382799786972332E+5, -3.256885197214938E+6)
     alpha(10) = cmplx(5.628442079602433E+3, -2.924284515884309E+4)
     alpha(11) = cmplx(2.151681283794220E+2, -1.121774011188224E+3)
     alpha(12) = cmplx(1.324720240514420E+3, -6.370088443140973E+4)
     alpha(13) = cmplx(1.617548476343347E+4, -1.008798413156542E+6)
     alpha(14) = cmplx(1.112729040439685E+2, -8.837109731680418E+1)
     alpha(15) = cmplx(1.074624783191125E+2, -1.457246116408180E+2)
     alpha(16) = cmplx(8.835727765158191E+1, -6.388286188419360E+1)
     alpha(17) = cmplx(9.354078136054179E+1, -2.195424319460237E+2)
     alpha(18) = cmplx(9.418142823531573E+1, -6.719055740098035E+2)
     alpha(19) = cmplx(1.040012390717851E+2, -1.693747595553868E+2)
     alpha(20) = cmplx(6.861882624343235E+1, -1.177598523430493E+1)
     alpha(21) = cmplx(8.766654491283722E+1, -4.596464999363902E+3)
     alpha(22) = cmplx(1.056007619389650E+2, -1.738294585524067E+3)
     alpha(23) = cmplx(7.738987569039419E+1, -4.311715386228984E+1)
     alpha(24) = cmplx(1.041366366475571E+2, -2.777743732451969E+2)
     theta(1) = cmplx(-4.465731934165702E+1, 6.233225190695437E+1)
     theta(2) = cmplx(-5.284616241568964E+0, 4.057499381311059E+1)
     theta(3) = cmplx(-8.867715667624458E+0, 4.325515754166724E+1)
     theta(4) = cmplx(3.493013124279215E+0, 3.281615453173585E+1)
     theta(5) = cmplx(1.564102508858634E+1, 1.558061616372237E+1)
     theta(6) = cmplx(1.742097597385893E+1, 1.076629305714420E+1)
     theta(7) = cmplx(-2.834466755180654E+1, 5.492841024648724E+1)
     theta(8) = cmplx(1.661569367939544E+1, 1.316994930024688E+1)
     theta(9) = cmplx(8.011836167974721E+0, 2.780232111309410E+1)
     theta(10) = cmplx(-2.056267541998229E+0, 3.794824788914354E+1)
     theta(11) = cmplx(1.449208170441839E+1, 1.799988210051809E+1)
     theta(12) = cmplx(1.853807176907916E+1, 5.974332563100539E+0)
     theta(13) = cmplx(9.932562704505182E+0, 2.532823409972962E+1)
     theta(14) = cmplx(-2.244223871767187E+1, 5.179633600312162E+1)
     theta(15) = cmplx(8.590014121680897E-1, 3.536456194294350E+1)
     theta(16) = cmplx(-1.286192925744479E+1, 4.600304902833652E+1)
     theta(17) = cmplx(1.164596909542055E+1, 2.287153304140217E+1)
     theta(18) = cmplx(1.806076684783089E+1, 8.368200580099821E+0)
     theta(19) = cmplx(5.870672154659249E+0, 3.029700159040121E+1)
     theta(20) = cmplx(-3.542938819659747E+1, 5.834381701800013E+1)
     theta(21) = cmplx(1.901323489060250E+1, 1.194282058271408E+0)
     theta(22) = cmplx(1.885508331552577E+1, 3.583428564427879E+0)
     theta(23) = cmplx(-1.734689708174982E+1, 4.883941101108207E+1)
     theta(24) = cmplx(1.316284237125190E+1, 2.042951874827759E+1)

     alpha_zero = 2.258038182743983D-47 !  Note double precision.

   end select

    !! CRAM solver
    !! OpenMC has good documentation on this:
    !! https://docs.openmc.org/en/stable/methods/depletion.html#matrix-exponential
    do j = 1, order/2
      ! Solves Ax = b
      call solveComplex(A - (I*theta(j)), v2, v1)
      v1 = v1 + TWO * real(alpha(j)*v2)
    end do

    ! Complex vector now holds the result of CRAM
    v1 = alpha_zero * v1
    ! Output real part for physical calculations
    y = real(v1)

  end subroutine cram

  !!
  !! Makes sure that workspace is allocated and has size > N
  !!
  subroutine getMem(N)
    integer(shortInt), intent(in) :: N

    if(allocated(workspace)) then
      ! Check that workspace has sufficient size
      if(size(workspace) < N) then
        deallocate(workspace)
        allocate(workspace(N))
      end if

    else
      allocate(workspace(N))

    end if

  end subroutine getMem

  !!
  !! Makes sure that complexWorkspace is allocated and has size > N
  !! used in solveComplex subroutine
  !!
  subroutine getMemComplex(N)
    integer(shortInt), intent(in) :: N

    if(allocated(complexWorkspace)) then
      ! Check that complex workspace has sufficient size
      if(size(complexWorkspace) < N) then
        deallocate(complexWorkspace)
        allocate(complexWorkspace(N))
      end if

    else
      allocate(complexWorkspace(N))

    end if

  end subroutine getMemComplex

  !!
  !! Returns module to its uninitialised state
  !!
  subroutine kill_linearAlgebra()

    if (allocated(workspace)) deallocate(workspace)
    if (allocated(complexWorkspace)) deallocate(complexWorkspace)

  end subroutine kill_linearAlgebra


end module linearAlgebra_func
