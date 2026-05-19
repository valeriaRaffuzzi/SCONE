module kalbachTable_class

  use numPrecision
  use errors_mod,        only : fatalError
  use genericProcedures, only : searchError, linearSearchFloor, interpolate, &
                                isSorted, numToChar
  use endfConstants

  implicit none
  private

  integer(shortInt),parameter  :: histogram  = tabPdfHistogram, &
                                  linLin     = tabPdfLinLin
  real(defReal),parameter      :: tolerance = 1.0e-6

  !!
  !! Probability table for kalbach-87 formalism
  !! Stores three correlated values x, A, R described by the same PDF
  !!
  type, public :: kalbachTable
    private
    real(defReal),dimension(:),allocatable       :: x
    real(defReal),dimension(:),allocatable       :: pdf
    real(defReal),dimension(:),allocatable       :: cdf
    real(defReal), dimension(:), allocatable     :: marginalCdf
    real(defReal),dimension(:),allocatable       :: R
    real(defReal),dimension(:),allocatable       :: A
    integer(shortInt)                            :: flag  = -2       !Interpolation flag
    logical(defBool)                             :: marginal
  contains
    generic   :: init          => initPdf, initCdf
    procedure :: sample
    procedure :: sampleConditional
    procedure :: bounds
    procedure :: probabilityOf
    procedure :: kill

    procedure :: initPdf
    procedure, private :: initCdf

    procedure, private :: initMarginalCdf

  end type kalbachTable

contains

  !!
  !! Samples x, R and A  given number r in <0;1>
  !! Does not check range of r
  !!
  subroutine sample(self, rand, x, R, A)
    class(kalbachTable), intent(in)  :: self
    real(defReal),intent(in)         :: rand
    real(defReal),intent(out)        :: x
    real(defReal),intent(out)        :: R
    real(defReal),intent(out)        :: A
    integer(shortInt)                :: idx
    real(defReal)                    :: f, delta, ci, pi, ONEmf
    character(100),parameter         :: Here='sample (kalbachTable_class.f90)'

    idx = linearSearchFloor(self % cdf, rand)
    call searchError(idx, Here)

    select case (self % flag)
      case (histogram)
        ci = self % cdf(idx)
        pi = self % pdf(idx)

        x = self % x(idx) + (rand-ci) / pi


        R = self % R(idx)
        A = self % A(idx)

      case (linLin)
        ci = self % cdf(idx)
        pi = self % pdf(idx)

        f = (self % pdf(idx+1) - self % pdf(idx)) / (self % x(idx+1) - self % x(idx))

        if (f == 0) then
          x = self % x(idx) + (rand-ci) / pi
        else
          delta = sqrt( pi*pi + 2*f*(rand-ci))
          x = self % x(idx) + (delta - pi) / f
        end if

        ! Calculate interpolation factor
        f = (x - self % x(idx)) / (self % x(idx+1) - self % x(idx))
        ONEmf = (ONE - f)

        ! Calculate R and A
        R    =  self % R(idx)*ONEmf + self % R(idx+1) * f
        A    =  self % A(idx)*ONEmf + self % A(idx+1) * f

      case default
        call fatalError(Here,'Unknown interpolation flag')

    end select

  end subroutine sample

  !!
  !! Samples x given number r in <0;1> within the marginal cdf
  !! Does not check range of r
  !! Optionaly return index of a sampled bin
  !!
  subroutine sampleConditional(self, rand, x1, x, R, A)
    class(kalbachTable), intent(in)  :: self
    real(defReal),intent(in)         :: rand
    real(defReal), intent(in)        :: x1
    real(defReal),intent(out)        :: x
    real(defReal),intent(out)        :: R
    real(defReal),intent(out)        :: A
    integer(shortInt)                :: idx
    real(defReal)                    :: xLim, gLim, f, rng
    character(100),parameter :: Here='sample (tabularPdf_class.f90)'

    ! Initialise wrong
    x = -ONE

    ! Find maximum allowable value
    xLim = self % x(size(self % x)) - x1

    ! Search index in data grid
    idx = linearSearchFloor(self % x, xLim)
    if (idx < 0) then
      print*, xLim, self % x(size(self % x)), x1
      print*, size(self % x)
      return
    end if
    call searchError(idx, Here)

    ! Find corresponding limit in cdf and normalise random number
    gLim = self % marginalCdf(idx)
    rng  = rand * gLim

    ! Sample bin
    idx = linearSearchFloor(self % marginalCdf, rng)
    call searchError(idx, Here)

    ! Smooth data
    f = (rng - self % marginalCdf(idx)) / (self % marginalCdf(idx + 1) - self % marginalCdf(idx))
    x = self % x(idx + 1) * f + self % x(idx) * (ONE - f)

    ! Return the other variables
    R = self % R(idx)
    A = self % A(idx)

  end subroutine sampleConditional

  !!
  !! Subroutine assigns x(1) to x_min and x(N) to x_max
  !! Returns bounds of the probability distribution
  !!
  subroutine bounds(self,x_min,x_max)
    class(kalbachTable), intent(in) :: self
    real(defReal), intent(out)      :: x_min
    real(defReal), intent(out)      :: x_max

    x_min = self % x(1)
    x_max = self % x(size(self % x))

  end subroutine bounds

  !!
  !! Given x returns prbability and corresponding values of R and A
  !!
  subroutine probabilityOf(self, x, prob, R, A)
    class(kalbachTable), intent(in) :: self
    real(defReal), intent(in)       :: x
    real(defReal), intent(out)      :: prob
    real(defReal), intent(out)      :: R
    real(defReal), intent(out)      :: A
    integer(shortInt)               :: idx
    character(100),parameter        :: Here='init (kalbachTable_class.f90)'

    idx = linearSearchFloor(self % x, x)
    call searchError(idx,Here)

    select case (self % flag)
      case (histogram)
        prob = self % pdf(idx)
        R    = self % R(idx)
        A    = self % A(idx)

      case (linLin)
        prob = interpolate(self % x(idx), self % x(idx+1), self % pdf(idx), self % pdf(idx+1), x)
        R    = interpolate(self % x(idx), self % x(idx+1), self % R(idx), self % R(idx+1), x)
        A    = interpolate(self % x(idx), self % x(idx+1), self % A(idx), self % A(idx+1), x)

      case default
        call fatalError(Here,'Unknown interpolation flag')

    end select

  end subroutine probabilityOf

  !!
  !! Return to uninitialised state
  !!
  elemental subroutine kill(self)
    class(kalbachTable), intent(inout) :: self

    ! Deallocate table
    if(allocated(self % x))   deallocate(self % x)
    if(allocated(self % pdf)) deallocate(self % pdf)
    if(allocated(self % cdf)) deallocate(self % cdf)
    if(allocated(self % R))   deallocate(self % R)
    if(allocated(self % A))   deallocate(self % A)

    ! Set intepolation flag
    self % flag = -2

  end subroutine kill


  !!
  !! Initialise table using PDF only
  !!
  subroutine initPdf(self,x,pdf,R,A,flag)
    class(kalbachTable), intent(inout)     :: self
    real(defReal),dimension(:),intent(in)  :: x
    real(defReal),dimension(:),intent(in)  :: pdf
    real(defReal),dimension(:),intent(in)  :: R
    real(defReal),dimension(:),intent(in)  :: A
    integer(shortInt),intent(in)           :: flag ! Interpolation scheme flag
    integer(shortInt)                      :: i
    character(100),parameter               :: Here='init (kalbachTable_class.f90)'

    ! Check Input
    if( size(x) /= size(pdf)) call fatalError(Here,'PDF and x have diffrent size')
    if( size(x) /= size(R))   call fatalError(Here,'R and x have diffrent size')
    if( size(x) /= size(A))   call fatalError(Here,'A and x have diffrent size')

    if( .not.(isSorted(x)))   call fatalError(Here,'Provided x grid is not sorted not descending')
    if ( any( pdf < 0.0 ))    call fatalError(Here,'Provided PDF contains -ve values')

    ! Initialise Data
    if(allocated(self % x))   deallocate(self % x)
    if(allocated(self % pdf)) deallocate(self % pdf)
    if(allocated(self % cdf)) deallocate(self % cdf)
    if(allocated(self % R))   deallocate(self % R)
    if(allocated(self % A))   deallocate(self % A)

    ! Allocate cdf
    allocate(self % cdf(size(x)))

    self % x   = x
    self % pdf = pdf
    self % cdf = ZERO
    self % R   = R
    self % A   = A

    select case (flag)
      case(histogram)
        self % flag = histogram
        self % cdf(1) = ZERO
        do i=2,size(x)
          self % cdf(i) = pdf(i-1) * (x(i)-x(i-1)) + self % cdf(i-1)
        end do

        if (abs(self % cdf(size(x))-1.0_defReal) > tolerance) then
          call fatalError(Here,'Calculated CDF does not integrate to 1.0 within tolerance:'&
                               //numToChar(self % cdf(size(x))))
        end if

      case(linLin)
        self % flag = linLin
        self % cdf(1) = ZERO
        do i=2,size(x)
          self % cdf(i) = 0.5 * (pdf(i)+pdf(i-1)) * (x(i)- x(i-1)) + self % cdf(i-1)

        end do

        if (abs(self % cdf(size(x))-1.0_defReal) > tolerance) then
          call fatalError(Here,'Calculated CDF does not integrate to 1.0 within tolerance:'&
                               //numToChar(self % cdf(size(x))))

        end if

      case default
        call fatalError(Here, 'Unrecognised interpolation flag')

    end select

  end subroutine initPdf

  !!
  !! Initialise Kalbach table using both pdf and cdf
  !!
  subroutine initCdf(self, x, pdf, cdf, R, A, flag, marginal)
    class(kalbachTable), intent(inout)     :: self
    real(defReal),dimension(:),intent(in)  :: x
    real(defReal),dimension(:),intent(in)  :: pdf
    real(defReal),dimension(:),intent(in)  :: cdf
    real(defReal),dimension(:),intent(in)  :: R
    real(defReal),dimension(:),intent(in)  :: A
    integer(shortInt),intent(in)           :: flag ! Interpolation scheme flag
    logical(defBool), intent(in), optional :: marginal
    character(100),parameter               :: Here='init (tabularPdf_class.f90)'

    ! Check Input
    if( size(x) /= size(pdf)) call fatalError(Here,'PDF and x have diffrent size')
    if( size(x) /= size(cdf)) call fatalError(Here,'CDF and x have diffrent size')
    if( size(x) /= size(R))   call fatalError(Here,'R and x have diffrent size')
    if( size(x) /= size(A))   call fatalError(Here,'A and x have diffrent size')

    if( .not.(isSorted(x)))   call fatalError(Here,'Provided x grid is not sorted not decending')
    if( .not.(isSorted(cdf))) call fatalError(Here,'Provided CDF is not sorted not descending')

    if ( any( pdf < 0.0 ))    call fatalError(Here,'Provided PDF contains -ve values')
    if ( any( cdf < 0.0 ))    call fatalError(Here,'Provided CDF contains -ve values')

    if( abs(cdf(1)) > tolerance ) call fatalError(Here,'Provided CDF does not begin with 0')

    if( abs(cdf(size(cdf))-1.0_defReal) > tolerance) then
      call fatalError(Here,'Provided CDF does not end with 1')
    end if

    ! Initialise Data
    if(allocated(self % x))   deallocate(self % x)
    if(allocated(self % pdf)) deallocate(self % pdf)
    if(allocated(self % cdf)) deallocate(self % cdf)
    if(allocated(self % R))   deallocate(self % R)
    if(allocated(self % A))   deallocate(self % A)

    self % x   = x
    self % pdf = pdf
    self % cdf = cdf
    self % R   = R
    self % A   = A

    select case (flag)
      case(histogram)
        self % flag = histogram

      case(linLin)
        self % flag = linLin

      case default
        call fatalError(Here, 'Unrecognised interpolation flag')

    end select

    if (present(marginal)) then
      self % marginal = marginal
      if (marginal) call self % initMarginalCdf()
    end if

  end subroutine initCDF


  !!
  !! Initialise table using both PDF and CDF
  !!
  subroutine initMarginalCdf(self)
    class(kalbachTable),intent(inout)      :: self
    real(defReal),dimension(:),allocatable :: dF, dF_bar, dG
    integer(shortInt)                      :: N, i, bin
    real(defReal)                          :: xMax, dx, xRef, f, sumG, denom, tail
    real(defReal), parameter :: TOL = 1.0e-12_defReal
    character(100),parameter :: Here = 'initMarginalCdf (tabularPdf_class.f90)'

    ! Save edge values
    N = size(self % x)
    xMax = self % x(N)

    ! Allocate stuff
    allocate(dF(N), dF_bar(N), dG(N), self % marginalCdf(N))

    ! Populate cumulative probabilities
    dF     = ZERO
    dF_bar = ZERO
    do i = 1, N - 1

      dx = self % x(i + 1) - self % x(i)
      dF(i) = self % pdf(i) * dx

      xRef = xMax - self % x(i)
      bin  = linearSearchFloor(self % x, xRef)
      f    = (xRef - self % x(bin)) / (self % x(bin + 1) - self % x(bin))
      dF_bar(i) = (self % pdf(bin + 1) * f + self % pdf(bin) * (ONE - f)) * dx

    end do

    ! Compute the marginal cdf recursively
    sumG  = ZERO
    denom = ZERO
    dG = ZERO

    do i = N, 1, -1

      tail = ONE - sumG

      if (tail < TOL) then
        dG(i) = ZERO
      else
        denom = denom + dF_bar(i) / tail

        if (denom < TOL) then
          dG(i) = ZERO
        else
          dG(i) = dF(i) / denom
        end if

      end if

      sumG = sumG + dG(i)

    end do

    ! Construct the actual cdf
    self % marginalCdf = ZERO
    do i = 2, N
      self % marginalCdf(i) = self % marginalCdf(i-1) + dG(i)
    end do

    ! Normalise
    self % marginalCdf = self % marginalCdf / self % marginalCdf(N)

  end subroutine initMarginalCdf

end module kalbachTable_class
