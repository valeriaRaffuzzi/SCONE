module linearAlgebra_test
  use numPrecision
  use linearAlgebra_func, only : kill_linearAlgebra, eig, solveReal, solveComplex, &
                                solveAdjointProblem, cram
  use pFUnit_mod

  implicit none

contains

  !!
  !! Test eigenvalue calculation
  !!
@Test
  subroutine testEigenvalue()
    real(defReal), dimension(3,3) :: mat33, v33
    real(defReal), dimension(4,4) :: mat44, v44
    real(defReal), dimension(3)   :: k3
    real(defReal), dimension(4)   :: k4
    real(defReal),parameter :: TOL = 1.0E-6_defReal

    ! Calculate eigenvectors of 3x3 Matrix
    mat33(1,:) = [ 2.0_defReal,  4.0_defReal, -2.0_defReal]
    mat33(2,:) = [ 2.0_defReal, -1.0_defReal, -2.0_defReal]
    mat33(3,:) = [-4.0_defReal, -2.0_defReal, -5.0_defReal]

    call eig(k3, v33, mat33)

    ! Check eigenvalues
    @assertEqual( 5.0_defReal, k3(1), TOL)
    @assertEqual(-3.0_defReal, k3(2), TOL)
    @assertEqual(-6.0_defReal, k3(3), TOL)

    ! Check eigenvectors
    @assertEqual([-0.81649658_defReal,-0.40824829_defReal, 0.40824829_defReal], v33(:,1), TOL)
    @assertEqual([-0.53452248_defReal, 0.80178372_defReal, 0.26726124_defReal], v33(:,2), TOL)
    @assertEqual([ 0.05842062_defReal, 0.35052374_defReal, 0.93472998_defReal], v33(:,3), TOL)

    ! Calculate eigenvectors of 4x4 Matrix
    mat44(1,:) = [16.0_defReal,  2.0_defReal,  3.0_defReal, 13.0_defReal]
    mat44(2,:) = [ 5.0_defReal, 11.0_defReal, 10.0_defReal,  8.0_defReal]
    mat44(3,:) = [ 9.0_defReal,  7.0_defReal,  6.0_defReal, 12.0_defReal]
    mat44(4,:) = [ 4.0_defReal, 14.0_defReal, 15.0_defReal,  3.0_defReal]

    call eig(k4, v44, mat44)

    ! Check eigenvalues
    @assertEqual(34.51699899_defReal, k4(1), TOL)
    @assertEqual( 8.79398337_defReal, k4(2), TOL)
    @assertEqual(-7.43156094_defReal, k4(3), TOL)
    @assertEqual( 0.12057858_defReal, k4(4), TOL)

    ! Check eigenvectors
    @assertEqual([-0.49616808_defReal, -0.49173210_defReal, -0.49480677_defReal, -0.51689675_defReal], v44(:,1), TOL)
    @assertEqual([-0.83035254_defReal,  0.39013716_defReal,  0.01092828_defReal,  0.39772885_defReal], v44(:,2), TOL)
    @assertEqual([ 0.38548337_defReal, -0.00693389_defReal,  0.45994233_defReal, -0.79987971_defReal], v44(:,3), TOL)
    @assertEqual([-0.25724392_defReal, -0.66999974_defReal,  0.64227835_defReal,  0.26908070_defReal], v44(:,4), TOL)

    ! Kill workspace
    call kill_linearAlgebra()

  end subroutine testEigenvalue

  !!
  !! Test linear solve of real system Ax = b
  !!
@Test
  subroutine testLinSolve()
    real(defReal),dimension(3,3) :: A
    real(defReal), dimension(3)  :: b
    real(defReal), dimension(3)  :: x
    real(defReal),parameter :: TOL = 1.0E-6_defReal

    ! Set linear system
    A(1,:) = [8.0_defReal, 1.0_defReal, 6.0_defReal]
    A(2,:) = [3.0_defReal, 5.0_defReal, 7.0_defReal]
    A(3,:) = [4.0_defReal, 9.0_defReal, 2.0_defReal]

    b = [ 3.0_defReal, 2.0_defReal, 1.0_defReal]

    ! Solve equation
    call solveReal(A,x,b)

    ! Verify results
    @assertEqual([0.21666667_defReal, -0.03333333_defReal, 0.21666667_defReal],x,TOL)

  end subroutine testLinSolve

  !!
  !! Test linear solve of complex system Ax = b
  !!
@Test
  subroutine testLinSolveComplex()
    complex(defReal),dimension(3,3) :: A
    complex(defReal), dimension(3)  :: b
    complex(defReal), dimension(3)  :: x
    real(defReal),parameter :: TOL = 1.0E-3_defReal

    ! Set linear system
    A(1,:) = [cmplx(8.0_defReal,0), cmplx(1.0_defReal,0), cmplx(6.0_defReal,0)]
    A(2,:) = [cmplx(3.0_defReal,0), cmplx(5.0_defReal,5), cmplx(7.0_defReal,0)]
    A(3,:) = [cmplx(4.0_defReal,0), cmplx(9.0_defReal,0), cmplx(2.0_defReal,0)]

    b = [ cmplx(3.0_defReal,0), cmplx(2.0_defReal,0), cmplx(1.0_defReal,0)]

    ! Solve equation
    call solveComplex(A,x,b)

    ! Verify results
    @assertEqual([cmplx(0.21402,-0.02378), cmplx(-0.032927,0.003659), cmplx(0.22012,0.03110)],x,TOL)

  end subroutine testLinSolveComplex

  !!
  !! Test solution of a generalised adjoint system
  !!
@Test
  subroutine testAdjointSolve()
    real(defReal),dimension(2,2) :: A
    real(defReal), dimension(2)  :: s
    real(defReal), dimension(2)  :: x
    real(defReal), dimension(2)  :: f
    real(defReal),parameter :: TOL = 1.0E-6_defReal

    ! Set linear system
    A(1,:) = [0.9_defReal,  0.1_defReal]
    A(2,:) = [0.1_defReal, 0.9_defReal]

    s = [ONE, -ONE] / sqrt(TWO)
    f = [ONE, ONE] / sqrt(TWO)

    call solveAdjointProblem(A, x, s, f)

    ! Verify solution
    @assertEqual([ONE/sqrt(TWO), -ONE/sqrt(TWO)], x, TOL)

  end subroutine testAdjointSolve

  !!
  !! Test CRAM solver with different orders
  !!
@Test
  subroutine testCram()
    real(defReal),dimension(2,2) :: A
    real(defReal), dimension(2)  :: s
    real(defReal), dimension(2)  :: x
    real(defReal)                :: dt
    integer(shortInt)            :: order

    ! Define time-step and order
    dt = 0.1_defReal

    ! Define input matrix
    ! Taken from OpenMC 48th order CRAM test (~/openMC/tests/test_deplete_cram.py)
    A(1,1) = -1.0_defReal
    A(1,2) = 0.0_defReal
    A(2,1) = -2.0_defReal
    A(2,2) = -3.0_defReal

    ! Define input vector
    s(1:1) = ONE
    s(1:2) = ONE

    ! Test order 4
    order = 4
    call cram(A*dt, s, order, x)
    @assertEqual([0.904837418035960_defReal, 0.576799023327476_defReal], x, 1.0E-03_defReal)

    ! Test order 16
    order = 16
    call cram(A*dt, s, order, x)
    @assertEqual([0.904837418035960_defReal, 0.576799023327476_defReal], x, 1.0E-06_defReal)

    ! Test order 48
    order = 48
    call cram(A*dt, s, order, x)
    @assertEqual([0.904837418035960_defReal, 0.576799023327476_defReal], x, 1.0E-06_defReal)

    ! Test different order. This will default to order 48
    order = 400
    call cram(A*dt, s, order, x)
    @assertEqual([0.904837418035960_defReal, 0.576799023327476_defReal], x, 1.0E-06_defReal)

  end subroutine testCram

end module linearAlgebra_test
