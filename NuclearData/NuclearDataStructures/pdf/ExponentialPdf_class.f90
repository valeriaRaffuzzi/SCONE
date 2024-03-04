module exponentialPdf_class

  use numPrecision
  use genericProcedures, only : fatalError
  use RNG_class,         only : RNG

  implicit none
  private

  !!
  !! Procedures to obtain samples and probabilities from the Exponential distribution
  !!
  type, public :: exponentialPdf
    private
  contains
    procedure :: sample
    procedure   :: probabilityOf

  end type exponentialPdf

contains

  !!
  !! Samples the Exponential distribution using the inverse method.
  !!
  function sample(self, lambda, rand) result (x)
    class(exponentialPdf), intent(in) :: self
    real(defReal), intent(in)         :: lambda
    class(RNG), intent(inout)         :: rand
    real(defReal)                     :: x, r

    r = rand % get()
    x = (-ONE / lambda) * log(r)

  end function sample

  !!
  !! Returns Exponential probability of x.
  !!
  function probabilityOf(self, x, lambda) result(prob)
    class(exponentialPdf), intent(in) :: self
    integer(shortInt), intent(in)     :: x
    real(defReal), intent(in)         :: lambda
    real(defReal)                     :: prob

    if (x >= 0.0) then
      prob = ONE - exp(-lambda * x)
    else
      prob = 0.0 
    end if

  end function probabilityOf

end module exponentialPdf_class
