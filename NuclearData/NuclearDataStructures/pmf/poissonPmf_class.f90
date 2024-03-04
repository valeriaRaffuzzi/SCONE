module poissonPmf_class

  use numPrecision
  use genericProcedures, only : fatalError
  use RNG_class,         only : RNG

  implicit none
  private

  !!
  !! Procedures to obtain samples and probabilities from the Poisson distribution
  !!
  !! For more information regarding the sampling procedure of the Poisson distribution
  !! using the inverse method, please refer to:
  !! Ross, S., 2023. Generating discrete random variables. In Simulation (6th edition).
  !!
  type, public :: poissonPmf
    private
  contains
    procedure :: sample
    procedure   :: probabilityOf

  end type poissonPmf

contains

  !!
  !! Samples the Poisson distribution using the inverse method.
  !!
  function sample(self, mu, rand) result (n)
    class(poissonPmf), intent(in) :: self
    real(defReal), intent(in)     :: mu
    class(RNG), intent(inout)     :: rand
    integer(shortInt)             :: n
    real(defReal)                 :: r, p, F

    r = rand % get()
    n = 0
    p = exp(-mu)
    F = p

    SPloop:do
        if (r < F) then
            exit SPloop
        else  
            p = mu * p / (n + 1)
            F = F + p
            n = n + 1
        end if
    end do SPloop

  end function sample

  !!
  !! Returns Poisson probability of n.
  !!
  function probabilityOf(self, n, mu) result(prob)
    class(poissonPmf), intent(in) :: self
    integer(shortInt), intent(in) :: n
    real(defReal), intent(in)     :: mu
    real(defReal)                 :: prob, Lprob, i

    i = 0.0
    Lprob = -mu

    ! Perform update in log-space to avoid numerical instability
    do while(i < n)
        Lprob = Lprob + log(mu) - log(i + 1.0)
        i = i + 1.0
    end do

    prob = exp(Lprob)

  end function probabilityOf

end module poissonPmf_class
