module particle_class

  use numPrecision
  use universalVariables
  use tallyCodes
  use genericProcedures
  use coord_class,       only : coordList
  use RNG_class,         only : RNG

  implicit none
  private

  !!
  !! Particle types paramethers
  !!
  integer(shortInt), parameter, public :: P_NEUTRON   = 1, &
                                          P_PHOTON    = 2, &
                                          P_PRECURSOR = 3

  !!
  !! Public particle type procedures
  !!
  public :: verifyType
  public :: printType

  !!
  !! Particle compressed for storage
  !!
  !! Public Members:
  !!   wgt        -> Weight of the particle
  !!   r          -> Global Position of the particle [cm]
  !!   dir        -> Direction vector of the particle (normalised to 1.0)
  !!   E          -> Energy of the particle [MeV]
  !!   G          -> Energy Group of the particle
  !!   isMG       -> True if particle uses MG data
  !!   type       -> Physical Type of the particle (NEUTRON, PHOTON etc.)
  !!   time       -> Position in time of the particle [s]
  !!   timeBinIdx -> Discrete position in time of the particle
  !!   lambda     -> Decay constant of precursor [/s]
  !!   matIdx     -> material Index in which particle is present
  !!   cellIdx    -> Cell Index at the lowest level in which particle is present
  !!   uniqueID   -> Unique ID of the cell at the lowest level in which particle is present
  !!   collisionN -> Number of collisions the particle went through
  !!
  !! Interface:
  !!   assignemnt(=)  -> Build particleState from particle
  !!   operator(.eq.) -> Return True if particle are exactly the same
  !!   display        -> Print debug information about the state to the console
  !!
  type, public :: particleState
    real(defReal)              :: wgt  = ZERO       ! Particle weight
    real(defReal),dimension(3) :: r    = ZERO       ! Global position
    real(defReal),dimension(3) :: dir  = ZERO       ! Global direction
    real(defReal)              :: E    = ZERO       ! Energy
    integer(shortInt)          :: G    = 0          ! Energy group
    integer(shortInt)          :: F    = 0          ! Family group if precursor
    logical(defBool)           :: isDead
    logical(defBool)           :: isMG = .false.    ! Is neutron multi-group
    integer(shortInt)          :: fate = no_FATE    !Neutron's fate after being subjected to an operator
    integer(shortInt)          :: type = P_NEUTRON  ! Particle physical type
    real(defReal)              :: time = ZERO       ! Particle time position
    real(defReal)              :: timeBirth = ZERO
    integer(shortInt)          :: timeBinIdx
    real(defReal)              :: lambda            ! Precursor decay constant
    integer(shortInt)          :: matIdx   = -1     ! Material index where particle is
    integer(shortInt)          :: cellIdx  = -1     ! Cell idx at the lowest coord level
    integer(shortInt)          :: uniqueID = -1     ! Unique id at the lowest coord level
    integer(shortInt)          :: collisionN = 0    ! Number of collisions
  contains
    generic    :: assignment(=)  => fromParticle
    generic    :: operator(.eq.) => equal_particleState
    procedure  :: display        => display_particleState
    procedure  :: fromParticle   => particleState_fromParticle
    procedure  :: kill           => kill_particleState

    ! Private procedures
    procedure,private :: equal_particleState
  end type particleState

!  !!
!  !! Archived state of the particle used for tallying transitions, fission matrixes etc.
!  !!
!  type, public,extends(phaseCoord) ::
!  contains
!    generic    :: operator(.eq.) => equal_particleState
!    procedure :: fromParticle    => particleState_fromParticle
!
!    ! Private procedures
!    procedure,private :: equal_particleState
!  end type

  !!
  !! This type represents particle
  !!
  !! In current form it was designed to support neutron and other neutral particles
  !! By extension(inheritance) support for photons or charged particles could be introduced
  !!
  !! Now also represents precursors.
  !!
  !! In addition to representing a particle by physical parameters, it containes additional
  !! data like RNG pointer. This is to enable access to objects associated with a particlular
  !! particle history from anywhere where a particle was passed. This it works kind of like a
  !! global scope for a specific particle. Such setup should enable particle to control how it
  !! is beeing processed during Monte Carlo transport.
  !!
  !! Particle also contains snapshots (states) at privious points in its history. This is used
  !! for tallying. It is necessary to compare current state with the earlier state to score e.g.
  !! collision probabilities.
  !!
  type, public :: particle
    ! Particle phase space data
    type(coordList)            :: coords
    real(defReal)              :: E         ! Particle Energy
    integer(shortInt)          :: G         ! Particle Energy Group
    real(defReal)              :: w         ! Particle Weight
    real(defReal)              :: time      ! Particle time point
    integer(shortInt)          :: timeBinIdx
    real(defReal)              :: timeBirth = ZERO

    ! Precursor particle data
    real(defReal)              :: lambda    ! Precursor decay constant
    integer(shortInt)          :: F         ! Family group if precursor

    ! Particle flags
    real(defReal)              :: w0             ! Particle initial weight (for implicit, variance reduction...)
    logical(defBool)           :: isDead
    logical(defBool)           :: isMG
    real(defReal)              :: timeMax = ZERO ! Maximum neutron time before cut-off
    integer(shortInt)          :: fate = no_FATE ! Neutron's fate after being subjected to an operator
    integer(shortInt)          :: type           ! Particle type
    integer(shortInt)          :: collisionN = 0 ! Index of the number of collisions the particle went through

    ! Particle processing information
    class(RNG), pointer        :: pRNG  => null()  ! Pointer to RNG associated with the particle
    real(defReal)              :: k_eff            ! Value of default keff for implicit source generation
    integer(shortInt)          :: geomIdx          ! Index of the geometry used by the particle
    integer(shortInt)          :: splitCount = 0   ! Counter of number of splits

    ! Archived snapshots of previous states
    type(particleState)        :: preHistory
    type(particleState)        :: preTransition
    type(particleState)        :: prePath
    type(particleState)        :: preCollision

  contains
     ! Build procedures
    generic              :: build => buildCE, buildMG
    generic              :: assignment(=) => particle_fromParticleState

    ! Inquiry about coordinates
    procedure                  :: rLocal
    procedure                  :: rGlobal
    procedure                  :: dirLocal
    procedure                  :: dirGlobal
    procedure                  :: nesting
    procedure                  :: getCellIdx
    procedure                  :: getUniIdx
    procedure                  :: matIdx
    procedure, non_overridable :: getType
    procedure                  :: getSpeed

    ! Inquiry about precursor parameters
    procedure               :: forcedPrecursorDecayWgt
    procedure               :: timedWgt

    ! Operations on coordinates
    procedure            :: moveGlobal
    procedure            :: moveLocal
    procedure            :: rotate
    procedure            :: teleport
    procedure            :: point
    procedure            :: takeAboveGeom
    procedure            :: setMatIdx

    ! Save particle state information
    procedure, non_overridable  :: savePreHistory
    procedure, non_overridable  :: savePreTransition
    procedure, non_overridable  :: savePrePath
    procedure, non_overridable  :: savePreCollision

    ! Debug procedures
    procedure            :: display => display_particle
    procedure            :: typeToChar

    !! Private - Implementation specific procedures
    procedure,private                   :: buildCE
    procedure,private                   :: buildMG
    procedure,non_overridable,private   :: particle_fromParticleState

  end type particle

contains

!!<><><><><><><>><><><><><><><><><><><>><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
!! Particle build and assignment procedures
!!<><><><><><><>><><><><><><><><><><><>><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

  !!
  !! Initialise CE particle
  !! Necessary arguments:
  !!   r   -> Global Position
  !!   dir -> Global direction
  !!   E   -> Energy [MeV]
  !!   w   -> particle weight
  !! Optional arguments:
  !!   t        -> particle time (default = 0.0)
  !!   type     -> particle type (default = P_NEUTRON)
  !!
  pure subroutine buildCE(self, r, dir, E, w, t, type, family, lambda)
    class(particle), intent(inout)          :: self
    real(defReal),dimension(3),intent(in)   :: r
    real(defReal),dimension(3),intent(in)   :: dir
    real(defReal),intent(in)                :: E
    real(defReal),intent(in)                :: w
    real(defReal),optional,intent(in)       :: t
    integer(shortInt),intent(in),optional   :: type
    integer(shortInt),intent(in),optional   :: family
    real(defReal),intent(in), optional      :: lambda

    call self % coords % init(r, dir)
    self % E  = E
    self % w  = w
    self % w0 = w

    self % isDead = .false.
    self % isMG   = .false.

    if (present(t)) then
      self % time = t
      self % timeBirth = t
    else
      self % time = ZERO
      self % timeBirth = ZERO
    end if

    if(present(type)) then
      self % type = type
      if (present(family)) self % F = family
      if (present(lambda)) self % lambda = lambda
    else
      self % type = P_NEUTRON
    end if

  end subroutine buildCE

  !!
  !! Initialise MG particle
  !! Necessary arguments:
  !!   r   -> Global Position
  !!   dir -> Global direction
  !!   G   -> Energy Group
  !!   w   -> particle weight
  !! Optional arguments:
  !!   t   -> particle time (default = 0.0)
  !!   type-> particle type (default = P_NEUTRON)
  !!
  subroutine buildMG(self, r, dir, G, w, t, type, family, lambda)
    class(particle), intent(inout)          :: self
    real(defReal),dimension(3),intent(in)   :: r
    real(defReal),dimension(3),intent(in)   :: dir
    real(defReal),intent(in)                :: w
    integer(shortInt),intent(in)            :: G
    real(defReal),intent(in),optional       :: t
    integer(shortInt),intent(in),optional   :: type
    integer(shortInt),intent(in),optional   :: family
    real(defReal),intent(in), optional      :: lambda

    call self % coords % init(r, dir)
    self % G  = G
    self % w  = w
    self % w0 = w

    self % isDead = .false.
    self % isMG   = .true.

    if (present(t)) then
      self % time = t
      self % timeBirth = t
    else
      self % time = ZERO
      self % timeBirth = ZERO
    end if

    if(present(type)) then
      self % type = type
      if (present(family)) self % F = family
      if (present(lambda)) self % lambda = lambda
    else
      self % type = P_NEUTRON
    end if

  end subroutine buildMG

  !!
  !! Copy phase coordinates into particle
  !!
  pure subroutine particle_fromParticleState(LHS,RHS)
    class(particle), intent(inout)   :: LHS
    type(particleState), intent(in)  :: RHS

    LHS % w                     = RHS % wgt
    LHS % w0                    = RHS % wgt
    call LHS % takeAboveGeom()
    LHS % coords % lvl(1) % r   = RHS % r
    LHS % coords % lvl(1) % dir = RHS % dir
    LHS % E                     = RHS % E
    LHS % G                     = RHS % G
    LHS % F                     = RHS % F
    LHS % lambda                = RHS % lambda
    LHS % isDead                = RHS % isDead
    LHS % isMG                  = RHS % isMG
    LHS % type                  = RHS % type
    LHS % time                  = RHS % time
    LHS % timeBinIdx            = RHS % timeBinIdx
    LHS % timeBirth             = RHS % timeBirth
    LHS % lambda                = RHS % lambda
    LHS % fate                  = RHS % fate
    LHS % collisionN            = RHS % collisionN
    LHS % splitCount            = 0 ! Reinitialise counter for number of splits

  end subroutine particle_fromParticleState

!!<><><><><><><>><><><><><><><><><><><>><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
!! Particle coordinates inquiry procedures
!!<><><><><><><>><><><><><><><><><><><>><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

  !!
  !! Return the position either at the deepest nested level or a specified level
  !!
  function rLocal(self,n)result(r)
    class(particle), intent(in)             :: self
    integer(shortInt), intent(in), optional :: n
    real(defReal), dimension(3)             :: r
    integer(shortInt)                       :: n_loc

    if(present(n)) then
      n_loc = n
    else
      n_loc = self % coords % nesting
    end if

    r = self % coords % lvl(n_loc) % r

  end function rLocal

  !!
  !! Return the position at the highest level
  !!
  pure function rGlobal(self)result(r)
    class(particle), intent(in) :: self
    real(defReal), dimension(3) :: r

    r = self % coords % lvl(1) % r

  end function rGlobal

  !!
  !! Return the direction either at the deepest nested level or at a specified level
  !!
  function dirLocal(self,n)result(dir)
    class(particle), intent(in) :: self
    integer(shortInt), optional :: n
    real(defReal), dimension(3) :: dir
    integer(shortInt)           :: n_loc

    if(present(n)) then
      n_loc = n
    else
      n_loc = self % coords % nesting
    end if

    dir = self % coords % lvl(n_loc) % dir

  end function dirLocal

  !!
  !! Return the direction at the highest nesting level
  !!
  function dirGlobal(self)result(dir)
    class(particle), intent(in) :: self
    real(defReal), dimension(3) :: dir

    dir = self % coords % lvl(1) % dir

  end function dirGlobal

  !!
  !! Return the lowest nesting level of the particle
  !!
  function nesting(self) result(n)
    class(particle), intent(in) :: self
    integer(shortInt)           :: n

    n = self % coords % nesting

  end function nesting

  !!
  !! Return cell index at a given nesting level n
  !!  If no n is given return for lowest nesting level
  !!
  pure function getCellIdx(self,n) result(idx)
    class(particle), intent(in)             :: self
    integer(shortInt),optional, intent(in)  :: n
    integer(shortInt)                       :: idx
    integer(shortInt)                       :: n_loc

    if(present(n)) then
      n_loc = n
    else
      n_loc = self % coords % nesting
    end if

    idx = self % coords % lvl(n_loc) % cellIdx

  end function getCellIdx

  !!
  !! Return universe index at a given nesting level n
  !!
  pure function getUniIdx(self,n) result(idx)
    class(particle), intent(in)             :: self
    integer(shortInt),optional, intent(in)  :: n
    integer(shortInt)                       :: idx
    integer(shortInt)                       :: n_loc

    if(present(n)) then
      n_loc = n
    else
      n_loc = self % coords % nesting
    end if

    idx = self % coords % lvl(n_loc) % uniIdx

  end function getUniIdx

  !!
  !! Return current material index
  !!
  pure function matIdx(self) result(Idx)
    class(particle), intent(in) :: self
    integer(shortInt)           :: idx

    idx = self % coords % matIdx

  end function matIdx

  !!
  !! Return one of the particle Tpes defined in universal variables
  !!
  !! Args:
  !!   None
  !!
  !! Result:
  !!   P_NEUTRON_CE, P_NEUTRON_MG
  !!
  !! Errors:
  !!   None
  !!
  pure function getType(self) result(type)
    class(particle), intent(in) :: self
    integer(shortInt)           :: type

    if (self % isMG) then
      type = P_NEUTRON_MG
    else
      type = P_NEUTRON_CE
    end if

  end function getType

  !!
  !! Return speed of the particle
  !!
  !! Args:
  !!   None
  !!
  !! Result:
  !!   Speed of light for photon, speed from E for neutron
  !!   Made up number for MG (corresponds to 0.025 eV)
  !!
  !! Errors:
  !!
  !!
  pure function getSpeed(self) result(speed)
    class(particle), intent(in) :: self
    real(defReal)               :: speed

    if (self % type == P_PHOTON) then
      speed = lightSpeed
    else
      if (self % isMG) then
        speed = 0.7294888_defReal
      else
        speed = sqrt(self % E * TWO/neutronMass) * lightSpeed
      end if
    end if

  end function getSpeed

  !!
  !! Return neutron weight for forced decay at time t
  !!
  !! Args:
  !!   t       -> time of forced decay
  !!   delta_T -> time step in which forced decay occurs
  !!
  !! Result:
  !!   Neutron weight for decay at time t
  !!   Returns ZERO if particle is not a precursor
  !!
  !! Errors:
  !!
  !!
  function forcedPrecursorDecayWgt(self, t, delta_T) result(w_d)
    class(particle), intent(in) :: self
    real(defReal), intent(in)   :: t
    real(defReal), intent(in)   :: delta_T
    real(defReal)               :: w_d

    w_d = self % w * delta_T * self % lambda * exp(-self % lambda * (t - self % time))

  end function forcedPrecursorDecayWgt

  !!
  !! Return current timed weight of particle
  !!
  !! Args:
  !!   t      -> current time [s]
  !!
  !! Result:
  !!   Current timed weight at time t
  !!   Returns ZERO if particle is not a precursor
  !!
  !! Errors:
  !!
  function timedWgt(self, t) result(w_timed)
    class(particle), intent(in) :: self
    real(defReal), intent(in)   :: t
    real(defReal)               :: w_timed

    w_timed = self % w * exp(-self % lambda * (t - self % time))
  end function timedWgt

!!<><><><><><><>><><><><><><><><><><><>><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
!! Particle operations on coordinates procedures
!!<><><><><><><>><><><><><><><><><><><>><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

  !!
  !! Move the particle above the geometry
  !! NOTE: regionID & matIdx will be reset!!!
  !!
  subroutine moveGlobal(self,distance)
    class(particle), intent(inout) :: self
    real(defReal), intent(in)      :: distance

    call self % coords % moveGlobal(distance)

  end subroutine moveGlobal

  !!
  !! Move particle in local co-ordinates down to nesting level n
  !!
  subroutine moveLocal(self,distance,n)
    class(particle), intent(inout) :: self
    real(defReal), intent(in)      :: distance
    integer(shortInt), intent(in)  :: n

    call self % coords % moveLocal(distance,n)

  end subroutine moveLocal

  !!
  !! Rotate particle
  !!  mu  -> cosine of deflection from current direction
  !!  phi -> azimuthal angle of rotation
  !!
  subroutine rotate(self,mu,phi)
    class(particle), intent(inout) :: self
    real(defReal), intent(in)      :: mu
    real(defReal), intent(in)      :: phi

    call self % coords % rotate(mu,phi)

  end subroutine rotate

  !!
  !! Place particle at an arbitrary point in above the geometry
  !!
  subroutine teleport(self, r)
    class(particle), intent(inout)          :: self
    real(defReal), dimension(3), intent(in) :: r

    call self % coords % assignPosition(r)

  end subroutine teleport

  !!
  !! Point particle in direction dir in highest nesting level
  !! Propagates new direction to lower levels
  !!
  subroutine point(self, dir)
    class(particle), intent(inout)          :: self
    real(defReal), dimension(3), intent(in) :: dir

    call self % coords % assignDirection(dir)

  end subroutine point

  !!
  !! Resets the particle's nesting level
  !!
  pure subroutine takeAboveGeom(self)
    class(particle), intent(inout) :: self

    call self % coords % takeAboveGeom()

  end subroutine takeAboveGeom

  !!
  !! Set Material index for testing purposes
  !!
  pure subroutine setMatIdx(self,matIdx)
    class(particle), intent(inout) :: self
    integer(shortInt), intent(in)  :: matIdx

    self % coords % matIdx = matIdx

  end subroutine setMatIdx

!!<><><><><><><>><><><><><><><><><><><>><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
!! Particle save state procedures
!!<><><><><><><>><><><><><><><><><><><>><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

  !!
  !! Save state of the particle at the beginning of history
  !!
  subroutine savePreHistory(self)
    class(particle), intent(inout) :: self

    self % preHistory = self

  end subroutine savePreHistory

  !!
  !! Save state of the particle at the beginning of history
  !!
  subroutine savePreTransition(self)
    class(particle), intent(inout) :: self

    self % preTransition = self

  end subroutine savePreTransition

  !!
  !! Save state of the particle at the beginning of history
  !!
  subroutine savePrePath(self)
    class(particle), intent(inout) :: self

    self % prePath = self

  end subroutine savePrePath

  !!
  !! Save state of the particle at the beginning of history
  !!
  subroutine savePreCollision(self)
    class(particle), intent(inout) :: self

    self % preCollision = self

  end subroutine savePreCollision

!!<><><><><><><>><><><><><><><><><><><>><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
!! Particle debug procedures
!!<><><><><><><>><><><><><><><><><><><>><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

  !!
  !! Display state of a particle
  !!
  subroutine display_particle(self)
    class(particle), intent(in) :: self
    type(particleState)         :: state

    state = self
    call state % display()
    print *, 'Material: ', self % coords % matIdx

  end subroutine display_particle

  !!
  !! Return character that describes the type of particle
  !!
  function typeToChar(self) result(c)
    class(particle), intent(in) :: self
    character(:), allocatable   :: c
    character(2)                :: eType

    if( self % isMG) then
      eType = 'MG'
    else
      eType = 'CE'
    end if

    c = eType // ' ' // trim(printType( self % type))

  end function typeToChar


!!<><><><><><><>><><><><><><><><><><><>><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
!! Particle state and phaseCoord procedures
!!<><><><><><><>><><><><><><><><><><><>><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

  !!
  !! Copy particle into phase coordinates
  !!
  subroutine particleState_fromParticle(LHS,RHS)
    class(particleState), intent(out)  :: LHS
    class(particle), intent(in)        :: RHS

    LHS % wgt        = RHS % w
    LHS % r          = RHS % rGlobal()
    LHS % dir        = RHS % dirGlobal()
    LHS % E          = RHS % E
    LHS % G          = RHS % G
    LHS % F          = RHS % F
    LHS % lambda     = RHS % lambda
    LHS % isDead     = RHS % isDead
    LHS % isMG       = RHS % isMG
    LHS % type       = RHS % type
    LHS % time       = RHS % time
    LHS % timeBinIdx = RHS % timeBinIdx
    LHS % timeBirth  = RHS % timeBirth
    LHS % fate       = RHS % fate

    ! Save all indexes
    LHS % matIdx   = RHS % coords % matIdx
    LHS % uniqueID = RHS % coords % uniqueId
    LHS % cellIdx  = RHS % coords % cell()
    LHS % collisionN = RHS % collisionN

  end subroutine particleState_fromParticle

  !!
  !! Define equal operation on phase coordinates
  !!  Phase coords are equal if all their components are the same
  !!
  function equal_particleState(LHS,RHS) result(isEqual)
    class(particleState), intent(in) :: LHS
    class(particleState), intent(in) :: RHS
    logical(defBool)              :: isEqual

    isEqual = .true.
    isEqual = isEqual .and. LHS % wgt == RHS % wgt
    isEqual = isEqual .and. all(LHS % r   == RHS % r)
    isEqual = isEqual .and. all(LHS % dir == RHS % dir)
    isEqual = isEqual .and. LHS % time == RHS % time
    isEqual = isEqual .and. LHS % timeBinIdx == RHS % timeBinIdx
    isEqual = isEqual .and. LHS % timeBirth == RHS % timeBirth
    isEqual = isEqual .and. LHS % isDead .eqv. RHS % isDead
    isEqual = isEqual .and. LHS % isMG .eqv. RHS % isMG
    isEqual = isEqual .and. LHS % type == RHS % type
    isEqual = isEqual .and. LHS % matIdx   == RHS % matIdx
    isEqual = isEqual .and. LHS % cellIdx  == RHS % cellIdx
    isEqual = isEqual .and. LHS % uniqueID == RHS % uniqueID
    isEqual = isEqual .and. LHS % fate == RHS % fate
    isEqual = isEqual .and. LHS % collisionN == RHS % collisionN

    if( LHS % isMG ) then
      isEqual = isEqual .and. LHS % G == RHS % G
    else
      isEqual = isEqual .and. LHS % E == RHS % E
    end if

    if(LHS % type == P_PRECURSOR .and. RHS % type == P_PRECURSOR) then
      isEqual = isEqual .and. LHS % F == RHS % F
      isEqual = isEqual .and. LHS % lambda == RHS % lambda
    end if

  end function equal_particleState

!  !!
!  !! Copy particle state into archive object
!  !!
!  subroutine particleState_fromParticle(LHS,RHS)
!    class(particleState), intent(out) :: LHS
!    class(particle), intent(in)       :: RHS
!
!    ! Call superclass procedure
!    call phaseCoord_fromParticle(LHS,RHS)
!
!  end subroutine particleState_fromParticle

!  !!
!  !! Extend equality definition to particle state
!  !!
!  function equal_particleState(LHS,RHS) result(isEqual)
!    class(particleState), intent(in) :: LHS
!    type(particleState), intent(in)  :: RHS
!    logical(defBool)                 :: isEqual
!
!    ! Call superclass procedure
!    isEqual = equal_phaseCoord(LHS,RHS)
!
!  end function equal_particleState

  !!
  !! Prints state of the phaseCoord
  !!
  subroutine display_particleState(self)
    class(particleState), intent(in) :: self

    print*, 'Position: ', self % r
    print*, 'Direction: ', self % dir
    print*, 'Energy: ', self % E
    print*, 'Group: ', self % G
    print*, 'isMG: ', self % isMG
    print*, 'Weight: ', self % wgt
    print*, 'Time: ', self % time

  end subroutine display_particleState

  !!
  !! Return to uninitialised state
  !!
  elemental subroutine kill_particleState(self)
    class(particleState), intent(inout) :: self

    self % wgt  = ZERO
    self % r    = ZERO
    self % dir  = ZERO
    self % E    = ZERO
    self % G    = 0
    self % F    = 0
    self % lambda = 0
    self % isMG = .false.
    self % type = P_NEUTRON
    self % time = ZERO
    self % matIdx   = -1
    self % cellIdx  = -1
    self % uniqueID = -1
    self % collisionN = 0

  end subroutine kill_particleState


!!<><><><><><><>><><><><><><><><><><><>><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
!! Misc Procedures
!!<><><><><><><>><><><><><><><><><><><>><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
  !!
  !! Returns .true. if the integer is valid particle type code
  !! Returns .false. otherwise
  !!
  elemental function verifyType(type) result(isValid)
    integer(shortInt), intent(in) :: type
    logical(defBool)              :: isValid

    isValid = .false.

    ! Check against particles types
    isValid = isValid .or. type == P_NEUTRON
    isValid = isValid .or. type == P_PHOTON
    isValid = isValid .or. type == P_PRECURSOR
  end function verifyType

  !!
  !! Returns character with a description of particle type
  !!
  pure function printType(type) result(name)
    integer(shortInt), intent(in) :: type
    character(:),allocatable      :: name

    select case(type)
      case(P_NEUTRON)
        name = 'Neutron'

      case(P_PHOTON)
        name = 'Photon'

      case(P_PRECURSOR)
        name = 'Precursor'

      case default
        name = 'INVALID PARTICLE TYPE'

    end select
  end function printType

end module particle_class
