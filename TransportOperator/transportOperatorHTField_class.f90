!!
!! Transport operator for hybrid tracking
!!
module transportOperatorHTField_class
  use numPrecision
  use universalVariables

  use errors_mod,                  only : fatalError
  use genericProcedures,           only : numToChar
  use particle_class,              only : particle
  use particleDungeon_class,       only : particleDungeon
  use dictionary_class,            only : dictionary

  ! Tally interface
  use tallyCodes
  use tallyAdmin_class,            only : tallyAdmin

  ! Superclass
  use transportOperator_inter,     only : transportOperator, init_super => init

  ! Geometry interfaces
  use geometry_inter,              only : geometry, distCache
  use displacementField_inter,     only : displacementField, displacementField_CptrCast
  use geometryReg_mod,             only : gr_fieldIdx => fieldIdx, gr_fieldPtr => fieldPtr

  ! Nuclear data interfaces
  use nuclearDataReg_mod,          only : ndReg_get => get
  use nuclearDatabase_inter,       only : nuclearDatabase

  implicit none
  private

  !!
  !! Transport operator that moves a particle with hybrid tracking
  !!
  type, public, extends(transportOperator) :: transportOperatorHTField
    real(defReal)    :: cutoff   ! Cutoff threshold between ST and DT
    logical(defBool) :: cache = .true.
    logical(defBool) :: collided
    class(displacementField), pointer :: displacement

  contains

    procedure :: transit => tracking_selection
    procedure, private :: deltaTracking
    procedure, private :: surfaceTracking

    ! Override procedure
    procedure :: init
    procedure :: step

  end type transportOperatorHTField

contains

  subroutine tracking_selection(self, p, tally, thisCycle, nextCycle)
    class(transportOperatorHTField), intent(inout)              :: self
    class(particle), intent(inout)                         :: p
    type(tallyAdmin), intent(inout)                        :: tally
    class(particleDungeon), intent(inout)                  :: thisCycle
    class(particleDungeon), intent(inout)                  :: nextCycle
    real(defReal)                                          :: majorant_inv, sigmaT, ratio
    character(100), parameter :: Here = 'hybridTracking (transportOperatorHTField_class.f90)'

    ! Get majornat XS inverse: 1/Sigma_majorant
    majorant_inv = ONE / self % xsData % getTrackingXS(p, p % matIdx(), MAJORANT_XS)

    ! Obtain the local cross-section. Always choose ST in void
    if (p % matIdx() == VOID_MAT) then
      sigmaT = ZERO
    else
      ! Get local conditions
      call self % localConditions(p)

      sigmaT = self % xsData % getTrackMatXS(p, p % matIdx())
    end if

    ! Calculate ratio between local cross-section and majorant
    ratio = sigmaT * majorant_inv

    ! Cut-off criterion to decide on tracking method
    if (ratio > (ONE - self % cutoff)) then
      call deltaTracking(self, p, tally, thisCycle, nextCycle)
    else
      call surfaceTracking(self, p, tally, thisCycle, nextCycle)

      if (.not. self % collided) then
        call deltaTracking(self, p, tally, thisCycle, nextCycle)
      end if

    end if

  end subroutine tracking_selection

  !!
  !! Move the particle in the geometry
  !!
  subroutine step(self, p, distance)
    class(transportOperatorHTField), intent(inout) :: self
    class(particle), intent(inout)                 :: p
    real(defReal), intent(in)                      :: distance
    real(defReal), dimension(3)                    :: displacement
    real(defReal)                                  :: delta

    ! Calculating displacement before using takeAboveGeom, in order to use maps
    ! (e.g., materialMaps) in the field
    displacement = self % displacement % at(p % coords)

    ! Pop particle out of the geometry
    call p % coords % takeAboveGeom()

    ! Move the particle to the real frame
    call p % coords % assignPosition(p % rGlobal() + displacement)

    call self % geom % teleport(p % coords, distance)

    delta = self % displacement % getDelta(p % coords)

    ! Move back to the map
    call p % coords % assignPosition(p % rGlobal() + self % displacement % backwards(p % coords, delta))
    call self % geom % placeCoord(p % coords)

  end subroutine step

  !!
  !! Performs delta tracking until a real collision point is found
  !!
  subroutine deltaTracking(self, p, tally, thisCycle, nextCycle)
    class(transportOperatorHTField), intent(inout) :: self
    class(particle), intent(inout)            :: p
    type(tallyAdmin), intent(inout)           :: tally
    class(particleDungeon), intent(inout)     :: thisCycle
    class(particleDungeon), intent(inout)     :: nextCycle
    real(defReal)                             :: majorant_inv, sigmaT, distance, speed, time
    character(100), parameter :: Here = 'deltaTracking (transportOperatorHTField_class.f90)'

    ! Get majorant XS inverse: 1/Sigma_majorant
    majorant_inv = ONE / self % xsData % getTrackingXS(p, p % matIdx(), MAJORANT_XS)

   ! Should never happen! Prevents Inf distances
    if (abs(majorant_inv) > huge(majorant_inv)) call fatalError(Here, "Majorant is 0")

    DTLoop:do
      distance = -log( p % pRNG % get() ) * majorant_inv

      speed = p % getSpeed()
      time = distance / speed + p % time

      ! Set a max flight distance due to hitting the time-boundary
      if (p % timeMax > ZERO .and. time > p % timeMax) then
        distance = speed * (p % timeMax - p % time)
        p % fate = AGED_FATE
      end if

      ! Move partice in the geometry
      ! call self % geom % teleport(p % coords, distance)
      call self % step(p, distance)
      p % time = p % time + distance / speed

      select case(p % matIdx())

        ! If particle has leaked exit
        case(OUTSIDE_FILL)
          p % fate = LEAK_FATE
          p % isDead = .true.
          exit DTLoop

        ! Check for void
        case(VOID_MAT)
          if (p % fate == AGED_FATE) exit DTLoop
          call tally % reportInColl(p, .true.)
          cycle DTLoop

        ! Give error if the particle somehow ended in an undefined material
        case(UNDEF_MAT)
          print *, "Particle location: ", p % rGlobal()
          call fatalError(Here, "Particle is in undefined material")

        ! Give error if the particle somehow ended in an overlap material
        case(OVERLAP_MAT)
          print *, "Particle location: ", p % rGlobal()
          call fatalError(Here, "Particle is in overlapping cells")

        case default
          ! All is well

      end select

      ! If particle has aged, exit
      if (p % fate == AGED_FATE) then
        exit DTLoop
      end if

      ! Get local conditions
      call self % localConditions(p)

      ! Obtain the local cross-section
      sigmaT = self % xsData % getTrackMatXS(p, p % matIdx())

      ! Roll RNG to determine if the collision is real or virtual
      ! Exit the loop if the collision is real, report collision if virtual
      if (p % pRNG % get() < sigmaT*majorant_inv) then
        exit DTLoop
      else
        call tally % reportInColl(p, .true.)
      end if

    end do DTLoop

    call tally % reportTrans(p)

  end subroutine deltaTracking

  !!
  !! Performs surface tracking until a collision point is found
  !!
  subroutine surfaceTracking(self, p, tally, thisCycle, nextCycle)
    class(transportOperatorHTField), intent(inout) :: self
    class(particle), intent(inout)            :: p
    type(tallyAdmin), intent(inout)           :: tally
    class(particleDungeon),intent(inout)      :: thisCycle
    class(particleDungeon),intent(inout)      :: nextCycle
    integer(shortInt)                         :: event, collFate
    real(defReal)                             :: sigmaT, dist, sigmaTrack, invSigmaTrack, &
                                                 speed, time
    real(defReal), parameter                  :: tol  = 1.0E-12
    type(distCache)                           :: cache
    character(100), parameter :: Here = 'surfaceTracking (transportOperatorHTField_class.f90)'

    ! Initialise default flag true unless changed
    self % collided = .true.

    STLoop: do

      ! Check if I am in perturbed material
      if (self % displacement % inDomain(p % coords)) then
        self % collided = .false.
        exit STLoop
      end if

      ! Get local conditions
      call self % localConditions(p)

      sigmaTrack = self % xsData % getTrackingXS(p, p % matIdx(), MATERIAL_XS)

      ! Obtain the local cross-section, depending on the material
      ! This branch is called in the case of voids with no imposed XS
      if (sigmaTrack < tol) then

        dist = INF
        invSigmaTrack = INF
        sigmaT = ZERO

      else

        invSigmaTrack = ONE / sigmaTrack
        dist = -log( p % pRNG % get()) * invSigmaTrack

        ! Obtain the local cross-section
        sigmaT = self % xsData % getTrackMatXS(p, p % matIdx())

        ! Should never happen! Catches NaN distances
        if (dist /= dist) call fatalError(Here, "Distance is NaN")

      end if

      speed = p % getSpeed()
      time = dist / speed + p % time

      ! Set a max flight distance due to hitting the time-boundary
      if (p % timeMax > ZERO .and. time > p % timeMax) then
        dist = speed * (p % timeMax - p % time)
        collFate = AGED_FATE
      else
        collFate = NO_FATE
      end if

      ! Save state before movement
      call p % savePrePath()

      ! Move to the next stop.
      if (self % cache) then
        call self % geom % move_withCache(p % coords, dist, event, cache)
      else
        call self % geom % move(p % coords, dist, event)
      end if

      ! Advance in time
      p % time = p % time + dist / speed

      ! Set fate if a collision occurred
      if (event == COLL_EV) p % fate = collFate

      ! Send tally report for a path moved
      call tally % reportPath(p, dist)

      select case(p % matIdx())

        ! Kill particle if it has leaked
        case(OUTSIDE_FILL)
          p % isDead = .true.
          p % fate = LEAK_FATE

        ! Give error if the particle somehow ended in an undefined material
        case(UNDEF_MAT)
          print*, 'Particle location: ', p % rGlobal()
          call fatalError(Here, "Particle is in undefined material")

        ! Give error if the particle ended in an overlap material
        case(OVERLAP_MAT)
          print*, 'Particle location: ', p % rGlobal()
          call fatalError(Here, "Particle is in overlapping cells")

        case default
          ! All is well

      end select

      ! Return if particle is stopped by death, or aging
      if (p % isDead .or. p % fate == AGED_FATE) exit STLoop

      ! Roll RNG to determine if the collision is real or virtual
      ! Exit the loop if the collision is real, report collision if virtual
      if (event == COLL_EV) then
        if (p % pRNG % get() < sigmaT*invSigmaTrack) then
          exit STLoop
        else
          call tally % reportInColl(p, .true.)
        end if
      end if

    end do STLoop

    call tally % reportTrans(p)

  end subroutine surfaceTracking

  !!
  !! Initialise HT operator from a dictionary
  !!
  !! See transportOperator_inter for more details
  !!
  subroutine init(self, dict)
    class(transportOperatorHTField), intent(inout) :: self
    class(dictionary), intent(in)                  :: dict
    integer(shortInt)                              :: idx

    ! Initialise superclass
    call init_super(self, dict)

    ! Retrieve DT-ST probability cutoff
    call dict % getOrDefault(self % cutoff,'cutoff',0.9_defReal)

    if (dict % isPresent('cache')) then
      call dict % get(self % cache, 'cache')
    end if

    ! Read geometry deformation
    idx = gr_fieldIdx(nameGeomDef)
    self % displacement => displacementField_CptrCast(gr_fieldPtr(idx))

  end subroutine init


end module transportOperatorHTField_class
