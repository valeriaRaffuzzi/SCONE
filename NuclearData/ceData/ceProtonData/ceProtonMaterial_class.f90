module ceProtonMaterial_class

  use numPrecision
  use universalVariables
  use genericProcedures,  only : fatalError, numToChar
  use RNG_class,          only : RNG
  use particle_class,     only : particle

  ! Nuclear Data Handles
  use materialHandle_inter,    only : materialHandle
  use neutronMaterial_inter,   only : neutronMaterial
  use neutronXsPackages_class, only : neutronMacroXSs

  ! CE Neutron Interfaces
  use ceProtonDatabase_inter, only : ceProtonDatabase
  use ceProtonNuclide_inter,  only : ceProtonNuclide, ceProtonNuclide_CptrCast

  ! Cache
  use ceCache_mod,             only : materialCache, nuclideCache

  implicit none
  private

  !!
  !! Public Pointer Cast
  !!
  public ceProtonMaterial_CptrCast
  public ceProtonMaterial_TptrCast

  !!
  !! An abstract class that represent all CE Neutron Material Data
  !!
  !! Exist mainly in order to decouple caching logic from the database implementation
  !! so there is no need to repeat it in every database type. Thus it will be easier to
  !! mantain and optimise.
  !!
  !! Note that a material without any composition is not allowed.
  !! Makes no assumption about the range of nucIdx. Allows for -ve values
  !!
  !! Interface:
  !!   materialHandle Interface
  !!   getMacroXSs -> return package of macroscopic XSs directly from Energy and RNG
  !!   set         -> Set data related to material by keyword association
  !!   setComposition -> Set composition of material from densities and nucIdxs
  !!   sampleNuclide  -> sample collision nuclide
  !!
  type, public, extends(neutronMaterial) :: ceProtonMaterial
    character(nameLen)                           :: name = ''
    integer(shortInt)                            :: matIdx  = 0
    real(defReal)                                :: kT      = ZERO
    real(defReal)                                :: excitEn = ZERO
    class(ceProtonDatabase), pointer             :: data => null()
    real(defReal), dimension(:), allocatable     :: dens
    integer(shortInt), dimension(:), allocatable :: nuclides
    logical(defBool)                             :: fissile = .false.

  contains

    ! Superclass procedures
    procedure :: kill
    generic   :: getMacroXSs => getMacroXSs_byE
    procedure :: getMacroXSs_byP
    procedure :: getMTxs

    ! Local procedures
    procedure, non_overridable :: set
    procedure, non_overridable :: setComposition
    procedure, non_overridable :: getMacroXSs_byE
    procedure                  :: isFissile
    procedure, non_overridable :: sampleNuclide

  end type ceProtonMaterial

contains

  !!
  !! Return to uninitialised state
  !!
  elemental subroutine kill(self)
    class(ceProtonMaterial), intent(inout) :: self

    self % matIdx  = 0
    self % kT      = ZERO
    self % data    => null()
    if (allocated(self % dens))     deallocate(self % dens)
    if (allocated(self % nuclides)) deallocate (self % nuclides)
    self % fissile = .false.

  end subroutine kill

  !!
  !! Return Macroscopic XSs for the material given particle
  !!
  !! See neutronMaterial_inter for details
  !!
  subroutine getMacroXSs_byP(self, xss, p)
    class(ceProtonMaterial), intent(in) :: self
    type(neutronMacroXSs), intent(out)   :: xss
    class(particle), intent(in)          :: p
    character(100), parameter :: Here = 'getMacroXSs_byP (ceProtonMaterial_class.f90)'

    if (.not. p % isMG) then
      call self % getMacroXSs(xss, p % E, p % pRNG)

    else
      call fatalError(Here,'MG neutron given to CE data')

    end if

  end subroutine getMacroXSs_byP

  !!
  !! Return Macroscopic XS for the material given particle and an MT number
  !!
  !! See neutronMaterial_inter for details
  !!
  function getMTxs(self, MT, p) result(xs)
    class(ceProtonMaterial), intent(in) :: self
    integer(shortInt), intent(in)        :: MT
    class(particle), intent(in)          :: p
    real(defReal)                        :: xs
    character(100), parameter :: Here = 'getMTxs_byP (ceProtonMaterial_class.f90)'

    if (p % isMG) call fatalError(Here,'MG neutron given to CE data')

    xs = self % data % getMaterialMTxs(p % E, self % matIdx, MT)

  end function getMTxs

  !!
  !! Set composition of the material in terms of nucIdx and atomic density
  !!
  !! Use this procedure ONLY during build. NEVER during transport.
  !! IT IS NOT THREAD SAFE!
  !!
  !! Args:
  !!   dens    [in] -> array of atomic densities [1/barn/cm] of nuclides
  !!   nucIdxs [in] -> correpsonding array with nucIdxs
  !!
  !! Errors:
  !!   FatalError if arrays have different size
  !!   FatalError if dens contains -ve values
  !!   FatalError if dens has size of 0 -> no composition
  !!
  subroutine setComposition(self, dens, nucIdxs)
    class(ceProtonMaterial), intent(inout)     :: self
    real(defReal), dimension(:), intent(in)     :: dens
    integer(shortInt), dimension(:), intent(in) :: nucIdxs
    character(100), parameter :: Here = 'setComposition (ceProtonMaterial_class.f90)'

    ! Check input
    if (size(dens) /= size(nucIdxs)) call fatalError(Here,'Different sizes of density and nuclide vector')
    if (any(dens < ZERO)) call fatalError(Here,'-ve nuclide densities are present')
    if (size(dens) == 0)  call fatalError(Here,'Empty composition is not allowed')

    ! Clean any current content
    if (allocated(self % dens))     deallocate(self % dens)
    if (allocated(self % nuclides)) deallocate(self % nuclides)

    ! Load values
    self % dens     = dens
    self % nuclides = nucIdxs

  end subroutine setComposition

  !!
  !! Set matIdx, pointer to a database and fissile flag
  !!
  !! All arguments are optional. Use with keyword association e.g.
  !!   call mat % set(matIdx = 7)
  !!
  !! Use this procedure ONLY during build. NEVER during transport.
  !! IT IS NOT THREAD SAFE!
  !!
  !! Args:
  !!   name [in]       -> material name
  !!   matIdx [in]     -> material index
  !!   database [in]   -> pointer to a database that updates XSs on the ceCache
  !!   fissile [in]    -> flag indicating whether fission data is present
  !!   temp [in]       -> TMS material temperature
  !!   excitation [in] -> mean excitation energy of the material
  !!
  subroutine set(self, name, matIdx, database, fissile, temp, excitation)
    class(ceProtonMaterial), intent(inout)                 :: self
    character(nameLen), intent(in), optional               :: name
    integer(shortInt), intent(in), optional                :: matIdx
    class(ceProtonDatabase), pointer, optional, intent(in) :: database
    logical(defBool), intent(in), optional                 :: fissile
    real(defReal), intent(in), optional                    :: temp
    real(defReal), intent(in), optional                    :: excitation
    character(100), parameter :: Here = 'set (ceProtonMaterial_class.f90)'

    if (present(name))       self % name    = name
    if (present(database))   self % data    => database
    if (present(fissile))    self % fissile = fissile
    if (present(matIdx))     self % matIdx  = matIdx
    if (present(temp))       self % kT      = (kBoltzmann * temp) / joulesPerMeV
    if (present(excitation)) self % excitEn = excitation

  end subroutine set

  !!
  !! Return Macroscopic XSs for the material
  !!
  !! Args:
  !!   xss [out]    -> Cross section package to store the data
  !!   E [in]       -> Requested energy [MeV]
  !!   rand [inout] -> Random Number Generator
  !!
  !! Errors:
  !!   fatalError if E is out-of-bounds for the stored data
  !!
  subroutine getMacroXSs_byE(self, xss, E, rand)
    class(ceProtonMaterial), intent(in) :: self
    type(neutronMacroXSs), intent(out)   :: xss
    real(defReal), intent(in)            :: E
    class(RNG), intent(inout)            :: rand

    ! Check Cache and update if needed
    if (materialCache(self % matIdx) % E_tail /= E .or. materialCache(self % matIdx) % E_tot /= E) then
      call self % data % updateMacroXSs(E, self % matIdx, rand)
    end if

    xss = materialCache(self % matIdx) % xss

  end subroutine getMacroXSs_byE

  !!
  !! Return .true. if material is fissile
  !!
  !! Args:
  !!   None
  !!
  !! Result:
  !!   .true. if fissile, .false. otherwise
  !!
  !! Errors:
  !!   None
  !!
  elemental function isFissile(self) result(isIt)
    class(ceProtonMaterial), intent(in) :: self
    logical(defBool)                     :: isIt

    isIt = self % fissile

  end function isFissile

  !!
  !! Sample collision nuclide at energy E
  !!
  !! This function randomly determines the exact nuclide for a collision
  !! It uses nuclide total XSs to determine nuclide
  !!
  !! Args:
  !!   E [in]       -> incident energy [MeV]
  !!   rand [inout] -> random number generator
  !!   nucIdx [out] -> sampled nuclide index
  !!   eOut [out]   -> relative energy between neutron and target (may be /= E in case of TMS)
  !!
  !! Errors:
  !!   fatalError if sampling fails for some reason (E.G. random number > 1)
  !!   fatalError if E is out-of-bounds of the present data
  !!
  subroutine sampleNuclide(self, E, rand, nucIdx, eOut)
    class(ceProtonMaterial), intent(in) :: self
    real(defReal), intent(in)           :: E
    class(RNG), intent(inout)           :: rand
    integer(shortInt), intent(out)      :: nucIdx
    real(defReal), intent(out)          :: eOut
    integer(shortInt)                   :: i
    real(defReal)                       :: trackMatXS, totNucXS, dens
    character(100), parameter :: Here = 'sampleNuclide (ceProtonMaterial_class.f90)'

    ! Save energy to be used to sample reaction
    eOut = E

    ! Get material tracking XS
    if (E /= materialCache(self % matIdx) % E_track) then
      call self % data % updateTrackMatXS(E, self % matIdx, rand)
    end if

    trackMatXS = materialCache(self % matIdx) % trackXS * rand % get()

    ! Loop over nuclides
    do i = 1,size(self % nuclides)

      nucIdx = self % nuclides(i)
      dens = self % dens(i)

      associate (nucCache => nuclideCache(nucIdx))

        ! Update nuclide cache if needed
        if (E /= nucCache % E_tot) call self % data % updateTotalNucXS(E, nucIdx, self % kT, rand)
        totNucXS = nucCache % xss % total

        trackMatXS = trackMatXS - totNucXS * dens

        ! Nuclide accepted
        if (trackMatXS < ZERO) return

      end associate

    end do

    ! Print error message as the inversion failed
    call fatalError(Here,'Nuclide sampling loop failed to terminate')

  end subroutine sampleNuclide

  !!
  !! Cast materialHandle pointer to ceProtonMaterial pointer
  !!
  !! Args:
  !!   source [in]    -> source pointer of class materialHandle
  !!
  !! Result:
  !!   Null is source is not of ceProtonMaterial
  !!   Pointer to source if source is ceProtonMaterial class
  !!
  pure function ceProtonMaterial_CptrCast(source) result(ptr)
    class(materialHandle), pointer, intent(in) :: source
    class(ceProtonMaterial), pointer          :: ptr

    select type(source)
      class is(ceProtonMaterial)
        ptr => source

      class default
        ptr => null()
    end select

  end function ceProtonMaterial_CptrCast

  !!
  !! Cast materialHandle pointer to ceProtonMaterial pointer
  !!
  !! Args:
  !!   source [in]    -> source pointer of class materialHandle
  !!
  !! Result:
  !!   Null is source is not of ceProtonMaterial
  !!   Pointer to source if source is ceProtonMaterial class
  !!
  pure function ceProtonMaterial_TptrCast(source) result(ptr)
    class(materialHandle), pointer, intent(in) :: source
    type(ceProtonMaterial), pointer            :: ptr

    select type(source)
      type is(ceProtonMaterial)
        ptr => source

      class default
        ptr => null()
    end select

  end function ceProtonMaterial_TptrCast


end module ceProtonMaterial_class
