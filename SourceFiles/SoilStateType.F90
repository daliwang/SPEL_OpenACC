module SoilStateType

  !------------------------------------------------------------------------------
  ! !USES:
  use shr_kind_mod    , only : r8 => shr_kind_r8
  !#py use shr_log_mod     , only : errMsg => shr_log_errMsg
  use decompMod       , only : bounds_type
  !#py use abortutils      , only : endrun
  !#py use spmdMod         , only : mpicom, MPI_INTEGER, masterproc
  !#py use ncdio_pio       , only : file_desc_t, ncd_defvar, ncd_io, ncd_double, ncd_int, ncd_inqvdlen
  !#py use ncdio_pio       , only : ncd_pio_openfile, ncd_inqfdims, ncd_pio_closefile, ncd_inqdid, ncd_inqdlen
  use elm_varpar      , only : more_vertlayers, numpft, numrad
  use elm_varpar      , only : nlevsoi, nlevgrnd, nlevlak, nlevsoifl, nlayer, nlayert, nlevurb, nlevsno
  use elm_varcon      , only : zsoi, dzsoi, zisoi, spval
  use elm_varcon      , only : secspday, pc, mu, denh2o, denice, grlnd
  use landunit_varcon , only : istice, istdlak, istwet, istsoil, istcrop, istice_mec
  use column_varcon   , only : icol_roof, icol_sunwall, icol_shadewall, icol_road_perv, icol_road_imperv
  use elm_varctl      , only : use_cn, use_lch4,use_dynroot, use_fates
  use elm_varctl      , only : use_erosion
  use elm_varctl      , only : use_var_soil_thick
  use elm_varctl      , only : iulog, fsurdat, hist_wrtch4diag
  use CH4varcon       , only : allowlakeprod
  use LandunitType    , only : lun_pp
  use ColumnType      , only : col_pp
  use VegetationType  , only : veg_pp
  !
  implicit none
  save
  public
  !
  ! !PUBLIC TYPES:
  type, public :: soilstate_type

     ! sand/ clay/ organic matter
     real(r8), pointer :: sandfrac_patch       (:)   ! patch sand fraction
     real(r8), pointer :: clayfrac_patch       (:)   ! patch clay fraction
     real(r8), pointer :: grvlfrac_patch       (:)   ! patch gravel fraction
     real(r8), pointer :: mss_frc_cly_vld_col  (:)   ! col mass fraction clay limited to 0.20
     real(r8), pointer :: cellorg_col          (:,:) ! col organic matter for gridcell containing column (1:nlevsoi)
     real(r8), pointer :: cellsand_col         (:,:) ! sand value for gridcell containing column (1:nlevsoi)
     real(r8), pointer :: cellclay_col         (:,:) ! clay value for gridcell containing column (1:nlevsoi)
     real(r8), pointer :: cellgrvl_col         (:,:) ! gravel value for gridcell containing column (1:nlevsoi)
     real(r8), pointer :: bd_col               (:,:) ! col bulk density of dry soil material [kg/m^3] (CN)

     ! hydraulic properties
     real(r8), pointer :: hksat_col            (:,:) ! col hydraulic conductivity at saturation (mm H2O /s)
     real(r8), pointer :: hksat_min_col        (:,:) ! col mineral hydraulic conductivity at saturation (hksat) (mm/s)
     real(r8), pointer :: hk_l_col             (:,:) ! col hydraulic conductivity (mm/s)
     real(r8), pointer :: smp_l_col            (:,:) ! col soil matric potential (mm)
     real(r8), pointer :: smpmin_col           (:)   ! col restriction for min of soil potential (mm)
     real(r8), pointer :: bsw_col              (:,:) ! col Clapp and Hornberger "b" (nlevgrnd)
     real(r8), pointer :: watsat_col           (:,:) ! col volumetric soil water at saturation (porosity)
     real(r8), pointer :: watdry_col           (:,:) ! col btran parameter for btran = 0
     real(r8), pointer :: watopt_col           (:,:) ! col btran parameter for btran = 1
     real(r8), pointer :: watfc_col            (:,:) ! col volumetric soil water at field capacity (nlevsoi)
     real(r8), pointer :: watmin_col           (:,:) ! col minimum volumetric soil water (nlevsoi)
     real(r8), pointer :: sucsat_col           (:,:) ! col minimum soil suction (mm) (nlevgrnd)
     real(r8), pointer :: sucmin_col           (:,:) ! col minimum allowable soil liquid suction pressure (mm) [Note: sucmin_col is a negative value, while sucsat_col is a positive quantity]
     real(r8), pointer :: soilbeta_col         (:)   ! col factor that reduces ground evaporation L&P1992(-)
     real(r8), pointer :: soilalpha_col        (:)   ! col factor that reduces ground saturated specific humidity (-)
     real(r8), pointer :: soilalpha_u_col      (:)   ! col urban factor that reduces ground saturated specific humidity (-)
     real(r8), pointer :: soilpsi_col          (:,:) ! col soil water potential in each soil layer (MPa) (CN)
     real(r8), pointer :: wtfact_col           (:)   ! col maximum saturated fraction for a gridcell
     real(r8), pointer :: porosity_col         (:,:) ! col soil porisity (1-bulk_density/soil_density) (VIC)
     real(r8), pointer :: eff_porosity_col     (:,:) ! col effective porosity = porosity - vol_ice (nlevgrnd)
     real(r8), pointer :: gwc_thr_col          (:)   ! col threshold soil moisture based on clay content

  !!!   ! thermal conductivity / heat capacity
     real(r8), pointer :: thk_col              (:,:) ! col thermal conductivity of each layer [W/m-K]
     real(r8), pointer :: tkmg_col             (:,:) ! col thermal conductivity, soil minerals  [W/m-K] (new) (nlevgrnd)
     real(r8), pointer :: tkdry_col            (:,:) ! col thermal conductivity, dry soil (W/m/Kelvin) (nlevgrnd)
     real(r8), pointer :: tksatu_col           (:,:) ! col thermal conductivity, saturated soil [W/m-K] (new) (nlevgrnd)
     real(r8), pointer :: csol_col             (:,:) ! col heat capacity, soil solids (J/m**3/Kelvin) (nlevgrnd)

  !!!   ! roots
     real(r8), pointer :: rootr_patch          (:,:) ! patch effective fraction of roots in each soil layer (nlevgrnd)
     real(r8), pointer :: rootr_col            (:,:) ! col effective fraction of roots in each soil layer (nlevgrnd)
     real(r8), pointer :: rootfr_col           (:,:) ! col fraction of roots in each soil layer (nlevgrnd)
     real(r8), pointer :: rootfr_patch         (:,:) ! patch fraction of roots in each soil layer (nlevgrnd)
     real(r8), pointer :: rootr_road_perv_col  (:,:) ! col effective fraction of roots in each soil layer of urban pervious road
     real(r8), pointer :: rootfr_road_perv_col (:,:) ! col effective fraction of roots in each soil layer of urban pervious road
     real(r8), pointer :: root_depth_patch     (:)   ! rooting depth of each PFT (m)
     real(r8), pointer :: k_soil_root_patch    (:,:) ! patch soil-root interface conductance [mm/s]
     real(r8), pointer :: root_conductance_patch(:,:) ! patch root conductance [mm/s]
     real(r8), pointer :: soil_conductance_patch(:,:) ! patch soil conductance [mm/s]

   contains

     procedure, public  :: Init
     procedure, public :: InitAllocate
     procedure, private :: InitHistory
     !#py procedure, private :: InitCold
     !#py procedure, public  :: Restart
     !#py procedure, public  :: InitColdGhost


  end type soilstate_type
  !------------------------------------------------------------------------
contains

  !------------------------------------------------------------------------
  subroutine Init(this, bounds)

    class(soilstate_type) :: this
    type(bounds_type), intent(in) :: bounds

    call this%InitAllocate(bounds)
    call this%InitHistory(bounds)
    !#py call this%InitCold(bounds)

  end subroutine Init

  !------------------------------------------------------------------------
  subroutine InitAllocate(this, bounds)
    !
    ! !DESCRIPTION:
    ! Initialize module data structure
    !
    ! !ARGUMENTS:
    class(soilstate_type) :: this
    type(bounds_type), intent(in) :: bounds
    !
    ! !LOCAL VARIABLES:
    integer :: begp, endp
    integer :: begc, endc
    integer :: begg, endg
    integer :: begc_all, endc_all
    !------------------------------------------------------------------------

    begp     = bounds%begp    ; endp     = bounds%endp
    begc     = bounds%begc    ; endc     = bounds%endc
    begg     = bounds%begg    ; endg     = bounds%endg
    begc_all = bounds%begc_all; endc_all = bounds%endc_all

    allocate(this%mss_frc_cly_vld_col  (begc:endc))                     ; this%mss_frc_cly_vld_col  (:)   = spval
    allocate(this%sandfrac_patch       (begp:endp))                     ; this%sandfrac_patch       (:)   = spval
    allocate(this%clayfrac_patch       (begp:endp))                     ; this%clayfrac_patch       (:)   = spval
    allocate(this%grvlfrac_patch       (begp:endp))                     ; this%grvlfrac_patch       (:)   = spval
    allocate(this%cellorg_col          (begc:endc,nlevgrnd))            ; this%cellorg_col          (:,:) = spval
    allocate(this%cellsand_col         (begc:endc,nlevgrnd))            ; this%cellsand_col         (:,:) = spval
    allocate(this%cellclay_col         (begc:endc,nlevgrnd))            ; this%cellclay_col         (:,:) = spval
    allocate(this%cellgrvl_col         (begc:endc,nlevgrnd))            ; this%cellgrvl_col         (:,:) = spval
    allocate(this%bd_col               (begc:endc,nlevgrnd))            ; this%bd_col               (:,:) = spval

    allocate(this%hksat_col            (begc_all:endc_all,nlevgrnd))    ; this%hksat_col            (:,:) = spval
    allocate(this%hksat_min_col        (begc:endc,nlevgrnd))            ; this%hksat_min_col        (:,:) = spval
    allocate(this%hk_l_col             (begc:endc,nlevgrnd))            ; this%hk_l_col             (:,:) = spval
    allocate(this%smp_l_col            (begc:endc,nlevgrnd))            ; this%smp_l_col            (:,:) = spval
    allocate(this%smpmin_col           (begc:endc))                     ; this%smpmin_col           (:)   = spval

    allocate(this%bsw_col              (begc_all:endc_all,nlevgrnd))    ; this%bsw_col              (:,:) = spval
    allocate(this%watsat_col           (begc_all:endc_all,nlevgrnd))    ; this%watsat_col           (:,:) = spval
    allocate(this%watdry_col           (begc:endc,nlevgrnd))            ; this%watdry_col           (:,:) = spval
    allocate(this%watopt_col           (begc:endc,nlevgrnd))            ; this%watopt_col           (:,:) = spval
    allocate(this%watfc_col            (begc:endc,nlevgrnd))            ; this%watfc_col            (:,:) = spval
    allocate(this%watmin_col           (begc:endc,nlevgrnd))            ; this%watmin_col           (:,:) = spval
    allocate(this%sucsat_col           (begc:endc,nlevgrnd))            ; this%sucsat_col           (:,:) = spval
    allocate(this%sucmin_col           (begc:endc,nlevgrnd))            ; this%sucmin_col           (:,:) = spval
    allocate(this%soilbeta_col         (begc:endc))                     ; this%soilbeta_col         (:)   = spval
    allocate(this%soilalpha_col        (begc:endc))                     ; this%soilalpha_col        (:)   = spval
    allocate(this%soilalpha_u_col      (begc:endc))                     ; this%soilalpha_u_col      (:)   = spval
    allocate(this%soilpsi_col          (begc:endc,nlevgrnd))            ; this%soilpsi_col          (:,:) = spval
    allocate(this%wtfact_col           (begc:endc))                     ; this%wtfact_col           (:)   = spval
    allocate(this%porosity_col         (begc:endc,nlayer))              ; this%porosity_col         (:,:) = spval
    allocate(this%eff_porosity_col     (begc:endc,nlevgrnd))            ; this%eff_porosity_col     (:,:) = spval
    allocate(this%gwc_thr_col          (begc:endc))                     ; this%gwc_thr_col          (:)   = spval

    allocate(this%thk_col              (begc:endc,-nlevsno+1:nlevgrnd)) ; this%thk_col              (:,:) = spval
    allocate(this%tkmg_col             (begc:endc,nlevgrnd))            ; this%tkmg_col             (:,:) = spval
    allocate(this%tkdry_col            (begc:endc,nlevgrnd))            ; this%tkdry_col            (:,:) = spval
    allocate(this%tksatu_col           (begc:endc,nlevgrnd))            ; this%tksatu_col           (:,:) = spval
    allocate(this%csol_col             (begc:endc,nlevgrnd))            ; this%csol_col             (:,:) = spval

    allocate(this%rootr_patch          (begp:endp,1:nlevgrnd))          ; this%rootr_patch          (:,:) = spval
    allocate(this%rootr_col            (begc:endc,nlevgrnd))            ; this%rootr_col            (:,:) = spval
    allocate(this%rootr_road_perv_col  (begc:endc,1:nlevgrnd))          ; this%rootr_road_perv_col  (:,:) = spval
    allocate(this%rootfr_patch         (begp:endp,1:nlevgrnd))          ; this%rootfr_patch         (:,:) = spval
    allocate(this%rootfr_col           (begc:endc,1:nlevgrnd))          ; this%rootfr_col           (:,:) = spval
    allocate(this%rootfr_road_perv_col (begc:endc,1:nlevgrnd))          ; this%rootfr_road_perv_col (:,:) = spval
    allocate(this%root_depth_patch     (begp:endp))                     ; this%root_depth_patch     (:)   = spval
    allocate(this%k_soil_root_patch    (begp:endp,1:nlevsoi))           ; this%k_soil_root_patch (:,:) = spval
    allocate(this%root_conductance_patch(begp:endp,1:nlevsoi))          ; this%root_conductance_patch (:,:) = spval
    allocate(this%soil_conductance_patch(begp:endp,1:nlevsoi))          ; this%soil_conductance_patch (:,:) = spval

  end subroutine InitAllocate

  !-----------------------------------------------------------------------
  subroutine InitHistory(this, bounds)
    !
    ! History fields initialization
    !
    ! !USES:
    !#py use histFileMod   , only: hist_addfld1d, hist_addfld2d, no_snow_normal
    !
    ! !ARGUMENTS:
    class(soilstate_type) :: this
    type(bounds_type), intent(in) :: bounds
    !
    ! !LOCAL VARIABLES:
    integer :: begc, endc
    integer :: begp, endp
    character(10)     :: active
    real(r8), pointer :: data2dptr(:,:), data1dptr(:) ! temp. pointers for slicing larger arrays
    !---------------------------------------------------------------------

    begp = bounds%begp; endp= bounds%endp
    begc = bounds%begc; endc= bounds%endc


    if (use_lch4) then
       if (hist_wrtch4diag) then
          active = "active"
       else
          active = "inactive"
       end if
    else
       active = "inactive"
    end if
    !#py call hist_addfld2d (fname='SMP',  units='mm', type2d='levgrnd',  &
         !#py avgflag='A', long_name='soil matric potential (vegetated landunits only)', &
         !#py !#py ptr_col=this%smp_l_col, set_spec=spval, l2g_scale_type='veg', default=active)

    if (use_cn) then
       this%bsw_col(begc:endc,:) = spval
       !#py call hist_addfld2d (fname='bsw', units='1', type2d='levgrnd', &
            !#py avgflag='A', long_name='clap and hornberger B', &
            !#py !#py ptr_col=this%bsw_col, default='inactive')
    end if

    if (use_cn) then
       this%rootfr_patch(begp:endp,:) = spval
       !#py call hist_addfld2d (fname='ROOTFR', units='proportion', type2d='levgrnd', &
            !#py avgflag='A', long_name='fraction of roots in each soil layer', &
            !#py !#py ptr_patch=this%rootfr_patch, default='inactive')
    end if

    if (use_cn) then
       this%rootr_patch(begp:endp,:) = spval
       !#py call hist_addfld2d (fname='ROOTR', units='proportion', type2d='levgrnd', &
            !#py avgflag='A', long_name='effective fraction of roots in each soil layer', &
            !#py !#py ptr_patch=this%rootr_patch, default='inactive')
    end if

    if (use_cn) then
       this%rootr_col(begc:endc,:) = spval
       !#py call hist_addfld2d (fname='ROOTR_COLUMN', units='proportion', type2d='levgrnd', &
            !#py avgflag='A', long_name='effective fraction of roots in each soil layer', &
            !#py !#py ptr_col=this%rootr_col, default='inactive')

    end if

    if (use_dynroot) then
       this%root_depth_patch(begp:endp) = spval
       !#py call hist_addfld1d (fname='ROOT_DEPTH', units="m", &
            !#py avgflag='A', long_name='rooting depth', &
            !#py !#py ptr_patch=this%root_depth_patch, default='inactive' )
    end if

    if (use_cn .or. use_fates) then
       this%soilpsi_col(begc:endc,:) = spval
       !#py call hist_addfld2d (fname='SOILPSI', units='MPa', type2d='levgrnd', &
            !#py avgflag='A', long_name='soil water potential in each soil layer', &
            !#py !#py ptr_col=this%soilpsi_col)
    end if

    this%thk_col(begc:endc,-nlevsno+1:0) = spval
    data2dptr => this%thk_col(:,-nlevsno+1:0)
    !#py call hist_addfld2d (fname='SNO_TK', units='W/m-K', type2d='levsno', &
         !#py avgflag='A', long_name='Thermal conductivity', &
         !#py !#py ptr_col=data2dptr, no_snow_behavior=no_snow_normal, default='inactive')

    this%hk_l_col(begc:endc,:) = spval
    !#py call hist_addfld2d (fname='HK',  units='mm/s', type2d='levgrnd',  &
         !#py avgflag='A', long_name='hydraulic conductivity (vegetated landunits only)', &
         !#py !#py ptr_col=this%hk_l_col, set_spec=spval, l2g_scale_type='veg', default='inactive')

    this%soilalpha_col(begc:endc) = spval
    !#py call hist_addfld1d (fname='SoilAlpha',  units='1',  &
         !#py avgflag='A', long_name='factor limiting ground evap', &
         !#py !#py ptr_col=this%soilalpha_col, set_urb=spval)

    this%soilalpha_u_col(begc:endc) = spval
    !#py call hist_addfld1d (fname='SoilAlpha_U',  units='1',  &
         !#py avgflag='A', long_name='urban factor limiting ground evap', &
         !#py !#py ptr_col=this%soilalpha_u_col, set_nourb=spval)

    if (use_cn) then
       this%watsat_col(begc:endc,:) = spval
       !#py call hist_addfld2d (fname='watsat', units='m^3/m^3', type2d='levgrnd', &
            !#py avgflag='A', long_name='water saturated', &
            !#py !#py ptr_col=this%watsat_col, default='inactive')
    end if

    if (use_cn) then
       this%eff_porosity_col(begc:endc,:) = spval
       !#py call hist_addfld2d (fname='EFF_POROSITY', units='proportion', type2d='levgrnd', &
            !#py avgflag='A', long_name='effective porosity = porosity - vol_ice', &
            !#py !#py ptr_col=this%eff_porosity_col, default='inactive')
    end if

    if (use_cn) then
       this%watfc_col(begc:endc,:) = spval
       !#py call hist_addfld2d (fname='watfc', units='m^3/m^3', type2d='levgrnd', &
            !#py avgflag='A', long_name='water field capacity', &
            !#py !#py ptr_col=this%watfc_col, default='inactive')
    end if

  end subroutine InitHistory

  !-----------------------------------------------------------------------
!#py   subroutine InitCold(this, bounds)
!#py     !
!#py     ! Initialize module surface albedos to reasonable values
!#py     !
!#py     ! !USES:
!#py     use pftvarcon           , only : noveg, roota_par, rootb_par
!#py     !#py use fileutils           , only : getfil
!#py     use organicFileMod      , only : organicrd
!#py     use SharedParamsMod   , only : ParamsShareInst
!#py     use FuncPedotransferMod , only : pedotransf, get_ipedof
!#py     use RootBiophysMod      , only : init_vegrootfr
!#py     !
!#py     ! !ARGUMENTS:
!#py     class(soilstate_type) :: this
!#py     type(bounds_type), intent(in) :: bounds
!#py     !
!#py                                                         ! !LOCAL VARIABLES:
!#py     integer            :: p, lev, c, l, g, j            ! indices
!#py     real(r8)           :: om_frac                       ! organic matter fraction
!#py     real(r8)           :: om_tkm         = 0.25_r8      ! thermal conductivity of organic soil (Farouki, 1986) [W/m/K]
!#py     real(r8)           :: om_watsat_lake = 0.9_r8       ! porosity of organic soil
!#py     real(r8)           :: om_hksat_lake  = 0.1_r8       ! saturated hydraulic conductivity of organic soil [mm/s]
!#py     real(r8)           :: om_sucsat_lake = 10.3_r8      ! saturated suction for organic matter (Letts, 2000)
!#py     real(r8)           :: om_b_lake      = 2.7_r8       ! Clapp Hornberger paramater for oragnic soil (Letts, 2000) (lake)
!#py     real(r8)           :: om_watsat                     ! porosity of organic soil
!#py     real(r8)           :: om_hksat                      ! saturated hydraulic conductivity of organic soil [mm/s]
!#py     real(r8)           :: om_sucsat                     ! saturated suction for organic matter (mm)(Letts, 2000)
!#py     real(r8)           :: om_csol        = 2.5_r8       ! heat capacity of peat soil *10^6 (J/K m3) (Farouki, 1986)
!#py     real(r8)           :: om_tkd         = 0.05_r8      ! thermal conductivity of dry organic soil (Farouki, 1981)
!#py     real(r8)           :: om_b                          ! Clapp Hornberger paramater for oragnic soil (Letts, 2000)
!#py     real(r8)           :: zsapric        = 0.5_r8       ! depth (m) that organic matter takes on characteristics of sapric peat
!#py     real(r8)           :: csol_bedrock   = 2.0e6_r8     ! vol. heat capacity of granite/sandstone  J/(m3 K)(Shabbir, 2000)
!#py     real(r8)           :: pcalpha        = 0.5_r8       ! percolation threshold
!#py     real(r8)           :: pcbeta         = 0.139_r8     ! percolation exponent
!#py     real(r8)           :: pc_lake        = 0.5_r8       ! percolation threshold
!#py     real(r8)           :: perc_frac                     ! "percolating" fraction of organic soil
!#py     real(r8)           :: perc_norm                     ! normalize to 1 when 100% organic soil
!#py     real(r8)           :: uncon_hksat                   ! series conductivity of mineral/organic soil
!#py     real(r8)           :: uncon_frac                    ! fraction of "unconnected" soil
!#py     real(r8)           :: bd                            ! bulk density of dry soil material [kg/m^3]
!#py     real(r8)           :: tkm                           ! mineral conductivity
!#py     real(r8)           :: xksat                         ! maximum hydraulic conductivity of soil [mm/s]
!#py     real(r8)           :: clay,sand,gravel              ! temporaries
!#py     real(r8)           :: organic_max                   ! organic matter (kg/m3) where soil is assumed to act like peat
!#py     integer            :: dimid                         ! dimension id
!#py     logical            :: readvar
!#py     type(file_desc_t)  :: ncid                          ! netcdf id
!#py     real(r8) ,pointer  :: zsoifl (:)                    ! Output: [real(r8) (:)]  original soil midpoint
!#py     real(r8) ,pointer  :: zisoifl (:)                   ! Output: [real(r8) (:)]  original soil interface depth
!#py     real(r8) ,pointer  :: dzsoifl (:)                   ! Output: [real(r8) (:)]  original soil thickness
!#py     real(r8) ,pointer  :: gti (:)                       ! read in - fmax
!#py     real(r8) ,pointer  :: sand3d (:,:)                  ! read in - soil texture: percent sand (needs to be a pointer for use in ncdio)
!#py     real(r8) ,pointer  :: clay3d (:,:)                  ! read in - soil texture: percent clay (needs to be a pointer for use in ncdio)
!#py     real(r8) ,pointer  :: grvl3d (:,:)                  ! read in - soil texture: percent gravel (needs to be a pointer for use in ncdio)
!#py     real(r8) ,pointer  :: organic3d (:,:)               ! read in - organic matter: kg/m3 (needs to be a pointer for use in ncdio)
!#py     character(len=256) :: locfn                         ! local filename
!#py     integer            :: nlevbed                       ! # of layers above bedrock
!#py     integer            :: ipedof
!#py     integer            :: begc, endc
!#py     integer            :: begg, endg
!#py     real(r8), parameter :: min_liquid_pressure = -10132500._r8 ! Minimum soil liquid water pressure [mm]
!#py     !-----------------------------------------------------------------------
!#py     begc = bounds%begc; endc= bounds%endc
!#py     begg = bounds%begg; endg= bounds%endg
!#py 
!#py     do c = bounds%begc, bounds%endc
!#py        this%smpmin_col(c) = -1.e8_r8
!#py     end do
!#py     ! --------------------------------------------------------------------
!#py     ! Initialize root fraction (computing from surface, d is depth in meter):
!#py     ! --------------------------------------------------------------------
!#py     ! Currently pervious road has same properties as soil
!#py     do c = bounds%begc, bounds%endc
!#py        l = col_pp%landunit(c)
!#py        if (lun_pp%urbpoi(l) .and. col_pp%itype(c) == icol_road_perv) then
!#py           do lev = 1, nlevgrnd
!#py              this%rootfr_road_perv_col(c,lev) = 0._r8
!#py           enddo
!#py           do lev = 1,nlevsoi
!#py              this%rootfr_road_perv_col(c,lev) = 0.1_r8  ! uniform profile
!#py           end do
!#py        end if
!#py     end do
!#py 
!#py     do c = bounds%begc,bounds%endc
!#py        this%rootfr_col (c,nlevsoi+1:nlevgrnd) = 0._r8
!#py        if (lun_pp%itype(l) == istsoil .or. lun_pp%itype(l) == istcrop) then
!#py           this%rootfr_col (c,nlevsoi+1:nlevgrnd) = 0._r8
!#py        else if (lun_pp%itype(l) == istdlak .and. allowlakeprod) then
!#py           this%rootfr_col (c,:) = spval
!#py        else  ! Inactive CH4 columns
!#py           this%rootfr_col (c,:) = spval
!#py        end if
!#py     end do
!#py 
!#py    ! Initialize root fraction
!#py 
!#py    call init_vegrootfr(bounds, nlevsoi, nlevgrnd, &
!#py         col_pp%nlevbed(bounds%begc:bounds%endc)    , &
!#py         this%rootfr_patch(bounds%begp:bounds%endp,1:nlevgrnd))
!#py 
!#py     ! --------------------------------------------------------------------
!#py     ! dynamic memory allocation
!#py     ! --------------------------------------------------------------------
!#py 
!#py     allocate(sand3d(begg:endg,nlevsoifl))
!#py     allocate(clay3d(begg:endg,nlevsoifl))
!#py     allocate(grvl3d(begg:endg,nlevsoifl))
!#py 
!#py     ! --------------------------------------------------------------------
!#py     ! Read surface dataset
!#py     ! --------------------------------------------------------------------
!#py 
!#py     if (masterproc) then
!#py        !#py write(iulog,*) 'Attempting to read soil color, sand and clay boundary data .....'
!#py     end if
!#py 
!#py     !#py call getfil (fsurdat, locfn, 0)
!#py     !#py call ncd_pio_openfile (ncid, locfn, 0)
!#py 
!#py     !#py !#py call ncd_inqdlen(ncid,dimid,nlevsoifl,name='nlevsoi')
!#py     if ( .not. more_vertlayers )then
!#py        if ( nlevsoifl /= nlevsoi )then
!#py           !#py call endrun(msg=' ERROR: Number of soil layers on file does NOT match the number being used'//&
!#py                !#py !#py errMsg(__FILE__, __LINE__))
!#py        end if
!#py     else
!#py        ! read in layers, interpolate to high resolution grid later
!#py     end if
!#py 
!#py     ! Read in organic matter dataset
!#py 
!#py     organic_max = ParamsShareInst%organic_max
!#py 
!#py     allocate(organic3d(bounds%begg:bounds%endg,nlevsoifl))
!#py     call organicrd(organic3d)
!#py 
!#py     ! Read in sand, clay, gravel data
!#py 
!#py     call ncd_io(ncid=ncid, varname='PCT_SAND', flag='read', data=sand3d, dim1name=grlnd, readvar=readvar)
!#py     if (.not. readvar) then
!#py        call endrun(msg=' ERROR: PCT_SAND NOT on surfdata file'//errMsg(__FILE__, __LINE__))
!#py     end if
!#py 
!#py     call ncd_io(ncid=ncid, varname='PCT_CLAY', flag='read', data=clay3d, dim1name=grlnd, readvar=readvar)
!#py     if (.not. readvar) then
!#py        call endrun(msg=' ERROR: PCT_CLAY NOT on surfdata file'//errMsg(__FILE__, __LINE__))
!#py     end if
!#py 
!#py     if (use_erosion) then
!#py        call ncd_io(ncid=ncid, varname='PCT_GRVL', flag='read', data=grvl3d, dim1name=grlnd, readvar=readvar)
!#py        if (.not. readvar) then
!#py           call endrun(msg=' ERROR: PCT_GRVL NOT on surfdata file'//errMsg(__FILE__, __LINE__))
!#py        end if
!#py     else
!#py        grvl3d(:,:) = 0._r8
!#py     end if
!#py 
!#py     do p = bounds%begp,bounds%endp
!#py        g = veg_pp%gridcell(p)
!#py        if ( sand3d(g,1)+clay3d(g,1) == 0.0_r8 )then
!#py           if ( any( sand3d(g,:)+clay3d(g,:) /= 0.0_r8 ) )then
!#py              call endrun(msg='found depth points that do NOT sum to zero when surface does'//&
!#py                   errMsg(__FILE__, __LINE__))
!#py           end if
!#py           sand3d(g,:) = 1.0_r8
!#py           clay3d(g,:) = 1.0_r8
!#py        end if
!#py        if ( any( sand3d(g,:)+clay3d(g,:) == 0.0_r8 ) )then
!#py           call endrun(msg='after setting, found points sum to zero'//errMsg(__FILE__, __LINE__))
!#py        end if
!#py 
!#py        this%sandfrac_patch(p) = sand3d(g,1)/100.0_r8
!#py        this%clayfrac_patch(p) = clay3d(g,1)/100.0_r8
!#py        this%grvlfrac_patch(p) = grvl3d(g,1)/100.0_r8
!#py     end do
!#py 
!#py     ! Read fmax
!#py 
!#py     allocate(gti(bounds%begg:bounds%endg))
!#py     call ncd_io(ncid=ncid, varname='FMAX', flag='read', data=gti, dim1name=grlnd, readvar=readvar)
!#py     if (.not. readvar) then
!#py        call endrun(msg=' ERROR: FMAX NOT on surfdata file'//errMsg(__FILE__, __LINE__))
!#py     end if
!#py     do c = bounds%begc, bounds%endc
!#py        g = col_pp%gridcell(c)
!#py        this%wtfact_col(c) = gti(g)
!#py     end do
!#py     deallocate(gti)
!#py 
!#py     ! Close file
!#py 
!#py     call ncd_pio_closefile(ncid)
!#py 
!#py     ! --------------------------------------------------------------------
!#py     ! get original soil depths to be used in interpolation of sand and clay
!#py     ! --------------------------------------------------------------------
!#py 
!#py     allocate(zsoifl(1:nlevsoifl), zisoifl(0:nlevsoifl), dzsoifl(1:nlevsoifl))
!#py     do j = 1, nlevsoifl
!#py        zsoifl(j) = 0.025*(exp(0.5_r8*(j-0.5_r8))-1._r8)    !node depths
!#py     enddo
!#py 
!#py     dzsoifl(1) = 0.5_r8*(zsoifl(1)+zsoifl(2))             !thickness b/n two interfaces
!#py     do j = 2,nlevsoifl-1
!#py        dzsoifl(j)= 0.5_r8*(zsoifl(j+1)-zsoifl(j-1))
!#py     enddo
!#py     dzsoifl(nlevsoifl) = zsoifl(nlevsoifl)-zsoifl(nlevsoifl-1)
!#py 
!#py     zisoifl(0) = 0._r8
!#py     do j = 1, nlevsoifl-1
!#py        zisoifl(j) = 0.5_r8*(zsoifl(j)+zsoifl(j+1))         !interface depths
!#py     enddo
!#py     zisoifl(nlevsoifl) = zsoifl(nlevsoifl) + 0.5_r8*dzsoifl(nlevsoifl)
!#py 
!#py     ! --------------------------------------------------------------------
!#py     ! Set soil hydraulic and thermal properties: non-lake
!#py     ! --------------------------------------------------------------------
!#py 
!#py     !   urban roof, sunwall and shadewall thermal properties used to
!#py     !   derive thermal conductivity and heat capacity are set to special
!#py     !   value because thermal conductivity and heat capacity for urban
!#py     !   roof, sunwall and shadewall are prescribed in SoilThermProp.F90
!#py     !   in SoilPhysicsMod.F90
!#py 
!#py 
!#py     do c = bounds%begc, bounds%endc
!#py        g = col_pp%gridcell(c)
!#py        l = col_pp%landunit(c)
!#py        nlevbed = col_pp%nlevbed(c)
!#py 
!#py        if (lun_pp%itype(l)==istwet .or. lun_pp%itype(l)==istice .or. lun_pp%itype(l)==istice_mec) then
!#py 
!#py           do lev = 1,nlevgrnd
!#py              this%bsw_col(c,lev)    = spval
!#py              this%watsat_col(c,lev) = spval
!#py              this%watfc_col(c,lev)  = spval
!#py              this%watmin_col(c,lev) = spval
!#py              this%hksat_col(c,lev)  = spval
!#py              this%sucsat_col(c,lev) = spval
!#py              this%sucmin_col(c,lev) = spval
!#py              this%watdry_col(c,lev) = spval
!#py              this%watopt_col(c,lev) = spval
!#py              this%bd_col(c,lev)     = spval
!#py              if (lev <= nlevsoi) then
!#py                 this%cellsand_col(c,lev) = spval
!#py                 this%cellclay_col(c,lev) = spval
!#py                 this%cellgrvl_col(c,lev) = spval
!#py                 this%cellorg_col(c,lev)  = spval
!#py              end if
!#py           end do
!#py 
!#py           do lev = 1,nlevgrnd
!#py              this%tkmg_col(c,lev)   = spval
!#py              this%tksatu_col(c,lev) = spval
!#py              this%tkdry_col(c,lev)  = spval
!#py              if (lun_pp%itype(l)==istwet .and. lev > nlevbed) then
!#py                 this%csol_col(c,lev) = csol_bedrock
!#py              else
!#py                 this%csol_col(c,lev)= spval
!#py              endif
!#py           end do
!#py 
!#py        else if (lun_pp%urbpoi(l) .and. (col_pp%itype(c) /= icol_road_perv) .and. (col_pp%itype(c) /= icol_road_imperv) )then
!#py 
!#py           ! Urban Roof, sunwall, shadewall properties set to special value
!#py           do lev = 1,nlevgrnd
!#py              this%watsat_col(c,lev) = spval
!#py              this%watfc_col(c,lev)  = spval
!#py              this%watmin_col(c,lev) = spval
!#py              this%bsw_col(c,lev)    = spval
!#py              this%hksat_col(c,lev)  = spval
!#py              this%sucsat_col(c,lev) = spval
!#py              this%sucmin_col(c,lev) = spval
!#py              this%watdry_col(c,lev) = spval
!#py              this%watopt_col(c,lev) = spval
!#py              this%bd_col(c,lev) = spval
!#py              if (lev <= nlevsoi) then
!#py                 this%cellsand_col(c,lev) = spval
!#py                 this%cellclay_col(c,lev) = spval
!#py                 this%cellgrvl_col(c,lev) = spval
!#py                 this%cellorg_col(c,lev)  = spval
!#py              end if
!#py           end do
!#py 
!#py           do lev = 1,nlevgrnd
!#py              this%tkmg_col(c,lev)   = spval
!#py              this%tksatu_col(c,lev) = spval
!#py              this%tkdry_col(c,lev)  = spval
!#py              this%csol_col(c,lev)   = spval
!#py           end do
!#py 
!#py        else
!#py 
!#py           do lev = 1,nlevgrnd
!#py              ! Number of soil layers in hydrologically active columns = NLEV2BED
!#py 	     nlevbed = col_pp%nlevbed(c)
!#py              if ( more_vertlayers )then ! duplicate clay and sand values from last soil layer
!#py 
!#py                 if (lev .eq. 1) then
!#py                    clay = clay3d(g,1)
!#py                    sand = sand3d(g,1)
!#py                    gravel = grvl3d(g,1)
!#py                    om_frac = organic3d(g,1)/organic_max
!#py                 else if (lev <= nlevsoi) then
!#py                    do j = 1,nlevsoifl-1
!#py                       if (zisoi(lev) >= zisoifl(j) .AND. zisoi(lev) < zisoifl(j+1)) then
!#py                          clay = clay3d(g,j+1)
!#py                          sand = sand3d(g,j+1)
!#py                          gravel = grvl3d(g,j+1)
!#py                          om_frac = organic3d(g,j+1)/organic_max
!#py                       endif
!#py                    end do
!#py                 else
!#py                    clay = clay3d(g,nlevsoifl)
!#py                    sand = sand3d(g,nlevsoifl)
!#py                    gravel = grvl3d(g,nlevsoifl)
!#py                    om_frac = 0._r8
!#py                 endif
!#py              else
!#py                 if (lev <= nlevsoi) then ! duplicate clay and sand values from 10th soil layer
!#py                    clay = clay3d(g,lev)
!#py                    sand = sand3d(g,lev)
!#py                    gravel = grvl3d(g,lev)
!#py                    om_frac = (organic3d(g,lev)/organic_max)**2._r8
!#py                 else
!#py                    clay = clay3d(g,nlevsoi)
!#py                    sand = sand3d(g,nlevsoi)
!#py                    gravel = grvl3d(g,nlevsoi)
!#py                    om_frac = 0._r8
!#py                 endif
!#py              end if
!#py 
!#py              if (lun_pp%itype(l) == istdlak) then
!#py 
!#py                 if (lev <= nlevsoi) then
!#py                    this%cellsand_col(c,lev) = sand
!#py                    this%cellclay_col(c,lev) = clay
!#py                    this%cellgrvl_col(c,lev) = gravel
!#py                    this%cellorg_col(c,lev)  = om_frac*organic_max
!#py                 end if
!#py 
!#py              else if (lun_pp%itype(l) /= istdlak) then  ! soil columns of both urban and non-urban types
!#py 
!#py                 if (lun_pp%urbpoi(l)) then
!#py                    om_frac = 0._r8 ! No organic matter for urban
!#py                 end if
!#py 
!#py                 if (lev <= nlevbed) then
!#py                    this%cellsand_col(c,lev) = sand
!#py                    this%cellclay_col(c,lev) = clay
!#py                    this%cellgrvl_col(c,lev) = gravel
!#py                    this%cellorg_col(c,lev)  = om_frac*organic_max
!#py                 end if
!#py 
!#py                 ! Note that the following properties are overwritten for urban impervious road
!#py                 ! layers that are not soil in SoilThermProp.F90 within SoilTemperatureMod.F90
!#py 
!#py                 !determine the type of pedotransfer function to be used based on soil order
!#py                 !I will use the following implementation to further explore the ET problem, now
!#py                 !I set soil order to 0 for all soils. Jinyun Tang, Mar 20, 2014
!#py 
!#py                 ipedof=get_ipedof(0)
!#py                 call pedotransf(ipedof, sand, clay, &
!#py                      this%watsat_col(c,lev), this%bsw_col(c,lev), this%sucsat_col(c,lev), xksat)
!#py 
!#py                 om_watsat         = max(0.93_r8 - 0.1_r8   *(zsoi(lev)/zsapric), 0.83_r8)
!#py                 om_b              = min(2.7_r8  + 9.3_r8   *(zsoi(lev)/zsapric), 12.0_r8)
!#py                 om_sucsat         = min(10.3_r8 - 0.2_r8   *(zsoi(lev)/zsapric), 10.1_r8)
!#py                 om_hksat          = max(0.28_r8 - 0.2799_r8*(zsoi(lev)/zsapric), 0.0001_r8)
!#py 
!#py                 this%bd_col(c,lev)        = (1._r8 - this%watsat_col(c,lev))*2.7e3_r8
!#py                 this%watsat_col(c,lev)    = (1._r8 - om_frac) * this%watsat_col(c,lev) + om_watsat*om_frac
!#py                 tkm                       = (1._r8-om_frac) * (8.80_r8*sand+2.92_r8*clay)/(sand+clay)+om_tkm*om_frac ! W/(m K)
!#py                 this%bsw_col(c,lev)       = (1._r8-om_frac) * (2.91_r8 + 0.159_r8*clay) + om_frac*om_b
!#py                 this%sucsat_col(c,lev)    = (1._r8-om_frac) * this%sucsat_col(c,lev) + om_sucsat*om_frac
!#py                 this%hksat_min_col(c,lev) = xksat
!#py 
!#py                 ! perc_frac is zero unless perf_frac greater than percolation threshold
!#py                 if (om_frac > pcalpha) then
!#py                    perc_norm=(1._r8 - pcalpha)**(-pcbeta)
!#py                    perc_frac=perc_norm*(om_frac - pcalpha)**pcbeta
!#py                 else
!#py                    perc_frac=0._r8
!#py                 endif
!#py 
!#py                 ! uncon_frac is fraction of mineral soil plus fraction of "nonpercolating" organic soil
!#py                 uncon_frac=(1._r8-om_frac)+(1._r8-perc_frac)*om_frac
!#py 
!#py                 ! uncon_hksat is series addition of mineral/organic conductivites
!#py                 if (om_frac < 1._r8) then
!#py                    uncon_hksat=uncon_frac/((1._r8-om_frac)/xksat &
!#py                         +((1._r8-perc_frac)*om_frac)/om_hksat)
!#py                 else
!#py                    uncon_hksat = 0._r8
!#py                 end if
!#py                 this%hksat_col(c,lev)  = uncon_frac*uncon_hksat + (perc_frac*om_frac)*om_hksat
!#py 
!#py                 this%tkmg_col(c,lev)   = tkm ** (1._r8- this%watsat_col(c,lev))
!#py 
!#py                 this%tksatu_col(c,lev) = this%tkmg_col(c,lev)*0.57_r8**this%watsat_col(c,lev)
!#py 
!#py                 this%tkdry_col(c,lev)  = ((0.135_r8*this%bd_col(c,lev) + 64.7_r8) / &
!#py                      (2.7e3_r8 - 0.947_r8*this%bd_col(c,lev)))*(1._r8-om_frac) + om_tkd*om_frac
!#py 
!#py                 this%csol_col(c,lev)   = ((1._r8-om_frac)*(2.128_r8*sand+2.385_r8*clay) / (sand+clay) + &
!#py                      om_csol*om_frac)*1.e6_r8  ! J/(m3 K)
!#py 
!#py                 if (lev > nlevbed) then
!#py                    this%csol_col(c,lev) = csol_bedrock
!#py                 endif
!#py 
!#py                 this%watdry_col(c,lev) = this%watsat_col(c,lev) * &
!#py                      (316230._r8/this%sucsat_col(c,lev)) ** (-1._r8/this%bsw_col(c,lev))
!#py                 this%watopt_col(c,lev) = this%watsat_col(c,lev) * &
!#py                      (158490._r8/this%sucsat_col(c,lev)) ** (-1._r8/this%bsw_col(c,lev))
!#py 
!#py                 !! added by K.Sakaguchi for beta from Lee and Pielke, 1992
!#py                 ! water content at field capacity, defined as hk = 0.1 mm/day
!#py                 ! used eqn (7.70) in CLM3 technote with k = 0.1 (mm/day) / secspday (day/sec)
!#py                 this%watfc_col(c,lev) = this%watsat_col(c,lev) * &
!#py                      (0.1_r8 / (this%hksat_col(c,lev)*secspday))**(1._r8/(2._r8*this%bsw_col(c,lev)+3._r8))
!#py 
!#py                 this%sucmin_col(c,lev) = min_liquid_pressure
!#py 
!#py                 this%watmin_col(c,lev) = &
!#py                      this%watsat_col(c,lev)*(-min_liquid_pressure/this%sucsat_col(c,lev))**(-1._r8/this%bsw_col(c,lev))
!#py 
!#py              end if
!#py           end do
!#py 
!#py           ! Urban pervious and impervious road
!#py           if (col_pp%itype(c) == icol_road_imperv) then
!#py              ! Impervious road layers -- same as above except set watdry and watopt as missing
!#py              do lev = 1,nlevgrnd
!#py                 this%watdry_col(c,lev) = spval
!#py                 this%watopt_col(c,lev) = spval
!#py              end do
!#py           else if (col_pp%itype(c) == icol_road_perv) then
!#py              ! pervious road layers  - set in UrbanInitTimeConst
!#py           end if
!#py 
!#py        end if
!#py     end do
!#py 
!#py     ! --------------------------------------------------------------------
!#py     ! Set soil hydraulic and thermal properties: lake
!#py     ! --------------------------------------------------------------------
!#py 
!#py     do c = bounds%begc, bounds%endc
!#py        g = col_pp%gridcell(c)
!#py        l = col_pp%landunit(c)
!#py 
!#py        if (lun_pp%itype(l)==istdlak) then
!#py 
!#py           do lev = 1,nlevgrnd
!#py              if ( lev <= nlevsoi )then
!#py                 clay    =  this%cellclay_col(c,lev)
!#py                 sand    =  this%cellsand_col(c,lev)
!#py                 gravel  =  this%cellgrvl_col(c,lev)
!#py                 om_frac = (this%cellorg_col(c,lev)/organic_max)**2._r8
!#py              else
!#py                 clay    = this%cellclay_col(c,nlevsoi)
!#py                 sand    = this%cellsand_col(c,nlevsoi)
!#py                 gravel  = this%cellgrvl_col(c,nlevsoi)
!#py                 om_frac = 0.0_r8
!#py              end if
!#py 
!#py              this%watsat_col(c,lev) = 0.489_r8 - 0.00126_r8*sand
!#py              this%bsw_col(c,lev)    = 2.91 + 0.159*clay
!#py              this%sucsat_col(c,lev) = 10._r8 * ( 10._r8**(1.88_r8-0.0131_r8*sand) )
!#py              bd                     = (1._r8-this%watsat_col(c,lev))*2.7e3_r8
!#py              this%watsat_col(c,lev) = (1._r8 - om_frac)*this%watsat_col(c,lev) + om_watsat_lake * om_frac
!#py              tkm                    = (1._r8-om_frac)*(8.80_r8*sand+2.92_r8*clay)/(sand+clay) + om_tkm * om_frac ! W/(m K)
!#py              this%bsw_col(c,lev)    = (1._r8-om_frac)*(2.91_r8 + 0.159_r8*clay) + om_frac * om_b_lake
!#py              this%sucsat_col(c,lev) = (1._r8-om_frac)*this%sucsat_col(c,lev) + om_sucsat_lake * om_frac
!#py              xksat                  = 0.0070556 *( 10.**(-0.884+0.0153*sand) ) ! mm/s
!#py 
!#py              ! perc_frac is zero unless perf_frac greater than percolation threshold
!#py              if (om_frac > pc_lake) then
!#py                 perc_norm = (1._r8 - pc_lake)**(-pcbeta)
!#py                 perc_frac = perc_norm*(om_frac - pc_lake)**pcbeta
!#py              else
!#py                 perc_frac = 0._r8
!#py              endif
!#py 
!#py              ! uncon_frac is fraction of mineral soil plus fraction of "nonpercolating" organic soil
!#py              uncon_frac = (1._r8-om_frac) + (1._r8-perc_frac)*om_frac
!#py 
!#py              ! uncon_hksat is series addition of mineral/organic conductivites
!#py              if (om_frac < 1._r8) then
!#py                 xksat = 0.0070556 *( 10.**(-0.884+0.0153*sand) ) ! mm/s
!#py                 uncon_hksat = uncon_frac/((1._r8-om_frac)/xksat + ((1._r8-perc_frac)*om_frac)/om_hksat_lake)
!#py              else
!#py                 uncon_hksat = 0._r8
!#py              end if
!#py 
!#py              this%hksat_col(c,lev)  = uncon_frac*uncon_hksat + (perc_frac*om_frac)*om_hksat_lake
!#py              this%tkmg_col(c,lev)   = tkm ** (1._r8- this%watsat_col(c,lev))
!#py              this%tksatu_col(c,lev) = this%tkmg_col(c,lev)*0.57_r8**this%watsat_col(c,lev)
!#py              this%tkdry_col(c,lev)  = ((0.135_r8*bd + 64.7_r8) / (2.7e3_r8 - 0.947_r8*bd))*(1._r8-om_frac) + &
!#py                                        om_tkd * om_frac
!#py              this%csol_col(c,lev)   = ((1._r8-om_frac)*(2.128_r8*sand+2.385_r8*clay) / (sand+clay) +   &
!#py                                        om_csol * om_frac)*1.e6_r8  ! J/(m3 K)
!#py              if (lev > nlevsoi) then
!#py                 this%csol_col(c,lev) = csol_bedrock
!#py              endif
!#py 
!#py              this%watdry_col(c,lev) = this%watsat_col(c,lev) * (316230._r8/this%sucsat_col(c,lev)) ** (-1._r8/this%bsw_col(c,lev))
!#py              this%watopt_col(c,lev) = this%watsat_col(c,lev) * (158490._r8/this%sucsat_col(c,lev)) ** (-1._r8/this%bsw_col(c,lev))
!#py 
!#py              !! added by K.Sakaguchi for beta from Lee and Pielke, 1992
!#py              ! water content at field capacity, defined as hk = 0.1 mm/day
!#py              ! used eqn (7.70) in CLM3 technote with k = 0.1 (mm/day) / (# seconds/day)
!#py              this%watfc_col(c,lev) = this%watsat_col(c,lev) * (0.1_r8 / &
!#py                                (this%hksat_col(c,lev)*secspday))**(1._r8/(2._r8*this%bsw_col(c,lev)+3._r8))
!#py           end do
!#py        endif
!#py 
!#py     end do
!#py 
!#py     ! --------------------------------------------------------------------
!#py     ! Initialize threshold soil moisture and mass fracion of clay limited to 0.20
!#py     ! --------------------------------------------------------------------
!#py 
!#py     do c = bounds%begc, bounds%endc
!#py        g = col_pp%gridcell(c)
!#py 
!#py        this%gwc_thr_col(c) = 0.17_r8 + 0.14_r8 * clay3d(g,1) * 0.01_r8
!#py        this%mss_frc_cly_vld_col(c) = min(clay3d(g,1) * 0.01_r8, 0.20_r8)
!#py     end do
!#py 
!#py     ! --------------------------------------------------------------------
!#py     ! Deallocate memory
!#py     ! --------------------------------------------------------------------
!#py 
!#py     deallocate(sand3d, clay3d, grvl3d, organic3d)
!#py     deallocate(zisoifl, zsoifl, dzsoifl)
!#py 
!#py   end subroutine InitCold

  !------------------------------------------------------------------------
!#py   subroutine Restart(this, bounds, ncid, flag)
!#py     !
!#py     ! !USES:
!#py     !#py use shr_log_mod, only : errMsg => shr_log_errMsg
!#py     !#py use spmdMod    , only : masterproc
!#py     !#py use abortutils , only : endrun
!#py     !#py use restUtilMod
!#py     !#py use ncdio_pio
!#py     use elm_varctl,  only : use_dynroot
!#py     use elm_varctl,  only : use_hydrstress
!#py     use RootBiophysMod      , only : init_vegrootfr
!#py     !
!#py     ! !ARGUMENTS:
!#py     class(soilstate_type) :: this
!#py     type(bounds_type), intent(in)    :: bounds
!#py     type(file_desc_t), intent(inout) :: ncid
!#py     character(len=*) , intent(in)    :: flag
!#py     !
!#py     ! !LOCAL VARIABLES:
!#py     logical          :: readvar   ! determine if variable is on initial file
!#py     logical          :: readrootfr = .false.
!#py     !-----------------------------------------------------------------------
!#py     if(use_hydrstress) then
!#py        call restartvar(ncid=ncid, flag=flag, varname='SMP', xtype=ncd_double,  &
!#py             dim1name='column', dim2name='levgrnd', switchdim=.true., &
!#py             long_name='soil matric potential', units='mm', &
!#py             interpinic_flag='interp', readvar=readvar, data=this%smp_l_col)
!#py 
!#py        call restartvar(ncid=ncid, flag=flag, varname='HK', xtype=ncd_double,  &
!#py             dim1name='column', dim2name='levgrnd', switchdim=.true., &
!#py             long_name='hydraulic conductivity', units='mm/s', &
!#py             interpinic_flag='interp', readvar=readvar, data=this%hk_l_col)
!#py     endif
!#py     if(use_dynroot) then
!#py        call restartvar(ncid=ncid, flag=flag, varname='root_depth', xtype=ncd_double,  &
!#py             dim1name='pft', &
!#py             long_name='root depth', units='m', &
!#py             interpinic_flag='interp', readvar=readvar, data=this%root_depth_patch)
!#py 
!#py        call restartvar(ncid=ncid, flag=flag, varname='rootfr', xtype=ncd_double,  &
!#py             dim1name='pft', dim2name='levgrnd', switchdim=.true., &
!#py             long_name='root fraction', units='', &
!#py             interpinic_flag='interp', readvar=readrootfr, data=this%rootfr_patch)
!#py     else
!#py        readrootfr = .false.
!#py     end if
!#py     if (flag=='read' .and. .not. readrootfr) then
!#py        if (masterproc) then
!#py           write(iulog,*) "can't find rootfr in restart (or initial) file..."
!#py           write(iulog,*) "Initialize rootfr to default"
!#py        end if
!#py        call init_vegrootfr(bounds, nlevsoi, nlevgrnd, &
!#py             col_pp%nlevbed(bounds%begc:bounds%endc), &
!#py             this%rootfr_patch(bounds%begp:bounds%endp,1:nlevgrnd))
!#py     end if
!#py !#py !#py   end subroutine Restart



end module SoilStateType
