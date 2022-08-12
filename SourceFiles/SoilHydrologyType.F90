Module SoilHydrologyType

  use shr_kind_mod          , only : r8 => shr_kind_r8
  !#py use shr_log_mod           , only : errMsg => shr_log_errMsg
  use decompMod             , only : bounds_type
  !#py use spmdMod               , only : masterproc, mpicom
  !#py use abortutils            , only : endrun
  use elm_varpar            , only : nlevgrnd, nlayer, nlayert, nlevsoi
  use elm_varpar            , only : more_vertlayers, nlevsoifl, toplev_equalspace
  use elm_varcon            , only : zsoi, dzsoi, zisoi, spval
  use elm_varctl            , only : iulog
  use SharedParamsMod     , only : ParamsShareInst
  use LandunitType          , only : lun_pp
  use ColumnType            , only : col_pp
  !
  ! !PUBLIC TYPES:
  implicit none
  private
  save
  !
  ! !PRIVATE MEMBER FUNCTIONS:
  private :: initSoilParVIC    ! Convert default elm soil properties to VIC parameters
  private :: initELMVICMap     ! Initialize map from VIC to elm layers
  private :: linear_interp     ! function for linear interperation
  !
  type, public :: soilhydrology_type

     integer , pointer :: h2osfcflag => null()    ! true => surface water is active (namelist)
     integer , pointer :: origflag   => null()    ! used to control soil hydrology properties (namelist)

     ! NON-VIC
     real(r8), pointer :: frost_table_col   (:)    => null() ! col frost table depth
     real(r8), pointer :: zwt_col           (:)    => null() ! col water table depth
     real(r8), pointer :: zwts_col          (:)    => null() ! col water table depth, the shallower of the two water depths
     real(r8), pointer :: zwt_perched_col   (:)    => null() ! col perched water table depth
     real(r8), pointer :: wa_col            (:)    => null() ! col water in the unconfined aquifer (mm)
     real(r8), pointer :: beg_wa_grc        (:)    => null() ! grid-level water in the unconfined aquifer at beginning of the time step (mm)
     real(r8), pointer :: end_wa_grc        (:)    => null() ! grid-level water in the unconfined aquifer at end of the time step (mm)
     real(r8), pointer :: qflx_bot_col      (:)    => null()
     real(r8), pointer :: qcharge_col       (:)    => null() ! col aquifer recharge rate (mm/s)
     real(r8), pointer :: fracice_col       (:,:)  => null() ! col fractional impermeability (-)
     real(r8), pointer :: icefrac_col       (:,:)  => null() ! col fraction of ice
     real(r8), pointer :: fcov_col          (:)    => null() ! col fractional impermeable area
     real(r8), pointer :: fsat_col          (:)    => null() ! col fractional area with water table at surface
     real(r8), pointer :: h2osfc_thresh_col (:)    => null() ! col level at which h2osfc "percolates"   (time constant)

     ! VIC
     real(r8), pointer :: hkdepth_col       (:)     => null()! col VIC decay factor (m) (time constant)
     real(r8), pointer :: b_infil_col       (:)     => null()! col VIC b infiltration parameter (time constant)
     real(r8), pointer :: ds_col            (:)     => null()! col VIC fracton of Dsmax where non-linear baseflow begins (time constant)
     real(r8), pointer :: dsmax_col         (:)     => null()! col VIC max. velocity of baseflow (mm/day) (time constant)
     real(r8), pointer :: Wsvic_col         (:)     => null()! col VIC fraction of maximum soil moisutre where non-liear base flow occurs (time constant)
     real(r8), pointer :: porosity_col      (:,:)   => null()! col VIC porosity (1-bulk_density/soil_density)
     real(r8), pointer :: vic_elm_fract_col (:,:,:) => null()! col VIC fraction of VIC layers in elm layers
     real(r8), pointer :: depth_col         (:,:)   => null()! col VIC layer depth of upper layer
     real(r8), pointer :: c_param_col       (:)     => null()! col VIC baseflow exponent (Qb)
     real(r8), pointer :: expt_col          (:,:)   => null()! col VIC pore-size distribution related paramter(Q12)
     real(r8), pointer :: ksat_col          (:,:)   => null()! col VIC Saturated hydrologic conductivity
     real(r8), pointer :: phi_s_col         (:,:)   => null()! col VIC soil moisture dissusion parameter
     real(r8), pointer :: moist_col         (:,:)   => null()! col VIC soil moisture (kg/m2) for VIC soil layers
     real(r8), pointer :: moist_vol_col     (:,:)   => null()! col VIC volumetric soil moisture for VIC soil layers
     real(r8), pointer :: max_moist_col     (:,:)   => null()! col VIC max layer moist + ice (mm)
     real(r8), pointer :: max_infil_col     (:)     => null()! col VIC maximum infiltration rate calculated in VIC
     real(r8), pointer :: i_0_col           (:)     => null()! col VIC average saturation in top soil layers
     real(r8), pointer :: ice_col           (:,:)   => null()! col VIC soil ice (kg/m2) for VIC soil layers

   contains

     procedure, public  :: Init
     procedure, private :: ReadNL
     procedure, public :: InitAllocate
     procedure, private :: InitHistory
     !#py procedure, private :: InitCold
     !#py procedure, public  :: Restart

  end type soilhydrology_type

  !-----------------------------------------------------------------------

contains

  !------------------------------------------------------------------------
  subroutine Init(this, bounds )

    class(soilhydrology_type) :: this
    type(bounds_type), intent(in)    :: bounds

    call this%ReadNL()
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
    ! !USES:
    !use shr_infnan_mod , only : nan => shr_infnan_nan, assignment(=)
    use elm_varpar     , only : nlevsno, nlevgrnd
    !
    ! !ARGUMENTS:
    class(soilhydrology_type) :: this
    type(bounds_type), intent(in) :: bounds
    !
    ! !LOCAL VARIABLES:
    integer :: begp, endp
    integer :: begc, endc
    integer :: begg, endg
    !------------------------------------------------------------------------

    begp = bounds%begp; endp= bounds%endp
    begc = bounds%begc; endc= bounds%endc
    begg = bounds%begg; endg= bounds%endg


    allocate(this%frost_table_col   (begc:endc))                 ; this%frost_table_col   (:)     = spval
    allocate(this%zwt_col           (begc:endc))                 ; this%zwt_col           (:)     = spval
    allocate(this%qflx_bot_col      (begc:endc))                 ; this%qflx_bot_col      (:)     = spval
    allocate(this%zwt_perched_col   (begc:endc))                 ; this%zwt_perched_col   (:)     = spval
    allocate(this%zwts_col          (begc:endc))                 ; this%zwts_col          (:)     = spval

    allocate(this%wa_col            (begc:endc))                 ; this%wa_col            (:)     = spval
    allocate(this%beg_wa_grc        (begg:endg))                 ; this%beg_wa_grc        (:)     = spval
    allocate(this%end_wa_grc        (begg:endg))                 ; this%end_wa_grc        (:)     = spval
    allocate(this%qcharge_col       (begc:endc))                 ; this%qcharge_col       (:)     = spval
    allocate(this%fracice_col       (begc:endc,nlevgrnd))        ; this%fracice_col       (:,:)   = spval
    allocate(this%icefrac_col       (begc:endc,nlevgrnd))        ; this%icefrac_col       (:,:)   = spval
    allocate(this%fcov_col          (begc:endc))                 ; this%fcov_col          (:)     = spval
    allocate(this%fsat_col          (begc:endc))                 ; this%fsat_col          (:)     = spval
    allocate(this%h2osfc_thresh_col (begc:endc))                 ; this%h2osfc_thresh_col (:)     = spval

    allocate(this%hkdepth_col       (begc:endc))                 ; this%hkdepth_col       (:)     = spval
    allocate(this%b_infil_col       (begc:endc))                 ; this%b_infil_col       (:)     = spval
    allocate(this%ds_col            (begc:endc))                 ; this%ds_col            (:)     = spval
    allocate(this%dsmax_col         (begc:endc))                 ; this%dsmax_col         (:)     = spval
    allocate(this%Wsvic_col         (begc:endc))                 ; this%Wsvic_col         (:)     = spval
    allocate(this%depth_col         (begc:endc,nlayert))         ; this%depth_col         (:,:)   = spval
    allocate(this%porosity_col      (begc:endc,nlayer))          ; this%porosity_col      (:,:)   = spval
    allocate(this%vic_elm_fract_col (begc:endc,nlayer, nlevsoi)) ; this%vic_elm_fract_col (:,:,:) = spval
    allocate(this%c_param_col       (begc:endc))                 ; this%c_param_col       (:)     = spval
    allocate(this%expt_col          (begc:endc,nlayer))          ; this%expt_col          (:,:)   = spval
    allocate(this%ksat_col          (begc:endc,nlayer))          ; this%ksat_col          (:,:)   = spval
    allocate(this%phi_s_col         (begc:endc,nlayer))          ; this%phi_s_col         (:,:)   = spval
    allocate(this%moist_col         (begc:endc,nlayert))         ; this%moist_col         (:,:)   = spval
    allocate(this%moist_vol_col     (begc:endc,nlayert))         ; this%moist_vol_col     (:,:)   = spval
    allocate(this%max_moist_col     (begc:endc,nlayer))          ; this%max_moist_col     (:,:)   = spval
    allocate(this%max_infil_col     (begc:endc))                 ; this%max_infil_col     (:)     = spval
    allocate(this%i_0_col           (begc:endc))                 ; this%i_0_col           (:)     = spval
    allocate(this%ice_col           (begc:endc,nlayert))         ; this%ice_col           (:,:)   = spval

  end subroutine InitAllocate

  !------------------------------------------------------------------------
  subroutine InitHistory(this, bounds)
    !
    ! !USES:
    !#py use shr_infnan_mod , only : nan => shr_infnan_nan, assignment(=)
    use elm_varctl     , only : create_glacier_mec_landunit, use_cn, use_lch4
    use elm_varpar     , only : nlevsno, crop_prog
    !#py use histFileMod    , only : hist_addfld1d, hist_addfld2d, no_snow_normal
    !
    ! !ARGUMENTS:
    class(soilhydrology_type) :: this
    type(bounds_type), intent(in) :: bounds
    !
    ! !LOCAL VARIABLES:
    integer           :: begp, endp
    integer           :: begc, endc
    integer           :: begg, endg
    !------------------------------------------------------------------------

    begp = bounds%begp; endp= bounds%endp
    begc = bounds%begc; endc= bounds%endc
    begg = bounds%begg; endg= bounds%endg

    this%wa_col(begc:endc) = spval
    !#py call hist_addfld1d (fname='WA',  units='mm',  &
         !#py avgflag='A', long_name='water in the unconfined aquifer (vegetated landunits only)', &
         !#py !#py ptr_col=this%wa_col, l2g_scale_type='veg')

    this%qcharge_col(begc:endc) = spval
    !#py call hist_addfld1d (fname='QCHARGE',  units='mm/s',  &
         !#py avgflag='A', long_name='aquifer recharge rate (vegetated landunits only)', &
         !#py !#py ptr_col=this%qcharge_col, l2g_scale_type='veg')

    this%fcov_col(begc:endc) = spval
    !#py call hist_addfld1d (fname='FCOV',  units='unitless',  &
         !#py avgflag='A', long_name='fractional impermeable area', &
         !#py !#py ptr_col=this%fcov_col, l2g_scale_type='veg')

    this%fsat_col(begc:endc) = spval
    !#py call hist_addfld1d (fname='FSAT',  units='unitless',  &
         !#py avgflag='A', long_name='fractional area with water table at surface', &
         !#py !#py ptr_col=this%fsat_col, l2g_scale_type='veg')

    this%frost_table_col(begc:endc) = spval
    !#py call hist_addfld1d (fname='FROST_TABLE',  units='m',  &
         !#py avgflag='A', long_name='frost table depth (vegetated landunits only)', &
         !#py !#py ptr_col=this%frost_table_col, l2g_scale_type='veg')

    this%zwt_col(begc:endc) = spval
    !#py call hist_addfld1d (fname='ZWT',  units='m',  &
         !#py avgflag='A', long_name='water table depth (vegetated landunits only)', &
         !#py !#py ptr_col=this%zwt_col, l2g_scale_type='veg')

    this%zwt_perched_col(begc:endc) = spval
    !#py call hist_addfld1d (fname='ZWT_PERCH',  units='m',  &
         !#py avgflag='A', long_name='perched water table depth (vegetated landunits only)', &
         !#py !#py ptr_col=this%zwt_perched_col, l2g_scale_type='veg')

  end subroutine InitHistory

  !-----------------------------------------------------------------------
!#py   subroutine InitCold(this, bounds)
!#py     !
!#py     ! !DESCRIPTION:
!#py     ! Initialize time constant variables and cold start conditions
!#py     !
!#py     ! !USES:
!#py     use shr_const_mod   , only : shr_const_pi, SHR_CONST_TKFRZ
!#py     !#py use shr_log_mod     , only : errMsg => shr_log_errMsg
!#py     use shr_spfn_mod    , only : shr_spfn_erf
!#py     use shr_kind_mod    , only : r8 => shr_kind_r8
!#py     use elm_varctl      , only : fsurdat, iulog, use_vichydro, use_var_soil_thick
!#py     use elm_varpar      , only : nlevsoi, nlevgrnd, nlevsno, nlevlak, nlevurb
!#py     use elm_varcon      , only : denice, denh2o, sb, bdsno
!#py     use elm_varcon      , only : h2osno_max, zlnd, tfrz, spval, pc
!#py     use elm_varcon      , only : nlvic, dzvic, pc, mu, grlnd
!#py     use landunit_varcon , only : istice, istwet, istsoil, istdlak, istcrop, istice_mec
!#py     use column_varcon   , only : icol_shadewall, icol_road_perv
!#py     use column_varcon   , only : icol_road_imperv, icol_roof, icol_sunwall
!#py     !#py use fileutils       , only : getfil
!#py     use organicFileMod  , only : organicrd
!#py     !#py use ncdio_pio       , only : file_desc_t, ncd_io, ncd_pio_openfile, ncd_pio_closefile
!#py     !
!#py     ! !ARGUMENTS:
!#py     class(soilhydrology_type) :: this
!#py     type(bounds_type) , intent(in)    :: bounds
!#py     !
!#py     ! !LOCAL VARIABLES:
!#py     integer            :: p,c,j,l,g,lev,nlevs
!#py     integer            :: nlevbed
!#py     integer            :: ivic,ivicstrt,ivicend
!#py     real(r8)           :: maxslope, slopemax, minslope
!#py     real(r8)           :: d, fd, dfdd, slope0,slopebeta
!#py     real(r8) ,pointer  :: tslope(:)
!#py     logical            :: readvar
!#py     type(file_desc_t)  :: ncid
!#py     character(len=256) :: locfn
!#py     real(r8)           :: clay,sand        ! temporaries
!#py     real(r8)           :: om_frac          ! organic matter fraction
!#py     real(r8)           :: organic_max      ! organic matter (kg/m3) where soil is assumed to act like peat
!#py     real(r8) ,pointer  :: b2d        (:)   ! read in - VIC b
!#py     real(r8) ,pointer  :: ds2d       (:)   ! read in - VIC Ds
!#py     real(r8) ,pointer  :: dsmax2d    (:)   ! read in - VIC Dsmax
!#py     real(r8) ,pointer  :: ws2d       (:)   ! read in - VIC Ws
!#py     real(r8), pointer  :: sandcol    (:,:) ! column level sand fraction for calculating VIC parameters
!#py     real(r8), pointer  :: claycol    (:,:) ! column level clay fraction for calculating VIC parameters
!#py     real(r8), pointer  :: om_fraccol (:,:) ! column level organic matter fraction for calculating VIC parameters
!#py     real(r8) ,pointer  :: sand3d     (:,:) ! read in - soil texture: percent sand
!#py     real(r8) ,pointer  :: clay3d     (:,:) ! read in - soil texture: percent clay
!#py     real(r8) ,pointer  :: organic3d  (:,:) ! read in - organic matter: kg/m3
!#py     real(r8) ,pointer  :: zisoifl    (:)   ! original soil interface depth
!#py     real(r8) ,pointer  :: zsoifl     (:)   ! original soil midpoint
!#py     real(r8) ,pointer  :: dzsoifl    (:)   ! original soil thickness
!#py     real(r8) ,pointer  :: fdrain     (:)   ! top-model drainage parameter
!#py     !-----------------------------------------------------------------------
!#py 
!#py     ! -----------------------------------------------------------------
!#py     ! Initialize frost table
!#py     ! -----------------------------------------------------------------
!#py 
!#py     this%wa_col(bounds%begc:bounds%endc)  = 5000._r8
!#py     this%zwt_col(bounds%begc:bounds%endc) = 0._r8
!#py 
!#py     if (use_var_soil_thick) then
!#py        do c = bounds%begc,bounds%endc
!#py           l = col_pp%landunit(c)
!#py           nlevbed = col_pp%nlevbed(c)
!#py           if (.not. lun_pp%lakpoi(l)) then  !not lake
!#py              if (lun_pp%urbpoi(l)) then
!#py                 if (col_pp%itype(c) == icol_road_perv) then
!#py                    this%wa_col(c)  = 0._r8
!#py                    this%zwt_col(c) = col_pp%zi(c,nlevbed)  ! At bedrock depth for variable soil thickness
!#py                 else
!#py                    this%wa_col(c)  = spval
!#py                    this%zwt_col(c) = spval
!#py                 end if
!#py                 ! initialize frost_table, zwt_perched
!#py                 this%zwt_perched_col(c) = spval
!#py                 this%frost_table_col(c) = spval
!#py              else
!#py                 this%wa_col(c)  = 0._r8
!#py                 this%zwt_col(c) = col_pp%zi(c,nlevbed)  ! At bedrock depth for variable soil thickness
!#py                 ! initialize frost_table, zwt_perched to bottom of soil column
!#py                 this%zwt_perched_col(c) = col_pp%zi(c,nlevbed)
!#py                 this%frost_table_col(c) = col_pp%zi(c,nlevbed)
!#py              end if
!#py           end if
!#py        end do
!#py     else
!#py        do c = bounds%begc,bounds%endc
!#py           l = col_pp%landunit(c)
!#py           nlevbed = col_pp%nlevbed(c)
!#py           if (.not. lun_pp%lakpoi(l)) then  !not lake
!#py              if (lun_pp%urbpoi(l)) then
!#py                 if (col_pp%itype(c) == icol_road_perv) then
!#py                    this%wa_col(c)  = 4800._r8
!#py                    this%zwt_col(c) = (25._r8 + col_pp%zi(c,nlevsoi)) - this%wa_col(c)/0.2_r8 /1000._r8  ! One meter below soil column
!#py                 else
!#py                    this%wa_col(c)  = spval
!#py                    this%zwt_col(c) = spval
!#py                 end if
!#py                 ! initialize frost_table, zwt_perched
!#py                 this%zwt_perched_col(c) = spval
!#py                 this%frost_table_col(c) = spval
!#py              else
!#py                 this%wa_col(c)  = 4000._r8
!#py                 this%zwt_col(c) = (25._r8 + col_pp%zi(c,nlevsoi)) - this%wa_col(c)/0.2_r8 /1000._r8  ! One meter below soil column
!#py                 ! initialize frost_table, zwt_perched to bottom of soil column
!#py                 this%zwt_perched_col(c) = col_pp%zi(c,nlevsoi)
!#py                 this%frost_table_col(c) = col_pp%zi(c,nlevsoi)
!#py              end if
!#py           end if
!#py        end do
!#py     end if
!#py 
!#py     ! Initialize VIC variables
!#py 
!#py     if (use_vichydro) then
!#py 
!#py        allocate(b2d        (bounds%begg:bounds%endg))
!#py        allocate(ds2d       (bounds%begg:bounds%endg))
!#py        allocate(dsmax2d    (bounds%begg:bounds%endg))
!#py        allocate(ws2d       (bounds%begg:bounds%endg))
!#py        allocate(sandcol    (bounds%begc:bounds%endc,1:nlevgrnd ))
!#py        allocate(claycol    (bounds%begc:bounds%endc,1:nlevgrnd ))
!#py        allocate(om_fraccol (bounds%begc:bounds%endc,1:nlevgrnd ))
!#py 
!#py        !#py call getfil (fsurdat, locfn, 0)
!#py        !#py call ncd_pio_openfile (ncid, locfn, 0)
!#py        !#py !#py call ncd_io(ncid=ncid, varname='binfl', flag='read', data=b2d, dim1name=grlnd, readvar=readvar)
!#py        if (.not. readvar) then
!#py           !#py !#py call endrun(msg=' ERROR: binfl NOT on surfdata file'//errMsg(__FILE__, __LINE__))
!#py        end if
!#py        !#py !#py call ncd_io(ncid=ncid, varname='Ds', flag='read', data=ds2d, dim1name=grlnd, readvar=readvar)
!#py        if (.not. readvar) then
!#py           !#py !#py call endrun(msg=' ERROR: Ds NOT on surfdata file'//errMsg(__FILE__, __LINE__))
!#py        end if
!#py        !#py !#py call ncd_io(ncid=ncid, varname='Dsmax', flag='read', data=dsmax2d, dim1name=grlnd, readvar=readvar)
!#py        if (.not. readvar) then
!#py           !#py !#py call endrun(msg=' ERROR: Dsmax NOT on surfdata file'//errMsg(__FILE__, __LINE__))
!#py        end if
!#py        !#py !#py call ncd_io(ncid=ncid, varname='Ws', flag='read', data=ws2d, dim1name=grlnd, readvar=readvar)
!#py        if (.not. readvar) then
!#py           !#py !#py call endrun(msg=' ERROR: Ws NOT on surfdata file'//errMsg(__FILE__, __LINE__))
!#py        end if
!#py        !#py call ncd_pio_closefile(ncid)
!#py 
!#py        !define the depth of VIC soil layers here
!#py        nlvic(1) = 3
!#py        nlvic(2) = toplev_equalspace - nlvic(1)
!#py        nlvic(3) = nlevsoi - (nlvic(1) + nlvic(2))
!#py 
!#py        dzvic(:) = 0._r8
!#py        ivicstrt = 1
!#py 
!#py        do ivic = 1,nlayer
!#py           ivicend = ivicstrt+nlvic(ivic)-1
!#py           do j = ivicstrt,ivicend
!#py              dzvic(ivic) = dzvic(ivic)+dzsoi(j)
!#py           end do
!#py           ivicstrt = ivicend+1
!#py        end do
!#py 
!#py        do c = bounds%begc, bounds%endc
!#py           g = col_pp%gridcell(c)
!#py           this%b_infil_col(c) = b2d(g)
!#py           this%ds_col(c)      = ds2d(g)
!#py           this%dsmax_col(c)   = dsmax2d(g)
!#py           this%Wsvic_col(c)   = ws2d(g)
!#py        end do
!#py 
!#py        do c = bounds%begc, bounds%endc
!#py           this%max_infil_col(c) = spval
!#py           this%i_0_col(c)       = spval
!#py           do lev = 1, nlayer
!#py              this%ice_col(c,lev)       = spval
!#py              this%moist_col(c,lev)     = spval
!#py              this%moist_vol_col(c,lev) = spval
!#py              this%max_moist_col(c,lev) = spval
!#py              this%porosity_col(c,lev)  = spval
!#py              this%expt_col(c,lev)      = spval
!#py              this%ksat_col(c,lev)      = spval
!#py              this%phi_s_col(c,lev)     = spval
!#py              this%depth_col(c,lev)     = spval
!#py              sandcol(c,lev)            = spval
!#py              claycol(c,lev)            = spval
!#py              om_fraccol(c,lev)         = spval
!#py           end do
!#py        end do
!#py 
!#py        allocate(sand3d(bounds%begg:bounds%endg,nlevsoifl))
!#py        allocate(clay3d(bounds%begg:bounds%endg,nlevsoifl))
!#py        allocate(organic3d(bounds%begg:bounds%endg,nlevsoifl))
!#py 
!#py        call organicrd(organic3d)
!#py 
!#py        call getfil (fsurdat, locfn, 0)
!#py        call ncd_pio_openfile (ncid, locfn, 0)
!#py        call ncd_io(ncid=ncid, varname='PCT_SAND', flag='read', data=sand3d, dim1name=grlnd, readvar=readvar)
!#py        if (.not. readvar) then
!#py           call endrun(msg=' ERROR: PCT_SAND NOT on surfdata file'//errMsg(__FILE__, __LINE__))
!#py        end if
!#py        call ncd_io(ncid=ncid, varname='PCT_CLAY', flag='read', data=clay3d, dim1name=grlnd, readvar=readvar)
!#py        if (.not. readvar) then
!#py           call endrun(msg=' ERROR: PCT_CLAY NOT on surfdata file'//errMsg(__FILE__, __LINE__))
!#py        end if
!#py        call ncd_pio_closefile(ncid)
!#py 
!#py        ! get original soil depths to be used in interpolation of sand and clay
!#py        allocate(zsoifl(1:nlevsoifl), zisoifl(0:nlevsoifl), dzsoifl(1:nlevsoifl))
!#py        do j = 1, nlevsoifl
!#py           zsoifl(j) = 0.025*(exp(0.5_r8*(j-0.5_r8))-1._r8)    !node depths
!#py        enddo
!#py 
!#py        dzsoifl(1) = 0.5_r8*(zsoifl(1)+zsoifl(2))             !thickness b/n two interfaces
!#py        do j = 2,nlevsoifl-1
!#py           dzsoifl(j)= 0.5_r8*(zsoifl(j+1)-zsoifl(j-1))
!#py        enddo
!#py        dzsoifl(nlevsoifl) = zsoifl(nlevsoifl)-zsoifl(nlevsoifl-1)
!#py 
!#py        zisoifl(0) = 0._r8
!#py        do j = 1, nlevsoifl-1
!#py           zisoifl(j) = 0.5_r8*(zsoifl(j)+zsoifl(j+1))         !interface depths
!#py        enddo
!#py        zisoifl(nlevsoifl) = zsoifl(nlevsoifl) + 0.5_r8*dzsoifl(nlevsoifl)
!#py 
!#py        organic_max = ParamsShareInst%organic_max
!#py 
!#py        do c = bounds%begc, bounds%endc
!#py           g = col_pp%gridcell(c)
!#py           l = col_pp%landunit(c)
!#py 
!#py           if (lun_pp%itype(l) /= istdlak) then  ! soil columns of both urban and non-urban types
!#py              if (lun_pp%itype(l)==istwet .or. lun_pp%itype(l)==istice .or. lun_pp%itype(l)==istice_mec) then
!#py                 ! do nothing
!#py              else if (lun_pp%urbpoi(l) .and. (col_pp%itype(c) /= icol_road_perv) .and. (col_pp%itype(c) /= icol_road_imperv) )then
!#py                 ! do nothing
!#py              else
!#py                 do lev = 1,nlevgrnd
!#py                    if ( more_vertlayers )then
!#py                       ! duplicate clay and sand values from last soil layer
!#py                       if (lev .eq. 1) then
!#py                          clay    = clay3d(g,1)
!#py                          sand    = sand3d(g,1)
!#py                          om_frac = organic3d(g,1)/organic_max
!#py                       else if (lev <= nlevsoi) then
!#py                          do j = 1,nlevsoifl-1
!#py                             if (zisoi(lev) >= zisoifl(j) .AND. zisoi(lev) < zisoifl(j+1)) then
!#py                                clay    = clay3d(g,j+1)
!#py                                sand    = sand3d(g,j+1)
!#py                                om_frac = organic3d(g,j+1)/organic_max
!#py                             endif
!#py                          end do
!#py                       else
!#py                          clay    = clay3d(g,nlevsoifl)
!#py                          sand    = sand3d(g,nlevsoifl)
!#py                          om_frac = 0._r8
!#py                       endif
!#py                    else
!#py                       ! duplicate clay and sand values from 10th soil layer
!#py                       if (lev <= nlevsoi) then
!#py                          clay    = clay3d(g,lev)
!#py                          sand    = sand3d(g,lev)
!#py                          om_frac = (organic3d(g,lev)/organic_max)**2._r8
!#py                       else
!#py                          clay    = clay3d(g,nlevsoi)
!#py                          sand    = sand3d(g,nlevsoi)
!#py                          om_frac = 0._r8
!#py                       endif
!#py                    end if
!#py 
!#py                    if (lun_pp%urbpoi(l)) om_frac = 0._r8
!#py                    claycol(c,lev)    = clay
!#py                    sandcol(c,lev)    = sand
!#py                    om_fraccol(c,lev) = om_frac
!#py                 end do
!#py              end if
!#py           end if ! end of if not lake
!#py 
!#py           if (lun_pp%itype(l) /= istdlak) then  ! soil columns of both urban and non-urban types
!#py              if (lun_pp%urbpoi(l)) then
!#py                 if (col_pp%itype(c)==icol_sunwall .or. col_pp%itype(c)==icol_shadewall .or. col_pp%itype(c)==icol_roof) then
!#py                    ! do nothing
!#py                 else
!#py                    this%depth_col(c, 1:nlayer)         = dzvic
!#py                    this%depth_col(c, nlayer+1:nlayert) = col_pp%dz(c, nlevsoi+1:nlevgrnd)
!#py 
!#py                    ! create weights to map soil moisture profiles (10 layer) to 3 layers for VIC hydrology, M.Huang
!#py                    call initelmVICMap(c, this)
!#py                    call initSoilParVIC(c, claycol, sandcol, om_fraccol, this)
!#py                 end if
!#py              else
!#py                 this%depth_col(c, 1:nlayer) = dzvic
!#py                 this%depth_col(c, nlayer+1:nlayert) = col_pp%dz(c, nlevsoi+1:nlevgrnd)
!#py 
!#py                 ! create weights to map soil moisture profiles (10 layer) to 3 layers for VIC hydrology, M.Huang
!#py                 call initelmVICMap(c, this)
!#py                 call initSoilParVIC(c, claycol, sandcol, om_fraccol, this)
!#py              end if
!#py           end if ! end of if not lake
!#py 
!#py        end do ! end of loop over columns
!#py 
!#py       deallocate(b2d, ds2d, dsmax2d, ws2d)
!#py       deallocate(sandcol, claycol, om_fraccol)
!#py       deallocate(sand3d, clay3d, organic3d)
!#py       deallocate(zisoifl, zsoifl, dzsoifl)
!#py 
!#py     end if ! end of if use_vichydro
!#py 
!#py     allocate(fdrain(bounds%begg:bounds%endg))
!#py     call getfil (fsurdat, locfn, 0)
!#py     call ncd_pio_openfile (ncid, locfn, 0)
!#py     call ncd_io(ncid=ncid, varname='fdrain', flag='read', data=fdrain, dim1name=grlnd, readvar=readvar)
!#py     if (.not. readvar) then
!#py        fdrain(:) = 2.5_r8
!#py     end if
!#py     call ncd_pio_closefile(ncid)
!#py 
!#py     associate(micro_sigma => col_pp%micro_sigma)
!#py       do c = bounds%begc, bounds%endc
!#py 
!#py          ! determine h2osfc threshold ("fill & spill" concept)
!#py          ! set to zero for no h2osfc (w/frac_infclust =large)
!#py 
!#py          this%h2osfc_thresh_col(c) = 0._r8
!#py          if (micro_sigma(c) > 1.e-6_r8 .and. (this%h2osfcflag /= 0)) then
!#py             d = 0.0
!#py             do p = 1,4
!#py                fd   = 0.5*(1.0_r8+shr_spfn_erf(d/(micro_sigma(c)*sqrt(2.0)))) - pc
!#py                dfdd = exp(-d**2/(2.0*micro_sigma(c)**2))/(micro_sigma(c)*sqrt(2.0*shr_const_pi))
!#py                d    = d - fd/dfdd
!#py             enddo
!#py             this%h2osfc_thresh_col(c) = 0.5*d*(1.0_r8+shr_spfn_erf(d/(micro_sigma(c)*sqrt(2.0)))) + &
!#py                  micro_sigma(c)/sqrt(2.0*shr_const_pi)*exp(-d**2/(2.0*micro_sigma(c)**2))
!#py             this%h2osfc_thresh_col(c) = 1.e3_r8 * this%h2osfc_thresh_col(c) !convert to mm from meters
!#py          else
!#py             this%h2osfc_thresh_col(c) = 0._r8
!#py          endif
!#py 
!#py          if (this%h2osfcflag == 0) then
!#py             this%h2osfc_thresh_col(c) = 0._r8    ! set to zero for no h2osfc (w/frac_infclust =large)
!#py          endif
!#py 
!#py          ! set decay factor
!#py          this%hkdepth_col(c) = 1._r8/2.5_r8
!#py 
!#py       end do
!#py     end associate
!#py 
!#py     deallocate(fdrain)
!#py 
!#py   end subroutine InitCold

  !------------------------------------------------------------------------
!#py   subroutine Restart(this, bounds, ncid, flag)
!#py     !
!#py     ! !USES:
!#py     !#py use ncdio_pio  , only : file_desc_t, ncd_io, ncd_double
!#py     !#py use restUtilMod
!#py     !
!#py     ! !ARGUMENTS:
!#py     class(soilhydrology_type) :: this
!#py     type(bounds_type) , intent(in)    :: bounds
!#py     type(file_desc_t) , intent(inout) :: ncid   ! netcdf id
!#py     character(len=*)  , intent(in)    :: flag   ! 'read' or 'write'
!#py     !
!#py     ! !LOCAL VARIABLES:
!#py     integer :: j,c ! indices
!#py     logical :: readvar      ! determine if variable is on initial file
!#py     !-----------------------------------------------------------------------
!#py 
!#py     call restartvar(ncid=ncid, flag=flag, varname='FROST_TABLE', xtype=ncd_double,  &
!#py          dim1name='column', &
!#py          long_name='frost table depth', units='m', &
!#py          interpinic_flag='interp', readvar=readvar, data=this%frost_table_col)
!#py     if (flag == 'read' .and. .not. readvar) then
!#py        this%frost_table_col(bounds%begc:bounds%endc) = col_pp%zi(bounds%begc:bounds%endc,nlevsoi)
!#py     end if
!#py 
!#py     call restartvar(ncid=ncid, flag=flag, varname='WA', xtype=ncd_double,  &
!#py          dim1name='column', &
!#py          long_name='water in the unconfined aquifer', units='mm', &
!#py          interpinic_flag='interp', readvar=readvar, data=this%wa_col)
!#py 
!#py     call restartvar(ncid=ncid, flag=flag, varname='ZWT', xtype=ncd_double,  &
!#py          dim1name='column', &
!#py          long_name='water table depth', units='m', &
!#py          interpinic_flag='interp', readvar=readvar, data=this%zwt_col)
!#py 
!#py     call restartvar(ncid=ncid, flag=flag, varname='ZWT_PERCH', xtype=ncd_double,  &
!#py          dim1name='column', &
!#py          long_name='perched water table depth', units='m', &
!#py          interpinic_flag='interp', readvar=readvar, data=this%zwt_perched_col)
!#py     if (flag == 'read' .and. .not. readvar) then
!#py        this%zwt_perched_col(bounds%begc:bounds%endc) = col_pp%zi(bounds%begc:bounds%endc,nlevsoi)
!#py     end if
!#py 
!#py !#py !#py   end subroutine Restart

  !-----------------------------------------------------------------------
  subroutine initSoilParVIC(c, claycol, sandcol, om_fraccol, soilhydrology_vars)
    !
    ! !DESCRIPTION:
    ! Convert default elm soil properties to VIC parameters
    ! to be used for runoff simulations
    ! added by M. Huang
    !
    ! !USES:
    use elm_varcon  , only : denh2o, denice
    use elm_varpar  , only : nlevsoi, nlayer, nlayert, nlevgrnd
    !
    ! !ARGUMENTS:
    integer                  , intent(in)    :: c               ! column index
    real(r8)                 , pointer       :: sandcol(:,:)    ! read in - soil texture: percent sand
    real(r8)                 , pointer       :: claycol(:,:)    ! read in - soil texture: percent clay
    real(r8)                 , pointer       :: om_fraccol(:,:) ! read in - organic matter: kg/m3
    type(soilhydrology_type) , intent(inout) :: soilhydrology_vars

    ! !LOCAL VARIABLES:
    real(r8) :: om_watsat    = 0.9_r8             ! porosity of organic soil
    real(r8) :: om_hksat     = 0.1_r8             ! saturated hydraulic conductivity of organic soil [mm/s]
    real(r8) :: om_tkm       = 0.25_r8            ! thermal conductivity of organic soil (Farouki, 1986) [W/m/K]
    real(r8) :: om_sucsat    = 10.3_r8            ! saturated suction for organic matter (Letts, 2000)
    real(r8) :: om_csol      = 2.5_r8             ! heat capacity of peat soil *10^6 (J/K m3) (Farouki, 1986)
    real(r8) :: om_tkd       = 0.05_r8            ! thermal conductivity of dry organic soil (Farouki, 1981)
    real(r8) :: om_b         = 2.7_r8             ! Clapp Hornberger paramater for oragnic soil (Letts, 2000)
    real(r8) :: om_expt      = 3._r8+2._r8*2.7_r8 ! soil expt for VIC
    real(r8) :: csol_bedrock = 2.0e6_r8           ! vol. heat capacity of granite/sandstone  J/(m3 K)(Shabbir, 2000)
    real(r8) :: pc           = 0.5_r8             ! percolation threshold
    real(r8) :: pcbeta       = 0.139_r8           ! percolation exponent
    real(r8) :: xksat                             ! maximum hydraulic conductivity of soil [mm/s]
    real(r8) :: perc_frac                         ! "percolating" fraction of organic soil
    real(r8) :: perc_norm                         ! normalize to 1 when 100% organic soil
    real(r8) :: uncon_hksat                       ! series conductivity of mineral/organic soil
    real(r8) :: uncon_frac                        ! fraction of "unconnected" soil
    real(r8) :: temp_sum_frac                     ! sum of node fractions in each VIC layer
    real(r8) :: sandvic(1:nlayert)                ! temporary, weighted averaged sand% for VIC layers
    real(r8) :: clayvic(1:nlayert)                ! temporary, weighted averaged clay% for VIC layers
    real(r8) :: om_fracvic(1:nlayert)             ! temporary, weighted averaged organic matter fract for VIC layers
    integer  :: i, j                              ! indices
    !-------------------------------------------------------------------------------------------

    ! soilhydrology_vars%depth_col(:,:)           Output: layer depth of upper layer(m)
    ! soilhydrology_vars%vic_elm_fract_col(:,:,:) Output: fraction of VIC layers in elm layers
    ! soilhydrology_vars%c_param_col(:)           Output: baseflow exponent (Qb)
    ! soilhydrology_vars%expt_col(:,:)            Output: pore-size distribution related paramter(Q12)
    ! soilhydrology_vars%ksat_col(:,:)            Output: Saturated hydrologic conductivity (mm/s)
    ! soilhydrology_vars%phi_s_col(:,:)           Output: soil moisture dissusion parameter
    ! soilhydrology_vars%porosity_col(:,:)        Output: soil porosity
    ! soilhydrology_vars%max_moist_col(:,:)       Output: maximum soil moisture (ice + liq)

    !  map parameters between VIC layers and elm layers
    soilhydrology_vars%c_param_col(c) = 2._r8

    ! map the elm layers to VIC layers
    do i = 1, nlayer

       sandvic(i)    = 0._r8
       clayvic(i)    = 0._r8
       om_fracvic(i) = 0._r8
       temp_sum_frac = 0._r8
       do j = 1, nlevsoi
          sandvic(i)    = sandvic(i)    + sandcol(c,j)    * soilhydrology_vars%vic_elm_fract_col(c,i,j)
          clayvic(i)    = clayvic(i)    + claycol(c,j)    * soilhydrology_vars%vic_elm_fract_col(c,i,j)
          om_fracvic(i) = om_fracvic(i) + om_fraccol(c,j) * soilhydrology_vars%vic_elm_fract_col(c,i,j)
          temp_sum_frac = temp_sum_frac +                   soilhydrology_vars%vic_elm_fract_col(c,i,j)
       end do

       !average soil properties, M.Huang, 08/11/2010
       sandvic(i) = sandvic(i)/temp_sum_frac
       clayvic(i) = clayvic(i)/temp_sum_frac
       om_fracvic(i) = om_fracvic(i)/temp_sum_frac

       !make sure sand, clay and om fractions are between 0 and 100%
       sandvic(i)    = min(100._r8 , sandvic(i))
       clayvic(i)    = min(100._r8 , clayvic(i))
       om_fracvic(i) = min(100._r8 , om_fracvic(i))
       sandvic(i)    = max(0._r8   , sandvic(i))
       clayvic(i)    = max(0._r8   , clayvic(i))
       om_fracvic(i) = max(0._r8   , om_fracvic(i))

       !calculate other parameters based on teh percentages
       soilhydrology_vars%porosity_col(c, i) = 0.489_r8 - 0.00126_r8*sandvic(i)
       soilhydrology_vars%expt_col(c, i)     = 3._r8+ 2._r8*(2.91_r8 + 0.159_r8*clayvic(i))
       xksat = 0.0070556 *( 10.**(-0.884+0.0153*sandvic(i)) )

       !consider organic matter, M.Huang
       soilhydrology_vars%expt_col(c, i)    = &
            (1._r8 - om_fracvic(i))*soilhydrology_vars%expt_col(c, i)    + om_fracvic(i)*om_expt
       soilhydrology_vars%porosity_col(c,i) = &
            (1._r8 - om_fracvic(i))*soilhydrology_vars%porosity_col(c,i) + om_watsat*om_fracvic(i)

       ! perc_frac is zero unless perf_frac greater than percolation threshold
       if (om_fracvic(i) > pc) then
          perc_norm=(1._r8 - pc)**(-pcbeta)
          perc_frac=perc_norm*(om_fracvic(i) - pc)**pcbeta
       else
          perc_frac=0._r8
       endif
       ! uncon_frac is fraction of mineral soil plus fraction of "nonpercolating" organic soil
       uncon_frac=(1._r8-om_fracvic(i))+(1._r8-perc_frac)*om_fracvic(i)

       ! uncon_hksat is series addition of mineral/organic conductivites
       if (om_fracvic(i) < 1._r8) then
          uncon_hksat=uncon_frac/((1._r8-om_fracvic(i))/xksat &
               +((1._r8-perc_frac)*om_fracvic(i))/om_hksat)
       else
          uncon_hksat = 0._r8
       end if

       soilhydrology_vars%ksat_col(c,i)  = &
            uncon_frac*uncon_hksat + (perc_frac*om_fracvic(i))*om_hksat

       soilhydrology_vars%max_moist_col(c,i) = &
            soilhydrology_vars%porosity_col(c,i) * soilhydrology_vars%depth_col(c,i) * 1000._r8 !in mm!

       soilhydrology_vars%phi_s_col(c,i) = &
            -(exp((1.54_r8 - 0.0095_r8*sandvic(i) + &
            0.0063_r8*(100.0_r8-sandvic(i)-clayvic(i)))*log(10.0_r8))*9.8e-5_r8)

    end do ! end of loop over layers

  end subroutine initSoilParVIC

   !-----------------------------------------------------------------------
   subroutine initELMVICMap(c, soilhydrology_vars)
     !
     ! !DESCRIPTION:
     ! This subroutine calculates mapping between elm and VIC layers
     ! added by AWang, modified by M.Huang for elm4
     ! NOTE: in elm h2osoil_liq unit is kg/m2, in VIC moist is mm
     ! h2osoi_ice is actually water equavlent ice content.
     !
     ! !USES:
     use elm_varcon  , only : denh2o, denice
     use elm_varpar  , only : nlevsoi, nlayer, nlayert, nlevgrnd
     !
     ! !ARGUMENTS:
     integer , intent(in)  :: c
     type(soilhydrology_type), intent(inout) :: soilhydrology_vars
     !
     ! !REVISION HISTORY:
     ! Created by Maoyi Huang
     ! 11/13/2012, Maoyi Huang: rewrite the mapping modules in elm4VIC
     !
     ! !LOCAL VARIABLES
     real(r8) :: sum_frac(1:nlayer)                  ! sum of fraction for each layer
     real(r8) :: deltal(1:nlayer+1)                  ! temporary
     real(r8) :: zsum                                ! temporary
     real(r8) :: lsum                                ! temporary
     real(r8) :: temp                                ! temporary
     integer :: i, j, fc
     !-----------------------------------------------------------------------

     associate(                                                    &
          dz            =>    col_pp%dz    ,                          & ! Input:  [real(r8) (:,:)   ]  layer depth (m)
          zi            =>    col_pp%zi    ,                          & ! Input:  [real(r8) (:,:)   ]  interface level below a "z" level (m)
          z             =>    col_pp%z     ,                          & ! Input:  [real(r8) (:,:)   ]  layer thickness (m)

          depth         =>    soilhydrology_vars%depth_col ,       & ! Input:  [real(r8) (:,:)   ]  layer depth of VIC (m)
          vic_elm_fract =>    soilhydrology_vars%vic_elm_fract_col & ! Output: [real(r8) (:,:,:) ]  fraction of VIC layers in elm layers
          )

       !  set fraction of VIC layer in each elm layer

       lsum = 0._r8
       do i = 1, nlayer
          deltal(i) = depth(c,i)
       end do
       do i = 1, nlayer
          zsum = 0._r8
          sum_frac(i) = 0._r8
          do j = 1, nlevsoi
             if( (zsum < lsum) .and. (zsum + dz(c,j) >= lsum ))  then
                call linear_interp(lsum, temp, zsum, zsum + dz(c,j), 0._r8, 1._r8)
                vic_elm_fract(c,i,j) = 1._r8 - temp
                if(lsum + deltal(i) < zsum + dz(c,j)) then
                   call linear_interp(lsum + deltal(i), temp, zsum, zsum + dz(c,j), 1._r8, 0._r8)
                   vic_elm_fract(c,i,j) = vic_elm_fract(c,i,j) - temp
                end if
             else if( (zsum < lsum + deltal(i)) .and. (zsum + dz(c,j) >= lsum + deltal(i)) ) then
                call linear_interp(lsum + deltal(i), temp, zsum, zsum + dz(c,j), 0._r8, 1._r8)
                vic_elm_fract(c,i,j) = temp
                if(zsum<=lsum) then
                   call linear_interp(lsum, temp, zsum, zsum + dz(c,j), 0._r8, 1._r8)
                   vic_elm_fract(c,i,j) = vic_elm_fract(c,i,j) - temp
                end if
             else if( (zsum >= lsum .and. zsum + dz(c,j) <= lsum + deltal(i)) )  then
                vic_elm_fract(c,i,j) = 1._r8
             else
                vic_elm_fract(c,i,j) = 0._r8
             end if
             zsum = zsum + dz(c,j)
             sum_frac(i) = sum_frac(i) + vic_elm_fract(c,i,j)
          end do                           ! end elm layer calculation
          lsum = lsum + deltal(i)
       end do                             ! end VIC layer calcultion

     end associate

   end subroutine initelmVICMap

   !-------------------------------------------------------------------
   subroutine linear_interp(x,y, x0, x1, y0, y1)
     !
     ! !DESCRIPTION:
     ! This subroutine provides linear interpolation
     !
     ! !ARGUMENTS:
     implicit none
     real(r8), intent(in)  :: x, x0, y0, x1, y1
     real(r8), intent(out) :: y
     !-------------------------------------------------------------------

     y = y0 + (x - x0) * (y1 - y0) / (x1 - x0)

   end subroutine linear_interp

   !-----------------------------------------------------------------------
   subroutine ReadNL( this )
     !
     ! !DESCRIPTION:
     ! Read namelist for SoilHydrology
     !
     ! !USES:
     !
     ! !ARGUMENTS:
     class(soilhydrology_type) :: this
     !!character(len=*), intent(IN) :: NLFilename ! Namelist filename
     !
     ! !LOCAL VARIABLES:
     integer :: ierr                 ! error code
     integer :: unitn                ! unit for namelist file
     integer :: origflag=0            !use to control soil hydraulic properties
     integer :: h2osfcflag=1          !If surface water is active or not
     character(len=32) :: subname = 'SoilHydrology_readnl'  ! subroutine name
     !-----------------------------------------------------------------------

     !! needed To avoid incorrect deepcopy to the GPU
     allocate(this%h2osfcflag)
     allocate(this%origflag  )


     this%h2osfcflag = h2osfcflag
     this%origflag   = origflag

   end subroutine ReadNL

 end Module SoilHydrologyType
