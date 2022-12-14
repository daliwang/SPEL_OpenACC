module SolarAbsorbedType

  !------------------------------------------------------------------------------
  ! !USES:
  use shr_kind_mod , only: r8 => shr_kind_r8
  !#py use shr_log_mod  , only: errMsg => shr_log_errMsg
  use decompMod    , only : bounds_type
  use elm_varcon   , only : spval
  !
  ! !PUBLIC TYPES:
  implicit none
  save
  private
  !
  ! !PUBLIC DATA MEMBERS:
  type, public :: solarabs_type

     ! Solar reflected
     real(r8), pointer :: fsr_patch              (:)   => null()! patch solar radiation reflected (W/m**2)

     ! Solar Absorbed
     real(r8), pointer :: fsa_patch              (:)   => null() ! patch solar radiation absorbed (total) (W/m**2)
     real(r8), pointer :: fsa_u_patch            (:)   => null() ! patch urban solar radiation absorbed (total) (W/m**2)
     real(r8), pointer :: fsa_r_patch            (:)   => null() ! patch rural solar radiation absorbed (total) (W/m**2)
     real(r8), pointer :: parsun_z_patch         (:,:) => null() ! patch absorbed PAR for sunlit leaves in canopy layer (W/m**2)
     real(r8), pointer :: parsha_z_patch         (:,:) => null() ! patch absorbed PAR for shaded leaves in canopy layer (W/m**2)

     real(r8), pointer :: sabg_soil_patch        (:)   => null() ! patch solar radiation absorbed by soil (W/m**2)
     real(r8), pointer :: sabg_snow_patch        (:)   => null() ! patch solar radiation absorbed by snow (W/m**2)
     real(r8), pointer :: sabg_patch             (:)   => null() ! patch solar radiation absorbed by ground (W/m**2)
     real(r8), pointer :: sabg_chk_patch         (:)   => null() ! patch fsno weighted sum (W/m**2)
     real(r8), pointer :: sabg_lyr_patch         (:,:) => null() ! patch absorbed radiation in each snow layer and top soil layer (pft,lyr) [W/m2]
     real(r8), pointer :: sabg_pen_patch         (:)   => null() ! patch (rural) shortwave radiation penetrating top soisno layer [W/m2]

     real(r8), pointer :: sub_surf_abs_SW_col    (:) => null()  ! col percent of solar radiation absorbed below first snow layer
     real(r8), pointer :: sabv_patch             (:) => null()  ! patch solar radiation absorbed by vegetation (W/m**2)

     real(r8), pointer :: sabs_roof_dir_lun      (:,:) => null() ! lun direct solar absorbed by roof per unit ground area per unit incident flux
     real(r8), pointer :: sabs_roof_dif_lun      (:,:) => null() ! lun diffuse solar absorbed by roof per unit ground area per unit incident flux
     real(r8), pointer :: sabs_sunwall_dir_lun   (:,:) => null() ! lun direct  solar absorbed by sunwall per unit wall area per unit incident flux
     real(r8), pointer :: sabs_sunwall_dif_lun   (:,:) => null() ! lun diffuse solar absorbed by sunwall per unit wall area per unit incident flux
     real(r8), pointer :: sabs_shadewall_dir_lun (:,:) => null() ! lun direct  solar absorbed by shadewall per unit wall area per unit incident flux
     real(r8), pointer :: sabs_shadewall_dif_lun (:,:) => null() ! lun diffuse solar absorbed by shadewall per unit wall area per unit incident flux
     real(r8), pointer :: sabs_improad_dir_lun   (:,:) => null() ! lun direct  solar absorbed by impervious road per unit ground area per unit incident flux
     real(r8), pointer :: sabs_improad_dif_lun   (:,:) => null() ! lun diffuse solar absorbed by impervious road per unit ground area per unit incident flux
     real(r8), pointer :: sabs_perroad_dir_lun   (:,:) => null() ! lun direct  solar absorbed by pervious road per unit ground area per unit incident flux
     real(r8), pointer :: sabs_perroad_dif_lun   (:,:) => null() ! lun diffuse solar absorbed by pervious road per unit ground area per unit incident flux

     ! Currently needed by lake code
     ! TODO (MV 8/20/2014) should be moved in the future
     real(r8), pointer :: fsds_nir_d_patch       (:)  => null() ! patch incident direct beam nir solar radiation (W/m**2)
     real(r8), pointer :: fsds_nir_i_patch       (:)  => null() ! patch incident diffuse nir solar radiation (W/m**2)
     real(r8), pointer :: fsds_nir_d_ln_patch    (:)  => null() ! patch incident direct beam nir solar radiation at local noon (W/m**2)
     real(r8), pointer :: fsr_nir_d_patch        (:)  => null() ! patch reflected direct beam nir solar radiation (W/m**2)
     real(r8), pointer :: fsr_nir_i_patch        (:)  => null() ! patch reflected diffuse nir solar radiation (W/m**2)
     real(r8), pointer :: fsr_nir_d_ln_patch     (:)  => null() ! patch reflected direct beam nir solar radiation at local noon (W/m**2)

   contains

     procedure, public  :: Init
     procedure, private :: InitAllocate
     procedure, private :: InitHistory
     procedure, private :: InitCold
     !#py procedure, public  :: Restart

  end type solarabs_type
  !-----------------------------------------------------------------------
contains

  !------------------------------------------------------------------------
  subroutine Init(this, bounds)

    class(solarabs_type) :: this
    type(bounds_type), intent(in) :: bounds

    call this%InitAllocate(bounds)
    call this%InitHistory(bounds)
    call this%InitCold(bounds)

  end subroutine Init

  !-----------------------------------------------------------------------
  subroutine InitAllocate(this, bounds)
    !
    ! Allocate module variables and data structures
    !
    ! !USES:
    use elm_varpar    , only : nlevcan, nlevcan, numrad, nlevsno
    !
    ! !ARGUMENTS:
    class(solarabs_type) :: this
    type(bounds_type), intent(in) :: bounds
    !
    ! !LOCAL VARIABLES:
    integer :: begp, endp
    integer :: begc, endc
    integer :: begl, endl
    !---------------------------------------------------------------------

    begp = bounds%begp; endp = bounds%endp
    begc = bounds%begc; endc = bounds%endc
    begl = bounds%begl; endl = bounds%endl

    allocate(this%fsa_patch              (begp:endp))              ; this%fsa_patch              (:)   = spval
    allocate(this%fsa_u_patch            (begp:endp))              ; this%fsa_u_patch            (:)   = spval
    allocate(this%fsa_r_patch            (begp:endp))              ; this%fsa_r_patch            (:)   = spval
    allocate(this%parsun_z_patch         (begp:endp,1:nlevcan))    ; this%parsun_z_patch         (:,:) = spval
    allocate(this%parsha_z_patch         (begp:endp,1:nlevcan))    ; this%parsha_z_patch         (:,:) = spval
    allocate(this%sabv_patch             (begp:endp))              ; this%sabv_patch             (:)   = spval
    allocate(this%sabg_patch             (begp:endp))              ; this%sabg_patch             (:)   = spval
    allocate(this%sabg_lyr_patch         (begp:endp,-nlevsno+1:1)) ; this%sabg_lyr_patch         (:,:) = spval
    allocate(this%sabg_pen_patch         (begp:endp))              ; this%sabg_pen_patch         (:)   = spval
    allocate(this%sabg_soil_patch        (begp:endp))              ; this%sabg_soil_patch        (:)   = spval
    allocate(this%sabg_snow_patch        (begp:endp))              ; this%sabg_snow_patch        (:)   = spval
    allocate(this%sabg_chk_patch         (begp:endp))              ; this%sabg_chk_patch         (:)   = spval
    allocate(this%sabs_roof_dir_lun      (begl:endl,1:numrad))     ; this%sabs_roof_dir_lun      (:,:) = spval
    allocate(this%sabs_roof_dif_lun      (begl:endl,1:numrad))     ; this%sabs_roof_dif_lun      (:,:) = spval
    allocate(this%sabs_sunwall_dir_lun   (begl:endl,1:numrad))     ; this%sabs_sunwall_dir_lun   (:,:) = spval
    allocate(this%sabs_sunwall_dif_lun   (begl:endl,1:numrad))     ; this%sabs_sunwall_dif_lun   (:,:) = spval
    allocate(this%sabs_shadewall_dir_lun (begl:endl,1:numrad))     ; this%sabs_shadewall_dir_lun (:,:) = spval
    allocate(this%sabs_shadewall_dif_lun (begl:endl,1:numrad))     ; this%sabs_shadewall_dif_lun (:,:) = spval
    allocate(this%sabs_improad_dir_lun   (begl:endl,1:numrad))     ; this%sabs_improad_dir_lun   (:,:) = spval
    allocate(this%sabs_improad_dif_lun   (begl:endl,1:numrad))     ; this%sabs_improad_dif_lun   (:,:) = spval
    allocate(this%sabs_perroad_dir_lun   (begl:endl,1:numrad))     ; this%sabs_perroad_dir_lun   (:,:) = spval
    allocate(this%sabs_perroad_dif_lun   (begl:endl,1:numrad))     ; this%sabs_perroad_dif_lun   (:,:) = spval
    allocate(this%sub_surf_abs_SW_col    (begc:endc))              ; this%sub_surf_abs_SW_col    (:)   = spval
    allocate(this%fsr_patch              (begp:endp))              ; this%fsr_patch              (:)   = spval
    allocate(this%fsr_nir_d_patch        (begp:endp))              ; this%fsr_nir_d_patch        (:)   = spval
    allocate(this%fsr_nir_i_patch        (begp:endp))              ; this%fsr_nir_i_patch        (:)   = spval
    allocate(this%fsr_nir_d_ln_patch     (begp:endp))              ; this%fsr_nir_d_ln_patch     (:)   = spval
    allocate(this%fsds_nir_d_patch       (begp:endp))              ; this%fsds_nir_d_patch       (:)   = spval
    allocate(this%fsds_nir_i_patch       (begp:endp))              ; this%fsds_nir_i_patch       (:)   = spval
    allocate(this%fsds_nir_d_ln_patch    (begp:endp))              ; this%fsds_nir_d_ln_patch    (:)   = spval

  end subroutine InitAllocate


  !-----------------------------------------------------------------------
  subroutine InitHistory(this, bounds)
    !
    ! History fields initialization
    !
    ! !USES:
    !#py use shr_infnan_mod, only : nan => shr_infnan_nan, assignment(=)
    use elm_varctl    , only : use_snicar_frc
    use elm_varpar    , only : nlevsno
    !#py use histFileMod   , only : hist_addfld1d, hist_addfld2d
    !#py use histFileMod   , only : no_snow_normal
    !
    ! !ARGUMENTS:
    class(solarabs_type) :: this
    type(bounds_type), intent(in) :: bounds
    !
    ! !LOCAL VARIABLES:
    integer :: begp, endp
    integer :: begc, endc
    real(r8), pointer :: data2dptr(:,:) ! temp. pointers for slicing larger arrays
    !---------------------------------------------------------------------

    begp = bounds%begp; endp = bounds%endp
    begc = bounds%begc; endc = bounds%endc

    this%fsa_patch(begp:endp) = spval
    !#py call hist_addfld1d (fname='FSA', units='W/m^2',  &
         !#py avgflag='A', long_name='absorbed solar radiation', &
         !#py !#py ptr_patch=this%fsa_patch, c2l_scale_type='urbanf')

    this%fsa_r_patch(begp:endp) = spval
    !#py call hist_addfld1d (fname='FSA_R', units='W/m^2',  &
         !#py avgflag='A', long_name='Rural absorbed solar radiation', &
         !#py !#py ptr_patch=this%fsa_r_patch, set_spec=spval)

    this%fsa_u_patch(begp:endp) = spval
    !#py call hist_addfld1d (fname='FSA_U', units='W/m^2',  &
         !#py avgflag='A', long_name='Urban absorbed solar radiation', &
         !#py !#py ptr_patch=this%fsa_u_patch, c2l_scale_type='urbanf', set_nourb=spval)

    this%fsr_patch(begp:endp) = spval
    !#py call hist_addfld1d (fname='FSR', units='W/m^2',  &
         !#py avgflag='A', long_name='reflected solar radiation', &
         !#py !#py ptr_patch=this%fsr_patch, c2l_scale_type='urbanf')
    ! Rename of FSR for Urban intercomparision project
    !#py call hist_addfld1d (fname='SWup', units='W/m^2',  &
         !#py avgflag='A', long_name='upwelling shortwave radiation', &
         !#py !#py ptr_patch=this%fsr_patch, c2l_scale_type='urbanf', default='inactive')

    this%sabg_lyr_patch(begp:endp,-nlevsno+1:0) = spval
    data2dptr => this%sabg_lyr_patch(:,-nlevsno+1:0)
    !#py call hist_addfld2d (fname='SNO_ABS', units='W/m^2', type2d='levsno',  &
         !#py avgflag='A', long_name='Absorbed solar radiation in each snow layer', &
         !#py !#py ptr_patch=data2dptr, no_snow_behavior=no_snow_normal, default='inactive')

    this%sabv_patch(begp:endp) = spval
    !#py call hist_addfld1d (fname='SABV', units='W/m^2',  &
         !#py avgflag='A', long_name='solar rad absorbed by veg', &
         !#py !#py ptr_patch=this%sabv_patch, c2l_scale_type='urbanf')

    this%sabg_patch(begp:endp) = spval
    !#py call hist_addfld1d (fname='SABG', units='W/m^2',  &
         !#py avgflag='A', long_name='solar rad absorbed by ground', &
         !#py !#py ptr_patch=this%sabg_patch, c2l_scale_type='urbanf')

    this%sabg_pen_patch(begp:endp) = spval
    !#py call hist_addfld1d (fname='SABG_PEN', units='watt/m^2',  &
         !#py avgflag='A', long_name='Rural solar rad penetrating top soil or snow layer', &
         !#py !#py ptr_patch=this%sabg_pen_patch, set_spec=spval)

     ! Currently needed by lake code - TODO should not be here
    this%fsds_nir_d_patch(begp:endp) = spval
    !#py call hist_addfld1d (fname='FSDSND', units='W/m^2',  &
         !#py avgflag='A', long_name='direct nir incident solar radiation', &
         !#py !#py ptr_patch=this%fsds_nir_d_patch)

    this%fsds_nir_i_patch(begp:endp) = spval
    !#py call hist_addfld1d (fname='FSDSNI', units='W/m^2',  &
         !#py avgflag='A', long_name='diffuse nir incident solar radiation', &
         !#py !#py ptr_patch=this%fsds_nir_i_patch)

    this%fsds_nir_d_ln_patch(begp:endp) = spval
    !#py call hist_addfld1d (fname='FSDSNDLN', units='W/m^2',  &
         !#py avgflag='A', long_name='direct nir incident solar radiation at local noon', &
         !#py !#py ptr_patch=this%fsds_nir_d_ln_patch)

    this%fsr_nir_d_patch(begp:endp) = spval
    !#py call hist_addfld1d (fname='FSRND', units='W/m^2',  &
         !#py avgflag='A', long_name='direct nir reflected solar radiation', &
         !#py !#py ptr_patch=this%fsr_nir_d_patch, c2l_scale_type='urbanf')

    this%fsr_nir_i_patch(begp:endp) = spval
    !#py call hist_addfld1d (fname='FSRNI', units='W/m^2',  &
         !#py avgflag='A', long_name='diffuse nir reflected solar radiation', &
         !#py !#py ptr_patch=this%fsr_nir_i_patch, c2l_scale_type='urbanf')

    this%fsr_nir_d_ln_patch(begp:endp) = spval
    !#py call hist_addfld1d (fname='FSRNDLN', units='W/m^2',  &
         !#py avgflag='A', long_name='direct nir reflected solar radiation at local noon', &
         !#py !#py ptr_patch=this%fsr_nir_d_ln_patch, c2l_scale_type='urbanf')

    this%sub_surf_abs_SW_col(begc:endc) = spval
    !#py call hist_addfld1d (fname='SNOINTABS', units='%', &
         !#py avgflag='A', long_name='Percent of incoming solar absorbed by lower snow layers', &
         !#py !#py ptr_col=this%sub_surf_abs_SW_col, set_lake=spval, set_urb=spval)

  end subroutine InitHistory

  !------------------------------------------------------------------------
  subroutine InitCold(this, bounds)
    !
    ! Initialize module surface albedos to reasonable values
    !
    use landunit_varcon, only : istsoil, istcrop
    !
    ! !ARGUMENTS:
    class(solarabs_type) :: this
    type(bounds_type), intent(in) :: bounds
    !
    ! !LOCAL VARIABLES:
    integer :: begl, endl
    !-----------------------------------------------------------------------

    begl = bounds%begl; endl = bounds%endl

    this%sabs_roof_dir_lun      (begl:endl, :) = 0._r8
    this%sabs_roof_dif_lun      (begl:endl, :) = 0._r8
    this%sabs_sunwall_dir_lun   (begl:endl, :) = 0._r8
    this%sabs_sunwall_dif_lun   (begl:endl, :) = 0._r8
    this%sabs_shadewall_dir_lun (begl:endl, :) = 0._r8
    this%sabs_shadewall_dif_lun (begl:endl, :) = 0._r8
    this%sabs_improad_dir_lun   (begl:endl, :) = 0._r8
    this%sabs_improad_dif_lun   (begl:endl, :) = 0._r8
    this%sabs_perroad_dir_lun   (begl:endl, :) = 0._r8
    this%sabs_perroad_dif_lun   (begl:endl, :) = 0._r8

  end subroutine InitCold

  !---------------------------------------------------------------------
!#py   subroutine Restart(this, bounds, ncid, flag)
!#py     !
!#py     ! !DESCRIPTION:
!#py     ! Read/Write module information to/from restart file.
!#py     !
!#py     ! !USES:
!#py     !#py use shr_infnan_mod , only : shr_infnan_isnan
!#py     use elm_varctl     , only : use_snicar_frc, iulog
!#py     !#py use spmdMod        , only : masterproc
!#py     !#py use abortutils     , only : endrun
!#py     !#py use ncdio_pio      , only : file_desc_t, ncd_defvar, ncd_io, ncd_double, ncd_int, ncd_inqvdlen
!#py     !#py use restUtilMod
!#py     !
!#py     ! !ARGUMENTS:
!#py     class(solarabs_type) :: this
!#py     type(bounds_type), intent(in)    :: bounds
!#py     type(file_desc_t), intent(inout) :: ncid   ! netcdf id
!#py     character(len=*) , intent(in)    :: flag   ! 'read' or 'write'
!#py     !
!#py     ! !LOCAL VARIABLES:
!#py     logical :: readvar      ! determine if variable is on initial file
!#py     integer :: p
!#py     !---------------------------------------------------------------------
!#py 
!#py     call restartvar(ncid=ncid, flag=flag, varname='sabs_roof_dir', xtype=ncd_double,  dim1name='landunit',            &
!#py          dim2name='numrad', switchdim=.true.,                                                                         &
!#py          long_name='direct solar absorbed by roof per unit ground area per unit incident flux', units='',             &
!#py          interpinic_flag='interp', readvar=readvar, data=this%sabs_roof_dir_lun)
!#py 
!#py     call restartvar(ncid=ncid, flag=flag, varname='sabs_roof_dif', xtype=ncd_double,  dim1name='landunit',            &
!#py          dim2name='numrad', switchdim=.true.,                                                                         &
!#py          long_name='diffuse solar absorbed by roof per unit ground area per unit incident flux', units='',            &
!#py          interpinic_flag='interp', readvar=readvar, data=this%sabs_roof_dif_lun)
!#py 
!#py     call restartvar(ncid=ncid, flag=flag, varname='sabs_sunwall_dir', xtype=ncd_double,  dim1name='landunit',         &
!#py          dim2name='numrad', switchdim=.true.,                                                                         &
!#py          long_name='direct solar absorbed by sunwall per unit wall area per unit incident flux', units='',            &
!#py          interpinic_flag='interp', readvar=readvar, data=this%sabs_sunwall_dir_lun)
!#py 
!#py     call restartvar(ncid=ncid, flag=flag, varname='sabs_sunwall_dif', xtype=ncd_double,  dim1name='landunit',         &
!#py          dim2name='numrad', switchdim=.true.,                                                                         &
!#py          long_name='diffuse solar absorbed by sunwall per unit wall area per unit incident flux', units='',           &
!#py          interpinic_flag='interp', readvar=readvar, data=this%sabs_sunwall_dif_lun)
!#py 
!#py     call restartvar(ncid=ncid, flag=flag, varname='sabs_shadewall_dir', xtype=ncd_double,  dim1name='landunit',       &
!#py          dim2name='numrad', switchdim=.true.,                                                                         &
!#py          long_name='direct solar absorbed by shadewall per unit wall area per unit incident flux', units='',          &
!#py          interpinic_flag='interp', readvar=readvar, data=this%sabs_shadewall_dir_lun)
!#py 
!#py     call restartvar(ncid=ncid, flag=flag, varname='sabs_shadewall_dif', xtype=ncd_double,  dim1name='landunit',       &
!#py          dim2name='numrad', switchdim=.true.,                                                                         &
!#py          long_name='diffuse solar absorbed by shadewall per unit wall area per unit incident flux', units='',         &
!#py          interpinic_flag='interp', readvar=readvar, data=this%sabs_shadewall_dif_lun)
!#py 
!#py     call restartvar(ncid=ncid, flag=flag, varname='sabs_improad_dir', xtype=ncd_double,  dim1name='landunit',         &
!#py          dim2name='numrad', switchdim=.true.,                                                                         &
!#py          long_name='direct solar absorbed by impervious road per unit ground area per unit incident flux', units='',  &
!#py          interpinic_flag='interp', readvar=readvar, data=this%sabs_improad_dir_lun)
!#py 
!#py     call restartvar(ncid=ncid, flag=flag, varname='sabs_improad_dif', xtype=ncd_double,  dim1name='landunit',         &
!#py          dim2name='numrad', switchdim=.true.,                                                                         &
!#py          long_name='diffuse solar absorbed by impervious road per unit ground area per unit incident flux', units='', &
!#py          interpinic_flag='interp', readvar=readvar, data=this%sabs_improad_dif_lun)
!#py 
!#py     call restartvar(ncid=ncid, flag=flag, varname='sabs_perroad_dir', xtype=ncd_double,  dim1name='landunit',         &
!#py          dim2name='numrad', switchdim=.true.,                                                                         &
!#py          long_name='direct solar absorbed by pervious road per unit ground area per unit incident flux', units='',    &
!#py          interpinic_flag='interp', readvar=readvar, data=this%sabs_perroad_dir_lun)
!#py 
!#py     call restartvar(ncid=ncid, flag=flag, varname='sabs_perroad_dif', xtype=ncd_double,  dim1name='landunit',         &
!#py          dim2name='numrad', switchdim=.true.,                                                                         &
!#py          long_name='diffuse solar absorbed by pervious road per unit ground area per unit incident flux', units='',   &
!#py          interpinic_flag='interp', readvar=readvar, data=this%sabs_perroad_dif_lun)
!#py 
!#py !#py !#py   end subroutine Restart


end module SolarAbsorbedType
