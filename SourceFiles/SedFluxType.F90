module SedFluxType
  #define nan spval
  #define masterproc 0
  !------------------------------------------------------------------------------
  ! !DESCRIPTION:
  ! Hold sediment, POC, PON and POP dynamic fluxes induced by soil erosion 
  !
  use shr_kind_mod      , only : r8 => shr_kind_r8
  !#py use shr_log_mod       , only : errMsg => shr_log_errMsg
  use elm_varcon        , only : spval
  use decompMod         , only : bounds_type
  !#py use abortutils        , only : endrun
  use ColumnType        , only : col_pp
  !
  implicit none
  save
  private
  !
  ! !PUBLIC TYPES:
  type, public :: sedflux_type

     ! Soil erosion model parameters
     real(r8), pointer :: pfactor_col(:)            ! col rainfall-driven erosion scaling factor
     real(r8), pointer :: qfactor_col(:)            ! col runoff-driven erosion scaling factor
     real(r8), pointer :: tfactor_col(:)            ! col transport capacity scaling factor
     
     ! Soil erosion fluxes
     real(r8), pointer :: sed_p_ero_col(:)          ! col sed detach driven by rainfall (kg/m2/s)
     real(r8), pointer :: sed_q_ero_col(:)          ! col sed detach driven by runoff (kg/m2/s)
     real(r8), pointer :: sed_ero_col(:)            ! col total sed detach (kg/m2/s)
     real(r8), pointer :: sed_crop_ero_col(:)       ! col sed detach on cropland (kg/m2/s) 
     real(r8), pointer :: sed_yld_col(:)            ! col total sed yield (kg/m2/s) 

  contains

     procedure, public  :: Init
     procedure, private :: InitAllocate
     procedure, private :: InitHistory
     procedure, private :: InitCold
     procedure, public  :: Restart

  end type sedflux_type
  !------------------------------------------------------------------------

contains

  !------------------------------------------------------------------------
  subroutine Init(this, bounds)
    
    class(sedflux_type)            :: this
    type(bounds_type) , intent(in) :: bounds

    call this%InitAllocate ( bounds )
    call this%InitHistory ( bounds )
    call this%InitCold ( bounds )

  end subroutine Init

  !------------------------------------------------------------------------
  subroutine InitAllocate(this, bounds)
    !
    ! !DESCRIPTION:
    ! Initialize module data structure
    !
    ! !USES:
    !#py use shr_infnan_mod , only : nan => shr_infnan_nan, assignment(=)
    !
    ! !ARGUMENTS:
    class(sedflux_type) :: this
    type(bounds_type), intent(in) :: bounds
    !
    ! !LOCAL VARIABLES:
    integer :: begc, endc
    !------------------------------------------------------------------------

    begc = bounds%begc; endc= bounds%endc

    allocate( this%sed_p_ero_col       (begc:endc))      ; this%sed_p_ero_col          (:) = nan
    allocate( this%sed_q_ero_col       (begc:endc))      ; this%sed_q_ero_col          (:) = nan
    allocate( this%sed_ero_col         (begc:endc))      ; this%sed_ero_col            (:) = nan
    allocate( this%sed_crop_ero_col    (begc:endc))      ; this%sed_crop_ero_col       (:) = nan 
    allocate( this%sed_yld_col         (begc:endc))      ; this%sed_yld_col            (:) = nan

    allocate( this%pfactor_col         (begc:endc))      ; this%pfactor_col            (:) = nan
    allocate( this%qfactor_col         (begc:endc))      ; this%qfactor_col            (:) = nan
    allocate( this%tfactor_col         (begc:endc))      ; this%tfactor_col            (:) = nan

  end subroutine InitAllocate

  !------------------------------------------------------------------------
  subroutine InitHistory(this, bounds)
    !
    ! !DESCRIPTION:
    ! Initialize module data structure
    !
    ! !USES:
    !#py use histFileMod    , only : hist_addfld1d
    !#py use shr_infnan_mod , only : nan => shr_infnan_nan, assignment(=)
    !
    ! !ARGUMENTS:
    class(sedflux_type) :: this
    type(bounds_type), intent(in) :: bounds
    !
    ! !LOCAL VARIABLES:
    integer           :: begc, endc
    character(24)     :: fieldname
    character(100)    :: longname
    real(r8), pointer :: data1dptr(:)   ! temp. pointer for slicing larger arrays
    !------------------------------------------------------------------------

    begc = bounds%begc; endc= bounds%endc

    this%sed_p_ero_col(begc:endc) = spval
    !#py call hist_addfld1d (fname='SED_P_ERO',  units='kg/m^2/s',  &
         !#py avgflag='A', long_name='hillslope rainfall-driven erosion', &
         !#py ptr_col=this%sed_p_ero_col, l2g_scale_type='veg', &
         !#py !#py default='inactive')

    this%sed_q_ero_col(begc:endc) = spval
    !#py call hist_addfld1d (fname='SED_Q_ERO',  units='kg/m^2/s',  &
         !#py avgflag='A', long_name='hillslope runoff-driven erosion', &
         !#py ptr_col=this%sed_q_ero_col, l2g_scale_type= 'veg', &
         !#py !#py default='inactive')

    this%sed_ero_col(begc:endc) = spval
    !#py call hist_addfld1d (fname='SED_ERO',  units='kg/m^2/s',  &
         !#py avgflag='A', long_name='hillslope total erosion', &
         !#py ptr_col=this%sed_ero_col, l2g_scale_type= 'veg', &
         !#py !#py default='inactive')

    this%sed_crop_ero_col(begc:endc) = spval
    !#py call hist_addfld1d (fname='SED_CROP_ERO',  units='kg/m^2/s',  &
         !#py avgflag='A', long_name='hillslope cropland erosion', &
         !#py ptr_col=this%sed_crop_ero_col, l2g_scale_type= 'veg', &
         !#py !#py default='inactive')

    this%sed_yld_col(begc:endc) = spval
    !#py call hist_addfld1d (fname='SED_YLD',  units='kg/m^2/s',  &
         !#py avgflag='A', long_name='hillslope total sediment yield', &
         !#py ptr_col=this%sed_yld_col, l2g_scale_type= 'veg', &
         !#py !#py default='inactive')

  end subroutine InitHistory

  !-----------------------------------------------------------------------
  subroutine InitCold(this, bounds)
    !
    ! !DESCRIPTION:
    ! Initialize cold start conditions for module variables
    !
    ! !USES:
    use shr_kind_mod   , only : r8 => shr_kind_r8
    use elm_varcon     , only : grlnd
    use elm_varctl     , only : fsurdat
    !#py use fileutils      , only : getfil
    !#py use ncdio_pio      , only : file_desc_t, ncd_io, ncd_pio_openfile, ncd_pio_closefile
    !
    ! !ARGUMENTS:
    class(sedflux_type)         :: this
    type(bounds_type) , intent(in) :: bounds
    !
    ! !LOCAL VARIABLES:
    integer            :: c, g
    logical            :: readvar
    !type(file_desc_t)  :: ncid
    character(len=256) :: locfn
    real(r8) ,pointer  :: pfactor2d       (:)      ! read in - pfactor
    real(r8) ,pointer  :: qfactor2d       (:)      ! read in - qfactor
    real(r8) ,pointer  :: tfactor2d       (:)      ! read in - tfactor
    !-----------------------------------------------------------------------

    allocate(pfactor2d(bounds%begg:bounds%endg))
    allocate(qfactor2d(bounds%begg:bounds%endg))
    allocate(tfactor2d(bounds%begg:bounds%endg))

    !#py call getfil (fsurdat, locfn, 0)
    !#py call ncd_pio_openfile (ncid, locfn, 0)
    !#py !#py call ncd_io(ncid=ncid, varname='parEro_c1', flag='read', data=pfactor2d, dim1name=grlnd, readvar=readvar)
    if (.not. readvar) pfactor2d(:) = 0._r8
    !#py !#py call ncd_io(ncid=ncid, varname='parEro_c2', flag='read', data=qfactor2d, dim1name=grlnd, readvar=readvar)
    if (.not. readvar) qfactor2d(:) = 0._r8 
    !#py !#py call ncd_io(ncid=ncid, varname='parEro_c3', flag='read', data=tfactor2d, dim1name=grlnd, readvar=readvar)
    if (.not. readvar) tfactor2d(:) = 0._r8 
    !#py call ncd_pio_closefile(ncid)

    do c = bounds%begc, bounds%endc
       this%sed_p_ero_col(c)                 = 0._r8
       this%sed_q_ero_col(c)                 = 0._r8
       this%sed_ero_col(c)                   = 0._r8
       this%sed_crop_ero_col(c)              = 0._r8
       this%sed_yld_col(c)                   = 0._r8

       g = col_pp%gridcell(c)
       this%pfactor_col(c)                   = pfactor2d(g)
       this%qfactor_col(c)                   = qfactor2d(g)
       this%tfactor_col(c)                   = tfactor2d(g)
    end do

    deallocate(pfactor2d, qfactor2d, tfactor2d)

  end subroutine InitCold

  !------------------------------------------------------------------------
  subroutine Restart(this, bounds,  flag)
    ! 
    ! !DESCRIPTION:
    ! Read/Write module information to/from restart file.
    !
    ! !USES:
    !#py use ncdio_pio       , only : file_desc_t
    !
    ! !ARGUMENTS:
    class(sedflux_type) :: this
    type(bounds_type), intent(in)    :: bounds
    !type(file_desc_t), intent(inout) :: ncid
    character(len=*) , intent(in)    :: flag
    !
    ! !LOCAL VARIABLES:
    !-----------------------------------------------------------------------

    ! do nothing

  end subroutine Restart

end module SedFluxType
