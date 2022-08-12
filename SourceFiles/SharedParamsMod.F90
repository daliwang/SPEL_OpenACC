module SharedParamsMod

  !-----------------------------------------------------------------------
  !
  ! !USES:
  use shr_kind_mod , only: r8 => shr_kind_r8
  implicit none
  save

  ! ParamsShareInst.  PGI wants the type decl. public but the instance
  ! is indeed protected.  A generic private statement at the start of the module
  ! overrides the protected functionality with PGI

  type, public :: ParamsShareType

      real(r8), pointer :: Q10_mr                => null() ! temperature dependence for maintenance respiraton
      real(r8), pointer :: Q10_hr                => null() ! temperature dependence for heterotrophic respiration
      real(r8), pointer :: minpsi                => null() ! minimum soil water potential for heterotrophic resp
      real(r8), pointer :: cwd_fcel              => null() ! cellulose fraction of coarse woody debris
      real(r8), pointer :: cwd_flig              => null() ! lignin fraction of coarse woody debris
      real(r8), pointer :: froz_q10              => null() ! separate q10 for frozen soil respiration rates
      real(r8), pointer :: decomp_depth_efolding => null() ! e-folding depth for reduction in decomposition (m)
      real(r8), pointer :: mino2lim              => null() ! minimum anaerobic decomposition rate as a fraction of potential aerobic rate
      real(r8), pointer :: organic_max           => null() ! organic matter content (kg/m3) where soil is assumed to act like peat

  end type ParamsShareType

  type(ParamsShareType), public :: ParamsShareInst

  !$acc declare create(ParamsShareInst)
  logical, public :: anoxia_wtsat = .false.
  integer, public :: nlev_soildecomp_standard = 5
  !$acc declare create(anoxia_wtsat)
  !$acc declare create(nlev_soildecomp_standard)

  !-----------------------------------------------------------------------

contains

  !-----------------------------------------------------------------------
   subroutine ParamsReadShared()
!#py      !
!#py      use ncdio_pio   , only : file_desc_t,ncd_io
!#py      use abortutils  , only : endrun
!#py      use shr_log_mod , only : errMsg => shr_log_errMsg
!#py      !
     implicit none
!#py      type(file_desc_t),intent(inout) :: ncid   ! pio netCDF file id
!#py      !
!#py      character(len=32)  :: subname = 'ParamsReadShared'
!#py      character(len=100) :: errCode = '-Error reading in CN and BGC shared params file. Var:'
!#py      logical            :: readv ! has variable been read in or not
!#py      real(r8)           :: tempr ! temporary to read in parameter
!#py      character(len=100) :: tString ! temp. var for reading
!#py      !-----------------------------------------------------------------------
!#py      !
!#py      ! netcdf read here
!#py      !
     allocate(ParamsShareInst%Q10_mr               )
     allocate(ParamsShareInst%Q10_hr               )
     allocate(ParamsShareInst%minpsi               )
     allocate(ParamsShareInst%cwd_fcel             )
     allocate(ParamsShareInst%cwd_flig             )
     allocate(ParamsShareInst%froz_q10             )
     allocate(ParamsShareInst%decomp_depth_efolding)
     allocate(ParamsShareInst%mino2lim             )
     allocate(ParamsShareInst%organic_max          )
!#py      tString='q10_mr'
!#py      call ncd_io(varname=trim(tString),data=tempr, flag='read', ncid=ncid, readvar=readv)
!#py      if ( .not. readv ) call endrun(msg=trim(errCode)//trim(tString)//errMsg(__FILE__, __LINE__))
!#py      ParamsShareInst%Q10_mr=tempr
!#py
!#py      tString='q10_hr'
!#py      call ncd_io(varname=trim(tString),data=tempr, flag='read', ncid=ncid, readvar=readv)
!#py      if ( .not. readv ) call endrun(msg=trim(errCode)//trim(tString)//errMsg(__FILE__, __LINE__))
!#py      ParamsShareInst%Q10_hr=tempr
!#py
!#py
!#py      tString='minpsi_hr'
!#py      call ncd_io(trim(tString),tempr, 'read', ncid, readvar=readv)
!#py      if ( .not. readv ) call endrun(msg=trim(errCode)//trim(tString)//errMsg(__FILE__, __LINE__))
!#py      ParamsShareInst%minpsi=tempr
!#py
!#py      tString='cwd_fcel'
!#py      call ncd_io(trim(tString),tempr, 'read', ncid, readvar=readv)
!#py      if ( .not. readv ) call endrun(msg=trim(errCode)//trim(tString)//errMsg(__FILE__, __LINE__))
!#py      ParamsShareInst%cwd_fcel=tempr
!#py
!#py      tString='cwd_flig'
!#py      call ncd_io(trim(tString),tempr, 'read', ncid, readvar=readv)
!#py      if ( .not. readv ) call endrun(msg=trim(errCode)//trim(tString)//errMsg(__FILE__, __LINE__))
!#py      ParamsShareInst%cwd_flig=tempr
!#py
!#py      tString='froz_q10'
!#py      call ncd_io(trim(tString),tempr, 'read', ncid, readvar=readv)
!#py      if ( .not. readv ) call endrun(msg=trim(errCode)//trim(tString)//errMsg(__FILE__, __LINE__))
!#py      ParamsShareInst%froz_q10=tempr
!#py
!#py      tString='decomp_depth_efolding'
!#py      call ncd_io(trim(tString),tempr, 'read', ncid, readvar=readv)
!#py      if ( .not. readv ) call endrun(msg=trim(errCode)//trim(tString)//errMsg(__FILE__, __LINE__))
!#py      ParamsShareInst%decomp_depth_efolding=tempr
!#py
!#py      tString='mino2lim'
!#py      call ncd_io(trim(tString),tempr, 'read', ncid, readvar=readv)
!#py      if ( .not. readv ) call endrun(msg=trim(errCode)//trim(tString)//errMsg(__FILE__, __LINE__))
!#py      ParamsShareInst%mino2lim=tempr
!#py      !ParamsShareInst%mino2lim=0.2_r8
!#py
!#py      tString='organic_max'
!#py      call ncd_io(trim(tString),tempr, 'read', ncid, readvar=readv)
!#py      if ( .not. readv ) call endrun(msg=trim(errCode)//trim(tString)//errMsg(__FILE__, __LINE__))
!#py      ParamsShareInst%organic_max=tempr
!#py
  end subroutine ParamsReadShared

end module SharedParamsMod
