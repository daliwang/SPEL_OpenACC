module soilorder_varcon

  !-----------------------------------------------------------------------
  ! !DESCRIPTION:
  ! Module containing vegetation constants and method to
  ! read and initialize vegetation (PFT) constants.
  !
  ! !USES:
  use shr_kind_mod, only : r8 => shr_kind_r8
  !#py !#py use shr_log_mod , only : errMsg => shr_log_errMsg
  !#py use abortutils  , only : endrun
  use elm_varpar  , only : nsoilorder
  use elm_varctl  , only : iulog, use_vertsoilc
  !
  ! !PUBLIC TYPES:
  implicit none
  save

!
! Soil order  constants
!
  character(len=40) soilordername(0:nsoilorder)    ! soil order description
  integer :: Water
  integer :: Andisols
  integer :: Gelisols
  integer :: Histosols
  integer :: Entisols
  integer :: Inceptisols
  integer :: Aridsols
  integer :: Vertisols
  integer :: Mollisols
  integer :: Alfisols
  integer :: Spodosols
  integer :: Ultisols
  integer :: Oxisols
  integer :: Shifting_sand
  integer :: rock_land
  integer :: Ice_Glacier

  real(r8), pointer :: smax(:)
  real(r8), pointer :: ks_sorption(:)
  real(r8), pointer :: r_weather(:)
  real(r8), pointer :: r_adsorp(:)
  real(r8), pointer :: r_desorp(:)
  real(r8), pointer :: r_occlude(:)
  real(r8), pointer :: k_s1_biochem(:)
  real(r8), pointer :: k_s2_biochem(:)
  real(r8), pointer :: k_s3_biochem(:)
  real(r8), pointer :: k_s4_biochem(:)

  !+
  real(r8), pointer :: r_mort_soilorder(:)
  !$acc declare create(smax(:)       )
  !$acc declare create(ks_sorption(:))
  !$acc declare create(r_weather(:)  )
  !$acc declare create(r_adsorp(:)   )
  !$acc declare create(r_desorp(:)   )
  !$acc declare create(r_occlude(:)  )
  !$acc declare create(k_s1_biochem(:))
  !$acc declare create(k_s2_biochem(:))
  !$acc declare create(k_s3_biochem(:))
  !$acc declare create(k_s4_biochem(:))

  !$acc declare create(r_mort_soilorder)


  ! !PUBLIC MEMBER FUNCTIONS:
  !#py public :: soilorder_conrd ! Read and initialize soil order dependent  constants
  !
  ! !REVISION HISTORY:
  ! Created by X.YANG 01/12/2015
  !-----------------------------------------------------------------------

contains

  !-----------------------------------------------------------------------
!#py   subroutine soilorder_conrd
!#py     !
!#py     ! !DESCRIPTION:
!#py     ! Read and initialize soil order dependent constants
!#py     !
!#py     ! !USES:
!#py     use shr_infnan_mod , only : nan => shr_infnan_nan, assignment(=)
!#py     use fileutils      , only : getfil
!#py     use ncdio_pio      , only : ncd_io, ncd_pio_closefile, ncd_pio_openfile,file_desc_t, &
!#py                             ncd_inqdid, ncd_inqdlen
!#py     use elm_varctl     , only : fsoilordercon
!#py     use spmdMod        , only : masterproc
!#py 
!#py     ! !ARGUMENTS:
!#py     implicit none
!#py     !
!#py     ! !REVISION HISTORY:
!#py     ! Created by Gordon Bonan
!#py     ! F. Li and S. Levis (11/06/12)
!#py     !
!#py     ! !LOCAL VARIABLES:
!#py     character(len=256) :: locfn ! local file name
!#py     integer :: i,n              ! loop indices
!#py     integer :: ier              ! error code
!#py     type(file_desc_t) :: ncid   ! pio netCDF file id
!#py     integer :: dimid            ! netCDF dimension id
!#py     integer :: nsoil             ! number of pfts on pft-physiology file
!#py     logical :: readv            ! read variable in or not
!#py     character(len=32) :: subname = 'soilorder_conrd'              ! subroutine name
!#py 
!#py     ! Expected soil order names: The names expected on the  file and the order
!#py     ! they are expected to be in.
!#py     !
!#py     character(len=40), parameter :: expected_soilnames(0:nsoilorder) = (/ &
!#py                  'Water                      '  &
!#py                , 'Andisols                   '  &
!#py                , 'Gelisols                   '  &
!#py                , 'Histosols                  '  &
!#py                , 'Entisols                   '  &
!#py                , 'Inceptisols                '  &
!#py                , 'Aridsols                   '  &
!#py                , 'Vertisols                  '  &
!#py                , 'Mollisols                  '  &
!#py                , 'Alfisols                   '  &
!#py                , 'Spodosols                  '  &
!#py                , 'Ultisols                   '  &
!#py                , 'Oxisols                    '  &
!#py                , 'shiftingsand               '  &
!#py                , 'rockland                   '  &
!#py                , 'iceglacier                 '  &
!#py     /)
!#py !-----------------------------------------------------------------------
!#py 
!#py     allocate( smax        (0:nsoilorder) ); smax         (:) = nan
!#py     allocate( ks_sorption (0:nsoilorder) ); ks_sorption  (:) = nan
!#py     allocate( r_weather   (0:nsoilorder) ); r_weather    (:) = nan
!#py     allocate( r_adsorp    (0:nsoilorder) ); r_adsorp     (:) = nan
!#py     allocate( r_desorp    (0:nsoilorder) ); r_desorp     (:) = nan
!#py     allocate( r_occlude   (0:nsoilorder) ); r_occlude    (:) = nan
!#py     allocate(k_s1_biochem (0:nsoilorder) ); k_s1_biochem (:) = nan
!#py     allocate(k_s2_biochem (0:nsoilorder) ); k_s2_biochem (:) = nan
!#py     allocate(k_s3_biochem (0:nsoilorder) ); k_s3_biochem (:) = nan
!#py     allocate(k_s4_biochem (0:nsoilorder) ); k_s4_biochem (:) = nan
!#py     allocate(r_mort_soilorder (0:nsoilorder) ); r_mort_soilorder (:) = nan
!#py 
!#py    ! Set specific soil order values
!#py 
!#py     if (masterproc) then
!#py        write(iulog,*) 'Attempting to read soil order dependent parameters .....'
!#py     end if
!#py     call getfil (fsoilordercon, locfn, 0)
!#py     call ncd_pio_openfile (ncid, trim(locfn), 0)
!#py     call ncd_inqdid(ncid,'soilorder',dimid)
!#py     call ncd_inqdlen(ncid,dimid,nsoil)
!#py 
!#py     call ncd_io('soilordername',soilordername, 'read', ncid, readvar=readv,posNOTonfile=.true.)
!#py     if ( .not. readv ) call endrun(msg=' ERROR: error in reading in soil order parameter'//errMsg(__FILE__, __LINE__))
!#py     call ncd_io('r_weather',r_weather, 'read', ncid, readvar=readv)
!#py     if ( .not. readv ) call endrun(msg=' ERROR: error in reading in soil order parameter'//errMsg(__FILE__, __LINE__))
!#py     call ncd_io('r_adsorp',r_adsorp, 'read', ncid, readvar=readv)
!#py     if ( .not. readv ) call endrun(msg=' ERROR: error in reading in soil order parameter'//errMsg(__FILE__, __LINE__))
!#py     call ncd_io('r_desorp',r_desorp, 'read', ncid, readvar=readv)
!#py     if ( .not. readv ) call endrun(msg=' ERROR: error in reading in soil order parameter'//errMsg(__FILE__, __LINE__))
!#py     call ncd_io('r_occlude',r_occlude, 'read', ncid, readvar=readv)
!#py     if ( .not. readv ) call endrun(msg=' ERROR: error in reading in soil order parameter'//errMsg(__FILE__, __LINE__))
!#py     call ncd_io('k_s1_biochem',k_s1_biochem, 'read', ncid, readvar=readv)
!#py     if ( .not. readv ) call endrun(msg=' ERROR: error in reading in soil order parameter'//errMsg(__FILE__, __LINE__))
!#py     call ncd_io('k_s2_biochem',k_s2_biochem, 'read', ncid, readvar=readv)
!#py     if ( .not. readv ) call endrun(msg=' ERROR: error in reading in soil order parameter'//errMsg(__FILE__, __LINE__))
!#py     call ncd_io('k_s3_biochem',k_s3_biochem, 'read', ncid, readvar=readv)
!#py     if ( .not. readv ) call endrun(msg=' ERROR: error in reading in soil order parameter'//errMsg(__FILE__, __LINE__))
!#py     call ncd_io('k_s4_biochem',k_s4_biochem, 'read', ncid, readvar=readv)
!#py     if ( .not. readv ) call endrun(msg=' ERROR: error in reading in soil order parameter'//errMsg(__FILE__, __LINE__))
!#py     call ncd_io('smax',smax, 'read', ncid, readvar=readv)
!#py     if ( .not. readv ) call endrun(msg=' ERROR: error in reading in soil order parameter'//errMsg(__FILE__, __LINE__))
!#py     call ncd_io('ks_sorption',ks_sorption, 'read', ncid, readvar=readv)
!#py     if ( .not. readv ) call endrun(msg=' ERROR: error in reading in soil order parameter'//errMsg(__FILE__, __LINE__))
!#py     call ncd_io('r_mort_soilorder',r_mort_soilorder, 'read', ncid, readvar=readv)
!#py     if ( .not. readv ) r_mort_soilorder(:) = 0.02_r8 !call endrun(msg=' ERROR: error in reading in soil order parameter'//errMsg(__FILE__, __LINE__))
!#py 
!#py     call ncd_pio_closefile(ncid)
!#py 
!#py 
!#py 
!#py     do i = 1,nsoilorder
!#py 
!#py        if ( trim(adjustl(soilordername(i))) /= trim(expected_soilnames(i)) )then
!#py           write(iulog,*)'soilorder_conrd: soil order name is NOT what is expected, name = ', &
!#py                         trim(soilordername(i)), ', expected name = ',trim(expected_soilnames(i))
!#py           call endrun( 'soilorder_conrd: bad name for soil order onfsoilordercon dataset' )
!#py        end if
!#py 
!#py     enddo
!#py 
!#py     if (masterproc) then
!#py        write(iulog,*) 'Successfully read soil order dependent parameters'
!#py        write(iulog,*)
!#py     end if
!#py 
!#py    end subroutine soilorder_conrd


end module soilorder_varcon
