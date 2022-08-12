module dynVarMod

!#py #include "shr_assert.h"

  !---------------------------------------------------------------------------
  ! !DESCRIPTION:
  ! Contains a derived type and associated methods for working with a single dynamic
  ! subgrid variable, which may or may not need time interpolation.
  !
  ! This is an abstract type that needs to be extended by a specific derived type
  ! (dyn_var_time_uninterp_type or dyn_var_time_interp_type). Besides the lack of
  ! definition of some methods, also note that it does NOT define the data
  ! themselves. This is because different type extensions have different needs for what
  ! data are stored - and particularly whether they need data at just time_index_lower, or data at both
  ! time_index_lower and time_index_upper.
  !
  ! The use of this class (or its extensions) is:
  !   In initialization:
  !     - create a new object using the appropriate constructor (a constructor for one of
  !       the children of dyn_var_type)
  !     - call get_current_data to get the initial data
  !
  !   Each time through the run loop:
  !     - call get_current_data to get the current value of the data
  !
  ! Note that, because of the reads that are done here, the methods of this class should
  ! NOT be called from inside threaded regions or loops over clumps
  !
  ! !USES:
  use shr_kind_mod   , only : r8 => shr_kind_r8
  use clm_varctl     , only : iulog
  !#py use dynFileMod     , only : dyn_file_type
  use dynTimeInfoMod , only : time_info_type
  !#py !#py use shr_log_mod    , only : errMsg => shr_log_errMsg
  !#py use abortutils     , only : endrun
  implicit none
  save
  private

  !
  ! !PUBLIC TYPES:

  ! maximum number of real dimensions allowed for the underlying variables stored in
  ! dyn_var_type
  integer, parameter, public :: dyn_var_max_dims = 2

  public :: dyn_var_type

  ! This is an abstract base class that should be extended.
  !
  ! Any type extension needs to define the following methods:
  ! - get_current_data_1d
  ! - get_current_data_2d
  ! - get_current_att
  ! - read_data_if_needed
  !
  ! In addition, a type extension needs to define the data field(s); these are not defined
  ! in the base class because different type extensions have different needs for what data
  ! are stored - and particularly whether they need data at just time_index_lower, or data at both time_index_lower
  ! and time_index_upper.
  !
  ! Although this base class doesn't define the data themselves, note that the
  ! implementation here assumes that the data will be stored as a 1-d vector, even if the
  ! data are truly 2-d. i.e., the type extension should define its data as:
  ! real(r8), allocatable :: data_at_tlower(:)
  type, abstract :: dyn_var_type
     private
     type(dyn_file_type), pointer :: dyn_file  ! pointer to the file containing this variable
     character(len=256) :: varname             ! variable name on file
     character(len=256) :: dim1name            ! dim1name on file
     real(r8) :: conversion_factor             ! data are DIVIDED by conversion_factor immediately after reading them

     ! Only relevant for 2-d variables: should we check to make sure that all sums equal 1?
     logical :: do_check_sums_equal_1

     ! Shape of data; max number of dimensions is given by dyn_var_max_dims in dynVarMod.
     ! First dimension is the spatial dimension.
     ! This is a pointer rather than an allocatable to work around a pgi compiler bug
     ! (pgi 13.9)
     integer, pointer :: data_shape(:)

     logical :: allow_nodata                   ! Flag to allow no data on the file or if should die

     logical :: data_on_file                   ! Flag to indicate if the data is on the file or not
   contains
     ! Public methods:

     ! The following are public only for the sake of type extensions of this base class;
     ! they should not be used outside of these type extensions
     procedure :: set_metadata   ! Set metadata that are common to all type extensions of this base class
     procedure :: get_dyn_file   ! Get the dyn_file component
     procedure :: get_data_shape ! Get the data_shape component
     !#py procedure :: get_att        ! Get the variable attribute
     !#py procedure :: read_variable  ! Wrapper to read a time slice of the variable

     ! The following need to be defined by any type extensions; they need to be public so
     ! they can be overridden, but they should not be used outside of type extensions
     procedure(get_current_data_1d_interface), deferred :: get_current_data_1d  ! Get the current value of the data, for a 1-d variable
     procedure(get_current_data_2d_interface), deferred :: get_current_data_2d  ! Get the current value of the daat, for a 2-d variable
     procedure(read_data_if_needed_interface), deferred :: read_data_if_needed  ! Read the next time slice of data, if necessary

     ! Private methods:
     !#py procedure, private :: read_variable_1d     ! Read a time slice of a 1-d variable
     !#py procedure, private :: read_variable_2d     ! Read a time slice of a 2-d variable
  end type dyn_var_type

  abstract interface

     subroutine read_data_if_needed_interface(this)
       ! !DESCRIPTION:
       ! Determine if new data need to be read from the file; if so, read them.
       !
       ! !USES:
       import :: dyn_var_type
       !
       ! !ARGUMENTS:
       class(dyn_var_type), intent(inout) :: this  ! this object
     end subroutine read_data_if_needed_interface

     ! DIMS 1,2
     subroutine get_current_data_{DIMS}d_interface(this, cur_data)
       ! !DESCRIPTION:
       ! Get the current (possibly interpolated) value of the data, in cur_data. cur_data
       ! should have the same dimensionality as the underlying data, as given by the
       ! data_shape argument that was passed to the constructor.
       !
       ! If necessary, new data are read from the file.
       !
       ! Should be called once per time step, AFTER calling set_current_year on the
       ! underlying dyn_file variable
       !
       ! !USES:
       use shr_kind_mod   , only : r8 => shr_kind_r8
       import :: dyn_var_type
       !
       ! !ARGUMENTS:
       class(dyn_var_type) , intent(inout) :: this             ! this object
       real(r8)            , intent(out)   :: cur_data{DIMSTR} ! current value of data
     end subroutine get_current_data_{DIMS}d_interface

  end interface


contains

  ! ======================================================================
  ! Public methods
  !
  ! The following are public only for the sake of type extensions of this base class.
  ! They should not be used outside these type extensions.
  ! ======================================================================

  !-----------------------------------------------------------------------
  subroutine set_metadata(this, dyn_file, varname, dim1name, conversion_factor, &
       do_check_sums_equal_1, data_shape, allow_nodata)
    !
    ! !DESCRIPTION:
    ! Set metadata that are common to all type extensions of this base class
    !
    ! !USES:
    !
    ! !ARGUMENTS:
    class(dyn_var_type) , intent(inout)      :: this              ! this object
    type(dyn_file_type) , target, intent(in) :: dyn_file          ! file containing this variable
    character(len=*)    , intent(in)         :: varname           ! variable name on file
    character(len=*)    , intent(in)         :: dim1name          ! dim1name on file
    real(r8)            , intent(in)         :: conversion_factor ! data are DIVIDED by conversion_factor immediately after reading them
    logical, optional   , intent(in)         :: allow_nodata      ! Flag to indicate if it's allowed to NOT have data on the file

     ! Only relevant for 2-d variables: should we check to make sure that all sums equal 1?
    logical, intent(in) :: do_check_sums_equal_1

    ! Shape of data; max number of dimensions is given by dyn_var_max_dims in dynVarMod.
    ! First dimension is the spatial dimension.
    integer, intent(in) :: data_shape(:)

    !
    ! !LOCAL VARIABLES:
    integer :: ndims   ! number of dimensions in data

    character(len=*), parameter :: subname = 'set_metadata'
    !-----------------------------------------------------------------------

    ndims = size(data_shape)

    ! Do some error checking on the inputs
    SHR_ASSERT(ndims <= dyn_var_max_dims, subname//' ERROR: ndims must be <= dyn_var_max_dims')
    if (do_check_sums_equal_1) then
       SHR_ASSERT(ndims == 2, subname//' ERROR: do_check_sums_equal_1 only valid for ndims==2')
    end if

    ! Set metadata for this variable
    this%dyn_file => dyn_file
    this%varname = varname
    this%dim1name = dim1name
    this%conversion_factor = conversion_factor
    this%do_check_sums_equal_1 = do_check_sums_equal_1
    allocate(this%data_shape(ndims))
    this%data_shape = data_shape

    if ( present(allow_nodata) )then
       this%allow_nodata = allow_nodata
    else
       this%allow_nodata = .false.
    end if
    this%data_on_file = .false.

  end subroutine set_metadata


  !-----------------------------------------------------------------------
  function get_dyn_file(this)
    !
    ! !DESCRIPTION:
    ! Get the dyn_file component of this object
    !
    ! !ARGUMENTS:
    type(dyn_file_type) , pointer    :: get_dyn_file
    class(dyn_var_type) , intent(in) :: this ! this object
    !-----------------------------------------------------------------------
    get_dyn_file => this%dyn_file
  end function get_dyn_file

  !-----------------------------------------------------------------------
  function get_data_shape(this)
    !
    ! !DESCRIPTION:
    ! Get the data_shape component of this object
    !
    ! !ARGUMENTS:
    integer             , allocatable :: get_data_shape(:)
    class(dyn_var_type) , intent(in)  :: this ! this object
    !-----------------------------------------------------------------------
    allocate(get_data_shape(size(this%data_shape)))
    get_data_shape = this%data_shape
  end function get_data_shape

  !-----------------------------------------------------------------------
!#py   subroutine get_att(this,attname,attvalue)
!#py     !
!#py     ! !DESCRIPTION:
!#py     ! Get the the value of an attribute on the file
!#py     !
!#py     ! !USES:
!#py     use ncdio_pio      , only : ncd_inqvid, ncd_getatt, var_desc_t
!#py     ! !ARGUMENTS:
!#py     class(dyn_var_type) , intent(inout):: this     ! this object
!#py     character(len=*) ,    intent(in)   :: attname  ! name of the attribute
!#py     character(len=*) ,    intent(out)  :: attvalue ! value of the attribute
!#py     ! !LOCAL VARIABLES:
!#py     integer                     :: varid     ! variable id
!#py     type(var_desc_t)            :: vardesc   ! variable descriptor
!#py     character(len=*), parameter :: subname = 'get_att'
!#py     !-----------------------------------------------------------------------
!#py     call ncd_inqvid(this%dyn_file, this%varname, varid, vardesc)
!#py     call ncd_getatt(this%dyn_file, varid, attname, attvalue)
!#py   end subroutine get_att

  !-----------------------------------------------------------------------
!#py   subroutine read_variable(this, nt, data)
!#py     !
!#py     ! !DESCRIPTION:
!#py     ! Wrapper to read a time slice of the variable; result goes in the data argument.
!#py     !
!#py     ! !USES:
!#py     use spmdMod   , only : masterproc
!#py     !
!#py     ! !ARGUMENTS:
!#py     class(dyn_var_type) , intent(inout) :: this    ! this object
!#py     integer             , intent(in)    :: nt      ! time index to read
!#py     real(r8)            , intent(out)   :: data(:) ! variable holding data read from file
!#py     !
!#py     ! !LOCAL VARIABLES:
!#py     integer :: ndims  ! number of dimensions of the underlying variable
!#py
!#py     character(len=*), parameter :: subname = 'read_variable'
!#py     !-----------------------------------------------------------------------
!#py
!#py     if (masterproc) then
!#py        write(iulog,*) 'Get data for variable ', trim(this%varname), ' for year ', &
!#py             this%dyn_file%time_info%get_year(nt)
!#py     end if
!#py
!#py     ndims = size(this%data_shape)
!#py     if (ndims == 1) then
!#py        call read_variable_1d(this, nt, data)
!#py     else if (ndims == 2) then
!#py        call read_variable_2d(this, nt, data)
!#py     else
!#py        call endrun(msg='ERROR: read_variable can only handle 1 or 2 dimensions'//&
!#py             errMsg(__FILE__, __LINE__))
!#py     end if
!#py
!#py   end subroutine read_variable


  ! ======================================================================
  ! Private methods
  ! ======================================================================

  ! DIMS 1,2
  !-----------------------------------------------------------------------
!#py   subroutine read_variable_{DIMS}d(this, nt, data)
!#py     !
!#py     ! !DESCRIPTION:
!#py     ! Read a time slice of a {DIMS}-d variable
!#py     ! This routine applies the conversion given by conversion_factor.
!#py     !
!#py     ! !USES:
!#py     use spmdMod        , only : masterproc
!#py     use ncdio_pio      , only : ncd_io
!#py     use surfrdUtilsMod , only : check_sums_equal_1
!#py     !
!#py     ! !ARGUMENTS:
!#py     class(dyn_var_type) , intent(inout) :: this ! this object (needs to be intent(inout) because this%dynfile is intent(inout) in the ncd_io call)
!#py     integer             , intent(in)    :: nt   ! time index to read
!#py     ! variable holding data read from file (note that this is 1-d regardless of the
!#py     ! dimensionality of the underlying data)
!#py     real(r8)            , intent(out) :: data(:)
!#py     !
!#py     ! !LOCAL VARIABLES:
!#py     real(r8), pointer :: arrayl{DIMSTR}  ! temporary array to hold data (needs to be a pointer)
!#py     logical           :: readvar         ! whether variable was read
!#py     logical           :: die_on_error    ! Flag if should die on error or just give a warning
!#py
!#py     character(len=*), parameter :: subname = 'read_variable_{DIMS}d'
!#py     !-----------------------------------------------------------------------
!#py     die_on_error = .true.
!#py     if ( this%allow_nodata ) then
!#py        die_on_error = .false.
!#py     end if
!#py
!#py     ! The following doesn't seem to be allowed:
!#py     ! allocate(arrayl(this%data_shape))
!#py     ! So instead we have to do this in a more ugly way:
!#py #if ({DIMS}==1)
!#py     allocate(arrayl(this%data_shape(1)))
!#py #elif ({DIMS}==2)
!#py     allocate(arrayl(this%data_shape(1), this%data_shape(2)))
!#py #endif
!#py
!#py     call ncd_io(ncid=this%dyn_file, varname=this%varname, flag='read', data=arrayl, &
!#py          dim1name=this%dim1name, nt=nt, readvar=readvar)
!#py     if (.not. readvar) then
!#py        if ( die_on_error ) then
!#py           call endrun(msg=' ERROR: ' // trim(this%varname) // ' NOT on file'//&
!#py                errMsg(__FILE__, __LINE__))
!#py        else
!#py           if ( masterproc ) &
!#py              write(iulog,*) ' WARNING: ' // trim(this%varname) // ' NOT on file set to zero'
!#py           data  = 0.0_r8
!#py        end if
!#py        this%data_on_file = .false.
!#py     end if
!#py
!#py     if ( readvar )then
!#py        this%data_on_file = .true.
!#py        arrayl = arrayl / this%conversion_factor
!#py
!#py        ! The following needs to be in an ifdef because the check_sums_equal_1 interface
!#py        ! requires a 2-d array
!#py #if ({DIMS}==2)
!#py        if (this%do_check_sums_equal_1) then
!#py           call check_sums_equal_1(arrayl, 1, this%varname, subname)
!#py        end if
!#py #endif
!#py
!#py        data = reshape(arrayl, shape(data))
!#py     end if
!#py
!#py     deallocate(arrayl)
!#py   end subroutine read_variable_{DIMS}d

end module dynVarMod
