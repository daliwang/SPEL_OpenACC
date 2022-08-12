module test_nested_derived_type

  implicit none
  integer, parameter :: name_maxlen = 128
  integer, parameter :: units_maxlen = 64
  integer, parameter :: subgrid_maxlen = 64
  type, public :: annual_flux_dribbler_type_test
     ! Metadata
     character(len=128), pointer  :: name
     character(len=64) , pointer  :: units

     ! Whether this dribbler allows non-zero deltas on time steps other than the first
     ! time step of the year
     logical, pointer :: allows_non_annual_delta

     ! Which subgrid level this dribbler is operating at, stored in various ways
     character(len=64), pointer :: dim1name
     character(len=64), pointer :: name_subgrid

     integer, pointer :: bounds_subgrid_level

     ! Annual amount to dribble in over the year
     real*8, pointer :: amount_to_dribble(:)

     ! Amount from the current timestep to pass through to the flux, if this isn't the
     ! first timestep of the year
     real*8, pointer :: amount_from_this_timestep(:)

   end type annual_flux_dribbler_type_test

end module test_nested_derived_type
