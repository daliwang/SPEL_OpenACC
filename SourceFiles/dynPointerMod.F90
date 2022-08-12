module dynPointerMod
  !This module is used to create array of pointers that
  !point to fields used by specific subroutines (e.g. dyn_cnbal_patch)
  !to parallelize them.  Modeled after the clmptr arrays from histFileMod

  !use histFileMod , only : clmpoint_rs
  implicit none

  !number of fields -- depends on number of species used
  integer,parameter :: number_flds = 146

  type clmpoint_rs                             ! Pointer to real scalar data (1D)
     real(r8), pointer :: ptr(:) => null()
  end type clmpoint_rs

  type(clmpoint_rs) :: dynptr(:)

end module dynPointerMod
