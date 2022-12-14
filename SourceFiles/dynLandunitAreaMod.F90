module dynLandunitAreaMod

!#py #include "shr_assert.h"

  !---------------------------------------------------------------------------
  !
  ! !DESCRIPTION:
  ! Handle dynamic landunit weights
  !
  ! !USES:
  use shr_kind_mod   , only : r8 => shr_kind_r8
  use elm_varctl     , only : iulog
  use elm_varcon     , only : ispval, namel
  use landunit_varcon, only : isturb_hd, isturb_md, isturb_tbd
  use landunit_varcon, only : istsoil, istcrop, istice, istdlak, istwet, max_lunit
  use decompMod      , only : bounds_type
  !#py use abortutils     , only : endrun
  use GridcellType   , only : grc_pp
  use LandunitType   , only : lun_pp
  use TopounitType   , only : top_pp
  !
  implicit none
  save
  private

  public :: update_landunit_weights      ! update landunit weights for all topounits

  ! The following is only public for the sake of unit testing; it should not be called
  ! directly by CLM code outside this module
  public :: update_landunit_weights_one_topounit

contains


  !-----------------------------------------------------------------------
  subroutine update_landunit_weights(bounds)
    !
    ! !DESCRIPTION:
    ! Update landunit weights for all topounits.
    !
    ! Assumes lun_pp%wttopounit has been updated for all landunits whose areas are specified by
    ! the dynamic subgrid code. Update lun_pp%wttopounit for all other landunits (including
    ! possibly changing some values of lun_pp%wttopounit for landunits whose areas are
    ! specified, e.g., if there are conflicts between glacier area and crop area).
    !
    ! !USES:
      !$acc routine seq
    !# use subgridWeightsMod, only : get_landunit_weight, set_landunit_weight
    !
    ! !ARGUMENTS:
    type(bounds_type), intent(in) :: bounds
    !
    ! !LOCAL VARIABLES:
    integer  :: t, l                        ! topounit index
    integer  :: ltype                       ! landunit type
    real(r8) :: landunit_weights(max_lunit) ! weight of each landunit on a single topounit

    !-----------------------------------------------------------------------

    do t = bounds%begt, bounds%endt
       ! Determine current landunit weights. Landunits that don't exist on this topounit
       ! get a weight of 0
       do ltype = 1, max_lunit
         l = top_pp%landunit_indices(ltype, t)
         if (l == ispval) then
            landunit_weights(ltype) = 0._r8
         else
            landunit_weights(ltype) = lun_pp%wttopounit(l)
         end if
       end do

       ! Adjust weights so they sum to 100%
       call update_landunit_weights_one_topounit(landunit_weights)

       ! Put the new landunit weights back into lun_pp%wttopounit
       do ltype = 1, max_lunit
         l = top_pp%landunit_indices(ltype, t)
         if (l /= ispval) then
            lun_pp%wttopounit(l) = landunit_weights(ltype)
         else if ( landunit_weights(ltype) > 0._r8) then
            print *, ' ERROR: Attempt to assign non-zero weight to a non-existent landunit'
         end if

       end do

    end do

  end subroutine update_landunit_weights


  !-----------------------------------------------------------------------
  subroutine update_landunit_weights_one_topounit(landunit_weights)
    !
    ! !DESCRIPTION:
    ! Update landunit weights for a single topounit.
    !
    ! This should be called with a vector of landunit weights for this topounit, which is
    ! updated in-place. Element #1 in this vector is the weight of landunit #1 (e.g.,
    ! istsoil, assuming istsoil==1), element #2 is the weight of landunit #2 (e.g.,
    ! istcrop, assuming istcrop==2), etc. Landunits that do not exist in this topounit
    ! should be given a weight of 0 in the vector.
    !
    ! After the execution of this routine, sum(landunit_weights) will be 1 within a small
    ! roundoff-level tolerance. This is achieved by growing or shrinking landunits as
    ! needed.
    !
    ! !ARGUMENTS:
      !$acc routine seq
    real(r8), intent(inout) :: landunit_weights(:)  ! weight of each landunit; this is updated in-place
    !
    ! !LOCAL VARIABLES:
    real(r8) :: landunit_sum   ! sum of all landunit weights on this topounit
    real(r8) :: excess         ! excess landunit weight that needs to be removed
    integer  :: decrease_index ! the current index into decrease_order
    integer  :: cur_landunit   ! the current element of decrease_order that we're dealing with

    ! This parameter specifies the order in which landunit areas are decreased when the
    ! specified areas add to greater than 100%. Landunits not listed here can never be
    ! decreased unless the forcings say they should be decreased. In particular, note
    ! that istice_mec doesn't appear here, so that the istice_mec area always will match
    ! the areas specified by GLC. In general, the code will NOT be robust if more than
    ! one landunit is excluded from this list. Meaning: since istice_mec is excluded from
    ! this list, all other landunits should appear in this list!
    integer, parameter :: decrease_order(8) = &
         (/istsoil, istcrop, isturb_md, isturb_hd, isturb_tbd, istwet, istdlak, istice/)

    real(r8), parameter :: tol = 1.e-14  ! tolerance for making sure sum of landunit weights equals 1

    !-----------------------------------------------------------------------
    landunit_sum = sum(landunit_weights)

    ! If landunits sum to ~ 100% already, we're done
    if (abs(landunit_sum - 1._r8) <= tol) then
       ! Do nothing

    ! If landunits sum to < 100%, increase natural vegetation so sum is 100%
    else if (landunit_sum < 1._r8) then
       landunit_weights(istsoil) = landunit_weights(istsoil) + (1._r8 - landunit_sum)

    ! If landunits sum to > 100%, decrease areas in priority order
    else
       decrease_index = 1
       excess = landunit_sum - 1._r8
       do while ((excess > tol) .and. decrease_index <= 8)
          ! Decrease weight of the next landunit, but not below 0
          cur_landunit = decrease_order(decrease_index)
          landunit_weights(cur_landunit) = landunit_weights(cur_landunit) - excess
          if (landunit_weights(cur_landunit) < 0._r8) then
             landunit_weights(cur_landunit) = 0._r8
          end if

          ! Update variables for next loop iteration
          landunit_sum = sum(landunit_weights)
          excess = landunit_sum - 1._r8
          decrease_index = decrease_index + 1
       end do
    end if

    ! Confirm that landunit sum is now equal to 100%, within tolerance
    landunit_sum = sum(landunit_weights)
    if (abs(landunit_sum - 1._r8) > tol) then
       print *, ' ERROR: After all landunit adjustments, landunit weights still do not equal 100%'
    end if

  end subroutine update_landunit_weights_one_topounit

end module dynLandunitAreaMod
