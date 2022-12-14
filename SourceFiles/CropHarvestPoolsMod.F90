module CropHarvestPoolsMod
  !-----------------------------------------------------------------------
  ! !DESCRIPTION:
  ! Calculate loss fluxes from crop harvest pools, and update product pool state variables
  !
  ! !USES:
  use shr_kind_mod        , only : r8 => shr_kind_r8
  use decompMod           , only : get_proc_bounds
  !#py use spmdMod             , only : masterproc
  use landunit_varcon     , only : istsoil
  use elm_varctl          , only : use_c13, use_c14
  use ColumnDataType      , only : col_cs, c13_col_cs, c14_col_cs
  use ColumnDataType      , only : col_cf, c13_col_cf, c14_col_cf
  use ColumnDataType      , only : col_ns, col_nf
  use ColumnDataType      , only : col_ps, col_pf
  !
  implicit none
  save
  private
  !
  ! !PUBLIC MEMBER FUNCTIONS:
  public:: CropHarvestPools
  !-----------------------------------------------------------------------

contains

  !-----------------------------------------------------------------------
  subroutine CropHarvestPools(num_soilc, filter_soilc, dt)
    !
    ! !DESCRIPTION:
    ! Update all loss fluxes from crop harvest pools, and update harvest pool state variables
    ! for both loss and gain terms. GAin terms are calculated in CropHarvestPools() for gains
    ! associated with crop harvest.
    !
    ! !ARGUMENTS:
      !$acc routine seq
    integer                  , intent(in)    :: num_soilc       ! number of soil columns in filter
    integer                  , intent(in)    :: filter_soilc(:) ! filter for soil columns
    real(r8)                 , intent(in)     :: dt        ! time step (seconds)
    ! !LOCAL VARIABLES:
    integer :: fc        ! lake filter indices
    integer :: c         ! indices
    real(r8) :: kprod1       ! decay constant for 1-year product pool
    !-----------------------------------------------------------------------


    ! calculate column-level losses from product pools
    ! the following (1/s) rate constants result in ~90% loss of initial state over 1 year,
    ! using a discrete-time fractional decay algorithm.
    kprod1 = 7.2e-9

    do fc = 1,num_soilc
       c = filter_soilc(fc)

       ! calculate fluxes (1/sec)
       col_cf%prod1c_loss(c)    = col_cs%prod1c(c)    * kprod1

       if ( use_c13 ) then
          c13_col_cf%prod1c_loss(c)  = c13_col_cs%prod1c(c)  * kprod1
       endif

       if ( use_c14 ) then
          c14_col_cf%prod1c_loss(c)  = c14_col_cs%prod1c(c)  * kprod1
       endif

       col_nf%prod1n_loss(c)    = col_ns%prod1n(c)    * kprod1
       col_pf%prod1p_loss(c)  = col_ps%prod1p(c)  * kprod1
    end do


   ! update wood product state variables
    do fc = 1,num_soilc
       c = filter_soilc(fc)

       ! fluxes into wood product pools, from harvest
       col_cs%prod1c(c)    = col_cs%prod1c(c)    + &
            col_cf%hrv_cropc_to_prod1c(c)*dt     - & ! from harvest
            col_cf%prod1c_loss(c)*dt                 ! from decomposition

       if ( use_c13 ) then
          c13_col_cs%prod1c(c)  = c13_col_cs%prod1c(c)  + &
               c13_col_cf%hrv_cropc_to_prod1c(c)*dt     - & ! from harvest
               c13_col_cf%prod1c_loss(c)*dt                 ! from decomposition
       endif

       if ( use_c14 ) then
          c14_col_cs%prod1c(c)  = c14_col_cs%prod1c(c)  + &
               c14_col_cf%hrv_cropc_to_prod1c(c)*dt     - & ! from harvest
               c14_col_cf%prod1c_loss(c)*dt                 ! from decomposition
       endif

       col_ns%prod1n(c)    = col_ns%prod1n(c)    + &
            col_nf%hrv_cropn_to_prod1n(c)*dt     - & ! from harvest
            col_nf%prod1n_loss(c)*dt                 ! from decomposition

       col_ps%prod1p(c)    = col_ps%prod1p(c)    + &
            col_pf%hrv_cropp_to_prod1p(c)*dt     - & ! from harvest
            col_pf%prod1p_loss(c)*dt                 ! from decomposition

    end do ! end of column loop

  end subroutine CropHarvestPools

end module CropHarvestPoolsMod
