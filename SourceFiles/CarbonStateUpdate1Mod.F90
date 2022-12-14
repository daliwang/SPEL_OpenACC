module CarbonStateUpdate1Mod

  !-----------------------------------------------------------------------
  ! Module for carbon state variable update, non-mortality fluxes.
  !
  ! !USES:
  use shr_kind_mod            , only : r8 => shr_kind_r8
  use decompMod               , only : bounds_type
  use elm_varpar              , only : ndecomp_cascade_transitions, nlevdecomp
  use elm_varpar              , only : i_met_lit, i_cel_lit, i_lig_lit, i_cwd
  use elm_varcon              , only : dzsoi_decomp
  use elm_varctl              , only : nu_com, use_c13, use_c14
  use elm_varctl              , only : use_pflotran, pf_cmode, use_fates
  use pftvarcon               , only : npcropmin, nc3crop
  use CNDecompCascadeConType  , only : decomp_cascade_type
  use CNStateType             , only : cnstate_type
  use CNDecompCascadeConType  , only : decomp_cascade_con
  use CropType                , only : crop_type

  use GridcellDataType        , only : grc_cs, c13_grc_cs, c14_grc_cs
  use GridcellDataType        , only : grc_cf, c13_grc_cf, c14_grc_cf
  use ColumnDataType          , only : column_carbon_state, column_carbon_flux
  use ColumnDataType          , only : col_cs, c13_col_cs, c14_col_cs
  use ColumnDataType          , only : col_cf, c13_col_cf, c14_col_cf
  use VegetationType          , only : veg_pp
  use VegetationDataType      , only : vegetation_carbon_state, vegetation_carbon_flux
  use VegetationPropertiesType, only : veg_vp
  use timeInfoMod             , only : year_curr, mon_curr, day_curr, secs_curr 
  !
  implicit none
  save
  private
  !
  ! !PUBLIC MEMBER FUNCTIONS:
  public :: CarbonStateUpdateDynPatch
  public :: CarbonStateUpdate0
  public :: CarbonStateUpdate_Phase1_COL
  public :: CarbonStateUpdate_Phase1_PFT
  public :: CarbonStateDynGridUpdate 
  !-----------------------------------------------------------------------

contains

!-------------------------------------------------------------------------
subroutine CarbonStateDynGridUpdate(g, dt)
   !
   ! !DESCRIPTION: 
   ! Update gridcell carbon states based on fluxes form dyn_cnbal_patch 
   !$acc routine seq 
   ! !ARGUMENTS 
   integer , intent(in), value :: g 
   real(r8), intent(in) :: dt 
   ! 
   grc_cs%seedc(g) = grc_cs%seedc(g) &
      - grc_cf%dwt_seedc_to_leaf(g)     * dt &
      - grc_cf%dwt_seedc_to_deadstem(g) * dt

   if (use_c13) then
     c13_grc_cs%seedc(g) = c13_grc_cs%seedc(g) &
         - c13_grc_cf%dwt_seedc_to_leaf(g)     * dt &
         - c13_grc_cf%dwt_seedc_to_deadstem(g) * dt
   end if

   if (use_c14) then
     c14_grc_cs%seedc(g) = c14_grc_cs%seedc(g) &
        - c14_grc_cf%dwt_seedc_to_leaf(g)     * dt &
        - c14_grc_cf%dwt_seedc_to_deadstem(g) * dt
   end if
end subroutine CarbonStateDynGridUpdate

!-----------------------------------------------------------------------
subroutine CarbonStateUpdateDynPatch(num_soilc_with_inactive, filter_soilc_with_inactive, dt)
 !
 ! !DESCRIPTION:
 ! Update carbon states based on fluxes from dyn_cnbal_patch
 ! !ARGUMENTS:
 integer          , intent(in) :: num_soilc_with_inactive       ! number of columns in soil filter
 integer          , intent(in) :: filter_soilc_with_inactive(:) ! soil column filter that includes inactive points
 real(r8)         , intent(in) :: dt  ! time step (seconds)

 !
 ! !LOCAL VARIABLES:
 integer  :: c   ! column index
 integer  :: fc  ! column filter index
 integer  :: j   ! level index

   !$acc parallel loop independent gang vector default(present)
   do fc = 1, num_soilc_with_inactive

      c = filter_soilc_with_inactive(fc)
      col_cs%prod10c(c) = col_cs%prod10c(c) + col_cf%dwt_prod10c_gain(c)*dt
      col_cs%prod100c(c) = col_cs%prod100c(c) + col_cf%dwt_prod100c_gain(c)*dt
      col_cs%prod1c(c) = col_cs%prod1c(c) + col_cf%dwt_crop_productc_gain(c)*dt

      if (use_c13) then
         c13_col_cs%prod10c(c) = c13_col_cs%prod10c(c) + c13_col_cf%dwt_prod10c_gain(c)*dt
         c13_col_cs%prod100c(c) = c13_col_cs%prod100c(c) + c13_col_cf%dwt_prod100c_gain(c)*dt
         c13_col_cs%prod1c(c) = c13_col_cs%prod1c(c) + c13_col_cf%dwt_crop_productc_gain(c)*dt
      end if

      if (use_c14) then
         c14_col_cs%prod10c(c) = c14_col_cs%prod10c(c) + c14_col_cf%dwt_prod10c_gain(c)*dt
         c14_col_cs%prod100c(c) = c14_col_cs%prod100c(c) + c14_col_cf%dwt_prod100c_gain(c)*dt
         c14_col_cs%prod1c(c) = c14_col_cs%prod1c(c) + c14_col_cf%dwt_crop_productc_gain(c)*dt
      end if
   end do 

!$acc parallel loop independent gang vector collapse(2) default(present) 
do j = 1,nlevdecomp
   do fc = 1, num_soilc_with_inactive
      c = filter_soilc_with_inactive(fc)
      col_cs%decomp_cpools_vr(c,j,i_met_lit) = col_cs%decomp_cpools_vr(c,j,i_met_lit) + &
         col_cf%dwt_frootc_to_litr_met_c(c,j) * dt
      col_cs%decomp_cpools_vr(c,j,i_cel_lit) = col_cs%decomp_cpools_vr(c,j,i_cel_lit) + &
         col_cf%dwt_frootc_to_litr_cel_c(c,j) * dt
      col_cs%decomp_cpools_vr(c,j,i_lig_lit) = col_cs%decomp_cpools_vr(c,j,i_lig_lit) + &
         col_cf%dwt_frootc_to_litr_lig_c(c,j) * dt
      col_cs%decomp_cpools_vr(c,j,i_cwd) = col_cs%decomp_cpools_vr(c,j,i_cwd) + &
         ( col_cf%dwt_livecrootc_to_cwdc(c,j) + col_cf%dwt_deadcrootc_to_cwdc(c,j) ) * dt

      if (use_c13) then
         c13_col_cs%decomp_cpools_vr(c,j,i_met_lit) = c13_col_cs%decomp_cpools_vr(c,j,i_met_lit) + &
            c13_col_cf%dwt_frootc_to_litr_met_c(c,j) * dt
         c13_col_cs%decomp_cpools_vr(c,j,i_cel_lit) = c13_col_cs%decomp_cpools_vr(c,j,i_cel_lit) + &
            c13_col_cf%dwt_frootc_to_litr_cel_c(c,j) * dt
         c13_col_cs%decomp_cpools_vr(c,j,i_lig_lit) = c13_col_cs%decomp_cpools_vr(c,j,i_lig_lit) + &
            c13_col_cf%dwt_frootc_to_litr_lig_c(c,j) * dt
         c13_col_cs%decomp_cpools_vr(c,j,i_cwd) = c13_col_cs%decomp_cpools_vr(c,j,i_cwd) + &
            ( c13_col_cf%dwt_livecrootc_to_cwdc(c,j) + c13_col_cf%dwt_deadcrootc_to_cwdc(c,j) ) * dt
      end if

      if (use_c14) then
         c14_col_cs%decomp_cpools_vr(c,j,i_met_lit) = c14_col_cs%decomp_cpools_vr(c,j,i_met_lit) + &
            c14_col_cf%dwt_frootc_to_litr_met_c(c,j) * dt
         c14_col_cs%decomp_cpools_vr(c,j,i_cel_lit) = c14_col_cs%decomp_cpools_vr(c,j,i_cel_lit) + &
            c14_col_cf%dwt_frootc_to_litr_cel_c(c,j) * dt
         c14_col_cs%decomp_cpools_vr(c,j,i_lig_lit) = c14_col_cs%decomp_cpools_vr(c,j,i_lig_lit) + &
            c14_col_cf%dwt_frootc_to_litr_lig_c(c,j) * dt
         c14_col_cs%decomp_cpools_vr(c,j,i_cwd) = c14_col_cs%decomp_cpools_vr(c,j,i_cwd) + &
            ( c14_col_cf%dwt_livecrootc_to_cwdc(c,j) + c14_col_cf%dwt_deadcrootc_to_cwdc(c,j) ) * dt
      end if
   end do
end do

end subroutine CarbonStateUpdateDynPatch

  !-----------------------------------------------------------------------
  subroutine CarbonStateUpdate0(p, veg_cs, veg_cf, dt)
    !
    ! !DESCRIPTION:
    ! On the radiation time step, update cpool carbon state
    !

    ! !ARGUMENTS:
      !$acc routine seq
    integer , value, intent(in)  :: p
    type(vegetation_carbon_state),intent(inout) :: veg_cs
    type(vegetation_carbon_flux) ,intent(inout) :: veg_cf
    real(r8),     intent(in)    :: dt
    !
    !-----------------------------------------------------------------------

    ! patch loop
    ! gross photosynthesis fluxes
    veg_cs%cpool(p) = veg_cs%cpool(p) + veg_cf%psnsun_to_cpool(p)*dt
    veg_cs%cpool(p) = veg_cs%cpool(p) + veg_cf%psnshade_to_cpool(p)*dt

  end subroutine CarbonStateUpdate0

  !-----------------------------------------------------------------------
  subroutine CarbonStateUpdate_Phase1_col(num_soilc, filter_soilc, col_cs, col_cf, dt)
    !
    ! !DESCRIPTION:
    ! On the radiation time step, update all the prognostic carbon state
    ! variables (except for gap-phase mortality and fire fluxes)
    !
    use tracer_varcon       , only : is_active_betr_bgc
    implicit none 
    ! !ARGUMENTS:
    integer                      , intent(in)    :: num_soilc       ! number of soil columns filter
    integer                      , intent(in)    :: filter_soilc(:) ! filter for soil columns
    type(column_carbon_state)    , intent(inout) :: col_cs
    type(column_carbon_flux)     , intent(inout) :: col_cf
    real(r8), intent(in) :: dt        ! radiation time step (seconds)

    !
    ! !LOCAL VARIABLES:
    integer  :: c,j,k,l ! indices
    integer  :: fc     ! lake filter indices
    !-----------------------------------------------------------------------

    associate(  &
         cascade_donor_pool    =>    decomp_cascade_con%cascade_donor_pool      , & ! Input:  [integer  (:)     ]  which pool is C taken from for a given decomposition step
         cascade_receiver_pool =>    decomp_cascade_con%cascade_receiver_pool    & ! Input:  [integer  (:)     ]  which pool is C added to for a given decomposition step
         )

      ! column level fluxes

      if (.not. is_active_betr_bgc .and. .not.(use_pflotran .and. pf_cmode) ) then

         ! plant to litter fluxes
         if(.not.use_fates)then
            !$acc parallel loop independent gang vector collapse(2) default(present)
            do j = 1,nlevdecomp
               ! column loop
               do fc = 1,num_soilc
                  c = filter_soilc(fc)
                  ! phenology and dynamic land cover fluxes
                  col_cf%decomp_cpools_sourcesink(c,j,i_met_lit) = &
                       col_cf%phenology_c_to_litr_met_c(c,j) * dt
                  col_cf%decomp_cpools_sourcesink(c,j,i_cel_lit) = &
                       col_cf%phenology_c_to_litr_cel_c(c,j) * dt
                  col_cf%decomp_cpools_sourcesink(c,j,i_lig_lit) = &
                       col_cf%phenology_c_to_litr_lig_c(c,j) * dt

                  col_cs%decomp_som2c_vr(c,j) = col_cs%decomp_cpools_vr(c,j,6)

               end do
            end do
         end if

         print *, "Carbonstate Print"
         ! litter and SOM HR fluxes
         !$acc parallel loop independent gang worker collapse(2) default(present)
         do j = 1,nlevdecomp
            do fc = 1,num_soilc
               ! column loop
               c = filter_soilc(fc)
               !$acc loop vector independent
               do k = 1, ndecomp_cascade_transitions
                  !$acc atomic update 
                  col_cf%decomp_cpools_sourcesink(c,j,cascade_donor_pool(k)) = &
                       col_cf%decomp_cpools_sourcesink(c,j,cascade_donor_pool(k)) &
                       - ( col_cf%decomp_cascade_hr_vr(c,j,k) + col_cf%decomp_cascade_ctransfer_vr(c,j,k)) *dt
                  !$acc end atomic 
               end do
            end do
         end do

         !$acc parallel loop independent gang worker collapse(2) default(present)
         do j = 1,nlevdecomp
            do fc = 1,num_soilc
               ! column loop
               !$acc loop vector independent
               do k = 1, ndecomp_cascade_transitions
                  if ( cascade_receiver_pool(k) /= 0 ) then  ! skip terminal transitions
                     c = filter_soilc(fc)
                     !$acc atomic update 
                     col_cf%decomp_cpools_sourcesink(c,j,cascade_receiver_pool(k)) = &
                          col_cf%decomp_cpools_sourcesink(c,j,cascade_receiver_pool(k)) &
                          + col_cf%decomp_cascade_ctransfer_vr(c,j,k)*dt
                     !$acc end atomic 
                    end if
               end do
            end do
         end do

      endif   !end if is_active_betr_bgc()

  end associate

  end subroutine CarbonStateUpdate_Phase1_COL

  subroutine CarbonStateUpdate_Phase1_PFT(p,crop_vars, veg_cs, veg_cf,dt)
      !$acc routine seq 
     implicit none

     ! integer                      , intent(in)    :: num_soilp       ! number of soil patches in filter
     ! integer                      , intent(in)    :: filter_soilp(:) ! filter for soil patches
     integer , intent(in), value :: p 
     real(r8), intent(in) :: dt
     type(crop_type)              , intent(inout) :: crop_vars
     type(vegetation_carbon_state), intent(inout) :: veg_cs
     type(vegetation_carbon_flux) , intent(inout) :: veg_cf
     real(r8), intent(in) :: dt        ! radiation time step (seconds)

     !
     ! !LOCAL VARIABLES:
     integer  :: p,l ! indices
     integer  :: fp      ! lake filter indices
     associate(  &
          ivt                   =>    veg_pp%itype                               , & ! Input:  [integer  (:)     ]  pft vegetation type
          woody                 =>    veg_vp%woody                               , & ! Input:  [real(r8) (:)     ]  binary flag for woody lifeform (1=woody, 0=not woody)
          harvdate              =>    crop_vars%harvdate_patch                     & ! Input:  [integer  (:)     ]  harvest date
          )
      !
      !if (.not.use_fates) then
      ! patch loop
          
      ! do fp = 1,num_soilp
         ! p = filter_soilp(fp)

         ! phenology: transfer growth fluxes
         veg_cs%leafc(p)           = veg_cs%leafc(p)       + veg_cf%leafc_xfer_to_leafc(p)*dt
         veg_cs%leafc_xfer(p)      = veg_cs%leafc_xfer(p)  - veg_cf%leafc_xfer_to_leafc(p)*dt
         veg_cs%frootc(p)          = veg_cs%frootc(p)      + veg_cf%frootc_xfer_to_frootc(p)*dt
         veg_cs%frootc_xfer(p)     = veg_cs%frootc_xfer(p) - veg_cf%frootc_xfer_to_frootc(p)*dt
         if (woody(ivt(p)) == 1._r8) then
                veg_cs%livestemc(p)       = veg_cs%livestemc(p)       + veg_cf%livestemc_xfer_to_livestemc(p)*dt
                veg_cs%livestemc_xfer(p)  = veg_cs%livestemc_xfer(p)  - veg_cf%livestemc_xfer_to_livestemc(p)*dt
                veg_cs%deadstemc(p)       = veg_cs%deadstemc(p)       + veg_cf%deadstemc_xfer_to_deadstemc(p)*dt
                veg_cs%deadstemc_xfer(p)  = veg_cs%deadstemc_xfer(p)  - veg_cf%deadstemc_xfer_to_deadstemc(p)*dt
                veg_cs%livecrootc(p)      = veg_cs%livecrootc(p)      + veg_cf%livecrootc_xfer_to_livecrootc(p)*dt
                veg_cs%livecrootc_xfer(p) = veg_cs%livecrootc_xfer(p) - veg_cf%livecrootc_xfer_to_livecrootc(p)*dt
                veg_cs%deadcrootc(p)      = veg_cs%deadcrootc(p)      + veg_cf%deadcrootc_xfer_to_deadcrootc(p)*dt
                veg_cs%deadcrootc_xfer(p) = veg_cs%deadcrootc_xfer(p) - veg_cf%deadcrootc_xfer_to_deadcrootc(p)*dt
         end if
         if (ivt(p) >= npcropmin) then ! skip 2 generic crops
            ! lines here for consistency; the transfer terms are zero
            veg_cs%livestemc(p)       = veg_cs%livestemc(p)      + veg_cf%livestemc_xfer_to_livestemc(p)*dt
            veg_cs%livestemc_xfer(p)  = veg_cs%livestemc_xfer(p) - veg_cf%livestemc_xfer_to_livestemc(p)*dt
            veg_cs%grainc(p)          = veg_cs%grainc(p)         + veg_cf%grainc_xfer_to_grainc(p)*dt
            veg_cs%grainc_xfer(p)     = veg_cs%grainc_xfer(p)    - veg_cf%grainc_xfer_to_grainc(p)*dt
         end if

         ! phenology: litterfall fluxes
         veg_cs%leafc(p) = veg_cs%leafc(p) - veg_cf%leafc_to_litter(p)*dt
         veg_cs%frootc(p) = veg_cs%frootc(p) - veg_cf%frootc_to_litter(p)*dt

         ! livewood turnover fluxes
         if (woody(ivt(p)) == 1._r8) then
            veg_cs%livestemc(p)  = veg_cs%livestemc(p)  - veg_cf%livestemc_to_deadstemc(p)*dt
            veg_cs%deadstemc(p)  = veg_cs%deadstemc(p)  + veg_cf%livestemc_to_deadstemc(p)*dt
            veg_cs%livecrootc(p) = veg_cs%livecrootc(p) - veg_cf%livecrootc_to_deadcrootc(p)*dt
            veg_cs%deadcrootc(p) = veg_cs%deadcrootc(p) + veg_cf%livecrootc_to_deadcrootc(p)*dt
         end if
         if (ivt(p) >= npcropmin) then ! skip 2 generic crops
            veg_cs%livestemc(p)  = veg_cs%livestemc(p)  - veg_cf%livestemc_to_litter(p)*dt
            veg_cs%grainc(p)     = veg_cs%grainc(p)     - veg_cf%grainc_to_food(p)*dt

            veg_cs%cropseedc_deficit(p) = veg_cs%cropseedc_deficit(p) &
                 - veg_cf%crop_seedc_to_leaf(p) * dt
         end if

         ! maintenance respiration fluxes from cpool
         veg_cs%cpool(p) = veg_cs%cpool(p) - veg_cf%cpool_to_xsmrpool(p)*dt
         veg_cs%cpool(p) = veg_cs%cpool(p) - veg_cf%leaf_curmr(p)*dt
         veg_cs%cpool(p) = veg_cs%cpool(p) - veg_cf%froot_curmr(p)*dt
         if (woody(ivt(p)) == 1._r8) then
            veg_cs%cpool(p) = veg_cs%cpool(p) - veg_cf%livestem_curmr(p)*dt
            veg_cs%cpool(p) = veg_cs%cpool(p) - veg_cf%livecroot_curmr(p)*dt
         end if
         if (ivt(p) >= npcropmin) then ! skip 2 generic crops
            veg_cs%cpool(p) = veg_cs%cpool(p) - veg_cf%livestem_curmr(p)*dt
            veg_cs%cpool(p) = veg_cs%cpool(p) - veg_cf%grain_curmr(p)*dt
         end if
         ! excess respiration flux from cpool
         veg_cs%cpool(p) = veg_cs%cpool(p) - veg_cf%xr(p)*dt

         ! maintenance respiration fluxes from xsmrpool
         veg_cs%xsmrpool(p) = veg_cs%xsmrpool(p) + veg_cf%cpool_to_xsmrpool(p)*dt
         veg_cs%xsmrpool(p) = veg_cs%xsmrpool(p) - veg_cf%leaf_xsmr(p)*dt
         veg_cs%xsmrpool(p) = veg_cs%xsmrpool(p) - veg_cf%froot_xsmr(p)*dt
         if (nu_com .ne. 'RD') then
            veg_cs%xsmrpool(p) = veg_cs%xsmrpool(p) - veg_cf%xsmrpool_turnover(p)*dt
         end if
         if (woody(ivt(p)) == 1._r8) then
            veg_cs%xsmrpool(p) = veg_cs%xsmrpool(p) - veg_cf%livestem_xsmr(p)*dt
            veg_cs%xsmrpool(p) = veg_cs%xsmrpool(p) - veg_cf%livecroot_xsmr(p)*dt
         end if
         if (ivt(p) >= npcropmin) then ! skip 2 generic crops
            veg_cs%xsmrpool(p) = veg_cs%xsmrpool(p) - veg_cf%livestem_xsmr(p)*dt
            veg_cs%xsmrpool(p) = veg_cs%xsmrpool(p) - veg_cf%grain_xsmr(p)*dt
            if (harvdate(p) < 999) then ! beginning at harvest, send to atm
               veg_cf%xsmrpool_to_atm(p) = veg_cf%xsmrpool_to_atm(p) + veg_cs%xsmrpool(p)/dt
               veg_cs%xsmrpool(p)        = veg_cs%xsmrpool(p)        - veg_cf%xsmrpool_to_atm(p)*dt
            end if
         end if

         ! allocation fluxes
         veg_cs%cpool(p)           = veg_cs%cpool(p)          - veg_cf%cpool_to_leafc(p)*dt
         veg_cs%leafc(p)           = veg_cs%leafc(p)          + veg_cf%cpool_to_leafc(p)*dt
         veg_cs%cpool(p)           = veg_cs%cpool(p)          - veg_cf%cpool_to_leafc_storage(p)*dt
         veg_cs%leafc_storage(p)   = veg_cs%leafc_storage(p)  + veg_cf%cpool_to_leafc_storage(p)*dt
         veg_cs%cpool(p)           = veg_cs%cpool(p)          - veg_cf%cpool_to_frootc(p)*dt
         veg_cs%frootc(p)          = veg_cs%frootc(p)         + veg_cf%cpool_to_frootc(p)*dt
         veg_cs%cpool(p)           = veg_cs%cpool(p)          - veg_cf%cpool_to_frootc_storage(p)*dt
         veg_cs%frootc_storage(p)  = veg_cs%frootc_storage(p) + veg_cf%cpool_to_frootc_storage(p)*dt
         if (woody(ivt(p)) == 1._r8) then
            veg_cs%cpool(p)               = veg_cs%cpool(p)              - veg_cf%cpool_to_livestemc(p)*dt
            veg_cs%livestemc(p)           = veg_cs%livestemc(p)          + veg_cf%cpool_to_livestemc(p)*dt
            veg_cs%cpool(p)               = veg_cs%cpool(p)              - veg_cf%cpool_to_livestemc_storage(p)*dt
            veg_cs%livestemc_storage(p)   = veg_cs%livestemc_storage(p)  + veg_cf%cpool_to_livestemc_storage(p)*dt
            veg_cs%cpool(p)               = veg_cs%cpool(p)              - veg_cf%cpool_to_deadstemc(p)*dt
            veg_cs%deadstemc(p)           = veg_cs%deadstemc(p)          + veg_cf%cpool_to_deadstemc(p)*dt
            veg_cs%cpool(p)               = veg_cs%cpool(p)              - veg_cf%cpool_to_deadstemc_storage(p)*dt
            veg_cs%deadstemc_storage(p)   = veg_cs%deadstemc_storage(p)  + veg_cf%cpool_to_deadstemc_storage(p)*dt
            veg_cs%cpool(p)               = veg_cs%cpool(p)              - veg_cf%cpool_to_livecrootc(p)*dt
            veg_cs%livecrootc(p)          = veg_cs%livecrootc(p)         + veg_cf%cpool_to_livecrootc(p)*dt
            veg_cs%cpool(p)               = veg_cs%cpool(p)              - veg_cf%cpool_to_livecrootc_storage(p)*dt
            veg_cs%livecrootc_storage(p)  = veg_cs%livecrootc_storage(p) + veg_cf%cpool_to_livecrootc_storage(p)*dt
            veg_cs%cpool(p)               = veg_cs%cpool(p)              - veg_cf%cpool_to_deadcrootc(p)*dt
            veg_cs%deadcrootc(p)          = veg_cs%deadcrootc(p)         + veg_cf%cpool_to_deadcrootc(p)*dt
            veg_cs%cpool(p)               = veg_cs%cpool(p)              - veg_cf%cpool_to_deadcrootc_storage(p)*dt
            veg_cs%deadcrootc_storage(p)  = veg_cs%deadcrootc_storage(p) + veg_cf%cpool_to_deadcrootc_storage(p)*dt
         end if
         if (ivt(p) >= npcropmin) then ! skip 2 generic crops
            veg_cs%cpool(p)               = veg_cs%cpool(p)              - veg_cf%cpool_to_livestemc(p)*dt
            veg_cs%livestemc(p)           = veg_cs%livestemc(p)          + veg_cf%cpool_to_livestemc(p)*dt
            veg_cs%cpool(p)               = veg_cs%cpool(p)              - veg_cf%cpool_to_livestemc_storage(p)*dt
            veg_cs%livestemc_storage(p)   = veg_cs%livestemc_storage(p)  + veg_cf%cpool_to_livestemc_storage(p)*dt
            veg_cs%cpool(p)               = veg_cs%cpool(p)              - veg_cf%cpool_to_grainc(p)*dt
            veg_cs%grainc(p)              = veg_cs%grainc(p)             + veg_cf%cpool_to_grainc(p)*dt
            veg_cs%cpool(p)               = veg_cs%cpool(p)              - veg_cf%cpool_to_grainc_storage(p)*dt
            veg_cs%grainc_storage(p)      = veg_cs%grainc_storage(p)     + veg_cf%cpool_to_grainc_storage(p)*dt
         end if

         ! growth respiration fluxes for current growth
         veg_cs%cpool(p) = veg_cs%cpool(p) - veg_cf%cpool_leaf_gr(p)*dt
         veg_cs%cpool(p) = veg_cs%cpool(p) - veg_cf%cpool_froot_gr(p)*dt
         if (woody(ivt(p)) == 1._r8) then
            veg_cs%cpool(p) = veg_cs%cpool(p) - veg_cf%cpool_livestem_gr(p)*dt
            veg_cs%cpool(p) = veg_cs%cpool(p) - veg_cf%cpool_deadstem_gr(p)*dt
            veg_cs%cpool(p) = veg_cs%cpool(p) - veg_cf%cpool_livecroot_gr(p)*dt
            veg_cs%cpool(p) = veg_cs%cpool(p) - veg_cf%cpool_deadcroot_gr(p)*dt
         end if
         if (ivt(p) >= npcropmin) then ! skip 2 generic crops
            veg_cs%cpool(p) = veg_cs%cpool(p) - veg_cf%cpool_livestem_gr(p)*dt
            veg_cs%cpool(p) = veg_cs%cpool(p) - veg_cf%cpool_grain_gr(p)*dt
         end if

         ! growth respiration for transfer growth
         veg_cs%gresp_xfer(p) = veg_cs%gresp_xfer(p) - veg_cf%transfer_leaf_gr(p)*dt
         veg_cs%gresp_xfer(p) = veg_cs%gresp_xfer(p) - veg_cf%transfer_froot_gr(p)*dt
         if (woody(ivt(p)) == 1._r8) then
            veg_cs%gresp_xfer(p) = veg_cs%gresp_xfer(p) - veg_cf%transfer_livestem_gr(p)*dt
            veg_cs%gresp_xfer(p) = veg_cs%gresp_xfer(p) - veg_cf%transfer_deadstem_gr(p)*dt
            veg_cs%gresp_xfer(p) = veg_cs%gresp_xfer(p) - veg_cf%transfer_livecroot_gr(p)*dt
            veg_cs%gresp_xfer(p) = veg_cs%gresp_xfer(p) - veg_cf%transfer_deadcroot_gr(p)*dt
         end if
         if (ivt(p) >= npcropmin) then ! skip 2 generic crops
            veg_cs%gresp_xfer(p) = veg_cs%gresp_xfer(p) - veg_cf%transfer_livestem_gr(p)*dt
            veg_cs%gresp_xfer(p) = veg_cs%gresp_xfer(p) - veg_cf%transfer_grain_gr(p)*dt
         end if

         ! growth respiration at time of storage
         veg_cs%cpool(p) = veg_cs%cpool(p) - veg_cf%cpool_leaf_storage_gr(p)*dt
         veg_cs%cpool(p) = veg_cs%cpool(p) - veg_cf%cpool_froot_storage_gr(p)*dt
         if (woody(ivt(p)) == 1._r8) then
            veg_cs%cpool(p) = veg_cs%cpool(p) - veg_cf%cpool_livestem_storage_gr(p)*dt
            veg_cs%cpool(p) = veg_cs%cpool(p) - veg_cf%cpool_deadstem_storage_gr(p)*dt
            veg_cs%cpool(p) = veg_cs%cpool(p) - veg_cf%cpool_livecroot_storage_gr(p)*dt
            veg_cs%cpool(p) = veg_cs%cpool(p) - veg_cf%cpool_deadcroot_storage_gr(p)*dt
         end if
         if (ivt(p) >= npcropmin) then ! skip 2 generic crops
            veg_cs%cpool(p) = veg_cs%cpool(p) - veg_cf%cpool_livestem_storage_gr(p)*dt
            veg_cs%cpool(p) = veg_cs%cpool(p) - veg_cf%cpool_grain_storage_gr(p)*dt
         end if

         ! growth respiration stored for release during transfer growth
         veg_cs%cpool(p)         = veg_cs%cpool(p)         - veg_cf%cpool_to_gresp_storage(p)*dt
         veg_cs%gresp_storage(p) = veg_cs%gresp_storage(p) + veg_cf%cpool_to_gresp_storage(p)*dt

         ! move storage pools into transfer pools
         veg_cs%leafc_storage(p)  = veg_cs%leafc_storage(p)  - veg_cf%leafc_storage_to_xfer(p)*dt
         veg_cs%leafc_xfer(p)     = veg_cs%leafc_xfer(p)     + veg_cf%leafc_storage_to_xfer(p)*dt
         veg_cs%frootc_storage(p) = veg_cs%frootc_storage(p) - veg_cf%frootc_storage_to_xfer(p)*dt
         veg_cs%frootc_xfer(p)    = veg_cs%frootc_xfer(p)    + veg_cf%frootc_storage_to_xfer(p)*dt
         if (woody(ivt(p)) == 1._r8) then
            veg_cs%livestemc_storage(p)  = veg_cs%livestemc_storage(p) - veg_cf%livestemc_storage_to_xfer(p)*dt
            veg_cs%livestemc_xfer(p)     = veg_cs%livestemc_xfer(p)    + veg_cf%livestemc_storage_to_xfer(p)*dt
            veg_cs%deadstemc_storage(p)  = veg_cs%deadstemc_storage(p) - veg_cf%deadstemc_storage_to_xfer(p)*dt
            veg_cs%deadstemc_xfer(p)     = veg_cs%deadstemc_xfer(p)    + veg_cf%deadstemc_storage_to_xfer(p)*dt
            veg_cs%livecrootc_storage(p) = veg_cs%livecrootc_storage(p)- veg_cf%livecrootc_storage_to_xfer(p)*dt
            veg_cs%livecrootc_xfer(p)    = veg_cs%livecrootc_xfer(p)   + veg_cf%livecrootc_storage_to_xfer(p)*dt
            veg_cs%deadcrootc_storage(p) = veg_cs%deadcrootc_storage(p)- veg_cf%deadcrootc_storage_to_xfer(p)*dt
            veg_cs%deadcrootc_xfer(p)    = veg_cs%deadcrootc_xfer(p)   + veg_cf%deadcrootc_storage_to_xfer(p)*dt
            veg_cs%gresp_storage(p)      = veg_cs%gresp_storage(p)     - veg_cf%gresp_storage_to_xfer(p)*dt
            veg_cs%gresp_xfer(p)         = veg_cs%gresp_xfer(p)        + veg_cf%gresp_storage_to_xfer(p)*dt
         end if
         if (ivt(p) >= npcropmin) then ! skip 2 generic crops
            ! lines here for consistency; the transfer terms are zero
            veg_cs%livestemc_storage(p)  = veg_cs%livestemc_storage(p) - veg_cf%livestemc_storage_to_xfer(p)*dt
            veg_cs%livestemc_xfer(p)     = veg_cs%livestemc_xfer(p)    + veg_cf%livestemc_storage_to_xfer(p)*dt
            veg_cs%grainc_storage(p)     = veg_cs%grainc_storage(p)    - veg_cf%grainc_storage_to_xfer(p)*dt
            veg_cs%grainc_xfer(p)        = veg_cs%grainc_xfer(p)       + veg_cf%grainc_storage_to_xfer(p)*dt
         end if

         ! end do ! end of patch loop

       end associate

  end subroutine CarbonStateUpdate_Phase1_PFT

end module CarbonStateUpdate1Mod
