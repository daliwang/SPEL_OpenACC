module WaterBudgetMod
  ! !USES:
  use shr_kind_mod      , only : r8 => shr_kind_r8
  !#py !#py use shr_log_mod       , only : errMsg => shr_log_errMsg
  !#py use shr_sys_mod       , only : shr_sys_abort
  use decompMod         , only : bounds_type
  !#py use abortutils        , only : endrun
  use clm_varctl        , only : iulog
  use atm2lndType       , only : atm2lnd_type
  use lnd2atmType       , only : lnd2atm_type
  use WaterfluxType     , only : waterflux_type
  !#py use spmdMod           , only : masterproc
  use SoilHydrologyType , only : soilhydrology_type
  use GridcellDataType  , only : grc_ws
  use ColumnDataType    , only : col_ws

  implicit none
  save
  private

  public :: WaterBudget_Reset
  public :: WaterBudget_Run
  public :: WaterBudget_Accum
  !public :: WaterBudget_Sum_Local
  public :: WaterBudget_Print
  !#py public :: WaterBudget_Restart
  public :: WaterBudget_SetBeginningMonthlyStates
  public :: WaterBudget_SetEndingMonthlyStates

  !--- F for flux ---
  integer, parameter :: f_rain = 1
  integer, parameter :: f_snow = 2
  integer, parameter :: f_evap = 3
  integer, parameter :: f_roff = 4
  integer, parameter :: f_ioff = 5

  integer, parameter, public :: f_size = f_ioff

  character(len=12),parameter :: fname(f_size) = &
       (/&
       '        rain', &
       '        snow', &
       '        evap', &
       '      runoff', &
       '      frzrof'  &
       /)

  !--- S for state ---
  integer, parameter :: s_w_beg     = 1
  integer, parameter :: s_w_end     = 2
  integer, parameter :: s_wcan_beg  = 3
  integer, parameter :: s_wcan_end  = 4
  integer, parameter :: s_wsno_beg  = 5
  integer, parameter :: s_wsno_end  = 6
  integer, parameter :: s_wsfc_beg  = 7
  integer, parameter :: s_wsfc_end  = 8
  integer, parameter :: s_wsliq_beg = 9
  integer, parameter :: s_wsliq_end = 10
  integer, parameter :: s_wsice_beg = 11
  integer, parameter :: s_wsice_end = 12
  integer, parameter :: s_wwa_beg   = 13
  integer, parameter :: s_wwa_end   = 14
  integer, parameter :: s_w_errh2o  = 15

  integer, parameter, public :: s_size = s_w_errh2o

  character(len=12),parameter :: sname(s_size) = &
       (/&
       ' total_w_beg', &
       ' total_w_end', &
       'canopy_w_beg', &
       'canopy_w_end', &
       '  snow_w_beg', &
       '  snow_w_end', &
       '   sfc_w_beg', &
       '   sfc_w_end', &
       'soiliq_w_beg', &
       'soiliq_w_end', &
       'soiice_w_beg', &
       'soiice_w_end', &
       ' aquif_w_beg', &
       ' aquif_w_end', &
       '   error h2o'  &
       /)

  !--- P for period ---

  integer, parameter :: p_inst = 1
  integer, parameter :: p_day  = 2
  integer, parameter :: p_mon  = 3
  integer, parameter :: p_ann  = 4
  integer, parameter :: p_inf  = 5

  integer, parameter, public :: p_size = p_inf

  character(len=8),parameter :: pname(p_size) = &
       (/'    inst','   daily',' monthly','  annual','all_time' /)

  real(r8) :: budg_fluxL(f_size, p_size)
  real(r8) :: budg_fluxG(f_size, p_size)
  real(r8) :: budg_fluxN(f_size, p_size)

  real(r8) :: budg_stateL(s_size, p_size)
  real(r8), public :: budg_stateG(s_size, p_size)

  logical,save :: first_time = .true.

  !----- formats -----
  character(*),parameter :: FA0= "('    ',12x,(3x,a10,2x),' | ',(3x,a10,2x))"
  character(*),parameter :: FF = "('    ',a12,f15.8,' | ',f18.2)"
  character(*),parameter :: FF2= "('    ',a12,a15,' | ',f18.2)"
  character(*),parameter :: FS = "('    ',a12,6(f18.2),18x,' | ',(f18.2))"
  character(*),parameter :: FS0= "('    ',12x,7(a18),' | ',(a18))"
  character(*),parameter :: FS2= "('    ',a12,54x,f18.2,54x,' | ',f18.2)"
  character(*),parameter :: FS3= "('    ',a12,7(f18.2),' | ',(f18.2))"

contains

  !-----------------------------------------------------------------------
  subroutine WaterBudget_Reset(mode)
    !
    !#py use clm_time_manager, only : get_curr_date, get_prev_date
    !
    implicit none
    !
    character(len=*), intent(in),optional :: mode
    !
    integer :: year, mon, day, sec
    integer :: ip
    character(*),parameter :: subName = '(WaterBudget_Reset) '

    if (.not.present(mode)) then
       !#py call get_curr_date(year, mon, day, sec)
       !#py call get_prev_date(year, mon, day, sec)

       do ip = 1,p_size
          if (ip == p_inst) then
             budg_fluxL(:,ip)  = 0.0_r8
             budg_fluxG(:,ip)  = 0.0_r8
             budg_fluxN(:,ip)  = 0.0_r8
          endif
          if (ip==p_day .and. sec==0) then
             budg_fluxL(:,ip)  = 0.0_r8
             budg_fluxG(:,ip)  = 0.0_r8
             budg_fluxN(:,ip)  = 0.0_r8
          endif
          if (ip==p_mon .and. day==1 .and. sec==0) then
             budg_fluxL(:,ip)  = 0.0_r8
             budg_fluxG(:,ip)  = 0.0_r8
             budg_fluxN(:,ip)  = 0.0_r8
          endif
          if (ip==p_ann .and. mon==1 .and. day==1 .and. sec==0) then
             budg_fluxL(:,ip)  = 0.0_r8
             budg_fluxG(:,ip)  = 0.0_r8
             budg_fluxN(:,ip)  = 0.0_r8
          endif
       enddo

    else

       if (trim(mode) == 'inst') then
          budg_fluxL  (:,p_inst)   = 0.0_r8
          budg_fluxG  (:,p_inst)   = 0.0_r8
          budg_fluxN  (:,p_inst)   = 0.0_r8
          budg_stateL (:,p_inst)   = 0.0_r8
          budg_stateG (:,p_inst)   = 0.0_r8
       elseif (trim(mode) == 'day') then
          budg_fluxL  (:,p_day)    = 0.0_r8
          budg_fluxG  (:,p_day)    = 0.0_r8
          budg_fluxN  (:,p_day)    = 0.0_r8
          budg_stateL (:,p_day)    = 0.0_r8
          budg_stateG (:,p_day)    = 0.0_r8
       elseif (trim(mode) == 'mon') then
          budg_fluxL  (:,p_mon)    = 0.0_r8
          budg_fluxG  (:,p_mon)    = 0.0_r8
          budg_fluxN  (:,p_mon)    = 0.0_r8
          budg_stateL (:,p_mon)    = 0.0_r8
          budg_stateG (:,p_mon)    = 0.0_r8
       elseif (trim(mode) == 'ann') then
          budg_fluxL  (:,p_ann)    = 0.0_r8
          budg_fluxG  (:,p_ann)    = 0.0_r8
          budg_fluxN  (:,p_ann)    = 0.0_r8
          budg_stateL (:,p_ann)    = 0.0_r8
          budg_stateG (:,p_ann)    = 0.0_r8
       elseif (trim(mode) == 'inf') then
          budg_fluxL  (:,p_inf)    = 0.0_r8
          budg_fluxG  (:,p_inf)    = 0.0_r8
          budg_fluxN  (:,p_inf)    = 0.0_r8
          budg_stateL (:,p_inf)    = 0.0_r8
          budg_stateG (:,p_inf)    = 0.0_r8
       elseif (trim(mode) == 'all') then
          budg_fluxL  (:,:)        = 0.0_r8
          budg_fluxG  (:,:)        = 0.0_r8
          budg_fluxN  (:,:)        = 0.0_r8
          budg_stateL (:,:)        = 0.0_r8
          budg_stateG (:,:)        = 0.0_r8
       else
          !#py call shr_sys_abort(subname//' ERROR in mode '//trim(mode))
       endif
    endif

  end subroutine WaterBudget_Reset

!-----------------------------------------------------------------------
  subroutine WaterBudget_Accum()
    !
    !#py use clm_time_manager, only : get_curr_date, get_prev_date, get_nstep
    !
    implicit none
    !
    integer                :: ip, nf
    integer                :: year_prev, month_prev, day_prev, sec_prev
    integer                :: year_curr, month_curr, day_curr, sec_curr
    character(*),parameter :: subName = '(WaterBudget_Accum)'
    logical                :: update_state_beg, update_state_end

    !#py call get_prev_date(year_prev, month_prev, day_prev, sec_prev)
    !#py call get_curr_date(year_curr, month_curr, day_curr, sec_curr)

    do ip = p_inst+1, p_size
       budg_fluxL(:,ip) = budg_fluxL(:,ip) + budg_fluxL(:,p_inst)
       update_state_beg = .false.
       update_state_end = .false.

       select case (ip)
       case (p_day)
          if (sec_prev == 0) update_state_beg = .true.
          if (sec_curr == 0) update_state_end = .true.
       case (p_mon)
          if (sec_prev == 0 .and. day_prev == 1) update_state_beg = .true.
          if (sec_curr == 0 .and. day_curr == 1) update_state_end = .true.
       case (p_ann)
          if (sec_prev == 0 .and. day_prev == 1 .and. month_prev == 1) update_state_beg = .true.
          if (sec_curr == 0 .and. day_curr == 1 .and. month_curr == 1) update_state_end = .true.
       case (p_inf)
          !#py if (get_nstep() == 1) update_state_beg = .true.
          update_state_end = .true.
       end select

       if (update_state_beg) then
          nf = s_w_beg     ; budg_stateL(nf,ip) = budg_stateL(nf, p_inst)
          nf = s_wcan_beg  ; budg_stateL(nf,ip) = budg_stateL(nf, p_inst)
          nf = s_wsno_beg  ; budg_stateL(nf,ip) = budg_stateL(nf, p_inst)
          nf = s_wsfc_beg  ; budg_stateL(nf,ip) = budg_stateL(nf, p_inst)
          nf = s_wsliq_beg ; budg_stateL(nf,ip) = budg_stateL(nf, p_inst)
          nf = s_wsice_beg ; budg_stateL(nf,ip) = budg_stateL(nf, p_inst)
          nf = s_wwa_beg   ; budg_stateL(nf,ip) = budg_stateL(nf, p_inst)
       endif

       if (update_state_end) then
          nf = s_w_end     ; budg_stateL(nf,ip) = budg_stateL(nf, p_inst)
          nf = s_wcan_end  ; budg_stateL(nf,ip) = budg_stateL(nf, p_inst)
          nf = s_wsno_end  ; budg_stateL(nf,ip) = budg_stateL(nf, p_inst)
          nf = s_wsfc_end  ; budg_stateL(nf,ip) = budg_stateL(nf, p_inst)
          nf = s_wsliq_end ; budg_stateL(nf,ip) = budg_stateL(nf, p_inst)
          nf = s_wsice_end ; budg_stateL(nf,ip) = budg_stateL(nf, p_inst)
          nf = s_wwa_end   ; budg_stateL(nf,ip) = budg_stateL(nf, p_inst)
       endif
       nf = s_w_errh2o  ; budg_stateL(nf,ip) = budg_stateL(nf,ip) + budg_stateL(nf, p_inst)
    end do
    budg_fluxN(:,:) = budg_fluxN(:,:) + 1._r8

  end subroutine WaterBudget_Accum

  !-----------------------------------------------------------------------
  subroutine WaterBudget_Run(bounds, atm2lnd_vars, lnd2atm_vars,  &
       soilhydrology_vars)
    !
    ! !DESCRIPTION:
    !
    use domainMod, only : ldomain
    use clm_varcon, only : re
    !
    implicit none

    type(bounds_type)        , intent(in) :: bounds
    type(atm2lnd_type)       , intent(in) :: atm2lnd_vars
    type(lnd2atm_type)       , intent(in) :: lnd2atm_vars
    type(soilhydrology_type) , intent(in) :: soilhydrology_vars

    integer  :: g, nf, ip
    real(r8) :: af, one_over_re2

    associate(                                                             &
         forc_rain          => atm2lnd_vars%forc_rain_not_downscaled_grc , &
         forc_snow          => atm2lnd_vars%forc_snow_not_downscaled_grc , &
         qflx_evap_tot      => lnd2atm_vars%qflx_evap_tot_grc            , &
         qflx_rofice        => lnd2atm_vars%qflx_rofice_grc              , &
         qflx_rofliq_qsur   => lnd2atm_vars%qflx_rofliq_qsur_grc         , &
         qflx_rofliq_qsurp  => lnd2atm_vars%qflx_rofliq_qsurp_grc        , &
         qflx_rofliq_qsub   => lnd2atm_vars%qflx_rofliq_qsub_grc         , &
         qflx_rofliq_qsubp  => lnd2atm_vars%qflx_rofliq_qsubp_grc        , &
         qflx_rofliq_qgwl   => lnd2atm_vars%qflx_rofliq_qgwl_grc         , &
         begwb_grc          => grc_ws%begwb                 , &
         endwb_grc          => grc_ws%endwb                 , &
         beg_wa_grc         => soilhydrology_vars%beg_wa_grc             , &
         beg_h2ocan_grc     => grc_ws%beg_h2ocan            , &
         beg_h2osno_grc     => grc_ws%beg_h2osno            , &
         beg_h2osfc_grc     => grc_ws%beg_h2osfc            , &
         beg_h2osoi_liq_grc => grc_ws%beg_h2osoi_liq        , &
         beg_h2osoi_ice_grc => grc_ws%beg_h2osoi_ice        , &
         end_wa_grc         => soilhydrology_vars%end_wa_grc             , &
         end_h2ocan_grc     => grc_ws%end_h2ocan            , &
         end_h2osno_grc     => grc_ws%end_h2osno            , &
         end_h2osfc_grc     => grc_ws%end_h2osfc            , &
         end_h2osoi_liq_grc => grc_ws%end_h2osoi_liq        , &
         end_h2osoi_ice_grc => grc_ws%end_h2osoi_ice        , &
         errh2o_grc         => grc_ws%errh2o                  &
         )

      ip = p_inst

      budg_stateL(:,ip) = 0._r8
      one_over_re2 = 1._r8/(re**2._r8)

      do g = bounds%begg, bounds%endg

        af   = (ldomain%area(g) * one_over_re2) * & ! area (converting km**2 to radians**2)
               ldomain%frac(g)                      ! land fraction

         nf = f_rain; budg_fluxL(nf,ip) = budg_fluxL(nf,ip) + forc_rain(g)*af
         nf = f_snow; budg_fluxL(nf,ip) = budg_fluxL(nf,ip) + forc_snow(g)*af
         nf = f_evap; budg_fluxL(nf,ip) = budg_fluxL(nf,ip) - qflx_evap_tot(g)*af
         nf = f_roff; budg_fluxL(nf,ip) = budg_fluxL(nf,ip) &
              - qflx_rofliq_qsur(g)*af - qflx_rofliq_qsurp(g)*af &
              - qflx_rofliq_qsub(g)*af - qflx_rofliq_qsubp(g)*af &
              - qflx_rofliq_qgwl(g)*af
         nf = f_ioff; budg_fluxL(nf,ip) = budg_fluxL(nf,ip) - qflx_rofice(g)*af

         nf = s_w_beg     ; budg_stateL(nf,ip) = budg_stateL(nf,ip) + begwb_grc(g)          *af
         nf = s_w_end     ; budg_stateL(nf,ip) = budg_stateL(nf,ip) + endwb_grc(g)          *af
         nf = s_wcan_beg  ; budg_stateL(nf,ip) = budg_stateL(nf,ip) + beg_h2ocan_grc(g)     *af
         nf = s_wcan_end  ; budg_stateL(nf,ip) = budg_stateL(nf,ip) + end_h2ocan_grc(g)     *af
         nf = s_wsno_beg  ; budg_stateL(nf,ip) = budg_stateL(nf,ip) + beg_h2osno_grc(g)     *af
         nf = s_wsno_end  ; budg_stateL(nf,ip) = budg_stateL(nf,ip) + end_h2osno_grc(g)     *af
         nf = s_wsfc_beg  ; budg_stateL(nf,ip) = budg_stateL(nf,ip) + beg_h2osfc_grc(g)     *af
         nf = s_wsfc_end  ; budg_stateL(nf,ip) = budg_stateL(nf,ip) + end_h2osfc_grc(g)     *af
         nf = s_wsliq_beg ; budg_stateL(nf,ip) = budg_stateL(nf,ip) + beg_h2osoi_liq_grc(g) *af
         nf = s_wsliq_end ; budg_stateL(nf,ip) = budg_stateL(nf,ip) + end_h2osoi_liq_grc(g) *af
         nf = s_wsice_beg ; budg_stateL(nf,ip) = budg_stateL(nf,ip) + beg_h2osoi_ice_grc(g) *af
         nf = s_wsice_end ; budg_stateL(nf,ip) = budg_stateL(nf,ip) + end_h2osoi_ice_grc(g) *af
         nf = s_wwa_beg   ; budg_stateL(nf,ip) = budg_stateL(nf,ip) + beg_wa_grc(g)         *af
         nf = s_wwa_end   ; budg_stateL(nf,ip) = budg_stateL(nf,ip) + end_wa_grc(g)         *af
         nf = s_w_errh2o  ; budg_stateL(nf,ip) = budg_stateL(nf,ip) + errh2o_grc(g)         *af
      end do

    end associate

  end subroutine WaterBudget_Run

!-----------------------------------------------------------------------
!#py   subroutine WaterBudget_Sum0()
!#py     !
!#py     use spmdMod    , only : mpicom
!#py     use shr_mpi_mod, only : shr_mpi_sum
!#py     !
!#py     implicit none
!#py     !
!#py     real(r8)               :: budg_fluxGtmp(f_size,p_size) ! temporary sum
!#py     real(r8)               :: budg_stateGtmp(s_size,p_size) ! temporary sum
!#py     character(*),parameter :: subName = '(WaterBudget_Sum0)'
!#py !#py
!#py     budg_fluxGtmp = 0._r8
!#py     budg_stateGtmp = 0._r8
!#py !#py
!#py     call shr_mpi_sum(budg_fluxL, budg_fluxGtmp, mpicom, subName)
!#py     call shr_mpi_sum(budg_stateL, budg_stateGtmp, mpicom, subName)
!#py !#py
!#py     budg_fluxG  = budg_fluxG + budg_fluxGtmp
!#py     budg_stateG = budg_stateGtmp
!#py !#py
!#py     budg_fluxL            = 0._r8 ! reset all fluxes
!#py     budg_stateL(:,p_inst) = 0._r8 ! only reset instantaneous states
!#py !#py
!#py   end subroutine WaterBudget_Sum0

  !-----------------------------------------------------------------------

  subroutine WaterBudget_Print(budg_print_inst,  budg_print_daily,  budg_print_month,  &
       budg_print_ann,  budg_print_ltann,  budg_print_ltend, year, mon, day, sec)
    !
    !#py use clm_time_manager, only : get_curr_date, get_prev_date, get_nstep, get_step_size
    use shr_const_mod   , only : shr_const_pi
    !
    implicit none
    !
    integer , intent(in) :: budg_print_inst
    integer , intent(in) :: budg_print_daily
    integer , intent(in) :: budg_print_month
    integer , intent(in) :: budg_print_ann
    integer , intent(in) :: budg_print_ltann
    integer , intent(in) :: budg_print_ltend
    integer , intent(in) :: year, mon, day, sec
    !!Nstep Dependence!!
    !
    ! !LOCAL VARIABLES:
    integer :: s,f,ic,nf,ip,is ! data array indicies
    integer :: plev        ! print level

    integer :: cdate
    logical :: sumdone
    real(r8) :: unit_conversion
    real(r8) :: budg_fluxGpr (f_size,p_size) ! values to print, scaled and such

    sumdone = .false.

    !if (get_nstep() <= 1) then
       !call get_prev_date(year, mon, day, sec);
    !else
       !call get_curr_date(year, mon, day, sec);
    !end if

    cdate = year*10000 + mon*100 + day

    do ip = 1,p_size
       plev = 0
       if (ip == p_inst) then
          plev = max(plev,budg_print_inst)
       endif
       if (ip==p_day .and. sec==0) then
          plev = max(plev,budg_print_daily)
       endif
       if (ip==p_mon .and. day==1 .and. sec==0) then
          plev = max(plev,budg_print_month)
       endif
       if (ip==p_ann .and. mon==1 .and. day==1 .and. sec==0) then
          plev = max(plev,budg_print_ann)
       endif
       if (ip==p_inf .and. mon==1 .and. day==1 .and. sec==0) then
          plev = max(plev,budg_print_ltann)
       endif

       if (plev > 0) then
          unit_conversion = 1.d0/(4.0_r8*shr_const_pi)*1.0e6_r8
          if (.not.sumdone) then
             sumdone = .true.
             !#py call WaterBudget_Sum0()
             budg_fluxGpr = budg_fluxG
             budg_fluxGpr = budg_fluxGpr*unit_conversion
             budg_fluxGpr = budg_fluxGpr/budg_fluxN
          end if

          !#py if (ip == p_day .and. get_nstep() == 1) cycle
          !#py if (ip == p_mon .and. get_nstep() == 1) cycle
          !#py if (ip == p_ann .and. get_nstep() == 1) cycle
          !#py if (ip == p_inf .and. get_nstep() == 1) cycle

          if (0) then
             !#py write(iulog,*)''
             !#py write(iulog,*)'NET WATER FLUXES : period ',trim(pname(ip)),': date = ',cdate,sec
             !#py write(iulog,FA0)'  Time  ','  Time    '
             !#py write(iulog,FA0)'averaged','integrated'
             !#py write(iulog,FA0)'kg/m2s*1e6','kg/m2*1e6'
             !#py write(iulog,'(32("-"),"|",20("-"))')
             do f = 1, f_size
                 !#py write(iulog,FF)fname(f),budg_fluxGpr(f,ip),budg_fluxG(f,ip)*unit_conversion*get_step_size()
             end do
             !#py write(iulog,'(32("-"),"|",20("-"))')
             !#py write(iulog,FF)'   *SUM*', &
                  !#py sum(budg_fluxGpr(:,ip)), sum(budg_fluxG(:,ip))*unit_conversion*get_step_size()
             !#py write(iulog,'(32("-"),"|",20("-"))')

             !#py write(iulog,*)''
             !#py write(iulog,*)'WATER STATES (kg/m2*1e6): period ',trim(pname(ip)),': date = ',cdate,sec
             !#py write(iulog,FS0) &
                  !#py '       Canopy  ', &
                  !#py '       Snow    ', &
                  !#py '       SFC     ', &
                  !#py '     Soil Liq  ', &
                  !#py '     Soil Ice  ', &
                  !#py '      Aquifer  ', &
                  !#py ' Grid-level Err', &
                  !#py '       TOTAL   '
             !#py write(iulog,'(143("-"),"|",23("-"))')
             !#py write(iulog,FS) '         beg', &
                  !#py budg_stateG(s_wcan_beg  , ip)*unit_conversion, &
                  !#py budg_stateG(s_wsno_beg  , ip)*unit_conversion, &
                  !#py budg_stateG(s_wsfc_beg  , ip)*unit_conversion, &
                  !#py budg_stateG(s_wsliq_beg , ip)*unit_conversion, &
                  !#py budg_stateG(s_wsice_beg , ip)*unit_conversion, &
                  !#py budg_stateG(s_wwa_beg   , ip)*unit_conversion, &
                  !#py budg_stateG(s_w_beg     , ip)*unit_conversion
             !#py write(iulog,FS) '         end', &
                  !#py budg_stateG(s_wcan_end  , ip)*unit_conversion, &
                  !#py budg_stateG(s_wsno_end  , ip)*unit_conversion, &
                  !#py budg_stateG(s_wsfc_end  , ip)*unit_conversion, &
                  !#py budg_stateG(s_wsliq_end , ip)*unit_conversion, &
                  !#py budg_stateG(s_wsice_end , ip)*unit_conversion, &
                  !#py budg_stateG(s_wwa_end   , ip)*unit_conversion, &
                  !#py budg_stateG(s_w_end     , ip)*unit_conversion
             !#py write(iulog,FS3)'*NET CHANGE*', &
                  !#py (budg_stateG(s_wcan_end  ,ip) - budg_stateG(s_wcan_beg  ,ip))*unit_conversion, &
                  !#py (budg_stateG(s_wsno_end  ,ip) - budg_stateG(s_wsno_beg  ,ip))*unit_conversion, &
                  !#py (budg_stateG(s_wsfc_end  ,ip) - budg_stateG(s_wsfc_beg  ,ip))*unit_conversion, &
                  !#py (budg_stateG(s_wsliq_end ,ip) - budg_stateG(s_wsliq_beg ,ip))*unit_conversion, &
                  !#py (budg_stateG(s_wsice_end ,ip) - budg_stateG(s_wsice_beg ,ip))*unit_conversion, &
                  !#py (budg_stateG(s_wwa_end   ,ip) - budg_stateG(s_wwa_beg   ,ip))*unit_conversion, &
                  !#py budg_stateG(s_w_errh2o ,ip) *unit_conversion, &
                  !#py (budg_stateG(s_w_end     ,ip) - budg_stateG(s_w_beg     ,ip))*unit_conversion
             !#py write(iulog,'(143("-"),"|",23("-"))')
             !#py write(iulog,FS2)'   *SUM*    ', &
                  !#py (budg_stateG(s_wcan_end  ,ip) - budg_stateG(s_wcan_beg  ,ip))*unit_conversion + &
                  !#py (budg_stateG(s_wsno_end  ,ip) - budg_stateG(s_wsno_beg  ,ip))*unit_conversion + &
                  !#py (budg_stateG(s_wsfc_end  ,ip) - budg_stateG(s_wsfc_beg  ,ip))*unit_conversion + &
                  !#py (budg_stateG(s_wsliq_end ,ip) - budg_stateG(s_wsliq_beg ,ip))*unit_conversion + &
                  !#py (budg_stateG(s_wsice_end ,ip) - budg_stateG(s_wsice_beg ,ip))*unit_conversion + &
                  !#py (budg_stateG(s_wwa_end   ,ip) - budg_stateG(s_wwa_beg   ,ip))*unit_conversion - &
                   !#py budg_stateG(s_w_errh2o ,ip) *unit_conversion, &
                  !#py (budg_stateG(s_w_end     ,ip) - budg_stateG(s_w_beg     ,ip))*unit_conversion
             !#py write(iulog,'(143("-"),"|",23("-"))')
          end if
       end if
    end do

  end subroutine WaterBudget_Print

  !-----------------------------------------------------------------------
!#py   subroutine WaterBudget_Restart(bounds, ncid, flag)
!#py     !
!#py     use ncdio_pio, only : file_desc_t, ncd_io, ncd_double, ncd_int
!#py     use ncdio_pio, only : ncd_defvar
!#py     !
!#py     implicit none
!#py     !
!#py     type(bounds_type), intent(in)    :: bounds
!#py     type(file_desc_t), intent(inout) :: ncid   ! netcdf id
!#py     character(len=*) , intent(in)    :: flag   ! 'read' or 'write'
!#py     !
!#py     character(len=*),parameter :: subname = 'WaterBudget_Restart'
!#py !#py
!#py     select case (trim(flag))
!#py     case ('define')
!#py        call WaterBudget_Restart_Define(bounds, ncid)
!#py     case ('write')
!#py        call WaterBudget_Restart_Write(bounds, ncid, flag)
!#py     case ('read')
!#py        call WaterBudget_Restart_Read(bounds, ncid, flag)
!#py     case default
!#py        write(iulog,*) trim(subname),' ERROR: unknown flag = ',flag
!#py        call endrun(msg=errMsg(__FILE__, __LINE__))
!#py     end select
!#py !#py
!#py   end subroutine WaterBudget_Restart

  !-----------------------------------------------------------------------
!#py   subroutine WaterBudget_Restart_Define(bounds, ncid)
!#py     !
!#py     use ncdio_pio, only : file_desc_t, ncd_io, ncd_double, ncd_defvar
!#py     !
!#py     implicit none
!#py     !
!#py     type(bounds_type), intent(in)    :: bounds
!#py     type(file_desc_t), intent(inout) :: ncid   ! netcdf id
!#py !#py
!#py     call ncd_defvar(varname='budg_fluxG', xtype=ncd_double, &
!#py          dim1name='budg_flux', &
!#py          long_name='budg_fluxG', units='mm', ncid=ncid)
!#py !#py
!#py     call ncd_defvar(varname='budg_fluxN', xtype=ncd_double, &
!#py          dim1name='budg_flux', &
!#py          long_name='budg_fluxN', units='-', ncid=ncid)
!#py !#py
!#py     call ncd_defvar(varname='budg_stateG', xtype=ncd_double, &
!#py          dim1name='budg_state', &
!#py          long_name='budg_stateG', units='mm', ncid=ncid)
!#py !#py
!#py   end subroutine WaterBudget_Restart_Define

  !-----------------------------------------------------------------------
!#py   subroutine WaterBudget_Restart_Write(bounds, ncid, flag)
!#py     !
!#py     use ncdio_pio   , only : file_desc_t, ncd_io, ncd_double, ncd_int
!#py     use ncdio_pio   , only : ncd_defvar
!#py     use spmdMod     , only : mpicom
!#py     use shr_mpi_mod , only : shr_mpi_sum
!#py     !
!#py     implicit none
!#py     !
!#py     type(bounds_type), intent(in)    :: bounds
!#py     type(file_desc_t), intent(inout) :: ncid   ! netcdf id
!#py     character(len=*) , intent(in)    :: flag   ! 'read' or 'write'
!#py     !
!#py     ! !LOCAL VARIABLES:
!#py     real(r8) :: budg_fluxGtmp(f_size,p_size) ! temporary sum
!#py     real(r8) :: budg_stateGtmp(s_size,p_size) ! temporary sum
!#py     real(r8) :: budg_fluxG_1D (f_size*p_size)
!#py     real(r8) :: budg_fluxN_1D (f_size*p_size)
!#py     real(r8) :: budg_stateG_1D(s_size*p_size)
!#py     integer  :: f, s, p, count
!#py     character(*),parameter :: subName = '(WaterBudget_Restart_Write) '
!#py !#py
!#py     call shr_mpi_sum(budg_fluxL, budg_fluxGtmp, mpicom, subName)
!#py     call shr_mpi_sum(budg_stateL, budg_stateGtmp, mpicom, subName)
!#py !#py
!#py     ! Copy data from 2D into 1D array
!#py     count = 0
!#py     do f = 1, f_size
!#py        do p = 1, p_size
!#py           count = count + 1
!#py           budg_fluxG_1D(count) = budg_fluxG(f,p) + budg_fluxGtmp(f,p)
!#py           budg_fluxN_1D(count) = budg_fluxN(f,p)
!#py        end do
!#py     end do
!#py !#py
!#py     ! Copy data from 2D into 1D array
!#py     count = 0
!#py     do s = 1, s_size
!#py        do p = 1, p_size
!#py           count = count + 1
!#py           budg_stateG_1D(count) = budg_stateGtmp(s,p)
!#py        end do
!#py     end do
!#py !#py
!#py     call ncd_io(flag=flag, varname='budg_fluxG', data=budg_fluxG_1D, ncid=ncid)
!#py     call ncd_io(flag=flag, varname='budg_fluxN', data=budg_fluxN_1D, ncid=ncid)
!#py     call ncd_io(flag=flag, varname='budg_stateG', data=budg_stateG_1D, ncid=ncid)
!#py !#py
!#py   end subroutine WaterBudget_Restart_Write

  !-----------------------------------------------------------------------
!#py   subroutine WaterBudget_Restart_Read(bounds, ncid, flag)
!#py     !
!#py     use ncdio_pio, only : file_desc_t, ncd_io, ncd_double, ncd_int
!#py     use ncdio_pio, only : ncd_defvar
!#py     !
!#py     implicit none
!#py     !
!#py     type(bounds_type), intent(in)    :: bounds
!#py     type(file_desc_t), intent(inout) :: ncid   ! netcdf id
!#py     character(len=*) , intent(in)    :: flag   ! 'read' or 'write'
!#py     !
!#py     ! !LOCAL VARIABLES:
!#py     real(r8) :: budg_fluxG_1D (f_size*p_size)
!#py     real(r8) :: budg_fluxN_1D (f_size*p_size)
!#py     real(r8) :: budg_stateG_1D(s_size*p_size)
!#py     integer  :: f, s, p, count
!#py !#py
!#py     call ncd_io(flag=flag, varname='budg_fluxG', data=budg_fluxG_1D, ncid=ncid)
!#py     call ncd_io(flag=flag, varname='budg_fluxN', data=budg_fluxN_1D, ncid=ncid)
!#py     call ncd_io(flag=flag, varname='budg_stateG', data=budg_stateG_1D, ncid=ncid)
!#py !#py
!#py     ! Copy data from 1D into 2D array
!#py     count = 0
!#py     do f = 1, f_size
!#py        do p = 1, p_size
!#py           count = count + 1
!#py           budg_fluxG(f,p) = budg_fluxG_1D(count)
!#py           budg_fluxN(f,p) = budg_fluxN_1D(count)
!#py        end do
!#py     end do
!#py !#py
!#py     ! Copy data from 1D into 2D array
!#py     if (masterproc) then
!#py        count = 0
!#py        do s = 1, s_size
!#py           do p = 1, p_size
!#py              count = count + 1
!#py              budg_stateL(s,p) = budg_stateG_1D(count)
!#py           end do
!#py        end do
!#py     end if
!#py !#py
!#py   end subroutine WaterBudget_Restart_Read

   !-----------------------------------------------------------------------
  subroutine WaterBudget_SetBeginningMonthlyStates(bounds, nstep, day_curr, sec_curr &
                                , day_prev, sec_prev)
    !
    ! !DESCRIPTION:
    ! Set grid-level water states at the beginning of a month
    !$acc routine seq
    ! !USES:
    use subgridAveMod    , only : p2c, c2g
    use clm_varpar       , only : nlevgrnd, nlevsoi, nlevurb
    use clm_varcon       , only : spval
    use column_varcon    , only : icol_roof, icol_sunwall, icol_shadewall
    use column_varcon    , only : icol_road_perv, icol_road_imperv
    !#py use clm_time_manager , only : get_curr_date, get_prev_date, get_nstep
    !
    ! !ARGUMENTS:
    type(bounds_type)         , intent(in)    :: bounds
    integer , intent(in) :: nstep
    !
    ! !LOCAL VARIABLES:
    integer, intent(in) :: day_curr, sec_curr
    integer, intent(in) :: day_prev, sec_prev
    !-----------------------------------------------------------------------

    associate(                                                       &
         begwb             =>    col_ws%begwb         , & ! Output: [real(r8) (:)   ]  water mass begining of the time step
         endwb             =>    col_ws%endwb         , & ! Output: [real(r8) (:)   ]  water mass begining of the time step
         tws_month_beg_grc =>    grc_ws%tws_month_beg   & ! Output: [real(r8) (:)   ]  grid-level water mass at the begining of a month
         )

      ! If at the beginning of a simulation, save grid-level TWS based on
      ! 'begwb' from the current time step
      if ( day_curr == 1 .and. sec_curr == 0 .and. nstep <= 1 ) then
         call c2g( bounds, begwb , tws_month_beg_grc , &
              c2l_scale_type= 1, l2g_scale_type=0 )
      endif

      ! If multiple steps into a simulation and the last time step was the
      ! end of a month, save grid-level TWS based on 'endwb' from the last
      ! time step
      if (nstep > 1 .and. day_prev == 1 .and. sec_prev == 0) then
         call c2g( bounds,  endwb , tws_month_beg_grc , &
              c2l_scale_type= 1, l2g_scale_type=0 )
      endif

    end associate

  end subroutine WaterBudget_SetBeginningMonthlyStates

   !-----------------------------------------------------------------------
  subroutine WaterBudget_SetEndingMonthlyStates(bounds, nstep, year, mon, day, sec)
    !
    ! !DESCRIPTION:
    ! Set grid-level water states at the end of a month
    !
    ! !USES:
      !$acc routine seq
    use subgridAveMod    , only : c2g
    use clm_varcon       , only : spval
    !#py use clm_time_manager , only : get_curr_date, get_nstep
    !
    ! !ARGUMENTS:
    type(bounds_type)         , intent(in)    :: bounds
    integer, intent(in)  :: nstep, year, mon, day, sec
    !
    ! !LOCAL VARIABLES:
    !-----------------------------------------------------------------------

    associate(                                                       &
         endwb             =>    col_ws%endwb         , & ! Output: [real(r8) (:)   ]  water mass at end of the time step
         tws_month_end_grc =>    grc_ws%tws_month_end   & ! Output: [real(r8) (:)   ]  grid-level water mass at the end of a month
         )

      ! If this is the end of a month, save grid-level total water storage
      !!call get_curr_date(year, mon, day, sec);

      if (nstep >= 1 .and. (day == 1 .and. sec == 0)) then
         call c2g( bounds, &
              endwb, &
              tws_month_end_grc, &
              c2l_scale_type= 1, l2g_scale_type=0 )
      else
         tws_month_end_grc(bounds%begg:bounds%endg) = spval
      end if

    end associate

  end subroutine WaterBudget_SetEndingMonthlyStates

end module WaterBudgetMod
