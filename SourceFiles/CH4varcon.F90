module CH4varcon

  !-----------------------------------------------------------------------
  ! Module containing CH4 parameters and logical switches and routine to read constants from CLM namelist.
  !
  use shr_kind_mod, only : r8 => shr_kind_r8
  !#py use abortutils  , only : endrun
  use elm_varctl  , only : iulog
  use elm_varctl  , only : NLFileName_in
  implicit none
  save
  !
  ! Methane Model Parameters
  !

  logical :: use_aereoxid_prog = .true. ! if false then aereoxid is read off of
  ! the parameter file and may be modifed by the user (default aereoxid on the
  ! file is 0.0).

  logical :: transpirationloss = .true. ! switch for activating CH4 loss from transpiration
                                      ! Transpiration loss assumes that the methane concentration in dissolved soil
                                      ! water remains constant through the plant and is released when the water evaporates
                                      ! from the stomata.
                                      ! Currently hard-wired to true; impact is < 1 Tg CH4/yr

  logical :: allowlakeprod = .false. ! Switch to allow production under lakes based on soil carbon dataset
                                     ! (Methane can be produced, and CO2 produced from methane oxidation,
                                     ! which will slowly reduce the available carbon stock, if ! replenishlakec, but no other biogeochem is done.)
                                     ! Note: switching this off turns off ALL lake methane biogeochem. However, 0 values
                                     ! will still be averaged into the concentration _sat history fields.


  logical :: usephfact = .false. ! Switch to use pH factor in methane production

  logical :: replenishlakec = .true. ! Switch for keeping carbon storage under lakes constant
                                      ! so that lakes do not affect the carbon balance
                                      ! Good for long term rather than transient warming experiments
               ! NOTE SWITCHING THIS OFF ASSUMES TRANSIENT CARBON SUPPLY FROM LAKES; COUPLED MODEL WILL NOT CONSERVE CARBON
               ! IN THIS MODE.

  ! New namelists added 6/12/11

  logical :: fin_use_fsat = .false. ! Use fsat rather than the inversion to Prigent satellite inundation obs. (applied to
                                    ! CLM water table depth and surface runoff) to calculated finundated which is
                                    ! used in methane code and potentially soil code
                                    !!!! Attn EK: Set this to true when Sean Swenson's prognostic, tested
                                       ! fsat is integrated. (CLM4 fsat is bad for these purposes.)

  logical :: usefrootc = .false.    ! Use CLMCN fine root C rather than ann NPP & LAI based parameterization to
                                    ! calculate tiller C for aerenchyma area calculation.
                                    ! The NPP & LAI param. was based on Wania for Arctic sedges and may not be
                                    ! appropriate for woody Patches, although nongrassporosratio above partly adjusts
                                    ! for this.  However, using fine root C reduces the aerenchyma area by a large
                                    ! factor.

  logical :: ch4offline = .true.    ! true --> Methane is not passed between the land & atmosphere.
                                    ! NEM is not added to NEE flux to atm. to correct for methane production,
                                    ! and ambient CH4 is set to constant 2009 value.

  logical :: ch4rmcnlim = .false.   ! Remove the N and low moisture limitations on SOM HR when calculating
                                    ! methanogenesis.
                                    ! Note: this option has not been extensively tested.
                                    ! Currently hardwired off.

  logical :: anoxicmicrosites = .false. ! Use Arah & Stephen 1998 expression to allow production above the water table
                                        ! Currently hardwired off; expression is crude.

  logical :: ch4frzout = .false.    ! Exclude CH4 from frozen fraction of soil pore H2O, to simulate "freeze-out" pulse
                                    ! as in Mastepanov 2008.
                                    ! Causes slight increase in emissions in the fall and decrease in the spring.
                                    ! Currently hardwired off; small impact.

  !-----------------------------------------------------------------------

  !$acc declare copyin(allowlakeprod   )
  !$acc declare copyin(replenishlakec  )
  !$acc declare copyin(fin_use_fsat    )
  !$acc declare copyin(ch4offline      )
  !$acc declare copyin(ch4rmcnlim      )
  !$acc declare copyin(anoxicmicrosites)
  !$acc declare copyin(ch4frzout        )
  !$acc declare copyin(usefrootc        )
  !$acc declare copyin(transpirationloss )
  !$acc declare copyin(use_aereoxid_prog )
  !$acc declare copyin(usephfact         )

  !#py public :: CH4conrd ! Read and initialize CH4 constants
  !-----------------------------------------------------------------------

contains

  !-----------------------------------------------------------------------
!#py   subroutine CH4conrd ()
!#py     !
!#py     ! !DESCRIPTION:
!#py     ! Read and initialize CH4 constants
!#py     !
!#py     ! !USES:
!#py     use fileutils   , only : relavu, getavu
!#py     use spmdMod     , only : masterproc, mpicom, MPI_REAL8, MPI_LOGICAL
!#py     use shr_nl_mod  , only : shr_nl_find_group_name
!#py     use shr_log_mod , only : errMsg => shr_log_errMsg
!#py     !
!#py     implicit none
!#py     !
!#py     integer :: i,j,n                ! loop indices
!#py     integer :: ierr                 ! error code
!#py     integer :: unitn                ! unit for namelist file
!#py     character(len=32) :: subname = 'CH4conrd'  ! subroutine name
!#py     !-----------------------------------------------------------------------
!#py 
!#py     ! ----------------------------------------------------------------------
!#py     ! Namelist Variables
!#py     ! ----------------------------------------------------------------------
!#py 
!#py     ! Driver
!#py     namelist /ch4par_in/ &
!#py          ch4offline, fin_use_fsat, replenishlakec, allowlakeprod
!#py 
!#py     ! Production
!#py     namelist /ch4par_in/ &
!#py          usephfact
!#py 
!#py     ! Methane
!#py     namelist /ch4par_in/ &
!#py          use_aereoxid_prog, usefrootc
!#py 
!#py        ! ----------------------------------------------------------------------
!#py        ! Read namelist from standard input.
!#py        ! ----------------------------------------------------------------------
!#py 
!#py     if (masterproc) then
!#py 
!#py        write(iulog,*) 'Attempting to read CH4 parameters .....'
!#py        unitn = getavu()
!#py        write(iulog,*) 'Read in ch4par_in namelist from: ', trim(NLFilename_in)
!#py        open( unitn, file=trim(NLFilename_in), status='old' )
!#py        call shr_nl_find_group_name(unitn, 'ch4par_in', status=ierr)
!#py        if (ierr == 0) then
!#py           read(unitn, ch4par_in, iostat=ierr)
!#py           if (ierr /= 0) then
!#py              call endrun(msg='error in reading in ch4par_in namelist'//&
!#py                   errMsg(__FILE__, __LINE__))
!#py           end if
!#py        end if
!#py        call relavu( unitn )
!#py 
!#py     end if ! masterproc
!#py 
!#py 
!#py     call mpi_bcast ( use_aereoxid_prog, 1 , MPI_LOGICAL, 0, mpicom, ierr )
!#py     call mpi_bcast (allowlakeprod, 1 , MPI_LOGICAL, 0, mpicom, ierr)
!#py     call mpi_bcast (usephfact, 1 , MPI_LOGICAL, 0, mpicom, ierr)
!#py     call mpi_bcast (replenishlakec, 1 , MPI_LOGICAL, 0, mpicom, ierr)
!#py     call mpi_bcast (fin_use_fsat, 1 , MPI_LOGICAL, 0, mpicom, ierr)
!#py     call mpi_bcast (usefrootc, 1 , MPI_LOGICAL, 0, mpicom, ierr)
!#py     call mpi_bcast (ch4offline, 1 , MPI_LOGICAL, 0, mpicom, ierr)
!#py 
!#py     if (masterproc) then
!#py        write(iulog,*) 'Successfully read CH4 namelist'
!#py        write(iulog,*)' '
!#py        write(iulog,*)'allowlakeprod = ', allowlakeprod
!#py        write(iulog,*)'usephfact = ', usephfact
!#py        write(iulog,*)'replenishlakec = ', replenishlakec
!#py        write(iulog,*)'fin_use_fsat = ', fin_use_fsat
!#py        write(iulog,*)'usefrootc = ', usefrootc
!#py        write(iulog,*)'ch4offline = ', ch4offline
!#py 
!#py        if (ch4offline) write(iulog,*)'CH4 Model will be running offline and not affect fluxes to atmosphere'
!#py 
!#py        write(iulog,*)'use_aereoxid_prog = ', use_aereoxid_prog
!#py        if ( .not. use_aereoxid_prog ) then
!#py           write(iulog,*) 'Aerenchyma oxidation (aereoxid) value is being read from '//&
!#py             ' the parameters file'
!#py        endif
!#py 
!#py        if (.not. allowlakeprod) write(iulog,*) 'Lake production has been disabled. '// &
!#py           '  Lakes will not factor into CH4 BGC.  "Sat" history fields will not average'// &
!#py           '  over lakes except for concentrations, which will average zero from lakes.'
!#py        if (.not. replenishlakec .and. .not. ch4offline) write(iulog,*)'LAKE SOIL CARBON WILL NOT BE REPLENISHED BUT INSTEAD ',&
!#py              'WILL BE TRANSIENTLY RELEASED: COUPLED MODEL WILL NOT CONSERVE CARBON IN THIS MODE!'
!#py        write(iulog,*)'Successfully initialized CH4 parameters from namelist.'
!#py        write(iulog,*)
!#py 
!#py     end if
!#py 
!#py     !$acc update device(&
!#py     !$acc allowlakeprod    &
!#py     !$acc ,replenishlakec  &
!#py     !$acc ,fin_use_fsat    &
!#py     !$acc ,ch4offline      &
!#py     !$acc ,ch4rmcnlim      &
!#py     !$acc ,anoxicmicrosites&
!#py     !$acc ,ch4frzout       &
!#py     !$acc ,usefrootc       &
!#py     !$acc ,transpirationloss &
!#py     !$acc ,use_aereoxid_prog &
!#py     !$acc ,usephfact        )
!#py 
!#py   end subroutine CH4conrd

end module CH4varcon
