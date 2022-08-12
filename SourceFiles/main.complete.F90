program main()

  use shr_kind_mod    , only : r8 => shr_kind_r8
  use update_accMod
  use elm_varctl
  use decompInitMod
  use filterMod
  use decompMod
  use verificationMod
  use cudafor
  use readConstants        , only : read_constants
  use landunit_varcon      , only : istice, istice_mec, istsoil
  use lakeCon              , only : LakeConInit
  use soilorder_varcon     , only : soilorder_conrd
  use timeInfoMod
  use pftvarcon
  use GridcellType
  use TopounitType
  use LandunitType
  use ColumnType
  use VegetationType
  use VegetationPropertiesType
  use elm_instMod
  !#USE_START
  use elm_varorb
  use GridcellDataType
  use TopounitDataType
  use LandunitDataType
  use ColumnDataType
  use VegetationDataType
  use dynPriorWeightsMod
  use SharedParamsMod
  use SoilLittDecompMod
  use DecompCascadeCNMod
  use DecompCascadeBGCMod
  use AllocationMod
  use NitrifDenitrifMod
  use CNDecompCascadeConType
  use GapMortalityMod
  use SoilLittVertTranspMod
  use UrbanParamsType
  use CH4Mod
  use PhotosynthesisMod
  use domainMod
  use DaylengthMod
  use atm2lndMod
  use CanopyHydrologyMod
  use SurfaceRadiationMod
  use UrbanRadiationMod
  use CanopyFluxesMod
  use CanopyTemperatureMod
  use BareGroundFluxesMod
  use UrbanFluxesMod
  use LakeFluxesMod
  use DUSTMod
  use LakeTemperatureMod
  use SoilTemperatureMod
  use SoilFluxesMod
  use HydrologyNoDrainageMod
  use AerosolMod
  use SnowSnicarMod
  use LakeHydrologyMod
  use EcosystemDynMod
  use SedYieldMod
  use AnnualUpdateMod
  use DryDepVelocity
  use Ch4Mod
  use dynInitColumnsMod
  use dynConsBiogeophysMod
  use dynConsBiogeochemMod
  use reweightMod
  use subgridWeightsMod
  use NitrogenDynamicsMod
  use CarbonStateUpdate1Mod
  use NitrogenStateUpdate1Mod
  use PhosphorusStateUpdate1Mod
  use FireMod
  use dynPriorWeightsMod
  use dynSubgridDriverMod
  use dynPatchStateUpdaterMod
  use dynColumnStateUpdaterMod
  use BalanceCheckMod
  use EcosystemBalanceCheckMod
  use SurfaceAlbedoMod
  use UrbanAlbedoMod
  use VerticalProfileMod
  use glc2lndMod
  use shr_orb_mod_elm
  !#USE_END

  !=======================================!
  implicit none
  type(bounds_type)  ::  bounds_clump
  integer :: beg=1,fin=10, p, nclumps, nc, step_count
  real*8 :: temp
  integer :: err
#if _CUDA
      integer(kind=cuda_count_kind) :: heapsize,free1,free2,total
      integer  :: istat, val
#endif
  character(len=50) :: clump_input_char,pproc_input_char
  integer :: clump_input,pproc_input, fc, c, l, fp,g,j
  logical :: found_thawlayer
  integer :: k_frz
  real(r8) :: declin, declinp1
  !========================== Initialize/Allocate variables =======================!
  !First, make sure the right number of inputs have been provided
  IF(COMMAND_ARGUMENT_COUNT() == 1) THEN
    WRITE(*,*)'ONE COMMAND-LINE ARGUMENT DETECTED, Defaulting to 1 site per clump'
    call get_command_argument(1,clump_input_char)
    READ(clump_input_char,*) clump_input
    pproc_input = 1 !1 site per clump

  ELSEIF(COMMAND_ARGUMENT_COUNT() == 2) THEN
     call get_command_argument(1,clump_input_char)
     call get_command_argument(2,pproc_input_char)
     READ(clump_input_char,*) clump_input
     READ(pproc_input_char,*) pproc_input
     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  END IF

  call elm_init(clump_input,pproc_input, dtime_mod, year_curr)
  declin = -0.4030289369547867
  step_count = 0
  nclumps = procinfo%nclumps
  print *, "number of clumps", nclumps
  print *, "step:", step_count
  if(step_count == 0 ) then
    print *, "transferring data to GPU"

     !$acc update device( &
     !$acc        spinup_state            &
     !$acc       , nyears_ad_carbon_only   &
     !$acc       , spinup_mortality_factor &
     !$acc       , carbon_only &
     !$acc       , carbonphosphorus_only &
     !$acc       , carbonnitrogen_only &
     !$acc       ,use_crop            &
     !$acc       ,use_snicar_frc      &
     !$acc       ,use_snicar_ad       &
     !$acc       ,use_vancouver       &
     !$acc       ,use_mexicocity      &
     !$acc       ,use_noio            &
     !$acc       ,use_var_soil_thick  &
     !$acc       ,NFIX_PTASE_plant &
     !$acc       ,tw_irr &
     !$acc       ,use_erosion &
     !$acc       ,ero_ccycle  &
     !$acc       ,anoxia &
     !$acc       , glc_do_dynglacier &
     !$acc       , all_active &
     !$acc       , co2_ppmv &
     !$acc       , const_climate_hist &
     !$acc     )
     !$acc update device(first_step, nlevgrnd, eccen, obliqr, lambm0, mvelpp )
     call update_acc_variables()
     !
     !$acc enter data copyin(filter(:))

     !#ACC_COPYIN

#if _CUDA
    istat = cudaDeviceGetLimit(heapsize, cudaLimitMallocHeapSize)
    print *, "SETTING Heap Limit from", heapsize
    heapsize = 10_8*1024_8*1024_8
    print *, "TO:",heapsize
    istat = cudaDeviceSetLimit(cudaLimitMallocHeapSize,heapsize)
    istat = cudaMemGetInfo(free1, total)
    print *, "Free1:",free1
#endif
    end if

    !$acc enter data copyin( doalb, declinp1, declin )
    !$acc serial default(present)
    doalb = .true.
    nstep_mod = 788412 - 1
    year_curr  = 91
    mon_curr = 1
    day_curr = 1
    secs_curr = 43200 - 3600
    !nextsw_cday_mod = 1.458333333333333
    !call increment_time_vars()
    declin = -0.4023686267583503
    declinp1 = -0.4023686267583503
    !$acc end serial



    #if _CUDA
      istat = cudaMemGetInfo(free1, total)
      print *, "free after kernel:",free1
    #endif


end Program main

subroutine elm_init(clump_input,pproc_input,dt, yr)

        use readMod                 , only : read_vars
        use subgridMod              , only : wt_lunit, urban_valid, wt_glc_mec
        use duplicateMod            , only : duplicate_clumps
        use readConstants           , only : read_constants
        use initializeParameters

        implicit none

        integer, intent(in)  :: clump_input, pproc_input
        real*8, intent(in) :: dt
        integer, intent(in) :: yr
        integer :: errc
        integer :: nc
        integer :: begp, endp, begc, endc, begg, endg, begl, endl, begt,endt
        integer :: i,j
        character(len=256) :: in_file_clumps = 'clump_filter.txt'
        character(len=256) :: in_file_vars = 'master-data/output_cnbal_patch_vars.txt'
        character(len=256) :: out_vars = 'test_o.txt'
        character(len=256) :: in_file_constants = "E3SM_constants.txt"
        type(bounds_type)  :: bounds_proc
        type(bounds_type)  :: bounds_clump
        integer :: number_of_sites
        integer :: ni, nj=1
        integer :: c, l, g, t
        integer, allocatable :: amask(:)
        real*8 , allocatable  :: icemask_grc(:)
        real(r8), allocatable :: h2osno_col(:)
        real(r8), allocatable :: snow_depth_col(:)

        clump_pproc = clump_input/pproc_input
        ni = clump_input

        !if(clump_pproc .ne. ni) stop "CLUMP_PPROC not equal to ni"

        print *, "initializing parameters"
        call elm_varpar_init()
        call read_constants(in_file_constants, mode = 0)

        call elm_varcon_init()

        call pftconrd()
        call soilorder_conrd()
        call LakeConInit()

        call init_params(bounds_proc)
        print *, "read_constants mode = 1"
        call read_constants(in_file_constants, mode = 1)

        print *,"nlevurb        ",nlevurb
        print *,"nlevlak        ",nlevlak
        print *,"nlevdecomp     ",nlevdecomp
        print *,"nlevdecomp_full",nlevdecomp_full
        allocate(amask(ni*nj)); amask(:) = 1
        print *, "calling decompInit_lnd"
        call decompInit_lnd(ni,nj,amask)

        ! Allocate surface grid dynamic memory (just gridcell bounds dependent)
        call get_proc_bounds(begg=begg, endg=endg)
        allocate (wt_lunit (begg:endg, max_lunit)); wt_lunit(:,:) = 0d0

        wt_lunit(:,1) = 0.9783325924552204d0
        wt_lunit(:,5) = 1.9999999999999997D-002
        wt_lunit(:,9) = 1.6674075447795688D-003

        allocate (urban_valid  (begg:endg));  urban_valid(:) = .true.

        !allocate (wt_nat_patch (begg:endg, natpft_lb:natpft_ub ))
        print *,"calling decompinit_clumps"
        call decompInit_clumps()

        ! No ghost cells
        procinfo%ncells_ghost    = 0
        !--------------------------------------------------------------------------------
        procinfo%ntopounits_ghost   = 0
        procinfo%nlunits_ghost   = 0
        procinfo%ncols_ghost     = 0
        procinfo%npfts_ghost     = 0
        procinfo%nCohorts_ghost  = 0

        procinfo%begg_ghost      = 0
        procinfo%begt_ghost      = 0
        procinfo%begl_ghost      = 0
        procinfo%begc_ghost      = 0
        procinfo%begp_ghost      = 0
        procinfo%begCohort_ghost = 0
        procinfo%endg_ghost      = 0
        procinfo%endt_ghost      = 0
        procinfo%endl_ghost      = 0
        procinfo%endc_ghost      = 0
        procinfo%endp_ghost      = 0
        procinfo%endCohort_ghost = 0

        ! All = local (as no ghost cells)
        procinfo%ncells_all      = procinfo%ncells
        procinfo%ntopounits_all  = procinfo%ntopounits
        procinfo%nlunits_all     = procinfo%nlunits
        procinfo%ncols_all       = procinfo%ncols
        procinfo%npfts_all       = procinfo%npfts
        procinfo%nCohorts_all    = procinfo%nCohorts

        procinfo%begg_all        = procinfo%begg
        procinfo%begt_all        = procinfo%begt
        procinfo%begl_all        = procinfo%begl
        procinfo%begc_all        = procinfo%begc
        procinfo%begp_all        = procinfo%begp
        procinfo%begCohort_all   = procinfo%begCohort
        procinfo%endg_all        = procinfo%endg
        procinfo%endt_all        = procinfo%endt
        procinfo%endl_all        = procinfo%endl
        procinfo%endc_all        = procinfo%endc
        procinfo%endp_all        = procinfo%endp
        procinfo%endCohort_all   = procinfo%endCohort

        call get_proc_bounds(bounds_proc)
        print *, "nclumps = ",procinfo%nclumps

        deallocate(wt_lunit,urban_valid)
        begp = bounds_proc%begp_all; endp = bounds_proc%endp_all;
        begc = bounds_proc%begc_all; endc = bounds_proc%endc_all;
        begg = bounds_proc%begg_all; endg = bounds_proc%endg_all;
        begl = bounds_proc%begl_all; endl = bounds_proc%endl_all;
        begt = bounds_proc%begt_all; endt = bounds_proc%endt_all;
        print *,"begp, endp",begp, endp
        print *,"begc, endc",begc, endc
        print *,"begg, endg",begg, endg
        print *,"begl, endl",begl, endl
        print *,"begt, endt",begt, endt
        allocate (h2osno_col(begc:endc)) ;    h2osno_col(:) = 0d0
        allocate (snow_depth_col(begc:endc)); snow_depth_col(:) = 0d0

        call set_namelist_vars()

        print *, "allocating PP derived types"
        call veg_pp%Init(begp, endp)
        call lun_pp%Init(begl, endl)
        call col_pp%Init(begc, endc)
        call grc_pp%Init(begg, endg)
        call top_pp%Init(begt, endt)

        call veg_vp%Init()

        call get_clump_bounds(1,bounds_clump)
        call init_proc_clump_info()

        call read_vars(in_file_vars, bounds_clump,mode=1)
        if(clump_pproc > 1) then
          print *, "Duplicating Clumps"
          call duplicate_clumps(mode = 0,num_sites=clump_input)
          call correct_physical_properties()
        end if
        call init_decomp_cascade_constants()


        !#VAR_INIT_START
        call cnstate_vars%Init(bounds_proc)
        call soilstate_vars%InitAllocate(bounds_proc)

        print *, "ALLOCATING Module derived types"
        call urbanparams_vars%Init(bounds_proc)
        call lun_es%Init(begl, endl)
        call grc_es%Init(begg, endg)
        call col_es%Init(begc, endc)
        call lun_ef%Init(begl, endl)

        call veg_ef%Init(begp, endp)
        call veg_es%Init(begp, endp)
        call veg_wf%Init(begp, endp)
        call veg_ws%Init(begp, endp)
        call col_wf%Init(begc, endc)
        call col_ef%Init(begc, endc)
        call grc_ef%Init(begg, endg)

        call lun_ws%Init(begl, endl)

        call col_ws%Init(bounds_proc%begc_all, bounds_proc%endc_all, &
             h2osno_col(begc:endc),                    &
             snow_depth_col(begc:endc),                &
             soilstate_vars%watsat_col(begc:endc, 1:))


        call top_as%Init(begt, endt)
        call top_af%Init(begt, endt)

        print *, " allocating carbon state derived types "
        if (use_cn .or. use_fates) then

           call grc_cs%Init(begg, endg, carbon_type='c12')
           call col_cs%Init(begc, endc, carbon_type='c12', ratio=1._r8)
           call veg_cs%Init(begp, endp, carbon_type='c12', ratio=1._r8)

           ! Note - always initialize the memory for the c13_carbonflux_vars and
           ! c14_carbonflux_vars data structure so that they can be used in
           ! associate statements (nag compiler complains otherwise)

           call grc_cf%Init(begg, endg, carbon_type='c12')
           call col_cf%Init(begc, endc, carbon_type='c12')
           call veg_cf%Init(begp, endp, carbon_type='c12')

           if (use_c13) then
             call c13_grc_cf%Init(begg, endg, carbon_type='c13')
             call c13_col_cf%Init(begc, endc, carbon_type='c13')
             call c13_veg_cf%Init(begp, endp, carbon_type='c13')
             call c13_grc_cs%Init(begg, endg, carbon_type='c13')
             call c13_col_cs%Init(begc, endc, carbon_type='c13', ratio=c13ratio, &
                    c12_carbonstate_vars=col_cs)
             call c13_veg_cs%Init(begp, endp, carbon_type='c13', ratio=c13ratio)
           end if

           if (use_c14) then
             call c14_grc_cf%Init(begg, endg, carbon_type='c14')
             call c14_col_cf%Init(begc, endc, carbon_type='c14')
             call c14_veg_cf%Init(begp, endp, carbon_type='c14')
             call c14_grc_cs%Init(begg, endg, carbon_type='c14')
             call c14_col_cs%Init(begc, endc, carbon_type='c14', ratio=c14ratio, &
                  c12_carbonstate_vars=col_cs)
             call c14_veg_cs%Init(begp, endp, carbon_type='c14', ratio=c14ratio)
           end if

        endif
        print *, "allocating Nitrogen State Types"
        if (use_cn) then
           call grc_ns%Init(begg, endg)
           call col_ns%Init(begc, endc, col_cs)
           call veg_ns%Init(begp, endp, veg_cs)

           call grc_nf%Init(begg, endg)
           call col_nf%Init(begc, endc)
           call veg_nf%Init(begp, endp)

           call grc_ps%Init(begg, endg)
           call col_ps%Init(begc, endc, col_cs)
           call veg_ps%Init(begp, endp, veg_cs)

           call grc_pf%Init(begg, endg)
           call col_pf%Init(begc, endc)
           call veg_pf%Init(begp, endp)
           call crop_vars%Init(bounds_proc)

        end if

        call grc_ws%Init(begg, endg)
        call grc_wf%Init(begg,endg, bounds_proc)
        call drydepvel_vars%InitAllocate(bounds_proc)
        call frictionvel_vars%InitAllocate(bounds_proc)
        call lakestate_vars%Init(bounds_proc)
        call atm2lnd_vars%InitAllocate    (bounds_proc)
        call lnd2atm_vars%InitAllocate( bounds_proc )
        call glc2lnd_vars%InitAllocate( bounds_proc )
        call photosyns_vars%InitAllocate  (bounds_proc)
        call canopystate_vars%InitAllocate(bounds_proc)
        call dust_vars%Init(bounds_proc)
        call aerosol_vars%InitAllocate    (bounds_proc)
        call ch4_vars%InitAllocate        (bounds_proc)
        call solarabs_vars%Init(bounds_proc)
        call surfalb_vars%Init(bounds_proc)
        call surfrad_vars%Init(bounds_proc)
        call energyflux_vars%init(bounds_proc, col_es%t_grnd(begc:endc))
        call soilhydrology_vars%InitAllocate(bounds_proc)
        !#VAR_INIT_STOP

        if (use_century_decomp) then
          #ifdef DECOMPCASCADEBGCMOD
          call init_decompcascade_bgc(bounds_proc, cnstate_vars, soilstate_vars)
          #endif
        else
          #ifdef DECOMPCASCADECNMOD
          call init_decompcascade_cn(bounds_proc, cnstate_vars)
          #endif
        end if

        if(associated(ldomain%nv)) then
               write(iulog,*) "already set nv"
        else
               print *, "setting nv in main"
               allocate(ldomain%nv      ); ldomain%nv = 2
        end if

        call domain_init(ldomain,.false.,ni,nj,begg,endg,'lndgrid')
        call domain_check(ldomain)
        !!read for first clumps only

        call get_clump_bounds(1,bounds_clump)
        print *, "READING IN REST OF VARIABLES"
        call read_vars(in_file_vars, bounds_clump, mode=0)

        if(clump_pproc > 1) then
          print *, "Duplicating Clumps"
          call duplicate_clumps(mode = 1,num_sites=clump_input)
          !call correct_physical_properties()
        end if

        print*, "Setting up Filters"
        call allocFilters()

        nclumps = procinfo%nclumps
        allocate(icemask_grc(nclumps)); icemask_grc(:) = 0d0

        do nc = 1, nclumps
          call get_clump_bounds(nc,bounds_clump)
          call setFilters(bounds_clump,icemask_grc)
        end do

        #ifdef DYNSUBGRID
          print *, "dynSubgrid _init:"
          call init_subgrid_weights_mod(bounds_proc)
          call dynSubgrid_init(bounds_proc, glc2lnd_vars, crop_vars)
        #endif

        deallocate(icemask_grc)
        deallocate (h2osno_col)
        deallocate (snow_depth_col)
        deallocate(amask)
        !deallocate(icemask_grc)

end subroutine elm_init

subroutine correct_physical_properties()
  use GridcellType  , only : grc_pp
  use VegetationType, only : veg_pp
  use ColumnType,     only : col_pp
  use LandunitType,   only : lun_pp
  use TopounitType,   only : top_pp
  use decompMod
  use landunit_varcon , only : max_lunit
  use elm_varcon , only : ispval
  implicit none

  integer :: nc, i,stride, j
  type(bounds_type)  :: bounds_clump
  integer :: delc = 17, delp = 33
  nclumps = procinfo%nclumps

  do nc = 1, nclumps
    grc_pp%gindex(nc) = nc
    grc_pp%topi(nc)   = nc
    grc_pp%topf(nc)   = nc
    grc_pp%ntopounits(nc) = 1

    lun_pp%gridcell((nc-1)*5+1:nc*5) = nc
    lun_pp%topounit((nc-1)*5+1:nc*5) = nc

    do j =1, max_lunit
      if(top_pp%landunit_indices(j,1) == ispval) cycle
      top_pp%landunit_indices(j,nc) = top_pp%landunit_indices(j,1) + 5*(nc-1)
    end do


  end do

  do nc = 0,nclumps-2
    lun_pp%coli((nc+1)*5+1) = lun_pp%coli(1) + delc*(nc+1)
    lun_pp%coli((nc+1)*5+2) = lun_pp%coli(2) + delc*(nc+1)
    lun_pp%coli((nc+1)*5+3) = lun_pp%coli(3) + delc*(nc+1)
    lun_pp%coli((nc+1)*5+4) = lun_pp%coli(4) + delc*(nc+1)
    lun_pp%coli((nc+1)*5+5) = lun_pp%coli(5) + delc*(nc+1)

    lun_pp%colf((nc+1)*5+1) = lun_pp%colf(1) + delc*(nc+1)
    lun_pp%colf((nc+1)*5+2) = lun_pp%colf(2) + delc*(nc+1)
    lun_pp%colf((nc+1)*5+3) = lun_pp%colf(3) + delc*(nc+1)
    lun_pp%colf((nc+1)*5+4) = lun_pp%colf(4) + delc*(nc+1)
    lun_pp%colf((nc+1)*5+5) = lun_pp%colf(5) + delc*(nc+1)

    lun_pp%pfti((nc+1)*5+1) = lun_pp%pfti(1) + delp*(nc+1)
    lun_pp%pfti((nc+1)*5+2) = lun_pp%pfti(2) + delp*(nc+1)
    lun_pp%pfti((nc+1)*5+3) = lun_pp%pfti(3) + delp*(nc+1)
    lun_pp%pfti((nc+1)*5+4) = lun_pp%pfti(4) + delp*(nc+1)
    lun_pp%pfti((nc+1)*5+5) = lun_pp%pfti(5) + delp*(nc+1)

    lun_pp%pftf((nc+1)*5+1) = lun_pp%pftf(1) + delp*(nc+1)
    lun_pp%pftf((nc+1)*5+2) = lun_pp%pftf(2) + delp*(nc+1)
    lun_pp%pftf((nc+1)*5+3) = lun_pp%pftf(3) + delp*(nc+1)
    lun_pp%pftf((nc+1)*5+4) = lun_pp%pftf(4) + delp*(nc+1)
    lun_pp%pftf((nc+1)*5+5) = lun_pp%pftf(5) + delp*(nc+1)

  end do

  do nc = 1,nclumps
    col_pp%gridcell((nc-1)*delc+1:nc*delc) = nc
    col_pp%topounit((nc-1)*delc+1:nc*delc) = nc

  end do

  do nc = 1,nclumps-1
    stride = nc*delc
    do i = 1,delc
      col_pp%landunit(stride+i) = col_pp%landunit(i) + 5*nc
      col_pp%pfti(stride+i) = col_pp%pfti(i) + delp*nc
      col_pp%pftf(stride+i) = col_pp%pftf(i) + delp*nc

    end do
  end do

  do nc = 1,nclumps
    veg_pp%gridcell((nc-1)*delp+1:nc*delp) = nc
    veg_pp%topounit((nc-1)*delp+1:nc*delp) = nc
  end do

  do nc = 1,nclumps-1
    stride = nc*delp
    do i = 1,delp
      veg_pp%landunit(stride+i) = veg_pp%landunit(i) + 5*nc
      veg_pp%column(stride+i)   = veg_pp%column(i) + delc*nc
    end do
  end do


end subroutine
