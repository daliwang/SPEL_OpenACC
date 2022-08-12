module readBoundsMod

  use filterMod               , only : filter
  use decompMod               , only : clumps,  procinfo
  use fileio_mod, only : fio_open, fio_read, fio_close

  public
contains

  subroutine readClumpsandFilter(in_file)

    implicit none
    character(len=256),intent(in) :: in_file
    integer :: errcode

    call fio_open(18,in_file, 1)

    !====================== filter ===========================!

    call fio_read(18,'filter(1)%natvegp', filter(1)%natvegp, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'filter(1)%num_natvegp', filter(1)%num_natvegp, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'filter(1)%pcropp', filter(1)%pcropp, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'filter(1)%num_pcropp', filter(1)%num_pcropp, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'filter(1)%soilnopcropp', filter(1)%soilnopcropp, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'filter(1)%num_soilnopcropp', filter(1)%num_soilnopcropp, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'filter(1)%lakep', filter(1)%lakep, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'filter(1)%num_lakep', filter(1)%num_lakep, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'filter(1)%nolakep', filter(1)%nolakep, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'filter(1)%num_nolakep', filter(1)%num_nolakep, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'filter(1)%lakec', filter(1)%lakec, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'filter(1)%num_lakec', filter(1)%num_lakec, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'filter(1)%nolakec', filter(1)%nolakec, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'filter(1)%num_nolakec', filter(1)%num_nolakec, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'filter(1)%soilc', filter(1)%soilc, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'filter(1)%num_soilc', filter(1)%num_soilc, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'filter(1)%soilp', filter(1)%soilp, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'filter(1)%num_soilp', filter(1)%num_soilp, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'filter(1)%snowc', filter(1)%snowc, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'filter(1)%num_snowc', filter(1)%num_snowc, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'filter(1)%nosnowc', filter(1)%nosnowc, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'filter(1)%num_nosnowc', filter(1)%num_nosnowc, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'filter(1)%lakesnowc', filter(1)%lakesnowc, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'filter(1)%num_lakesnowc', filter(1)%num_lakesnowc, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'filter(1)%lakenosnowc', filter(1)%lakenosnowc, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'filter(1)%num_lakenosnowc', filter(1)%num_lakenosnowc, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'filter(1)%hydrologyc', filter(1)%hydrologyc, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'filter(1)%num_hydrologyc', filter(1)%num_hydrologyc, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'filter(1)%hydrononsoic', filter(1)%hydrononsoic, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'filter(1)%num_hydrononsoic', filter(1)%num_hydrononsoic, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'filter(1)%urbanl', filter(1)%urbanl, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'filter(1)%num_urbanl', filter(1)%num_urbanl, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'filter(1)%nourbanl', filter(1)%nourbanl, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'filter(1)%num_nourbanl', filter(1)%num_nourbanl, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'filter(1)%urbanc', filter(1)%urbanc, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'filter(1)%num_urbanc', filter(1)%num_urbanc, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'filter(1)%nourbanc', filter(1)%nourbanc, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'filter(1)%num_nourbanc', filter(1)%num_nourbanc, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'filter(1)%urbanp', filter(1)%urbanp, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'filter(1)%num_urbanp', filter(1)%num_urbanp, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'filter(1)%nourbanp', filter(1)%nourbanp, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'filter(1)%num_nourbanp', filter(1)%num_nourbanp, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'filter(1)%nolakeurbanp', filter(1)%nolakeurbanp, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'filter(1)%num_nolakeurbanp', filter(1)%num_nolakeurbanp, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'filter(1)%icemecc', filter(1)%icemecc, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'filter(1)%num_icemecc', filter(1)%num_icemecc, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'filter(1)%do_smb_c', filter(1)%do_smb_c, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'filter(1)%num_do_smb_c', filter(1)%num_do_smb_c, errcode=errcode)
    if (errcode .ne. 0) stop

    !====================== clumps(1) ======================!

    call fio_read(18,'clumps(1)%owner', clumps(1)%owner, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'clumps(1)%ncells', clumps(1)%ncells, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'clumps(1)%ntopounits', clumps(1)%ntopounits, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'clumps(1)%nlunits', clumps(1)%nlunits, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'clumps(1)%ncols', clumps(1)%ncols, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'clumps(1)%npfts', clumps(1)%npfts, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'clumps(1)%nCohorts', clumps(1)%nCohorts, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'clumps(1)%begg', clumps(1)%begg, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'clumps(1)%begt', clumps(1)%begt, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'clumps(1)%begl', clumps(1)%begl, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'clumps(1)%begc', clumps(1)%begc, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'clumps(1)%begp', clumps(1)%begp, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'clumps(1)%endg', clumps(1)%endg, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'clumps(1)%endt', clumps(1)%endt, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'clumps(1)%endl', clumps(1)%endl, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'clumps(1)%endc', clumps(1)%endc, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'clumps(1)%endp', clumps(1)%endp, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'clumps(1)%begCohort', clumps(1)%begCohort, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'clumps(1)%endCohort', clumps(1)%endCohort, errcode=errcode)
    if (errcode .ne. 0) stop

    !====================== procinfo ======================!

    call fio_read(18,'procinfo%nclumps', procinfo%nclumps, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'procinfo%cid', procinfo%cid, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'procinfo%ncells', procinfo%ncells, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'procinfo%ntopounits', procinfo%ntopounits, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'procinfo%nlunits', procinfo%nlunits, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'procinfo%ncols', procinfo%ncols, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'procinfo%npfts', procinfo%npfts, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'procinfo%nCohorts', procinfo%nCohorts, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'procinfo%begg', procinfo%begg, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'procinfo%begt', procinfo%begt, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'procinfo%begl', procinfo%begl, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'procinfo%begc', procinfo%begc, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'procinfo%begp', procinfo%begp, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'procinfo%begCohort', procinfo%begCohort, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'procinfo%ncells_ghost', procinfo%ncells_ghost, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'procinfo%ntopounits_ghost', procinfo%ntopounits_ghost, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'procinfo%nlunits_ghost', procinfo%nlunits_ghost, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'procinfo%ncols_ghost', procinfo%ncols_ghost, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'procinfo%npfts_ghost', procinfo%npfts_ghost, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'procinfo%nCohorts_ghost', procinfo%nCohorts_ghost, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'procinfo%begg_ghost', procinfo%begg_ghost, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'procinfo%begt_ghost', procinfo%begt_ghost, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'procinfo%begl_ghost', procinfo%begl_ghost, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'procinfo%begc_ghost', procinfo%begc_ghost, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'procinfo%begp_ghost', procinfo%begp_ghost, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'procinfo%begCohort_ghost', procinfo%begCohort_ghost, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'procinfo%ncells_all', procinfo%ncells_all, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'procinfo%ntopounits_all', procinfo%ntopounits_all, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'procinfo%nlunits_all', procinfo%nlunits_all, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'procinfo%ncols_all', procinfo%ncols_all, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'procinfo%npfts_all', procinfo%npfts_all, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'procinfo%nCohorts_all', procinfo%nCohorts_all, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'procinfo%begg_all', procinfo%begg_all, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'procinfo%begt_all', procinfo%begt_all, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'procinfo%begl_all', procinfo%begl_all, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'procinfo%begc_all', procinfo%begc_all, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'procinfo%begp_all', procinfo%begp_all, errcode=errcode)
    if (errcode .ne. 0) stop
    call fio_read(18,'procinfo%begCohort_all', procinfo%begCohort_all, errcode=errcode)
    if (errcode .ne. 0) stop

    call fio_close(18)

  end subroutine readClumpsandFilter

end module readBoundsMod
