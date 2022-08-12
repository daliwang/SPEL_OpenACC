err = cudaMemGetInfo(free_before, total)

!$acc enter data copyin( &
!$acc canopystate_vars     , &
!$acc col_pp     , &
!$acc veg_vp     , &
!$acc lun_pp     , &
!$acc veg_wf     , &
!$acc veg_es     , &
!$acc col_ws     , &
!$acc frictionvel_vars     , &
!$acc veg_pp     , &
!$acc col_ef     , &
!$acc soilstate_vars     , &
!$acc top_as     , &
!$acc veg_ef     , &
!$acc col_es      &
!$acc   )

!$acc parallel loop independent gang &
!$acc private(nc,bounds_clump) present( &
!$acc canopystate_vars     , &
!$acc col_pp     , &
!$acc veg_vp     , &
!$acc lun_pp     , &
!$acc veg_wf     , &
!$acc veg_es     , &
!$acc col_ws     , &
!$acc frictionvel_vars     , &
!$acc veg_pp     , &
!$acc col_ef     , &
!$acc soilstate_vars     , &
!$acc top_as     , &
!$acc veg_ef     , &
!$acc col_es      &
!$acc   )
do nc = 1, nclumps
  call get_clump_bounds_gpu(nc, bounds_clump)

  call CanopyTemperature(bounds_clump,                                   &
              filter(nc)%num_nolakec, filter(nc)%nolakec,                       &
              filter(nc)%num_nolakep, filter(nc)%nolakep,                       &
              atm2lnd_vars, canopystate_vars, soilstate_vars, frictionvel_vars, &
              energyflux_vars)
end do

err = cudaMemGetInfo(free_after, total)

print *,"CanopyTemperature:",free_before - free_after, "bytes"

!$acc exit data delete( &
!$acc canopystate_vars     , &
!$acc col_pp     , &
!$acc veg_vp     , &
!$acc lun_pp     , &
!$acc veg_wf     , &
!$acc veg_es     , &
!$acc col_ws     , &
!$acc frictionvel_vars     , &
!$acc veg_pp     , &
!$acc col_ef     , &
!$acc soilstate_vars     , &
!$acc top_as     , &
!$acc veg_ef     , &
!$acc col_es      &
!$acc   )


!$acc enter data copyin( &
!$acc veg_ps     , &
!$acc veg_ws     , &
!$acc solarabs_vars     , &
!$acc ParamsShareInst     , &
!$acc veg_ns     , &
!$acc frictionvel_vars     , &
!$acc top_af     , &
!$acc ch4_vars     , &
!$acc soilstate_vars     , &
!$acc canopystate_vars     , &
!$acc col_pp     , &
!$acc veg_wf     , &
!$acc veg_ef     , &
!$acc grc_pp     , &
!$acc surfalb_vars     , &
!$acc veg_es     , &
!$acc photosyns_vars     , &
!$acc col_es     , &
!$acc lun_pp     , &
!$acc veg_pp     , &
!$acc col_ef     , &
!$acc veg_vp     , &
!$acc cnstate_vars     , &
!$acc top_as     , &
!$acc col_ws     , &
!$acc energyflux_vars      &
!$acc   )


!$acc parallel loop independent gang &
!$acc private(nc,bounds_clump) present(&
!$acc veg_ps     , &
!$acc veg_ws     , &
!$acc solarabs_vars     , &
!$acc ParamsShareInst     , &
!$acc veg_ns     , &
!$acc frictionvel_vars     , &
!$acc top_af     , &
!$acc ch4_vars     , &
!$acc soilstate_vars     , &
!$acc canopystate_vars     , &
!$acc col_pp     , &
!$acc veg_wf     , &
!$acc veg_ef     , &
!$acc grc_pp     , &
!$acc surfalb_vars     , &
!$acc veg_es     , &
!$acc photosyns_vars     , &
!$acc col_es     , &
!$acc lun_pp     , &
!$acc veg_pp     , &
!$acc col_ef     , &
!$acc veg_vp     , &
!$acc cnstate_vars     , &
!$acc top_as     , &
!$acc col_ws     , &
!$acc energyflux_vars      &
!$acc   )

do nc = 1, nclumps
  call get_clump_bounds_gpu(nc, bounds_clump)

  call CanopyFluxes(bounds_clump,                                                   &
           filter(nc)%num_nolakeurbanp, filter(nc)%nolakeurbanp,                        &
           atm2lnd_vars, canopystate_vars, cnstate_vars, energyflux_vars,               &
           frictionvel_vars, soilstate_vars, solarabs_vars, surfalb_vars,               &
           ch4_vars, photosyns_vars, dtime, year, mon, day, time)

end do

err = cudaMemGetInfo(free_after, total)

print *,"CanopyFluxes:",free_before - free_after, "bytes"

!$acc exit data delete(&
!$acc veg_ps     , &
!$acc veg_ws     , &
!$acc solarabs_vars     , &
!$acc ParamsShareInst     , &
!$acc veg_ns     , &
!$acc frictionvel_vars     , &
!$acc top_af     , &
!$acc ch4_vars     , &
!$acc soilstate_vars     , &
!$acc canopystate_vars     , &
!$acc col_pp     , &
!$acc veg_wf     , &
!$acc veg_ef     , &
!$acc grc_pp     , &
!$acc surfalb_vars     , &
!$acc veg_es     , &
!$acc photosyns_vars     , &
!$acc col_es     , &
!$acc lun_pp     , &
!$acc veg_pp     , &
!$acc col_ef     , &
!$acc veg_vp     , &
!$acc cnstate_vars     , &
!$acc top_as     , &
!$acc col_ws     , &
!$acc energyflux_vars      &
!$acc   )



err = cudaMemGetInfo(free_before, total)
!$acc enter data copyin(&
!$acc soilstate_vars     , &
!$acc col_es     , &
!$acc col_ef     , &
!$acc top_af     , &
!$acc veg_pp     , &
!$acc solarabs_vars     , &
!$acc lun_pp     , &
!$acc col_ws     , &
!$acc lun_es     , &
!$acc col_wf     , &
!$acc veg_ef     , &
!$acc lun_ef     , &
!$acc urbanparams_vars     , &
!$acc col_pp     , &
!$acc veg_wf     , &
!$acc canopystate_vars      &
!$acc   )

!$acc parallel loop independent gang &
!$acc private(nc,bounds_clump) present(&
!$acc soilstate_vars     , &
!$acc col_es     , &
!$acc col_ef     , &
!$acc top_af     , &
!$acc veg_pp     , &
!$acc solarabs_vars     , &
!$acc lun_pp     , &
!$acc col_ws     , &
!$acc lun_es     , &
!$acc col_wf     , &
!$acc veg_ef     , &
!$acc lun_ef     , &
!$acc urbanparams_vars     , &
!$acc col_pp     , &
!$acc veg_wf     , &
!$acc canopystate_vars      &
!$acc   )
do nc = 1, nclumps
  call get_clump_bounds_gpu(nc, bounds_clump)

  call SoilTemperature(bounds_clump,                            &
              filter(nc)%num_urbanl  , filter(nc)%urbanl,       &
              filter(nc)%num_nolakec , filter(nc)%nolakec,      &
              atm2lnd_vars, urbanparams_vars, canopystate_vars, &
              solarabs_vars, soilstate_vars, energyflux_vars, dtime)

end do

err = cudaMemGetInfo(free_after, total)

print *,"SoilTemperature:",free_before - free_after, "bytes"

!$acc exit data delete(&
!$acc soilstate_vars     , &
!$acc col_es     , &
!$acc col_ef     , &
!$acc top_af     , &
!$acc veg_pp     , &
!$acc solarabs_vars     , &
!$acc lun_pp     , &
!$acc col_ws     , &
!$acc lun_es     , &
!$acc col_wf     , &
!$acc veg_ef     , &
!$acc lun_ef     , &
!$acc urbanparams_vars     , &
!$acc col_pp     , &
!$acc veg_wf     , &
!$acc canopystate_vars      &
!$acc   )

err = cudaMemGetInfo(free_before, total)
!$acc enter data copyin( &
!$acc atm2lnd_vars     , &
!$acc veg_pp     , &
!$acc crop_vars     , &
!$acc veg_pf     , &
!$acc col_cf     , &
!$acc c14_veg_cf     , &
!$acc col_wf     , &
!$acc decomp_cascade_con     , &
!$acc col_ws     , &
!$acc canopystate_vars     , &
!$acc col_ps     , &
!$acc ch4_vars     , &
!$acc veg_cs     , &
!$acc col_nf     , &
!$acc DecompBGCParamsInst     , &
!$acc veg_es     , &
!$acc DecompCNParamsInst     , &
!$acc col_pp     , &
!$acc ParamsShareInst     , &
!$acc col_ns     , &
!$acc photosyns_vars     , &
!$acc veg_ns     , &
!$acc col_es     , &
!$acc col_pf     , &
!$acc veg_nf     , &
!$acc cnstate_vars     , &
!$acc veg_cf     , &
!$acc veg_ps     , &
!$acc soilstate_vars     , &
!$acc veg_vp     , &
!$acc c13_veg_cf      &
!$acc   )


!$acc parallel loop independent gang &
!$acc private(nc,bounds_clump) present( &
!$acc atm2lnd_vars     , &
!$acc veg_pp     , &
!$acc crop_vars     , &
!$acc veg_pf     , &
!$acc col_cf     , &
!$acc c14_veg_cf     , &
!$acc col_wf     , &
!$acc decomp_cascade_con     , &
!$acc col_ws     , &
!$acc canopystate_vars     , &
!$acc col_ps     , &
!$acc ch4_vars     , &
!$acc veg_cs     , &
!$acc col_nf     , &
!$acc DecompBGCParamsInst     , &
!$acc veg_es     , &
!$acc DecompCNParamsInst     , &
!$acc col_pp     , &
!$acc ParamsShareInst     , &
!$acc col_ns     , &
!$acc photosyns_vars     , &
!$acc veg_ns     , &
!$acc col_es     , &
!$acc col_pf     , &
!$acc veg_nf     , &
!$acc cnstate_vars     , &
!$acc veg_cf     , &
!$acc veg_ps     , &
!$acc soilstate_vars     , &
!$acc veg_vp     , &
!$acc c13_veg_cf      &
!$acc   )

do nc = 1, nclumps
  call get_clump_bounds_gpu(nc, bounds_clump)

  call EcosystemDynNoLeaching1(bounds_clump,                            &
                         filter(nc)%num_soilc, filter(nc)%soilc,        &
                         filter(nc)%num_soilp, filter(nc)%soilp,        &
                         cnstate_vars, atm2lnd_vars,                    &
                         canopystate_vars, soilstate_vars, crop_vars,   &
                         ch4_vars, photosyns_vars,                      &
                         dtime, dayspyr,year, mon, day, sec)

end do

err = cudaMemGetInfo(free_after, total)

print *,"EcosystemDynNoLeaching1:",free_before - free_after, "bytes"

!$acc exit data delete( &
!$acc atm2lnd_vars     , &
!$acc veg_pp     , &
!$acc crop_vars     , &
!$acc veg_pf     , &
!$acc col_cf     , &
!$acc c14_veg_cf     , &
!$acc col_wf     , &
!$acc decomp_cascade_con     , &
!$acc col_ws     , &
!$acc canopystate_vars     , &
!$acc col_ps     , &
!$acc ch4_vars     , &
!$acc veg_cs     , &
!$acc col_nf     , &
!$acc DecompBGCParamsInst     , &
!$acc veg_es     , &
!$acc DecompCNParamsInst     , &
!$acc col_pp     , &
!$acc ParamsShareInst     , &
!$acc col_ns     , &
!$acc photosyns_vars     , &
!$acc veg_ns     , &
!$acc col_es     , &
!$acc col_pf     , &
!$acc veg_nf     , &
!$acc cnstate_vars     , &
!$acc veg_cf     , &
!$acc veg_ps     , &
!$acc soilstate_vars     , &
!$acc veg_vp     , &
!$acc c13_veg_cf      &
!$acc   )

err = cudaMemGetInfo(free_before, total)
!$acc enter data copyin(  &
!$acc AllocParamsInst     , &
!$acc atm2lnd_vars     , &
!$acc CNDecompParamsInst     , &
!$acc CNGapMortParamsInst     , &
!$acc grc_pp     , &
!$acc crop_vars     , &
!$acc canopystate_vars     , &
!$acc c14_veg_cf     , &
!$acc col_cf     , &
!$acc col_ws     , &
!$acc veg_ns     , &
!$acc veg_es     , &
!$acc col_ps     , &
!$acc veg_pf     , &
!$acc c13_col_cs     , &
!$acc top_af     , &
!$acc col_es     , &
!$acc c13_col_cf     , &
!$acc veg_cs     , &
!$acc col_cs     , &
!$acc ParamsShareInst     , &
!$acc col_pf     , &
!$acc veg_pp     , &
!$acc col_ns     , &
!$acc top_as     , &
!$acc col_nf     , &
!$acc soilhydrology_vars     , &
!$acc c14_veg_cs     , &
!$acc soilstate_vars     , &
!$acc col_pp     , &
!$acc c13_veg_cf     , &
!$acc c14_col_cf     , &
!$acc c14_col_cs     , &
!$acc veg_ps     , &
!$acc energyflux_vars     , &
!$acc veg_nf     , &
!$acc NitrifDenitrifParamsInst     , &
!$acc decomp_cascade_con     , &
!$acc ch4_vars     , &
!$acc veg_cf     , &
!$acc veg_vp     , &
!$acc cnstate_vars      &
!$acc   )


!$acc parallel loop independent gang &
!$acc private(nc,bounds_clump) present(  &
!$acc AllocParamsInst     , &
!$acc atm2lnd_vars     , &
!$acc CNDecompParamsInst     , &
!$acc CNGapMortParamsInst     , &
!$acc grc_pp     , &
!$acc crop_vars     , &
!$acc canopystate_vars     , &
!$acc c14_veg_cf     , &
!$acc col_cf     , &
!$acc col_ws     , &
!$acc veg_ns     , &
!$acc veg_es     , &
!$acc col_ps     , &
!$acc veg_pf     , &
!$acc c13_col_cs     , &
!$acc top_af     , &
!$acc col_es     , &
!$acc c13_col_cf     , &
!$acc veg_cs     , &
!$acc col_cs     , &
!$acc ParamsShareInst     , &
!$acc col_pf     , &
!$acc veg_pp     , &
!$acc col_ns     , &
!$acc top_as     , &
!$acc col_nf     , &
!$acc soilhydrology_vars     , &
!$acc c14_veg_cs     , &
!$acc soilstate_vars     , &
!$acc col_pp     , &
!$acc c13_veg_cf     , &
!$acc c14_col_cf     , &
!$acc c14_col_cs     , &
!$acc veg_ps     , &
!$acc energyflux_vars     , &
!$acc veg_nf     , &
!$acc NitrifDenitrifParamsInst     , &
!$acc decomp_cascade_con     , &
!$acc ch4_vars     , &
!$acc veg_cf     , &
!$acc veg_vp     , &
!$acc cnstate_vars      &
!$acc   )


do nc = 1, nclumps
  call get_clump_bounds_gpu(nc, bounds_clump)

  call EcosystemDynNoLeaching2(bounds_clump,                                   &
                      filter(nc)%num_soilc, filter(nc)%soilc,                  &
                      filter(nc)%num_soilp, filter(nc)%soilp,                  &
                      filter(nc)%num_pcropp, filter(nc)%pcropp, doalb,         &
                      cnstate_vars, atm2lnd_vars,canopystate_vars,&
                      soilstate_vars, crop_vars, ch4_vars, &
                      photosyns_vars, soilhydrology_vars, energyflux_vars, &
                      year, mon, day, sec, tod, offset, dayspyr, dtime)

end do

err = cudaMemGetInfo(free_after, total)

print *,"EcosystemDynNoLeaching2:",free_before - free_after, "bytes"

!$acc exit data delete( &
!$acc AllocParamsInst     , &
!$acc atm2lnd_vars     , &
!$acc CNDecompParamsInst     , &
!$acc CNGapMortParamsInst     , &
!$acc grc_pp     , &
!$acc crop_vars     , &
!$acc canopystate_vars     , &
!$acc c14_veg_cf     , &
!$acc col_cf     , &
!$acc col_ws     , &
!$acc veg_ns     , &
!$acc veg_es     , &
!$acc col_ps     , &
!$acc veg_pf     , &
!$acc c13_col_cs     , &
!$acc top_af     , &
!$acc col_es     , &
!$acc c13_col_cf     , &
!$acc veg_cs     , &
!$acc col_cs     , &
!$acc ParamsShareInst     , &
!$acc col_pf     , &
!$acc veg_pp     , &
!$acc col_ns     , &
!$acc top_as     , &
!$acc col_nf     , &
!$acc soilhydrology_vars     , &
!$acc c14_veg_cs     , &
!$acc soilstate_vars     , &
!$acc col_pp     , &
!$acc c13_veg_cf     , &
!$acc c14_col_cf     , &
!$acc c14_col_cs     , &
!$acc veg_ps     , &
!$acc energyflux_vars     , &
!$acc veg_nf     , &
!$acc NitrifDenitrifParamsInst     , &
!$acc decomp_cascade_con     , &
!$acc ch4_vars     , &
!$acc veg_cf     , &
!$acc veg_vp     , &
!$acc cnstate_vars      &
!$acc   )





!++++++++++++++++++++++++++++++++ VARIABLES ++++++++++++++++++++++++++++!!!
err = cudaMemGetInfo(free_before, total)
!$acc enter data copyin(col_ps )
err = cudaMemGetInfo(free_after, total)

print *,"col_ps",(free_before - free_after)/1000000.0


err = cudaMemGetInfo(free_before, total)
!$acc  enter data copyin(veg_cf)
err = cudaMemGetInfo(free_after, total)

print *,"veg_cf",(free_before - free_after)/1000000.0

err = cudaMemGetInfo(free_before, total)
!$acc  enter data copyin(veg_nf)
err = cudaMemGetInfo(free_after, total)

print *,"veg_nf",(free_before - free_after)/1000000.0

err = cudaMemGetInfo(free_before, total)
!$acc  enter data copyin(DecompBGCParamsInst)
err = cudaMemGetInfo(free_after, total)

print *,"DecompBGCParamsInst",(free_before - free_after)/1000000.0


err = cudaMemGetInfo(free_before, total)
!$acc  enter data copyin(ch4_vars)
err = cudaMemGetInfo(free_after, total)

print *,"ch4_vars",(free_before - free_after)/1000000.0

err = cudaMemGetInfo(free_before, total)
!$acc  enter data copyin(veg_pp)
err = cudaMemGetInfo(free_after, total)

print *,"veg_pp",(free_before - free_after)/1000000.0

err = cudaMemGetInfo(free_before, total)
!$acc  enter data copyin(lnd2atm_vars)
err = cudaMemGetInfo(free_after, total)

print *,"lnd2atm_vars",(free_before - free_after)/1000000.0


err = cudaMemGetInfo(free_before, total)
!$acc  enter data copyin(col_ns)
err = cudaMemGetInfo(free_after, total)

print *,"col_ns",(free_before - free_after)/1000000.0

err = cudaMemGetInfo(free_before, total)
!$acc enter data copyin(canopystate_vars)
err = cudaMemGetInfo(free_after, total)

print *,"canopystate_vars",(free_before - free_after)/1000000.0

err = cudaMemGetInfo(free_before, total)
!$acc  enter data copyin(veg_es)
err = cudaMemGetInfo(free_after, total)

print *,"veg_es",(free_before - free_after)/1000000.0


err = cudaMemGetInfo(free_before, total)
!$acc  enter data copyin(solarabs_vars)
err = cudaMemGetInfo(free_after, total)

print *,"solarabs_vars",(free_before - free_after)/1000000.0

err = cudaMemGetInfo(free_before, total)
!$acc  enter data copyin(col_es)
err = cudaMemGetInfo(free_after, total)

print *,"col_es",(free_before - free_after)/1000000.0


err = cudaMemGetInfo(free_before, total)
!$acc  enter data copyin(lun_ef)
err = cudaMemGetInfo(free_after, total)

print *,"lun_ef",(free_before - free_after)/1000000.0

err = cudaMemGetInfo(free_before, total)
!$acc  enter data copyin(veg_pf)
err = cudaMemGetInfo(free_after, total)

print *,"veg_pf",(free_before - free_after)/1000000.0


err = cudaMemGetInfo(free_before, total)
!$acc  enter data copyin(surfrad_vars)
err = cudaMemGetInfo(free_after, total)

print *,"surfrad_vars",(free_before - free_after)/1000000.0

err = cudaMemGetInfo(free_before, total)
!$acc  enter data copyin(grc_ef)
err = cudaMemGetInfo(free_after, total)

print *,"grc_ef",(free_before - free_after)/1000000.0

err = cudaMemGetInfo(free_before, total)
!$acc  enter data copyin(grc_pp)
err = cudaMemGetInfo(free_after, total)

print *,"grc_pp",(free_before - free_after)/1000000.0

err = cudaMemGetInfo(free_before, total)
!$acc  enter data copyin(veg_cs)
err = cudaMemGetInfo(free_after, total)

print *,"veg_cs",(free_before - free_after)/1000000.0


err = cudaMemGetInfo(free_before, total)
!$acc  enter data copyin(col_ws)
err = cudaMemGetInfo(free_after, total)

print *,"col_ws",(free_before - free_after)/1000000.0

err = cudaMemGetInfo(free_before, total)
!$acc  enter data copyin(lakestate_vars)
err = cudaMemGetInfo(free_after, total)

print *,"lakestate_vars",(free_before - free_after)/1000000.0

err = cudaMemGetInfo(free_before, total)
!$acc  enter data copyin(col_nf)
err = cudaMemGetInfo(free_after, total)

print *,"col_nf",(free_before - free_after)/1000000.0

err = cudaMemGetInfo(free_before, total)
!$acc  enter data copyin(soilstate_vars)
err = cudaMemGetInfo(free_after, total)

print *,"soilstate_vars",(free_before - free_after)/1000000.0

err = cudaMemGetInfo(free_before, total)
!$acc  enter data copyin(veg_vp)
err = cudaMemGetInfo(free_after, total)

print *,"veg_vp",(free_before - free_after)/1000000.0

err = cudaMemGetInfo(free_before, total)
!$acc  enter data copyin(col_wf)
err = cudaMemGetInfo(free_after, total)

print *,"col_wf",(free_before - free_after)/1000000.0

err = cudaMemGetInfo(free_before, total)
!$acc  enter data copyin(CNGapMortParamsInst)
err = cudaMemGetInfo(free_after, total)

print *,"CNGapMortParamsInst",(free_before - free_after)/1000000.0
err = cudaMemGetInfo(free_before, total)
!$acc  enter data copyin(lun_ws)
err = cudaMemGetInfo(free_after, total)

print *,"lun_ws",(free_before - free_after)/1000000.0

err = cudaMemGetInfo(free_before, total)
!$acc  enter data copyin(top_as)
err = cudaMemGetInfo(free_after, total)

print *,"top_as",(free_before - free_after)/1000000.0

err = cudaMemGetInfo(free_before, total)
!$acc  enter data copyin(aerosol_vars)
err = cudaMemGetInfo(free_after, total)

print *,"aersol_vars",(free_before - free_after)/1000000.0

err = cudaMemGetInfo(free_before, total)
!$acc  enter data copyin(veg_ps)
err = cudaMemGetInfo(free_after, total)

print *,"veg_ps",(free_before - free_after)/1000000.0

err = cudaMemGetInfo(free_before, total)
!$acc  enter data copyin(urbanparams_vars)
err = cudaMemGetInfo(free_after, total)

print *,"urbanparams_vars",(free_before - free_after)/1000000.0

err = cudaMemGetInfo(free_before, total)
!$acc  enter data copyin(lun_es)
err = cudaMemGetInfo(free_after, total)

print *,"lun_es",(free_before - free_after)/1000000.0

err = cudaMemGetInfo(free_before, total)
!$acc  enter data copyin(soilhydrology_vars)
err = cudaMemGetInfo(free_after, total)

print *,"soilhydrology_vars",(free_before - free_after)/1000000.0

err = cudaMemGetInfo(free_before, total)
!$acc  enter data copyin(NitrifDenitrifParamsInst)
err = cudaMemGetInfo(free_after, total)

print *,"NitrifDenitrifParamsInst",(free_before - free_after)/1000000.0


err = cudaMemGetInfo(free_before, total)
!$acc  enter data copyin(drydepvel_vars)
err = cudaMemGetInfo(free_after, total)

print *,"drydepvel_vars",(free_before - free_after)/1000000.0

err = cudaMemGetInfo(free_before, total)
!$acc  enter data copyin(grc_ws)
err = cudaMemGetInfo(free_after, total)

print *,"grc_ws",(free_before - free_after)/1000000.0

err = cudaMemGetInfo(free_before, total)
!$acc  enter data copyin(top_af)
err = cudaMemGetInfo(free_after, total)

print *,"top_af",(free_before - free_after)/1000000.0

err = cudaMemGetInfo(free_before, total)
!$acc  enter data copyin(col_pf)
err = cudaMemGetInfo(free_after, total)

print *,"col_pf",(free_before - free_after)/1000000.0

err = cudaMemGetInfo(free_before, total)
!$acc  enter data copyin(veg_ef)
err = cudaMemGetInfo(free_after, total)

print *,"veg_ef",(free_before - free_after)/1000000.0

err = cudaMemGetInfo(free_before, total)
!$acc  enter data copyin(frictionvel_vars)
err = cudaMemGetInfo(free_after, total)

print *,"frictionvel_vars",(free_before - free_after)/1000000.0

err = cudaMemGetInfo(free_before, total)
!$acc  enter data copyin(energyflux_vars)
err = cudaMemGetInfo(free_after, total)

print *,"energyflux_vars",(free_before - free_after)/1000000.0

err = cudaMemGetInfo(free_before, total)
!$acc  enter data copyin(col_pp)
err = cudaMemGetInfo(free_after, total)

print *,"col_pp",(free_before - free_after)/1000000.0

err = cudaMemGetInfo(free_before, total)
!$acc  enter data copyin(lun_pp)
err = cudaMemGetInfo(free_after, total)

print *,"lun_pp",(free_before - free_after)/1000000.0

err = cudaMemGetInfo(free_before, total)
!$acc  enter data copyin(cnstate_vars)
err = cudaMemGetInfo(free_after, total)

print *,"cnstate_vars",(free_before - free_after)/1000000.0

err = cudaMemGetInfo(free_before, total)
!$acc  enter data copyin(grc_wf)
err = cudaMemGetInfo(free_after, total)

print *,"grc_wf",(free_before - free_after)/1000000.0

err = cudaMemGetInfo(free_before, total)
!$acc  enter data copyin(dust_vars)
err = cudaMemGetInfo(free_after, total)

print *,"dust_vars",(free_before - free_after)/1000000.0

err = cudaMemGetInfo(free_before, total)
!$acc  enter data copyin(crop_vars)
err = cudaMemGetInfo(free_after, total)

print *,"crop_vars",(free_before - free_after)/1000000.0

err = cudaMemGetInfo(free_before, total)
!$acc  enter data copyin(veg_ns)
err = cudaMemGetInfo(free_after, total)

print *,"veg_ns",(free_before - free_after)/1000000.0
err = cudaMemGetInfo(free_before, total)
!$acc  enter data copyin(col_cs)
err = cudaMemGetInfo(free_after, total)

print *,"col_cs",(free_before - free_after)/1000000.0

err = cudaMemGetInfo(free_before, total)
!$acc  enter data copyin(veg_wf)
err = cudaMemGetInfo(free_after, total)

print *,"veg_wf",(free_before - free_after)/1000000.0

err = cudaMemGetInfo(free_before, total)
!$acc  enter data copyin(atm2lnd_vars)
err = cudaMemGetInfo(free_after, total)

print *,"atm2lnd_vars",(free_before - free_after)/1000000.0

err = cudaMemGetInfo(free_before, total)
!$acc  enter data copyin(surfalb_vars)
err = cudaMemGetInfo(free_after, total)

print *,"surfalb_vars",(free_before - free_after)/1000000.0

err = cudaMemGetInfo(free_before, total)
!$acc  enter data copyin(photosyns_vars)
err = cudaMemGetInfo(free_after, total)

print *,"photosyns_vars",(free_before - free_after)/1000000.0

err = cudaMemGetInfo(free_before, total)
!$acc  enter data copyin(col_ef)
err = cudaMemGetInfo(free_after, total)

print *,"col_ef",(free_before - free_after)/1000000.0

err = cudaMemGetInfo(free_before, total)
!$acc  enter data copyin(glc2lnd_vars)
err = cudaMemGetInfo(free_after, total)

print *,"glc2lnd_vars",(free_before - free_after)/1000000.0

err = cudaMemGetInfo(free_before, total)
!$acc  enter data copyin(veg_ws)
err = cudaMemGetInfo(free_after, total)

print *,"veg_ws",(free_before - free_after)/1000000.0

err = cudaMemGetInfo(free_before, total)
!$acc  enter data copyin(col_cf)
err = cudaMemGetInfo(free_after, total)

print *,"col_cf",(free_before - free_after)/1000000.0
