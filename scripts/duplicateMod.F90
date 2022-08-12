module duplicateMod
contains
subroutine duplicate_clumps(mode,num_sites)
     use elm_instMod, only : canopystate_vars 
     use GridcellType, only : grc_pp 
     use TopounitDataType, only : top_as 
     use LandunitType, only : lun_pp 
     use ColumnType, only : col_pp 
     use ColumnDataType, only : col_es 
     use ColumnDataType, only : col_ef 
     use ColumnDataType, only : col_ws 
     use ColumnDataType, only : col_wf 
     use VegetationType, only : veg_pp 
     use VegetationDataType, only : veg_wf 
     use elm_instMod, only : atm2lnd_vars 
     use elm_instMod, only : aerosol_vars 
     use elm_instMod, only : soilstate_vars 
     use TopounitType, only : top_pp 
     use elm_instMod, only : soilhydrology_vars 
     use decompMod, only : bounds_type, get_clump_bounds, procinfo 
     use elm_varcon 
     use elm_varpar 
     use elm_varctl 
     use landunit_varcon 
     implicit none 
     integer, intent(in) :: mode, num_sites 
     type(bounds_type)  :: bounds_clump1, bounds_clump 
     integer :: errcode, nc, nclumps 
     integer :: begp, endp,deltap 
     integer :: begc, endc,deltac 
     integer :: begg, endg,deltag 
     integer :: begt, endt,deltat 
     integer :: begl, endl,deltal 
     call get_clump_bounds(1,bounds_clump1) 
     begp =  1;  endp = 33; 
     begc =  1;  endc = 17; 
     begg =  1;  endg = 1 ; 
     begl =  1;  endt = 1 ; 
     begt =  1;  endl = 5 ; 
     nclumps = num_sites !procinfo%nclumps 
     deltap = endp - begp + 1 
     deltac = endc - begc + 1 
     deltag = endg - begg + 1 
     deltat = endt - begt + 1 
     deltal = endl - begl + 1 
     print *, 'begp, endp,deltap',begp, endp,deltap
     print *, 'begc, endc,deltac',begc, endc,deltac
     print *, 'begg, endg,deltag',begg, endg,deltag
     print *, 'begt, endt,deltat',begt, endt,deltat
     print *, 'begl, endl,deltal',begl, endl,deltal
     if(mode == 0) then
     do nc=1,nclumps-1 
          grc_pp%gindex(nc*deltag+1:(nc+1)*deltag) &
                    = grc_pp%gindex(begg:endg)
          grc_pp%area(nc*deltag+1:(nc+1)*deltag) &
                    = grc_pp%area(begg:endg)
          grc_pp%lat(nc*deltag+1:(nc+1)*deltag) &
                    = grc_pp%lat(begg:endg)
          grc_pp%lon(nc*deltag+1:(nc+1)*deltag) &
                    = grc_pp%lon(begg:endg)
          grc_pp%latdeg(nc*deltag+1:(nc+1)*deltag) &
                    = grc_pp%latdeg(begg:endg)
          grc_pp%londeg(nc*deltag+1:(nc+1)*deltag) &
                    = grc_pp%londeg(begg:endg)
          grc_pp%topi(nc*deltag+1:(nc+1)*deltag) &
                    = grc_pp%topi(begg:endg)
          grc_pp%topf(nc*deltag+1:(nc+1)*deltag) &
                    = grc_pp%topf(begg:endg)
          grc_pp%ntopounits(nc*deltag+1:(nc+1)*deltag) &
                    = grc_pp%ntopounits(begg:endg)
          grc_pp%lndi(nc*deltag+1:(nc+1)*deltag) &
                    = grc_pp%lndi(begg:endg)
          grc_pp%lndf(nc*deltag+1:(nc+1)*deltag) &
                    = grc_pp%lndf(begg:endg)
          grc_pp%nlandunits(nc*deltag+1:(nc+1)*deltag) &
                    = grc_pp%nlandunits(begg:endg)
          grc_pp%coli(nc*deltag+1:(nc+1)*deltag) &
                    = grc_pp%coli(begg:endg)
          grc_pp%colf(nc*deltag+1:(nc+1)*deltag) &
                    = grc_pp%colf(begg:endg)
          grc_pp%ncolumns(nc*deltag+1:(nc+1)*deltag) &
                    = grc_pp%ncolumns(begg:endg)
          grc_pp%pfti(nc*deltag+1:(nc+1)*deltag) &
                    = grc_pp%pfti(begg:endg)
          grc_pp%pftf(nc*deltag+1:(nc+1)*deltag) &
                    = grc_pp%pftf(begg:endg)
          grc_pp%npfts(nc*deltag+1:(nc+1)*deltag) &
                    = grc_pp%npfts(begg:endg)
          grc_pp%max_dayl(nc*deltag+1:(nc+1)*deltag) &
                    = grc_pp%max_dayl(begg:endg)
          grc_pp%dayl(nc*deltag+1:(nc+1)*deltag) &
                    = grc_pp%dayl(begg:endg)
          grc_pp%prev_dayl(nc*deltag+1:(nc+1)*deltag) &
                    = grc_pp%prev_dayl(begg:endg)
          grc_pp%landunit_indices(:,nc*deltag+1:(nc+1)*deltag) &
                    = grc_pp%landunit_indices(:, begg:endg)
          lun_pp%gridcell(nc*deltal+1:(nc+1)*deltal) &
                    = lun_pp%gridcell(begl:endl)
          lun_pp%wtgcell(nc*deltal+1:(nc+1)*deltal) &
                    = lun_pp%wtgcell(begl:endl)
          lun_pp%topounit(nc*deltal+1:(nc+1)*deltal) &
                    = lun_pp%topounit(begl:endl)
          lun_pp%wttopounit(nc*deltal+1:(nc+1)*deltal) &
                    = lun_pp%wttopounit(begl:endl)
          lun_pp%coli(nc*deltal+1:(nc+1)*deltal) &
                    = lun_pp%coli(begl:endl)
          lun_pp%colf(nc*deltal+1:(nc+1)*deltal) &
                    = lun_pp%colf(begl:endl)
          lun_pp%ncolumns(nc*deltal+1:(nc+1)*deltal) &
                    = lun_pp%ncolumns(begl:endl)
          lun_pp%pfti(nc*deltal+1:(nc+1)*deltal) &
                    = lun_pp%pfti(begl:endl)
          lun_pp%pftf(nc*deltal+1:(nc+1)*deltal) &
                    = lun_pp%pftf(begl:endl)
          lun_pp%npfts(nc*deltal+1:(nc+1)*deltal) &
                    = lun_pp%npfts(begl:endl)
          lun_pp%itype(nc*deltal+1:(nc+1)*deltal) &
                    = lun_pp%itype(begl:endl)
          lun_pp%ifspecial(nc*deltal+1:(nc+1)*deltal) &
                    = lun_pp%ifspecial(begl:endl)
          lun_pp%lakpoi(nc*deltal+1:(nc+1)*deltal) &
                    = lun_pp%lakpoi(begl:endl)
          lun_pp%urbpoi(nc*deltal+1:(nc+1)*deltal) &
                    = lun_pp%urbpoi(begl:endl)
          lun_pp%glcmecpoi(nc*deltal+1:(nc+1)*deltal) &
                    = lun_pp%glcmecpoi(begl:endl)
          lun_pp%active(nc*deltal+1:(nc+1)*deltal) &
                    = lun_pp%active(begl:endl)
          lun_pp%canyon_hwr(nc*deltal+1:(nc+1)*deltal) &
                    = lun_pp%canyon_hwr(begl:endl)
          lun_pp%wtroad_perv(nc*deltal+1:(nc+1)*deltal) &
                    = lun_pp%wtroad_perv(begl:endl)
          lun_pp%wtlunit_roof(nc*deltal+1:(nc+1)*deltal) &
                    = lun_pp%wtlunit_roof(begl:endl)
          lun_pp%ht_roof(nc*deltal+1:(nc+1)*deltal) &
                    = lun_pp%ht_roof(begl:endl)
          lun_pp%z_0_town(nc*deltal+1:(nc+1)*deltal) &
                    = lun_pp%z_0_town(begl:endl)
          lun_pp%z_d_town(nc*deltal+1:(nc+1)*deltal) &
                    = lun_pp%z_d_town(begl:endl)
          col_pp%gridcell(nc*deltac+1:(nc+1)*deltac) &
                    = col_pp%gridcell(begc:endc)
          col_pp%wtgcell(nc*deltac+1:(nc+1)*deltac) &
                    = col_pp%wtgcell(begc:endc)
          col_pp%topounit(nc*deltac+1:(nc+1)*deltac) &
                    = col_pp%topounit(begc:endc)
          col_pp%wttopounit(nc*deltac+1:(nc+1)*deltac) &
                    = col_pp%wttopounit(begc:endc)
          col_pp%landunit(nc*deltac+1:(nc+1)*deltac) &
                    = col_pp%landunit(begc:endc)
          col_pp%wtlunit(nc*deltac+1:(nc+1)*deltac) &
                    = col_pp%wtlunit(begc:endc)
          col_pp%pfti(nc*deltac+1:(nc+1)*deltac) &
                    = col_pp%pfti(begc:endc)
          col_pp%pftf(nc*deltac+1:(nc+1)*deltac) &
                    = col_pp%pftf(begc:endc)
          col_pp%npfts(nc*deltac+1:(nc+1)*deltac) &
                    = col_pp%npfts(begc:endc)
          col_pp%itype(nc*deltac+1:(nc+1)*deltac) &
                    = col_pp%itype(begc:endc)
          col_pp%active(nc*deltac+1:(nc+1)*deltac) &
                    = col_pp%active(begc:endc)
          col_pp%glc_topo(nc*deltac+1:(nc+1)*deltac) &
                    = col_pp%glc_topo(begc:endc)
          col_pp%micro_sigma(nc*deltac+1:(nc+1)*deltac) &
                    = col_pp%micro_sigma(begc:endc)
          col_pp%n_melt(nc*deltac+1:(nc+1)*deltac) &
                    = col_pp%n_melt(begc:endc)
          col_pp%topo_slope(nc*deltac+1:(nc+1)*deltac) &
                    = col_pp%topo_slope(begc:endc)
          col_pp%topo_std(nc*deltac+1:(nc+1)*deltac) &
                    = col_pp%topo_std(begc:endc)
          col_pp%hslp_p10(nc*deltac+1:(nc+1)*deltac,:) &
                    = col_pp%hslp_p10(begc:endc,:)
          col_pp%nlevbed(nc*deltac+1:(nc+1)*deltac) &
                    = col_pp%nlevbed(begc:endc)
          col_pp%zibed(nc*deltac+1:(nc+1)*deltac) &
                    = col_pp%zibed(begc:endc)
          col_pp%snl(nc*deltac+1:(nc+1)*deltac) &
                    = col_pp%snl(begc:endc)
          col_pp%dz(nc*deltac+1:(nc+1)*deltac,:) &
                    = col_pp%dz(begc:endc,:)
          col_pp%z(nc*deltac+1:(nc+1)*deltac,:) &
                    = col_pp%z(begc:endc,:)
          col_pp%zi(nc*deltac+1:(nc+1)*deltac,:) &
                    = col_pp%zi(begc:endc,:)
          col_pp%zii(nc*deltac+1:(nc+1)*deltac) &
                    = col_pp%zii(begc:endc)
          col_pp%dz_lake(nc*deltac+1:(nc+1)*deltac,:) &
                    = col_pp%dz_lake(begc:endc,:)
          col_pp%z_lake(nc*deltac+1:(nc+1)*deltac,:) &
                    = col_pp%z_lake(begc:endc,:)
          col_pp%lakedepth(nc*deltac+1:(nc+1)*deltac) &
                    = col_pp%lakedepth(begc:endc)
          col_pp%hydrologically_active(nc*deltac+1:(nc+1)*deltac) &
                    = col_pp%hydrologically_active(begc:endc)
          veg_pp%gridcell(nc*deltap+1:(nc+1)*deltap) &
                    = veg_pp%gridcell(begp:endp)
          veg_pp%wtgcell(nc*deltap+1:(nc+1)*deltap) &
                    = veg_pp%wtgcell(begp:endp)
          veg_pp%topounit(nc*deltap+1:(nc+1)*deltap) &
                    = veg_pp%topounit(begp:endp)
          veg_pp%wttopounit(nc*deltap+1:(nc+1)*deltap) &
                    = veg_pp%wttopounit(begp:endp)
          veg_pp%landunit(nc*deltap+1:(nc+1)*deltap) &
                    = veg_pp%landunit(begp:endp)
          veg_pp%wtlunit(nc*deltap+1:(nc+1)*deltap) &
                    = veg_pp%wtlunit(begp:endp)
          veg_pp%column(nc*deltap+1:(nc+1)*deltap) &
                    = veg_pp%column(begp:endp)
          veg_pp%wtcol(nc*deltap+1:(nc+1)*deltap) &
                    = veg_pp%wtcol(begp:endp)
          veg_pp%itype(nc*deltap+1:(nc+1)*deltap) &
                    = veg_pp%itype(begp:endp)
          veg_pp%mxy(nc*deltap+1:(nc+1)*deltap) &
                    = veg_pp%mxy(begp:endp)
          veg_pp%active(nc*deltap+1:(nc+1)*deltap) &
                    = veg_pp%active(begp:endp)
          veg_pp%is_veg(nc*deltap+1:(nc+1)*deltap) &
                    = veg_pp%is_veg(begp:endp)
          veg_pp%is_bareground(nc*deltap+1:(nc+1)*deltap) &
                    = veg_pp%is_bareground(begp:endp)
          veg_pp%wt_ed(nc*deltap+1:(nc+1)*deltap) &
                    = veg_pp%wt_ed(begp:endp)
          veg_pp%is_fates(nc*deltap+1:(nc+1)*deltap) &
                    = veg_pp%is_fates(begp:endp)
          top_pp%gridcell(nc*deltat+1:(nc+1)*deltat) &
                    = top_pp%gridcell(begt:endt)
          top_pp%topo_grc_ind(nc*deltat+1:(nc+1)*deltat) &
                    = top_pp%topo_grc_ind(begt:endt)
          top_pp%wtgcell(nc*deltat+1:(nc+1)*deltat) &
                    = top_pp%wtgcell(begt:endt)
          top_pp%lndi(nc*deltat+1:(nc+1)*deltat) &
                    = top_pp%lndi(begt:endt)
          top_pp%lndf(nc*deltat+1:(nc+1)*deltat) &
                    = top_pp%lndf(begt:endt)
          top_pp%nlandunits(nc*deltat+1:(nc+1)*deltat) &
                    = top_pp%nlandunits(begt:endt)
          top_pp%coli(nc*deltat+1:(nc+1)*deltat) &
                    = top_pp%coli(begt:endt)
          top_pp%colf(nc*deltat+1:(nc+1)*deltat) &
                    = top_pp%colf(begt:endt)
          top_pp%ncolumns(nc*deltat+1:(nc+1)*deltat) &
                    = top_pp%ncolumns(begt:endt)
          top_pp%pfti(nc*deltat+1:(nc+1)*deltat) &
                    = top_pp%pfti(begt:endt)
          top_pp%pftf(nc*deltat+1:(nc+1)*deltat) &
                    = top_pp%pftf(begt:endt)
          top_pp%npfts(nc*deltat+1:(nc+1)*deltat) &
                    = top_pp%npfts(begt:endt)
          top_pp%landunit_indices(:,nc*deltat+1:(nc+1)*deltat) &
                    = top_pp%landunit_indices(:, begt:endt)
          top_pp%active(nc*deltat+1:(nc+1)*deltat) &
                    = top_pp%active(begt:endt)
          top_pp%area(nc*deltat+1:(nc+1)*deltat) &
                    = top_pp%area(begt:endt)
          top_pp%lat(nc*deltat+1:(nc+1)*deltat) &
                    = top_pp%lat(begt:endt)
          top_pp%lon(nc*deltat+1:(nc+1)*deltat) &
                    = top_pp%lon(begt:endt)
          top_pp%elevation(nc*deltat+1:(nc+1)*deltat) &
                    = top_pp%elevation(begt:endt)
          top_pp%slope(nc*deltat+1:(nc+1)*deltat) &
                    = top_pp%slope(begt:endt)
          top_pp%aspect(nc*deltat+1:(nc+1)*deltat) &
                    = top_pp%aspect(begt:endt)
          top_pp%emissivity(nc*deltat+1:(nc+1)*deltat) &
                    = top_pp%emissivity(begt:endt)
          top_pp%surfalb_dir(nc*deltat+1:(nc+1)*deltat,:) &
                    = top_pp%surfalb_dir(begt:endt,:)
          top_pp%surfalb_dif(nc*deltat+1:(nc+1)*deltat,:) &
                    = top_pp%surfalb_dif(begt:endt,:)
     end do
     else
     do nc=1,nclumps-1 
          canopystate_vars%frac_veg_nosno_patch(nc*deltap+1:(nc+1)*deltap) &
                    = canopystate_vars%frac_veg_nosno_patch(begp:endp)
          canopystate_vars%vegwp_patch(nc*deltap+1:(nc+1)*deltap,:) &
                    = canopystate_vars%vegwp_patch(begp:endp,:)
          top_as%windbot(nc*deltat+1:(nc+1)*deltat) &
                    = top_as%windbot(begt:endt)
          col_es%t_soisno(nc*deltac+1:(nc+1)*deltac,:) &
                    = col_es%t_soisno(begc:endc,:)
          col_es%t_h2osfc(nc*deltac+1:(nc+1)*deltac) &
                    = col_es%t_h2osfc(begc:endc)
          col_ef%imelt(nc*deltac+1:(nc+1)*deltac,:) &
                    = col_ef%imelt(begc:endc,:)
          col_ws%h2osoi_liq(nc*deltac+1:(nc+1)*deltac,:) &
                    = col_ws%h2osoi_liq(begc:endc,:)
          col_ws%h2osoi_ice(nc*deltac+1:(nc+1)*deltac,:) &
                    = col_ws%h2osoi_ice(begc:endc,:)
          col_ws%h2osoi_vol(nc*deltac+1:(nc+1)*deltac,:) &
                    = col_ws%h2osoi_vol(begc:endc,:)
          col_ws%h2osfc(nc*deltac+1:(nc+1)*deltac) &
                    = col_ws%h2osfc(begc:endc)
          col_ws%swe_old(nc*deltac+1:(nc+1)*deltac,:) &
                    = col_ws%swe_old(begc:endc,:)
          col_ws%snw_rds(nc*deltac+1:(nc+1)*deltac,:) &
                    = col_ws%snw_rds(begc:endc,:)
          col_ws%h2osno(nc*deltac+1:(nc+1)*deltac) &
                    = col_ws%h2osno(begc:endc)
          col_ws%int_snow(nc*deltac+1:(nc+1)*deltac) &
                    = col_ws%int_snow(begc:endc)
          col_ws%snow_depth(nc*deltac+1:(nc+1)*deltac) &
                    = col_ws%snow_depth(begc:endc)
          col_ws%snow_persistence(nc*deltac+1:(nc+1)*deltac) &
                    = col_ws%snow_persistence(begc:endc)
          col_ws%do_capsnow(nc*deltac+1:(nc+1)*deltac) &
                    = col_ws%do_capsnow(begc:endc)
          col_ws%frac_sno(nc*deltac+1:(nc+1)*deltac) &
                    = col_ws%frac_sno(begc:endc)
          col_ws%frac_sno_eff(nc*deltac+1:(nc+1)*deltac) &
                    = col_ws%frac_sno_eff(begc:endc)
          col_ws%frac_iceold(nc*deltac+1:(nc+1)*deltac,:) &
                    = col_ws%frac_iceold(begc:endc,:)
          col_ws%frac_h2osfc(nc*deltac+1:(nc+1)*deltac) &
                    = col_ws%frac_h2osfc(begc:endc)
          col_wf%qflx_rain_grnd(nc*deltac+1:(nc+1)*deltac) &
                    = col_wf%qflx_rain_grnd(begc:endc)
          col_wf%qflx_sub_snow(nc*deltac+1:(nc+1)*deltac) &
                    = col_wf%qflx_sub_snow(begc:endc)
          col_wf%qflx_evap_grnd(nc*deltac+1:(nc+1)*deltac) &
                    = col_wf%qflx_evap_grnd(begc:endc)
          col_wf%qflx_tran_veg(nc*deltac+1:(nc+1)*deltac) &
                    = col_wf%qflx_tran_veg(begc:endc)
          col_wf%qflx_dew_snow(nc*deltac+1:(nc+1)*deltac) &
                    = col_wf%qflx_dew_snow(begc:endc)
          col_wf%qflx_dew_grnd(nc*deltac+1:(nc+1)*deltac) &
                    = col_wf%qflx_dew_grnd(begc:endc)
          col_wf%qflx_ev_soil(nc*deltac+1:(nc+1)*deltac) &
                    = col_wf%qflx_ev_soil(begc:endc)
          col_wf%qflx_ev_h2osfc(nc*deltac+1:(nc+1)*deltac) &
                    = col_wf%qflx_ev_h2osfc(begc:endc)
          col_wf%qflx_rootsoi(nc*deltac+1:(nc+1)*deltac,:) &
                    = col_wf%qflx_rootsoi(begc:endc,:)
          col_wf%qflx_infl(nc*deltac+1:(nc+1)*deltac) &
                    = col_wf%qflx_infl(begc:endc)
          col_wf%qflx_surf(nc*deltac+1:(nc+1)*deltac) &
                    = col_wf%qflx_surf(begc:endc)
          col_wf%qflx_top_soil(nc*deltac+1:(nc+1)*deltac) &
                    = col_wf%qflx_top_soil(begc:endc)
          col_wf%qflx_snow_h2osfc(nc*deltac+1:(nc+1)*deltac) &
                    = col_wf%qflx_snow_h2osfc(begc:endc)
          col_wf%qflx_floodc(nc*deltac+1:(nc+1)*deltac) &
                    = col_wf%qflx_floodc(begc:endc)
          col_wf%qflx_snomelt(nc*deltac+1:(nc+1)*deltac) &
                    = col_wf%qflx_snomelt(begc:endc)
          col_wf%qflx_snow_melt(nc*deltac+1:(nc+1)*deltac) &
                    = col_wf%qflx_snow_melt(begc:endc)
          atm2lnd_vars%forc_aer_grc(nc*deltag+1:(nc+1)*deltag,:) &
                    = atm2lnd_vars%forc_aer_grc(begg:endg,:)
          aerosol_vars%mss_bcpho_col(nc*deltac+1:(nc+1)*deltac,:) &
                    = aerosol_vars%mss_bcpho_col(begc:endc,:)
          aerosol_vars%mss_bcphi_col(nc*deltac+1:(nc+1)*deltac,:) &
                    = aerosol_vars%mss_bcphi_col(begc:endc,:)
          aerosol_vars%mss_ocpho_col(nc*deltac+1:(nc+1)*deltac,:) &
                    = aerosol_vars%mss_ocpho_col(begc:endc,:)
          aerosol_vars%mss_ocphi_col(nc*deltac+1:(nc+1)*deltac,:) &
                    = aerosol_vars%mss_ocphi_col(begc:endc,:)
          aerosol_vars%mss_dst1_col(nc*deltac+1:(nc+1)*deltac,:) &
                    = aerosol_vars%mss_dst1_col(begc:endc,:)
          aerosol_vars%mss_dst2_col(nc*deltac+1:(nc+1)*deltac,:) &
                    = aerosol_vars%mss_dst2_col(begc:endc,:)
          aerosol_vars%mss_dst3_col(nc*deltac+1:(nc+1)*deltac,:) &
                    = aerosol_vars%mss_dst3_col(begc:endc,:)
          aerosol_vars%mss_dst4_col(nc*deltac+1:(nc+1)*deltac,:) &
                    = aerosol_vars%mss_dst4_col(begc:endc,:)
          soilstate_vars%hksat_col(nc*deltac+1:(nc+1)*deltac,:) &
                    = soilstate_vars%hksat_col(begc:endc,:)
          soilstate_vars%smp_l_col(nc*deltac+1:(nc+1)*deltac,:) &
                    = soilstate_vars%smp_l_col(begc:endc,:)
          soilstate_vars%smpmin_col(nc*deltac+1:(nc+1)*deltac) &
                    = soilstate_vars%smpmin_col(begc:endc)
          soilstate_vars%bsw_col(nc*deltac+1:(nc+1)*deltac,:) &
                    = soilstate_vars%bsw_col(begc:endc,:)
          soilstate_vars%watsat_col(nc*deltac+1:(nc+1)*deltac,:) &
                    = soilstate_vars%watsat_col(begc:endc,:)
          soilstate_vars%sucsat_col(nc*deltac+1:(nc+1)*deltac,:) &
                    = soilstate_vars%sucsat_col(begc:endc,:)
          soilstate_vars%wtfact_col(nc*deltac+1:(nc+1)*deltac) &
                    = soilstate_vars%wtfact_col(begc:endc)
          soilstate_vars%rootr_patch(nc*deltap+1:(nc+1)*deltap,:) &
                    = soilstate_vars%rootr_patch(begp:endp,:)
          soilstate_vars%k_soil_root_patch(nc*deltap+1:(nc+1)*deltap,:) &
                    = soilstate_vars%k_soil_root_patch(begp:endp,:)
          soilhydrology_vars%frost_table_col(nc*deltac+1:(nc+1)*deltac) &
                    = soilhydrology_vars%frost_table_col(begc:endc)
          soilhydrology_vars%zwt_col(nc*deltac+1:(nc+1)*deltac) &
                    = soilhydrology_vars%zwt_col(begc:endc)
          soilhydrology_vars%zwt_perched_col(nc*deltac+1:(nc+1)*deltac) &
                    = soilhydrology_vars%zwt_perched_col(begc:endc)
          soilhydrology_vars%wa_col(nc*deltac+1:(nc+1)*deltac) &
                    = soilhydrology_vars%wa_col(begc:endc)
          soilhydrology_vars%qcharge_col(nc*deltac+1:(nc+1)*deltac) &
                    = soilhydrology_vars%qcharge_col(begc:endc)
          soilhydrology_vars%fracice_col(nc*deltac+1:(nc+1)*deltac,:) &
                    = soilhydrology_vars%fracice_col(begc:endc,:)
          soilhydrology_vars%fsat_col(nc*deltac+1:(nc+1)*deltac) &
                    = soilhydrology_vars%fsat_col(begc:endc)
          soilhydrology_vars%h2osfc_thresh_col(nc*deltac+1:(nc+1)*deltac) &
                    = soilhydrology_vars%h2osfc_thresh_col(begc:endc)
          soilhydrology_vars%b_infil_col(nc*deltac+1:(nc+1)*deltac) &
                    = soilhydrology_vars%b_infil_col(begc:endc)
          soilhydrology_vars%porosity_col(nc*deltac+1:(nc+1)*deltac,:) &
                    = soilhydrology_vars%porosity_col(begc:endc,:)
          soilhydrology_vars%vic_elm_fract_col(nc*deltac+1:(nc+1)*deltac,:,:) &
                    = soilhydrology_vars%vic_elm_fract_col(begc:endc,:,:)
          soilhydrology_vars%depth_col(nc*deltac+1:(nc+1)*deltac,:) &
                    = soilhydrology_vars%depth_col(begc:endc,:)
          soilhydrology_vars%moist_col(nc*deltac+1:(nc+1)*deltac,:) &
                    = soilhydrology_vars%moist_col(begc:endc,:)
          soilhydrology_vars%max_moist_col(nc*deltac+1:(nc+1)*deltac,:) &
                    = soilhydrology_vars%max_moist_col(begc:endc,:)
          soilhydrology_vars%max_infil_col(nc*deltac+1:(nc+1)*deltac) &
                    = soilhydrology_vars%max_infil_col(begc:endc)
          soilhydrology_vars%i_0_col(nc*deltac+1:(nc+1)*deltac) &
                    = soilhydrology_vars%i_0_col(begc:endc)
          soilhydrology_vars%ice_col(nc*deltac+1:(nc+1)*deltac,:) &
                    = soilhydrology_vars%ice_col(begc:endc,:)
     end do
     end if 
end subroutine duplicate_clumps 
end module 
