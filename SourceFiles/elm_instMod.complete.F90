module elm_instMod
  !-----------------------------------------------------------------------
  ! initialize elm data types
  !
  use shr_kind_mod               , only : r8 => shr_kind_r8
  use elm_varctl                 , only : use_cn, use_voc, use_c13, use_c14, use_fates, use_betr
  !-----------------------------------------
  ! Definition of component types
  !-----------------------------------------
  use AerosolType                , only : aerosol_type
  use CanopyStateType            , only : canopystate_type
  use CH4Mod                     , only : ch4_type
  use CNStateType                , only : cnstate_type
  use CropType                   , only : crop_type
  use DryDepVelocity             , only : drydepvel_type
  use DUSTMod                    , only : dust_type
  use EnergyFluxType             , only : energyflux_type
  use FrictionVelocityType       , only : frictionvel_type
  use LakeStateType              , only : lakestate_type
  use PhotosynthesisType         , only : photosyns_type
  use SedFluxType                , only : sedflux_type
  use SoilHydrologyType          , only : soilhydrology_type
  use SoilStateType              , only : soilstate_type
  use SolarAbsorbedType          , only : solarabs_type
  use SurfaceRadiationMod        , only : surfrad_type
  use SurfaceAlbedoType          , only : SurfaceAlbedoInitTimeConst
  use SurfaceAlbedoType          , only : surfalb_type
  use VOCEmissionMod             , only : vocemis_type
  use atm2lndType                , only : atm2lnd_type
  use lnd2atmType                , only : lnd2atm_type
  use lnd2glcMod                 , only : lnd2glc_type
  use glc2lndMod                 , only : glc2lnd_type
  use glcDiagnosticsMod          , only : glc_diagnostics_type
  use SoilWaterRetentionCurveMod , only : soil_water_retention_curve_type
  use VegetationPropertiesType   , only : veg_vp             ! Ecophysical Constants
  use SoilorderConType           , only : soilordercon       ! Constants
  use GridcellDataType           , only : grc_es, grc_ef, grc_ws, grc_wf
  use GridcellDataType           , only : grc_cs, c13_grc_cs, c14_grc_cs
  use GridcellDataType           , only : grc_cf, c13_grc_cf, c14_grc_cf
  use GridcellDataType           , only : grc_ns, grc_nf
  use GridcellDataType           , only : grc_ps, grc_pf
  use LandunitType               , only : lun_pp
  use LandunitDataType           , only : lun_es, lun_ef, lun_ws
  use ColumnType                 , only : col_pp
  use ColumnDataType             , only : col_es, col_ef, col_ws, col_wf
  use ColumnDataType             , only : col_cs, c13_col_cs, c14_col_cs
  use ColumnDataType             , only : col_cf, c13_col_cf, c14_col_cf
  use ColumnDataType             , only : col_ns, col_nf
  use ColumnDataType             , only : col_ps, col_pf
  use VegetationType             , only : veg_pp
  use VegetationDataType         , only : veg_es, veg_ef, veg_ws, veg_wf
  use VegetationDataType         , only : veg_cs, c13_veg_cs, c14_veg_cs
  use VegetationDataType         , only : veg_cf, c13_veg_cf, c14_veg_cf
  use VegetationDataType         , only : veg_ns, veg_nf
  use VegetationDataType         , only : veg_ps, veg_pf
  use UrbanParamsType            , only : urbanparams_vars

  implicit none
  save

 public   ! By default everything is public
  !
  !-----------------------------------------
  ! Instances of component types
  !-----------------------------------------
  !
  type(ch4_type)                                      :: ch4_vars
  type(crop_type)                                     :: crop_vars
  type(cnstate_type)                                  :: cnstate_vars
  type(dust_type)                                     :: dust_vars
  type(vocemis_type)                                  :: vocemis_vars
  type(drydepvel_type)                                :: drydepvel_vars
  type(aerosol_type)                                  :: aerosol_vars
  type(canopystate_type)                              :: canopystate_vars
  type(energyflux_type)                               :: energyflux_vars
  type(frictionvel_type)                              :: frictionvel_vars
  type(lakestate_type)                                :: lakestate_vars
  type(photosyns_type)                                :: photosyns_vars
  type(sedflux_type)                                  :: sedflux_vars
  type(soilstate_type)                                :: soilstate_vars
  type(soilhydrology_type)                            :: soilhydrology_vars
  type(solarabs_type)                                 :: solarabs_vars
  type(surfalb_type)                                  :: surfalb_vars
  type(surfrad_type)                                  :: surfrad_vars
  type(atm2lnd_type)                                  :: atm2lnd_vars
  type(glc2lnd_type)                                  :: glc2lnd_vars
  type(lnd2atm_type)                                  :: lnd2atm_vars
  type(lnd2glc_type)                                  :: lnd2glc_vars
  type(glc_diagnostics_type)                          :: glc_diagnostics_vars
  class(soil_water_retention_curve_type), allocatable :: soil_water_retention_curve

end module elm_instMod
