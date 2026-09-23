#' Read in the NCSS database
#' 
#' This function reads in the NCSS sql database from the USDA-NRCS, Go here and download the sqlite zip file. https://ncsslabdatamart.sc.egov.usda.gov/database_download.aspx and place it in the data directory.
#'
#' @param dataDir file location for the data folder
#' @param dataLevel flag for level 0 or level 1 data return
#' @param verbose boolean flag denoting whether or not to print status messages
#' 
#' @return a list of either the original tables or meta data and long primary data
#' @export
#'
#' @importFrom RSQLite dbConnect dbListTables
#' @importFrom dplyr filter filter_out select mutate full_join left_join semi_join case_when if_else bind_rows
#' @importFrom tidyr pivot_longer
#' @importFrom readr read_csv cols col_character
#' @importFrom tidyselect everything all_of
#' @importFrom stringr str_detect
#' @importFrom bibtex read.bib
#' @importFrom lubridate as_date ymd
#'

readNCSS <- function(dataDir, 
                     dataLevel = c('level0', 'level1')[1],
                     verbose = FALSE){
  
  # Declare datafiles ####
  
  # these files were manually pulled 26 Feb 2026 and committed to the repo
  #...and need to be updated periodically
  tableDesc.file <- file.path(dataDir, 'NCSS Table Description.csv')
  columnDesc.file <- file.path(dataDir, 'NCSS Columns Description.csv')
  relationships.file <- file.path(dataDir, 'Relationships.csv')
  uniqueID.file <- file.path(dataDir, 'Unique Constraints.csv')
  
  # this file was downloaded but not repo committed
  sqlDownload <- file.path(dataDir, 'temp','ncss_labdata.sqlite')
  
  # Bibliography files
  primaryCitation.file <- file.path(dataDir, 'NCSS_SCD.bib')
  methodsCitation.file <- file.path(dataDir, 'NCSS_SCD_Methods.bib')
  
  
  keepVars <- c(ls(), 'keepVars')
  
  # Read in level 0 data #####
  
  myconnect <- RSQLite::dbConnect(drv = RSQLite::SQLite(), 
                                  dbname = sqlDownload)
  
  actualTableNames <- RSQLite::dbListTables(myconnect)
  
  table_names <- c("lab_analysis_procedure", "lab_analyte",
                   "lab_chemical_properties", "lab_physical_properties",
                   "lab_calculations_including_estimates_and_default_values",
                   'lab_area', "lab_pedon", "lab_layer", "lab_site",
                   "lab_preparation", "lab_method_code")
  
  if(length(setdiff(table_names, actualTableNames)) != 0){
    stop('Looking for tables that are not there!')
  }
  
  lvl0.primary <- plyr::llply(
    setNames(as.list(table_names), table_names),
    .fun = function(xx){
      ans.ls <- RSQLite::dbReadTable(myconnect, xx)
      #names(ans.ls) <- xx
      return(ans.ls)
    })
  
  RSQLite::dbDisconnect(myconnect)
  
  
  #constructed manually from documentation
  lvl0.meta <- list(
    table = readr::read_csv(file = tableDesc.file,
                     col_types = readr::cols(.default = readr::col_character())),
    columns = readr::read_csv(file = columnDesc.file,
                       col_types = readr::cols(.default = readr::col_character())),
    relationships = readr::read_csv(file = relationships.file,
                             col_types = readr::cols(.default = readr::col_character())),
    uniqueID = readr::read_csv(file = uniqueID.file,
                        col_types = readr::cols(.default = readr::col_character()))
  )
  
  data.lvl0.ls <- list(
    data = lvl0.primary,
    meta = lvl0.meta,
    citations = list(primary = bibtex::read.bib(file = primaryCitation.file),
                     methods = bibtex::read.bib(file = methodsCitation.file))
  )
  
  if(dataLevel == 'level0'){
    return(data.lvl0.ls)
  }
  
  keepVars <- c(keepVars, 'data.lvl0.ls')
  
  rm(list = setdiff(ls(), keepVars)) #remove large file
  
  # Generate column names for level 1####
  #generate the names in the tables of interest below and comment out the names
  #...we are not interested for the current curation purpose. This is here
  #...so that in the future we can add more observations from this rich dataset
  
  #### Physical property names ####
  #### cat(paste0("'", names(data.lvl0.ls$data$lab_physical_properties), "'", collapse = ', 
  #'))
  #
  lab_physical_properties.names <- c(
    #'OBJECTID', 'objectid_1', 
    'layer_key', #'labsampnum', 'result_source_key', 
    #'prep_code', #not uniquely defined for direct cross reference, TODO figure out indirect cross reference
    #'texture_lab', 
    'particle_size_method',
    #  'clay_total', 'silt_total', 'sand_total', 'clay_fine', 
    #  'clay_caco3', 'silt_fine', 'silt_coarse', 'sand_very_fine', 'sand_fine', 
    #  'sand_medium', 'sand_coarse', 'sand_very_coarse', 
    #  'frag_2_5_mm_wt_pct_lt_75', 'frag__2_20_mm_wt_pct_lt_75', 
    #  'frag_5_20_mm_wt_pct_lt_75', 'frag_20_75_mm_wt_pct_lt_75', 
    'total_frag_wt_pct_gt_2_mm_ws', 
    #'wt_pct_1_tenth_to_75_mm', 
    #'bulk_density_tenth_bar', 'bulk_density_tenth_bar_method', 
    #'bulk_density_third_bar', 'bulk_density_third_bar_method', 
    'bulk_density_oven_dry', 'bulk_density_oven_dry_method', 
    'bulk_density_lt_2_mm_air_dry', 'bulk_density_air_dry_method', 
    #'bd_third_bar_lt2_reconstituted', 'bd_thirdbar_reconstituted_method', 
    #'bulk_den_ovendry_reconstituted', 'bulk_density_odreconstituted_method', 
    #'bulk_density_field_moist', 'bulk_density_field_moist_method', 
    #'particle_density_less_than_2mm', 'particle_density_lt_2mm_method', 
    #'particle_density_gt_2_mm', 'particle_density_gt_2mm_method', 
    #'cole_whole_soil', 'cole_whole_soil_method', 
    #'le_third_fifteen_lt2_mm', 'le_third_fifteen_lt2_method', 
    #'le_third_ovendry_lt_2_mm', 'le_third_ovendry_lt_2_mm_method', 
    #'le_field_moist_to_oben_dry', 'le_fm_to_od_method', 
    #'water_retention_0_bar_sieve', 'water_retention_0_bar_method', 
    #'water_retention_6_hundredths', 'water_retention_6_hund_method', 
    #'water_retention_10th_bar', 'water_retention_10th_bar_method', 
    #'water_retention_third_bar', 'water_retention_thirdbar_method', 
    #'water_retention_1_bar', 'water_retention_1_bar_method', 
    #'water_retention_2_bar', 'water_retention_2_bar_method', 
    # 'water_retention_3_bar_sieve', 'water_retention_3_bar_method', 
    #'water_retention_5_bar_sieve', 'water_retention_5_bar_method', 
    #'water_retention_15_bar', 'water_retention_15_bar_method', 
    #'water_retention_field_state', 'water_retention_field_state_me', 
    # 'airdry_ovendry_ratio', 
    #'atterberg_liquid_limit', 'atterberg_liquid_limit_method', 
    #'atterberg_plasticity_index', 
    #'plastic_limit', 'plastic_limit_method', 
    #'aggregate_stability_05_2_mm', 'aggregate_stability_05_2_method', 
    #'le_to_clay_third_bar_to_ovendry', 
    #'water_15_bar_to_clay_ratio', 
    #'cec7_clay_ratio', 
    #'effective_cec_to_clay_ratio', 
    #'psda_ethanol_dispersion_method', 'sand_total_ethanol_dispersible',  'silt_total_ethanol_dispersible', 'clay_total_ethanol_dispersible', 'sand_very_fine_ethanol_dispersible', 'sand_fine_ethanol_dispersible', 'sand_medium_ethanol_dispersible', 'sand_coarse_ethanol_dispersible', 'sand_very_coarse_ethanol_dispersible', 
    #'water_dispersible_fraction_method', 'clay_tot_h2o_dispersible', 'clay_fine_h2o_dispersible', 'clay_co3_h2o_dispersible', 'silt_total_h2o_dispersible', 'silt_fine_h2o_dispersible', 'silt_coarse_h2o_dispersible', 'sand_total_h2o_dispersible', 'sand_vf_h2o_dispersible', 'sand_fine_h2o_dispersible', 'sand_medium_h2o_dispersible', 'sand_coarse_h2o_dispersible', 'sand_vc_h2o_dispersible', 
    # 'color_pyrophosphate_extractable', 'color_pyrophosphate_method', 
    #'bd_thirdbar_before_rewet_organ', 'bd_before_rewet_organic_method',  'bd_thirdbar_rewet_organic_soil', 'bd_third_rewet_organic_method', 'bulk_den_rewet_oven_dry', 'bulk_density_rewet_oven_dry_method', 
    'mineral_content_loss_on_ignition', 'mineral_content_loss_ignition_method'#, 
    #'estimated_organic_matter'#, 'estimated_om_plus_mineral', 
    # 'fiber_analysis_method','fiber_unrubbed', 'fiber_rubbed', 'decomposition_state', 'limnic_material_type'
  )
  
  #### Chemical property names ####
  ####cat(paste0("'", names(data.lvl0.ls$data$lab_chemical_properties), "'", collapse = ', 
  ####'))
  #
  lab_chemical_properties.names <- c(
    #  'OBJECTID', 'objectid_1', 
    'layer_key', #'labsampnum', 
    #'result_source_key', 
    #'prep_code', #not uniquely defined for direct cross reference, TODO figure out indirect cross reference
    #  'ca_nh4_ph_7', 'ca_nh4_ph_7_method', 'mg_nh4_ph_7', 'mg_nh4_ph_7_method', 'na_nh4_ph_7', 'na_nh4_ph_7_method', 'k_nh4_ph_7', 'k_nh4_ph_7_method', 'acidity_bacl2_tea_ph_8_2', 'acidity_bacl2_tea_ph_82_method', 'aluminum_kcl_extractable', 'aluminum_kcl_extract_method', 'manganese_kcl_extractable', 'manganese_kcl_extract_method', 'iron_kcl_extractable', 'iron_kcl_extractable_method', 'cec_nh4_ph_7', 'cec_nh4_ph_7_method', 
    'total_carbon_ncs', 'total_carbon_ncs_method', 
    #  'total_nitrogen_ncs', 'total_nitrogen_ncs_method', 
    #  'total_sulfur_ncs', 'total_sulfur_ncs_method', 
    'organic_carbon_walkley_black', 'oc_walkley_black_method', 
    #  'fe_dithionite_citrate_extractable', 'iron_dc_extract_method', 'aluminum_dithionite_citrate', 'aluminum_dc_extract_method', 'manganese_dithionite_citrate', 'manganese_dc_extract_method', 'ammoniumoxalate_opticaldensity', 'ammonium_ox_opt_dens_method', 'fe_ammoniumoxalate_extractable', 'iron_ammonium_oxalate_method', 'aluminum_ammonium_oxalate', 'al_ammonium_oxalate_method', 'silica_ammonium_oxalate', 'silica_ammonium_oxalate_method', 'manganese_ammonium_oxalate', 'mn_ammonium_oxalate_method', 'carbon_sodium_pyro_phosphate', 'c_na_pyro_phosphate_method', 'iron_sodium_pyro_phosphate', 'iron_na_pyro_phosphate_method', 'aluminum_na_pyro_phosphate', 'aluminum_na_pyro_phosphate_method', 'manganese_na_pyro_phosphate', 'mn_na_pyro_phosphate_method', 
    #  'ph_kcl', 'ph_kcl_method', 'ph_cacl2', 'ph_cacl2_method', 'ph_h2o', 'ph_h2o_method', 'ph_saturated_paste', 'ph_saturated_paste_method', 'ph_oxidized', 'ph_oxidized_initial', 'ph_oxidized_method', 'ph_naf', 'ph_naf_method', 'ph_water_extractable', 'ph_water_extract_method', 
    'caco3_lt_2_mm', 'caco3_lt_2_mm_method', 
    #  'corrected_gypsum_lt_2_mm', 'corrected_gyp_lt_2_mm_method', 
    #  'resistivity_saturated_paste', 'resistivity_sp_method', 
    #  'ca_satx', 'ca_satx_method', 'mg_satx', 'mg_satx_method', 'ca_plus_mg_satx', 'ca_plus_mg_satx_method', 'na_satx', 'na_satx_method', 'k_satx', 'k_satx_method', 'co3_satx', 'co3_satx_method', 'hco3_satx', 'hco3_satx_method', 'co3_plus_hco3_satx', 'co3_plus_hco3_satx_method', 'cl_satx', 'cl_satx_method', 'f_satx', 'f_satx_method', 'po4_satx', 'po4_satx_method', 'br_satx', 'br_satx_method', 'oac_satx', 'oac_satx_method', 'so4_satx', 'so4_satx_method', 'no2_satx', 'no2_satx_method', 'no3_satx', 'no3_satx_method', 'h20_satx', 'h20_satx_method', 'electrical_conductivity_satx', 'electrical_cond_satx_method', 
    #  'ec_predict_one_to_two', 'ec_predict_one_to_two_method',
    #  'melanic_index', 'melanic_index_method', 
    #  'new_zealand_phosphorus_retent', 'new_zealand_phos_retent_method', 'phosphorus_ammonium_oxalate', 'phosphorus_ammonium_oxalate_method', 'phosphorus_anion_resin_one_hr', 'phosphorus_anion_resin_24_hr', 'phosphorus_anion_resin_method', 'phosphorus_bray1', 'phosphorus_bray1_method', 'phosphorus_bray2', 'phosphorus_bray2_method', 'phosphorus_citric_acid', 'phosphorus_citric_acid_method', 'phosphorus_mehlich_3', 'phosphorus_mehlich_3_method', 'phosphorus_olsen', 'phosphorus_olsen_method', 'phosphorus_water', 'phosphorus_water_method', 
    #  'nitrate_1m_kcl', 'nitrate_1m_kcl_method', 
    #  'water_extract_method', 'acetate_water_extractable', 'aluminum_water_extractable', 'arsenic_water_extractable', 'barium_water_extractable', 'boron_water_extractable', 'bromide_water_extractable', 'cadmium_water_extractable', 'calcium_water_extractable', 'chloride_water_extractable', 'chromium_water_extractable', 'cobalt_water_extractable', 'copper_water_extractable', 'ec_water_extractable', 'fluoride_water_extractable', 'iron_water_extractable', 'lead_water_extractable', 'magnesium_water_extractable', 'manganese_water_extractable', 'molybdenum_water_extractable', 'nickel_water_extractable', 'nitrate_n_water_extractable', 'nitrate_water_extractable', 'nitrite_water_extractable', 'phosphorus_water_extractable', 'phosphate_water_extractable', 'potassium_water_extractable', 'selenium_water_extractable', 'silicon_water_extractable', 'sodium_water_extractable', 'strontium_water_extractable', 'sulfate_water_extractable', 'vanadium_water_extractable', 'zinc_water_extractable', 
    #  'mehlich_3_extractable_method', 'aluminum_mehlich3_extractable', 'arsenic_mehlich3_extractable', 'barium_mehlich3_extractable', 'cadmium_mehlich3_extractable', 'calcium_mehlich3_extractable', 'chromium_mehlich3_extractable', 'cobalt_mehlich3_extractable', 'copper_mehlich3_extractable', 'iron_mehlich3_extractable', 'lead_mehlich3_extractable', 'magnesium_mehlich3_extractable', 'manganese_mehlich3_extractable', 'molybdenum_mehlich3_extractable', 'nickel_mehlich3_extractable', 'phosphorus_mehlich3_extractable', 'potassium_mehlich3_extractable', 'selenium_mehlich3_extractable', 'silicon_mehlich3_extractable', 'sodium_mehlich3_extractable', 'strontium_mehlich3_extractable', 'zinc_mehlich3_extractable', 
    #'sum_of_nh4_ph_7_Ext_bases', 'sum_of_cations_cec_pH_8_2', 'ecec_base_plus_aluminum', 'aluminum_saturation', 'base_sat_sum_of_cations_ph_8_2', 'base_sat_nh4oac_ph_7', 
    'estimated_organic_carbon')#, 
  #'carbon_to_nitrogen_ratio', 
  #'aluminum_plus_half_iron_oxalate', 
  #  'caco3_lt_20_mm')
  #'gypsum_lt_20_mm', 
  #'ca_to_mg_ratio', 'total_estimated_salts_satx', 'exchangeable_sodium', 'sodium_absorption_ratio', 'phosphorus_anion_resin_capacity')
  
  ### Layer property names ####
  #cat(paste0("'", names(data.lvl0.ls$data$lab_layer), "'", collapse = ', '))
  
  lab_layer.names <- c(
    #'OBJECTID', 'objectid_1', 
    'layer_key', #'labsampnum', 
    #'project_key', 
    'site_key', 'pedon_key', 
    'layer_sequence', 
    'layer_type', 
    #'layer_field_label_1', 'layer_field_label_2', 'layer_field_label_3', 
    'hzn_top', 'hzn_bot')#,
  #'hzn_desgn_old', 'hzn_desgn', 'hzn_discontinuity', 
  #'hzn_master', 'hzn_prime', 'hzn_vert_subdvn', 'hzn_desgn_other',
  #'non_hzn_desgn', 
  #'stratified_textures_flag', 
  #'texture_description')
  
  ###Pedon names ####
  #cat(paste0("'", names(data.lvl0.ls$data$lab_pedon), "'", collapse = ', '))
  lab_pedon.names <- c(#'OBJECTID', 'objectid_1', 
    'pedon_key', #'pedlabsampnum', 
    'observation_date', 
    #'user_pedon_id', 'pedon_seq_num', 
    #'cntrl_depth_to_top', 'cntrl_depth_to_bot', 
    #'fldsyb', 'mapsyb', 
    'site_key')
  
  ###Site names ####
  #cat(paste0("'", names(data.lvl0.ls$data$lab_site), "'", collapse = ', '))
  lab_site.names <- c(
    #'OBJECTID', 'objectid_1', 
    'site_key', #'user_site_id', 
    'horizontal_datum_name',
    'latitude_direction', 
    #'latitude_degrees', 'latitude_minutes', 'latitude_seconds',
    'longitude_direction',
    #'longitude_degrees', 'longitude_minutes', 'longitude_seconds', 
    'latitude_std_decimal_degrees', 'longitude_std_decimal_degrees')#, 
  #'msrepl_tran_version')
  #'

  
  # Pull individual table information####
  ###Physical table####
  
  #pull bulk density, coarse fraction, and LOI
  physical.df <- data.lvl0.ls$data$lab_physical_properties |>
    dplyr::select(tidyselect::all_of(lab_physical_properties.names)) |> #406 281 obs
    dplyr::filter(is.finite(total_frag_wt_pct_gt_2_mm_ws)) |> #210 027 obs
    dplyr::filter(is.finite(bulk_density_oven_dry) |
             is.finite(bulk_density_lt_2_mm_air_dry)) |> #82 046 obs
    dplyr::mutate(across(everything(), as.character),
           table_name = 'lab_physical_properties',
           from_source = 'Table lab_physical_properties') |>
    tidyr::pivot_longer(cols = -c(table_name, layer_key, from_source),
                 names_to = 'column_name',
                 values_to = 'with_entry',
                 values_drop_na = TRUE) |>
    dplyr::mutate(
      is_type = dplyr::case_when(
        stringr::str_detect(column_name, pattern = 'method$') ~ 'method',
        column_name %in% c('total_frag_wt_pct_gt_2_mm_ws',
                           'bulk_density_oven_dry',
                           'bulk_density_lt_2_mm_air_dry',
                           'mineral_content_loss_on_ignition') ~ 'value',
        .default = 'unknown'
      ),
      of_variable = dplyr::case_when(
        column_name %in% c('particle_size_method', 'total_frag_wt_pct_gt_2_mm_ws') ~ 'coarse_fraction',
        column_name %in% c('bulk_density_oven_dry', 'bulk_density_oven_dry_method') ~ 'bulk_density_stock',
        column_name %in% c('bulk_density_lt_2_mm_air_dry', 'bulk_density_air_dry_method') ~ 'fine_earth_bulk_density',
        column_name %in% c('mineral_content_loss_on_ignition', 'mineral_content_loss_ignition_method') ~ 'loss_on_ignition',
        .default = 'unknown'
      )
    )
  
  #check for unknowns with this code
  #physical.df |> reframe(count = n(), .by = c(is_type, of_variable)) 
  
  ###Chemistry table####
  chemical.df <- data.lvl0.ls$data$lab_chemical_properties |>
    dplyr::select(tidyselect::all_of(lab_chemical_properties.names)) |> #325 740 obs
    dplyr::filter(is.finite(total_carbon_ncs - caco3_lt_2_mm) |
             is.finite(estimated_organic_carbon) |
             is.finite(organic_carbon_walkley_black)) |> #296 603 obs
    dplyr::mutate(across(tidyselect::everything(), as.character),
           table_name = 'lab_chemical_properties',
           from_source = 'Table lab_chemical_properties') |>
    tidyr::pivot_longer(cols = -c(table_name, layer_key, from_source),
                 names_to = 'column_name',
                 values_to = 'with_entry',
                 values_drop_na = TRUE) |>
    dplyr::mutate(
      is_type = dplyr::case_when(
        column_name %in% c('total_carbon_ncs_method',
                           'caco3_lt_2_mm_method',
                           'oc_walkley_black_method') ~ 'method',
        column_name %in% c('total_carbon_ncs',
                           'estimated_organic_carbon',
                           'caco3_lt_2_mm',
                           'organic_carbon_walkley_black') ~ 'value',
        .default = 'unknown'
      ),
      of_variable = dplyr::case_when(
        column_name %in% c("total_carbon_ncs", "total_carbon_ncs_method") ~ 'carbon_total',
        column_name %in% c('estimated_organic_carbon') ~ 'carbon_organic',
        column_name %in% c('caco3_lt_2_mm', 'caco3_lt_2_mm_method') ~ 'carbon_inorganic',
        column_name %in% c('organic_carbon_walkley_black', 'oc_walkley_black_method') ~ 'carbon_organic_walkley_black',
        .default = 'unknown'
      )
    )
  
  #check for unknowns with this code
  #chemical.df |> reframe(count = n(), .by = c(is_type, of_variable)) 
  
  
  ###Layer table####
  #select => filter => mutate - character/table/source => pivot => mutate - types/varibles
  layer.df <- data.lvl0.ls$data$lab_layer |>
    dplyr::select(tidyselect::all_of(lab_layer.names)) |>
    #keep layers that are in the soil observation tables
    dplyr::filter(layer_key %in% c(chemical.df$layer_key, physical.df$layer_key)) |>
    dplyr::mutate(across(tidyselect::everything(), as.character),
           table_name = 'lab_layer',
           from_source = 'Table Lab_layer') |>
    tidyr::pivot_longer(cols = c("layer_sequence", "layer_type", "hzn_top", "hzn_bot"),
                 names_to = 'column_name',
                 values_to = 'with_entry',
                 values_drop_na = TRUE) |>
    dplyr::mutate(of_variable = 'depth_layer',
           is_type = dplyr::case_when(column_name == 'hzn_bot' ~ 'lower',
                               column_name == 'hzn_top' ~ 'upper',
                               column_name == 'layer_sequence' ~ 'order',
                               column_name == 'layer_type' ~ 'boundary',
                               .default = 'unknown')) 
  # layer.df |> reframe(count = n(), .by = c(is_type, of_variable)) 
  
  ###Pedon table####
  #select => filter => mutate - character/table/source => pivot => mutate - types/varibles
  pedon.df <- data.lvl0.ls$data$lab_pedon |>
    dplyr::select(tidyselect::all_of(lab_pedon.names)) |>
    dplyr::filter(is.finite(observation_date)) |>
    #cross reference with the layer table defined by the 
    dplyr::filter(pedon_key %in% layer.df$pedon_key) |>
    #recast the observation_date
    dplyr::mutate(observation_date = 
             lubridate::as_date(observation_date,
                                origin = lubridate::ymd('1900-01-01')) |>
             as.character()) |>
    dplyr::mutate(across(tidyselect::everything(), as.character)) |>
    tidyr::pivot_longer(cols = -c(pedon_key, site_key),
                 names_to = 'column_name',
                 values_to = 'with_entry') |>
    dplyr::mutate(of_variable = dplyr::if_else(column_name == 'observation_date',
                                 'observation_date', 'unknown'),
           is_type = dplyr::if_else(column_name == 'observation_date', 
                             'date', 'unknown'),
           from_source = 'Table lab_pedon',
           table_name = 'lab_pedon') 
  
  #pedon.df |> reframe(count = n(), .by = c(is_type, of_variable)) 
  
  ###Site table####
  #select => filter => mutate - character/table/source => pivot => mutate - types/varibles
  site.df <- data.lvl0.ls$data$lab_site |> 
    dplyr::select(tidyselect::all_of(lab_site.names)) |>
    dplyr::filter(site_key %in% c(pedon.df$site_key, layer.df$site_key)) |> #50 614 obs
    dplyr::filter(is.finite(latitude_std_decimal_degrees) &
             is.finite(longitude_std_decimal_degrees)) |> #39 308 obs
    dplyr::mutate(across(tidyselect::everything(), as.character),
           table_name = 'lab_site',
           from_source = 'Table lab_site') |>
    tidyr::pivot_longer(cols = -c(site_key, table_name, from_source),
                 names_to = 'column_name',
                 values_to = 'with_entry',
                 values_drop_na = TRUE) |>
    dplyr::mutate(of_variable = dplyr::if_else(
      column_name %in% c('horizontal_datum_name',
                         'latitude_direction',
                         'longitude_direction',
                         'latitude_std_decimal_degrees',
                         'longitude_std_decimal_degrees'), 'geolocation', 'unknown'),
      is_type = dplyr::case_when(
        column_name %in% c('horizontal_datum_name') ~ 'datum',
        column_name %in% c('latitude_std_decimal_degrees') ~ 'latitude',
        column_name == 'longitude_std_decimal_degrees' ~ 'longitude',
        column_name == 'longitude_direction' ~ 'longitude_direction',
        column_name == 'latitude_direction' ~ 'latitude_direction',
        .default = 'unknown'
      )
    )
  
  ##Merge keys ####
  
  key.df <- layer.df |> 
    dplyr::select(site_key, pedon_key, layer_key) |>
    unique() |> #1 194 008 obs
    dplyr::filter(!is.na(site_key),
           !is.na(pedon_key),
           !is.na(layer_key)) #299 323 obs
  
  soilObs.df <- key.df |>
    dplyr::left_join(layer.df,
              by = join_by(site_key, pedon_key, layer_key)) |>
    dplyr::bind_rows( key.df |>
                 dplyr::left_join(dplyr::bind_rows(physical.df, chemical.df),
                           by = join_by(layer_key)))
  
  key.df <- layer.df |> 
    dplyr::select(site_key, pedon_key) |>
    unique() |> #50 986 obs
    dplyr::filter(!is.na(site_key),
           !is.na(pedon_key)) #50 985 obs
  
  geoObs.df <- key.df  |>
    dplyr::left_join(pedon.df,
              by = join_by(site_key, pedon_key)) |>
    dplyr::bind_rows(key.df |>
                dplyr::left_join(site.df,
                          by = join_by(site_key),
                          #some locations are observed many times
                          relationship = 'many-to-many') )
  # names(physical.df) #layer_key
  # names(chemical.df) #layer_key
  # names(pedon.df) #pedon_key, site_key
  # names(layer.df) #layer_key, site_key, pedon_key
  # names(site.df) #site_key
  
  ##Pull metadata from column discriptions####
  meta.df <- data.lvl0.ls$meta$columns |>
    dplyr::semi_join(dplyr::bind_rows(geoObs.df, soilObs.df),
              by = join_by(table_name, column_name)) |>
    dplyr::filter_out(stringr::str_detect(column_name, 'method$')) |> #remove method columns
    dplyr::mutate(from_source = 'Web site table: NCSS Columns Description') |>
    #coded from description interpretation with cross reference to data dictionary and methods descriptions
    dplyr::mutate(unit = dplyr::case_when(
      column_description == "Carbonate in the < 2mm fraction is measured by CO2 evolution after acid treatment. It is reported as gravimetric percent CaCO3 on a <2 mm base, even though carbonates of Mg, Na, K, and Fe may be present and react with the acid." ~ 'fine-earth-mass percent',
      column_description == "Total carbon is a measure of all organic and inorganic carbon, including that found in carbonate minerals." ~ 'fine-earth-mass percent', #expert interpretation from method understanding,
      column_description == 'CMS analyte. Organic carbon is a measure of all organic forms of carbon in the soil, including organic carbon within minerals.' ~ 'fine-earth-mass percent', #expert interpretation from method understanding,
      column_description == "Organic carbon is a measure of all organic forms of carbon in the soil, including organic carbon within minerals." ~ 'fine-earth-mass percent', #expert interpretation from method understanding,
      column_description == "The oven dry weight of the less than 2 mm soil material per unit volume of soil exclusive of the desiccation cracks, measured on a coated clod." ~ 'fine-earth g per whole-soil cm3', #expert interpretation
      column_description == 'Bulk density, <2mm fraction, air-dry is the weight per unit volume of the <2 mm fraction, with volume measured after air drying.  It is reported as grams per cubic centimeter on a <2 mm base.' ~ 'fine-earth g per fine-earth cm3',
      column_description == "The best estimate of the total horizon fragment weight, in percent, on a whole soil basis." ~ 'whole-soil mass percent',
      column_name %in% c('hzn_top', 'hzn_bot') ~ 'cm',
      column_name %in% c('layer_sequence', 'layer_type',
                         'observation_date',
                         'horizontal_datum_name',
                         'latitude_direction', 'longitude_direction') ~ NA_character_,
      column_name %in% c('latitude_std_decimal_degrees', 'longitude_std_decimal_degrees') ~ 'decimal degrees',
      .default = 'unknown'
    )) |>
    dplyr::select(table_name, column_name, from_source, description = column_description, unit) |>
    tidyr::pivot_longer(cols = c(description, unit),
                 names_to = 'is_type', values_to = 'with_entry',
                 values_drop_na = TRUE) |>
    dplyr::left_join(
      dplyr::bind_rows(
        dplyr::select(soilObs.df, table_name, column_name, of_variable) |>
          unique(),
        dplyr::select(geoObs.df, table_name, column_name, of_variable) |>
          unique()),
      by = join_by(table_name, column_name))
  
  
  # Create level 1 #####
  ## Pull everything together into by stacking the meta and primary data tables
  data.lvl1.ls <- list(
    meta = meta.df,
    layer = soilObs.df,
    geolocation = geoObs.df,
    citations = data.lvl0.ls$citations
  )
  
  if(dataLevel == 'level1'){
    return(data.lvl1.ls)
  }else{
    stop('unknown data level')
  }
  
  #keepVars <- c(keepVars, 'data.lvl1.ls')
  #rm(list = setdiff(ls(), keepVars)) #remove large file
  
}
