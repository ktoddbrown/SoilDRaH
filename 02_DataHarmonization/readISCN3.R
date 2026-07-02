#' Load ISCN3
#'
#' Database citation: Nave, L., K. Johnson, C. van Ingen, D. Agarwal, M. Humphrey, and N. Beekwilder. 2022. International Soil Carbon Network version 3 Database (ISCN3) ver 1. Environmental Data Initiative. https://doi.org/10.6073/pasta/cc751923c5576b95a6d6a227d5afe8ba (Accessed 2024-06-13).
#'
#' @param dataDir path to the folder SoilDRaH/01_DataRescue/ISCN3_2015.
#' @param dataLevel flag for level 0 or level 1 data return
#' @param verbose boolean flag denoting whether or not to print status messages
#' 
#' @return list of annotation and data tables
#'
#' @importFrom tibble tribble cols col_character
#' @importFrom readr read_delim
#' @importFrom dplyr mutate bind_rows filter select if_else left_join full_join join_by
#' @importFrom tidyr pivot_longer
#' 
#' @export
#' 

readISCN3 <- function(dataDir, dataLevel = c('level0', 'level1')[1],
                          verbose = FALSE){
  
  ### set veriables for development
  #dataDir <- '01_DataRescue/ISCN3_2015'
  #dataLevel <-  c('level0', 'level1')[1]
  
  ### construct file paths ####
  # verify that the user specified a file path
  if(is.null(dataDir)){
    stop('Data folder must be specified.')
  }
  
  ### setup ####
  # These files have some level of manual rescue/transcription to them
  
  # Control vocabulary transcribed from EDI file `TemplateCVs.pdf`
  controlVocabulary <- file.path(dataDir, 'ISCN3_TemplateCVs.csv')
  
  ## Files from the `TemplateSubmit.pdf` document include a formatted header
  ##... and the table discriptions
  
  # Transcription of the submission guidelines from the EDI file `TemplateSubmit.pdf`
  readME_templateSubmit <- file.path(dataDir, 'README_TemplateSubmit.md')
  
  #Transcription of the control vocabulary pdf from EDI TemplateCVs.pdf
  readme_templateCV <- file.path(dataDir, 'README_TemplateCV.md')
  ISCN3_controlVocab <- file.path(dataDir, 'ISCN3_TemplateCVs.csv')
  
  # Rescued transcriptions of individual table meta data from the 
  #... EDI file `TemplateSubmit.pdf` for the layer, profile, site
  #... disturbance, and fraction tables. These do not match exactly the 
  #... EDI archived tables but do provide the metadata for those tables
  ISCN3_layerAnnotations <- file.path(dataDir, 'ISCN3_TemplateSubmit_TableLayer.csv')
  ISCN3_profileAnnotations <- file.path(dataDir, 'ISCN3_TemplateSubmit_TableProfile.csv')
  ISCN3_siteAnnotations <- file.path(dataDir, 'ISCN3_TemplateSubmit_TableSite.csv')
  ISCN3_disturbanceAnnotations <- file.path(dataDir, 'ISCN3_TemplateSubmit_TableDisturbance.csv')
  ISCN3_fractionAnnotations <- file.path(dataDir, 'ISCN3_TemplateSubmit_TableFraction.csv')
  
  #### Files that have to do with the fixed citations
  
  # Transcribed citations from the EDI file `ISCN3_citation.csv`
  #...table and then extended manually when files were mis-cited
  rescuedContributedCitations <- file.path(dataDir, 'ISCN3_ISCN3_citation.csv')
  
  # Bibliography files
  primaryCitation.file <- file.path(dataDir, 'ISCN3_2015.bib')
  contributionsCitation.file <- file.path(dataDir, 'ISCN3_Contributors.bib')
  
  ### dataDownloadEDI ####
  ### "Download and identify each file in the EDI package."
  dataDownload.dir <- file.path(dataDir, 'temp')
  # verify that the user specified a file path
  if(!file.exists(dataDownload.dir)){
    warning(paste('creating directory for download:', dataDownload.dir))
    dir.create(dataDownload.dir)
  }
  
  # link the table name to the download url and file path(s) for data package
  pasta_package <- 'https://pasta.lternet.edu/package/data/eml/edi/1160/1/'
  
  #transcribe from the EDI site, should be stable and not need future updates
  download_table <- tibble::tribble(
    ~table, ~entity_id, ~file_name, 
    'layer', "4af719a84f8981fcc63f1f92760cb253", 'ISCN3_layer.csv',
    'profile', "40527580cc045d33d9a5aaf728bf204e", 'ISCN3_profile.csv',
    'citation', "320e31ca911f187550ca2143c31fd408", 'ISCN3_citation.csv',
    'dataset', "cdd0c7a4cac3f28d6d788c91f506775f", 'ISCN3_dataset.csv',
    'control_vocabulary', "a8eef6e94b669b365e443c15d9402a03", 'ISCNTranscribed_TemplateCVs.xlsx',
    'template', "cb5c1cd86e9ab17f699f7a05044325b7", 'ISCNtemplate.xlsx',
    'other_pdfs', "eb320ae7b57296765f543cbb370b0f24", 'TemplateCVs.pdf',
    'other_pdfs', "b81a7f4214d176280a5ff4e0f0d52d8b", 'TemplateSubmit.pdf')
  
  #One by one, download the data in each row from the download_table
  #... and store that in the appropriate file path location on your computer
  for(row_index in 1:nrow(download_table)){
    #check to see if the file exists before trying to download it
    if(!file.exists(file.path(dataDownload.dir,
                              download_table$file_name[row_index]))){
      #tell the user you are downloading the file
      print(paste('Download file:', 
                  download_table$file_name[row_index]))
      #use the utility download function to pull the file based on the 
      #... pasta url for ISCN3 and the entity ID, match that up with the file
      #... name on that row
      utils::download.file(
        paste0(pasta_package, download_table$entity_id[row_index]), 
        file.path(dataDownload.dir, download_table$file_name[row_index]), 
        extra  = "--max-time 300")
    } #end file exists check
  } #end for loop through the download table
  
  
  ### readLevel0 ####
  #This chunk has two purposes. 
  #...1) Check the formatting by reading in everything 
  #...2) Create a list of everything to process later in level 1
  
  data.lvl0.ls <- list(
    ####Read in a list of all the bib files####
    citation = list(
      #Citation for the article transcriptions are pulled from
      primary = read.bib(file = primaryCitation.file), 
      #Citations for all referenced articles
      contributed = read.bib(file = contributionsCitation.file)
    ),
    ####Read in the tables and their meta data###
    data = list(
      ####Citation table####
      TableCitation = list(
        #manual correction to out of date citations, originally
        #...this was the `temp/ISCN_citation.csv`
        primary = readr::read_delim(file = rescuedContributedCitations, 
                                    skip = 1,
                                    delim = ';', 
                                    col_types = 
                                      readr::cols(.default = 
                                                    readr::col_character())),
        caption = readr::read_delim(file = rescuedContributedCitations, 
                                    n_max = 1,
                                    delim = ';',
                                    col_names = FALSE,
                                    col_types = 
                                      readr::cols(.default = 
                                                    readr::col_character()))[[1]]
        #look in the metadata in the profile table for some variables
      ), #end citation list
      ####Dataset table####
      TableDataset = list(
        primary = readr::read_delim(file = file.path(dataDownload.dir,
                                                     'ISCN3_dataset.csv'), 
                                    delim = ';', 
                                    col_types = 
                                      readr::cols(.default = 
                                                    readr::col_character()))
        #look in the metadata in the profile table for some variables
      ), #end dataset list
      ###Site table####
      TableSite = list(
        #no primary data here for sites, look in the profile and layer tables
        meta = readr::read_delim(file = ISCN3_siteAnnotations,
                                 delim = ",",
                                 col_types = readr::cols(.default = readr::col_character())
        )
      ), #end site
      TableFraction = list(
        #no primary data here
        meta = readr::read_delim(file = ISCN3_fractionAnnotations,
                                 delim = ",",
                                 col_types = readr::cols(.default = readr::col_character())
        )
      ), #end fraction
      TableDisturbance = list(
        #no primary data here
        meta = readr::read_delim(file = ISCN3_disturbanceAnnotations,
                                 delim = ",",
                                 col_types = readr::cols(.default = readr::col_character())
        )
      ), #end disturbance
      ###Profile table####
      TableProfile = list(
        primary = readr::read_delim(file = file.path(dataDownload.dir,
                                                     'ISCN3_profile.csv'), 
                                    delim = ';', 
                                    col_types = 
                                      readr::cols(.default = 
                                                    readr::col_character())),
        meta = readr::read_delim( file = ISCN3_profileAnnotations,
                                  delim = ",",
                                  col_types = 
                                    readr::cols(.default =
                                                  readr::col_character()))
      ), #end profile
      #### Layer table ####
      TableLayer = list(
        primary = readr::read_delim(file = file.path(dataDownload.dir,
                                                     'ISCN3_layer.csv'), 
                                    delim = ';', 
                                    col_types = 
                                      readr::cols(.default = 
                                                    readr::col_character())),
        meta = readr::read_delim( file = ISCN3_layerAnnotations, 
                                  delim = ",",
                                  col_types = readr::cols(
                                    .default = readr::col_character()))
      ) #end layer table
    ), # end data list
    #### General control vocabulary that is not broke down by table ####
    control_vocabulary = list(
      readme = read_lines(file = readme_templateCV),
      caption = readr::read_delim( file = ISCN3_controlVocab, 
                                   n_max = 1,
                                   col_names = FALSE,
                                   delim = ",",
                                   col_types = readr::cols(
                                     .default = readr::col_character()))[[1]],
      primary = readr::read_delim( file = ISCN3_controlVocab, 
                                   skip = 1,
                                   delim = ",",
                                   col_types = readr::cols(
                                     .default = readr::col_character())))
  )
  
  # If the level 0 format is asked for, then stop here and 
  # return the list we just made.
  if(dataLevel == 'level0'){
    return(data.lvl0.ls)
  }
  
  ### createLvl1 ####
  ### 'Create a level 1 data set with SoilDRaH standard vocabulary and table structures.'
  
  #Site table
  site.df <- data.lvl0.ls$data$TableProfile$primary |>
    dplyr::select("dataset_name_sub", "dataset_name_soc", #ID
           "site_name", "profile_name", #ID
           "lat (dec. deg)", "long (dec. deg)", "datum (datum)", #geolocation
           "state (state_province)", "country (country)", #region
           "observation_date (YYYY-MM-DD)", #observation_date
           "profile_zero_ref (profile_zero_ref)") |>  #depth
    tidyr::pivot_longer(cols = all_of(c(
      "lat (dec. deg)", "long (dec. deg)", "datum (datum)", #geolocation
      "state (state_province)", "country (country)", #region
      "observation_date (YYYY-MM-DD)", #observation_date
      "profile_zero_ref (profile_zero_ref)"
    )),
    names_to = 'column_header', values_to = 'with_entry',
    values_drop_na = TRUE) |>
    dplyr::mutate(of_variable = case_when(
      column_header %in% c("lat (dec. deg)", "long (dec. deg)", "datum (datum)") ~ 'geolocation',
      column_header %in% c("state (state_province)", "country (country)") ~ 'region', #region
      column_header %in% c("observation_date (YYYY-MM-DD)") ~ 'observation_date', #observation_date
      column_header %in% c("profile_zero_ref (profile_zero_ref)") ~ 'depth',
      .default = NA_character_),
      is_type = case_when(
        column_header %in% c("lat (dec. deg)") ~ 'latitude',
        column_header %in% c("long (dec. deg)") ~ 'longitude',
        column_header %in% c("datum (datum)") ~ 'datum',
        column_header %in% c("state (state_province)") ~ 'state',
        column_header %in% c("country (country)") ~ 'country',
        column_header %in% c("observation_date (YYYY-MM-DD)") ~ 'date', #observation_date
        column_header %in% c("profile_zero_ref (profile_zero_ref)") ~ 'zero_reference',
        .default = NA_character_)
    ) |>
    dplyr::mutate(from_source = 'Table Profile')
  
  #collection table
  collection.df <- data.lvl0.ls$data$TableDataset$primary |>
    dplyr::select("dataset_name", #matches dataset_name_sub or dataset_name_soc
           #"dataset_type (dataset_type)",
           "curator_name", "curator_organization", "curator_email", 
           "contact_name", "contact_email", 
           "dataset_description") |>
    dplyr::mutate(source_from = 'Table Dataset') |>
    dplyr::full_join(
      data.lvl0.ls$data$TableCitation$primary |>
        dplyr::select("dataset_name", #matches dataset_name_sub or dataset_name_soc
               "reference", 
               "citation", "citation_usage", 
               "acknowledgement", "acknowledgement_usage") |>
        dplyr::mutate(source_from = 'Table Citation'),
      by = join_by(dataset_name, source_from)) |>
    tidyr::pivot_longer(cols = -c(dataset_name, source_from),
                 names_to = 'column_header', values_to = 'with_entry',
                 values_drop_na = TRUE) |>
    dplyr::mutate(of_variable = str_extract(column_header, '^[^_]+'),
           is_type = str_extract(column_header, '[^_]+$')) |>
    dplyr::mutate(is_type = dplyr::if_else(of_variable == is_type, 'value', is_type))
  
  layer.df <- data.lvl0.ls$data$TableLayer$primary |>
    #make layer names unique, some were numeric based and got truncated 
    dplyr::mutate(layer_name = sprintf('%s (%s to %s)', layer_name, `layer_top (cm)`, `layer_bot (cm)`)) |>
    #There are 2136 layers that are not uniquely defined, looks like rows doubled form NRCS prep codes
    dplyr::mutate(row_name = paste0('R', 1:n())) |>
    dplyr::select('row_name', 
           #"ISCN 1-1 (2015-12-10)", 
           "dataset_name_sub", "dataset_name_soc", 
           "site_name", "profile_name", 
           #"lat (dec. deg)", "long (dec. deg)", "datum (datum)",
           #"state (state_province)", "country (country)", 
           #"observation_date (YYYY-MM-DD)",
           "layer_name", "layer_top (cm)", "layer_bot (cm)", "layer_note", 
           #"hzn_desgn_other", "hzn", "hzn_desgn", "color",
           #"vegclass_local", 
           #"soil_taxon", "soil_series", 
           "bd_method", "bd_samp (g cm-3)", "bd_tot (g cm-3)", "bd_whole (g cm-3)", "bd_other (g cm-3)", "bdNRCS_prep_code", 
           "cNRCS_prep_code", "c_method", "c_tot (percent)", "oc (percent)", "loi (percent)", 
           #"n_tot (percent)", 
           #"c_to_n (mass ratio)", 
           "soc (g cm-2)", "soc_carbon_flag", "soc_method", 
           #"ph_method", "ph_cacl", "ph_h2o", "ph_other", 
           "caco3 (percent)", 
           #"sand_tot_psa (percent)", "silt_tot_psa (percent)", "clay_tot_psa (percent)", 
           "wpg2_method", "wpg2 (percent)", 
           #"cat_exch (cmol H+ kg-1)", 
           #"al_dith (specified by al_fe_units)", "al_ox (specified by al_fe_units)", "al_other (specified by al_fe_units)", "fe_dith (specified by al_fe_units)", "fe_ox (specified by al_fe_units)", "fe_other (specified by al_fe_units)", "mn_dith (specified by al_fe_units)", "mn_ox (specified by al_fe_units)", "mn_other (specified by al_fe_units)", "al_fe_units (extract_units)", "al_fe_method",
           #"ca_al (specified by bc_units)", "ca_ext (specified by bc_units)", "k_ext (specified by bc_units)", "mg_ext (specified by bc_units)", "na_ext (specified by bc_units)", "bc_units (extract_units)", "bc_method", "base_sum (specified by cec_h_units)", "cec_sum (specified by cec_h_units)", "ecec (specified by cec_h_units)", "cec_h_units (extract_units)", 
           #"bs (percent)", "bs_sum (percent)", 
           #"h_ext (specified by metal_ext_units)", "zn_ext (specified by metal_ext_units)", "metal_ext_units (extract_units)", "metal_ext_method", 
           #"p_bray (specified by p_units)", "p_ox (specified by p_units)", "p_meh (specified by p_units)", "p_other (specified by p_units)", "p_units (extract_units)", "p_method", 
           #"root_quant_size", "root_weight (g)",
           #"15n (‰)", "13c (‰)", "14c (‰)", "14c_sigma (‰)", "14c_age (BP)", "14c_age_sigma (BP)", "fraction_modern", "fraction_modern_sigma", 
           #"textureClass", "locator_parent_alias"
    ) |>
    tidyr::pivot_longer(cols = -contains('_name'),
                 names_to = 'column_header', values_to = 'with_entry',
                 values_drop_na = TRUE) |>
    dplyr::mutate(of_variable = 
             case_when(
               column_header %in% c("layer_top (cm)", "layer_bot (cm)", "layer_note") ~ 'layer',
               #column_header %in% c("color") ~ 'soil_color',
               #column_header %in% c("hzn_desgn_other", "hzn", "hzn_desgn") ~ 'soil_horizon',
               #column_header %in% c("soil_taxon", "soil_series") ~ 'soil_classificaction',
               #Bulk density types taken from metadata
               column_header %in% c( "bd_samp (g cm-3)") ~ 'bulk_density_fine_earth',
               #yes this bd_whole and bd_tot are correct according to the meta data
               column_header %in% c("bd_whole (g cm-3)") ~ 'bulk_density_estimated_fine_earth',
               column_header %in% c( "bd_tot (g cm-3)") ~ 'bulk_density_whole_soil',
               column_header %in% c("bd_method", "bd_other (g cm-3)", "bdNRCS_prep_code") ~ 'bulk_density',
               #And the splitting strategy with soil carbon
               column_header %in% c("oc (percent)") ~ 'carbon_organic_fraction',
               column_header %in% c("soc (g cm-2)", "soc_carbon_flag", "soc_method") ~ 'carbon_organic_volume_density',
               column_header %in% c("loi (percent)") ~'loss_on_ignition',
               #need to associated with other carbon variables later
               column_header %in% c("cNRCS_prep_code", "c_method") ~'carbon',
               column_header %in% c("c_tot (percent)") ~ 'carbon_total',
               column_header %in% c("caco3 (percent)") ~ 'carbon_inorganic',
               column_header %in% c("wpg2_method", "wpg2 (percent)") ~ 'coarse_fraction',
               .default = NA_character_
             ),
           is_type = 
             case_when(
               str_detect(column_header, 'note') ~ 'note',
               str_detect(column_header, 'method') ~ 'method',
               str_detect(column_header, 'NRCS_prep_code') ~ 'NRCS_prep_code',
               str_detect(column_header, 'flag') ~ 'flag',
               column_header == "layer_top (cm)" ~ 'upper',
               column_header == "layer_bot (cm)" ~ 'lower',
               .default = 'value')) |>
    #Check the classifications of the columns
    #   dplyr::select(column_header, of_variable, is_type) |> unique() |> view()
    #   Check for uniquenesss
    #  dplyr::filter(n() > 1, .by =c(contains('_name'), of_variable, is_type))
    dplyr::mutate(source_from = 'Table Layer' )
  
  #this is nto exactly clean but I think it's workable
  layer.meta <- layer.df |>
    dplyr::select(column_header, of_variable) |>
    unique() |>
    dplyr::mutate(ISCN_variable = str_extract(column_header, '^\\S+')) |>
    dplyr::mutate(across(everything(), str_trim)) |>
    dplyr::left_join(data.lvl0.ls$data$TableLayer$meta |>
                dplyr::mutate(across(everything(), str_trim)),
              by = c('ISCN_variable' = 'Variable')) |>
    dplyr::select(column_header, of_variable, 
           ISCN3_vocabulary = ISCN_variable, name = Name, unit = Units, description = Description) |>
    tidyr::pivot_longer(cols = c(ISCN3_vocabulary, name, unit, description),
                 names_to = 'is_type', values_to = 'with_entry',
                 values_drop_na = TRUE) |>
    dplyr::mutate(from_source = 'Meta for Layer (ISCN3_TemplateSubmit_TableLayer.csv)')
  
  
  
  # What is true across all of ISCN3
  study.df <- tribble(~of_variable, ~is_type, ~with_entry, ~from_source,
                      'citation', 'value', '@ISCN3', 'EDI download page',
                      'version', 'value', "ISCN 1-1 (2015-12-10)", 'orginal table header number 1',
                      'geolocation', 'unit', 'decimal degrees', 'profile and layer table column names',
                      'observation_date', 'format', 'days since 1900-01-01', 'infered given past history in Excel.') |> 
    dplyr::bind_rows(layer.meta)
  
  #### Create level 1 #####
  ## Pull everything together
  data.lvl1.ls <- list(
    study = study.df,
    collection = collection.df,
    site = site.df,
    layer = layer.df,
    citations = c(data.lvl0.ls$citation$primary, data.lvl0.ls$citation$contributed)
  )
  
  if(dataLevel == 'level1'){
    return(data.lvl1.ls)
  }
  
  stop("dataLevel option unknown")
}