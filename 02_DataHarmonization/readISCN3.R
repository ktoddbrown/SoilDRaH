#' Load ISCN3
#'
#' Database citation: Nave, L., K. Johnson, C. van Ingen, D. Agarwal, M. Humphrey, and N. Beekwilder. 2022. International Soil Carbon Network version 3 Database (ISCN3) ver 1. Environmental Data Initiative. https://doi.org/10.6073/pasta/cc751923c5576b95a6d6a227d5afe8ba (Accessed 2024-06-13).
#'
#' @param dataDir path to the folder SoilDRaH/01_DataRescue/ISCN3_2015. This should contain a folder called 'temp' which is where the downloads will live.
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
  #dataLevel <-  c('level0', 'level1')[2]
  
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
  readME_templateSubmit <- file.path(dataDir, 'ISCN3_TemplateSubmit_preamble.md')
  
  #Transcription of the control vocabulary pdf from EDI TemplateCVs.pdf
  readme_templateCV <- file.path(dataDir, 'ISCN3_TemplateCV_preamble.md')
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
  
  ### Create level 1 ####

  #constants for precisions
  precisions.ls <- list(latLong_digits = 4, #assume lat/lon precision of 0.0001
                        #assume precision of 1cm
                        depth_digits = 0,
                        #assume precision of bulk density at 0.01 g cm-3
                        bulkDensity_digits = 2,
                        #assume precision of 0.1% for loss on ignition
                        organicFraction_digits = 1,
                        #assume precision of 0.01% for carbon fraction that is often measured using an elemental analyizer, this could be method specific in the future.
                        carbonFraction_digits = 2,
                        #since this is normalized to the layer thickness, use a very high precision here
                        carbonStock_digits = 10) 
  
  #Create citation table for the data collection
  #collection table
  collection.df <- 
    #### Create cross key for the citation dataset naming to include the double
    #### citations needed for the Permafrost RCN data sets
    data.lvl0.ls$data$TableDataset$primary |>
    select("dataset_name", #matches dataset_name_sub or dataset_name_soc
           #"dataset_type (dataset_type)",
           "curator_name", "curator_organization", "curator_email", 
           "contact_name", "contact_email", 
           "dataset_description"
    ) |>
    mutate(from_source = 'Table Dataset') |>
    #expand the Permafrost RCN datasets so we can pull in that citation
    reframe(dataset_name_citation = c('Permafrost_RCN', dataset_name)[c(str_detect(dataset_name, '^Permafrost RCN_'), TRUE)],
            .by = everything()) |>
    #cross reference with citation table to pull the bibkey from the 'citation' field
    full_join(
      data.lvl0.ls$data$TableCitation$primary |>
        select(dataset_name_citation = "dataset_name", #matches dataset_name_sub or dataset_name_soc
               "reference", 
               "citation", "citation_usage", 
               "acknowledgement", "acknowledgement_usage") |>
        mutate(from_source = 'Table Citation'),
      relationship = "many-to-many",
      by = join_by(dataset_name_citation)) |>
    #### Trim NRCS, remove 2 collection. Work with the current NRCS data download instead
    filter(!str_detect(dataset_name, 'NRCS')) |>
    #merge multiple citations for each dataset name (for example if it was a permafrost RCN study this will have both the RCN citation and original source)
    reframe(with_entry = paste(sort(unique(citation)), collapse = ';'),
            from_source = paste(sort(unique(c(from_source.x, from_source.y))), collapse = ';'),
            .by = dataset_name) |>
    mutate(of_variable = 'bibkey',
           is_type = 'value')
  
  #### Pre-processing ####
  
  
  #> data.lvl0.ls$data$TableProfile$primary$`lat (dec. deg)` |> unique() |> as.numeric() |> sort() |> diff() |> quantile(c(0.01, 0.05, 0.1, 0.2, 0.5))
  #     1%      5%     10%     20%     50% 
  #0.00001 0.00002 0.00004 0.00008 0.00028
  #Format numerical values and impose precision
  site_org.df <- data.lvl0.ls$data$TableProfile$primary |>
    select("dataset_name_sub", "dataset_name_soc", #ID
           "site_name", "profile_name", #ID
           "lat (dec. deg)", "long (dec. deg)", "datum (datum)", #geolocation
           "state (state_province)", "country (country)", #region
           "observation_date (YYYY-MM-DD)", #observation_date
           "profile_zero_ref (profile_zero_ref)")  |> #pull for use in layer.df
    #remove 75 651 profiles. Work with the current NRCS data download instead
    filter(!str_detect(dataset_name_sub, 'NRCS')) |>
    #correct the observation, some are entered in as years an others 
    #...as days since origin (excel convention)
    mutate(date_number = as.numeric(`observation_date (YYYY-MM-DD)`)) |>
    mutate(observation_date_formula = 
             case_when(is.na(date_number) ~ 'missing',
                       date_number > 2015 ~ 'days since origin',
                       date_number <= 2015 ~ 'year CE',
                       .default = 'default') |>
             as.factor(),
           observation_date = 
             case_when(is.na(date_number) ~ NA_Date_,
                       #assign origin based on Excel/Access historical convention
                       date_number > 2015 ~ as_date(date_number, origin = ymd('1900-01-01')),
                       #process things with just the year specified using the truncated flag
                       date_number <= 2015 ~ ymd(`observation_date (YYYY-MM-DD)`, 
                                                 quiet = TRUE, truncated = 2),
                       .default = NA_Date_)) |>
    #set the geolocations to numerics
    mutate(across(c(`lat (dec. deg)`, `long (dec. deg)`), .fns = as.numeric)) |>
    #round to 4 digits of precision or about 10 m https://wiki.openstreetmap.org/wiki/Precision_of_coordinates
    #... many numbers look overly precise and there was likely numerical error
    #....introduced during initial processing (excel/access exports?)
    mutate(across(c(`lat (dec. deg)`, `long (dec. deg)`), 
                  .fns = ~ round(.x, digits = precisions.ls$latLong_digits))) |>
    mutate(across(everything(), as.character)) |>
    mutate(from_source = 'Table Profile')
  
  site_location.df <- site_org.df |>
    select(dataset_name = dataset_name_sub,
           site_name, profile_name, from_source,
           latitude = `lat (dec. deg)`, longitude = `long (dec. deg)`,
           datum = `datum (datum)`) |>
    mutate(unit = 'decimal degrees') |>
    pivot_longer(cols = c(latitude, longitude, datum, unit),
                 names_to = 'is_type', values_to = 'with_entry',
                 values_drop_na = TRUE) |>
    mutate(of_variable = 'geolocation')
  
  site_time.df <- site_org.df |>
    select(dataset_name = dataset_name_sub,
           site_name, profile_name, from_source,
           with_entry = observation_date) |>
    mutate(of_variable = 'observation_date',
           is_type = 'date')
  
  site_region.df <- site_org.df |>
    select(dataset_name = dataset_name_sub,
           site_name, profile_name, from_source,
           state = `state (state_province)`, country = `country (country)`) |>
    pivot_longer(cols = c(state, country),
                 names_to = 'is_type', values_to = 'with_entry',
                 values_drop_na = TRUE) |>
    mutate(of_variable = 'georegion')
  
  
  ### Layer preprocessing
  
  layer.df <- data.lvl0.ls$data$TableLayer$primary |>
    select(#"ISCN 1-1 (2015-12-10)", 
      "dataset_name_sub", "dataset_name_soc", 
      "site_name", "profile_name", 
      #"lat (dec. deg)", "long (dec. deg)", "state (state_province)", "country (country)", "observation_date (YYYY-MM-DD)", #use location information from the site not layer
      "layer_name", "layer_top (cm)", "layer_bot (cm)", "layer_note", 
      #"hzn_desgn_other", "hzn", "hzn_desgn", "color",
      #"vegclass_local", 
      #"soil_taxon", "soil_series", 
      "bd_method", "bd_samp (g cm-3)", "bd_tot (g cm-3)", "bd_whole (g cm-3)", "bd_other (g cm-3)", "bdNRCS_prep_code", 
      "cNRCS_prep_code", "c_method", "c_tot (percent)", "oc (percent)", "loi (percent)", 
      #"n_tot (percent)", "c_to_n (mass ratio)", 
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
    #remove 373 326 layers. Work with the current NRCS data download instead
    filter(!str_detect(dataset_name_sub, 'NRCS')) |> #72503 obs of 25 variables
    #remove columns that are empty because some of these are NRCS only observations
    filter_out(if_all(everything(), is.na)) |> #does not remove columns right now but might if the variables were expanded
    #enforce precision
    mutate(across(c(`layer_top (cm)`, `layer_bot (cm)`), 
                  .fns= ~ as.numeric(.x, digits = precisions.ls$depth_digits) )) |>
    #Pull in the zero reference from the site table
    left_join(site_org.df |>
                select(dataset_name_sub, site_name, profile_name,
                       `profile_zero_ref (profile_zero_ref)`) |>
                unique(),
              by = join_by(dataset_name_sub, site_name, profile_name)) |>
    #Construct the layer ID from the rounded depths and 
    #...add in the zero reference if it exists. Some layer_names were mangled.
    mutate(layer_id = case_when( 
      !is.na(`profile_zero_ref (profile_zero_ref)`) &
        !is.na(`layer_top (cm)` + `layer_bot (cm)`) ~
        sprintf('layer (zero-ref:%s) [%d,%d]',
                `profile_zero_ref (profile_zero_ref)`,
                `layer_top (cm)`,`layer_bot (cm)`),
      is.na(`profile_zero_ref (profile_zero_ref)`) &
        !is.na(`layer_top (cm)` + `layer_bot (cm)`) ~ 
        sprintf('layer [%d,%d]', `layer_top (cm)`, `layer_bot (cm)`),
      !is.na(`profile_zero_ref (profile_zero_ref)`) &
        is.na(`layer_top (cm)` + `layer_bot (cm)`) ~
        sprintf('layer (zero-ref:%s)',
                `profile_zero_ref (profile_zero_ref)`),
      is.na(`profile_zero_ref (profile_zero_ref)`) &
        is.na(`layer_top (cm)` + `layer_bot (cm)`) ~ NA_character_,
      .default = 'missing'
    )) |> 
    #create a row number, organic carbon causes the sample ids to duplicate with 
    #... multiple methods for the same variable. Group these with a row number.
    mutate(row_id = paste('R', 1:n()),
           from_source = 'Table Layer')

  ####Primary processing for level 1 ####

  #Layer depth
  depth.df <- layer.df  |>
    select(layer_id, from_source,
           upper = `layer_top (cm)`, lower = `layer_bot (cm)`,
           zero_ref = `profile_zero_ref (profile_zero_ref)`) |> #72 503 obs
    filter(!is.na(upper + lower)) |> #59 895 obs
    mutate(of_variable = 'depth_interval',
           unit = 'cm',
           across(.cols = c(upper, lower), .fns = as.character)) |>
    pivot_longer(cols = c(upper, lower, zero_ref, unit),
                 names_to = 'is_type', values_to = 'with_entry',
                 values_drop_na = TRUE) |>
    #correct for the zero reference being pulled form the profile table
    mutate(from_source = if_else(is_type == 'zero_ref',
                                 'Table Profile', from_source))
  
  #Organic matter
  organic_fraction.df <- layer.df  |>
    select(dataset_name = dataset_name_sub,
           site_name, profile_name, layer_name,
           layer_id, row_id, from_source,
           `loi (percent)`)  |>
    filter(!is.na(`loi (percent)`)) |>  #938 obs
    #remove excel export errors
    mutate(value = sprintf(paste0('%.', precisions.ls$carbonFraction_digits, 'f'),
                           as.numeric(`loi (percent)`))) |>
    mutate(of_variable = 'organic_fraction',
           unit = 'mass-percent',
           method = 'loss on ignition from fine earth',
           column_id = 'loi (percent)') |>
    select(dataset_name,
           site_name, profile_name, layer_name,
           layer_id, row_id, column_id, from_source,
           of_variable, value, unit, method) |>
    pivot_longer(cols = c(value, unit, method),
                 names_to = 'is_type', values_to = 'with_entry',
                 values_drop_na = TRUE) #|>
  #check that rows are uniquely identified
  #pivot_wider(names_from = 'is_type', values_from = 'with_entry')
  
  #### Carbon fraction map ####
  #### With the removal of the NRCS data the cNRCS_prep_code is all missing
  #### thus not included here
  carbonFraction_map <- tribble(~column_id,~c_method, ~what, ~how,
                                "oc (percent)","MIR", 'carbon_fraction:organic', 'formula:MIR',
                                "c_tot (percent)","dry combustion, LECO analyzer", 'carbon_fraction:total', 'dry combustion:elemental analyzer',
                                "c_tot (percent)",NA, 'carbon_fraction:total', NA,
                                "c_tot (percent)","dry combustion",'carbon_fraction:total', 'dry combustion',
                                "c_tot (percent)","Perkin Elmer CHN (model 2400).", 'carbon_fraction:total', 'elemental analyzer',
                                "c_tot (percent)","M", 'carbon_fraction:total', NA,
                                "c_tot (percent)","G", 'carbon_fraction:total', NA,
                                "oc (percent)","L", 'carbon_fraction:organic', NA,
                                "oc (percent)","A", 'carbon_fraction:organic', NA,
                                "oc (percent)","T", 'carbon_fraction:organic', NA,
                                "oc (percent)","wet oxidation", 'carbon_fraction:organic', 'wet oxidation',
                                "caco3 (percent)","dry combustion", 'carbon_fraction:inorganic:CaCO3', 'dry combustion',
                                "caco3 (percent)",NA, 'carbon_fraction:inorganic:CaCO3', NA,
                                "oc (percent)","Determined total %C from high temp combustion + elemental analyzer (on <2mm fine earth fraction)", 'carbon_fraction:organic', 'dry combustion:elemental analyzer',
                                "c_tot (percent)","mean of ≥3 samples, dry combustion, Perkin-Elmer 2399",'carbon_fraction:total', 'mean:dry combustion:elemental analyzer',
                                "c_tot (percent)","mean of ≥3 samples, dry combustion, Perkin-Elmer 2400",'carbon_fraction:total', 'mean:dry combustion:elemental analyzer',
                                "c_tot (percent)","mean of ≥3 samples, dry combustion, Perkin-Elmer 2417",'carbon_fraction:total', 'mean:dry combustion:elemental analyzer',
                                "c_tot (percent)","mean of ≥3 samples, dry combustion, Perkin-Elmer 2418",'carbon_fraction:total', 'mean:dry combustion:elemental analyzer',
                                "c_tot (percent)","mean of ≥3 samples, dry combustion, Perkin-Elmer 2401",'carbon_fraction:total', 'mean:dry combustion:elemental analyzer',
                                "c_tot (percent)","mean of ≥3 samples, dry combustion, Perkin-Elmer 2402",'carbon_fraction:total', 'mean:dry combustion:elemental analyzer',
                                "c_tot (percent)","mean of ≥3 samples, dry combustion, Perkin-Elmer 2419",'carbon_fraction:total', 'mean:dry combustion:elemental analyzer',
                                "c_tot (percent)","mean of ≥3 samples, dry combustion, Perkin-Elmer 2420",'carbon_fraction:total', 'mean:dry combustion:elemental analyzer',
                                "c_tot (percent)","mean of ≥3 samples, dry combustion, Perkin-Elmer 2405",'carbon_fraction:total', 'mean:dry combustion:elemental analyzer',
                                "c_tot (percent)","mean of ≥3 samples, dry combustion, Perkin-Elmer 2406",'carbon_fraction:total', 'mean:dry combustion:elemental analyzer',
                                "c_tot (percent)","mean of ≥3 samples, dry combustion, Perkin-Elmer 2423",'carbon_fraction:total', 'mean:dry combustion:elemental analyzer',
                                "c_tot (percent)","mean of ≥3 samples, dry combustion, Perkin-Elmer 2424",'carbon_fraction:total', 'mean:dry combustion:elemental analyzer',
                                "c_tot (percent)","mean of ≥3 samples, dry combustion, Perkin-Elmer 2407",'carbon_fraction:total', 'mean:dry combustion:elemental analyzer',
                                "c_tot (percent)","mean of ≥3 samples, dry combustion, Perkin-Elmer 2408",'carbon_fraction:total', 'mean:dry combustion:elemental analyzer',
                                "c_tot (percent)","mean of ≥3 samples, dry combustion, Perkin-Elmer 2425",'carbon_fraction:total', 'mean:dry combustion:elemental analyzer',
                                "c_tot (percent)","mean of ≥3 samples, dry combustion, Perkin-Elmer 2426",'carbon_fraction:total', 'mean:dry combustion:elemental analyzer',
                                "c_tot (percent)","mean of ≥3 samples, dry combustion, Perkin-Elmer 2411",'carbon_fraction:total', 'mean:dry combustion:elemental analyzer',
                                "c_tot (percent)","mean of ≥3 samples, dry combustion, Perkin-Elmer 2412",'carbon_fraction:total', 'mean:dry combustion:elemental analyzer',
                                "c_tot (percent)","mean of ≥3 samples, dry combustion, Perkin-Elmer 2429",'carbon_fraction:total', 'mean:dry combustion:elemental analyzer',
                                "c_tot (percent)","mean of ≥3 samples, dry combustion, Perkin-Elmer 2430",'carbon_fraction:total', 'mean:dry combustion:elemental analyzer',
                                "c_tot (percent)","mean of ≥3 samples, dry combustion, Perkin-Elmer 2415",'carbon_fraction:total', 'mean:dry combustion:elemental analyzer',
                                "c_tot (percent)","mean of ≥3 samples, dry combustion, Perkin-Elmer 2416",'carbon_fraction:total', 'mean:dry combustion:elemental analyzer',
                                "c_tot (percent)","mean of ≥3 samples, dry combustion, Perkin-Elmer 2433",'carbon_fraction:total', 'mean:dry combustion:elemental analyzer',
                                "c_tot (percent)","mean of ≥3 samples, dry combustion, Perkin-Elmer 2434",'carbon_fraction:total', 'mean:dry combustion:elemental analyzer',
                                "c_tot (percent)","mean of ≥3 samples, dry combustion, Perkin-Elmer 2403",'carbon_fraction:total', 'mean:dry combustion:elemental analyzer',
                                "c_tot (percent)","mean of ≥3 samples, dry combustion, Perkin-Elmer 2404",'carbon_fraction:total', 'mean:dry combustion:elemental analyzer',
                                "c_tot (percent)","mean of ≥3 samples, dry combustion, Perkin-Elmer 2421",'carbon_fraction:total', 'mean:dry combustion:elemental analyzer',
                                "c_tot (percent)","mean of ≥3 samples, dry combustion, Perkin-Elmer 2422",'carbon_fraction:total', 'mean:dry combustion:elemental analyzer',
                                "c_tot (percent)","mean of ≥3 samples, dry combustion, Perkin-Elmer 2409",'carbon_fraction:total', 'mean:dry combustion:elemental analyzer',
                                "c_tot (percent)","mean of ≥3 samples, dry combustion, Perkin-Elmer 2410",'carbon_fraction:total', 'mean:dry combustion:elemental analyzer',
                                "c_tot (percent)","mean of ≥3 samples, dry combustion, Perkin-Elmer 2427",'carbon_fraction:total', 'mean:dry combustion:elemental analyzer',
                                "c_tot (percent)","mean of ≥3 samples, dry combustion, Perkin-Elmer 2428",'carbon_fraction:total', 'mean:dry combustion:elemental analyzer',
                                "c_tot (percent)","mean of ≥3 samples, dry combustion, Perkin-Elmer 2413",'carbon_fraction:total', 'mean:dry combustion:elemental analyzer',
                                "c_tot (percent)","mean of ≥3 samples, dry combustion, Perkin-Elmer 2414",'carbon_fraction:total', 'mean:dry combustion:elemental analyzer',
                                "c_tot (percent)","mean of ≥3 samples, dry combustion, Perkin-Elmer 2431",'carbon_fraction:total', 'mean:dry combustion:elemental analyzer',
                                "c_tot (percent)","mean of ≥3 samples, dry combustion, Perkin-Elmer 2432",'carbon_fraction:total', 'mean:dry combustion:elemental analyzer',
                                "caco3 (percent)","A", 'carbon_fraction:inorganic:CaCO3', NA,
                                "c_tot (percent)","Leco TruSpec CHN (model 630-100-400).", 'carbon_fraction:total', 'elemental analyzer',
                                "oc (percent)","manometric", 'carbon_fraction:organic', 'manometric',
                                "c_tot (percent)","dry combustion (CHN) of oven-dried, root-free sample, <2mm fraction", 'carbon_fraction:total', 'dry combustion:elemental analyzer',
                                "c_tot (percent)","Element Analyzer.  Loss of Ignition", 'carbon_fraction:total', 'formula:loss on ignition:elemental analyzer',
                                "c_tot (percent)","Carlo Erba CN Elemental Analyzer.", 'carbon_fraction:total', 'elemental analyzer',
                                "caco3 (percent)","wet oxidation",'carbon_fraction:inorganic:CaCO3', 'wet oxidation',
                                "oc (percent)",NA, 'carbon_fraction:organic', NA,
                                "oc (percent)","assumed to have no inorganic carbonate ; Combustion analyser 1200 deg C  (Manies et al, 2006)", 'carbon_fraction:organic', 'dry combustion:elemental analyzer:assumed no inorganics',
                                "oc (percent)","Fumigated in 12 M HCl (48 h) to remove carbonates (Hedges and Stern, 1984);.freeze-dried and kept in a desiccator; elemental analyzer (Carlo Erba 2500) used to determine C% and N% content (all samples were we have N% values).", 'carbon_fraction:organic', 'elemental analyzer:acid removal of inorganics',
                                "c_tot (percent)","assumed to have no inorganic carbonate ; Combustion analyser 1200 deg C  (Manies et al, 2006)", 'carbon_fraction:total', 'dry combustion:elemental analyzer',
                                "c_tot (percent)","all mineral samples fumigated in 12M HCl (48h) to remove carbonates (Hedges and Stern 1984) then run by combustion analyzer re Manies et al, 2006", 'carbon_fraction:total', 'elemental analyzer',
                                "oc (percent)","all mineral samples fumigated in 12M HCl (48h) to remove carbonates (Hedges and Stern 1984) then run by combustion analyzer re Manies et al, 2006", 'carbon_fraction:organic', 'elemental analyzer:acid removal of inorganics',
                                "oc (percent)","Site vegetation reflects pH; acidic vegetation sites do not have carbonates, but sites with non-tussock vegetation could also contain carbonates within the %C measurement reported here", 'carbon_fraction:total', NA,
                                "oc (percent)","8",'carbon_fraction:organic', NA,
                                "oc (percent)","Loss on Ignition (LOI, weight %) at 550° C (for 6 h) was used to determine organic content, and at 950° C (for 2h) to determine carbonate content (Dean, 1974; Heiri et al., 2001).  A 3rd order polynomial regression model based on individual soil samples where both LOI and C% (from elemental analyser) were available (n= 171, R2= 0.98) was used to predict C% (y) for the soil samples where only LOI% was available: y= (-0.00005x3)+(0.0059x2)+(0.362x)", 'carbon_fraction:organic', 'formula:loss on ignition')
  
  #### Process carbon fraction data####
  
  carbon_fraction.df <- layer.df  |>
    select(dataset_name = dataset_name_sub,
           site_name, profile_name, layer_name,
           layer_id, row_id, from_source,
           `oc (percent)`, `c_tot (percent)`, `caco3 (percent)`,
           #cNRCS_prep_code, #empty after the removal of NRCS data
           c_method)  |>
    pivot_longer(cols = contains('percent'),
                 names_to = 'column_id', values_to = 'value', 
                 values_drop_na = TRUE) |>
    # apply precision
    mutate(value = sprintf(paste0('%.', precisions.ls$carbonFraction_digits, 'f'),
                           as.numeric(value))
    ) |>
    # standardize methods
    left_join(carbonFraction_map,
              by = join_by(column_id, c_method)) |>
    mutate(of_variable = str_extract(what, pattern = '\\w+'),
           method = if_else(is.na(how),
                            str_remove(what, pattern = '\\w+:'),
                            paste(str_remove(what, pattern = '\\w+:'),
                                  how,sep = '-'))) |> #51 059 obs
    mutate(unit = 'mass-percent') |>
    select(dataset_name,
           site_name, profile_name, layer_name,
           layer_id, row_id, column_id, from_source,
           of_variable, unit, value, method) |>
    pivot_longer(cols = c(unit, value, method),
                 names_to = 'is_type', values_to = 'with_entry',
                 values_drop_na = TRUE) #|>
  #pivot_wider(names_from = 'is_type', values_from = 'with_entry')
  
  #### Create bulk density method map####
  # Map the methods to column names and bulk density 
  bulkDensity_methodMap <- tribble(~column_id, ~bd_method, ~bdNRCS_prep_code, ~what, ~how,
                                   "bd_samp (g cm-3)","bd_samp values are means of multiple observations",NA, 'bulk_density-fine_earth', 'measured:mean',
                                   "bd_other (g cm-3)","bd = dry weight/volume",NA, 'bulk_density-unspecified',NA,
                                   #TODO likely NRCS prep codes, G. and M. seem to be leaders for air dried prep
                                   "bd_tot (g cm-3)","G",NA, 'bulk_density-whole_soil', NA,
                                   "bd_tot (g cm-3)","M",NA, 'bulk_density-whole_soil', NA,
                                   "bd_other (g cm-3)","G",NA, 'bulk_density-unspecified', NA,
                                   "bd_other (g cm-3)","M",NA, 'bulk_density-unspecified', NA,
                                   "bd_samp (g cm-3)","measured as weight per unit volume",NA, 'bulk_density-fine_earth', NA,
                                   "bd_other (g cm-3)","predicted from %C",NA, 'bulk_density-unspecified', 'formula:carbon_fraction',
                                   "bd_other (g cm-3)","estimated from weight per unit area",NA, 'bulk_density-unspecified', 'estimated:weight per area',
                                   "bd_other (g cm-3)","gap-filled with value from nearby sample",NA, 'bulk_density-unspecified', 'gap_filled:neighboring_samples',
                                   "bd_samp (g cm-3)","calculated based on soil texture, rock fragment, and organic matter content",NA, 'bulk_density-fine_earth', 'formula:soil texture, rock fragment, organic matter',
                                   "bd_tot (g cm-3)","gap-filled value from re-measurement of same plot in 2000",NA, 'bulk_density-whole_soil', 'gap_filled:resampled',
                                   "bd_tot (g cm-3)","core volume/mass",NA, 'bulk_density-whole_soil', NA,
                                   "bd_samp (g cm-3)","Calculated as oven-dry mass of core, <2mm fraction, divided by core volume.",NA, 'bulk_density-stock', 'measured:fine dry mass over core volume',
                                   "bd_other (g cm-3)","bd = (1.45*0.111)/((loi/100)*1.45+(1-(loi/100))*0.111)",NA, 'bulk_density-unspecified', 'formula:organic_matter_fraction',
                                   "bd_other (g cm-3)","interpolated from adjacent horizons",NA, 'bulk_density-unspecified', 'gap_filled:neighboring horizons',
                                   "bd_tot (g cm-3)",NA,NA, 'bulk_density-whole_soil', NA,
                                   "bd_samp (g cm-3)",NA,NA, 'bulk_density-fine_earth', NA,
                                   "bd_other (g cm-3)","g of <2mm fraction divided by total volume of material (often includes roots)",NA, 'bulk_density-stock', 'measured:fine dry mass over core volume',
                                   "bd_other (g cm-3)","gap-filled from horizon type",NA, 'bulk_density-unspecified', 'gap_filled:neighboring_horizons',
                                   "bd_other (g cm-3)","BD was estimated based on C % using a power-based regression model based on individual soil samples where both C % (x) and DBD (y) were available (n= 831, R2= 0.73): y= 1.2559x-0.6831. In samples with permafrsot, the regression-derived BD was corrected for volumetric ice-content",NA, 'bulk_density-unspecified', 'formula:carbon_fraction',
                                   "bd_other (g cm-3)","took average of similar horizons types",NA, 'bulk_density-unspecified', 'gap_filled:similar horizons')
  
  #### Process bulk density ####
  # Process the data frame
  bulkdensity.df <- layer.df  |>  #72 503 obs
    select(dataset_name = dataset_name_sub,
           site_name, profile_name, layer_name,
           layer_id, row_id, from_source,
           "bd_samp (g cm-3)", "bd_tot (g cm-3)", "bd_whole (g cm-3)",
           "bd_other (g cm-3)",
           "bdNRCS_prep_code", "bd_method") |> #dataset_name keep for citation development
    #Cast the bulk density values, ensure they are positive (-999 flags present), 
    #... and impose a precision to clean up export issues
    mutate(across(.cols = ends_with('(g cm-3)'), .fns = as.numeric)) |>
    pivot_longer(cols = ends_with('(g cm-3)'), names_to = 'column_id',
                 values_to = 'value', values_drop_na = TRUE) |>
    filter(value >= 0) |> #ensure there are positive values
    #impose precision
    mutate(value = sprintf(paste0('%.', precisions.ls$carbonFraction_digits, 'f'),
                           value)) |>
    #Join the manual methods map
    left_join(bulkDensity_methodMap,
              by = join_by(bd_method, bdNRCS_prep_code, column_id)) |>
    mutate(of_variable = str_extract(what, pattern = '^\\w+'),
           method = if_else(is.na(how),
                            str_remove(what, pattern = '^\\w+-'),
                            paste(str_remove(what, pattern = '^\\w+-'), 
                                  how, sep = '-'))) |> #59 455 obs
    mutate(unit = 'g cm-3') |>
    select(dataset_name, site_name, profile_name, layer_name,
           layer_id, row_id, column_id, 
           from_source, of_variable, value, method, unit) |>
    pivot_longer(cols = c(value, method, unit), 
                 names_to = 'is_type', values_to = 'with_entry',
                 values_drop_na = TRUE) #|>
  #check that we have unique ids
  #pivot_wider(names_from = 'is_type', values_from = 'with_entry')
  
  ####Coarse fraction ####

  coarse_fraction.df <- layer.df  |>
    select(dataset_name = dataset_name_sub,
           site_name, profile_name, layer_name,
           layer_id, row_id, from_source,
           value = `wpg2 (percent)`, `wpg2_method`) |>
    filter(!is.na(value)) |> #16 509 obs
    # # apply precision
    mutate(value = sprintf(paste0('%.', precisions.ls$carbonFraction_digits, 'f'),
                           as.numeric(value))) |>
    # standardize methods
    mutate(of_variable = 'coarse_fraction',
           method = case_when(
             is.na(wpg2_method) ~ NA,
             wpg2_method == 'as observed in profile/field' ~ 'estimated:in field',
             wpg2_method == 'assumed' ~ 'estimated:assumed',
             wpg2_method == 'estimated' ~ 'estimated',
             wpg2_method == 'measured' ~ 'measured',
             wpg2_method == 'weight of >2mm fraction/weight of total sample' ~ NA,
             .default = '[bad coding STOP]'
           )) |> #16 509 obs
    mutate(unit = 'mass-percent',
           column_id = 'wpg2 (percent)') |>
    select(dataset_name, site_name, profile_name, layer_name, #provided ids
         layer_id, row_id, column_id,#constructed ids
         from_source, of_variable, value, unit, method) |>
    pivot_longer(cols = c(unit, value, method),
                 names_to = 'is_type', values_to = 'with_entry',
                 values_drop_na = TRUE) #|>
  #make sure we have unique identifers
  #pivot_wider(names_from = 'is_type', values_from = 'with_entry') #16509
  
  ### Construct level 1 ###
  
  data.lvl1.ls <- list(
    site = site_location.df |>
      bind_rows(site_region.df) |>
      bind_rows(site_time.df),
    layer = depth.df |>
      bind_rows(bulkdensity.df) |>
      bind_rows(coarse_fraction.df) |>
      bind_rows(carbon_fraction.df) |>
      #bind_rows(soc.df) |> #leave out due to no depth reporting with stock-area values
      bind_rows(organic_fraction.df),
    collection = collection.df,
    citations = c(data.lvl0.ls$citation$primary,
                  data.lvl0.ls$citation$contributed)
  )
  
  if(dataLevel == 'level1'){
    return(data.lvl1.ls)
  }
  
  stop("dataLevel option unknown")
}