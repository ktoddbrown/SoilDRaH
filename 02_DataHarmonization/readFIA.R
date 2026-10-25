#' Read function for FIA data
#' 
#' This function downloads the entire csv Forest Inventory Analysis Database from https://www.fs.usda.gov/research/products/dataandtools/tools/fia-datamart, unzips the file, reads in a project specific data annotation as well as the tables that have annotations. The data is either returned in the orginal table structure or a long format.
#'
#' @param dataDir a string that specifies the filepath of where the data should be downloaded.
#' @param dataLevel a string that specifies level0 or level1 data
#' @param verbose a boolean that prints out the stage of the file
#'
#' @importFrom readr read_csv col_character cols
#' @importFrom dplyr select rename mutate full_join filter join_by 
#' @importFrom tidyr pivot_longer unnest
#' @importFrom plyr ldply
#' @importFrom tidyselect one_of everything
#' @importFrom stringr str_remove
#' @importFrom bibtex read.bib
#' 
#'
#' @return a list containing the annotation data frame and the data that is annotated. Note that for this function we trim some of the data due to size and only return plot information related to the soil data.

readFIA <- function(dataDir, dataLevel = c('level0', 'level1')[1],
                    verbose = FALSE){
  
  # debug values
  # dataDir <- '01_DataRescue/FIA2024/'
  # verbose <- TRUE
  # dataLevel <- c('level0', 'level1')[1]
  #### Expected filenames ####
  
  # Download url for the entire FIA database
  downloadUrl <- "https://apps.fs.usda.gov/fia/datamart/CSV/CSV_FIADB_ENTIRE.zip"
  # name the zip file
  CSV_FIADB_ENTIRE_filename <- "CSV_FIADB_ENTIRE.zip"
  
  #Download data directory
  #Change this file to run locally for you
  #dataDir  <- 'temp'
  
  # The below csv tables are used to construct annotations for FIADB
  metaTranscriptions_filename <- tribble( 
    ~table_name, ~metadata_filename,
    'ENTIRE_PLOT', 'FIA2024_TableEntirePLOT.csv',
    'ENTIRE_SURVEY', 'FIA2024_TableSurvey.csv',
    'ENTIRE_SUBPLOT', 'FIA2024_TableSubPlot.csv',
    'ENTIRE_SUBP_SOIL_SAMPLE_LOC', 'FIA2024_TableSubPlot_Soil_Sample_Location.csv',
    'ENTIRE_SUBP_SOIL_SAMPLE_LAYER', 'FIA2024_TableSubplot_Soil_Sample_Layer.csv',
    'ENTIRE_TREE',  'FIA2024_TableTREE.csv'
  )
  
  #Pulled from nested table in the manual
  controlVocabulary <- 'FIA2024_Entire_Control_Vocabulary.csv'
  
  # Bibliography files
  primaryCitation.file <- 'FIA2024.bib'
  
  #### Download data ####
  
  if(!file.exists(file.path(dataDir, 'temp', CSV_FIADB_ENTIRE_filename))){
    if(verbose) message('Database not found, downloading...')
    if(verbose) warning('This is a large file and may require a manual download https://www.fs.usda.gov/research/products/dataandtools/tools/fia-datamart, select the `Entire FIADB CSV Archive` download under `Additional Download Options`.' )
    utils::download.file(url = downloadUrl, 
                         destfile = file.path(dataDir, 'temp',
                                              CSV_FIADB_ENTIRE_filename))
  }
  
  # Unzip zip file
  if(!file.exists(file.path(dataDir, 'temp', 'CSV_FIADB_ENTIRE'))){
    if(verbose) message('Unzipping file...')
    if(verbose) warning('This large file may require a manual unzip. Navigate to the data directory and unzip the file to a folder called `CSV_FIADB_ENTIRE`.')
    utils::unzip(file.path(dataDir, 'temp', CSV_FIADB_ENTIRE_filename), 
                 exdir = file.path(dataDir, 'temp'), overwrite = FALSE)
    if(verbose) message('Unzipping done.')
  }
  
  
  labMartTables <- list.files(path = file.path( dataDir, 'temp', 'CSV_FIADB_ENTIRE'), 
                              pattern = '.csv', full.names = TRUE)
  
  
  #### Read in transcribed meta data ####
  
  tableColumns <- plyr::ldply(labMartTables, 
                              function(filename){
                                tibble::tibble(column_name = 
                                                 names(readr::read_csv(filename, n_max = 0,
                                                                       show_col_types = FALSE)),
                                               table_name = stringr::str_remove(string = basename(filename),
                                                                                pattern = '.csv$'))
                              })
  
  transcriptions.df <- metaTranscriptions_filename |>
    reframe(meta = readr::read_csv(file = file.path(dataDir, metadata_filename),
                                   show_col_types = FALSE),
            .by = tidyselect::everything()) |>
    tidyr::unnest(meta) |>
    dplyr::rename(column_name = 'Column name (attribute)' ) |>
    dplyr::full_join(tableColumns |>
                       dplyr::filter(table_name %in% metaTranscriptions_filename$table_name),
                     by = dplyr::join_by(table_name, column_name))
  
  #pull relevent metadata
  meta.df <- transcriptions.df |>
    #commented out code below explores soil carbon variables across the tables
    # mutate(soil_obs = str_detect(column_names, pattern = 'BULK') |
    #          str_detect(column_names, pattern = '^C_.*_PCT$') |
    #          str_detect(column_names, pattern = 'COARSE_FRACTION_PCT'),
    #        depth = str_detect(column_names, pattern = '^LAYER_T(YPE)|(HICKNESS)') |
    #          str_detect(column_names, pattern = '^CORE') | #sample details
    #          str_detect(column_names, pattern = '^RESTRICTION_DEPTH_\\d'),
    #        time_obs =  str_detect(column_names, pattern = 'INVYR') | #obs time
    #          str_detect(column_names, pattern = 'MEAS'),
    #        location_obs = str_detect(column_names, pattern = 'LON') |
    #          str_detect(column_names, pattern = 'LAT') |
    #          str_detect(column_names, pattern = 'STATE') |
    #          str_detect(column_names, pattern = 'COUNTY'),
    #        ids = str_detect(column_names, pattern = 'CN$') |
    #          str_detect(column_names, pattern = 'NBR$') |
    #          str_detect(column_names, pattern = '_ID$')) |>
    # filter(soil_obs | depth | time_obs | location_obs | ids) |>
    # arrange(desc(soil_obs)) |>
    filter( table_name %in% c('ENTIRE_SUBP_SOIL_SAMPLE_LAYER', 
                              'ENTIRE_PLOT',
                              'ENTIRE_SURVEY'),
            column_name %in% c('CN', 'PLT_CN', 'SRV_CN',
                               'INVYR', 
                               'LAT', 'LON', 'STATE_CD', 'STATENM',
                               'LAYER_TYPE', 'LAYER_THICKNESS', 'LAYER_COLLECTED_CD',
                               'BULK_DENSITY', 'BULK_DENSITY_FINES',
                               'COARSE_FRACTION_PCT',
                               'C_ORG_PCT', 'C_INORG_PCT', 'C_TOTAL_PCT'))
  
  #### Read in downloaded primary data ####
  readCheck <- Inf #big tables to read in so develop code with subset
  
  if(verbose) message('reading FIA layer table')
  layer_data <- readr::read_csv(
    file = file.path(dataDir, 'temp',
                     'CSV_FIADB_ENTIRE/ENTIRE_SUBP_SOIL_SAMPLE_LAYER.csv'),
    col_types = readr::cols(.default = readr::col_character()),
    n_max = readCheck) |>
    dplyr::select(tidyselect::one_of(meta.df$column_name[meta.df$table_name == 'ENTIRE_SUBP_SOIL_SAMPLE_LAYER'])) 
  
  if(verbose) message('reading FIA plot table')
  plot_data <-  readr::read_csv(
    file = file.path(dataDir, 'temp',
                     'CSV_FIADB_ENTIRE/ENTIRE_PLOT.csv'),
    col_types = readr::cols(.default = readr::col_character()),
    n_max = readCheck
  ) |>
    dplyr::select(one_of(meta.df$column_name[meta.df$table_name == 'ENTIRE_PLOT'])) |>
    dplyr::rename(PLT_CN = CN) |> #2 023 534 obs
    dplyr::filter(PLT_CN %in% layer_data$PLT_CN) # 1 359 obs
  
  if(verbose) message('reading FIA survey table')
  survey_data <- readr::read_csv(
    file = file.path(dataDir, 'temp', 
                     'CSV_FIADB_ENTIRE/ENTIRE_SURVEY.csv'),
    col_types = readr::cols(.default = readr::col_character()),
    n_max = readCheck
  ) |>
    #need the RSCD to define the datum
    dplyr::select(SRV_CN = CN, RSCD, STATECD, STATENM) |>
    dplyr::filter(SRV_CN %in% plot_data$SRV_CN) |>
    unique()
  
  if(verbose) message('reading FIA control vocabulary table')
  cv.df <- readr::read_csv(
    file = file.path(dataDir, controlVocabulary),
    col_types = readr::cols(.default = readr::col_character())
  ) |>
    dplyr::rename(table_name = Table_ID, column_name = Name) |>
    dplyr::filter(column_name %in% meta.df$column_name)
  
  if(verbose) message('done with FIA level 0 table reads')
  level0.ls <- list(
    citation = bibtex::read.bib(file = file.path(dataDir, primaryCitation.file)),
    meta = meta.df,
    Table = list(ENTIRE_SUBP_SOIL_SAMPLE_LAYER = layer_data,
                 ENTIRE_SURVEY = survey_data,
                 ENTIRE_PLOT = plot_data),
    controlVocabulary = cv.df
  )
  
  if(dataLevel == 'level0'){
    return(level0.ls)
  }
  
  #### Create level 1#####
  
  if(verbose) message('constructing FIA level 1')
  meta.df <- level0.ls$meta |>
    dplyr::filter((table_name == 'ENTIRE_PLOT' & column_name %in% c('LAT', 'LON')) |
                    (table_name == 'ENTIRE_SUBP_SOIL_SAMPLE_LAYER' &
                       column_name %in% c('INVYR', 
                                          'LAYER_TYPE', #LAYER_THICKNESS, LAYER_COLLECTED_CD, 
                                          'BULK_DENSITY', 'BULK_DENSITY_FINES', 
                                          'COARSE_FRACTION_PCT', 'C_ORG_PCT', 'C_INORG_PCT', 'C_TOTAL_PCT'))) |>
    dplyr::select(column_name, description = Description, long_name = `Descriptive name`) |>
    dplyr::mutate(
      unit = dplyr::case_when(
        stringr::str_detect(column_name, 'BULK_DENSITY') ~ 'g cm-3',
        stringr::str_detect(column_name, '_PCT$') ~ 'mass percent',
        column_name %in% c('LAT', 'LON') ~ 'decimal degrees',
        .default = NA_character_),
      privacy_protcol = dplyr::if_else(column_name %in% c('LAT', 'LON'),
                                       '1 mile with 20% swap within county', NA_character_),
      from_source = 'FIADB User Guides Volume: Database Description (version 9.4)',
      of_variable = dplyr::case_when(
        column_name %in% c('LAT', 'LON', 'DATUM') ~ 'geolocation',
        column_name == 'INVYR' ~ 'observation_year',
        column_name == 'STATENM' ~ 'region',
        column_name %in% c('LAYER_TYPE', 'Depth_upper_cm', 'Depth_lower_cm') ~ 'layer',
        column_name == 'BULK_DENSITY' ~ 'bulk_density',
        column_name == 'BULK_DENSITY_FINES' ~ 'bulk_density_fine_earth',
        column_name == 'COARSE_FRACTION_PCT' ~ 'coarse_fraction',
        column_name == 'C_ORG_PCT' ~ 'carbon_organic',
        column_name == 'C_INORG_PCT' ~ 'carbon_inorganic',
        column_name == 'C_TOTAL_PCT' ~ 'carbon',
        .default = NA_character_)) |>
    tidyr::pivot_longer(cols = c(description, long_name, unit, privacy_protcol),
                        names_to = 'is_type', values_to = 'with_entry',
                        values_drop_na = TRUE)
  
  location.df <- level0.ls$Table$ENTIRE_PLOT |>
    dplyr::left_join(level0.ls$Table$ENTIRE_SURVEY,
                     by = dplyr::join_by(SRV_CN)) |>
    dplyr::mutate(DATUM = if_else(
      STATECD %in% as.character(c(60, 64, 66, 68, 69, 70)) & RSCD %in% c('26'),
      'WSG84', 'NAD83')) |>
    #drop the RSCD and STATECD now that the state name and data are merged
    dplyr::select(PLT_CN, #INVYR,#use the inventory year from the layer informatino
                  LAT, LON, DATUM, STATENM) |>
    unique() |>
    tidyr::pivot_longer(cols = c('LAT', 'LON', 'STATENM', 'DATUM'),
                        names_to = 'column_name', values_to = 'with_entry') |>
    dplyr::mutate(of_variable = 
                    dplyr::case_when(
                      column_name %in% c('LAT', 'LON', 'DATUM') ~ 'geolocation',
                      column_name == 'INVYR' ~ 'observation_year',
                      column_name == 'STATENM' ~ 'region',
                      .default = NA_character_),
                  is_type = 
                    dplyr::case_when(
                      column_name == 'LAT' ~ 'latitude',
                      column_name == 'LON' ~ 'longitude',
                      column_name == 'DATUM' ~ 'datum',
                      column_name == 'INVYR' ~ 'YYYY',
                      column_name == 'STATENM' ~ 'state',
                      .default = NA_character_))
  
  layer.df <- level0.ls$Table$ENTIRE_SUBP_SOIL_SAMPLE_LAYER |>
    dplyr::select(CN, PLT_CN, 
                  INVYR, 
                  LAYER_TYPE, #LAYER_THICKNESS, LAYER_COLLECTED_CD, 
                  BULK_DENSITY, BULK_DENSITY_FINES, 
                  COARSE_FRACTION_PCT, C_ORG_PCT, C_INORG_PCT, C_TOTAL_PCT) |>
    dplyr::mutate(Depth_upper_cm = str_extract(LAYER_TYPE, '\\d\\d?(?=-\\d{2,3}_CM)'),
                  Depth_lower_cm = str_extract(LAYER_TYPE, '\\d{2,3}(?=_CM)')) |>
    tidyr::pivot_longer(cols = c(INVYR, 
                                 LAYER_TYPE, #LAYER_THICKNESS, LAYER_COLLECTED_CD, 
                                 Depth_lower_cm, Depth_upper_cm,
                                 BULK_DENSITY, BULK_DENSITY_FINES, 
                                 COARSE_FRACTION_PCT, C_ORG_PCT, C_INORG_PCT, C_TOTAL_PCT),
                        names_to = 'column_name', values_to = 'with_entry',
                        values_drop_na = TRUE) |>
    dplyr::mutate(of_variable = 
                    dplyr::case_when(
                      column_name %in% c('LAYER_TYPE', 'Depth_upper_cm', 'Depth_lower_cm') ~ 'depth_interval',
                      column_name == 'INVYR' ~ 'observation_year',
                      column_name == 'BULK_DENSITY' ~ 'bulk_density',
                      column_name == 'BULK_DENSITY_FINES' ~ 'bulk_density_fine_earth',
                      column_name == 'COARSE_FRACTION_PCT' ~ 'coarse_fraction',
                      column_name == 'C_ORG_PCT' ~ 'carbon_organic',
                      column_name == 'C_INORG_PCT' ~ 'carbon_inorganic',
                      column_name == 'C_TOTAL_PCT' ~ 'carbon',
                      .default = NA_character_),
                  is_type = 
                    dplyr::case_when(
                      column_name %in% c('LAYER_TYPE') ~ 'type',
                      column_name %in% c('Depth_upper_cm') ~ 'upper',
                      column_name == 'Depth_lower_cm' ~ 'lower',
                      column_name %in% c('BULK_DENSITY', 'BULK_DENSITY_FINES', 
                                         'COARSE_FRACTION_PCT', 
                                         'C_ORG_PCT', 'C_INORG_PCT', 'C_TOTAL_PCT') ~ 'value',
                      column_name == 'INVYR' ~ 'YYYY',
                      .default = NA_character_))
  
  ## Pull everything together into by stacking the meta and primary data tables
  data.lvl1.ls <- list(
    meta = meta.df,
    site = location.df,
    layer = layer.df,
    citation = level0.ls$citation
  )
  
  if(verbose) message('done with FIA level 1')
  if(dataLevel == 'level1'){
    return(data.lvl1.ls)
  }

  stop('unknown dataLevel')
}
