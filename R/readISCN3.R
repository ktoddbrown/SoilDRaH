#' Load ISCN3
#'
#' This function first downloads the layer, profile, citation, and dataset tables from the International Soil Carbon Network version 3 Database into 4 distinct data frames.
#' It also loads in the annotation table for ISCN3 into a data frame.
#' Depending on the requested format (original or long) it changes the appropriate data frames into said format and then returns a named list the requested format and annotations.
#' 
#' Generally QA/QC is not done in this read function.
#' However there are data model relevant currents that need to be imposed in the long table
#'    1) layer names are expanded to include upper/lower interval bounds for two datasets which appear to have trailing zeros dropped from a digit identifier
#'    2) shared soc column_id between the profile and layer tables refer to the entire profile or layer level soc values respectively. We add a `profile` to the column_id to distinguish these columns.
#'
#' Database citation: Nave, L., K. Johnson, C. van Ingen, D. Agarwal, M. Humphrey, and N. Beekwilder. 2022. International Soil Carbon Network version 3 Database (ISCN3) ver 1. Environmental Data Initiative. https://doi.org/10.6073/pasta/cc751923c5576b95a6d6a227d5afe8ba (Accessed 2024-06-13).
#'
#' @param dataDir path to the folder containing data files.
#' @param verbose boolean flag denoting whether or not to print status messages
#' @param annotationFilename path to the annotation file (generally located in 'data' of the git repository)
#' @param format flag for the format to return the data 
#' 
#' @return list of annotation and data tables
#'
#' @importFrom data.table rbindlist
#' @importFrom tibble tribble cols col_character
#' @importFrom readr read_delim
#' @importFrom dplyr mutate bind_rows 
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect all_of
#' 
#' @export
#' 

readISCN3 <- function(dataDir,
                      annotationFilename,
                      format = c('original', 'long')[1],
                      verbose = TRUE){
  
  ### dev sets
  #dataDir <- '~/Dropbox (UFL)/Research/Datasets/ISCN3'
  #annotationFilename <- 'data/ISCN3Annotations.csv'
  
  ### construct file paths ####
  # verify that the user specified a file path
  if(is.null(dataDir)){
    stop('Data folder must be specified.')
  }
  
  ### Download the data ####
  # link the table name to the download url and file path(s) for data package
  download_table <- tibble::tribble(
    ~table, ~download_url, ~file_name, 
    'layer', "https://portal-s.edirepository.org/nis/dataviewer?packageid=edi.360.4&entityid=4af719a84f8981fcc63f1f92760cb253", file.path(dataDir, 'ISCN3_layer.csv'),
    'profile', "https://portal-s.edirepository.org/nis/dataviewer?packageid=edi.360.4&entityid=40527580cc045d33d9a5aaf728bf204e", file.path(dataDir, 'ISCN3_profile.csv'),
    'citation', "https://portal-s.edirepository.org/nis/dataviewer?packageid=edi.360.4&entityid=320e31ca911f187550ca2143c31fd408", file.path(dataDir, 'ISCN3_citation.csv'),
    'dataset', "https://portal-s.edirepository.org/nis/dataviewer?packageid=edi.360.4&entityid=cdd0c7a4cac3f28d6d788c91f506775f", file.path(dataDir, 'ISCN3_dataset.csv'),
    'control_vocabulary', "https://portal-s.edirepository.org/nis/dataviewer?packageid=edi.360.4&entityid=a8eef6e94b669b365e443c15d9402a03", file.path(dataDir, 'ISCNTranscribed_TemplateCVs.csv'),
    'template', "https://portal-s.edirepository.org/nis/dataviewer?packageid=edi.360.4&entityid=114c95dd318e088108158adc3ae4eb23", file.path(dataDir, 'ISCNtemplate.csv'),
    'other_pdfs', "https://portal-s.edirepository.org/nis/dataviewer?packageid=edi.360.4&entityid=eb320ae7b57296765f543cbb370b0f24", file.path(dataDir, 'TemplateCVs.pdf'),
    'other_pdfs', "https://portal-s.edirepository.org/nis/dataviewer?packageid=edi.360.4&entityid=b81a7f4214d176280a5ff4e0f0d52d8b", file.path(dataDir, 'TemplateSubmit.pdf'),
    'other_pdfs', "https://portal-s.edirepository.org/nis/dataviewer?packageid=edi.360.4&entityid=f914e6978c595c9a373dbc58365b6795", file.path(dataDir, 'Gen3-DB-documentation_REV.pdf'),
    'other_pdfs', "https://portal-s.edirepository.org/nis/dataviewer?packageid=edi.360.4&entityid=4dbc81eab612e09b84c688bb387d06c2", file.path(dataDir, 'ISCN-Database.pdf'),
    'other_Rmds', "https://portal-s.edirepository.org/nis/dataviewer?packageid=edi.360.4&entityid=3903927ae52655ff6359bc7c454aa42e", file.path(dataDir, 'C&QA.Rmd'))
  
  #One by one, download the data in each row from the download_table
  #... and store that in the appropriate file path location on your computer
  for(row_index in 1:nrow(download_table)){
    if(!file.exists(download_table$file_name[row_index])){
      if(verbose) print(paste('Download file:', 
                              download_table$file_name[row_index]))
      utils::download.file(download_table$download_url[row_index], 
                           download_table$file_name[row_index], 
                           quiet=!verbose)
    }
  }
  
  #### Read primary data files ####
  # Using the download_table specified above, pull the primary data tables by
  # Note that the tables are ';' separated and we do not let R guess the column
  # ...types because the sparse columns will read as falls logical columns.
  
  if(verbose) print('Read ISCN3 primary data.')
  
  orginalTables <- plyr::dlply(
    .data = download_table |>
      dplyr::filter(table %in% c('citation', 'dataset', 'profile', 'layer')), 
    .variables = c('table'),
    .fun = function(xx){
      return(
        readr::read_delim(file = xx$file_name, delim = ';', 
          col_types = readr::cols(.default = readr::col_character())))
    }
  )
  
  #### Read annotations ####
  # Read in the annotations data frame made that was generated for this project
  # ...In this case, this was done by hand.
  # ...Note that this is a comma delineated file and we, again, restrict the
  # ...column types to characters.
  if(verbose) print('Read in the annotations file generated by SoilDRaH.')
  annotations.df <- readr::read_delim(
    file = annotationFilename,
    delim = ',', 
    col_types = readr::cols(.default = readr::col_character()))
  
  #### Return orginal ####
  # If the orginal format is asked for, then stop here and return the primary
  # ... data tables, download documentation, and the annotations.
  if(format == 'original'){
    return(list(original = c(orginalTables, 
                               list(files = download_table)),
                annotation = annotations.df))
  }
  
  #### Modify annotations####
  ## Modify the annotations to reflect the changes to the profile columns
  
  annotations.df <- annotations.df |>
  #The profile and layer tables share top/bottom layer and soc values _but_
    #...they refer to different observations (profile summary vs layer).
  #...Append the table name before these columns
    dplyr::mutate(
      column_id = dplyr::if_else(column_id %in%
          c('layer_top (cm)', 'layer_bot (cm)', 
            'soc (g cm-2)', 'soc_method', 'soc_carbon_flag') &
            table_id == 'profile', 
          paste0('profile::', column_id), 
          column_id))
  
  #### Combine the profile+layer tables ####
  #The profile and layer tables share 'dataset_name_sub', 'dataset_name_soc', 
  #...'site_name', 'profile_name'. Merge these two tables first and then 
  #...shoe-string them 
  long.df <- orginalTables$profile |>
    #The profile and layer tables share top/bottom layer and soc values _but_
    #...they refer to different observations (profile summary vs layer).
    #...Append the table name before these columns
    dplyr::rename(`profile::layer_top (cm)` = `layer_top (cm)`,
           `profile::layer_bot (cm)` = `layer_bot (cm)`,
           `profile::soc (g cm-2)` = `soc (g cm-2)`,
           `profile::soc_method` = `soc_method`, 
           `profile::soc_carbon_flag` = soc_carbon_flag) |>
    dplyr::full_join(
      #join the tables together to get the profile information distributed
      orginalTables$layer,
      by = dplyr::join_by(
        `ISCN 1-1 (2015-12-10)`, 
        dataset_name_sub, dataset_name_soc, 
        `lat (dec. deg)`, `long (dec. deg)`, `datum (datum)`, 
        `state (state_province)`, `country (country)`,
        `observation_date (YYYY-MM-DD)`, site_name, profile_name, 
        soil_taxon, soil_series,
        vegclass_local, locator_parent_alias)) |>
        #Modify the layer_name for World Soils and Northern Circumpolar
        #... to correct for repeated id's
        #... this was probably caused by Excel dropping the tailing 0's form
        #... a digit-based identifier
        dplyr::mutate(
          layer_name = 
            dplyr::if_else(
              dataset_name_sub %in% 
                c('Worldwide soil carbon and nitrogen data', 
                  'Northern Circumpolar Soil Carbon Database (NCSCD)'),
              paste0(layer_name, '::interval ', 
                     `layer_top (cm)`, '-', 
                     `layer_bot (cm)`), layer_name)) |>
    tidyr::pivot_longer(cols = -c(dataset_name_sub, dataset_name_soc,
                                  site_name, profile_name, layer_name),
                        names_to = 'column_id',
                        values_to = 'with_entry',
                        values_drop_na = TRUE) |> #nrow 20 269 712
    #remove duplicates
    unique() #nrow 20 229 958
  
  
  #### Combine citation+dataset tables ####
  #The citation and the dataset share a common key 'dataset_name'
  #...in addition they are hybrid formats where each row is not
  #...uniquely identified. We first make these tables long and then
  #...stack them.
  dataset_name.df <- orginalTables$citation |>
    tidyr::pivot_longer(cols = -dataset_name,
                        names_to = 'column_id',
                        values_to = 'with_entry',
                        values_drop_na = TRUE) |>
    dplyr::bind_rows(
      orginalTables$dataset |>
        tidyr::pivot_longer(cols = -dataset_name,
                            names_to = 'column_id',
                            values_to = 'with_entry',
                            values_drop_na = TRUE) ) |>
    unique()
  
  #### Populate dataset information at layer level ####
  ##There are two foreign key dataset citations with both the sub and soc
  ##Cross references them here with the key columns from the profile+layer 
  ##tables
  
  ##Pull the unique identifiers to cross reference the citation and dataset
  # ... tables
  key.df <- long.df |>
    dplyr::select(dataset_name_soc, dataset_name_sub, site_name, profile_name,
                  layer_name) |>
    unique() #nrow 470 578
  
  dataset.df <- key.df |>
    dplyr::left_join(dataset_name.df,
                     by = dplyr::join_by(dataset_name_sub == dataset_name),
                     relationship = "many-to-many") |> #nrow 1 427 130
    dplyr::bind_rows( key.df |>
                        dplyr::left_join(
                          dataset_name.df,
                          by = dplyr::join_by(
                            dataset_name_soc == dataset_name),
                          relationship = "many-to-many")  #nrow 1 299 471
                      ) |>
    unique()
  
  ####Stack the two tables####
  ##Stack the data and minimize memory using factors
  ans.df <- dataset.df |>
    dplyr::bind_rows(long.df) |> #1.8 Gb
    dplyr::mutate(across(c(dataset_name_sub, dataset_name_soc, 
                           site_name, profile_name, layer_name, 
                           column_id), as.factor))
  #1.06 Gb; nrow 32 905 350
  
  # make sure that the user knows this is a lot of data
  if(verbose) message('Returning long data (>1Gb).')
  
  #### Return long ####
  
  return(list(long =  ans.df,
              annotation = annotations.df))
}