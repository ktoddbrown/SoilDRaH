#' Read function for FIA data
#' 
#' This function downloads the entire csv Forest Inventory Analysis Database from https://www.fs.usda.gov/research/products/dataandtools/tools/fia-datamart, unzips the file, reads in a project specific data annotation as well as the tables that have annotations. The data is either returned in the orginal table structure or a long format.
#'
#' @param dataDir a string that specifies the filepath of where the data should be downloaded.
#' @param annotationFilename a string that specifies where the data annotations are
#' @param format a string flat to return the original format or the long format.
#' @param verbose a boolean that prints out the stage of the file
#'
#' @importFrom readr read_csv col_character cols
#' @importFrom dplyr mutate full_join right_join rename_with filter join_by
#' @importFrom tidyr pivot_longer
#' 
#'
#' @return a list containing the annotation data frame and the data that is annotated. Note that for this function we trim some of the data due to size and only return plot information related to the soil data.

readFIA <- function(dataDir,
                    annotationFilename,
                    format = c('original', 'long')[1],
                    verbose = TRUE){
  
  if(verbose) warning('This function may have a high memory demand.')
  
  # Download entire FIA database
  downloadUrl <- "https://apps.fs.usda.gov/fia/datamart/CSV/CSV_FIADB_ENTIRE.zip"
  
  # What name is the downloaded file
  filename <- "CSV_FIADB_ENTIRE.zip"
  
  if(!file.exists(file.path(dataDir, filename))){
    if(verbose) message('Database not found, downloading...')
    if(verbose) warning('This is a large file and may require a manual download https://www.fs.usda.gov/research/products/dataandtools/tools/fia-datamart, select the `Entire FIADB CSV Archive` download under `Additional Download Options`.' )
    utils::download.file(url = downloadUrl, 
                         destfile = file.path(dataDir, 
                                              filename))
    if(verbose) message('Download done.')
  }
  
  # Unzip zip file
  if(!file.exists(file.path(dataDir, 'CSV_FIADB_ENTIRE'))){
    if(verbose) message('Unzipping file...')
    if(verbose) warning('This large file may require a manual unzip. Navigate to the data directory and unzip the file to a folder called `CSV_FIADB_ENTIRE`.')
    utils::unzip(file.path(dataDir, filename), exdir = dataDir, overwrite = FALSE)
    if(verbose) message('Unzipping done.')
  }
  
  # Read in annotations
  if(verbose) message('Loading annotations.')
  annotations.df <- readr::read_csv(annotationFilename,
                                        col_type = readr::cols(
                                          .default = readr::col_character()))
  
  # Get list of annotated table names
  tables <- unique(annotations.df$table_id)
  
  # Read in the original files
  if(verbose) message('Starting data read... ')
  
  # Read in csv's if annotated
  #... go through each of the tables that we have annotations for,
  #... load the entire table using read_csv as character reads (don't let R guess!)
  original_data <- lapply(file.path(dataDir, 
                                    'CSV_FIADB_ENTIRE', 
                                    paste(tables, ".csv", sep = "")), 
                          FUN = readr::read_csv, 
                          col_type = readr::cols(
                            .default = readr::col_character()))
  
  #make sure the names are set right
  names(original_data) <- tables
  
  if(verbose) message('done.')
  
  # if we are just loading the data, then return things here.
  if(format == 'original'){
    return(list(annotations = annotations.df, original = original_data))
  }
  
  # Move into a set of id-of_variable-is_type-with_entry long tables
  #... in this dataset the id is composed of all CN columns which are table specific unique identifiers, these are taken together to create a unique set of identifier across the tables. 
  #... in addition to the row, the column of the orginal data entry is recorded using the combined table-column name as an identifier.
  if(verbose) message('Transforming data to id(table, column, row) - with_entry... ')
  if(verbose) warning('this results in a >2Gb data object')
  
  # Notes from P2 guide
  # ENTIRE_PLOT::SRV_CN links to ENTIRE_SURVEY::CN
  # Notes from P3 guide
  # ENTIRE_SOILS_VISIT::PLT_CN links to ENTIRE_PLOT:CN
  # ENTIRE_SOILS_EROSION::PLT_CN links to ENTIRE_PLOT:CN
  # ENTIRE_SOILS_SAMPLE_LOC::PLT_CN links to ENTIRE_PLOT:CN
  # ENTIRE_SOILS_LAB::PLT_CN links to ENTIRE_PLOT:CN
  
  #CN is always a row identifier for each table but there is no assurance it is unique across the data set
  #Natural identifiers in each table are:
  # ENTIRE_PLOT => STATECD, INVYR, UNITCD, COUNTYCD, PLOT
  # ENTIRE_SOILS_VISIT => STATECD, INVYR, COUNTYCD, PLOT
  # ENTIRE_SOILS_EROSION => STATECD, INVYR, COUNTYCD, PLOT, SUBP
  # ENTIRE_SOILS_SAMPLE_LOC => STATECD, INVYR, COUNTYCD, PLOT, SMPLNNBR
  # ENTIRE_SOILS_LAB => STATECD, INVYR, COUNTYCD, PLOT, SMPLNNBR, LAYER_TYPE
  
  allData <- original_data$ENTIRE_PLOT %>%
    #rename the plot CN to match the foreign key in other tables
    dplyr::mutate(PLT_CN = CN) %>% 
    #add in the table name to columns
    dplyr::rename_with(.cols = !PLT_CN, ~paste0(.x, '.ENTIRE_PLOT')) %>%
    dplyr::right_join(original_data$ENTIRE_SOILS_VISIT %>% 
                 #add in the table name to columns
                 dplyr::rename_with(.cols = !PLT_CN, 
                             ~paste0(.x, '.ENTIRE_SOILS_VISIT')), 
               by = dplyr::join_by(PLT_CN)) %>%
    dplyr::full_join(original_data$ENTIRE_SOILS_SAMPLE_LOC %>% 
                #add in the table name to columns
                dplyr::rename_with(.cols = !PLT_CN, 
                            ~paste0(.x, '.ENTIRE_SOILS_SAMPLE_LOC')), 
              by = dplyr::join_by(PLT_CN)) %>%
    dplyr::full_join(original_data$ENTIRE_SOILS_EROSION %>%
                #add in the table name to columns
                dplyr::rename_with(.cols = !PLT_CN, 
                            ~paste0(.x, '.ENTIRE_SOILS_EROSION')), ,
              by = dplyr::join_by(PLT_CN),
              relationship = "many-to-many") %>%
    dplyr::full_join(original_data$ENTIRE_SOILS_LAB %>%
                #add in the table name to columns
                dplyr::rename_with(.cols = !PLT_CN, 
                            ~paste0(.x, '.ENTIRE_SOILS_LAB')), ,
              by = dplyr::join_by(PLT_CN),
              relationship = "many-to-many") %>%
    dplyr::filter(!is.na(C_ORG_PCT.ENTIRE_SOILS_LAB)) %>%
    #before pivot => 0.4 Gb
    #slice_head(n=100) %>%
    #make everything long
    tidyr::pivot_longer(cols = !c(starts_with('CN.'), PLT_CN), 
                 values_drop_na = TRUE,
                 names_to = c('column_id', 'table_id'),
                 names_sep = '\\.',
                 names_transform = as.factor,
                 values_to = 'with_entry')
  #after pivot => 2.1 Gb
  
  if(verbose) message('done.')
  
  return(list(annotations = annotations.df, long = allData))
}