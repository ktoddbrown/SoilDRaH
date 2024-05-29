#' Read function for FIA data
#' 
#'
#' @param dataDir a string that specifies the filepath of where the data should be downloaded.
#' @param annotationFilename a string that specifies where the data annotations are
#' @param format a string flat to return the original format or the long format.
#' @param verbose a boolean that prints out the stage of the file
#'
#' @importFrom readr read_csv col_character cols
#' @importFrom tibble tribble
#' @importFrom plyr ldply
#' @importFrom dplyr mutate full_join if_else select
#' @importFrom tidyr pivot_longer
#' 
#'
#' @return a list of data frames.

readFIA <- function(dataDir,
                    annotationFilename,
                    format = c('original', 'long')[1],
                    verbose = TRUE){
  
  if(verbose) warning('This function may have a high memory demand.')
  
  # Download entire FIA database
  downloadUrl <- "https://apps.fs.usda.gov/fia/datamart/CSV/CSV_FIADB_ENTIRE.zip"
  filename <- "CSV_FIADB_ENTIRE.zip"
  
  if(!file.exists(file.path(dataDir, filename))){
    if(verbose) message('Database not found, downloading...')
    utils::download.file(url = downloadUrl, 
                         destfile = file.path(dataDir, 
                                              filename))
    if(verbose) message('Download done.')
  }
  
  # Unzip zip file
  if(!file.exists(file.path(dataDir, 'CSV_FIADB_ENTIRE'))){
    if(verbose) message('Unzipping file...')
    utils::unzip(file.path(dataDir, filename), exdir = dataDir, overwrite = FALSE)
    if(verbose) message('Unzipping done.')
  }
  
  # Create return list
  ans.ls <- list()
  
  # Read in annotations
  if(verbose) message('Loading annotations.')
  ans.ls$annotations <- readr::read_csv(annotationFilename,
                                        col_type = readr::cols(.default = readr::col_character()))
  
  # Get list of annotated table names
  tables <- unique(ans.ls$annotations$table_id)
  
  # Read in the original files
  if(verbose) message('Starting data read... ')
  
  # Read in csv's if annotated
  ans.ls$original_data <- lapply(file.path(dataDir, 
                                           'CSV_FIADB_ENTIRE', paste(tables, ".csv", sep = "")), 
                                 FUN = readr::read_csv, 
                                 col_type = readr::cols(.default = readr::col_character()))
  names(ans.ls$original_data) <- tables
  
  
  if(verbose) message('done.')
  
  
  
  if(format == 'original'){
    return(ans.ls)
  }
  
  # Move into a set of id-of_variable-is_type-with_entry long tables
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
  
  allData <- ans.ls$original_data$ENTIRE_PLOT %>%
    #rename the plot CN to match the foreign key in other tables
    mutate(PLT_CN = CN) %>% 
    #add in the table name to columns
    rename_with(.cols = !PLT_CN, ~paste0(.x, '.ENTIRE_PLOT')) %>%
    right_join(ans.ls$original_data$ENTIRE_SOILS_VISIT %>% 
                 #add in the table name to columns
                 rename_with(.cols = !PLT_CN, 
                             ~paste0(.x, '.ENTIRE_SOILS_VISIT')), 
               by = join_by(PLT_CN)) %>%
    full_join(ans.ls$original_data$ENTIRE_SOILS_SAMPLE_LOC %>% 
                #add in the table name to columns
                rename_with(.cols = !PLT_CN, 
                            ~paste0(.x, '.ENTIRE_SOILS_SAMPLE_LOC')), 
              by = join_by(PLT_CN)) %>%
    full_join(ans.ls$original_data$ENTIRE_SOILS_EROSION %>%
                #add in the table name to columns
                rename_with(.cols = !PLT_CN, 
                            ~paste0(.x, '.ENTIRE_SOILS_EROSION')), ,
              by = join_by(PLT_CN),
              relationship = "many-to-many") %>%
    full_join(ans.ls$original_data$ENTIRE_SOILS_LAB %>%
                #add in the table name to columns
                rename_with(.cols = !PLT_CN, 
                            ~paste0(.x, '.ENTIRE_SOILS_LAB')), ,
              by = join_by(PLT_CN),
              relationship = "many-to-many") %>%
    filter(!is.na(C_ORG_PCT.ENTIRE_SOILS_LAB)) %>%
    #before pivot => 0.4 Gb
    #slice_head(n=100) %>%
    pivot_longer(cols = !c(starts_with('CN.'), PLT_CN), 
                 values_drop_na = TRUE,
                 names_to = c('column_id', 'table_id'),
                 names_sep = '\\.',
                 names_transform = as.factor,
                 values_to = 'with_entry')
  #after pivot => 2.1 Gb
  
  if(verbose) message('done.')
  
  return(list(annotations = ans.ls$annotations, long_data = allData))
}