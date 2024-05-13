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
                    format = c('orginal', 'long')[1],
                    verbose = TRUE){
  
  
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
  
  # Read in csvs if annotated
  ans.ls$original_data <- lapply(file.path(dataDir, 
                                           'CSV_FIADB_ENTIRE', paste(tables, ".csv", sep = "")), 
                                 FUN = readr::read_csv, col_type = readr::cols(.default = readr::col_character()))
  names(ans.ls$original_data) <- tables
  
  
  if(verbose) message('done.')
  
  
  if(format == 'original'){
    return(ans.ls)
  }
  
  #remove too large table
  # TODO keep the table but remove rows that do not have soil data
  ans.ls$original_data$ENTIRE_PLOT <- NULL
  
  # Move into a set of id-of_variable-is_type-with_entry long tables
  if(verbose) message('Transforming data... ')
  
  # Pivot data into long format and merge with annotation information
  ans.ls$longtable <- plyr::ldply(.data = ans.ls$original_data, .fun = function(x) {
    
    #check if row_number column already exists
    if("row_number" %in% colnames(x)) {
      warning("Replacing row_number with row order and using as a unique identifier.")
    }
    
    temp <- x %>%
      
      #give each row a number as unique identifier
      dplyr::mutate(row_number = 1:n()) %>%
      
      #pivot table longer
      tidyr::pivot_longer(cols = -c(row_number), names_to = 'column_id',
                          values_to = 'with_entry', values_drop_na = TRUE)
    
    return(temp)
  }, .id = "table_id") %>%
    
    #join long table with annotations
    dplyr::full_join(ans.ls$annotations, 
                     by = join_by(table_id, column_id),
                     suffix = c('.data', ''),
                     relationship = "many-to-many") %>%
    
    #replace value placeholders in with_entry column with values from data
    dplyr::mutate(
      with_entry = dplyr::if_else((with_entry == "--") | is.na(with_entry), 
                                  with_entry.data, with_entry)) %>%
    dplyr::select(-with_entry.data) %>%
  drop_na(row_number)
  
  if(verbose) message('done.')
  ##TODO Take a look at data size. Maybe ONLY return the log table 
  ##... and make strings factors where we can.
  return(ans.ls)
}