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
  if(verbose) message('Transforming data... ')
  
  dataEntryAnnotations <- ans.ls$annotations %>%
    filter(with_entry == '--' | is.na(with_entry)) %>%
    select(-with_entry)
  
  #Trim down the ENTIRE_PLOT to only include the soil sample locations
  #...do this to keep the size of the file down
  warning('Dropping plot information for plots not in the soils sample location table due to size')
  ans.ls$original_data$ENTIRE_PLOT <- ans.ls$original_data$ENTIRE_PLOT %>%
    semi_join(ans.ls$original_data$ENTIRE_SOILS_SAMPLE_LOC,
              by = join_by(STATECD, COUNTYCD, CN == PLT_CN))
  
  ##Start with lab data, add in sample location, erosion, visit, plot, survey
  
  # Pivot data into long format and merge with annotation information
  ans.ls$longtable <- plyr::ldply(.data = ans.ls$original_data, 
                                  .fun = function(x) {
    
    #check if row_number column already exists
    if("row_number" %in% colnames(x)) {
      warning("Replacing row_number with row order and using as a unique identifier.")
    }
    
    #set row numbers
    
    #pivot everything
    
    temp <- x %>%
      
      #give each row a number as unique identifier
      dplyr::mutate(row_number = 1:n()) 
    
    #pivot table for the entries
    entries.df <- tidyr::pivot_longer(cols = -c(row_number),
                                      names_to = 'column_id',
                                      values_to = 'with_entry', 
                                      values_drop_na = TRUE)
    
     id_cols <- dataEntryAnnotations %>%
       select(is_type == 'identifier')
     
    print(names(x))
    identifiers.df <- temp %>%
       select(row_number, one_of)
    
    return(entries.df)
  }, .id = "table_id") #%>%
    dplyr::full_join(dataEntryAnnotations,
                     by = join_by(table_id, column_id),
                     relationship = "many-to-many") %>%
    #join long table with annotations
    # dplyr::full_join(ans.ls$annotations, 
    #                  by = join_by(table_id, column_id),
    #                  suffix = c('.data', ''),
    #                  multiple = "all")%>%
    # #replace value placeholders in with_entry column with values from data
    # dplyr::mutate(
    #   with_entry = dplyr::if_else((with_entry == "--") | is.na(with_entry), 
    #                               with_entry.data, with_entry)) %>%
    # dplyr::select(-with_entry.data) %>%
    #without factors and integer casts: 406Mb
    #with casts: 299 Mb
    mutate(table_id = as.factor(table_id),
          column_id = as.factor(column_id),
          is_type = as.factor(is_type),
          row_number = as.integer(row_number))
  
  if(verbose) message('Removing orginal data.')
  ans.ls$original_data <- NULL
  
  if(verbose) message('done.')
  
  return(ans.ls)
}