#' Template for a read function
#' 
#' This is a template for a generic read function. 
#' Add the full citation and any data package url to this header information
#'
#' @param dataDir a character that specifies the filename of where the data should be downloaded.
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
#'
readTemplate <- function(dataDir,
                         format = c('orginal', 'long')[1],
                         verbose = TRUE){
  
  
  # Download datasets
  # this could be read in from annotations
  # downloadUrl.df <- tibble::tribble(~url, ~base_filename)
  
  # Test
  downloadUrl.df <- tibble::tribble(~url, ~base_filename, 
                                    "url1", "test_data_table1.csv", 
                                    "url2", "test_data_table2.csv")
  
  if(verbose) message('Starting download... ')
  for(rowIndex in 1:nrow(downloadUrl.df)){
    if(!file.exists(file.path(dataDir, 
                              downloadUrl.df$base_filename[rowIndex]))){
      utils::download.file(url = downloadUrl.df$url[rowIndex], 
                    destfile = file.path(dataDir, 
                                         downloadUrl.df$base_filename[rowIndex]))
    }
  }
  
  # Read in the original files
  if(verbose) message('Starting data read... ')
  
  ans.ls <- list()
  
  # TODO we assume destfiles are csvs, if not change line below to only read in data tables
  ans.ls$original_data <- lapply(file.path(dataDir, downloadUrl.df$base_filename), 
                                 FUN = read_csv, col_type = readr::cols(.default = readr::col_character()))
  names(ans.ls$original_data) = sub(".csv", "", downloadUrl.df$base_filename)
  
  # Generate annotations from metadata or script their creation
  # ans.ls$annotations <- SoilDRaH::templateAnnotations
  
  #Test
  ans.ls$annotations <- readr::read_csv(file.path(dataDir, "test_annotations.csv"),
                                 col_type = readr::cols(.default = readr::col_character()))
  
  if(verbose) message('done.')
  
  
  if(format == 'original'){
    return(ans.ls)
  }
  
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
      with_entry = dplyr::if_else((with_entry == "--") | is.na(with_entry), with_entry.data, with_entry)) %>%
    dplyr::select(-with_entry.data)
  
  if(verbose) message('done.')
  return(ans.ls)
}
