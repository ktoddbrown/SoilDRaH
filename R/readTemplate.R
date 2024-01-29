#' Template for a read function
#' 
#' This is a template for a generic read function. 
#' Add the full citation and any data package url to this header information
#'
#' @param dataDir a character that specifies the filename of where the data should be downloaded.
#' @param format a string flat to return the orgial format or the long format.
#' @param verbose a boolean that prints out the stage of the file
#'
#' @return a list of data frames.
#'
readTemplate <- function(dataDir,
                         format = c('orginal', 'long')[1],
                         verbose = TRUE){
  
  
  # Download datasets
  # this could be read in from annotations
  downloadUrl.df <- dplyr::tribble(~url, ~base_filename)
  
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
  
  # Generate annotations from metadata or script their creation
  ans.ls$annotation <- SoilDRaH::templateAnnotations
  
  if(verbose) message('done.')
  
  
  if(format == 'original'){
    return(ans.ls)
  }
  
  # Move into a set of id-of_variable-is_type-with_entry long tables
  if(verbose) message('Transforming data... ')
  
  ans$longtable <- plyr::ldply(.data = data.ls, .fun = function(x) {
    
    #check if row_number column already exists
    if("row_number" %in% colnames(x)) {
      warning("Replacing row_number with row order and using as a unique identifier.")
    }
    
    temp <- x %>%
      
      #convert all columns to character type
      mutate(across(.cols = everything(), .fns = as.character)) %>%
      
      #give each row a number as unique identifier
      ungroup() %>%
      mutate(row_number = 1:n()) %>%
      
      #pivot table longer
      pivot_longer(cols = -c(row_number), names_to = 'column_id',
                   values_to = 'with_entry', values_drop_na = TRUE)
    
    return(temp)
  }, .id = "table_id") %>%
    
    #join long table with annotations
    full_join(ans$annotation, 
              by = join_by(table_id, column_id),
              suffix = c('.data', ''),
              relationship = "many-to-many") %>%
    
    #replace value placeholders in with_entry column with values from data
    mutate(
      with_entry = if_else((with_entry == "--") | is.na(with_entry), with_entry.data, with_entry)) %>%
    select(-with_entry.data) %>%
    
    #remove rows with no row number
    drop_na(row_number)
  
  if(verbose) message('done.')
  return(ans.ls)
}