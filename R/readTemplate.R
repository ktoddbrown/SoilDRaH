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
  
  # Read in the orgnial files
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
  
  if(verbose) message('done.')
  return(ans.ls)
}