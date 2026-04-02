#' Check annotations
#' 
#' This function checks that the annotations files adhere to self documented standards. 
#'
#' @param filename the annotation files being checked
#' @param annotationsCheckFile the standard annotation file to be checked against
#' @param annotationsDirectory the directory holding the annotation files
#'
#' @return 0 if everything checks out, otherwise throws an error.
#' @export
#' 
#' @importFrom dplyr mutate filter across everything
#' @importFrom readr read_delim cols col_character
#' @importFrom tidyr separate_longer_delim separate_wider_delim
#'
checkAnnotations <- function(filename,
                             annotationsCheckFile = 'selfDocumentAnnotations.csv',
                             annotationsDirectory = 'data'){
  
  selfDoc <- readr::read_delim(file.path(annotationsDirectory, 'selfDocumentAnnotations.csv'), 
                               delim = ';',
                               col_types = readr::cols(.default = readr::col_character()),)
  
  expectedHeaders <- selfDoc$column_id |>
    unique()
  
  data_ref <- '--' #string for reference to annotated data to appear in the `with_entry` column
  
  control_vocab.df <- selfDoc |>
    dplyr::filter(column_id == 'is_type', is_type == 'control_vocabulary') |>
    dplyr::select(with_entry) |>
    tidyr::separate_longer_delim(cols = with_entry, delim = ';') |>
    tidyr::separate_wider_delim(cols = with_entry, delim = '|', names = c('vocabulary', 'definition')) |>
    dplyr::mutate(dplyr::across(.cols = dplyr::everything(), .fns = trimws))
  
  valid_is_type <- control_vocab.df$vocabulary
  
  #assume ';' deliminator to be consistent with how R structures csv in data folders
  annotation.df <- readr::read_delim(file.path(annotationsDirectory, filename),
                             col_types = readr::cols(.default = readr::col_character()),
                             delim = ';')
  
  
  #check minimum names
  if(!all(expectedHeaders %in%
         names(annotation.df))){
    stop(paste('annotation file missing expected names: ', 
               paste0(setdiff(expectedHeaders, names(annotation.df)), collapse = ', ')))
  }
  
  #throw a warning that there are extra headers
  if(!all(names(annotation.df) %in%
          expectedHeaders)){
    warning(paste('annotation file has extra names: ', 
               paste0(setdiff(names(annotation.df), expectedHeaders), collapse = ', ')))
  }
  
  #check is_type elements
  if(!all(unique(annotation.df$is_type) %in% valid_is_type)){
    stop(paste('annotation file contains invalid type: ', 
               setdiff(unique(annotation.df$is_type), valid_is_type)))
  }
  
  check_self_ref <- annotation.df |>
    dplyr::filter(with_entry == data_ref) |>
    dplyr::mutate(across(.cols = dplyr::everything(),.fns = function(xx){
      xx[xx == data_ref] <- NA
      return(xx)
    }))
  
  #check that only table-columns have the '--'
  if((any(is.na(check_self_ref$column_id)))){
    stop('no reference to data table premitted without specifying the column_id')
  }
  
  return(0)
}
