checkAnnotations <- function(filename,
                             annotationsCheckFile = 'data/selfDocumentAnnotations.csv'){
  
  selfDoc <- readr::read_delim('data/selfDocumentAnnotations.csv', 
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
  annotation.df <- read_delim(filename, 
                             col_types = cols(.default = col_character()),
                             delim = ';')
  
  
  #check minimum names
  if(!all(expectedHeaders %in%
         names(annotation.df))){
    stop(paste('annotation file missing expected names: ', 
               paste0(setdiff(expectedHeaders, names(annotation.df)), collapse = ', ')))
  }
  
  #check is_type elements
  if(!all(unique(annotation.df$is_type) %in% valid_is_type)){
    stop(paste('annotation file contains invalid type: ', 
               setdiff(unique(annotation.df$is_type), valid_is_type)))
  }
  
  check_self_ref <- annotation.df %>%
    filter(with_entry == data_ref) %>%
    mutate(across(.cols = everything(),.fns = function(xx){
      xx[xx == data_ref] <- NA
      return(xx)
    }))
  
  #check that only table-columns have the '--'
  if((any(is.na(check_self_ref$column_id)))){
    stop('no reference to data table premitted without specifying the column_id')
  }
  
  return(0)
}
