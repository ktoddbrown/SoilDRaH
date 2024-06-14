checkAnnotations <- function(filename){
  
  expectedHeaders <- c("table_id", #Table id should match the basename of the csv file
                       "column_id", #This should match the exact text of the headers
                       "of_variable", #variables should match how the orginal file talks about data. For example if bulk density is reported as both fine earth and whole the variable might be `fine_bulk_density` and `whole_bulk_densith`. Best practice is to use `_` to seperate words and lower case.
                       "is_type", #should be one of the control vocabulary below
                       "with_entry" #should be either the value/entry associated with this time and varible OR a `--` to indicate a reference to the described data table.
                       )
  
  data_ref <- '--' #string for reference to annotated data to appear in the `with_entry` column
  
  valid_is_type <- c('identifier',
                     'description',
                     'foreign_key',
                     'value',
                     'unit',
                     'method',
                     'note',
                     'control_vocabulary',
                     'standard_deviation')
  
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
