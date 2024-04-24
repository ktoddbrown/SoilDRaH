checkAnnotations <- function(filename){
  
  expectedHeaders <- c("table_id", "column_id", "of_variable", "is_type", "with_entry" )
  
  valid_is_type <- c('identifier', 'value', 'unit', 'method', 'note',
                     'standard_deviation')
  
  annotation.df <- read_csv2(filename, 
                             col_types = cols(.default = col_character()))
  
  
  #check minimum names
  if(!all(expectedHeaders %in%
         names(annotation.df))){
    stop(paste('annotation file missing expected names: ', 
               setdiff(expectedHeaders, names(annotation.df))))
  }
  
  #check is_type elements
  if(!all(unique(annotation.df$is_type) %in% valid_is_type)){
    stop(paste('annotation file missing expected names: ', 
               setdiff(unique(annotation.df$is_type), valid_is_type)))
  }
  
}