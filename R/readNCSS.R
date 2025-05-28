#' Read in the NCSS database
#' 
#' This data base works with the Web of Science hosted N
#' download the sqlite zip file. https://ncsslabdatamart.sc.egov.usda.gov/database_download.aspx and place it in the directory
#'
#' @param dataDir 
#' @param annotationFilename 
#' @param format 
#' @param verbose 
#'
#' @return
#' @export
#'
#' @importFrom RSQLite dbConnect SQLite dbListTables
#' @importFrom dplyr filter select mutate full_join
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect everything all_of
#'
readNCSS <- function(dataDir,
                     annotationFilename,
                     format = c('original', 'long')[1],
                     verbose = TRUE){
  
  ##Dev variables
  # format <- 'long'
  # dataDir <- "temp/NRCS_NCSS_20230922/" # debugging
  # annotationFilename <- "data/NCSS_Annotations.csv" #debugging
   
  #### Set up the file names ####
  sqldb_filename <- file.path(dataDir,'ncss_labdata.sqlite')
  
  metadata_dir <- file.path(dataDir,'FederalGeographicDataCommitteeMetadata')
  
  metadata_filenames <- lapply(list(col_desc = "NCSS Columns Description.csv", 
                                    table_desc = "NCSS Table Description.csv", 
                                    table_relation = "Relationships.csv", 
                                    table_unique = "Unique Constraints.csv"),
                               function(xx) file.path(metadata_dir, xx))
  
  
  #### Read in the annotations file ####
  annotations.df <- readr::read_delim(annotationFilename, delim = ';',
                               col_types = readr::cols(.default = readr::col_character()))
  
  #### Check downloads ####
  
  if(!file.exists(sqldb_filename)){
    message(paste('Expected zip file not found. Go here and download the sqlite zip file. https://ncsslabdatamart.sc.egov.usda.gov/database_download.aspx and place it in the directory:', file.path(getwd(), dataDir )))
  }
  
  if(any(!file.exists(unlist(metadata_filenames)))){
    message(paste('Expected data description files not found. Go here [https://ncsslabdatamart.sc.egov.usda.gov/database_download.aspx], scroll to the bottom of page and select export csv for the following tables [', paste(basename(unlist(metadata_filenames)), collapse = ', '), ']. Place them in [', file.path(getwd(), dataDir ), '].'))
  }
  
  #### Connect the SQL database ####
  
  myconnect <- RSQLite::dbConnect( drv = RSQLite::SQLite(), dbname = sqldb_filename)
  
  # Useful relationship tables
  #relationship.df <-  read_csv(metadata_filenames$table_relation)
  #relationship.df

  if(format == 'original'){
    annotatedTables <- annotations.df$table_id |>
      unique() |>
      #only read in annotations that are there, some annotated tables provided by NRCS are 'meta'
      intersect(dbListTables(myconnect))
    
    #tableNames <- dbListTables(myconnect)
    
    if(verbose) message('Reading in all tables, this takes some time and results in 6.4 GB data object')
    orginalTables <- lapply(setNames(object = as.list(annotatedTables), annotatedTables),
                            function(xx) {RSQLite::dbReadTable(myconnect, xx)})
    
    ### Clean up the connection ####
    RSQLite::dbDisconnect(myconnect)
    
    return(list(annotations = annotations.df, original = orginalTables))
  }else{
    warning('This function currently only extracts tables associated with non-NA `of_variables` (currently layer-resolved soil organic carbon and geolocation).')
    
    reducedAnnotations <- annotations.df |>
      dplyr::filter(any(!is.na(of_variable)), .by = table_id)
    
    annotatedTables <- reducedAnnotations$table_id |>
      unique() |>
      #only read in annotations that are there, some annotated tables provided by NRCS are 'meta'
      intersect(RSQLite::dbListTables(myconnect))
    
    #tableNames <- dbListTables(myconnect)
    
    if(verbose) message('Reading in all tables, this takes some time and results in 1.3 GB data object')
    orginalTables <- lapply(setNames(object = as.list(annotatedTables), annotatedTables),
                            function(xx) {
                              RSQLite::dbReadTable(myconnect, xx)
                              })
    
        
    ### Clean up the connection ####
    RSQLite::dbDisconnect(myconnect)
    
    ### Make the tables that we are interested in longer ###
    
    #Only thse tables have been verified for the soc, obs time, and geo-location variables
    verified_tables <- list('lab_physical_properties' = 'lab_physical_properties',
               'lab_chemical_properties' = 'lab_chemical_properties',
               'lab_calculations_including_estimates_and_default_values' = 'lab_calculations_including_estimates_and_default_values', 
               lab_layer = 'lab_layer',
               lab_site = 'lab_site',
               lab_pedon = 'lab_pedon',
               #country, state, county information below
               lab_area = 'lab_area',
               lab_combine_nasis_ncss = 'lab_combine_nasis_ncss')
    
    message("This function only reads the annotated information from the following tables:")
    message(paste(names(verified_tables), collapse = ', '))
    
    #link all the tables together
    key.df <- orginalTables$lab_layer |>
                  dplyr::select('site_key', 'pedon_key', 'layer_key') |>
      dplyr::mutate(
        dplyr::across(
          tidyselect::everything(), as.character))
    
     ident_columns <- reducedAnnotations |>
                              dplyr::filter(is_type  == 'identifier') |>
                              dplyr::select(table_id, column_id, of_variable)
    
     
    regional_area <- orginalTables$lab_combine_nasis_ncss
    
    ans.df <- plyr::ldply(
      #lab area is key'ed differently
      setdiff(verified_tables,c('lab_area', 'lab_combine_nasis_ncss')), 
                          function(tableName.str){
                            temp_key <- reducedAnnotations |>
                              dplyr::filter(table_id == tableName.str,
                                            with_entry == '--',
                                            !is.na(of_variable)) |>
                              dplyr::select(-with_entry) |>
                              dplyr::select(table_id, column_id, of_variable, is_type)
                            
                            non_id_columns <- temp_key |>
                              dplyr::filter(is_type != 'identifier') |>
                              dplyr::select(table_id, column_id, of_variable, is_type)
                            
                            ident_columns <- temp_key |>
                              dplyr::filter(is_type  == 'identifier') |>
                              dplyr::select(table_id, column_id, of_variable)
                            
                            temp.long <- orginalTables[[tableName.str]] |>
                              # only pull the columns that are annotated
                              dplyr::select(tidyselect::all_of(unique(temp_key$column_id))) |>
                              #make sure everything is a character so that we don't have type conflicts
                              dplyr::mutate(dplyr::across(.cols = everything(), .fns = as.character)) |>
                              # fill out the keys to link to other tables
                              left_join(key.df, 
                                        by = unique(ident_columns$column_id),
                                        relationship = "many-to-many") |>
                              # make anything not a row-id column long
                              tidyr::pivot_longer(cols = tidyselect::all_of(non_id_columns$column_id),
                                                  names_to = 'column_id', values_to = 'with_entry',
                                                  values_drop_na = TRUE) |>
                              #link in the meta data to pull in what variable and type is the datum
                              dplyr::full_join(non_id_columns, 
                                               by = join_by(column_id),
                                               relationship = "many-to-many")
                            return(temp.long)
                          }, .id = NULL)
    
    
    # # Debugging and dev code for the function above
    # tableName.str <- 'lab_layer'
    # 
    # temp_key <- reducedAnnotations |>
    #   dplyr::filter(table_id == tableName.str,
    #                 with_entry == '--') |>
    #   dplyr::select(-with_entry) |>
    #   dplyr::select(table_id, column_id, of_variable, is_type)
    # 
    # non_id_columns <- temp_key |>
    #   dplyr::filter(is_type != 'identifier') |>
    #   dplyr::select(table_id, column_id, of_variable, is_type)
    # 
    # ident_columns <- temp_key |>
    #   dplyr::filter(is_type  == 'identifier') |>
    #   dplyr::select(table_id, column_id, of_variable)
    # 
    # temp.long <- orginalTables[[tableName.str]] |>
    #   # only pull the columns that are annotated
    #   dplyr::select(tidyselect::all_of(unique(temp_key$column_id))) |>
    #   #make sure everything is a character so that we don't have type conflicts
    #   dplyr::mutate(dplyr::across(.cols = everything(), .fns = as.character)) |>
    #   # fill out the keys to link to other tables
    #   left_join(key.df, 
    #             by = unique(ident_columns$column_id),
    #             relationship = "many-to-many") |>
    #   # make anything not a row-id column long
    #   tidyr::pivot_longer(cols = tidyselect::all_of(non_id_columns$column_id),
    #                names_to = 'column_id', values_to = 'with_entry',
    #                values_drop_na = TRUE) |>
    #   #link in the meta data to pull in what variable and type is the datum
    #   dplyr::full_join(non_id_columns, 
    #                    by = join_by(column_id),
    #                    relationship = "many-to-many")
    # # # 

    
    return(list(annotations = annotations.df, long = ans.df))
  }
  
  
  
}