#' Read in the NCSS database
#' 
#' This function reads in the NCSS sql database from the USDA-NRCS, Go here and download the sqlite zip file. https://ncsslabdatamart.sc.egov.usda.gov/database_download.aspx and place it in the data directory.
#'
#' @param dataDir file location for the data folder
#' @param annotationFilename file location of the annotations file
#' @param format a flag for the returning data format
#' @param verbose a flag to return lots of debug statements
#'
#' @return a list of either the original tables or meta data and long primary data
#' @export
#'
#' @importFrom RSQLite dbConnect   dbListTables
#' @importFrom dplyr filter select mutate full_join
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect everything all_of
#'

readNCSS <- function(dataDir,
                     annotationFilename,
                     #Want to move format into pivotBind or joinPivot and away from long as an option
                     format = c('original', 'long')[1],
                     verbose = TRUE){
  
  ##Dev variables
  # format <- 'long'
  # dataDir <- "temp/NRCS_NCSS_20230922/" # debugging
  # dataDir <- '~/Dropbox (UFL)/Research/Datasets/NRCS_NCSS_20230922'
  # annotationFilename <- "data/NCSS_Annotations.csv" #debugging
   
  #### Set up the file names ####
  # Set the file paths for the NCSS SQL database and all of its metadata
  sqldb_filename <- file.path(dataDir,'ncss_labdata.sqlite')
  
  metadata_dir <- file.path(dataDir,'FederalGeographicDataCommitteeMetadata')
  
  metadata_filenames <- lapply(list(col_desc = "NCSS Columns Description.csv", 
                                    table_desc = "NCSS Table Description.csv", 
                                    table_relation = "Relationships.csv", 
                                    table_unique = "Unique Constraints.csv"),
                               function(xx) file.path(metadata_dir, xx))
  
  
  #### Read in the annotations file ####
  # Read in the annotations file for NCSS that the Todd-Brown lab generated 
  annotations.df <- readr::read_delim(annotationFilename, delim = ';',
                               col_types = readr::cols(.default = readr::col_character()))
  
  #### Check downloads ####
  # Verify that the NCSS SQL database file exist in the proper directory, otherwise prompt the user to download it and store it in the proper directory
  if(!file.exists(sqldb_filename)){
    message(paste('Expected zip file not found. Go here and download the sqlite zip file. https://ncsslabdatamart.sc.egov.usda.gov/database_download.aspx and place it in the directory:', file.path(getwd(), dataDir )))
  }
  # Verify that the NCSS metadata files exist in the proper directory, otherwise prompt the user to download them and store them in the proper directory
  if(any(!file.exists(unlist(metadata_filenames)))){
    message(paste('Expected data description files not found. Go here [https://ncsslabdatamart.sc.egov.usda.gov/database_download.aspx], scroll to the bottom of page and select export csv for the following tables [', paste(basename(unlist(metadata_filenames)), collapse = ', '), ']. Place them in [', file.path(getwd(), dataDir ), '].'))
  }
  
  #### Connect the SQL database ####
  myconnect <- RSQLite::dbConnect(drv = RSQLite::SQLite(), dbname = sqldb_filename)
  
  # Useful relationship tables
  #relationship.df <-  read_csv(metadata_filenames$table_relation)
  #relationship.df

  if(format == 'original'){
    # For every unique table_id's in this lab's generated annotations that is also present as a name of one of the SQL data tables, append it to the annotatedTables char
    annotatedTables <- annotations.df$table_id |>
      unique() |>
      #only read in annotations that are there, some annotated tables provided by NRCS are 'meta'
      intersect(dbListTables(myconnect))

    #tableNames <- dbListTables(myconnect)
    
    # For each value in annotatedTables, read in the data from the correspondingly named data table in the SQL database
    if(verbose) message('Reading in all tables, this takes some time and results in 6.4 GB data object')
    originalTables <- lapply(setNames(object = as.list(annotatedTables), annotatedTables),
                            function(xx) {RSQLite::dbReadTable(myconnect, xx)})
    
    ### Clean up the connection ####
    RSQLite::dbDisconnect(myconnect)
    
    # Return a named list where the first name, annotations, refers to and accesses the lab's generated NCSS annotations; the second name, original, refers to and accesses the original formatted NCSS SQL data tables
    return(list(annotations = annotations.df, original = originalTables))
  }else{
    warning('This function currently only extracts tables associated with non-NA `of_variables` (currently layer-resolved soil organic carbon and geolocation).')
    
    # For each table_id in this lab's generated annotations, if at least one of the rows with said table_id has its of_variable's column cell value set to anything other than NA, keep all rows containing said table_id, 
    # else remove all rows containing said table_id; set the resulting table to reducedAnnotations
    reducedAnnotations <- annotations.df |>
      #Consider keeping the NA variables maybe?
      dplyr::filter(any(!is.na(of_variable)), .by = table_id)
    
    # For every unique table_id's in reducedAnnotations that is also present as a name of one of the SQL data tables, append it to the annotatedTables char
    annotatedTables <- reducedAnnotations$table_id |>
      unique() |>
      #Only process tables that have annotations and are in the sql table list
      #... NRCS has 'meta' tables that we don't care about
      intersect(RSQLite::dbListTables(myconnect))
    
    #tableNames <- dbListTables(myconnect)
    
    # For each value in annotatedTables, read in the data from the correspondingly named data table in the SQL database
    if(verbose) message('Reading in all tables, this takes some time and results in 1.3 GB data object')
    #TODO: expand to include `lab_analysis_procedure` to pull the references for the measurements in
    originalTables <- lapply(setNames(object = as.list(annotatedTables), annotatedTables),
                            function(xx) {
                              RSQLite::dbReadTable(myconnect, xx)
                              })
    
    ### Clean up the connection ####
    RSQLite::dbDisconnect(myconnect)
    
    ### Make the tables that we are interested in longer ###
    
    #Only these tables have been verified for the soc, obs time, and geolocation variables
    verified_tables <- list('lab_physical_properties' = 'lab_physical_properties',
               'lab_chemical_properties' = 'lab_chemical_properties',
               'lab_calculations_including_estimates_and_default_values' = 'lab_calculations_including_estimates_and_default_values', 
               lab_layer = 'lab_layer',
               lab_site = 'lab_site',
               lab_pedon = 'lab_pedon', 
               lab_combine_nasis_ncss = 'lab_combine_nasis_ncss')
    
    message("This function only reads the annotated information from the following tables:")
    message(paste(paste(names(verified_tables), collapse = ', '),
              'lab_area', sep = ', '))
    
    #link all the tables together
    key.df <- originalTables$lab_layer |>
                  select('site_key', 'pedon_key', 'layer_key') |>
      dplyr::mutate(across(everything(), as.character))
    
    #pull in the two tables to define the state/country
    #...control vocabulary style cross references here so processing separately
    #lab_area_crosswalk = 'lab_combine_nasis_ncss',
    #lab_area_key = 'lab_area'
    location_names <- originalTables$lab_combine_nasis_ncss |>
      select('site_key', 'pedon_key', #table ids
             "country_key", "state_key", 
             "county_key", "mlra_key", 
             "ssa_key", "npark_key", "nforest_key") |>
      #all of these are area_key which are unique in the lab_area table
      #...making this a long table for a join
      pivot_longer(cols = c("country_key", "state_key", 
                            "county_key", "mlra_key", 
                            "ssa_key", "npark_key", "nforest_key"),
                   names_to = 'column_id', values_to = 'area_key',
                   values_drop_na = TRUE) |>
      #joining the table
      left_join(originalTables$lab_area |>
                  mutate(area_type = #combine the area_type and sub_type
                           if_else(is.na(area_sub_type),
                                   area_type,
                                   paste0(area_type, '::', area_sub_type))) |>
                  select(area_key, area_name, area_type),
                by = join_by(area_key)) |>
      select(site_key, pedon_key,
             column_id,
             of_variable = area_type,
             with_entry = area_name) |>
      mutate(table_id = 'lab_area',
             is_type = 'value') |>
      mutate(across(everything(), as.character))
    
    # Get all of the data from the verified tables and pivot it long to be bound with the location_names table later
    ans.df <- plyr::ldply(verified_tables,
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
                            
                            temp.long <- originalTables[[tableName.str]] |>
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
    # temp.long <- originalTables[[tableName.str]] |>
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
    
    # Return a named list where the first name, annotations, refers to and accesses the lab's generated NCSS annotations; the second name, long, refers to and accesses the long formatted 
    # verified tables data and location names data from the NCSS SQL data tables
    return(list(annotations = annotations.df,
                long = ans.df |>
                  bind_rows(location_names)))
  }
  
  
  
}
