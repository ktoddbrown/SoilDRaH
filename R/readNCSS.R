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
                     format = c('original', 'long')[1],
                     verbose = TRUE){
  
  ##Dev variables
  #format <- 'long'
  #dataDir <- "temp/NRCS_NCSS_20230922/" # debugging
  #annotationFilename <- "data/NCSS_Annotations.csv" #debugging
   
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
      dplyr::filter(any(!is.na(of_variable)), .by = table_id)
    
    # For every unique table_id's in reducedAnnotations that is also present as a name of one of the SQL data tables, append it to the annotatedTables char
    annotatedTables <- reducedAnnotations$table_id |>
      unique() |>
      #only read in annotations that are there, some annotated tables provided by NRCS are 'meta'
      intersect(RSQLite::dbListTables(myconnect))
    
    #tableNames <- dbListTables(myconnect)
    
    # For each value in annotatedTables, read in the data from the correspondingly named data table in the SQL database
    if(verbose) message('Reading in all tables, this takes some time and results in 1.3 GB data object')
    originalTables <- lapply(setNames(object = as.list(annotatedTables), annotatedTables),
                            function(xx) {RSQLite::dbReadTable(myconnect, xx)})
    
    ### Clean up the connection ####
    dbDisconnect(myconnect)
    
    ### Make the tables that we are interested in longer ###
    
    #Only these tables have been verified for the soc, obs time, and geolocation variables
    verified_tables <- list('lab_physical_properties' = 'lab_physical_properties',
               'lab_chemical_properties' = 'lab_chemical_properties',
               'lab_calculations_including_estimates_and_default_values' = 'lab_calculations_including_estimates_and_default_values', 
               lab_layer = 'lab_layer',
               lab_site = 'lab_site',
               lab_pedon = 'lab_pedon')
    
    message("This function only reads the annotated information from the following tables:")
    message(paste(paste(names(verified_tables), collapse = ', '),
              'lab_area', 'lab_combine_nasis_ncss', sep = ', '))
    
    #link all the tables together
    key.df <- originalTables$lab_layer |>
                  select('site_key', 'pedon_key', 'layer_key') |>
      mutate(across(everything(), as.character))
    
    #pull in the two tables to define the state/country
    #...non-standard cross references here so processing separately
    #lab_area_crosswalk = 'lab_combine_nasis_ncss',
    #lab_area_key = 'lab_area'
    location_names <- originalTables$lab_combine_nasis_ncss |>
      select('site_key', 'pedon_key',
             #'lab_area::site_observation_date' = 'site_obsdate',
             'lab_area::latitude' = "latitude_decimal_degrees",
             'lab_area::longitude' = "longitude_decimal_degrees",
             "country_key", "state_key", 
             "county_key", "mlra_key", 
             "ssa_key", "npark_key", "nforest_key") |>
      # Append country_type and country_name columns to the preceding table result, where each new columns' cell value in any given row depends on the 
      # following: if for said row's corresponding country_key's cell value there exist a originalTables' lab_area table's area_key's cell value that 
      # equals it, then the new columns' cell value is set to that of the appropriate lab_area table column's cell value; else set the cell value to NA
      left_join(originalTables$lab_area |>
                  select(area_key, 
                         country_type = area_type, 
                         country_name = area_name),
                by = join_by(country_key == area_key)) |>
      # Append state_type and state_name columns to the preceding table result, where each new columns' cell value in any given row depends on the 
      # following: if for said row's corresponding state_name's cell value there exist a originalTables' lab_area table's area_key's cell value that 
      # equals it, then the new columns' cell value is set to that of the appropriate lab_area table column's cell value; else set the cell value to NA
      left_join(originalTables$lab_area |>
                  mutate(state_type = 
                           paste(area_type, area_sub_type)) |>
                  select(area_key,
                         state_type,
                         state_name = area_name),
                by = join_by(state_key == area_key)) |>
      # Append county_type and county_name columns to the preceding table result, where each new columns' cell value in any given row depends on the 
      # following: if for said row's corresponding county_key's cell value there exist a originalTables' lab_area table's area_key's cell value that 
      # equals it, then the new columns' cell value is set to that of the appropriate lab_area table column's cell value; else set the cell value to NA
      left_join(originalTables$lab_area |>
                  select(area_key, 
                         county_type = area_type, 
                         county_name = area_name),
                by = join_by(county_key == area_key)) |>
      # Append mlra_type and mlra_name columns to the preceding table result, where each new columns' cell value in any given row depends on the 
      # following: if for said row's corresponding mlra_key's cell value there exist a originalTables' lab_area table's area_key's cell value that 
      # equals it, then the new columns' cell value is set to that of the appropriate lab_area table column's cell value; else set the cell value to NA
      left_join(originalTables$lab_area |>
                  select(area_key, 
                         mlra_type = area_type, 
                         mlra_name = area_name),
                by = join_by(mlra_key == area_key)) |>
      # Append ssa_type and ssa_name columns to the preceding table result, where each new columns' cell value in any given row depends on the 
      # following: if for said row's corresponding ssa_key's cell value there exist a originalTables' lab_area table's area_key's cell value that 
      # equals it, then the new columns' cell value is set to that of the appropriate lab_area table column's cell value; else set the cell value to NA
      left_join(originalTables$lab_area |>
                  select(area_key, 
                         ssa_type = area_type, 
                         ssa_name = area_name),
                by = join_by(ssa_key == area_key))|>
      # Append npark_type and npark_name columns to the preceding table result, where each new columns' cell value in any given row depends on the 
      # following: if for said row's corresponding npark_key's cell value there exist a originalTables' lab_area table's area_key's cell value that 
      # equals it, then the new columns' cell value is set to that of the appropriate lab_area table column's cell value; else set the cell value to NA
      left_join(originalTables$lab_area |>
                  select(area_key, 
                         npark_type = area_type, 
                         npark_name = area_name),
                by = join_by(npark_key == area_key)) |>
      # Append nforest_type and nforest_name columns to the preceding table result, where each new columns' cell value in any given row depends on the 
      # following: if for said row's corresponding nforest_key's cell value there exist a originalTables' lab_area table's area_key's cell value that 
      # equals it, then the new columns' cell value is set to that of the appropriate lab_area table column's cell value; else set the cell value to NA
      left_join(originalTables$lab_area |>
                  select(area_key, 
                         nforest_type = area_type, 
                         nforest_name = area_name),
                by = join_by(nforest_key == area_key)) |>
      mutate(across(ends_with('_type'), as.factor)) |>
      mutate(across(everything(), as.character)) |>
      pivot_longer(cols = -c('site_key', 'pedon_key'),
                   #apply the headers to the variable name instead of a column id
                   names_to = 'of_variable',
                   values_to = 'with_entry',
                   values_drop_na = TRUE) |>
      #remove the lab_area keys from the variables
      filter(!stringr::str_detect(of_variable, regex('_key$'))) |>
      mutate(is_type = 'value') |> #Dev: Consider coming back to this with the name/type separation
      left_join(key.df, by = join_by(site_key, pedon_key) )
    
    # Get all of the data from the verified tables and pivot it long to be bound with the location_names table later
    ans.df <- plyr::ldply(verified_tables,
                          function(tableName.str){
                            temp_key <- reducedAnnotations |>
                              dplyr::filter(table_id == tableName.str,
                                            with_entry == '--') |>
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
