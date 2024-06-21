#' Title
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
#'
#' @examples
readNCSS <- function(dataDir,
                     annotationFilename,
                     format = c('original', 'long')[1],
                     verbose = TRUE){
  
  
  #dataDir <- "temp/NRCS_NCSS_20230922/" # debugging
  #annotationFilename <- "data/NCSS_Annotations.csv" #debugging
   
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
                               col_types = readr::cols(.default = col_character()))
  
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
    dbDisconnect(myconnect)
    
    return(list(annotations = annotations.df, original = orginalTables))
  }else{
    warning('This function currently only extracts the layer-resolved soil organic carbon variables.')
    
    ### Clean up the connection ####
    dbDisconnect(myconnect)
  }
  
  
  ### Clean up the connection ####
  dbDisconnect(myconnect)
  
  
}