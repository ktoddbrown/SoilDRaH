#' CPEAT data sets
#' 
#' This reads in the first 1000 records of the CPEAT project from PANGAEA with search term "project:label:PAGES_C-PEAT", as of 2023-December there are 875 data packages found.
#'
#' @param dataDir filename of the download directory to store the data in
#' @param annotationFilename filename for the data annotations csv file 
#' @param format character flag to either return a list of cores or the cores as a set of tables
#' @param verbose boolean status to print out updates
#'
#' @importFrom pangaear pg_search pg_cache
#' @importFrom plyr dlply ldply 
#' @importFrom dplyr mutate across reframe
#' @importFrom tidyr pivot_longer
#' @importFrom stringr str_extract
#' @import magrittr
#' @export

# Defining CPEAT function
readCPEAT <- function(dataDir, 
                      annotationFilename, 
                      format=c('original', 'long')[1], 
                      verbose=FALSE){
  
  ##### find CPEAT datasets and load in from the pangaea repository 
  
  pangaearSearchTerm <- "project:label:PAGES_C-PEAT" # searching the repo using keywords
  pages.df <- pangaear::pg_search(pangaearSearchTerm, count = 500) %>%  # search limit for 500 results
    dplyr::bind_rows(pangaear::pg_search(pangaearSearchTerm, count = 500, offset = 500)) %>% #the output will contain all columns that appear in any of the inputs
    dplyr::mutate(fullcitation = paste0(citation, ". PANGAEA, https://doi.org/", doi)) # adding a full citation column
  
  if(nrow(pages.df) != 876){
    warning("unexpected number of CPEAT core datasets found; code could behave unexpectedly. Please report to the repository.") # a warning would let you run the rest of the code!
  }
  
  #### Download CPEAT datasets
  
  #Keep the old cache directory so that we can reset it 
  oldCache <- pangaear::pg_cache$cache_path_get()
  
  #create a new directory for all the CPEAT downloads if it doesn't exist
  if(!file.exists(file.path(dataDir, 'CPEAT'))){
    dir.create(file.path(dataDir, 'CPEAT'))
  }
  
  # set the new data directory 
  dataDir <- file.path(dataDir, 'CPEAT')
  
  #set the cache, we reset back to the oldCache at the end of this function
  pangaear::pg_cache$cache_path_set(full_path = dataDir)
  
  if(verbose) message('Loading the CPEAT datasets, this takes a while...')
  
  #Pull into a list, all the data from the files the specified dois' in the search results 
  CPEAT.original <- plyr::dlply(pages.df, #search results
                            c("doi"), #grouping on unique identifier
                            .fun = function(xx) { #defining our fetch function as a wrapper
                              
                              # fetch data package from PANGAEA; this loads in the original data for the studies, in this case C-PEAT
                              datapackage <- pangaear::pg_data(doi = xx$doi, overwrite = FALSE)
                              
                              #check that the pg_data returns a list of a single item, remove that from the parent list
                              if(length(datapackage) != 1){
                                stop(paste("Data package contains more then one item. DOI:", xx$doi))
                              }
                              
                              return(datapackage[[1]]) # this returns the first item in data
                            }) 
  
  #### Return the original data without transformation in a list format
  
  if(format == 'original'){
    return(list(orginal = CPEAT.original, 
                annotations = read_csv(file = annotationFilename, 
                                       col_types = cols(.default = col_character())))) # this will change the column structure into a character type
  }

    
#############################
###Shoestring all the data

  long.df <- plyr::ldply(CPEAT.original, .id = 'doi', # the ldply function is used to apply a function to each dataset; identified by 'doi' here
                             .fun = function(xx, verbose = FALSE) {
    
    if(verbose) print(paste('Processing - ', xx$doi))
                               
    ###################################
    #### Process layer data
                               
    if(length(names(xx$data)) > length(unique(names(xx$data)))){ # testing if there are any duplicate column names
      #warning(names(xx$data))
      ##This is a know issue. Suppressing warning.
      #warning(paste(xx$doi, "- Duplicate column names detected"))
      #TODO make this more elegant, we may need to read in and parse the txt files instead of relying on pangear
      #right now pivoting column names has to be unique so adding a counter to the end
      names(xx$data) <- paste(names(xx$data), 1:length(names(xx$data)))
    }
                               
    colume_number <- tibble(column_name = names(xx$data), # creates a tibble with two columns with original column names and corresponding column numbers
                            column_number = 1:length(names(xx$data)))
    
    layerData <- xx$data %>%
      mutate(across(.cols = everything(), as.character)) %>%  # coverts all columns to character type
      mutate(row_number = 1:n()) %>%  # adding a row number column grouped by doi
      pivot_longer(cols = -row_number, names_to = 'column_name', # pivoting all columns except the row_number column from wide into a long format
                   values_to = 'with_entry',
                   values_drop_na = TRUE) %>% # dropping all NA values in the data
      left_join(colume_number, by = join_by(column_name)) %>% # joining by column_name; keeping all the rows and columns from xx$data
      mutate(table_name = 'data') # adding table_name column
    
    #####################################
    ### Process primary study information
    ### This information is stored in the first level list
    
    #list out all the possible names for the study info, be sure to update
    #...this list manually if the warning is thrown.
    primaryNames <- intersect(names(xx), c('parent_doi', 'doi', 'citation', 
                                           'url', 'path'))
    
    #There shouldn't be any names that we don't include above.
    if(any( ! (names(xx) %in% c(primaryNames, 'metadata', 'data')))){
      warning(paste('possible missing informatin at primary level for', xx$doi,
                    setdiff(names(xx), c(primaryNames, 'metadata', 'data'))))
    }
    
    ##############
    ###Meta data
    ###This information is stored under a list named 'metadata'
    
    #deal with the information in the metadata, again we name each possible
    #...list item here and if there are new names this needs to be updated.
    metaNames <- intersect(names(xx$metadata), c("citation", "related_to", "further_details",
                                                 "projects" , "coverage",
                                                 "abstract", "keywords",
                                                 "status",
                                                 "license", "size", "comment"))
    
    if(any( ! (names(xx$metadata) %in% c(metaNames, 'events', 'parameters')))){
      warning(paste('possible missing informatin at metadata level for', xx$doi))
    }
    
    #############
    ###Events
    ###This information is under the list 'metadata$events'
    
    #Pull in the study information from the 'events' item in the list
    #...again there should be no items that are not matching the manual array here
    eventsNames <- intersect(names(xx$metadata$events), 
                             c("LATITUDE", "LONGITUDE",
                               "ELEVATION", "ELEVATION START", "ELEVATION END",
                               "Penetration", "Recovery",
                               "LOCATION", "METHOD/DEVICE", 
                               "COMMENT"))
    
    #Take out the first item of the list which is actually the core name itself.
    if(any( ! (names(xx$metadata$events)[-1] %in%  
               c("LATITUDE", "LONGITUDE", "ELEVATION",
                 "ELEVATION START", "ELEVATION END", 
                 "Penetration","Recovery",
                 "LOCATION", "METHOD/DEVICE", "COMMENT")))){
      warning(paste('possible missing informatin at metadata-events level for', xx$doi))
    }
    
    
    #The core name is a special case where the information is in the name and not
    #...in the list values. Deal with that and append the events information.
    studyData <- as.data.frame(c(xx[primaryNames], # combining the primary study, metadata, and event data info into a single data frame
                                 xx$metadata[metaNames],
                                 list(core_name = names(xx$metadata$events)[1]), # this extracts and includes the core names (typically the first element of the 'events' sub-list)  as a separate entry column 
                                 xx$metadata$events[eventsNames])) %>%
      pivot_longer(cols = everything(), # pivoting it into long format
                   names_to = 'column_name',
                   values_to = 'with_entry')
    
    
    ####### 
    ###Process parameter information
    
    if(xx$doi == "10.1594/PANGAEA.934274" &
       length(xx$metadata$parameters) > nrow(colume_number)){
      #A comment on the data age material got split between two column entries here
      #...combine them into the same entry
      xx$metadata$parameters[[5]] <- c(xx$metadata$parameters[[5]],
                                           xx$metadata$parameters[[6]])
      xx$metadata$parameters[6] <- NULL
    }
    
    # creating a description column to the column_number tibble for each parameter table by combining its elements into a single string.
    colume_number$description <- unlist(lapply(xx$metadata$parameters, 
                                               function(yy){
      return(paste(as.character(yy), collapse = ' '))
                                                 }))
    
    layerData <- colume_number %>%
      pivot_longer(cols = description, 
                   names_to = 'is_type', 
                   values_to = 'with_entry') %>%
      mutate(table_name = 'data') %>%
      bind_rows(layerData) 
    
    # Convert all columns to character type and return the modified data
    return(bind_rows(studyData, layerData))
  }) 
  
  #Set the table and column IDs to match the annotations
  long.df <- long.df %>%
    # creating new columns 
    mutate(column_id = trimws(str_remove(column_name, pattern = '(\\(|\\[).+')), # removes any extra text that matches either an opening '(' or. '[' or any more characters that follow; and also trims any trailing white spaces 
           table_id = if_else(is.na(table_name), 'study', 'core')) # if table_name is NS; it is set to 'study', if not, set to 'core'

    return(list(long = long.df, 
                annotations = read_csv(file = annotationFilename, 
                                       col_types = cols(.default = col_character()))
                ))
    
  
}


