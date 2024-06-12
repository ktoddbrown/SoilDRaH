#' CPEAT data sets
#' 
#' This reads in the first 1000 records of the CPEAT project from PANGAEA with search term "project:label:PAGES_C-PEAT", as of 2023-December there are 875 data packages found.
#'
#' @param dataDir filename of the download directory to store the data in
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
    warning("unexpected number of CPEAT core datasets found; annotations might be incomplete.") # a warning would let you run the rest of the code!
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
  
  #Pull into a list, all the data from the files the specified dois in the search results 
  allData.ls <- plyr::dlply(pages.df, #search results
                            c("doi"), #grouping on unique identifer
                            .fun = function(xx) { #defining our fetch function as a wrapper
                              
                              # fetch data package from PANGAEA; this loads in the original data for the studies, in this case C-PEAT
                              datapackage <- pangaear::pg_data(doi = xx$doi, overwrite = FALSE)
                              
                              #check that the pg_data returns a list of a single item, remove that from the parent list
                              if(length(datapackage) != 1){
                                stop(paste("Data package contains more then one item. DOI:", xx$doi))
                              }
                              
                              return(datapackage[[1]]) # this returns the first item in data
                            }) 
  
  #### Return the original data without transformation
  
  if(format == 'original'){
    return(list(orginal = allData.ls, 
                annotations = read_csv(file = annotationFilename, 
                                       col_types = cols(.default = col_character()))))
  }

    
#############################
###Shoestring all the data

  long.df <- plyr::ldply(CPEAT.original, .id = 'doi',
                             .fun = function(xx, verbose = FALSE) {
    
    if(verbose) print(paste('Processing - ', xx$doi))
                               
    ###################################
    #### Process layer data
                               
    if(length(names(xx$data)) > length(unique(names(xx$data)))){
      #warning(names(xx$data))
      warning(paste(xx$doi, "- Duplicate column names detected"))
      #TODO make this more elegant
      #right now pivoting column names has to be unique so adding a counter to the end
      names(xx$data) <- paste(names(xx$data), 1:length(names(xx$data)))
    }
                               
    colume_number <- tibble(column_name = names(xx$data),
                            column_number = 1:length(names(xx$data)))
    
    layerData <- xx$data %>%
      mutate(across(.cols = everything(), as.character)) %>%
      mutate(row_number = 1:n()) %>%
      pivot_longer(cols = -row_number, names_to = 'column_name',
                   values_to = 'with_entry',
                   values_drop_na = TRUE) %>%
      left_join(colume_number, by = join_by(column_name)) %>%
      mutate(table_name = 'data')
    
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
    studyData <- as.data.frame(c(xx[primaryNames], 
                                 xx$metadata[metaNames],
                                 list(core_name = names(xx$metadata$events)[1]),
                                 xx$metadata$events[eventsNames])) %>%
      pivot_longer(cols = everything(), 
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
  

    return(list(long = long.df, 
                annotations = read_csv(file = annotationFilename, 
                                       col_types = cols(.default = col_character()))
                ))
    
  
}


