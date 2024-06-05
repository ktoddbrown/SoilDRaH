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
readCPEAT <- function(dataDir,
                      format=c('original', 'long')[1], 
                      verbose=FALSE){
  
  ##### find CPEAT datasets 
  
  pangaearSearchTerm <- "project:label:PAGES_C-PEAT"
  pages.df <- pangaear::pg_search(pangaearSearchTerm, count = 500) %>% 
    dplyr::bind_rows(pangaear::pg_search(pangaearSearchTerm, count = 500, offset = 500)) %>% #the output will contain all columns that appear in any of the inputs
    dplyr::mutate(fullcitation = paste0(citation, ". PANGAEA, https://doi.org/", doi))
  
  if(nrow(pages.df) != 876){
    warning("unexpected number of CPEAT core datasets found; annotations might be incomplete.") # a warning would let you run the rest of the code!
  }
  
  #### Download CPEAT datasets
  
  #Keep the old cache directory so that we can reset it
  oldCache <- pangaear::pg_cache$cache_path_get()
  
  #create a new directory for all the CPEAT downloads
  if(!file.exists(file.path(dataDir, 'CPEAT'))){
    dir.create(file.path(dataDir, 'CPEAT'))
  }
  dataDir <- file.path(dataDir, 'CPEAT')
  
  #set the cache, we reset back to the oldCache at the end of this function
  pangaear::pg_cache$cache_path_set(full_path = dataDir)
  
  if(verbose) message('Loading the CPEAT datasets, this takes a while...')
  #Pull into a list, all the data from the files the specified dois in the search results 
  allData.ls <- plyr::dlply(pages.df, #search results
                            c("doi"), #grouping on unique identifer
                            .fun = function(xx) { #defining our fetch function as a wrapper
                              
                              datapackage <- pangaear::pg_data(doi = xx$doi, overwrite = FALSE)
                              
                              #check that the pg_data returns a list of a single item, remove that from the parent list
                              if(length(datapackage) != 1){
                                stop(paste("Data package contains more then one item. DOI:", xx$doi))
                              }
                              
                              return(datapackage[[1]])
                            }) 
  
  #### Return the original data without transformation
  ## TODO- load in annotations and return with the rest of the data
  ### TODO - replace the arguments for the format with original and long instead of byCore and byDatatype
  
  if(format == 'original'){
    return(allData.ls)
  }
  
  #Pull the core information by accessing the data item in the lists
  allCores.df <- plyr::ldply(allData.ls, .fun = function(xx) {
    #print(xx$doi)
    if(xx$doi %in% c("10.1594/PANGAEA.934281")){
      #names(xx$data)[names(xx$data) == "Age unc [±] (Age AD, calculated, 1 sigma)"] <- 'Age unc [±] (Age, tephra-chronostratigraphy, calculated, 1 sigma)'
      names(xx$data)[10] <- 'Age unc [±] (Age, tephra-chronostratigraphy, calculated, 1 sigma)' # refer to the actual variable name instead of the number to index
    } # the issue we are running into if we replace the position index for the column with the actual variable name, these column names are duplicates in the original data! 
    
    if(xx$doi %in% c("10.1594/PANGAEA.934343")){
      names(xx$data)[9] <- 'Age unc [±] (Age, tephra-chronostratigraphy, calculated, 1 sigma)'
    }
    
    if(xx$doi %in% c("10.1594/PANGAEA.941094")){
      names(xx$data)[10] <- 'Age [a AD/CE] alt.'
    }
    
    if(xx$doi %in% c("10.1594/PANGAEA.929068")){
      names(xx$data)[5:7] <- paste(names(xx$data)[5:7], 'alt.') # unknown
    }
    
    if(identical(names(xx$data)[c(2:4, 6:8)], 
                 c("Cal age [ka BP] (Median Age, Age, 14C calibrat...)", 
                   "Cal age max [ka BP] (Age, 14C calibrated, OxCal 4....)",
                   "Cal age min [ka BP] (Age, 14C calibrated, OxCal 4....)",
                   "Cal age [ka BP] (Median Age, Age, 14C calibrat...)",
                   "Cal age max [ka BP] (Age, 14C calibrated, Bacon 2....)",
                   "Cal age min [ka BP] (Age, 14C calibrated, Bacon 2....)")) |
       xx$doi %in% c("10.1594/PANGAEA.929655", "10.1594/PANGAEA.930133")){
      #The method note (OxCal & Bacon) were dropped from the Median Age, so we are adding them back in
      names(xx$data)[c(2:4, 6:8)] <- c(
        'Cal age [ka BP] (Median Age, 14C calibrated, OxCal 4.2.4)', 
        'Cal age max [ka BP] (Age, 14C calibrated, OxCal 4.2.4)', 
        'Cal age min [ka BP] (Age, 14C calibrated, OxCal 4.2.4)',
        'Cal age [ka BP] (Median Age, 14C calibrated, Bacon 2.2)', 
        'Cal age max [ka BP] (Age, 14C calibrated, Bacon 2.2)', 
        'Cal age min [ka BP] (Age, 14C calibrated, Bacon 2.2)') 
    }
    
    if(identical(names(xx$data)[2:7], 
                 c("Cal age [ka BP] (Median Age, Age, 14C calibrat...)", 
                   "Cal age max [ka BP] (Age, 14C calibrated, OxCal 4....)",
                   "Cal age min [ka BP] (Age, 14C calibrated, OxCal 4....)",
                   "Cal age [ka BP] (Median Age, Age, 14C calibrat...)",
                   "Cal age max [ka BP] (Age, 14C calibrated, Bacon 2....)",
                   "Cal age min [ka BP] (Age, 14C calibrated, Bacon 2....)")) |
       xx$doi %in% c("10.1594/PANGAEA.930030")){
      
      names(xx$data)[2:7] <- c(
        'Cal age [ka BP] (Median Age, 14C calibrated, OxCal 4.2.4)', 
        'Cal age max [ka BP] (Age, 14C calibrated, OxCal 4.2.4)', 
        'Cal age min [ka BP] (Age, 14C calibrated, OxCal 4.2.4)',
        'Cal age [ka BP] (Median Age, 14C calibrated, Bacon 2.2)', 
        'Cal age max [ka BP] (Age, 14C calibrated, Bacon 2.2)', 
        'Cal age min [ka BP] (Age, 14C calibrated, Bacon 2.2)') #add the methods that are dropped here
    }
    
    
    if(length(names(xx$data)) > length(unique(names(xx$data)))){
      print(names(xx$data)) ## todo - consider throwing a warning that the names are not unique
      warning("Duplicate column names detected")
    }
    return(dplyr::mutate(.data = xx$data, across(.cols = everything(),  as.character)))
  }) 
  
  #Pull the study information
  allStudy.df <- plyr::ldply(allData.ls, .fun = function(xx) {
    #list out all the possible names for the study info, be sure to update
    #...this list manually if the warning is thrown.
    primaryNames <- intersect(names(xx), c('parent_doi', 'doi', 'citation', 
                                           'url', 'path'))
    
    #There shouldn't be any names that we don't include above.
    if(any( ! (names(xx) %in% c(primaryNames, 'metadata', 'data')))){
      warning(paste('possible missing informatin at primary level for', xx$doi,
                    setdiff(names(xx), c(primaryNames, 'metadata', 'data'))))
    }
    
    #access the study information in the primary list
    ans.ls <- xx[primaryNames]
    
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
    
    #append the metadata items to the items from the primary list
    ans.ls <- c(ans.ls, xx$metadata[metaNames])
    
    #Pull in the study information from the 'events' item in the list
    #...again there should be no items that are not matching the manual array here
    eventsNames <- intersect(names(xx$metadata$events), c("LATITUDE", "LONGITUDE",
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
    ans.ls <- c(list(core_name = names(xx$metadata$events)[1]),
                ans.ls, 
                xx$metadata$events[eventsNames])
    
    # change everything to a data frame to make it easier to read
    return(as.data.frame(ans.ls, stringsAsFactors = FALSE))
  })
  
  
  allParameters.df <-  plyr::ldply(allData.ls, .fun = function(xx) {
    
    ##Copy-paste (sigh... yes I know) from allCores.df construction
    if(xx$doi %in% c("10.1594/PANGAEA.934281")){
      names(xx$data)[10] <- 'Age unc [±] (Age, tephra-chronostratigraphy, calculated, 1 sigma)'
    }
    
    if(xx$doi %in% c("10.1594/PANGAEA.934343")){
      names(xx$data)[9] <- 'Age unc [±] (Age, tephra-chronostratigraphy, calculated, 1 sigma)'
    }
    
    if(xx$doi %in% c("10.1594/PANGAEA.941094")){
      names(xx$data)[10] <- 'Age [a AD/CE] alt.'
    }
    
    if(xx$doi %in% c("10.1594/PANGAEA.929068")){
      names(xx$data)[5:7] <- paste(names(xx$data)[5:7], 'alt.') #true replicates??
    }
    
    if(identical(names(xx$data)[c(2:4, 6:8)], 
                 c("Cal age [ka BP] (Median Age, Age, 14C calibrat...)", 
                   "Cal age max [ka BP] (Age, 14C calibrated, OxCal 4....)",
                   "Cal age min [ka BP] (Age, 14C calibrated, OxCal 4....)",
                   "Cal age [ka BP] (Median Age, Age, 14C calibrat...)",
                   "Cal age max [ka BP] (Age, 14C calibrated, Bacon 2....)",
                   "Cal age min [ka BP] (Age, 14C calibrated, Bacon 2....)")) |
       xx$doi %in% c("10.1594/PANGAEA.929655", "10.1594/PANGAEA.930133")){
      names(xx$data)[c(2:4, 6:8)] <- c(
        'Cal age [ka BP] (Median Age, 14C calibrated, OxCal 4.2.4)', 
        'Cal age max [ka BP] (Age, 14C calibrated, OxCal 4.2.4)', 
        'Cal age min [ka BP] (Age, 14C calibrated, OxCal 4.2.4)',
        'Cal age [ka BP] (Median Age, 14C calibrated, Bacon 2.2)', 
        'Cal age max [ka BP] (Age, 14C calibrated, Bacon 2.2)', 
        'Cal age min [ka BP] (Age, 14C calibrated, Bacon 2.2)') #add the methods that are dropped here
    }
    
    if(identical(names(xx$data)[2:7], 
                 c("Cal age [ka BP] (Median Age, Age, 14C calibrat...)", 
                   "Cal age max [ka BP] (Age, 14C calibrated, OxCal 4....)",
                   "Cal age min [ka BP] (Age, 14C calibrated, OxCal 4....)",
                   "Cal age [ka BP] (Median Age, Age, 14C calibrat...)",
                   "Cal age max [ka BP] (Age, 14C calibrated, Bacon 2....)",
                   "Cal age min [ka BP] (Age, 14C calibrated, Bacon 2....)")) |
       xx$doi %in% c("10.1594/PANGAEA.930030")){
      
      names(xx$data)[2:7] <- c(
        'Cal age [ka BP] (Median Age, 14C calibrated, OxCal 4.2.4)', 
        'Cal age max [ka BP] (Age, 14C calibrated, OxCal 4.2.4)', 
        'Cal age min [ka BP] (Age, 14C calibrated, OxCal 4.2.4)',
        'Cal age [ka BP] (Median Age, 14C calibrated, Bacon 2.2)', 
        'Cal age max [ka BP] (Age, 14C calibrated, Bacon 2.2)', 
        'Cal age min [ka BP] (Age, 14C calibrated, Bacon 2.2)') #add the methods that are dropped here
    }
    
    colHeader <- names(xx$data)
    colDescript <- unlist(lapply(xx$metadata$parameters, paste, collapse = " "))
    
    if(xx$doi == "10.1594/PANGAEA.934274"){
      colDescript <- c(colDescript[1:4], 
                       paste(colDescript[5], ';', colDescript[6]),
                       colDescript[7:9])
    }
    
    #pull the parameter descriptions in and append them to the headers in the data
    ParametersGeo <- data.frame(header = colHeader, 
                                description = colDescript, 
                                stringsAsFactors = FALSE) %>%
      #add in a column index
      dplyr::mutate(column_index = 1:ncol(xx$data))
    
    return(ParametersGeo)
  })
  
  # include a read function for the data annotations associated with CPEAT
  #... check that there isn't new columns
  
  #reset the orginal cache directory
  pangaear::pg_cache$cache_path_set(full_path = oldCache)
  
  allStudy.df <- allStudy.df %>%
    mutate(across(c(ELEVATION, `ELEVATION.START`, `ELEVATION.END`,
                    Recovery, Penetration,
                    size), ~stringr::str_extract(.x, '\\d+')))
  
  allParameters.df <- allParameters.df %>%
    mutate(core_id = doi)
  
  return(list(core = allCores.df,
              study = allStudy.df, 
              parameters = allParameters.df))
  
  
}


