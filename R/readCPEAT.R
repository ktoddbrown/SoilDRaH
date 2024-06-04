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
  ## TODO- load in annotations and return with the rest of the data
  ### TODO - replace the arguments for the format with original and long instead of byCore and byDatatype
  
  if(format == 'original'){
    return(allData.ls)
  }
  
  #Pull the core information by accessing the data item in the lists
  allCores.df <- plyr::ldply(allData.ls, .fun = function(xx) {
    #print(xx$doi)
    
    # renaming the columns (indexing using the column numbers!)
    if(xx$doi %in% c("10.1594/PANGAEA.934281")){
      #names(xx$data)[names(xx$data) == "Age unc [±] (Age AD, calculated, 1 sigma)"] <- 'Age unc [±] (Age, tephra-chronostratigraphy, calculated, 1 sigma)'
      names(xx$data)[10] <- 'Age unc [±] (Age, tephra-chronostratigraphy, calculated, 1 sigma)' # refer to the actual variable name instead of the number to index
    } # the issue we are running into if we replace the position index for the column with the actual variable name, these column names are duplicates in the original data! 
    
    # renaming the columns based on specific DOIs
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
    
    # Check for duplicate column names
    if(length(names(xx$data)) > length(unique(names(xx$data)))){
      print(names(xx$data)) ## todo - consider throwing a warning that the names are not unique
      warning("Duplicate column names detected")
    }
    
    # Convert all columns to character type and return the modified data
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
      names(xx$data)[5:7] <- paste(names(xx$data)[5:7], 'alt.') #true replicates??; they are not true replicates in the data but have same column names- hence adding alt. to the column name to distinguish!
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
        'Cal age min [ka BP] (Age, 14C calibrated, Bacon 2.2)') #add the methods that are dropped here; i think there are no methods here to add to this dataset!
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
    
    # extract column headers and descriptions
    colHeader <- names(xx$data)
    colDescript <- unlist(lapply(xx$metadata$parameters, paste, collapse = "!"))
    
    if(xx$doi == "10.1594/PANGAEA.934274"){
      colDescript <- c(colDescript[1:4], 
                       paste(colDescript[5], ';', colDescript[6]), # concatenating the 5th and 6th element into a single string
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
  
  #TODO move this over to the data folder once we have the pipeline debugged
  if(TRUE){
    ##Draft annotations for later manual curation
    constructedCoreAnnotations <- allCores.df %>%
      mutate(across(.cols=everything(), as.character)) %>%
      mutate(core_id = doi) %>% #1:n()) %>%
      pivot_longer(cols=-c(core_id), values_drop_na = TRUE, names_to = 'column_name')  %>%
      reframe(example_entry = paste(unique(value)[1:5], collapse = '; '),
              obs_count = sum(!is.na(value)),
              .by = c(core_id, column_name)) %>%
      full_join(allParameters.df,
                by = c('core_id' = 'doi', 'column_name' = 'header')) %>%
      select(core_id, column_name, description, example_entry, obs_count) %>%
      mutate(of_variable = case_when( str_detect(column_name, 'doi') ~ 'doi',
                                      str_detect(column_name, '[Dd]epth sed') ~ 'layer_mid',
                                      str_detect(column_name, '[Dd]epth bot') ~ 'layer_bottom',
                                      str_detect(column_name, '[Dd]epth top') ~ 'layer_top',
                                      str_detect(column_name, 'Peat') ~ 'peat',
                                      str_detect(column_name, '[Aa]ge') ~ 'age',
                                      str_detect(column_name, 'Samp thick') ~ 'layer_thickness',
                                      str_detect(column_name, 'DBD') ~ 'bulk_density',
                                      str_detect(column_name, '^OM') ~ 'organic_matter',
                                      str_detect(column_name, '^LOI') ~ 'loss_on_ignition',
                                      str_detect(column_name, 'Corg') ~ 'organic_carbon',
                                      str_detect(column_name, '[Vv]ol ') ~ 'volume',
                                      str_detect(column_name, '^C ') ~ 'organic_carbon',
                                      str_detect(column_name, 'TOC') ~ 'organic_carbon',
                                      str_detect(column_name, 'TIC') ~ 'inorganic_carbon',
                                      str_detect(column_name, '^TC') ~ 'total_carbon',
                                      str_detect(column_name, '^TN') ~ 'total_nitrogen',
                                      str_detect(column_name, 'Dated material') ~ 'age',
                                      str_detect(column_name, '[Cc]omm') ~ 'age',
                                      str_detect(column_name, 'label') ~ 'lab_label',
                                      str_detect(column_name, 'Activity') ~ 'modern_carbon_activity',
                                      str_detect(column_name, 'F14C') ~ 'modern_carbon_activity',
                                      str_detect(column_name, 'SR') ~ 'sedimentation_rate',
                                      str_detect(column_name, 'Water') ~ 'water_content',
                                      str_detect(column_name, 'Cum mass') ~ 'total_core_mass',
                                      str_detect(column_name, 'PCAR') ~ 'peat_accumulation_rate',
                                      TRUE ~ NA),
             unit = case_when(
               str_detect(column_name, '\\[m\\]') ~ 'm',
               str_detect(column_name, '\\[ka.*\\]') ~ 'ka BP',
               str_detect(column_name, '\\[g/cm\\*\\*3\\]') ~ 'g cm-3',
               str_detect(column_name, '\\[cm\\]') ~ 'cm',
               str_detect(column_name, '\\[%\\]') ~ 'mass percent',
               str_detect(column_name, '\\[pMC\\]') ~ 'modern carbon percent',
               str_detect(column_name, 'F14C') ~ 'modern carbon fraction',
               str_detect(column_name, '\\[a AD/CE\\]') ~ 'a CE',
               str_detect(column_name, '\\[cm/a\\]') ~ 'cm yr-1',
               str_detect(column_name, '\\[ml\\]') ~ 'mL',
               str_detect(column_name, '\\[cm\\*\\*3\\]') ~ 'cm3',
               str_detect(column_name, '\\[g/cm\\*\\*2\\]') ~ 'g cm-2',
               str_detect(column_name, '\\[g/m\\*\\*2\\/a]') ~ 'g m-2 yr-1',
               str_detect(column_name, '[a] (years ago)') ~ 'yr before measure',
               TRUE ~ NA),
             type = case_when( str_detect(column_name, 'max') ~ 'maximum',
                               str_detect(column_name, 'min') ~ 'maximum',
                               str_detect(column_name, '±') ~ '1 sigma',
                               str_detect(column_name, 'Peat') ~ 'note', #no control vocabulary
                               !is.na(unit) ~ 'value',
                               str_detect(column_name, 'Dat.* material') ~ 'method',
                               str_detect(column_name, '[Cc]omm') ~ 'method',
                               str_detect(column_name, 'label') ~ 'id',
                               str_detect(column_name, 'doi') ~ 'id',
                               TRUE ~ NA)) %>%
      mutate(calibration_status = case_when(of_variable != 'age' | type != 'value' ~ '',
                                            str_detect(column_name, 'Age dated [ka]') ~ 'uncalibrated',
                                            str_detect(column_name, '[Uu]ncalibrated') ~ 'uncalibrated',
                                            str_detect(description, '[Uu]ncalibrated') ~ 'uncalibrated',
                                            str_detect(column_name, 'calibrated') ~ 'calibrated',
                                            str_detect(description, 'calibrated') ~ 'calibrated',
                                            TRUE ~ ''),
             age_model = case_when(of_variable != 'age' | type != 'value' ~ '',
                                   str_detect(column_name, 'OxCal') ~ 'OxCal_4.2.4',
                                   str_detect(description, 'OxCal') ~ 'OxCal_4.2.4',
                                   str_detect(column_name, 'Bacon') ~ 'Bacon_2.2',
                                   str_detect(description, 'Bacon') ~ 'Bacon_2.2',
                                   TRUE ~ ''),
             isotope = case_when(of_variable != 'age' | type != 'value' ~ '',
                                 str_detect(column_name, '14C') ~ '14C',
                                 str_detect(description, '14C') ~ '14C',
                                 str_detect(column_name, '210Pb') ~ '210Pb',
                                 str_detect(description, '210Pb') ~ '210Pb',
                                 str_detect(column_name, 'ephra-chronostratigraphy') ~ 'tephra-chronostratigraphy',
                                 str_detect(description, 'ephra-chronostratigraphy') ~ 'tephra-chronostratigraphy',
                                 TRUE ~ '')) %>%
      mutate(across(.cols = -column_name, str_trim)) %>%
      mutate(method = paste(isotope, age_model, calibration_status)) %>%
      mutate(table_name = 'core') %>%
      select(core_id, column_name, of_variable, unit, type, method, description) %>%
      mutate(across(.cols = -column_name, str_trim)) %>%
      unique()
    
    coreAnnotationSub1 <- constructedCoreAnnotations %>% 
      select(core_id, column_name, of_variable, of_type = type) %>%
      unique() %>%
      mutate(with_entry = '--')
    
    coreAnnotationSub2 <- constructedCoreAnnotations %>% 
      select(-type) %>%
      unique() %>%
      pivot_longer(cols = c(unit, method, description), names_to = 'of_type', values_to = 'with_entry') %>%
      mutate(with_entry = str_trim(with_entry)) %>%
      filter(str_detect(pattern = '\\w', with_entry))
    
    studyAnnotation <- tribble(~column_name, ~description, ~of_variable, ~unit,
                               "core_name", "Core name", "core_name", NA_character_,
                               "parent_doi", "data package DOI", "data_doi",  NA_character_,
                               "doi",  "data package DOI", "data_doi",  NA_character_, #same as doi
                               "citation", "Citation", 'citation',  NA_character_,
                               "url", "download url", 'download_url',  NA_character_,
                               "path", "download path", 'data_local_filepath',  NA_character_,
                               "citation.1", "Citation", 'citation', NA_character_, #same as citation
                               "related_to", "Related citations", 'related_citation', NA_character_,
                               "projects", "Related project", 'project', NA_character_,
                               "coverage", "Bounded coverage", 'bounded_coverage', NA_character_,
                               "license", "Data license", 'data_license', NA_character_,
                               "size", "Observation size", 'observation_size', 'data point count',
                               "LATITUDE", "Latitude", 'latitude', 'decimal degree',
                               "LONGITUDE", "Longitude", 'longitude', 'decimal degree',
                               "LOCATION", "Adminstrative region", 'region', NA_character_,
                               "METHOD.DEVICE", "Sampling devise", 'sample_devise', NA_character_,
                               "COMMENT", "Data package comment", 'core_comment', NA_character_,
                               "further_details", "Citations", 'futher_citations', NA_character_,
                               "ELEVATION", "elevation", 'elevation', 'm',
                               "Recovery", "core length", 'core_length', 'cm',
                               "Penetration", "core length", 'core_length', 'cm',
                               "ELEVATION.START", "Start of elevation transect", 'elevation_start', 'm',
                               "ELEVATION.END", "End of elevation transect", 'elevation_end', 'm',
                               "abstract", "Abstract", 'abstract', NA_character_,
                               "keywords", "Keywords", 'keywords', NA_character_,
                               "status", "Curation level", 'cutation_level', NA_character_,
                               "comment", "Dataset Comment", 'core_comment', NA_character_) %>%
      mutate(table_name = 'study')
    
    studyAnnotationSub1 <- studyAnnotation %>%
      pivot_longer(cols = c(description, unit), 
                   names_to = 'of_type',
                   values_to = 'with_entry',
                   values_drop_na = TRUE)
    
    studyAnnotationSub2 <- studyAnnotation %>% 
      select(column_name, of_variable) %>%
      mutate(of_type = if_else(of_variable == 'core_comment', 'note', 'value'),
             with_entry = '--') 
    
    
    annotation.df <- bind_rows(coreAnnotationSub1, coreAnnotationSub2,
                               studyAnnotationSub1, studyAnnotationSub2) %>%
      arrange(table_name, core_id, of_variable, of_type)
  }
  
  return(list(core = allCores.df,
              study = allStudy.df, 
              parameters = allParameters.df,
              annotation = annotation.df))
  
  
}


