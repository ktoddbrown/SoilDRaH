#' CPEAT data sets
#' 
#' This reads in the first 1000 records of the CPEAT project from PANGAEA with search term "project:label:PAGES_C-PEAT", as of 2024-July there are 878 data packages found.
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
                      randomSubset = Inf,
                      verbose=FALSE){
  
  ##Dev sets
  # dataDir <- 'temp/CPEAT'
  # verbose <- TRUE
  # randomSubset <- 10
  # #annotationFilename <- 'data/CPEAT_annotations.csv'
  # annotationFilename <- 'data/annotations_CPEAT2.csv'
  
  #### find CPEAT data ####
  
  #searching the repo using the CPEAT project label
  pangaearSearchTerm <- "project:label:PAGES_C-PEAT"
  
  # Pangaear only returns top 500 results as a hard max.
  # ...coding this up explicitly and then repeating.
  pages.df <- pangaear::pg_search(pangaearSearchTerm, count = 500) |>  
    #This should only get a total of 876, calling for the entire 500 just incase
    dplyr::bind_rows(pangaear::pg_search(pangaearSearchTerm, 
                                         count = 500, offset = 500)) |>
    #Add Pangaea with the doi address to create a full citation
    dplyr::mutate(fullcitation = paste0(citation, 
                                        ". PANGAEA, https://doi.org/", doi)) 
  
  if(nrow(pages.df) != 878){
    # Tell the user not to expect the data cleaning to work here 
    warning("unexpected number of CPEAT core datasets found; code could behave unexpectedly. Please report to the repository.") 
  }
  
  #This takes a long time so for dev purposes it can be useful
  #...to subset the pages
  
  pages.df <- pages.df |>
    dplyr::slice_sample(n = randomSubset)
  
  #### Download and load CPEAT data ####
  
  # Keep the old cache directory so that we can reset it 
  oldCache <- pangaear::pg_cache$cache_path_get()
  
  #set the cache for the download function, 
  #...we reset back to the oldCache at the end of this function
  pangaear::pg_cache$cache_path_set(full_path = dataDir)
  
  if(verbose) message('Loading the CPEAT datasets, this takes a while...')
  
  #Pull into a list all the data from the files the search results by doi
  CPEAT.original <- plyr::dlply(pages.df, #search results
                                c("doi"), #grouping on unique identifier
                                .fun = function(xx) {
                                  # fetch and loads in the original data with 
                                  #...the metadata parsed
                                  datapackage <- pangaear::pg_data(
                                    doi = xx$doi,
                                    overwrite = FALSE) #does not seem to work, 
                                  #...files are always overwritten
                                  
                                  #check that the pg_data returns a list of a 
                                  #...single item
                                  if(length(datapackage) != 1){
                                    stop(paste("Data package contains more 
                                               then one item. DOI:", xx$doi))
                                  }
                                  
                                  #remove from the parent list
                                  return(datapackage[[1]])
                                }) 
  
  #reset the cache for the download function
  pangaear::pg_cache$cache_path_set(full_path = oldCache)
  
  CPEAT.annotations <- readr::read_csv(
    file = annotationFilename,
    #make sure everything is a character
    col_types = readr::cols(.default = readr::col_character()))
  
  #### Return the original data with annotations ####
  if(format == 'original'){
    return(list(original = CPEAT.original, 
                annotations = CPEAT.annotations))
  }
  

  #### Shoestring all the data ####
  
  primaryAnnotations <- CPEAT.annotations |>
    dplyr::filter(with_entry == '--') |>
    dplyr::select(-with_entry)
  
  # Combine all metadata and primary data into one long format
  long.df <- plyr::ldply(
    CPEAT.original,
    #use the name of the list elements as an id called 'doi'
    .id = 'doi',
    .fun = function(xx, verbose = FALSE) {
      if(verbose) print(paste('Processing - ', xx$doi))
      
      ##Generally each list has the following elements
      ## - '$data' a data table with the layer data
      ## - '$metadata' a list of information on the core
      ## - '$metadata$events a list with 
      ##   ...geospatial and sampling information
      ## - 'metadata$parameter' a list of lists with parameter 
      ##  ...name followed by other metadata information that is
      ##  ...often problematically parsed.
      ## - 'metadata' also has the following elements (strings):
      ##  ..."citation", "related_to", "further_details",
      ##  ..."projects" , "coverage", "abstract", "keywords", 
      ##  ..."status", "license", "size", "comment", "change_history"
      ## - other primary level elements include strings: 
      ##  ...'parent_doi', 'doi', 'citation', 'url', 'path'
      
      #### Correct metadata$parameter ####
      
      #A comment on the data age material got split 
      #...between two column entries here
      #...combine them into the same entry for one specific
      #...doi
      if(xx$doi == "10.1594/PANGAEA.934274" &
         #check the length just in case this gets fixed 
         #...by the archive in the future
         length(xx$metadata$parameters) > length(names(xx$data))){
        
        #This was manually checked for the specific DOI and is not
        #...generalizable
        xx$metadata$parameters[[5]] <- c(xx$metadata$parameters[[5]],
                                         xx$metadata$parameters[[6]])
        
        xx$metadata$parameters[6] <- NULL
      }else if(length(xx$metadata$parameters) != 
               length(names(xx$data))){
        stop('parameter description length does not match data columns')
      }
      
      
      #### Process data, and metadata$parameter #######
      
      # To pivot the layers we need to ensure unique 
      #... column names. If they are not unique, then add
      #... a number to the end of them
      #if(length(names(xx$data)) > 
      #   length(unique(names(xx$data)))){
      #  #TODO make this more elegant, we may need to read 
      #  #...in and parse the txt files instead of relying 
      #  #...on pangear
      #  names(xx$data) <- paste(names(xx$data), 
      #                          1:length(names(xx$data)))
      #}
      
      #track the column name and numbers from the data
      column.df <- tibble::tibble(
        #pull in unique header id out of the column names
        #...this column names are generally formated as
        #...'name here [unit] (method)'
        header_id = names(xx$data) |>
          #remove anything after a ( or [ to get 'name here'
          stringr::str_remove(pattern = 
                                stringr::regex('(\\[|\\().*$')) |>
          #clean up whitespaces at the start and end
          stringr::str_trim(),
        header_unit = names(xx$data) |>
          #extract anything between [ ] to get 'unit'
          stringr::str_extract(pattern = 
                                 stringr::regex('(?<=\\[).*(?=\\])')),
        header_method = names(xx$data) |>
          #extract anything after the ( to get the 'method)'
          #TODO this is an imperfect regular expression but many of
          #...the methods appear truncated anyway. Fix this later
          stringr::str_extract(pattern = 
                                 stringr::regex('\\(.*$')),
        header = names(xx$data),
        #header names do not match parameters exactly (possible
        #...spreadsheet truncation), go by position instead
        column_number = paste0('V', 1:length(names(xx$data))))
      
      
        # Create a description column to the column_number
        #... The formatting here is not consistent so
        #... bind everything together as a human readable
        #... description and add it to the column_number
        #... table
      parameters.df <- column.df |>
        mutate(table_name = 'metadata$parameters',
               #Take each element of the parameters list, which is
               #...also a list, and collapse it into a single string
               #...to treat this as an entry
          with_entry = unlist(
          lapply(xx$metadata$parameters, 
                 function(yy){
                   return(paste(as.character(yy), 
                                collapse = ' '))
                 })
          )
          ) |>
        #Add in the of_variable and is_type from the annotations
        #...for the parameter elements
        dplyr::left_join(primaryAnnotations, 
                          by = dplyr::join_by(header_id, table_name))
        
      
      # rename the columns by index, this is done because some
      #... headers are truncated and don't match the parameters
      #... exactly. Moving over to an 'V[INDEX]' format instead.
      temp <- xx$data
      names(temp) <- paste0('V', 1:length(temp))
      
      data.df <- temp |>
        #To avoid type conflicts in the pivot, convert
        #...everything to characters
        dplyr::mutate(dplyr::across(.cols = tidyselect::everything(), 
                                    as.character)) |>
        #Add a row number to ensure a unique identifier,
        #...make sure it is also a character
        dplyr::mutate(row_number = paste0('R', 1:dplyr::n())) |>
        #Create a general row_id-column_number-with_entry
        #...tuple which is the basis for our general long
        #...format by pivoting
        tidyr::pivot_longer(cols = -row_number, 
                            names_to = 'column_number',
                            values_to = 'with_entry',
                            #keep table small(ish) by dropping NA
                            values_drop_na = TRUE) |>
        # adding table_name column so that we can keep
        # ...adding information from the metadata later
        dplyr::mutate(table_name = 'data') |>
        #Join in the breakdown of the columns
        #...to link in the header ID for that column_number
        dplyr::left_join(column.df,
                          by = dplyr::join_by(column_number)) |>
        #Add in the of_variable and is_type from the annotations
        dplyr::left_join(primaryAnnotations, 
                          by = dplyr::join_by(header_id, table_name)) 
      
      #### Check primary list entries ####
      
      #list out all the possible names for the study info
      #... that will be found at the top level list
      primaryNames <- intersect( names(xx), 
                                 c('parent_doi', 'doi', 'citation', 
                                   'url', 'path'))
      
      #These primary names combined with metadata and data
      #...should be the only elements of the list
      if(any( !(names(xx) %in% 
                c(primaryNames, 'metadata', 'data')))){
        warning(
          paste('unexpected elements at primary level:', 
                xx$doi,
                setdiff(names(xx),
                        c(primaryNames,'metadata', 'data'))
          )
        )
      }
      
      #deal with reading in the primary data after we have
      #...also checked the info in the metadata list
      
      #### Check metadata list entries ####
      
      #list out all the possible names for the study info
      #... that will be found at the metadata list
      metaNames <- intersect(
        names(xx$metadata), 
        c("citation", "related_to", "further_details",
          "projects" , "coverage", "abstract", "keywords", "status",
          "license", "size", "comment", "change_history"))
      
      if(any( ! (names(xx$metadata) %in% 
                 c(metaNames, 'events', 'parameters')))){
        warning(
          paste('unexpected elements at metadata level:', 
                xx$doi,
                setdiff(
                  names(xx$metadata),
                  c(metaNames, 'events', 'parameters')
                )
          )
        )
      }
      
      
      #deal with reading in the metadata after we have
      #...processed the events and parameters
      
      #### Check metadata$events ####
      
      #Pull in the study information from the 'events'
      #... data frame. Again there should be no items that
      #... are not matching the array here so that we know
      #... what to expect
      eventsNames <- intersect(
        names(xx$metadata$events), 
        c("LATITUDE", "LONGITUDE",
          "ELEVATION", "ELEVATION START", "ELEVATION END",
          "Penetration", "Recovery",
          "LOCATION", "METHOD/DEVICE", 
          "COMMENT"))
      
      #Instead of just checking the `names` we need to take
      #...out the first element of the name which is
      #...the name of the current core using `[-1]`
      if(any( !(names(xx$metadata$events)[-1] %in%  
                c("LATITUDE", "LONGITUDE", "ELEVATION",
                  "ELEVATION START", "ELEVATION END", 
                  "Penetration","Recovery",
                  "LOCATION", "METHOD/DEVICE",
                  "COMMENT")))){
        
        warning(
          paste('unexpected information in metadata$events:',
                xx$doi,
                setdiff(
                  names(xx$metadata$events)[-1],
                  c("LATITUDE", "LONGITUDE", "ELEVATION",
                    "ELEVATION START", "ELEVATION END", 
                    "Penetration","Recovery",
                    "LOCATION", "METHOD/DEVICE",
                    "COMMENT")
                ))
        )
      }
      
      #deal with reading in the metadata$events next
      
      #### Process primary, metadata, and metadata$events ####
      
      # read in the primary, metadata and metadata$events
      #...lists that we just checked above
      study.df <- tibble::tibble(
        table_name = c(rep('.', times = length(primaryNames)),
                       rep('metadata', times = length(metaNames)),
                       'metadata$events',
                       rep('metadata$events', 
                           times = length(eventsNames))),
        header = c(primaryNames,
                        metaNames, 
                        'core_name',
                        eventsNames),
        with_entry = c(
          unlist(xx[primaryNames]),
          unlist(xx$metadata[metaNames]),
          #catch the core name separately since it's in
          #...the key rather then the element itself
          names(xx$metadata$events)[1],
          unlist(xx$metadata$events[eventsNames])
        ) ) |>
        #Similar to processing the parameter list above, 
        #...pull out the id unit method
        dplyr::mutate(
          header_id = stringr::str_remove(string = header,
                                          pattern = stringr::regex(
                                            '(\\[|\\().*$')) |>
            stringr::str_trim(),
          header_unit = stringr::str_extract(string = header,
                                             pattern = stringr::regex(
                                               '(?<=\\[).*(?=\\])')),
          header_method = stringr::str_extract(string = header,
                                               pattern = stringr::regex(
                                                 '\\(.*$'))) |>
        dplyr::left_join(primaryAnnotations,
                         by = dplyr::join_by(table_name, header_id),
                         relationship = "many-to-many") 
      
      ### Stack everything and return as a single table ####
      ans <- dplyr::bind_rows(parameters.df, study.df,
                              data.df)
      
      return(ans)
    })
  
  ### Clean up long table ####
  #Set the table and column IDs to match the annotations
  temp <- long.df |>
    # the column ids are shorter versions of the column names
    #... column names often have units or methods in () or []
    #... remove those as well as any space pads. Save that as a column_id
    dplyr::select(doi, table_name,  column_number, row_number, #primary ids
                  #below are the header_X columns that need to be merged
                    header, header_id, header_unit, header_method, 
                  #information columns to be joined with header_X
                   is_type, of_variable, with_entry) |>
    #first spread things out
    tidyr::pivot_wider(names_from = is_type,
                       values_from = with_entry) |>
    #then make it longer including the header_X columns
    tidyr::pivot_longer(
      cols = -c(doi, table_name, column_number, row_number, 
                of_variable),
      names_to = 'is_type',
      values_to = 'with_entry',
      values_drop_na = TRUE)

  
  return(list(long = temp, 
              annotations = CPEAT.annotations
  ))
}
