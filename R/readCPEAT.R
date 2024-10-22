#' CPEAT data sets
#' 
#'
#'
#' @param dataDir filename of the download directory to store the data in
#' @param annotationFilename filename for the data annotations csv file 
#' @param format character flag to either return a list of cores or the cores as a set of tables
#' @param verbose boolean status to print out updates
#'
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
                      fileCount = Inf,
                      verbose=FALSE){
  
  ##Dev sets
  #dataDir <- 'temp/CPEAT'
  #verbose <- TRUE
  #fileCount <- 10
  #annotationFilename <- 'data/annotations_CPEAT3.csv'
  # 
  
  readSingleFile <- function(filename, 
                             format = c('original', 'long')[1],
                             verbose = FALSE){
    
    fileread <- read_lines(filename) |>
      paste(collapse = '\n')
    
    
    ##### Parse meta data ####
    
    metaData <- str_extract(fileread, 
                            pattern = regex('(?<=/\\* DATA DESCRIPTION:).*(?=\\*/)',
                                            dotall = TRUE)) |>
      trimws()
    
    temp <- metaData |>
      str_split(pattern = '\n(?!\t)') |>
      unlist() |>
      str_split(pattern = '\t') 
    
    tempNames <- lapply(temp, first)|>
      unlist() |>
      str_remove(regex(':$'))
    
    names(temp) <- tempNames
    
    metaList <- llply(temp, .fun = function(xx){
      ans <- xx[-1] #remove name
      
      if(str_detect(xx[1], 'DATA DESCRIPTION') |
         xx[1] == ""){
        return(NULL)
      }
      
      if(str_detect(xx[1], 'Related to') | 
         str_detect(xx[1], 'Project') |
         str_detect(xx[1], 'Citation') |
         str_detect(xx[1], 'License') |
         str_detect(xx[1], 'Size') |
         str_detect(xx[1], 'Abstract') |
         str_detect(xx[1], 'Keyword') |
         str_detect(xx[1], 'Status') |
         str_detect(xx[1], 'Comment') |
         str_detect(xx[1], 'Further details') |
         str_detect(xx[1], 'Change history')){
        ans <- paste(ans, collapse = '; ')
        return(ans)
      }
      
      if(str_detect(xx[1], 'Event')){
        allEvents <- str_split(ans, pattern = '\\s?\\*\\s?')
        coreName <- unlist(allEvents)[1]
        eventNames <- c('NAME', str_extract(unlist(allEvents)[-1], regex('[^:]*(?=:)')))
        eventValues <- c(list(coreName), str_remove(unlist(allEvents)[-1], regex('[^:]*:\\s*')))
        names(eventValues) <- eventNames
        return(eventValues)
      }
      
      if(str_detect(xx[1], 'Parameter')){
        allParms <- str_split(ans, ' \\* ')
        parameter.ls <- lapply(allParms, function(yy){
          yy_names <- str_remove(yy, regex(':.*$', dotall = TRUE)) |>
            trimws()
          yy_values <- str_extract(yy, regex('(?<=: ).*\\n?$')) |>
            trimws()
          yy_values[1] <- yy_names[1]
          yy_names[1] <- 'description'
          yy_values[is.na(yy_values)] <- TRUE
          setNames(as.list(yy_values), yy_names)
        })
        names(parameter.ls) <- paste0('V', 1:length(parameter.ls))
        
        return(parameter.ls)
      }
      
      if(str_detect(xx[1], 'Coverage')){
        temp2 <- str_split(ans, ' \\* ')|>
          unlist() |>
          trimws()
        allCoverage <- setNames(as.list(str_remove(temp2, '.*: ')), 
                                str_extract(temp2, '.*(?=:)'))
        return(allCoverage)
      }
      
      print(xx)
      stop('Parser flag is not recognized.')
    })
    
    #### Read primary data ####
    
    primaryData <- str_extract(fileread, 
                               pattern = regex('(?<=\\*/\n).*', dotall = TRUE)) |>
      read_tsv(col_types = cols(.default = col_character()),
               name_repair = 'unique_quiet') |>
      mutate(across(.cols = everything(), .fns = trimws))
    
    #### Return orginal format ####
    
    if(format == 'original'){
      return(list(meta = metaList, data = primaryData))
    }
    
    ### Convert to tuple format
    
    temp.df <- as.tibble(as.list(unlist(metaList))) |>
      pivot_longer(cols=everything(), values_to = 'with_entry') |>
      separate_wider_delim(cols = name, delim = '.',
                           names = c('table_name', 'header_name', 'is_type'),
                           too_few = 'align_start') |>
      mutate(header_name = if_else(is.na(header_name), table_name, header_name),
             is_type = if_else(is.na(is_type), 'value', is_type)) |>
      mutate(table_name = if_else(table_name == header_name, '.', table_name)) |>
      mutate(across(.cols = everything(), .fns = trimws)) |>
      mutate(column_id = if_else(str_detect(header_name, pattern = '^V\\d+$'),
                                 header_name, NA))
    
    column_header.df <- temp.df |>
      filter(!is.na(column_id),
             is_type == 'description') |>
      mutate(header_name = str_extract(with_entry, 
                                       pattern = regex('[^\\(|\\[]*(?= \\(|\\[)')) |>
               trimws()|>
               str_to_sentence()) |>
      select(header_name, column_id) 
    
    meta.df <- column_header.df |>
      full_join(temp.df |>
                  filter(!is.na(column_id)) |>
                  select(-header_name),
                by = join_by(column_id)) |>
      bind_rows(temp.df |>
                  filter(is.na(column_id)))
    
    #if there is only one PI then move that information up from the column level
    if(length(unique(meta.df$with_entry[meta.df$is_type == 'PI'])) == 1){
      meta.df <- meta.df |>
        mutate(header_name = if_else(is_type == 'PI', 'PI', header_name),
               column_id = if_else(is_type == 'PI', NA_character_, column_id)) |>
        mutate(is_type = if_else(is_type == 'PI', 'value', is_type)) |>
        unique()
    }
    
    
    ## add in the column and row IDs
    temp <- primaryData
    
    #use the traditional R 'V' notation for vertical columns
    names(temp) <- paste0('V', 1:ncol(primaryData))
    rowIDs <- paste0('R', 1:nrow(primaryData))
    temp <- temp |>
      mutate(observation_id = rowIDs) 
    
    
    
    data.df <- temp |>
      pivot_longer(cols = -observation_id, 
                   names_to = 'column_id', values_to = 'with_entry') |>
      mutate(is_type = 'value') |>
      mutate(table_name = 'data') |>
      full_join(column_header.df, 
                by = join_by(column_id))
    
    ans <- bind_rows(meta.df, data.df) |>
      select(table_name, header_name, column_id, observation_id, 
              is_type, with_entry)
    
    return(ans)
  }

  ### download files from url
  
  download_urls <- read_delim(file = annotationFilename, 
                              delim = ';', show_col_types = FALSE) |>
    filter(of_variable == 'download_url') |>
    select(download_url = with_entry) |>
    mutate(doi = str_extract(download_url, '10.1594.*\\d')) |>
    mutate(filename = file.path(dataDir, 
                                sprintf('%s.txt', 
                                        str_replace_all(doi, '/', '_')))) |>
    head(n = fileCount)
  
  if(verbose) message(sprintf('Downloading %d of %d files ...',
                              sum(!file.exists(download_urls$filename)),
                              nrow(download_urls)))
  
  for(ii in 1:nrow(download_urls)){
    if(ii %% 50 == 1 & verbose) cat(paste(ii, '... '))
    
    if(!file.exists(download_urls$filename[ii])){
      download.file(download_urls$download_url[ii],
                    download_urls$filename[ii],
                    quiet=TRUE)
    }
  }
  
  if(verbose) message('Downloads done.')
  
  inputFiles <- list.files(path = dataDir, full.names = TRUE) |>
    as.list() |>
    head(n = fileCount)
  
  names(inputFiles) <- str_remove(basename(unlist(inputFiles)), '.txt')
  
  metaData <- read_delim(file = annotationFilename, 
                              delim = ';', show_col_types = FALSE)
  
  if(format =='original'){
    orgData <- llply(inputFiles, .fun = readSingleFile, format = 'original')
    return(list(annotations=metaData,
                orginial = orgData))
  }else{
    
    longData <- ldply(inputFiles, 
                      .id = 'doi', .fun = readSingleFile, 
                      format = 'long') |>
      full_join(metaData |>
                  filter(with_entry == '--') |>
                  select(-with_entry),
                by = join_by(table_name, header_name, is_type))
    
    return(list(annotations = metaData |>
                  filter(with_entry != '--'),
                long = longData))
  }
  
}
