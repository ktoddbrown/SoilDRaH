#' Read all CPEAT cores
#'
#' @param dataDir location for the data download
#' @param dataLevel a flag for a level0 (complete original) or level 1 (standardized) data return
#' @param verbose flag for more messages
#'
#' @returns a list with the data tables with citations and methods included for level 0
readCPEAT2025 <- function(dataDir, dataLevel = c('level0', 'level1')[1],
                          verbose = FALSE){
  
  #dataDir <- '01_DataRescue/CPEAT2025'
  
  dataDownload.dir <- file.path(dataDir, 'temp', 'CPEAT')
  methodsCitation.file <- file.path(dataDir, 'CPEAT2025_Methods.bib')
  primaryCitation.file <- file.path(dataDir, 'CPEAT2025.bib')
  methods.file <- file.path(dataDir, 'CPEAT2025_Methods.md')
  
  #### Process the download URLs from bib citation ####
  allMethodCitations <- bibtex::read.bib(file = methodsCitation.file)
  
  allURLs.df <- plyr::ldply(allMethodCitations, 
                            .fun = function(xx){
                              return(c(primary = xx$url))
                            },
                            .id = 'bibKey') |>
    dplyr::filter(bibKey != 'Loisel2021aa') |>
    dplyr::mutate(downloadData = 
                    sprintf('%s?format=textfile', 
                            stringr::str_replace(primary, 
                                                 'doi.org', 'doi.pangaea.de')))
  
  #### Download data files ####
  
  basenames.arr <- allURLs.df$downloadData |> 
    # start with something like:
    # "https://doi.pangaea.de/10.1594/PANGAEA.889936?format=textfile"
    stringr::str_extract(pattern = '(?<=pangaea.de/).*(?=.format)') |> 
    # extract the string between the doi url and the format information:
    # 10.1594/PANGAEA.889936
    stringr::str_replace(pattern = '/', replacement = '_') |>
    # replace the forward slash so that it is not treated as a folder
    # 10.1594_PANGAEA.889936
    paste0('.txt') 
  # add the extension
  
  download.flag <- !file.exists(file.path(dataDownload.dir, basenames.arr))
  
  if(any(download.flag)){
    download.file(url = allURLs.df$downloadData[download.flag], 
                  destfile = file.path(dataDownload.dir,
                                       basenames.arr[download.flag]))
  }
  
  #### read text ####
  
  basenames.ls <- setNames(object = as.list(basenames.arr),
                           allURLs.df$bibKey)
  #str_remove(basenames.arr, '.txt'))
  
  all.text <- plyr::llply(.data = basenames.ls, 
                          .fun = function(basename.str){
                            readr::read_file(file.path(dataDownload.dir, 
                                                       basename.str))
                          })
  
  #### Extract the header information from the raw text ####
  
  all.meta <- plyr::ldply(
    .data = all.text,
    .fun = 
      function(file.str){
        #### Parse the first level of the metadata ####
        #file.str <- all.text$`10.1594_PANGAEA.928061`
        #Pull everything between /* and */ as the header
        header.str <- file.str |>
          stringr::str_extract(regex('(?<=/\\* ).*(?=\\*/)', 
                                     multiline=TRUE, dotall=TRUE))
        
        #split out the metadata using returns (\n) tabs (\t) and :
        meta.df <- header.str |>
          stringr::str_split_1('(?<=\\n)[^\\t]*\\:') |>
          as.list() |>
          setNames(c('meta', 
                     stringr::str_extract_all(header.str,
                                              '(?<=\\n)[^\\t]*\\:') |> 
                       unlist() |>
                       stringr::str_remove('\\:$'))) |>
          tibble::as_tibble_row()
        return(meta.df)
      }, .id = 'bibKey') 
  
  #### Extract the primary data table ####
  
  all.primary <- plyr::ldply(.data = all.text, .fun = function(file.str){
    
    #### Parse the primary data file ####
    
    header.str <- file.str |> #all.text[388] |> #
      stringr::str_extract(regex('(?<=\\*/\\n)[^\\n]*', 
                                 multiline=TRUE, dotall=TRUE)) |>
      stringr::str_split(patter = '\\t') |>
      unlist()
    
    #"10.1594_PANGAEA.928439" has an extra 3 at the end of the 
    #file randomly, this throws a warning but does not affect how the 
    #data is read in
    primary.df <- file.str |> #all.text[388] |> #
      stringr::str_extract(regex('(?<=\\*/\\n).*', multiline=TRUE, dotall=TRUE)) |>
      readr::read_tsv(col_types = readr::cols(.default = readr::col_character()),
                      skip = 1, col_names = FALSE) |>
      dplyr::mutate(across(.cols = tidyselect::everything(), .fns = trimws)) |>
      dplyr::mutate(row_id = paste0('R',1:n())) |>
      tidyr::pivot_longer(cols = -row_id, 
                          names_to = 'column_index', values_to = 'with_entry') |>
      dplyr::full_join(tibble(column_index = paste0('X', 1:length(header.str)),
                              column_name = header.str),
                       by = dplyr::join_by(column_index))
    
    return(primary.df)
  }, .id = 'bibKey')
  
  #### core data (pre-processing) ####
  #### to be broken up into coverage, event, and parameter information
  core.df <- all.meta |>
    dplyr::select(bibKey, coverage = Coverage,
                  event = `Event(s)`, parameters = `Parameter(s)`) |>
    tidyr::separate_wider_delim(cols = parameters, 
                                delim = '\t',
                                names = paste0('X', 0:14),
                                too_few = 'align_start')
  
  
  #identify and process the sub-patterns
  structures.df <- all.meta |>
    dplyr::select(Coverage, `Event(s)`, Size, `Parameter(s)`) |>
    dplyr::mutate(
      Coverage_pattern = 
        stringr::str_replace_all(Coverage, '\\-?\\d*\\.?\\d+', '%d'),
      Events_pattern = 
        stringr::str_replace(`Event(s)`, '^[^\\*]*\\*', '%s') |>
        stringr::str_replace(':[^\\*]*$', ': %s') |>
        stringr::str_replace_all(':[^\\*]*\\*', ': %s'),
      Size_pattern = 
        stringr::str_replace_all(Size, '\\d+', '%d'))
  
  ##Do this --
  ## - [x] Coverage: Separate variables based on * and \n, then by [name] : [entry], then extract unit from entry. Strip white space
  ## - [x] Events: Separate variables based on * , then by [name] : [entry], process Comments via ; split, then by [name] : [entry]
  ## -[x] Parameters: Separate and process the parameter descriptions by str subtraction
  
  ### size data ####
  ### number of data points for each bibKey
  size.df <- all.meta |>
    dplyr::select(bibKey, Size) |>
    dplyr::mutate(value = stringr::str_extract(Size, '\\d+'),
                  unit = stringr::str_remove(Size, '\\d+'),
                  of_variable = 'data_size') |>
    dplyr::mutate(dplyr::across(tidyselect::everything(), trimws)) |>
    tidyr::pivot_longer(cols = c(value, unit), 
                        names_to = 'is_type', values_to = 'with_entry') |>
    dplyr::select(bibKey, of_variable, is_type, with_entry)
  
  #### coverage data ####
  #### lat/long and core depth interval
  coverage.df <- all.meta |>
    dplyr::select(bibKey, temp_str = Coverage) |>
    dplyr::mutate(temp_str = trimws(temp_str)) |>
    tidyr::separate_longer_delim(temp_str, 
                                 delim = stringr::regex('\\*|\\n')) |>
    tidyr::separate_wider_delim(temp_str, 
                                delim = ':',
                                names = c('of_variable', 'text')) |>
    dplyr::mutate(dplyr::across(tidyselect::everything(), trimws)) |>
    dplyr::mutate(value = stringr::str_remove(text, ' c?m$'),
                  unit = trimws(stringr::str_extract(text, ' c?m$'))) |>
    dplyr::select(-text) |>
    tidyr::pivot_longer(cols = c(value, unit),
                        names_to = 'is_type', values_to = 'with_entry',
                        values_drop_na = TRUE) |>
    dplyr::mutate(dplyr::across(tidyselect::everything(), trimws))
  
  #### event data (pre-processing) ####
  #### lat/long, location (region), elevation, method and device (coring), 
  #### core recovery length, core penetration depth,
  ####  and structured comments (processed next)
  events.df <- all.meta |>
    dplyr::select(bibKey, temp_str = `Event(s)`) |>
    dplyr::mutate(core_name =  
                    stringr::str_extract(temp_str, '^.*(?= \\* LATITUDE)')) |>
    dplyr::mutate(temp_str =  
                    stringr::str_remove(temp_str, '^.* \\* (?=LATITUDE)')) |>
    tidyr::separate_wider_delim(temp_str, names = paste0('X', 1:10), 
                                delim = ' * ',
                                too_few = "align_start") |>
    tidyr::pivot_longer(cols = starts_with('X'),
                        names_to = 'drop_names',
                        values_to = 'component',
                        values_drop_na = TRUE) |>
    dplyr::mutate(is_type =  stringr::str_extract(component, '^[^:]+'),
                  with_entry =  stringr::str_remove(component, '^[^:]+: ')) |>
    dplyr::select(bibKey, is_type, with_entry) |>
    dplyr::mutate(dplyr::across(tidyselect::everything(), trimws)) |>
    tidyr::pivot_wider(names_from = is_type, values_from = with_entry)
  
  ### event comment data ####
  ### coring year, peatland type, basal age, basal age depth, core microtopography, core length, number of dates, carbon rate site, peat properties sample size, bulk density sample volumne, surface age, altitude, site name, and comment
  events_comment.df <- events.df |>
    dplyr::select(bibKey, COMMENT) |>
    #remove reference to core name in comments
    dplyr::mutate(
      COMMENT =  stringr::str_remove(COMMENT,
                                     '^.*e[ta]+ils.*(?=c|Coring year)')) |>
    #fix typos that hinder parsing
    dplyr::mutate(
      COMMENT =  stringr::str_replace(COMMENT, 
                                      'sphagnum, Volume of bulk density samples',
                                      'sphagnum; Volume of bulk density samples')) |>#X3
    dplyr::mutate(
      COMMENT =  stringr::str_replace(COMMENT, 
                                      'Stordalen_core2 Dominant peat type',
                                      'Stordalen_core2; Dominant peat type')) |> #X2
    dplyr::mutate(
      COMMENT =  stringr::str_replace(COMMENT, 
                                      'KOE.', 
                                      'KOE')) |>
    dplyr::mutate(
      COMMENT =  stringr::str_replace(COMMENT, 
                                      '17 samples:', 
                                      '17 samples - ')) |>
    dplyr::mutate(
      COMMENT =  stringr::str_replace(COMMENT, 
                                      'uncal., basal', 
                                      'uncal.; basal')) |>
    dplyr::mutate(
      COMMENT =  stringr::str_replace(COMMENT, 
                                      'basal age 1', 
                                      'basal age: 1')) |>
    # parse separaters
    tidyr::separate_longer_delim(cols = 'COMMENT', delim = ';') |>
    dplyr::mutate(COMMENT = trimws(COMMENT)) |>
    tidyr::separate_wider_delim(cols = COMMENT, delim = ':',
                                names = c('of_variable', 'with_entry'), 
                                too_few = 'align_end') |>
    #remove things that are NA for both the with_entry and of_variable
    filter(!(is.na(with_entry) & is.na(of_variable))) |>
    #make unspecified of_variables comments
    dplyr::mutate(of_variable = dplyr::if_else(is.na(of_variable), 
                                               'COMMENT', of_variable)) |>
    #specify where things came from in the variable name
    dplyr::mutate(of_variable = paste0('EVENT::COMMENT::',
                                       str_to_lower(of_variable)))
  
  ### event (clean) data ####
  ### lat/long, location (region), elevation, method and device (coring), 
  ### core recovery length, and core penetration depth
  ### Note structured comments are processed above separately
  events_clean.df <- events.df |>
    #remove the COMMENT that we already processed above
    dplyr::select(-COMMENT) |>
    tidyr::pivot_longer(-bibKey, names_to = 'of_variable', values_to='temp',
                        values_drop_na = TRUE) |>
    #Separate the values from their units (in this case units are all cm or m)
    dplyr::mutate(value =  stringr::str_remove(temp, ' c?m$'),
                  unit = trimws(str_extract(temp, ' c?m$'))) |>
    #drop the intermediary place holder
    dplyr::select(-temp) |>
    #put things into a data tuple
    tidyr::pivot_longer(cols = c(value, unit), 
                        names_to = 'is_type', values_to = 'with_entry', 
                        values_drop_na = TRUE) |>
    #Track where the variable lives
    dplyr::mutate(of_variable = paste0('EVENT::', of_variable))
  
  #### column information ####
  #### column data model includes column index, name as `of_variable`, and `is_type` including: description, unit, comment, MethodDevice, geocode, and PI
  column_meta.df <- all.meta |>
    dplyr::select(bibKey, `Parameter(s)`) |>
    #separate the parameter information by tabs
    tidyr::separate_wider_delim(cols = `Parameter(s)`, 
                                delim = '\t',
                                names = paste0('X', 0:14),
                                too_few = 'align_start') |>
    #drop the empty leading column
    dplyr::select(-X0) |>
    #track the column index because the names are not always unique
    tidyr::pivot_longer(cols = starts_with('X'),
                        names_to = 'column_index',
                        values_to = 'column_name',
                        values_drop_na = TRUE) |>
    #mutate(column_name_match =  stringr::str_detect(column_name, '^.+ \\*( GEOCODE \\*)? PI:.+( \\* METHOD/DEVICE:.+)?( \\* COMMENT:.+)?$')) |>
    # match specific patterns for a set of tags
    dplyr::mutate(
      comment =  stringr::str_extract(column_name, '(?<= \\* COMMENT:).+$'),
      MethodDevice =  stringr::str_extract(
        stringr::str_remove(column_name, '( \\* COMMENT:.+)?$'), #trim the end
        '(?<= \\* METHOD/DEVICE:).+$'), #extraction pattern
      PI =  stringr::str_extract(
        stringr::str_remove(column_name, 
                            '( \\* METHOD/DEVICE:.+)?( \\* COMMENT:.+)?$'), #trim
        '(?<= PI: ).+$'), #extract
      geocode =  stringr::str_extract(column_name, 'GEOCODE'),
      #make the unmatched string smaller based on what doesn't match above
      temp =  stringr::str_remove(
        column_name,
        ' \\*( GEOCODE \\*)? PI:.+( \\* METHOD/DEVICE:.+)?( \\* COMMENT:.+)?$')) |>
    dplyr::mutate(description =  stringr::str_remove(temp, '[\\[\\(].*$'),
                  unit =  stringr::str_extract(temp, '(?<=\\[).+(?=\\])'),
                  of_variable =  stringr::str_extract(temp, '(?<=\\().+(?=\\))')) |>
    #remove the columns that have now been processed
    dplyr::select(!c(column_name, temp)) |>
    dplyr::mutate(dplyr::across(tidyselect::everything(), trimws)) |>
    tidyr::pivot_longer(cols = -c(bibKey, column_index, of_variable),
                        names_to = 'is_type',
                        values_to = 'with_entry',
                        values_drop_na = TRUE)
  
  ###Compile study-level data####
  ###put everything together except the column and layer data
  study_meta.df <- all.meta |>
    #trim out the things that don't need heavy processing
    dplyr::select(bibKey, citation = Citation, 
                  replaced_by = `Replaced by`, 
                  related_to = `Related to`, 
                  further_details = `Further details`,
                  projects = `Project(s)`, 
                  change_history = `Change history`, 
                  license = License, 
                  abstract = Abstract, 
                  keywords = `Keyword(s)`, 
                  status = Status, 
                  header_comment = Comment) |>
    tidyr::pivot_longer(cols = -bibKey, 
                        names_to = 'of_variable', values_to = 'with_entry',
                        values_drop_na = TRUE) |>
    dplyr::mutate(is_type = 'value') |>
    #pull in the processed data from above
    dplyr::bind_rows(size.df,
                     events_clean.df,
                     events_comment.df,
                     coverage.df) |>
    dplyr::mutate(dplyr::across(tidyselect::everything(), trimws))
  
  
  
  ####Create level 0 data list####
  
  data.lvl0.ls <- list(
    #Read in a list of all the bib files
    citation = list(
      #Citation for the article transcriptions are pulled from
      primary = bibtex::read.bib(file = primaryCitation.file), 
      #Citations for all referenced articles
      methods = bibtex::read.bib(file = methodsCitation.file)
    ),
    #Read in the text transcription of the primary website
    method = readr::read_lines(file = methods.file),
    #Read in the downloaded data
    data =  list(study = study_meta.df,
                 columns = column_meta.df,
                 primary = all.primary)
  )
  
  if(dataLevel == 'level0'){
    return(data.lvl0.ls)
  }
  
  ####Data Level 1####
  
  ####Remove the replaced cores####
  #Find the dois of each core
  doi_core <- data.lvl0.ls$data$study |>
    #remove 112 dois that have been replaced
    dplyr::filter(of_variable %in% 'citation') |>
    dplyr::mutate(doi = stringr::str_extract(with_entry, 
                                             '(?<=https://doi.org/).*$'))
  
  #Find the cores that are flagged as replaced and the replacement doi
  replaced_core <- data.lvl0.ls$data$study |>
    #remove 112 dois that have been replaced
    dplyr::filter(of_variable %in% 'replaced_by') |>
    dplyr::mutate(
      replacement_doi = stringr::str_extract(with_entry, 
                                             '(?<=https://doi.org/).*$'))
  
  if(all(replaced_core$replacement_doi %in% doi_core$doi)){
    if(verbose) message('Replacement cores present in dataset')
  }else{
    warning('There are missing replacement cores. Dropping cores that were not replaced.')
  }
  
  # Take the cores out of each of the data frames
  study.df <- data.lvl0.ls$data$study |>
    dplyr::filter(!('replaced_by' %in% of_variable), .by = bibKey)
  
  column.df <- data.lvl0.ls$data$columns |>
    dplyr::filter(bibKey %in% study.df$bibKey)
  
  primary.df <- data.lvl0.ls$data$primary |>
    dplyr::filter(bibKey %in% study.df$bibKey)
  
  #### Move the PI info to the study data ####
  #There is a lot or repeated PI's in the column
  pi.df <- column.df |>
    dplyr::filter(is_type == 'PI') |>
    dplyr::select(bibKey, with_entry) |>
    unique() |>
    # #Confirm none of bibKeys have more then one PI, moving information to study.df
    # filter(n() > 1,
    #        .by = bibKey)
    dplyr::mutate(of_variable = 'PI',
                  is_type = 'value')
  
  #remove PI from the column.df
  column_noPI.df <- column.df |>
    dplyr::filter(is_type != 'PI') |>
    #drop geocode
    dplyr::filter(is_type != 'geocode') |>
    dplyr::mutate(across(everything(), trimws))
  
  # Create add the pi information to the study 
  study_PI.df <- study.df |>
    #add the PI to the study.df
    dplyr::bind_rows(pi.df)
  
  ####Create unique variable groups####
  ####some of the variables have the same name but different methods, within the 
  ####same core (!). Go through and add in an index to group the unique variable
  ####dimensions
  
  unique_column.df <- column_noPI.df |>
    tidyr::pivot_wider(names_from = is_type, values_from = with_entry) |>
    dplyr::reframe(doi_list = stringr::str_c(bibKey, column_index, 
                                             sep = '$', collapse = ';'),
                   .by = -c(bibKey, column_index))|>
    dplyr::mutate(variable_id = paste0('Variable_',1:n()) ) |>
    dplyr::rename(method = MethodDevice) |>
    tidyr::pivot_longer(cols = c('description', 'unit',
                                 'comment', 'method'),
                        names_to = 'is_type', 
                        values_to = 'with_entry', values_drop_na = TRUE)
  
  ####Move variable ID over to primary data####
  expanded_primary.df <- primary.df |>
    dplyr::full_join(unique_column.df |>
                       tidyr::separate_longer_delim(cols = doi_list, delim = ';') |>
                       tidyr::separate_wider_delim(cols = doi_list, delim = '$', 
                                                   names = c('bibKey', 'column_index')) |>
                       dplyr::select(bibKey, column_index, variable_id, of_variable) |>
                       unique(),
                     by = dplyr::join_by(bibKey, column_index)) |>
    dplyr::mutate(is_type = 'value')
  
  #Finally create the variable map that matches the ISCN4 data purpose
  lvl1_data.ls <- list(
    #Read in a list of all the bib files
    citation = list(
      #Citation for the article transcriptions are pulled from
      primary = bibtex::read.bib(file = primaryCitation.file), 
      #Citations for all referenced articles
      methods = bibtex::read.bib(file = methodsCitation.file)
    ),
    #Read in the text transcription of the primary website
    method = readr::read_lines(file = methods.file),
    data = plyr::llply(
      list(core = study_PI.df,
           variable = unique_column.df,
           primary = expanded_primary.df),
      .fun = function(xx){
        xx |>
          dplyr::mutate(of_variable_DRaH = dplyr::case_when(
            of_variable == 'C' ~ 'carbon_percent',
            of_variable == 'TC' ~ 'total_carbon_percent',
            of_variable == 'TIC' ~ 'inorganic_carbon_percent',
            of_variable == 'TOC' ~ 'organic_carbon_percent',
            of_variable == 'Corg dens' ~ 'organic_carbon_density',
            of_variable == 'DBD' ~ 'dry_bulk_density',
            of_variable == 'Depth bot' ~ 'layer_bottom',
            of_variable == 'Depth top' ~ 'layer_top',
            #of_variable == 'Depth sed' & str_detect(comment, 'LOI') ~ 'layer_middle_LOI',
            of_variable == 'Depth sed' ~ 'layer_middle',
            of_variable == 'Samp thick' ~ 'layer_thickness',
            of_variable == 'LOI' ~ 'loss_on_ignition',
            of_variable == 'OM' ~ 'organic_matter_percent',
            of_variable == 'OM dens' ~ 'organic_matter_density',
            of_variable == "Water wm" ~ 'field_water_percent',
            
            of_variable == 'citation' ~ 'citation',
            of_variable == 'related_to' ~ 'related_research',
            of_variable == 'projects' ~ 'projects',
            of_variable == 'license' ~ 'data_license',
            of_variable == 'header_comment' ~ 'core_comment',
            of_variable == "EVENT::LATITUDE" ~ 'latitude',
            of_variable == "EVENT::LONGITUDE" ~ 'longitude',
            of_variable == "EVENT::LOCATION" ~ 'region',
            of_variable == "EVENT::METHOD/DEVICE" ~ 'sampling_method',
            of_variable == "EVENT::ELEVATION" ~ 'elevation',
            of_variable == "EVENT::ELEVATION START" ~ 'elevation_start',
            of_variable == "EVENT::ELEVATION END" ~ 'elevation_end',
            of_variable == "EVENT::COMMENT::coring year" ~ 'observation_year',
            of_variable == "EVENT::COMMENT::type of peatland"  ~ 'peatland_class',
            of_variable == "EVENT::COMMENT::peatland type"  ~ 'peatland_class',
            of_variable == 'EVENT::COMMENT::site name' ~ 'site_name',
            of_variable == 'PI' ~ 'principal_investigator',
            TRUE ~ NA_character_
          )) |>
          dplyr::rename(of_variable_CPEAT = of_variable)
      }))
  
  if(dataLevel == 'level1'){
    return(lvl1_data.ls)
  }
  
  error('`dataLevel` provided does not match cases coded.')
  
}

