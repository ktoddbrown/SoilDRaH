#' Read Ares etal 2001
#'
#' Ares etal 2001 primarily measures tree growth parameters (including litterfall) but also characterized elemental fractions of soil. They use this data to look at factors affecting a specific invasive tree growth in Hawaii including nitrogen and moisture gradients.
#' 
#' A. Ares and J. H. Fownes. Productivity, resource use, and competitive interactions of fraxinus uhdei in hawaii uplands. Canadian Journal of Forest Research, 31(1):132-142, [https://doi.org/10.1139/x00-156](https://doi.org/10.1139/x00-156). 2001
#'
#' @param dataDir string with the directory address for where the data rescue files are located
#' @param dataLevel level of data product to be returned
#' @param verbose print out messages as processing data, currently not used
#'
#' @returns a list of data tibbles
#' @export
#' 
#' @importFrom bibtex read.bib
#' @importFrom readr read_lines read_csv cols col_character
#' @importFrom tibble tribble
#' @importFrom dplyr mutate n case_when bind_rows select filter arrange join_by left_join full_join case_match across
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom stringr str_extract
#' @importFrom tidyselect everything
#'
readAres2001 <- function(dataDir,
                         dataLevel = c('level0', 'level1', 'level2-HiSOC')[1],
                         verbose = TRUE){
  
  #coment out this before leaving
  #dataDir <- 'data/rescue/Ares2001'
  #   library(tidyverse)
  # library(kableExtra)
  # library(bibtex)
  # 
  methods.file <- file.path(dataDir, 'Ares2001_Methods.md')
  table1.file <- file.path(dataDir, 'Ares2001_Table1.csv')
  table3.file <- file.path(dataDir, 'Ares2001_Table3.csv')
  primaryCitation.file <- file.path(dataDir, 'Ares2001.bib')
  methodsCitation.file <- file.path(dataDir, 'Ares2001_Methods.bib')
  
  #dataLevel <- 'level0'
  
  ### Build Level 0 data ####
  ###
  #This chunk has two purposes. 
  #...1) Check the formatting by reading in everything 
  #...2) Create a list of everything to process later in level 1
  
  data.lvl0.ls <- list(
    #Read in a list of all the bib files
    citation = list(
      #Citation for the article transcriptions are pulled from
      primary = bibtex::read.bib(file = primaryCitation.file), 
      #Citations for all referenced articles
      methods = bibtex::read.bib(file = methodsCitation.file)
    ),
    #Read in the text transcription of the article's methods section
    method = readr::read_lines(file = methods.file),
    #Read in the results as tables or figure transcriptions. This includes
    #...the caption as well as the tables themselves
    data = list(
      Table1 = list(
        #Read the caption as a text string. Captions are the first cell on 
        #...the first row.
        caption = readr::read_csv(file = table1.file,
                                  col_types = readr::cols(.default = readr::col_character()),
                                  n_max = 1, col_names = FALSE)$X1[1],
        #Read in all the data, skipping the first row with the caption and read
        #...in the table as character. This element is a tibble (data.frame).
        primary = readr::read_csv(file = table1.file,
                                  col_types = readr::cols(.default = readr::col_character()),
                                  skip = 1)
      ), 
      #Same format as Table1
      Table3 = list(
        caption = readr::read_csv(file = table3.file,
                                  col_types = readr::cols(.default = readr::col_character()),
                                  n_max = 1, col_names = FALSE)$X1[1],
        primary = readr::read_csv(file = table3.file,
                                  col_types = readr::cols(.default = readr::col_character()),
                                  skip = 1)
      )))
  
  if(dataLevel == 'level0'){
      return(data.lvl0.ls)
  }
  
  ### Build level 1 data ####
  ### The purpose of this section is to create a harmonized data tuple that matches
  ### the SoilDRaH data model and vocabularies
  
  #Create table with data that is consistent across the study
  studyMeta <- tibble::tribble(~of_variable, ~is_type, ~with_entry, ~from_source,
                       'region', 'value', 'Honaunau Forest on the southwestern slopes of Mauna Loa, island of Hawaii', paste('Method ln5:', paste(data.lvl0.ls$method[5], collapse = ' ')), #no actual lat-long, need to get this translated to geo-location
                      'land_use', 'value', 'Plantation: Tree stands', paste('Method ln14-16:', paste(data.lvl0.ls$method[14:16], collapse = ' ')),
                      'land_use', 'interval', '1959/', paste('Method ln14:', data.lvl0.ls$method[14]),
                      'land_use', 'interval_format', 'YYYY/YYYY', NA,
                      'observation_time', 'value', '1996', paste('Method ln 30;53;58:', paste0(data.lvl0.ls$method[c(30,53,58)], collapse = '... ')),
                      'observation_time', 'format', 'YYYY', NA,
                       'citation', 'value', format(data.lvl0.ls$citation$primary), 'journal citation',
                       'doi', 'value', data.lvl0.ls$citation$primary$doi, 'journal citation')
  
  # Process the primary data from Table 1 by maintaining data groups and standardizing the variable name so that we can link this to the study metadata and data in the other tables when needed.
  Table1Primary <- data.lvl0.ls$data$Table1$primary |>
    #Add a row id preserving row grouping -- not unique
    dplyr::mutate(row_id = paste0('R', 1:dplyr::n())) |>
    #Transform the table to long data by row group and column names. This allows us to then create the cross links between the tables.
    tidyr::pivot_longer(cols = -row_id,
                 names_to = 'column_name', values_to = 'with_entry',
                 values_drop_na = TRUE) |> #drop missing values 
    #Link the variable names to the column names using a switch statement
    dplyr::mutate(of_variable = dplyr::case_when(
      column_name == "Stand type" ~ 'stand_type',
      column_name == 'Elevation (m)' ~ 'elevation',
      column_name == 'Soil type' ~ 'soil_class',
      column_name == 'Soil pH' ~ 'soil_ph',
      column_name == 'Soil organic carbon (%)' ~ 'soil_organic_carbon',
      column_name == 'Soil N (%)' ~ 'soil_nitrogen',
      column_name == 'Soil P (mg kg<sup>-1</sup>)' ~ 'soil_phosphorus',
      column_name == 'Stand age (years)' ~ 'stand_age',
           column_name == '*F. uhdei* Stem density (trees / ha)' ~ 'stem_density',
           column_name == '*F. uhdei* Mean DBH (cm)' ~ 'diameter_at_breast_height',
           column_name == '*F. uhdei* Mean height(m)' ~ 'height',
           column_name == '*A. koa* Stem density (trees / ha)' ~ 'stem_density',
           column_name == '*A. koa* Mean DBH (cm)' ~ 'diameter_at_breast_height',
           column_name == "*A. koa* Mean height(m)"~ 'height')) |>
    dplyr::mutate(is_type = if_else(str_detect(column_name, 'Mean'), 'mean', 'value')) |>
    #Flag the source of this data as Table 1, this table is merged later with other data so this keeps track of where the data came from
    dplyr::mutate(from_source = 'Table 1')
  
  # Create the metadata for table one from the header information and cited methods
  Table1Meta <- Table1Primary |>
    dplyr::select(column_name, of_variable) |>
    unique() |>
    #Grab everything between the parentheses as units and attribute the source as the column names.
    dplyr::mutate(unit = stringr::str_extract(column_name, pattern = '(?<=\\().*(?=\\))'),
                  species = case_when(str_detect(column_name, 'F. uhdei') ~ 'Fraxinus uhdei',
                             str_detect(column_name, 'A. koa') ~ 'Acacia koa',
                             .default = NA_character_),
           from_source = 'Table 1 column names.') |>
    #If there aren't units then drop the row
    dplyr::filter(!is.na(unit) | is.na(species)) |>
    #Stack into the table the methods for each variable that we can find
    dplyr::bind_rows(
      tibble::tribble(~of_variable, ~method, ~from_source,
              'soil_ph', paste0(data.lvl0.ls$method[72:73], collapse = ' '), 'Methods ln72-73', #pasting in a method from specific rows
              'soil_organic_carbon', paste0(data.lvl0.ls$method[72:73], collapse = ' '), 'Methods ln72-73',
              'soil_nitrogen', paste0(data.lvl0.ls$method[72:73], collapse = ' '), 'Methods ln72-73',
              'soil_phosphorus', paste0(data.lvl0.ls$method[72:73], collapse = ' '), 'Methods ln72-73',
              'soil_class', paste0(data.lvl0.ls$method[10:12], collapse = ' '), 'Methods ln10-12')  |>
        #Stack onto the tables the controlled vocabulary used
        dplyr::bind_rows(
          tibble::tribble(~of_variable, ~control_vocabulary, ~from_source,
                          'stand_type', '*F. uhdei*: pure stands of Fraxinus uhdei (Wenzig) Lingelsh|Mixed: mixed stands of *Fraxinus uhdei* (Wenzig) Lingelsh and *Acacia koa* Grey', 'Abstract ln1',
                          'soil_class', 'Histosol:USDA classification for histosol soil type|Andisols:USDA classification for andisol soil type', 'expert informed') )|>
        #put the column names back in
        dplyr::left_join(Table1Primary |>
                    dplyr::select(column_name, of_variable) |>
                    unique(),
                  by = dplyr::join_by(of_variable))
      
    ) |> #close methods stack
    # Push the dimensions of the variable into a single column. Making this a long table.
    tidyr::pivot_longer(cols = c(unit, species, method, control_vocabulary),
                 names_to = 'is_type',
                 values_drop_na = TRUE,
                 values_to = 'with_entry')
  
  
  #The following is doing the same process as above but fitted to the data and formatting of Table3
  Table3Primary <- data.lvl0.ls$data$Table3$primary |>
    tidyr::pivot_longer(cols = -variable,
                 names_to = 'row_id', values_to = 'with_entry',
                 values_drop_na = TRUE) |>
    #dplyr::mutate(`elevation_id` = stringr::str_extract(elevation_id, '\\d{3,} m')) |>
    tidyr::pivot_wider(names_from = 'variable', values_from = 'with_entry') |>
    dplyr::mutate(`Elevation (m)` = stringr::str_extract(row_id, '\\d{3,}')) |>
    tidyr::pivot_longer(cols = -row_id, 
                 names_to = 'column_name', values_to = 'with_entry') |>
    dplyr::mutate(of_variable = dplyr::case_when(
      column_name == "Mean air temperature (degree C)"~ 'mean_air_temperature',
      column_name == "Total rainfall (mm)" ~ 'total_rainfall',
      column_name == "Mean total solar radiation (MJ m<sup>-2</sup> day<sup>-1</sup>)" ~ 'mean_solar_radiation',
      column_name == "Mean maximum vapor pressure deficit (kPa)" ~ 'mean_max_vapor_pressure_deficit',
      column_name == "Elevation (m)" ~ 'elevation',
      TRUE ~ NA_character_)) |>
    dplyr::mutate(from_source = 'Table 3')
  
  Table3Meta <- Table3Primary |>
    dplyr::select(column_name, of_variable) |>
    unique() |>
    dplyr::mutate(unit = stringr::str_extract(column_name, pattern = '(?<=\\().*(?=\\))'),
           method = paste('Methods ln72-73:', paste0(data.lvl0.ls$method[18:23], collapse = ' '))) |>
    dplyr::select(-column_name) |>
    tidyr::pivot_longer(cols = -of_variable,
                 names_to = 'is_type', values_to = 'with_entry') |>
    dplyr::mutate(from_source = dplyr::case_when(is_type == 'unit' ~ 'Table 3 column names.',
                              is_type == 'method' ~ 'Methods ln72-73',
                              TRUE ~ NA_character_))
  
  
  # Pull everything together into by stacking the meta and primary data tables
  data.lvl1.ls <- list(
    study = studyMeta,
    primary_meta = dplyr::bind_rows(Table1Meta, Table3Meta),
    primary = dplyr::bind_rows(Table1Primary, Table3Primary)|>
      dplyr::mutate(elevation_id = with_entry[of_variable == 'elevation'],
             is_type = 'value',
             .by = row_id) |>
      dplyr::arrange(row_id, elevation_id, column_name,
              of_variable, is_type, with_entry, from_source)
  )
  
  if(dataLevel == 'level1'){
    return(data.lvl1.ls)
  }
  
  ### Build level 2 - HiSOC ####
  ### The purpose of this section is to create a curated element for the HiSOC data project
  
  # Define the variables that we want specifically for HiSOC database, this could be expanded to other data collections later so we filter here even though it has no effect
  HISOC_variables <- list(
    source = c('citation', 'doi'),
    site = c('region', 'observation_time', #temporal-geolocation
             #climate
             'mean_air_temperature', 'total_rainfall',
             #geology/geography
             'elevation', 'soil_class'),
    layer = c('layer_top', 'layer_bottom', #in methods
              'soil_organic_carbon', 'soil_ph', 'soil_phosphorus', 'soil_nitrogen'),
    site_history = c('land_use')
  )
  
  source.df <- data.lvl1.ls$study |>
    dplyr::filter(of_variable %in% HISOC_variables$source) |>
    dplyr::mutate(source_id = 'Ares2001')  |>
    dplyr::select(source_id, of_variable, with_entry) |>
    #all of the source information is from the same from_source value so this is easy
    pivot_wider(names_from = of_variable, values_from = with_entry)
  
  site_history.df <- data.lvl1.ls$study |>
    dplyr::filter(of_variable %in% HISOC_variables$site_history) |>
    dplyr::mutate(from_source = from_source[is_type == 'value']) |>
    dplyr::mutate(is_type = dplyr::case_match(is_type, 'value' ~ 'description',
                                .default = is_type)) |>
    dplyr::mutate(column_name = paste0(of_variable, '::', is_type)) |>
    dplyr::select(column_name, with_entry) |>
    pivot_wider(names_from = column_name, values_from=with_entry) |>
    dplyr::mutate(source_id = 'Ares2001',
           land_use_id = 'LU1', #only land use in the study
           land_use = 'Plantation' #from discription
    ) |>
    dplyr::select(source_id, land_use_id, land_use, 
           'land_use::description', 'land_use::interval', 'land_use::interval_format')
  
  layer.df <- data.lvl1.ls$primary |>
    #pull the layer variables out of the primary data
    dplyr::filter(of_variable %in% HISOC_variables$layer) |>
    #rely on the row id and of variable to ID the elements
    dplyr::select(-c(column_name, from_source, elevation_id))|>
    dplyr::bind_rows(#bind the primary data with associated meta data
      #... generally this is the unit or methods
      data.lvl1.ls$primary_meta |>
        dplyr::filter(of_variable %in% HISOC_variables$layer) |>
        dplyr::select(of_variable, is_type, with_entry) |>
        reframe(row_id = unique(data.lvl1.ls$primary$row_id),
                .by = tidyselect::everything()) 
    ) |>
    # add back in the site/elevation ids now that we have the methods bound
    dplyr::left_join(data.lvl1.ls$primary |> dplyr::select(row_id, elevation_id) |> unique(),
              by = dplyr::join_by(row_id)) |>
    #only keep things if there is an associated value we want in the row
    dplyr::filter((any(is_type == 'value')), .by = row_id) |>
    #create new column names with the variable and type in it
    dplyr::mutate(column_name = paste0(of_variable,'::',is_type)) |>
    #covert things to more resonable named IDs and pull in the header-values
    dplyr::select(layer_id = row_id, site_id = elevation_id, column_name, with_entry) |>
    #make everything wide for the poor humans
    pivot_wider(names_from = column_name, values_from = with_entry) |>
    #layer information transcribed from methods for soil variables
    dplyr::mutate(`layer::top` = '0', `layer::bottom` = '15', 
           `layer::unit` = 'cm')
  
  
  #The climate across various elevations needs to be gap-filled. 
  #  Here we create a linear interpolation to gapfill the temperature and rainfall
  #  values based on elevation. Details on model fit are in the comments below and
  #  fitted from the data originally provided.
  
  climate_var <- c('mean_air_temperature', 'elevation', 'total_rainfall')
  
  #create an elevation identified climate table
  elevation.df <- data.lvl1.ls$primary|>
    dplyr::filter(of_variable %in% climate_var) |>
    dplyr::select(elevation_id, of_variable, with_entry, is_type) |>
    unique() |>
    #convert from character to do the math
    dplyr::mutate(with_entry = as.numeric(with_entry)) |>
    #pivot things wider to make filling in missing values easier
    pivot_wider(names_from = c(of_variable, is_type), names_sep = '::', values_from = with_entry) |>
    #apply fitted linear interpolation
    dplyr::mutate(
      #Figure out linear interpolation with
      #lm(formula = `mean_air_temperature::value` ~ `elevation::value`, data = elevation.df)
      #N = 3, Adjusted R-squared:  0.997 , p-value: 0.02449
      `mean_air_temperature::value` = if_else(is.na(`mean_air_temperature::value`), 23.15 - 0.006429 * `elevation::value`, `mean_air_temperature::value`), 
      #N = 3, Adjusted R-squared:  0.9886; p-value: 0.0482
      `total_rainfall::value` = if_else(is.na(`total_rainfall::value`),
                                        19078.83 - 9.97 * `elevation::value`, `total_rainfall::value`)) |>
    dplyr::mutate(`mean_air_temperature::method` = 'linear interpolation from provided elevation values',
           `total_rainfall::method` = 'linear interpolation from provided elevation values') |>
    #convert everything back to characters for merging in with the rest of the data
    dplyr::mutate(dplyr::across(tidyselect::everything(), as.character)) |>
    #add on the source_id
    dplyr::mutate(source_id = 'Ares2001',
           land_use_id = site_history.df$land_use_id |> unique())
  
  # Pull in the data from the primary table that we did not interpolate above
  site.df <- data.lvl1.ls$primary |>
    #take the entries in HiSOC_variables taht are not in climate_var
    dplyr::filter(of_variable %in% setdiff(HISOC_variables$site, climate_var)) |>
    #similar to above, identify based on of_variable and ignore the source
    dplyr::select(-c(column_name, from_source)) |>
    unique() |>
    #make things wide for cleaner joins
    pivot_wider(names_from = c(of_variable, is_type), 
                names_sep = '::', values_from = with_entry) |>
    #row identifier is no longer needed
    dplyr::select(-row_id) |>
    dplyr::bind_cols( #pull in the variables in the study table like region and obs time
      data.lvl1.ls$study |> 
        dplyr::filter(of_variable %in% HISOC_variables$site) |>
        dplyr::select(-from_source) |>
        #make things wide for cleaner joins
        tidyr::pivot_wider(names_from = c(of_variable, is_type),
                    values_from = c(with_entry),
                    names_sep = '::')
    ) |>
    #add in the study id and land useID
    dplyr::mutate(source_id = 'Ares2001',
           land_use_id = site_history.df$land_use_id) |>
    dplyr::left_join(elevation.df,
              by = dplyr::join_by(elevation_id, source_id, land_use_id)) |>
    dplyr::rename(site_id = elevation_id)
  
  
  # return a list of tables
  
  data.lvl2HiCSC.df <- list(
    source = source.df,
    #make sure the site_ids have a letter in front of them so it is not read in as a numerical when read/write to csv
    site = site.df |>
      dplyr::mutate(site_id = paste0('S', site_id)),
    site_history = site_history.df,
    layer = layer.df|>
      dplyr::mutate(site_id = paste0('S', site_id))
  )
  
  if(dataLevel == 'level2-HiSOC'){
    return(data.lvl2HiCSC.df)
  }
  
  stop('Badly specified dataLevel flag')
}
