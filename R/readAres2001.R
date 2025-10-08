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
#' @importFrom dplyr mutate n case_when bind_rows select filter arrange
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom stringr str_extract
#'
readAres2001 <- function(dataDir,
                         dataLevel = c('level0', 'level1')[1],
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
  
  dataLevel <- 'level0'
  
  ###Level 0 data read rescued from pdf ####
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
  
  #### Build level 1 data ####
  
  #Create table with data that is consistent across the study
  studyMeta <- tibble::tribble(~of_variable, ~is_type, ~with_entry, ~source,
                       'region', 'value', 'Honaunau Forest on the southwestern slopes of Mauna Loa, island of Hawaii', paste('Method ln5:', paste(data.lvl0.ls$method[5], collapse = ' ')), #no actual lat-long, need to get this translated to geo-location
                       'land_use_type', 'value', 'Tree stands', paste('Method ln14-16:', paste(data.lvl0.ls$method[14:16], collapse = ' ')),
                       'inital_planting', 'value', '1959', paste('Method ln14:', data.lvl0.ls$method[14]),
                       'observation_year', 'value', '1996', paste('Method ln 30;53;58:', paste0(data.lvl0.ls$method[c(30,53,58)], collapse = '... ')),
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
      column_name == '*F. uhdei* Stem density (trees / ha)' ~ 'stem_density_F_uhdei',
      column_name == '*F. uhdei* Mean DBH (cm)' ~ 'diameter_at_breast_height_F_uhdei',
      column_name == '*F. uhdei* Mean height(m)' ~ 'height_F_uhdei',
      column_name == '*A. koa* Stem density (trees / ha)' ~ 'stem_density_A_koa',
      column_name == '*A. koa* Mean DBH (cm)' ~ 'diameter_at_breast_height_A_koa',
      column_name == "*A. koa* Mean height(m)"~ 'height_A_koa')) |>
    #Flag the source of this data as Table 1, this table is merged later with other data so this keeps track of where the data came from
    dplyr::mutate(source = 'Table 1')
  
  # Create the metadata for table one from the header information and cited methods
  Table1Meta <- Table1Primary |>
    dplyr::select(column_name, of_variable) |>
    unique() |>
    #Grab everything between the parentheses as units and attribute the source as the column names.
    dplyr::mutate(unit = stringr::str_extract(column_name, pattern = '(?<=\\().*(?=\\))'),
           source = 'Table 1 column names.') |>
    #If there aren't units then drop the row
    dplyr::filter(!is.na(unit)) |>
    #Don't keep the column name now that you have the units, cross link via the variable
    dplyr::select(-column_name) |>
    #Stack into the table the methods for each variable that we can find
    dplyr::bind_rows(
      tibble::tribble(~of_variable, ~method, ~source,
              'soil_ph', paste0(data.lvl0.ls$method[72:73], collapse = ' '), 'Methods ln72-73', #pasting in a method from specific rows
              'soil_organic_carbon', paste0(data.lvl0.ls$method[72:73], collapse = ' '), 'Methods ln72-73',
              'soil_nitrogen', paste0(data.lvl0.ls$method[72:73], collapse = ' '), 'Methods ln72-73',
              'soil_phosphorus', paste0(data.lvl0.ls$method[72:73], collapse = ' '), 'Methods ln72-73',
              'soil_class', paste0(data.lvl0.ls$method[10:12], collapse = ' '), 'Methods ln10-12') ) |>
    #Stack onto the tables the controlled vocabulary used
    dplyr::bind_rows(
      tibble::tribble(~of_variable, ~control_vocabulary, ~source,
              'stand_type', '*F. uhdei*: pure stands of Fraxinus uhdei (Wenzig) Lingelsh|Mixed: mixed stands of *Fraxinus uhdei* (Wenzig) Lingelsh and *Acacia koa* Grey', 'Abstract ln1',
              'soil_class', 'Histosol:USDA classification for histosol soil type|Andisols:USDA classification for andisol soil type', 'expert informed')
    ) |>
    # Push the dimensions of the variable into a single column. Making this a long table.
    tidyr::pivot_longer(cols = c(unit, method, control_vocabulary),
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
    dplyr::mutate(source = 'Table 3')
  
  Table3Meta <- Table3Primary |>
    dplyr::select(column_name, of_variable) |>
    unique() |>
    dplyr::mutate(unit = stringr::str_extract(column_name, pattern = '(?<=\\().*(?=\\))'),
           method = paste('Methods ln72-73:', paste0(data.lvl0.ls$method[18:23], collapse = ' '))) |>
    dplyr::select(-column_name) |>
    tidyr::pivot_longer(cols = -of_variable,
                 names_to = 'is_type', values_to = 'with_entry') |>
    dplyr::mutate(source = dplyr::case_when(is_type == 'unit' ~ 'Table 3 column names.',
                              is_type == 'method' ~ 'Methods ln72-73',
                              TRUE ~ NA_character_))
  
  
  # Pull everything together into by stacking the meta and primary data tables
  data.lvl1.ls <- list(
    meta = dplyr::bind_rows(studyMeta, Table1Meta, Table3Meta),
    primary = dplyr::bind_rows(Table1Primary, Table3Primary)|>
      dplyr::mutate(elevation_id = with_entry[of_variable == 'elevation'],
             is_type = 'value',
             .by = row_id) |>
      dplyr::arrange(row_id, elevation_id, column_name,
              of_variable, is_type, with_entry, source)
  )
  
  return(data.lvl0.ls)
}
