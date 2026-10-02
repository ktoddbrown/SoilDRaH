#' Read Binkley1999 and Resh 1999
#'
#' Read in the rescued data from Binkley1999 and Resh 1999 that was ingested for the HiCSC project in 2025. 
#' Binkley and Resh 1999 was looks at soil carbon stocks under ~3 years of Eucalytus plantation from sugarcane.
#' They use this data to conclude that there was no change in carbon stocks but there was a shift from sugarcane derived carbon to Eucalytus.
#' 
#' Binkley, D. and Resh, S.C. (1999), Rapid Changes in Soils Following Eucalyptus Afforestation in Hawaii. Soil Science Society of America Journal, 63: 222-225. https://doi.org/10.2136/sssaj1999.03615995006300010032x

#' @param dataDir string with the directory address for where the data rescue files are located
#' @param dataLevel level of data product to be returned
#' @param verbose print out messages as processing data, currently not used
#'
#' @returns a list of data frames and bib-entries
#' @export
#' 
#' @importFrom bibtex read.bib
#' @importFrom readr read_lines read_csv cols col_character
#' @importFrom tibble tribble
#' @importFrom dplyr mutate n case_when bind_rows select left_join join_by arrange
#' @importFrom tidyr pivot_longer
#' 
#'  
#'  
#'   @examples
readBinkley1999 <- function(dataDir,
                            dataLevel = c('level0', 'level1')[1],
                            verbose = TRUE){
 # dataDir <- '01_DataRescue/Austin2010'
 # dataLevel <- 'level0'
 # verbose <- TRUE
  
 #### Set the files####
  
  # data files for level 0
  methods.file <- file.path(dataDir, "Binkley1999_Methods.md")
  table1.file <- file.path(dataDir, 'Binkley1999_Table1.csv')
  
  # Bibliograph files
  primaryCitation.file <- file.path(dataDir, 'Binkley1999.bib')
  methodsCitation.file <- file.path(dataDir, 'Binkley1999_Methods.bib')
  
  #### Construct level 0 ####

  data.lvl0.ls <- list(citation = 
                         list(primary = 
                                bibtex::read.bib(file = primaryCitation.file), 
                              methods = 
                                bibtex::read.bib(file = methodsCitation.file)
                         ),
                       method = readr::read_lines(file = methods.file),
                       data = list(
                         Table1 = list(
                           caption = 
                                            readr::read_csv(file = table1.file,
                                                     col_types = readr::cols(.default = readr::col_character()),
                                                     n_max = 1, col_names = FALSE)$X1[1],
                                          primary = 
                                            readr::read_csv(file = table1.file,
                                                     col_types = readr::cols(.default = readr::col_character()),
                                                     skip = 1,
                                                     na = '-')
                                     )
                       )
  )
  
  if(dataLevel == 'level0'){
    return(data.lvl0.ls)
  }
  #### Construct level 1 ####
  
  studyMeta <- tibble::tribble(~of_variable, ~is_type, ~with_entry, ~from_source,
                       'region', 'site', '13 km NNE of downtown Hilo', paste('Method ln3:', paste(data.lvl0.ls$method[3], collapse = ' ')),
                       'region', 'state', 'HI', paste('Method ln3:', paste(data.lvl0.ls$method[3], collapse = ' ')),
                       'geolocation', 'latitude', as.character(19 + 50/60 + 28.1/3600), paste('Method ln3:', paste(data.lvl0.ls$method[3], collapse = ' ')),
                       'geolocation', 'longitude', as.character(-1*(155 + 7/60 + 28.3/3600)), paste('Method ln3:', paste(data.lvl0.ls$method[3], collapse = ' ')),
                       'air_temperature', 'value', '21',paste('Method ln3:', paste(data.lvl0.ls$method[3], collapse = ' ')),
                       'air_temperature', 'unit', '°C',paste('Method ln3:', paste(data.lvl0.ls$method[3], collapse = ' ')),
                       'rainfall', 'min', '300', paste('Method ln3:', paste(data.lvl0.ls$method[3], collapse = ' ')),
                       'rainfall', 'max', '400', paste('Method ln3:', paste(data.lvl0.ls$method[3], collapse = ' ')),
                       'rainfall', 'unit', 'mm mo<sup>-1</sup>', paste('Method ln3:', paste(data.lvl0.ls$method[3], collapse = ' ')),
                       'elevation', 'value', '350', paste('Method ln3:', paste(data.lvl0.ls$method[3], collapse = ' ')),
                       'elevation', 'unit', 'm', paste('Method ln3:', paste(data.lvl0.ls$method[3], collapse = ' ')),
                       'soil_class', 'value', 'Kaiwiki thixotropic, isothermic Typic Hydrandepts', paste('Method ln4:',paste(data.lvl0.ls$method[4], collapse = ' ')),
                       'initial_planting', 'value', '1994', paste('Method ln11:', data.lvl0.ls$method[11]),
                       'observation_time', 'value', '1997', paste('Method ln23:', data.lvl0.ls$method[23]),
                       'soil_sample_prep', 'method', 'oven dried at 100 C to constant weight', paste('Method ln22:', data.lvl0.ls$method[22]),
                       'soil_sampling', 'description', '30 by 30 m plots with trees at two spacings (1 by 1 m and 3 by 3 m)', paste('Method ln12:', data.lvl0.ls$method[12]),
                       'stand_type', 'value', 'E. saligna', paste('Method ln3:', data.lvl0.ls$method[3]),
                       'current_land_use', 'description', '4-ha plantation of *E. saligna*', paste('Method ln1:', data.lvl0.ls$method[1]),
                       'site_history', 'description', 'Sugarcane was cropped on this site for more than 80 yr.', paste('Method ln3:', data.lvl0.ls$method[3]),
                       'citation', 'value', format(data.lvl0.ls$citation$primary), 
                       'journal_citation', 'doi', 'value', data.lvl0.ls$citation$primary$doi, 'journal citation')
  #### Table 1 ####
  
  Table1Primary <- data.lvl0.ls$data$Table1$primary |>
    dplyr::mutate(row_id = paste0('R', 1:dplyr::n())) |>
    tidyr::pivot_longer(cols = -row_id,
                 names_to = 'column_name', values_to = 'with_entry',
                 values_drop_na = TRUE) |>
    dplyr::mutate(of_variable = dplyr::case_when(
      column_name == "Age (mo)" ~ "stand_age",
      column_name == "Depth (cm)" ~ "layer",
      column_name == "C (g m<sup>-2</sup>)" ~ "soil_organic_carbon",
      column_name == "N (g m<sup>-2</sup>)" ~ "soil_nitrogen",
      column_name == "pH<sub>CaCl2</sub>" ~ "soil_ph",
      column_name == "Ca (mmol m<sup>-2</sup>)" ~ "soil_calcium",
      column_name == "Mg (mmol m<sup>-2</sup>)" ~ "soil_magensium",
      column_name == "C K† (mmol m<sup>-2</sup>)" ~ "soil_potassium",
      column_name == "F K† (mmol m<sup>-2</sup>)" ~ "soil_potassium",
      column_name == "Al (mmol m<sup>-2</sup>)" ~ "soil_aluminum")) |>
    
    dplyr::mutate(
      value = case_when(
        column_name == "Age (mo)" & str_detect(with_entry,"(?i)change") ~ "Change",
        column_name == "Age (mo)" ~ with_entry,
        TRUE ~ NA_character_
      ),
      lowerbound = case_when(
        str_detect(with_entry, '^\\d+\\s*-\\s*\\d+') ~ str_extract(with_entry, "\\d+(?=-)"),
        TRUE ~ NA_character_
      ),
      upperbound = case_when(
        str_detect(with_entry, '^\\d+\\s*-\\s*\\d+') ~ str_extract(with_entry, "(?<=-)\\d+"),
        TRUE ~ NA_character_
      ),
      mean = as.character(
        str_extract(with_entry, '^-?\\d*\\.?\\d+(?=\\s*\\()')),
      `standard error` = as.character(
        str_extract(with_entry, '(?<=\\()[0-9.]+(?=\\))')),
    ) |>
    dplyr::select(-with_entry) |>
    
    tidyr::pivot_longer(cols = c('lowerbound','upperbound','mean','standard error','value'),
                 names_to = 'is_type',
                 values_to = 'with_entry',
                 values_drop_na = TRUE) |>
    
    dplyr::mutate(from_source = 'Table 1')
  
  Table1Meta <- Table1Primary |>
    dplyr::select(column_name, of_variable) |>
    unique() |>
    #Grab everything between the parentheses as units and attribute the source as the column names.
    dplyr::mutate(unit = stringr::str_extract(column_name, pattern = '(?<=\\().*(?=\\))'),
           
           species = dplyr::case_when(str_detect(column_name, 'E. saligna') ~ 'Eucalyptus saligna',                             .default = NA_character_),
           from_source = 'Table 1 column names.') |>
    #If there aren't units or a species flag then drop the row
    dplyr::filter(!is.na(unit) | !is.na(species)) |>
    
    dplyr::bind_rows(
      tibble::tribble(~of_variable, ~method, ~from_source,
              'soil_ph', paste0(data.lvl0.ls$method[25:26], collapse = ' '), 'Methods ln25-26',
              'soil_calcium', paste0(data.lvl0.ls$method[27], collapse = ' '), 'Methods ln27',
              'soil_magensium', paste0(data.lvl0.ls$method[27], collapse = ' '), 'Methods ln27',
              'soil_aluminum', paste0(data.lvl0.ls$method[27], collapse = ' '), 'Methods ln27',
              'soil_potassium', paste0(data.lvl0.ls$method[c(14:15,27)], collapse = ' '), 'Methods ln14-15;27',
              'soil_potassium', paste0(data.lvl0.ls$method[c(16:17,27)], collapse = ' '), 'Methods ln16-17;27',
              'stand_age', paste0(data.lvl0.ls$method[11], collapse = ' '), 'Methods ln11',
              'layer', paste0(data.lvl0.ls$method[c(19:20,23:24)], collapse = ' '), 'Methods ln 19-20;23-24',
              'soil_organic_carbon', paste0(data.lvl0.ls$method[29:30], collapse = ' '), 'Methods ln 29-30',
              'soil_nitrogen', paste0(data.lvl0.ls$method[29:30], collapse = ' '), 'Methods ln 29-30') ) |>
    dplyr::bind_rows(
      tibble::tribble(~of_variable, ~control_vocabulary, ~from_source,
              'soil_class', 'Kaiwiki thixotropic: USDA classification for deep, well drained soils formed from weathered volcanic ash|isothermic Typic Hydrandept: USDA taxonomic class of deep, well drained soils formed in material weathered from basic volcanic ash', 'USDA',
              'stand_type', '*E. saligna*: pure stands of Eucalyptus saligna (Sm.)', 'Abstract ln3',) |>
        dplyr::left_join(Table1Primary |>
                           dplyr::select(column_name, of_variable) |>
                    unique(),
                  by = dplyr::join_by(of_variable)))|>
    tidyr::pivot_longer(cols = c(unit, species, method, control_vocabulary),
                 names_to = 'is_type',
                 values_drop_na = TRUE,
                 values_to = 'with_entry')
  
  #### Create level 1
  
  data.lvl1.ls <- list(
    study = studyMeta,
    primary_meta = dplyr::bind_rows(Table1Meta),
    primary = dplyr::bind_rows(Table1Primary)|>
      dplyr::mutate(age_id = with_entry[of_variable == 'stand_age'],
             is_type = 'value',
             .by = row_id) |>
      dplyr::arrange(row_id, age_id, column_name,
              of_variable, is_type, with_entry, from_source)
  )
  if(dataLevel == 'level1'){
    return(data.lvl1.ls)
  }
}
