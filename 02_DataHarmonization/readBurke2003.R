#' Read Burke etal 2003
#'
#' Burke etal 2003 investigates the effects of soil type and land use on soil microbial community structure and orgnaic carbon stocks. This is done by analyzing soil phospholipid fatty acid profiles (PLFA) and stable carbon isotope ratios as indicators of microbial activity.
#' 
#' Burke, R.A., Molina, M., Cox, J.E., Osher, L.J. and Piccolo, M.C. (2003), Stable Carbon Isotope Ratio and Composition of Microbial Fatty Acids in Tropical Soils. J. Environ. Qual., 32: 198-206. https://doi.org/10.2134/jeq2003.1980
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
readBurke2003 <- function(dataDir,
                          dataLevel = c('level0', 'level1')[1],
                          verbose = TRUE){
#comment out this before leaving
#dataDir <- '01_DataRescue/Burke2003'
# dataLevel < 'level0'
# verbose <- TRUE
  
#### Set the files ####

methods.file <- file.path(dataDir, 'Burke2003_Methods.md')
table1.file <- file.path(dataDir, 'Burke2003_Table1.csv')

primaryCitation.file <- file.path(dataDir, 'Burke2003.bib')
methodsCitation.file <- file.path(dataDir, 'Burke2003_Methods.bib')

#dataLevel <- 'level0'

### Build Level 0 data ####
###
#This chunk has two purposes. 
#...1) Check the formatting by reading in everything 
#...2) Create a list of everything to process later in level 1

data.lvl0.df <- list(
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
    ) 
    
    ))
  if(dataLevel == 'level0'){
  return(data.lvl0.df)

  }
 
#### Methods section only ####

studyMeta <- tribble(~region_id, ~site_id, ~of_variable, ~is_type, ~with_entry, ~from_source,
                     # Hawaii
                     'Hawaii', 'all Hawaii sites','region', 'site', 'Hamakua Coast, Hawaii, northeast flank of Mauna Kea', paste('Method ln7:', data.lvl0.df$method[7]),
                     'Hawaii','all Hawaii sites','region', 'state', 'HI', paste('Method ln7:', data.lvl0.df$method[7]),
                     'Hawaii','all Hawaii sites', 'elevation', 'value', '700', paste('Method ln7:', data.lvl0.df$method[7]),
                     'Hawaii','all Hawaii sites', 'elevation', 'unit', 'm', paste('Method ln7:', data.lvl0.df$method[7]),
                     'Hawaii','all Hawaii sites','soil_class', 'value', 'Typic Distrandepts', paste('Method ln8:', data.lvl0.df$method[8]),
                     'Hawaii','all Hawaii sites','soil_age', 'maximum', '15000', paste('Method ln8:', data.lvl0.df$method[8]),
                     'Hawaii', 'all Hawaii sites','soil_age', 'minimum', '20000', paste('Method ln8:', data.lvl0.df$method[8]),
                     'Hawaii', 'all Hawaii sites','soil_age', 'unit', 'yr', paste('Method ln8:', data.lvl0.df$method[8]),
                     'Hawaii', 'all Hawaii sites','rainfall', 'maximum', '2500', paste('Method ln9:', data.lvl0.df$method[9]),
                     'Hawaii','all Hawaii sites', 'rainfall', 'minimum', '4000', paste('Method ln9:', data.lvl0.df$method[9]),
                     'Hawaii', 'all Hawaii sites','rainfall', 'unit', 'mm', paste('Method ln9:', data.lvl0.df$method[9]),
                     'Hawaii', 'all Hawaii sites','air_temperature', 'value', '22', paste('Method ln10:', data.lvl0.df$method[10]),
                     'Hawaii', 'all Hawaii sites','air_temperature', 'unit', '°C', paste('Method ln10:', data.lvl0.df$method[10]),
                     'Hawaii','all Hawaii sites', 'stand_age', 'value', '90', paste('Method ln12:', data.lvl0.df$method[12]),
                     'Hawaii', 'all Hawaii sites','stand_age', 'unit', 'yr', paste('Method ln12:', data.lvl0.df$method[12]),
                     'Hawaii', 'all Hawaii sites','observation_time', 'value', '1997', paste('Method ln17:', data.lvl0.df$method[17]),
                     
                     # Brazil
                     'Brazil', 'all Brazil sites','region', 'municiplaity', 'Piracicaba', paste('Method ln23:', data.lvl0.df$method[23]),
                     'Brazil', 'all Brazil sites','region', 'city', 'São Paulo', paste('Method ln23:', data.lvl0.df$method[23]),
                     'Brazil','all Brazil sites', 'region', 'country', 'Brazil', paste('Method ln23:', data.lvl0.df$method[23]),
                     'Brazil', 'all Brazil sites','elevation', 'value', '575', paste('Method ln23:', data.lvl0.df$method[23]),
                     'Brazil', 'all Brazil sites','elevation', 'unit', 'm', paste('Method ln23:', data.lvl0.df$method[23]),
                     'Brazil','all Brazil sites', 'rainfall', 'value', '1400', paste('Method ln24:', data.lvl0.df$method[24]),
                     'Brazil','all Brazil sites', 'rainfall', 'unit', 'mm', paste('Method ln24:', data.lvl0.df$method[24]),
                     'Brazil', 'all Brazil sites','air_temperature', 'value', '20', paste('Method ln24:', data.lvl0.df$method[24]),
                     'Brazil','all Brazil sites', 'air_temperature', 'unit', '°C', paste('Method ln24:', data.lvl0.df$method[24]),
                     'Brazil', 'all Brazil sites','soil_class', 'value', 'isothermic Typic Haplorthox', paste('Method ln25:', data.lvl0.df$method[25]),
                     'Brazil', 'all Brazil sites','stand_age', 'value', '27', paste('Method ln28:', data.lvl0.df$method[28]),
                     'Brazil', 'all Brazil sites','stand_age', 'value', '65', paste('Method ln28:', data.lvl0.df$method[28]),
                     'Brazil', 'all Brazil sites','stand_age', 'unit', 'yr', paste('Method ln28:', data.lvl0.df$method[28]),
                     'Brazil','all Brazil sites', 'observation_time', 'value', '1998', paste('Method ln28:', data.lvl0.df$method[28]),
                     'Brazil', 'all Brazil sites','mineralogy', 'value', 'clayey, kalonitic', paste('Method ln25:', data.lvl0.df$method[25]),
                     
                     # Ecuador
                     'Ecuador', 'all Ecuador sites','region', 'value', 'Western slope of Ecuadorian Andes', paste('Method ln38:', data.lvl0.df$method[38]),
                     'Ecuador', 'all Ecuador sites','elevation', 'value', '1400', paste('Method ln38:', data.lvl0.df$method[38]),
                     'Ecuador', 'all Ecuador sites','elevation', 'unit', 'm', paste('Method ln38:', data.lvl0.df$method[38]),
                     'Ecuador','all Ecuador sites', 'rainfall', 'value', '3200', paste('Method ln39:', data.lvl0.df$method[39]),
                     'Ecuador', 'all Ecuador sites','rainfall', 'unit', 'mm', paste('Method ln39:', data.lvl0.df$method[39]),
                     'Ecuador', 'all Ecuador sites','air_temperature', 'minimum', '17', paste('Method ln39:', data.lvl0.df$method[39]),
                     'Ecuador','all Ecuador sites', 'air_temperature', 'maximum', '26', paste('Method ln39:', data.lvl0.df$method[39]),
                     'Ecuador', 'all Ecuador sites','air_temperature', 'unit', '°C', paste('Method ln39:', data.lvl0.df$method[39]),
                     'Ecuador', 'all Ecuador sites','soil_class', 'value', 'Andic Humitropepts', paste('Method ln40:', data.lvl0.df$method[40]),
                     'Ecuador','all Ecuador sites', 'soil_age', 'value', '2500', paste('Method ln40:', data.lvl0.df$method[40]),
                     'Ecuador', 'all Ecuador sites','soil_age', 'unit', 'yr', paste('Method ln40:', data.lvl0.df$method[40]),
                     'Ecuador', 'all Ecuador sites','stand_age', 'value', '25', paste('Method ln43:', data.lvl0.df$method[43]),
                     'Ecuador', 'all Ecuador sites','stand_age', 'unit', 'yr', paste('Method ln43:', data.lvl0.df$method[43]),
                     'Ecuador', 'all Ecuador sites','mineralogy', 'value', 'andesitic volcanic ash', paste('Method ln40:', data.lvl0.df$method[40]),
                     'Ecuador', 'all Ecuador sites','observation_time', 'value', '1998', paste('Method ln41:', data.lvl0.df$method[41])
)

citationMeta <- tribble(~of_variable, ~is_type, ~with_entry, ~from_source,
                        'citation', 'value', format(data.lvl0.df$citation$primary), 
                        'journal_citation', 'doi', 'value', data.lvl0.df$citation$primary$doi, 'journal citation')            

land_use.df <- tribble(~land_use_id, ~region_id, ~site_id, ~of_variable, ~is_type, ~with_entry, ~from_source,
                       # LU1 is Hawaii's Kalopa Site
                       'LU1', 'Hawaii', 'Kalopa', 'land_use', 'description', 'sugarcane', paste('Method ln13:', data.lvl0.df$method[13]),
                       'LU1', 'Hawaii', 'Kalopa', 'land_use', 'duration', 'P50Y', paste('Method ln13:', data.lvl0.df$method[13]),
                       
                       # LU2 is Hawaii's Humuula site
                       'LU2', 'Hawaii', 'Humnula','land_use', 'description', 'sugarcane', paste('Method ln13:', data.lvl0.df$method[13]),
                       'LU2', 'Hawaii', 'Humnula', 'land_use', 'duration', 'P90Y', paste('Method ln13:', data.lvl0.df$method[13]),
                       
                       # LU3 is Brazil's Forest site
                       'LU3', 'Brazil', 'Forest', 'land_use', 'description', 'Forest', paste('Method ln23:', data.lvl0.df$method[23]),
                       
                       #LU4 is Brazil's Sugarcane P27Y
                       'LU4', 'Brazil', 'Sugarcane_27Y', 'land_use', 'description', 'extensive sugarcane cultivation', paste('Method ln23:', data.lvl0.df$method[23]),
                       'LU4', 'Brazil', 'Sugarcane_27Y', 'land_use', 'duration', 'P27Y', paste('Method ln28:', data.lvl0.df$method[28]),
                       
                       #LU5 is Brazil's Sugarcane P65Y
                       'LU5', 'Brazil', 'Sugarcane_65Y', 'land_use', 'description', 'extensive sugarcane cultivation', paste('Method ln23:', data.lvl0.df$method[23]),
                       'LU5', 'Brazil', 'Sugarcane_65Y', 'land_use', 'duration', 'P65Y', paste('Method ln28:', data.lvl0.df$method[28]),
                       
                       #LU6 is Ecuador's Forest Site
                       'LU6', 'Ecuador', 'Forest', 'land_use', 'description', 'a closed canopy and a healthy, solid understory of ferns and forbs', paste('Method ln45:', data.lvl0.df$method[45]),
                       'LU6', 'Ecuador', 'Forest', 'land_use', 'duration', 'P25Y', paste('Method ln45:', data.lvl0.df$method[45]),
                       
                       #LU7 is Ecuador's Pasture Site
                       'LU7', 'Ecuador', 'Pasture','land_use', 'description', 'African bristlegrass [*Setaria sphacelata* (Schumacher) Moss] pasture sites', paste('Method ln41:', data.lvl0.df$method[41]),
                       'LU7', 'Ecuador', 'Pasture', 'land_use', 'duration', 'P25Y', paste('Method ln43:', data.lvl0.df$method[43]),
                       
                       #LU8 is Ecuador's Sugarcane Site
                       'LU8', 'Ecuador', 'Sugarcane', 'land_use', 'description', 'Sugarcane', paste('Method ln42:', data.lvl0.df$method[42]),
                       'LU8', 'Ecuador', 'Sugarcane', 'land_use', 'duration', 'P25Y', paste('Method ln43:', data.lvl0.df$method[43])
)

#### Table 1 ####

Table1Primary <- data.lvl0.df$data$Table1$primary |>
  dplyr::mutate(row_id = paste0('R', 1:dplyr::n())) |>
  tidyr::pivot_longer(cols = -c(row_id),
               names_to = 'column_name', values_to = 'with_entry',
               values_drop_na = TRUE) |>
  dplyr::mutate(of_variable = dplyr::case_when(
    column_name == 'Location' ~ 'region_id',
    column_name == 'Land use' ~ 'site_id',
    column_name == 'Soil depth (cm)' ~ 'layer',
    column_name == 'PFLA (mg kg-1)' ~ 'phospholipid_acid_content',
    column_name == 'Organic C content (%)' ~ 'organic_carbon',
    column_name == 'δ13C-SOM (per mil)' ~ 'delta_13_carbon',
    .default = 'Bad Match'
  )) |>
  
  
  dplyr::mutate(
    value = case_when(
      column_name == "Location" ~ with_entry,
      column_name == "Land use" ~ with_entry,
      column_name == "PFLA (mg kg-1)" & str_detect(with_entry, '^\\d+(\\.\\d+)?$') ~ with_entry,
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
      str_extract(with_entry, '^-?\\d*\\.?\\d+(?=\\s*±)')),
    `standard error` = as.character(
      str_extract(with_entry, '(?<=\\±)\\s*\\d*\\.?\\d+')),
  ) |>
  dplyr::select(-with_entry) |>
  
  tidyr::pivot_longer(cols = c('lowerbound','upperbound','mean','standard error', 'value'),
               names_to = 'is_type',
               values_to = 'with_entry',
               values_drop_na = TRUE) |>
  
  dplyr::mutate(from_source = 'Table 1')

Table1Meta <- Table1Primary |>
  dplyr::select(column_name, of_variable) |>
  unique() |>
  
  dplyr::mutate(unit = stringr::str_extract(column_name, pattern = '(?<=\\().*(?=\\))'),
         #   .default = NA_character_)
         from_source = 'Table 1 column names.') |>
  dplyr::filter(!is.na(unit)) |>
  
  dplyr::bind_rows(
    tibble::tribble(~of_variable, ~method, ~from_source,
            'region_id', paste0(data.lvl0.df$method[c(7,24,38)], collapse = ' '), 'Methods ln7;24;28',
            'site_id', paste0(data.lvl0.df$method[c(11:13,28,41:42)], collapse = ' '), 'Methods ln11-13,28,41-42',
            'layer', paste0(data.lvl0.df$method[c(17:19,34,47)], collapse = ' '), 'Methods ln17-19,34,47',
            'phospholipid_acid_content', paste0(data.lvl0.df$method[51:67], collapse = ' '), 'Methods ln51-67',
            'delta_13_carbon', paste0(data.lvl0.df$method[c(89:90,103)], collapse = ' '), 'Methods ln89-90,103') |> 
      dplyr::bind_rows(
        tibble::tribble(~of_variable, ~control_vocabulary, ~from_source,
                #struggled to find info on the soil types
                'stand_type', '*Saccharum spp.*: sugarcane', 'Abstract ln2')
      ) |>
      dplyr::left_join(Table1Primary |>
                  dplyr::select(column_name, of_variable) |>
                  unique(),
                by = dplyr::join_by(of_variable)))|>
  tidyr::pivot_longer(cols = c(unit, method, control_vocabulary),
               names_to = 'is_type',
               values_drop_na = TRUE,
               values_to = 'with_entry')


#### Create level 1

data.lvl1.ls <- list(
  study =  dplyr::bind_rows(studyMeta, citationMeta, land_use.df),
  primary_meta = dplyr::bind_rows(Table1Meta),
  primary = dplyr::bind_rows(Table1Primary) |>
    dplyr::mutate(location_id = with_entry[of_variable == 'region_id'],
           is_type = 'value',
           .by = row_id) |>
    dplyr::arrange(row_id, location_id, column_name,
            of_variable, is_type, with_entry, from_source)
)
if(dataLevel == 'level1'){
  return(data.lvl1.ls)
    }
  }
