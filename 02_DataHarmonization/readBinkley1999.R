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
#' @importFrom dplyr mutate filter ends_with starts_with select n
#' @importFrom stringr str_extract
#' @importFrom tidyr pivot_longer separate_wider_delim
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

  #### Pull info true across study ####
  
  studyMeta <- tibble::tribble(~of_variable, ~is_type, ~with_entry, ~from_source,
                       'region', 'site', '13 km NNE of downtown Hilo', paste('Method ln3:', paste(data.lvl0.ls$method[3], collapse = ' ')),
                       'region', 'state', 'HI', paste('Method ln3:', paste(data.lvl0.ls$method[3], collapse = ' ')),
                       #convert to decimal degrees
                       'geolocation', 'latitude', as.character(19 + 50/60 + 28.1/3600), paste('Method ln3:', paste(data.lvl0.ls$method[3], collapse = ' ')),
                       'geolocation', 'longitude', as.character(-1*(155 + 7/60 + 28.3/3600)), paste('Method ln3:', paste(data.lvl0.ls$method[3], collapse = ' ')),
                       'geolocation', 'unit', 'decimal_degree', 'manual conversion in level1 codebase',
                       #pull climate variables
                       'air_temperature', 'value', '21',paste('Method ln3:', paste(data.lvl0.ls$method[3], collapse = ' ')),
                       'air_temperature', 'unit', '°C',paste('Method ln3:', paste(data.lvl0.ls$method[3], collapse = ' ')),
                       'rainfall', 'min', '300', paste('Method ln3:', paste(data.lvl0.ls$method[3], collapse = ' ')),
                       'rainfall', 'max', '400', paste('Method ln3:', paste(data.lvl0.ls$method[3], collapse = ' ')),
                       'rainfall', 'unit', 'mm mo<sup>-1</sup>', paste('Method ln3:', paste(data.lvl0.ls$method[3], collapse = ' ')),
                       #Elevation info
                       'elevation', 'value', '350', paste('Method ln3:', paste(data.lvl0.ls$method[3], collapse = ' ')),
                       'elevation', 'unit', 'm', paste('Method ln3:', paste(data.lvl0.ls$method[3], collapse = ' ')),
                       #sampling info
                       'soil_class', 'value', 'Kaiwiki thixotropic, isothermic Typic Hydrandepts', paste('Method ln4:',paste(data.lvl0.ls$method[4], collapse = ' ')),
                       'soil_sample_prep', 'method', 'oven dried at 100 C to constant weight', paste('Method ln22:', data.lvl0.ls$method[22]),
                       'soil_sampling', 'description', '30 by 30 m plots with trees at two spacings (1 by 1 m and 3 by 3 m)', paste('Method ln12:', data.lvl0.ls$method[12]),
                       'carbon_organic', 'unit', 'g m-2', 'Table 1 column name',
                       'carbon_organic', 'method', 'CN analyzer on carbonate-corrected sample then multipled by sampled bulk density and depth of sample', paste('Methods ln 21, 24, 29-30, 55', paste0(data.lvl0.ls$method[c(21, 24, 29:30, 55)], collapse = ' ')),
                       'depth', 'unit', 'cm', 'Table 1 column name',
                       'depth', 'method', paste0(data.lvl0.ls$method[c(19:20,23:24)], collapse = ' '), 'Methods ln 19-20;23-24',
                       #land use information
                       'initial_planting', 'value', '1994', paste('Method ln11:', data.lvl0.ls$method[11]),
                       'observation_time', 'value', '1997', paste('Method ln23:', data.lvl0.ls$method[23]),
                       'stand_age', 'unit', 'month', 'Table 1 column name',
                       'stand_age', 'method', 'Six month seedlings planted late April/early May', paste('Method ln11:', data.lvl0.ls$method[11]),
                       'stand_type', 'value', 'Eucalyptus saligna', paste('Method ln3:', data.lvl0.ls$method[3]),
                       #study citation
                       'citation', 'value', format(data.lvl0.ls$citation$primary), 
                       'journal_citation', 'doi', 'value', data.lvl0.ls$citation$primary$doi, 'journal citation') 
  
  #### Construct land use history ####
  
  land_use.df <- tibble::tribble(~land_use_id, ~of_variable, ~is_type, ~with_entry, ~from_source,
                         #Group the two different land use descriptions, LU1 is the current
                         'LU1', 'land_use', 'description', '4-ha plantation of *E. saligna*', paste('Method ln1:', data.lvl0.ls$method[1]),
                         'LU1', 'land_use', 'time_period', '1994/1997', paste('Method ln11,23:', paste(data.lvl0.ls$method[11], data.lvl0.ls[23], collapse = '...')),
                         #... LU2 is the historical land use
                         'LU2', 'land_use', 'description', 'Sugarcane', paste('Method ln3:', data.lvl0.ls$method[3]),
                         'LU2', 'land_use', 'duration', 'P80Y/1994', paste('Method ln3:', data.lvl0.ls$method[3])
  )
  
  #### Table 1 ####
  
  Table1Primary <- data.lvl0.ls$data$Table1$primary |>
    dplyr::mutate(row_id = paste0('R', 1:n())) |>
    dplyr::filter(`Stock or change` == 'stock') |>
    dplyr::mutate(timeSincePlanting_id = paste0('Months ', `Age/duration (mo)`),
           layer_id = paste0('Layer ', `Depth (cm)`)) |>
    dplyr::select(dplyr::ends_with('_id'), `Age/duration (mo)`, 
                  `Depth (cm)`, `C (g m<sup>-2</sup>)`) |>
    dplyr::mutate(depth__top = stringr::str_extract(`Depth (cm)`, pattern = "^\\d+(?=-)"),
           depth__bottom = stringr::str_extract(`Depth (cm)`, pattern ="(?<=-)\\d+$"),
           carbon_organic__mean = stringr::str_extract(`C (g m<sup>-2</sup>)`, pattern = '^\\d+'),
           carbon_organic__standard_error = stringr::str_extract(`C (g m<sup>-2</sup>)`, pattern = '(?<=\\()\\d+(?=\\))'),
           stand_age__value = `Age/duration (mo)`) |>
    dplyr::select(dplyr::ends_with('_id'), dplyr::starts_with('depth', ignore.case = FALSE),
           dplyr::starts_with('carbon'), dplyr::starts_with('stand')) |>
    tidyr::pivot_longer(cols = -c(row_id, timeSincePlanting_id, layer_id),
                 names_to = 'column_name', values_to = 'with_entry',
                 values_drop_na = TRUE) |>
    tidyr::separate_wider_delim(cols = column_name, delim = '__',
                         names = c('of_variable', 'is_type')) |>
    dplyr::mutate(from_source = 'Table 1')
  
  
  #### Create level 1
  
  data.lvl1.ls <- list(
    study = studyMeta,
    land_use = land_use.df,
    layer = Table1Primary
  )
  
  
  if(dataLevel == 'level1'){
    return(data.lvl1.ls)
  }
}
