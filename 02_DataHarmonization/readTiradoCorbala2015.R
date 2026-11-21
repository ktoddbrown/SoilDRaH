#' Read TiradoCorbala 2015
#'
#' Read in the rescued data from Triado-Corbala 2015 that was ingested for the HiCSC project in 2026. 
#' Tirado-Corbala 2015 examines four sugarcane cultivars grown in Hawai‘i to assess their soil carbon sequestration potential and how this varies across different soil series. They use this data to determine how the two distinct soil types can affect total carbon, total nitrogen, dissolved organic carbon, and nitrates.
#' 
#' Tirado-Corbalá, R., Anderson, R. G., Wang, D., & Ayars, J. E. (2015). Soil Carbon and Nitrogen Stocks of Different Hawaiian Sugarcane Cultivars. Agronomy, 5(2), 239-261. https://doi.org/10.3390/agronomy5020239

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
#' @importFrom dplyr mutate select bind_rows
#' @importFrom stringr str_extract
#' @importFrom tidyr pivot_longer
#' 
#'  
#'  
#'   @examples
readTriadoCorbala2015 <- function(dataDir,
                                  dataLevel = c('level0', 'level1')[1],
                                  verbose = TRUE){
  # dataDir <- '01_DataRescue/TiradoCorbala2015/'
  # dataLevel <- 'level0'
  # verbose <- TRUE
  
  #### Set the files####
  
  # data files for level 0
  methods.file <- file.path(dataDir, 'TiradoCorbala2015_Methods.md')
  table2.file <- file.path(dataDir, 'TiradoCorbala2015_Table2.csv')
  table3.file <- file.path(dataDir, 'TiradoCorbala2015_Table3.csv')
  table4.file <- file.path(dataDir, 'TiradoCorbala2015_Table4.csv')
  figure1.file <- file.path(dataDir, 'TiradoCorbala2015_Figure1.csv')
  figure2.file <- file.path(dataDir, 'TiradoCorbala2015_Figure2.csv')
  figure3.file <- file.path(dataDir, 'TiradoCorbala2015_Figure3.csv')
  figure4.file <- file.path(dataDir, 'TiradoCorbala2015_Figure4.csv')
  figure9.file <- file.path(dataDir, 'TiradoCorbala2015_Figure9.csv')
  figure10.file <- file.path(dataDir, 'TiradoCorbala2015_Figure10.csv')
  
  # Bibliograph files
  primaryCitation.file <- file.path(dataDir, 'TiradoCorbala2015.bib')
  methodsCitation.file <- file.path(dataDir, 'TiradoCorbala2015_Methods.bib')
  
  #### Construct level 0 ####
  data.lvl0.ls <- list(
    citation = 
      list(primary = bibtex::read.bib(file = primaryCitation.file), 
           methods = bibtex::read.bib(file = methodsCitation.file)
      ),
    method = readr::read_lines(file = methods.file),
    data =  lapply(
      list(figure1 = figure1.file, figure2 = figure2.file,
           figure3 = figure3.file, figure4 = figure4.file,
           figure9 = figure9.file, figure10 = figure10.file,
           table2 = table2.file, table3 = table3.file,
           table4 = table4.file),
      FUN = function(element.file){
        list(
          caption = readr::read_csv(
            file = element.file,
            col_types = readr::cols(.default = readr::col_character()),
            n_max = 1, col_names = FALSE)$X1[1],
          primary = readr::read_csv(
            file = element.file,
            col_types = readr::cols(.default = readr::col_character()),
            skip = 1)
        )
      }
    )
  )
  
  #### Return level 0 data ####
  if(dataLevel == 'level0'){
    return(data.lvl0.ls)
  }
  
  #### Pull info true across study ####
  
  
  study.df <- tibble::tribble(
    ~of_variable, ~is_type, ~with_entry, ~from_source,
    'island', 'value', 'Maui', paste('Method ln5:', paste(data.lvl0.ls$method[5], collapse = ' ')),
    'land_use', 'description', 'commercial sugar cane plantation', paste('Method ln5:', paste(data.lvl0.ls$method[5], collapse = ' ')),
    'geolocation', 'latitude', as.character(20 + 54/60), paste('Method ln5:', paste(data.lvl0.ls$method[5], collapse = ' ')),
    'geolocation', 'longitude', as.character(156 + 1/26), paste('Method ln5:', paste(data.lvl0.ls$method[5], collapse = ' ')),
    'geolocation', 'precision', as.character(1/60), paste('Method ln5:', paste(data.lvl0.ls$method[5], collapse = ' ')),
    'bulk_density_stock', 'method', 'inferred', paste('Method ln26,35:', paste(data.lvl0.ls$method[c(26,35)], collapse = ' ')),
    'carbon_total', 'method', 'dry combustion with CN analyzer', paste('Method ln31:', paste(data.lvl0.ls$method[31], collapse = ' ')),
    'carbon_inorganic', 'method', 'HCL acidification', paste('Method ln32:', paste(data.lvl0.ls$method[32], collapse = ' ')),
    'growth_stage', 'control_vocabulary', 'tillering;grand growth;maturnity', paste('Method ln30:', paste(data.lvl0.ls$method[30], collapse = ' '))
  )
  
  #units from figure legends
  ##methods for soil collection
  
  soil.df <- tibble::tribble(
    ~soil_id, ~of_variable, ~is_type, ~with_entry, ~from_source,
    #soil type
    'Pulehu', 'soil_class', 'value', 'Pulehu series-cobbly silt loam (fine-loamy, mixed semiactive, isohyperthermic Cumulic Haplustolls)', paste('Method ln6:', paste(data.lvl0.ls$method[6], collapse = ' ')),
    'Molokai', 'soil_class', 'value', 'Molokai series-silty clay loam (Very-fine, kaolinitic, isohyperthermic Typic Eutrotorrox)', paste('Method ln6:', paste(data.lvl0.ls$method[6], collapse = ' ')),
    #planting time and harvest
    'Pulehu', 'planting', 'period', '2011-07-19/2013-06-09', paste('Method ln9:', paste(data.lvl0.ls$method[9], collapse = ' ')),
    'Pulehu', 'planting', 'crop', 'sugarcane', paste('Method ln8:', paste(data.lvl0.ls$method[8], collapse = ' ')),
    'Molokai', 'planting', 'crop', 'sugarcane', paste('Method ln8:', paste(data.lvl0.ls$method[8], collapse = ' ')),
    'Molokai', 'planting', 'period', '2011-06-23/2013-05-07', paste('Method ln11:', paste(data.lvl0.ls$method[11], collapse = ' '))
  )
  
  #depth, TC, IC, OC stocks, collection time by growth stage
  # bulk density, total carbon, and soil depth in Table 2 (does not include organic/inorganic)
  # Table 3 total carbon - depth not reported; probably drop
  # table 4 total carbon including soil and roots - depth not reported - probably drop
  # Figure 3 - Disoloved organic carbon - drop
  # Figure 4 - DOC
  # Figure 9 - carbon change, same info as figure 1&2
  # Figure 10 - roots
  ### pull in these elements
  # Table 2: bulk density and total carbon
  # Figure 1: total organic/inorganic carbon on Pulehu soils, multiple time points and depth intervals
  # Figure 2: same as figure 1 for Molokai - check figures
  
  stage.df <- data.lvl0.ls$data$figure2$primary |>
    dplyr::mutate(stage_id = stage,
                  from_source = 'Figure 2') |>
    dplyr::bind_rows(data.lvl0.ls$data$figure1$primary |>
                       dplyr::mutate(from_source = 'Figure 1')) |>
    dplyr::select(stage_id, from_source,
                  `soil depth (cm)`, `Total carbon (kg*m^-2)`,
                  `Inorganic carbon (kg*m^-2)`,
                  stage, site) |>
    tidyr::pivot_longer(cols = c(`Inorganic carbon (kg*m^-2)`, 
                                 `Total carbon (kg*m^-2)`,
                                 stage), 
                        names_to = 'column_name', values_to = 'value') |>
    dplyr::mutate(unit = stringr::str_extract(column_name, '(?<=\\().*(?=\\))'),
                  of_variable = dplyr::case_when(
                    column_name == 'Inorganic carbon (kg*m^-2)' ~ 'carbon_inorganic',
                    column_name == 'Total carbon (kg*m^-2)' ~ 'carbon_total',
                    column_name == 'stage' ~ 'growth_stage',
                    .default = 'unknown')) |>
    rename(layer_id = "soil depth (cm)") |>
    dplyr::mutate(soil_id = stringr::str_extract(site, '\\w+$')) |>
    dplyr::select(soil_id, layer_id, stage_id, site_id = site, 
                  of_variable, value, unit) |>
    tidyr::pivot_longer(cols = c(value, unit),
                        names_to = 'is_type', values_to = 'with_entry',
                        values_drop_na = TRUE)
  
  layer.df <- data.lvl0.ls$data$table2$primary |>
    dplyr::select(soil_id = Soil, `Soil Depth (cm)`, 
                  `TC (kg*m^-2)`, `BD a (g*cm^-3)`) |>
    dplyr::mutate(row_id = paste0('T2 R', 1:n()),
                  layer_id = lapply(stringr::str_split(`Soil Depth (cm)`, 
                                                       pattern = '\\-'),
                                    function(xx)mean(as.numeric(xx))) |> 
                    unlist() |> as.character()) |>
    tidyr::pivot_longer(cols = -c(row_id, layer_id, soil_id), 
                        names_to = 'column_name',
                        values_to = 'value_str') |>
    dplyr::mutate(unit = stringr::str_extract(column_name,  '(?<=\\().*(?=\\))'),
                  lower = stringr::str_extract(value_str,  '(?<=\\-)\\d*'),
                  upper = stringr::str_extract(value_str,  '\\d*(?=\\-)'),
                  value = dplyr::if_else(
                    stringr::str_detect(value_str, pattern = '\\-'), 
                    NA_character_, 
                    stringr::str_extract(value_str, '^\\d+\\.?\\d*')),
                  of_variable = dplyr::case_when(
                    column_name == 'Soil Depth (cm)' ~ 'layer',
                    column_name == 'TC (kg*m^-2)' ~ 'carbon_total',
                    column_name == 'BD a (g*cm^-3)' ~ 'bulk_density_stock',
                    .default = 'unknown')) |>
    dplyr::select(soil_id, layer_id, of_variable, value, lower, upper, unit) |>
    tidyr::pivot_longer(cols = c(value, lower, upper, unit),
                        names_to = 'is_type', values_to = 'with_entry',
                        values_drop_na = TRUE) |>
    dplyr::mutate(from_source = 'Table 2',
                  stage_id = 'baseline')
  
  #### Create level 1 #####
  ## Pull everything together into by stacking the meta and primary data tables
  data.lvl1.ls <- list(
    study = study.df,
    soil = soil.df,
    layer = dplyr::bind_rows(stage.df, layer.df)
  )
  
  #### return level 1 ####
  
  if(dataLevel == 'level1'){
    return(data.lvl1.ls)
  }
}
