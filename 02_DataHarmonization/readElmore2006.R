#' Read Elmore and Asner 2006
#' 
#' Read in the rescued data from Elmore and Asner 2006 that was ingested for the HiCSC project in 2025. 
#' Elmore and Asner 2006 was investigates the impact of livestock grazing on soil carbon (C) levels following the clearing of dry tropical forests in Hawaii. 
#' The findings show that long-term grazing reduces soil organic carbon, with the extent of these changes increasing at higher elevations.
#' 
#' Elmore, A.J. and Asner, G.P. (2006), Effects of grazing intensity on soil carbon stocks following deforestation of a Hawaiian dry tropical forest. Global Change Biology, 12: 1761-1772. https://doi.org/10.1111/j.1365-2486.2006.01198.x
#' 
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
#' @importFrom dplyr mutate select filter case_when bind_rows
#' @importFrom stringr str_detect str_extract
#' @importFrom tidyr pivot_longer
#'
#' @examples
readElmore2006 <- function(dataDir,
                           dataLevel = c('level0', 'level1')[1],
                           verbose = TRUE){
  
  # #Code devellopment variables, comment out before using this function
  # dataDir <- '01_DataRescue/Elmore2006
  # dataLevel <- 'level0'
  # verbose <- TRUE
  
  ####Set the files####

  # data files for level 0
  methods.file <- file.path(dataDir, "Elmore2006_Methods.md")
  table1.file <- file.path(dataDir, 'Elmore2006_Table1.csv')
  
  # Bibliograph files
  primaryCitation.file <- file.path(dataDir, 'Elmore2006.bib')
  methodsCitation.file <- file.path(dataDir, 'Elmore2006_Methods.bib')
  
  ####Construct level 0 ####  

  data.lvl0.ls <- list(
    citation = 
      list(primary = 
             bibtex::read.bib(file = primaryCitation.file), 
           methods = 
             bibtex::read.bib(file = methodsCitation.file)
      ), #end citation
    method = readr::read_lines(file = methods.file),
    data = list(
      Table1 = 
                  list(
                    caption = 
                         readr::read_csv(file = table1.file,
                                  col_types = readr::cols(.default = readr::col_character()),
                                  n_max = 1, col_names = FALSE)$X1[1],
                       primary = 
                         readr::read_csv(file = table1.file,
                                  col_types = readr::cols(.default = readr::col_character()),
                                  skip = 1,
                                  na = '-')
                  ) #end Table 1
    ) #end data
  ) 
  
  if(dataLevel == 'level0'){
    return(data.lvl0.ls)
  }
  
  #### Construct level 1 ####
  #### 
  
  #Create a table for the data in the method-section that is not directly 
  #...associated with any table. Often this is study level information.
  
  studyMeta <- tibble::tribble(~of_variable, ~is_type, ~with_entry, ~from_source,
                       'citation', 'value', format(data.lvl0.ls$citation$primary), 'journal citation',
                       'doi', 'value', data.lvl0.ls$citation$primary$doi, 'journal citation',
                       'region', 'island', "Island of Hawai'i", paste('Method ln1:', paste(data.lvl0.ls$method[1], collapse = ' ')),
                       'region', 'locality', "portion of Pu'u Wa'awa'a Ranch on the north side of Hualalai volcano", paste('Method ln1:', paste(data.lvl0.ls$method[1], collapse = ' ')), 
                       'elevation', 'min', '850', paste('Method ln8:', paste(data.lvl0.ls$method[8], collapse = ' ')),
                       'elevation', 'max', '1100', paste('Method ln8:', paste(data.lvl0.ls$method[8], collapse = ' ')),
                       'elevation', 'unit', 'm', paste('Method ln8:', paste(data.lvl0.ls$method[8], collapse = ' ')),
                       'soil_age', 'min', '2500', paste('Method ln6:', paste(data.lvl0.ls$method[6], collapse = ' ')),
                       'soil_age', 'max', '3500', paste('Method ln6:', paste(data.lvl0.ls$method[6], collapse = ' ')),
                       'soil_age', 'unit', 'yrs', paste('Method ln6:', paste(data.lvl0.ls$method[6], collapse = ' ')),
                       'precipitation', 'min', '625', paste('Method ln7:', paste(data.lvl0.ls$method[7], collapse = ' ')),
                       'precipitation', 'max', '900', paste('Method ln7:', paste(data.lvl0.ls$method[7], collapse = ' ')),
                       'precipitation', 'unit', 'mm', paste('Method ln7:', paste(data.lvl0.ls$method[7], collapse = ' ')),
                       'land_use', 'value', 'Cattle grazing', paste('Method ln21:', paste(data.lvl0.ls$method[21], collapse = ' ')),
                       'land_use', 'interval', '1900',  paste('Method ln21:', paste(data.lvl0.ls$method[21], collapse = ' ')),
                       'land_use', 'interval_format', 'YYYY', NA,
                       'transect', 'value', '1000',   paste('Method ln57:', paste(data.lvl0.ls$method[57], collapse = ' ')),
                       'transect', 'unit', 'm',   paste('Method ln57:', paste(data.lvl0.ls$method[57], collapse = ' ')),
                       'soil_sampling', 'value', 'soil cores were collected at 25 m intervals close to the grazing center, and spaced to 50 and 100 m further from the center',  paste('Method ln74:', paste(data.lvl0.ls$method[74], collapse = ' ')) )
  
  #### Table 1 ####
  
  Table1Primary <- data.lvl0.ls$data$Table1$primary |>
    dplyr::mutate(row_id = paste0('R', 1:n())) |>
    tidyr::pivot_longer(cols = -row_id,
                 names_to = 'column_name', values_to = 'with_entry',
                 values_drop_na = TRUE) |>
    
    dplyr::mutate(of_variable = dplyr::case_when(
      column_name == "Sample #" ~ "sample_id",
      column_name == "Distance (m)" ~ "grazing_distance",
      column_name == "Elevation (m)" ~ "elevation",
      column_name == 'Percent Carbon' ~ "soil_carbon",
      column_name == 'delta$^{13}$C' ~ "delta_13_carbon",
      column_name == "rho(g cm$^{-3}$)" ~ "dry_bulk_density",
      column_name == "TOC (kg C m$^{-2}$)" ~ "soil_organic_carbon")) |>
    dplyr::mutate(from_source = 'Table 1')
  Table1Meta <- Table1Primary |>
    dplyr::select(column_name, of_variable) |>
    unique() |>
    dplyr::mutate(unit = str_extract(column_name, pattern = '(?<=\\().*(?=\\))'),
           unit = if_else(column_name == "% C",
                          "percent", unit),
           from_source = 'Table 1 column names.') |>
    dplyr::filter(!is.na(unit)) |>
    dplyr::bind_rows(
      tibble::tribble(~of_variable, ~method, ~from_source,
              'delta_13_carbon', paste0(data.lvl0.ls$method[c(99,103:106)], collapse = ' '), 'Methods ln99;103-106',
              'dry_bulk_density', paste0(data.lvl0.ls$method[96], collapse = ' '), 'Methods ln96',
              'soil_organic_carbon',  paste0(data.lvl0.ls$method[c(94, 96, 98)], collapse = ' '), 'Methods ln94;96;98') |>
        
        dplyr::left_join(Table1Primary |>
                    dplyr::select(column_name, of_variable) |>
                    unique(),
                  by = dplyr::join_by(of_variable))) |>
    tidyr::pivot_longer(cols = c(unit, method),
                 names_to = 'is_type',
                 values_drop_na = TRUE,
                 values_to = 'with_entry')
  #### Create level 1 #####
  data.lvl1.ls <- list(
    study = studyMeta,
    primary_meta = dplyr::bind_rows(Table1Meta),
    primary = bind_rows(Table1Primary)|>
      dplyr::mutate(grazing_id = with_entry[of_variable == 'sample_id'],
             is_type = 'value',
             .by = row_id) |>
      dplyr::arrange(row_id, grazing_id, column_name,
              of_variable, is_type, with_entry, from_source)
  )
  if(dataLevel == 'level1'){
    return(data.lvl1.ls)
  }
  stop('Bad data level specified')
}