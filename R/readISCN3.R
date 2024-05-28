#' Load ISCN3
#'
#' This function first downloads the layer, profile, citation, and dataset tables from 
#'
#' @param dataDir path to the folder containing data files.
#' @param verbose boolean flag denoting whether or not to print lots of status messages
#' @param annotationFilename 
#' @param format 
#' 
#' @return list of annotation and data tables
#'
#' @importFrom data.table rbindlist
#' @importFrom tibble tribble cols col_character
#' @importFrom readr read_delim
#' @importFrom dplyr mutate bind_rows 
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect all_of
#' @import magrittr
#' 
#' @export
#' 
readISCN3 <- function(dataDir,
                      annotationFilename,
                      format = c('original', 'long')[1],
                      verbose = TRUE){
  
  ## construct file paths ####
  if(is.null(dataDir)){
    stop('Data folder must be specified.')
  }
  
  dataFiles.ls <- list(layer = file.path(dataDir, 'ISCN3_layer.csv'), 
                    profile = file.path(dataDir, 'ISCN3_profile.csv'),
                    citation = file.path(dataDir, 'ISCN3_citation.csv'),
                    dataset = file.path(dataDir, 'ISCN3_dataset.csv'),
                    otherDocs = file.path(dataDir, c('ISCNtemplate_2016.csv',
                                            'ISCNTranscribed_TemplateCVs.csv',
                                            'ISCN-Database.pdf',
                                            'Gen3-DB-documentation_REV.pdf',
                                            'TemplateCVs.pdf',
                                            'TemplateSubmit.pdf',
                                            'C&QA.Rmd')))
  
  ## Download the data ####
  
  download_table <- tibble::tribble(~download_url, ~file_name, 
                                    "https://portal-s.edirepository.org/nis/dataviewer?packageid=edi.360.4&entityid=4af719a84f8981fcc63f1f92760cb253", file.path(dataDir, 'ISCN3_layer.csv'),
                                    "https://portal-s.edirepository.org/nis/dataviewer?packageid=edi.360.4&entityid=40527580cc045d33d9a5aaf728bf204e", file.path(dataDir, 'ISCN3_profile.csv'),
                                    "https://portal-s.edirepository.org/nis/dataviewer?packageid=edi.360.4&entityid=320e31ca911f187550ca2143c31fd408", file.path(dataDir, 'ISCN3_citation.csv'),
                                    "https://portal-s.edirepository.org/nis/dataviewer?packageid=edi.360.4&entityid=cdd0c7a4cac3f28d6d788c91f506775f", file.path(dataDir, 'ISCN3_dataset.csv'),
                                    "https://portal-s.edirepository.org/nis/dataviewer?packageid=edi.360.4&entityid=a8eef6e94b669b365e443c15d9402a03", file.path(dataDir, 'ISCNTranscribed_TemplateCVs.csv'),
                                    "https://portal-s.edirepository.org/nis/dataviewer?packageid=edi.360.4&entityid=114c95dd318e088108158adc3ae4eb23", file.path(dataDir, 'ISCNtemplate.csv'),
                                    "https://portal-s.edirepository.org/nis/dataviewer?packageid=edi.360.4&entityid=eb320ae7b57296765f543cbb370b0f24", file.path(dataDir, 'TemplateCVs.pdf'),
                                    "https://portal-s.edirepository.org/nis/dataviewer?packageid=edi.360.4&entityid=b81a7f4214d176280a5ff4e0f0d52d8b", file.path(dataDir, 'TemplateSubmit.pdf'),
                                    "https://portal-s.edirepository.org/nis/dataviewer?packageid=edi.360.4&entityid=f914e6978c595c9a373dbc58365b6795", file.path(dataDir, 'Gen3-DB-documentation_REV.pdf'),
                                    "https://portal-s.edirepository.org/nis/dataviewer?packageid=edi.360.4&entityid=4dbc81eab612e09b84c688bb387d06c2", file.path(dataDir, 'ISCN-Database.pdf'),
                                    "https://portal-s.edirepository.org/nis/dataviewer?packageid=edi.360.4&entityid=3903927ae52655ff6359bc7c454aa42e", file.path(dataDir, 'C&QA.Rmd'))
  

  
  for(row_index in 1:nrow(download_table)){
    if(!file.exists(download_table$file_name[row_index])){
        if(verbose) print(paste('Download file:', download_table$file_name[row_index]))
      utils::download.file(download_table$download_url[row_index], download_table$file_name[row_index], quiet=!verbose)
    }
  }
  
  
  
  ## Read data files ####
  
  if(verbose) print('Meta data read in.')
  
  citation.df <- readr::read_delim(dataFiles.ls$citation, 
                                   delim = ';', 
                                   col_types = readr::cols(.default = readr::col_character()))
  
  dataset.df <- readr::read_delim(dataFiles.ls$dataset, 
                                   delim = ';', 
                                   col_types = readr::cols(.default = readr::col_character()))
  
  if(verbose) print('Profile data read in.')
  profile.df <- readr::read_delim(dataFiles.ls$profile, 
                                   delim = ';', 
                                   col_types = readr::cols(.default = readr::col_character()))
  
  if(verbose) print('Layer data read in.')
  layer.df <- readr::read_delim(dataFiles.ls$layer, 
                                   delim = ';', 
                                   col_types = readr::cols(.default = readr::col_character()))
  
  #TODO add to the annotation file
  ##add collection level details like citation
  # collection.df <- data.table::data.table(collection_name_id = 'ISCN3.2', 
  #                                                  variable = 'collection_citation',
  #                                                  entry = 'Nave L, Johnson K, van Ingen C, Agarwal D, Humphrey M, Beekwilder N. 2017. International Soil Carbon Network (ISCN) Database, Version 3.2. DOI: 10.17040/ISCN/1305039. Database Report: ISCN_SOC-DATA_LAYER_1-1. Accessed 2 February 2017',
  #                                                  type = 'value')
  
  annotations.df <- readr::read_delim(file = annotationFilename,
                                      delim = ',', 
                                   col_types = readr::cols(.default = readr::col_character()))
  
  if(format == 'orginal'){
    return(list(orginal = list(citation = citation.df, 
                               datasets = dataset.df,
                               profile = profile.df,
                               layer = layer.df,
                               files = download_table),
                annotation = annotations.df))
  }
  
  citation_long <- citation.df %>% 
    dplyr::mutate(dataset_id = dataset_name) %>%
    tidyr::pivot_longer(cols = -dataset_id,
                        names_to = 'column_id',
                        values_to = 'with_entry',
                        values_drop_na = TRUE) %>%
    dplyr::mutate(table_id = 'citation')
  
  dataset_long <- dataset.df %>%
    dplyr::mutate(dataset_id = dataset_name) %>%
    tidyr::pivot_longer(cols = -dataset_id,
                        names_to = 'column_id',
                        values_to = 'with_entry',
                        values_drop_na = TRUE) %>%
    dplyr::mutate(table_id = 'dataset')
  
  temp <- dplyr::bind_rows(citation_long,
                    dataset_long)

  profile_long <- profile.df %>%
    dplyr::mutate(dataset_id = dataset_name_sub,
                  profile_id = profile_name,
                  soc_id = dataset_name_soc) %>%
    tidyr::pivot_longer(cols = -tidyselect::any_of(c("dataset_id", 
                                         "profile_id", "soc_id")),
                        names_to = 'column_id',
                        values_to = 'with_entry',
                        values_drop_na = TRUE) %>%
    dplyr::mutate(table_id = 'profile')
  
  layer_long <- layer.df %>%
    dplyr::mutate(dataset_id = dataset_name_sub,
                  profile_id = profile_name,
                  layer_id = layer_name,
                  soc_id = dataset_name_soc) %>%
    tidyr::pivot_longer(cols = -tidyselect::any_of(c("dataset_id", 
                                         "profile_id", 
                                         "layer_id","soc_id")),
                        names_to = 'column_id',
                        values_to = 'with_entry',
                        values_drop_na = TRUE) %>%
    dplyr::mutate(table_id = 'layer')
    
  if(verbose) message('Returning long data (>1Gb).')
  return(list(annotation = annotations.df,
              long =  dplyr::bind_rows(citation_long, 
                                       dataset_long,
                                       profile_long,
                                       layer_long)))

}


