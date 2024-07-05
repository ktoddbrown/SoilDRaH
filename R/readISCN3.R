#' Load ISCN3
#'
#' This function first downloads the layer, profile, citation, and dataset tables from the International Soil Carbon Network version 3 Database.
#' 
#' Database reference: Nave, L., K. Johnson, C. van Ingen, D. Agarwal, M. Humphrey, and N. Beekwilder. 2022. International Soil Carbon Network version 3 Database (ISCN3) ver 1. Environmental Data Initiative. https://doi.org/10.6073/pasta/cc751923c5576b95a6d6a227d5afe8ba (Accessed 2024-06-13).
#'
#' @param dataDir path to the folder containing data files.
#' @param verbose boolean flag denoting whether or not to print lots of status messages
#' @param annotationFilename path to the annotation file (generally located in 'data')
#' @param format flag for the format to return the data 
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
  
  ##dev sets
  #dataDir <- '~/Dropbox (UFL)/Research/Datasets/ISCN3'
  #annotationFilename <- 'data/ISCN3Annotations.csv'
  
  ## construct file paths ####
  if(is.null(dataDir)){
    stop('Data folder must be specified.')
  }
  
  # list the files for each table and other documentation.
  
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
  
  if(verbose) print('Read data about the dataset and citations in.')
  
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
  
  if(verbose) print('Read in the annotations file generated by SoilDRaH.')
  annotations.df <- readr::read_delim(file = annotationFilename,
                                      delim = ',', 
                                   col_types = readr::cols(.default = readr::col_character()))
  
  # if the flag is in the original format don't change anything and return it
  if(format == 'orginal'){
    return(list(orginal = list(citation = citation.df, 
                               datasets = dataset.df,
                               profile = profile.df,
                               layer = layer.df,
                               files = download_table),
                annotation = annotations.df))
  }
  
  #identify the id column and make everything else long
  citation_long <- citation.df %>% 
    dplyr::mutate(dataset_id = dataset_name) %>%
    tidyr::pivot_longer(cols = -dataset_id,
                        names_to = 'column_id',
                        values_to = 'with_entry',
                        values_drop_na = TRUE) %>%
    #add in the table id
    dplyr::mutate(table_id = 'citation')
  
  #identify the dataset id
  dataset_long <- dataset.df %>%
    dplyr::mutate(dataset_id = dataset_name) %>%
    tidyr::pivot_longer(cols = -dataset_id,
                        names_to = 'column_id',
                        values_to = 'with_entry',
                        values_drop_na = TRUE) %>%
    #add in the table id
    dplyr::mutate(table_id = 'dataset')
  
  #bind everything using a bind_rows
  temp <- dplyr::bind_rows(citation_long,
                    dataset_long)
  
  
  profile_long <- profile.df %>%
    #set the ids for the dataset, profile, and soc
    dplyr::mutate(dataset_id = dataset_name_sub,
                  profile_id = profile_name,
                  soc_id = dataset_name_soc) %>%
    #pivot everything longer
    tidyr::pivot_longer(cols = -tidyselect::any_of(c("dataset_id", 
                                         "profile_id", "soc_id")),
                        names_to = 'column_id',
                        values_to = 'with_entry',
                        values_drop_na = TRUE) %>%
    #set the data table
    dplyr::mutate(table_id = 'profile')
  
  layer_long <- layer.df %>%
    #add the layer id to the dataset, profile, and soc source
    dplyr::mutate(dataset_id = dataset_name_sub,
                  profile_id = profile_name,
                  layer_id = layer_name,
                  soc_id = dataset_name_soc) %>%
    #Modify the layer_id for World Soils data set to correct for repeated id's across depth
    dplyr::mutate(layer_id = 
                    if_else(dataset_id %in% 
                              c('Worldwide soil carbon and nitrogen data', 'Northern Circumpolar Soil Carbon Database (NCSCD)'),
                            paste0(layer_id, '::interval ', 
                                   `layer_top (cm)`, '-', 
                                   `layer_bot (cm)`), layer_id)) %>%
    #pivot everything else longer
    tidyr::pivot_longer(cols = -tidyselect::any_of(c("dataset_id", 
                                         "profile_id", 
                                         "layer_id","soc_id")),
                        names_to = 'column_id',
                        values_to = 'with_entry',
                        values_drop_na = TRUE) %>%
    #add the id
    dplyr::mutate(table_id = 'layer')
    
  #make sure that the user knows this is a lot of data
  if(verbose) message('Returning long data (>1Gb).')
  
  #bind all the data tables together in a long format
  return(list(long =  dplyr::bind_rows(citation_long, 
                                       dataset_long,
                                       profile_long,
                                       layer_long),
              annotation = annotations.df))

}


