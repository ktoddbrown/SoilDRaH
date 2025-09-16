#' Title
#'
#' @param dataDir 
#' @param format 
#' @param verbose 
#'
#' @returns
#' @export
#' 
#' @importFrom bibtex read.bib
#' @importFrom readr read_lines read_csv cols col_character
#'
#' @examples
readAres2001 <- function(dataDir,
                    #annotationFilename,
                    format = c('original', 'level1')[1],
                    verbose = TRUE){
  
  #coment out this before leaving
  dataDir <- 'data/rescue/Ares2001'
#   library(tidyverse)
# library(kableExtra)
# library(bibtex)
# 
  methods.file <- file.path(dataDir, 'Ares2001_Methods.md')
  table1.file <- file.path(dataDir, 'Ares2001_Table1.csv')
  table3.file <- file.path(dataDir, 'Ares2001_Table3.csv')
  primaryCitation.file <- file.path(dataDir, 'Ares2001.bib')
  methodsCitation.file <- file.path(dataDir, 'Ares2001_Methods.bib')
  
  ###Level 0 data read rescued from pdf ####
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

  return(data.lvl0.df)
}
