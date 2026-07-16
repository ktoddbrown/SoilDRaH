readAustin2010 <- function(dataDir,
                         dataLevel = c('level0', 'level1')[1],
                         verbose = TRUE){
  
  # #Code devellopment variables, comment out before using this function
  # dataDir <- '01_DataRescue/Austin2010'
  # dataLevel <- 'level0'
  # verbose <- TRUE
  
  ####Set the files####
  
  # data files for level 0
  methods.file <- file.path(dataDir, "Austin2010_Methods.md")
  figure1.file <- file.path(dataDir, 'Austin2010_Figure1.csv')
  
  # Bibliograph files
  primaryCitation.file <- file.path(dataDir, 'Austin2010.bib')
  methodsCitation.file <- file.path(dataDir, 'Austin2010_Methods.bib')
  
  ####Construct level 0 ####
  
  data.lvl0.ls <- list(
  #Read in a list of all the bib files
  citation = list(
    #Citation for the article transcriptions are pulled from
    primary = read.bib(file = primaryCitation.file), 
    #Citations for all referenced articles
    methods = read.bib(file = methodsCitation.file)
  ),
  #Read in the text transcription of the article's methods section
  method = read_lines(file = methods.file),
  #Read in the results as tables or figure transcriptions. This includes
  #...the caption as well as the tables themselves
  data = list(
    Table1 = list(
      #Read the caption as a text string. Captions are the first cell on 
      #...the first row.
      caption = read_csv(file = figure1.file,
                         col_types = cols(.default = col_character()),
                         n_max = 1, col_names = FALSE)$X1[1],
      #Read in all the data, skipping the first row with the caption and read
      #...in the table as character. This element is a tibble (data.frame).
      primary = read_csv(file = figure1.file,
                         col_types = cols(.default = col_character()),
                         skip = 1)
    ) #End 'Table1' element
    #Repeat for all tables and figures...
   ) #end 'data' element
)
  
  if(dataLevel == 'level0'){
    return(data.lvl0.ls)
  }
  
  #### Construct level 1 ####
  #### 
  
  #Create a table for the data in the method-section that is not directly 
  #...associated with any table. Often this is study level information.
  
  study.df <- tribble(~of_variable, ~is_type, ~with_entry, ~from_source,
                      'region', 'country', 'Argentina', paste('Method ln4:', paste(data.lvl0.ls$method[4], collapse = ' ')),
                      'region', 'province', 'Córdoba', paste('Method ln4:', paste(data.lvl0.ls$method[4], collapse = ' ')),
                      'geolocation', 'latitude', as.character(-1*(31 + 4/60)), paste('Method ln4:', paste(data.lvl0.ls$method[4], collapse = ' ')),
                      'geolocation', 'longitude', as.character(64 + 31/60), paste('Method ln4:', paste(data.lvl0.ls$method[4], collapse = ' ')),
                      'geolocation', 'unit', 'decimal degrees', paste('Converted from method ln4:', paste(data.lvl0.ls$method[4], collapse = ' ')),
                      'exposure_duration', 'value', '120', paste('Method ln4:', paste(data.lvl0.ls$method[14], collapse = ' ')),
                      'exposure_duration', 'unit', 'days', paste('Method ln4:', paste(data.lvl0.ls$method[14], collapse = ' ')),
                      'soil_contact', 'value', 'none', paste('Method ln8-11:', paste(data.lvl0.ls$method[8:11], collapse = ' ')),
                      'litter_type', 'value', 'perennial tussock grasses, including *Festuca hieronymi* and *Stipa trichotoma*', paste('Method ln5:', paste(data.lvl0.ls$method[5], collapse = ' ')),
                      'mass_loss', 'unit', 'mass-percent per day', 'Table 1 column name',
                      'litter_age_class', 'value', 'previous year', paste('Method ln6-7:', paste(data.lvl0.ls$method[6:7], collapse = ' ')),
                      'vegitation_class', 'value', 'open woodland vegetation', paste('Method ln4:', paste(data.lvl0.ls$method[4], collapse = ' ')),
                      #Initial litter quality measurements for carbon chemistry, made by using a standard acid detergent method (@VanSoest1963), were the following: cellulose and sugars, 49.1 ± 0.4%; hemicellulose, 37.3 ± 0.2%; and lignin,7.3 ± 0.2%.
                      'litter_fraction', 'cellulose_and_sugar:mean', '49.1', paste('Method ln16:', paste(data.lvl0.ls$method[16], collapse = ' ')),
                      'litter_fraction', 'cellulose_and_sugar:sd', '0.4', paste('Method ln16:', paste(data.lvl0.ls$method[16], collapse = ' ')),
                      'litter_fraction', 'hemicellulose:mean', '37.3', paste('Method ln16:', paste(data.lvl0.ls$method[16], collapse = ' ')),
                      'litter_fraction', 'hemicellulose:sd', '0.2', paste('Method ln16:', paste(data.lvl0.ls$method[16], collapse = ' ')),
                      'litter_fraction', 'lignin:mean', '7.3', paste('Method ln16:', paste(data.lvl0.ls$method[16], collapse = ' ')),
                      'litter_fraction', 'lignin:sd', '0.2', paste('Method ln16:', paste(data.lvl0.ls$method[16], collapse = ' ')),
                      'litter_fraction', 'unit', 'mass percent', paste('Method ln16:', paste(data.lvl0.ls$method[16], collapse = ' ')),
                      'litter_fraction', 'method', 'acid digestion', paste('Method ln16:', paste(data.lvl0.ls$method[16], collapse = ' ')),
                      'litter_deployment', 'value', 'open box', paste('Method ln9-11:', paste(data.lvl0.ls$method[9:11], collapse = ' ')),
                      'rain_out', 'value', 'TRUE',paste('Method ln13:', paste(data.lvl0.ls$method[13], collapse = ' ')),
                      'mass_loss', 'unit', 'mass-percent per day', 'Table 1 header'
  ) #close tibble
  
  treatment.df <- tribble(~treatement_id, ~of_variable, ~is_type, ~with_entry,
                          'control', 'solar_filter', 'transmission_fraction', '0.95',
                          'control', 'solar_filter', 'transmission_wavelength', 'all',
                          'UV-B', 'solar_filter', 'attenuation', '280-315 nm',
                          'UV', 'solar_filter', 'attenuation', '280-400 nm',
                          'UV and blue', 'solar_filter', 'attenuation', '280-450 nm'
  ) |>
    mutate(from_source = paste('Method ln4:', paste(data.lvl0.ls$method[12], collapse = ' ')))
  
  litter_loss.df <- data.lvl0.ls$data$Table1$primary |>
    select(litter_type = Substrate, transmission_wavelengths = `Transmitted Wavelengths (nm)`,
           mass_loss = `Mass loss (% per day)`, mass_loss_sem = `Mass loss (% per day) SEM`) |>
    #pull in the grass litter but not the artificial substrate  
    filter(str_detect(litter_type, '[Gg]rass.*')) |>
    mutate(treatement_id = case_when(
      transmission_wavelengths == '>450' ~ 'UV and blue',
      transmission_wavelengths == '>400' ~ 'UV',
      transmission_wavelengths == '>315' ~ 'UV-B',
      transmission_wavelengths == '>290' ~ 'control',
      .default = '[bad coding. Stop!]'
    )) |>
    #discard the grass information
    #NOTE if you end up pulling the synthetic decompotion you will need to change this code here!
    select(treatement_id, mass_loss, mass_loss_sem) |>
    pivot_longer(cols = c(mass_loss, mass_loss_sem),
                 names_to = 'column_name', values_to = 'with_entry') |>
    mutate(is_type = case_when(str_detect(column_name, 'sem$') ~ 'sem',
                               column_name == 'mass_loss' ~ 'value',
                               .default = '[BAD MATCH]'),
           of_variable = str_extract(column_name, 'mass_loss')) |>
    select(treatement_id, of_variable, is_type, with_entry) |>
    mutate(from_source = 'Table 1')
  

  ## Pull everything together into by stacking the meta and primary data tables
  data.lvl1.ls <- list(
    meta = study.df,
    primary = treatment.df |>
      bind_rows(litter_loss.df)
  )
  
  if(dataLevel == 'level1'){
    return(data.lvl1.ls)
  }
  
  stop('Bad data level specified')
}