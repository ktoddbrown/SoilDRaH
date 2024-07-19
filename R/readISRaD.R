# Code for reading in the International Soil Radiocarbon Database (ISRaD)
# 
# Location of the latest release of the ISRaD database (periodic pushes from GitHub): https://soilradiocarbon.org/
# 

library(tidyverse)

# Download URL for entire ISRaD database
downloadUrl <- "https://github.com/International-Soil-Radiocarbon-Database/ISRaD/raw/main/ISRaD_data_files/database/ISRaD_database_files.zip"

# Where is downloaded data stored?
# Kate is unsure where the downloaded data is stored because she doesn't see the csv files on the GitHub repo
# So just working on personal hard drive for now
dataDir <- "C:\\Users\\kheck\\Documents\\Data"

# What name is the downloaded folder?
foldername <- "ISRaD_database_files.zip"

# What name is the downloaded file for *layer-level* data?
# NOTE that the meta-data and site level tables are already joined to any files with data at a finer resolution (i.e. "layer", "fraction", "flux", "interstitial")
# This matters for how annotations are put together... one "table" or several tables of annotations?
filename <- "ISRaD_data_flat_layer_v 2.6.6.2024-01-25.csv"

#Download ISRaD files
utils::download.file(url = downloadUrl, 
                     destfile = file.path(dataDir, 
                                          foldername))

# Unzip zip folder
utils::unzip(file.path(dataDir, foldername), exdir = dataDir, overwrite = FALSE)

# Read the CSV file with all columns as characters
original_data <- read_csv("C:\\Users\\kheck\\Documents\\Data\\ISRaD_data_flat_layer_v 2.6.6.2024-01-25.csv",
                          col_types = cols(.default = col_character()))
colnames(original_data)

# There are 36,706 observations for 184 variables at the *layer* level
# NOTE that the csv files served by ISRaD include row numbers as column 1, so will need to ignore the first row when reading in data?