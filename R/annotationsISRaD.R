library(tidyverse)
library(readxl)

# Annotations for ISRaD *layer-level* data

# Download the metadata (template information) file with variable/column definitions and controlled vocabulary from: https://soilradiocarbon.org/

downloadUrl <- "https://github.com/International-Soil-Radiocarbon-Database/ISRaD/raw/master/Rpkg/inst/extdata/ISRaD_Template_Info.xlsx?raw=true"
dataDir <- "C:\\Users\\kheck\\Documents\\Data"
filename <- "ISRaD_Template_Info.xlsx"

# Download the template information file
utils::download.file(url = downloadUrl, 
                     destfile = file.path(dataDir, 
                                          filename))

# The file is an Excel Workbook with nine sheets
# the sheets of interest for the layer-level data are:
  # metadata
  # site
  # profile
  # layer
# The sheets can't be easily joined. There are no common identifiers.
# ... annotate as four different tables?
# The flattened data (actual database data) downloaded as csv files is nested as follows:
# metadata <- site <- profile <- layer


# Specify the path to the Excel file
file_path <- "C:\\Users\\kheck\\Downloads\\ISRaD_Template_Info.xlsx"

# Read the names of all sheets in the Excel file
sheet_names <- excel_sheets(file_path)

# Read the sheets into a list of data frames
sheets <- lapply(sheet_names, function(sheet) read_excel(file_path, sheet = sheet))

# Name the list elements with the sheet names
names(sheets) <- sheet_names

# View the list of data frames
str(sheets)

#For ISRaD annotations
## id = Column_Name
## of_variable = Variable_Name
## is_type = some combination of Units/Info, Variable_class, Min, Max, Vocab
## some columns are repeated across the different metadata tables (entry_name,
#        site_name, pro_name)

#######################################################################
# Metadata table annotations
## Rename columns and select for only columns of interest
metadata <- sheets$metadata %>% rename(id = Column_Name, of_variable = Variable_Name, 
                                       unit = "Units/Info", description = Description) %>% 
  select(id, of_variable, unit, description)

## Pivot longer
metadata <- pivot_longer(metadata, c(unit, description), names_to = "is_type", values_to = "with_entry",
                         values_transform = list(with_entry = as.character))

#######################################################################
# Site table annotations
## Rename columns and select for columns of interest
site <- sheets$site %>% rename(id = Column_Name, of_variable = Variable_Name, 
                               unit = "Units/Info", description = Description,
                               control_vocabularies = Vocab) %>% 
  select(id, of_variable, unit, description, control_vocabularies)

##Pivot longer
site <- pivot_longer(site, c(unit, description, control_vocabularies), names_to = "is_type",
                     values_to = "with_entry", values_drop_na = TRUE, values_transform = list(with_entry = as.character))

######################################################################
#Profile table annotations
profile <- sheets$profile %>% rename(id = Column_Name, of_variable = Variable_Name,
                                     unit = "Units/info", description = Description,
                                     control_vocabularies = Vocab) %>% 
  select(id, of_variable, unit, description, control_vocabularies)

##Pivot longer
profile <- pivot_longer(profile, c(unit, description, control_vocabularies), names_to = "is_type",
                     values_to = "with_entry", values_drop_na = TRUE, values_transform = list(with_entry = as.character))

#####################################################################
#Layer table annotations
layer <- sheets$layer %>% rename(id = Column_Name, of_variable = Variable_Name,
                                     unit = "Units/Info", description = Description,
                                     control_vocabularies = Vocab) %>% 
  select(id, of_variable, unit, description, control_vocabularies)

##Pivot longer
layer <- pivot_longer(layer, c(unit, description, control_vocabularies), names_to = "is_type",
                        values_to = "with_entry", values_drop_na = TRUE, values_transform = list(with_entry = as.character))

#####################################################################
#Bind tables into one annotation table and remove duplicate rows (there are multiple instances of 'entry_name', 'site_name', and 'pro_name')
ISRaD_annotations <- bind_rows(metadata, site, profile, layer)
ISRaD_annotations <- distinct(ISRaD_annotations)

#Write to csv?
#Not supposed to create csv files until there's a definite final version?
#write.csv(ISRaD_annotations,"ISRaD_annotations.csv", row.names = FALSE)

#####################################################################
#How to properly code the controlled vocabularies so they are properly separated into key and value?
#There are also columns for maximum and minimum values for the numeric data - I don't think we need to retain these?
