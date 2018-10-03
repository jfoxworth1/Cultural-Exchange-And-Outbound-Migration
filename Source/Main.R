# List Dependencies
require(tidyverse)

# list important directories
MainDir <- "/home/me/Documents/Uni-Jena/Thesis/Cultural-Exchange-And-Outbound-Migration/"
WorkDir <- paste0(MainDir, "Source")
DataDir <- paste0(MainDir, "Data")

# download data into memory
setwd(DataDir)
files <- list.files(pattern = ".csv$")
for(i in seq_along(files)){
  object_name <- str_remove(files[i], ".csv")
  tmp <- read_csv(files[i])
  names(tmp) <- tolower(colnames(tmp))
  assign(object_name, tmp)
}
data_oecd <- MIG_16092018154825543
rm(MIG_16092018154825543, files, tmp, object_name)
setwd(WorkDir)

# Build combined data frame
# Data is the amount of germans that flow into other countries
# Limit OECD data to necessary bits
names(data_oecd)[7] <- "country_iso3"
names(data_oecd)[2] <- "nationality" 
data_oecd <- filter(data_oecd, nationality == "Germany")
data_oecd <- filter(data_oecd, data_oecd$variable == "Inflows of foreign population by nationality")

# Join all the data together
data_joined <- right_join(data_oecd, data_abroad) 
data_joined <- right_join(data_joined, data_presence)
data_joined <- filter(data_joined, !is.na(var))
rm(data_abroad, data_ger, data_presence, data_oecd)

# Build list of Countries
Countries <- unique(data_joined$country)
Countries <- Countries[order(Countries)]

