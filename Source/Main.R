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
  assign(str_remove(files[i], ".csv"), read_csv(files[i]))
}
data_oecd <- MIG_16092018154825543
rm(MIG_16092018154825543, files)

# Return to WorkDir
setwd(WorkDir)
names(data_oecd)[1] <- "country_iso3"
data_merged <- merge(data_oecd, data_presence) %>%
  merge(data_abroad)

