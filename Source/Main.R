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

# Return to WorkDir
# Build combined data frame
setwd(WorkDir)
names(data_oecd)[1] <- "country_iso3"
names(data_oecd)[2] <- "nationality" 
data_joined <- right_join(data_oecd, data_abroad) 
data_joined <- right_join(data_joined, data_presence)
data_joined <- filter(data_joined, !is.na(var))
data_joined <- filter(data_joined, nationality = "Germany")
rm(data_abroad, data_ger, data_presence, data_oecd)

# Build list of Countries
Countries <- unique(data_joined$`Country of birth/nationality`)