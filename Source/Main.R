# list important directories
MainDir <- "/home/me/Documents/Uni-Jena/Thesis/Cultural-Exchange-And-Outbound-Migration/"
WorkDir <- paste0(MainDir, "Source/")
DataDir <- paste0(MainDir, "Data/")

# List Dependencies
require(tidyverse)
require(tseries)
source(paste0(WorkDir, "is_open.R"))
source(paste0(WorkDir, "teaches_language.R"))
source(paste0(WorkDir, "is_EU.R"))

# download data into memory
setwd(DataDir)
files <- list.files(pattern = ".csv$")
for(i in seq_along(files)){
  object_name <- str_remove(files[i], ".csv")
  tmp <- read_csv(files[i])
  names(tmp) <- tolower(colnames(tmp))
  assign(object_name, tmp)
}
rm(files, tmp, object_name)
setwd(WorkDir)

# Combine Data about OECD countries and trim out unnecesary parts
names(OECD_GDP)[1] <- "country_iso3"
names(OECD_GDP)[6] <- "year" 
names(OECD_GDP)[7] <- "GDP_USD_CAP"
OECD_GDP <- select(OECD_GDP, country_iso3, year, GDP_USD_CAP)

# German GDP Figure
Germany_GDP <- filter(OECD_GDP, country_iso3 == "DEU")
names(Germany_GDP)[3] <- "Germany_GDP_USD_CAP"
Germany_GDP <- select(Germany_GDP, -country_iso3)

# German Migrant Employment rates
OECD_Migrant_Emplyment_Rates <- OECD_Migrant_Emplyment_Rates[c(-6, -9,-15)]
names(OECD_Migrant_Emplyment_Rates)[1] <- "country_iso3"
names(OECD_Migrant_Emplyment_Rates)[6] <- "rate_code"
names(OECD_Migrant_Emplyment_Rates)[14] <- "Migrant_Employment_Rate"
OECD_Migrant_Emplyment_Rates <- filter(OECD_Migrant_Emplyment_Rates, gender == "TOT",
                                rate_code == "N_RATE",
                                birth == "FB")
OECD_Migrant_Emplyment_Rates <- select(OECD_Migrant_Emplyment_Rates, country_iso3, year, Migrant_Employment_Rate)

# Population numbers measured in millions
names(OECD_Population)[1] <- "country_iso3"
names(OECD_Population)[6] <- "year"
names(OECD_Population)[7] <- "Population_Value_Mil"
OECD_Population <- select(OECD_Population, country_iso3, year, Population_Value_Mil)

# Base of data_oecd
names(data_oecd)[7] <- "country_iso3"
names(data_oecd)[11] <- "Migration_Value"
data_oecd <- filter(data_oecd, data_oecd$`country of birth/nationality` == "Germany", 
                    data_oecd$variable == "Inflows of foreign population by nationality",
                    year <= max(data_abroad$year))
data_oecd <- select(data_oecd, -co2, -'country of birth/nationality', -var, -variable, -gen, -gender,-yea, -'flag codes', -flags)

# EU_Member dummy variable
data_oecd <- cbind(data_oecd, rep(0, length(data_oecd[1]))) 
names(data_oecd)[length(data_oecd)] <- "EU_Member"
data_oecd$EU_Member[mapply(is_EU, data_oecd$country_iso3, data_oecd$year)] <- 1

# GI present dummy variable
# uses is_open from other file (WordDir/is_open.R)
data_oecd <- cbind(data_oecd, rep(0, length(data_oecd[1]))) 
names(data_oecd)[length(data_oecd)] <- "GI_Present"
data_oecd$GI_Present[mapply(is_open, data_oecd$country_iso3, data_oecd$year)] <- 1

# Offer Language Learning Dummy
data_oecd <- cbind(data_oecd, rep(1, length(data_oecd[1])))
names(data_oecd)[length(data_oecd)] <- "Language_Taught"
data_oecd$Language_Taught[!mapply(teaches_language, data_oecd$country_iso3, data_oecd$year)] <- 0

# Data joining
data_oecd <- right_join(OECD_GDP, data_oecd, by = c("country_iso3", "year"))
data_oecd <- right_join(OECD_Migrant_Emplyment_Rates, data_oecd, by = c("country_iso3", "year"))
data_oecd <- right_join(Germany_GDP, data_oecd, by = "year")
data_oecd <- right_join(OECD_Population, data_oecd, by = c("country_iso3", "year"))

# Garbage collection
rm(OECD_GDP, OECD_Migrant_Emplyment_Rates, Germany_GDP, EU_Countries, OECD_Population)
# Create tables for the two different analysis
# Based on Units
data_abroad <- filter(data_abroad, data_abroad$country_iso3 %in% unique(data_oecd$country_iso3),
                      data_abroad$year >= min(data_oecd$year))
data_unit_analysis <- right_join(data_abroad, data_oecd, by = c("year", "country_iso3"))
data_unit_analysis$units_sold[is.na(data_unit_analysis$units_sold)] <- 0

# Based on Presence
data_presence <- filter(data_presence, data_presence$country_iso3 %in% unique(data_oecd$country_iso3),
                        data_presence$year >= min(data_oecd$year))
data_presence_analysis <- right_join(data_oecd, data_presence)
# Remove unused data
rm(data_abroad, data_ger, data_presence, data_oecd)

# Units Analysis
units_model <- lm(Migration_Value ~ units_sold + log(GDP_USD_CAP) + EU_Member + Germany_GDP_USD_CAP 
                  + log(Population_Value_Mil) + Migrant_Employment_Rate + GI_Present + Language_Taught, 
                  data = data_unit_analysis)

units_model_summary <- summary(units_model)
print(units_model_summary)


# # Graph function
#
# ggplot(data = tmp_city_analysis) +
#   geom_point(aes(x = year, y = units_sold, color = country))








