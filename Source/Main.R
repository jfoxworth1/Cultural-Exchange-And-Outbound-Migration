##### list important directories ####
MainDir <- "/home/me/Documents/Uni-Jena/Thesis/Cultural-Exchange-And-Outbound-Migration/"
WorkDir <- paste0(MainDir, "Source/")
DataDir <- paste0(MainDir, "Data/")

#### List Dependencies ####
require(tidyverse)
require(tseries)
require(maps)
source(paste0(WorkDir, "is_open.R"))
source(paste0(WorkDir, "teaches_language.R"))
source(paste0(WorkDir, "is_EU.R"))
source(paste0(WorkDir, "to_country_code.R"))

##### download data into memory ####
setwd(DataDir)
files <- list.files(pattern = ".csv$")
for(i in seq_along(files)){
  object_name <- str_remove(files[i], ".csv")
  tmp <- read_csv(files[i])
  names(tmp) <- tolower(colnames(tmp))
  assign(object_name, tmp)
}
rm(files, tmp, object_name, i)
setwd(WorkDir)

#### Organizing and unifying data ####
# Country Code Table
country_codes <- unique(select(Migration_Flow_DEMIG_Net, `country codes -un based-`,  countries))
names(country_codes) <- c("country_iso3", "country")

# Combine Data about OECD countries and trim out unn ecesary parts
Migration_Flow_DEMIG_Net <- filter(Migration_Flow_DEMIG_Net, 
                                   `country codes -un based-` == "DEU",
                                   criterion == "COC",
                                   `collection method` == "Population Register",
                                   `coverage -citizens/foreigners/both-` != "Citizen",
                                   gender == "Total"
)
country_iso3 <- vapply(Migration_Flow_DEMIG_Net$`reporting country`, to_country_code, character(1)) %>% unname()
Migration_Flow_DEMIG_Net <- cbind(country_iso3, Migration_Flow_DEMIG_Net)
Migration_Flow_DEMIG_Net <- filter(Migration_Flow_DEMIG_Net, country_iso3 != "NA")
names(Migration_Flow_DEMIG_Net)[2] <- "country"
Migration_Flow_DEMIG_Net <- select(Migration_Flow_DEMIG_Net, country, country_iso3, year, value)
rm(country_iso3)

#OECD_GDP filtering
names(OECD_GDP)[1] <- "country_iso3"
names(OECD_GDP)[6] <- "year" 
names(OECD_GDP)[7] <- "GDP_USD_CAP"
OECD_GDP <- filter(OECD_GDP, measure == "USD_CAP", subject =="TOT", frequency == "A")
OECD_GDP <- select(OECD_GDP, country_iso3, year, GDP_USD_CAP)

# German GDP Figure
Germany_GDP <- filter(OECD_GDP, country_iso3 == "DEU")
names(Germany_GDP)[3] <- "Germany_GDP_USD_CAP"
Germany_GDP <- select(Germany_GDP, -country_iso3)

# # OECD Employment rates
# names(OECD_Employment)[1] <- "country_iso3"
# names(OECD_Employment)[6] <- "year"
# names(OECD_Employment)[7] <- "Employment_Rate"
# OECD_Employment <- select(OECD_Employment, country_iso3, year, Employment_Rate)

#OECD Unemployment Rate
names(OECD_Unemployment)[1] <- "country_iso3"
names(OECD_Unemployment)[6] <- "year"
names(OECD_Unemployment)[7] <- "Unemployment_Rate"
OECD_Unemployment <- filter(OECD_Unemployment, subject == "TOT", frequency == "A", measure == "PC_LF")
OECD_Unemployment <- select(OECD_Unemployment, country_iso3, year, Unemployment_Rate)

# Population numbers measured in millions
names(OECD_Population)[1] <- "country_iso3"
names(OECD_Population)[6] <- "year"
names(OECD_Population)[7] <- "Population_Value_Mil"
OECD_Population <- filter(OECD_Population, measure == "MLN_PER", subject =="TOT", frequency == "A")
OECD_Population <- select(OECD_Population, country_iso3, year, Population_Value_Mil)

# OECD Migration Data
names(data_oecd)[7] <- "country_iso3"
names(data_oecd)[11] <- "Migration_Value"
data_oecd <- filter(data_oecd, data_oecd$`country of birth/nationality` == "Germany", 
                    data_oecd$variable == "Inflows of foreign population by nationality",
                    year <= max(data_abroad$year))
data_oecd <- select(data_oecd, -co2, -'country of birth/nationality', -var, -variable, -gen, -gender,-yea, -'flag codes', -flags)

# Combination country and year
# needed to combine with other variables
start_year <- 1965
end_year <- 2018
years <- rep(start_year:end_year, length(country_codes$country_iso3))
country_codes <- lapply(country_codes$country_iso3,
                        function(country, start_year, end_year)return(rep(country, length(start_year:end_year))),
                        start_year = start_year, end_year = end_year)
data_control <- tibble(country_iso3 = unlist(country_codes), year = years)
rm(start_year, end_year, years, country_codes)

# EU_Member dummy variable
#uses Wordkir/is_EU.R
EU_dummy <- mapply(is_EU, data_control$country_iso3, data_control$year) %>%
  as.numeric()
data_control <- cbind(data_control, EU_dummy)

# GI present dummy variable
# uses WorkDir/is_open.R
GI_Present <- mapply(is_open, data_control$country_iso3, data_control$year) %>%
  as.numeric()
data_control <- cbind(data_control, GI_Present) 

# Offer Language Learning Dummy
# uses WorkDir/teaches_language.R
Language_Taught <- mapply(teaches_language, data_control$country_iso3, data_control$year) %>%
  as.numeric()
data_control <- cbind(data_control,Language_Taught)
# Remove dummies
rm(EU_dummy, GI_Present, Language_Taught)

# Joining all control data
data_control <- right_join(OECD_GDP, data_control, by = c("country_iso3", "year"))
data_control <- right_join(OECD_Unemployment, data_control, by = c("country_iso3", "year"))
data_control <- right_join(Germany_GDP, data_control, by = "year")
data_control <- right_join(OECD_Population, data_control, by = c("country_iso3", "year"))

####################################################################################################################################
### Analysis ###
####################################################################################################################################
###### Create tables for the different analysis ######
### Based on Units ###
# Using OECD Data
data_abroad <- filter(data_abroad, data_abroad$country_iso3 %in% unique(data_oecd$country_iso3),
                      data_abroad$year >= min(data_oecd$year))
data_oecd_units <- right_join(data_abroad, data_oecd, by = c("year", "country_iso3"))
data_oecd_units <- right_join(data_control, data_oecd_units, by = c("year", "country_iso3"))
data_oecd_units$units_sold[is.na(data_oecd_units$units_sold)] <- 0
data_oecd_units$country_iso3 <- as_factor(data_oecd_units$country_iso3)

# Using DEMIR Data
  


# Based on Presence
# data_presence <- filter(data_presence, data_presence$country_iso3 %in% unique(data_oecd$country_iso3),
#                         data_presence$year >= min(data_oecd$year))
# data_presence_analysis <- right_join(data_oecd, data_presence)


# QUESTION -> not sure if I should use the log of migration values or not since they are values and not a rate
data_oecd_units <- data_oecd_units %>%
  group_by(year) %>%
  mutate(wmean_migration = weighted.mean(Migration_Value, Population_Value_Mil))
data_oecd_units <- data_oecd_units %>%
  group_by(year) %>%
  mutate(wmean_units_sold = weighted.mean(units_sold, Population_Value_Mil))
# Units Analysis

Units_model_fixed_effects <- lm(Migration_Value ~ units_sold + factor(country_iso3) + factor(year) +
                                  # controls
                                  log(GDP_USD_CAP) + EU_Member + Germany_GDP_USD_CAP + log(Population_Value_Mil) + Unemployment_Rate + 
                                  GI_Present,
                                data = data_oecd_units)  


units_model_CCE <- lm(Migration_Value ~ units_sold + factor(country_iso3) + factor(year) + factor(country_iso3):wmean_migration +
                        factor(country_iso3):wmean_units_sold +                    
                        # controls
                        log(GDP_USD_CAP) + EU_Member + Germany_GDP_USD_CAP + log(Population_Value_Mil) + Unemployment_Rate + 
                        GI_Present,
                      data = data_oecd_units, weights = Population_Value_Mil)  





units_model_summary <- summary(units_model)
print(units_model_summary)
#rm(units_model, units_model_summary, data_presence_analysis, data_oecd_units)


# # Graph function
#
# ggplot(data = tmp_city_analysis) +
#   geom_point(aes(x = year, y = units_sold, color = country))

ggplot(data = filter(data_oecd_units, year >= 1997)) +
  ggtitle("Units Sold 1997-2014") +
  geom_boxplot(aes(y = units_sold/1000, x = year, group = year)) +
  ylab("Units Sold (thousands)")

# ggplot(data = filter(data_oecd_units, year >= 1997)) +
#   ggtitle("Distribution of Units Sold by Country 1997-2014") +
#   geom_point(aes(x = year, y = units_sold, color = country_iso3)) +
#   guides(col = guide_legend(ncol = 12)) +
#   theme(legend.position = "bottom", legend.title=element_blank(), axis.title.x = "Units Sold", axis.title.y = "Year")

write_csv(data_oecd_units, paste0(WorkDir, "data_oecd_units.csv"))

open_years <- c(data_presence$open1, data_presence$open2, data_presence$open3) %>%
  as.numeric()
open_years <- open_years[open_years >= 1980]
ggplot(data = as.tibble(open_years)) +
  geom_histogram(aes(x = value), stat = "count") +
  labs(title = "Goethe Institutes Opened", subtitle = "1980 - 2014", x = "Year", y = "Number Opened") +
  scale_y_continuous(breaks = seq(0, 600, 50)) +
  scale_x_continuous(breaks = seq(1980, 2015, 5))

WorldData <- map_data('world')
WorldData %>% filter(region != "Antarctica") -> WorldData
WorldData <- fortify(WorldData)
df <- select(data_oecd_units, country, Migration_value)

ggplot() + 
  geom_map(data=WorldData, map=WorldData,
           aes(x=long, y=lat, group=group, map_id=region),
           fill="white", colour="#7f7f7f", size=0.5) +
  geom_map(data=df, map=WorldData,
           aes(fill=value, map_id=region),
           colour="#7f7f7f", size=0.5) +
  scale_y_continuous(breaks=c()) +
  scale_x_continuous(breaks=c()) +
  labs(fill="legend", title="Title", x="", y="") +
  theme_bw() +
  theme(panel.border = element_blank())

