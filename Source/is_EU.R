##### is_EU #####
# determines if a country year combination is in the EU

is_EU <- function(country_iso3, year){
  if(!exists("EU_Countries")){
    if(exists("DataDir") && exists("WorkDir")){
      setwd(DataDir)
      if(Reduce("|", str_detect(list.files(pattern = ".csv$"), "EU_Countries.csv"))){
        EU_Countries <- read_csv("EU_Countries.csv")
        setwd(WorkDir)
      } else {
        stop("EU_Countries.csv missing")
      }
    } else {
      stop("EU_Countries missing and data or work directory unknown")
    }
  }
  
  countries <- filter(EU_Countries, country_iso3 == !!country_iso3)

  output <- FALSE
  if(length(countries$country_iso3) > 0){
    acceptance_year <- as.numeric(strsplit(countries$year, "\\.")[[1]][3])
    
    if(acceptance_year <= year){
      output <- TRUE
    }
  }
  
  return(output)
}
