##### teaches_language #####
# tells if a country year combo offers language courses

teaches_language <- function(country_iso3, year){
  if(!exists("data_presence")){
    if(exists("DataDir") && exists("WorkDir")){
      setwd(DataDir)
      if(Reduce("|", str_detect(list.files(pattern = ".csv$"), "data_presence.csv"))){
        data_presence <- read_csv("data_presence.csv")
        setwd(WorkDir)
      } else {
        stop("data_presence.csv missing")
      }
    } else {
      stop("data_presence missing and data or work directory unknown")
    }
  }
  
  presence <- filter(data_presence, data_presence$country_iso3 == !!country_iso3,
                     data_presence$year == !!year)
  return(Reduce("|", c(1,2) %in% presence$language))
}
