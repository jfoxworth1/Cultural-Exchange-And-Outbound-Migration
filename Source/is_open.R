##### is_open #####
# This is a function to tell if an entry in data_oecd had an open or closed GI during the specified year

is_open <- function(country_iso3, year){
  if(!exists("data_presence")){
    if(exists("DataDir") && exists("WorkDir")){
      setwd(DataDir)
      if(str_detect(list.files(pattern = ".csv$"), "data_presence.csv")){
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
  output <- FALSE
  for (i in seq_along(presence$city)) {
    if(output){
      break()
    }
    
    if(presence$open1[i] <= year){
      if(is.na(presence$close1[i])){
        output <- TRUE
      } else if(presence$close1[i] > year){
        output <- TRUE
      } else if(presence$close1[i] <= year && is.na(presence$open2[i])){
        next()
      } else if (presence$open2[i] <= year){
        if(is.na(presence$close2[i])){
          output <- TRUE
        } else if(presence$close2[i] > year){
          output <- TRUE
        } else if(presence$close2[i] <= year && is.na(presence$open3[i])){
          next()
        } else if(presence$open3[i] <= year){
          if(is.na(presence$close3[i]) || presence$close3[i] > year){
            output <- TRUE
          }
        }
      }
    }
  }

return(output)
}
