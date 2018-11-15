##### to_country_code #####
# Converts country name to country code

to_country_code <- function(country_name){
  if(!exists("country_codes")){
    if(!exists("Migration_Flow_DEMIG_Inflow")){
      if(exists("DataDir") && exists("WorkDir")){
        setwd(DataDir)
        if(Reduce("|", str_detect(list.files(pattern = ".csv$"), "Migration_Flow_DEMIG_Inflow.csv"))){
          Migration_Flow_DEMIG_Inflow <- read_csv("Migration_Flow_DEMIG_Inflow.csv")
          country_codes <- unique(select(Migration_Flow_DEMIG_Inflow, `country codes -un based-`,  countries))
          rm(Migration_Flow_DEMIG_Inflow)
          setwd(WorkDir)
        } else {
          stop("Migration_Flow_DEMIG_Inflow.csv missing, can' generate country_codes")
        }
      } else {
        stop("Migration_Flow_DEMIG_Inflow.csv missing and data or work directory unknown, can' generate country_codes")
      }
    } else {
      country_codes <- unique(select(Migration_Flow_DEMIG_Inflow, `country codes -un based-`,  countries))
    }
  }
  
  output <- country_codes$`country codes -un based-`[country_codes$countries == country_name]
  if(length(output) == 0){
    output <- 'NA'
  }
  return(output)
}
