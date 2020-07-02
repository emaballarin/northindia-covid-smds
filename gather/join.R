library(dplyr)
library(lubridate)

setwd("/home/emaballarin/DSSC/statmeth/EXAM/GATHER/")
load("individual.Rda")
load("statewise.Rda")



# Thanks Ben Rollert
individual$date <- as.Date(parse_date_time(individual$date,"dmy"))

date_state_info <- dplyr::full_join(individual, statewise)

save(date_state_info, file="date_state_info.Rda")
