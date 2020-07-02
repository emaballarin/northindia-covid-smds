library(dplyr)

setwd("/home/emaballarin/DSSC/statmeth/EXAM/GATHER/")

load("d_s_data.Rda")
load("state_info.Rda")

final_dataframe <- dplyr::full_join(d_s_data, state_info)

final_dataframe <- final_dataframe[final_dataframe$state != "UttarPradesh" | final_dataframe$date != 5 | final_dataframe$deltaswabs != 0,]

final_dataframe <- final_dataframe[order(final_dataframe$date, final_dataframe$state),]
final_dataframe <- final_dataframe[final_dataframe$state != "Ladakh",]

final_dataframe <- final_dataframe[-6]    # remove deltaneg

save(final_dataframe,file="final_dataframe.Rda")
