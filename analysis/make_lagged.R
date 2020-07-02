library(MASS)
library(dplyr)
library(tidyr)
################################################################################
setwd("/home/emaballarin/DSSC/statmeth/EXAM/ANALYSIS/")
################################################################################
load("final_dataframe.Rda")
final_dataframe <- final_dataframe %>% relocate("pos", .after = last_col())
################################################################################

# Introduce lagged componens programatically

lagmake <- function(dataframe, date_column, var_column, lag_len, current_date)
{
    if ((sum(dataframe[,date_column] == current_date)))
    {
        alldates <- unique(dataframe[,date_column])
        startidx <- which(alldates == current_date)
        cursor_idx <- startidx

        while(TRUE)
        {
            cursor_idx <- (cursor_idx - 1)

            if (cursor_idx < 1)
            {
                return(NA)
            }
            else
            {
                cursor <- alldates[cursor_idx]
                if (current_date == cursor + lag_len)
                {
                    return(as.numeric(dataframe[,var_column][cursor_idx]))
                }
                else if (cursor + lag_len < current_date)
                {
                    return( ((as.numeric(dataframe[,var_column][cursor_idx+1]) - as.numeric(dataframe[,var_column][cursor_idx])) / (alldates[cursor_idx+1] - cursor)) * (current_date - lag_len - cursor) + as.numeric(dataframe[,var_column][cursor_idx]))
                };
            };
        }
    }
    else
        {
            print("Selected a non-existent current date!")
        };
}

################################################################################

#final_dataframe$deltaswabs_1d <- array(-1, nrow(final_dataframe))
#final_dataframe$deltaswabs_2d <- array(-1, nrow(final_dataframe))
final_dataframe$deltaswabs_3d <- array(-1, nrow(final_dataframe))
final_dataframe$deltaswabs_4d <- array(-1, nrow(final_dataframe))
#final_dataframe$deltaswabs_5d <- array(-1, nrow(final_dataframe))
final_dataframe$lagged_deltapos <- array(-1, nrow(final_dataframe))
final_dataframe$lagged_avgage <- array(-1, nrow(final_dataframe))

final_dataframe$deltaswabs_34 <- array(-1, nrow(final_dataframe))

curstates <- unique(final_dataframe$state)

for (curstate in 1:length(curstates))
{
    data_for_state <- final_dataframe[final_dataframe$state == curstates[curstate],]

    ###########################################################################
    for (idx in 1:nrow(data_for_state))
    {
        buffer_pos <- lagmake(dataframe = data_for_state, date_column = "date", var_column = "deltapos", lag_len = 10, current_date = data_for_state$date[idx])
        buffer_3 <- lagmake(dataframe = data_for_state, date_column = "date", var_column = "deltaswabs", lag_len = 3, current_date = data_for_state$date[idx])
        buffer_4 <- lagmake(dataframe = data_for_state, date_column = "date", var_column = "deltaswabs", lag_len = 4, current_date = data_for_state$date[idx])
        buffer_age <- lagmake(dataframe = data_for_state, date_column = "date", var_column = "avg_age", lag_len = 10, current_date = data_for_state$date[idx])
        buffer_34 <- mean(c(buffer_3, buffer_4))
        final_dataframe[(final_dataframe$state == curstates[curstate] & final_dataframe$date == data_for_state$date[idx]),]$deltaswabs_3d <- buffer_3
        final_dataframe[(final_dataframe$state == curstates[curstate] & final_dataframe$date == data_for_state$date[idx]),]$deltaswabs_4d <- buffer_4
        final_dataframe[(final_dataframe$state == curstates[curstate] & final_dataframe$date == data_for_state$date[idx]),]$deltaswabs_34 <- buffer_34
        final_dataframe[(final_dataframe$state == curstates[curstate] & final_dataframe$date == data_for_state$date[idx]),]$lagged_deltapos <- buffer_pos
        final_dataframe[(final_dataframe$state == curstates[curstate] & final_dataframe$date == data_for_state$date[idx]),]$lagged_avgage <- buffer_age
    }
}

################################################################################

final_dataframe <- tidyr::drop_na(final_dataframe)
lagged_data     <- final_dataframe

################################################################################
remove(data_for_state)
################################################################################

save(lagged_data,file="lagged.Rda")
