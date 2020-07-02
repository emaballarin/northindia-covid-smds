library(MASS)
library(dplyr)
library(ggplot2)

################################################################################
setwd("/home/emaballarin/DSSC/statmeth/EXAM/ANALYSIS/")
################################################################################
load("final_dataframe.Rda")
load("lagged.Rda")

final_dataframe <- lagged_data
final_dataframe <- final_dataframe %>% relocate("pos", .after = last_col())
################################################################################


size <-nrow(final_dataframe)

ttsr <- 0.86
tsen <- round(size*ttsr)


train <- final_dataframe[1:round(size*ttsr),]
test <- final_dataframe[(tsen+1):size,]


################################################################################

qpglm <- glm(formula = deltapos ~
                 date +
                 urbanh +
                 ruralh +
                 Urbanization +
                 Density +
                 Females +
                 Density:Urbanization  +
                 Males:Urbanization +
                 lagged_deltapos +
                 Males
                ,
                data = train,
                family = quasipoisson(link = log)
                )

################################################################################
states <- unique(test$state)
times  <- unique(test$date)

predict_df <- as.data.frame()

#(qpglm)

# for (state_idx in 1:length(states))
# {
#
#     state_df <- test[test$state == states[state_idx],]
#     state_dates <- state_df$date
#     state_true  <- state_df$deltapos
#
#     state_preds <- predict.glm(qpglm, type = "response", newdata = state_df)
#
#     state_preds_aug <- array(NA, length(times))
#
#     i <- 1
#     for (j in 1:length(state_dates))
#     {
#         for (i in i:length(times))
#         {
#             if (state_dates[j] == times[i])
#             {
#                 state_preds_aug[i] <- state_preds[j]
#                 lwb <- i
#             }
#         }
#     }
#
#
# }


#p <- predict.glm(qpglm,
#                 type = "response",
#                 newdata=test
#                 )

################################################################################

#trueval <- as.numeric(test$deltapos)
#predval <- as.numeric(p)

# <- function(true, pred)
#{
#    mean(abs(true - pred)/true)
#}

################################################################################

#mreloss(trueval, predval)

#summary(qpglm)

#plot(qpglm)

#final_dataframe %>%
#    ggplot(aes(x = unique(test$date), y = pos, color = test)) + geom_point() +
#    labs(title="Positive cases vs Days", subtitle = "COVID 19 North India")+
#    xlab("Days") + ylab("Number of positive cases")
