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

loessloess <- loess(

    formula = deltapos ~
        date +
        urbanh +
        ruralh +
        Urbanization +
        Density +
        Females +
        Density:Urbanization  +
        Males:Urbanization +
        lagged_deltapos +
        Males,

    data = train,
    #degree = 3

)

XYZ <- predict(object = loessloess,
               newdata = test,
               type = "response")

################################################################################

trueval <- as.numeric(test$deltapos)
predval <- as.numeric(XYZ)

mreloss <- function(true, pred)
{
    mean(abs(true - pred)/true)
}

################################################################################

mreloss(trueval, predval)
