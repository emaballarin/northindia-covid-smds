library(MASS)
library(dplyr)
library(ggplot2)
library(earth)

################################################################################
#setwd("/home/emaballarin/DSSC/statmeth/EXAM/ANALYSIS/")
################################################################################
load("final_dataframe.Rda")
load("lagged.Rda")

final_dataframe <- lagged_data
#final_dataframe <- final_dataframe %>% relocate("pos", .after = last_col())
################################################################################

size <-nrow(final_dataframe)

ttsr <- 0.86
tsen <- round(size*ttsr)


train <- final_dataframe[1:round(size*ttsr),]
test <- final_dataframe[(tsen+1):size,]

################################################################################

marsmars <- earth::earth(

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
    degree = 3,
    varmod.method="earth"
)

# XYZ <- predict(object = marsmars,
#                newdata = test,
#                type = "response",
#                interval = "pint",
#                level=.95)

# test$fit <- XYZ$fit
# test$lwr <- XYZ$lwr
# test$upr <- XYZ$upr


################################################################################

trueval <- as.numeric(test$deltapos)
predval <- as.numeric(XYZ$fit)

mreloss <- function(true, pred)
{
    mean(abs(true - pred)/true)
}

################################################################################

mreloss(trueval, predval)

################################################################################


# north_regions <- unique(test$state)
# myplots <- list()  # new empty list
# 
# for (state in 1:length(north_regions)) {
#     
#     statewise <- test[test$state==north_regions[state],]
#     print(nrow(statewise))
#     myplots[[state]] <- ggplot(statewise, aes(x = date, y = deltapos)) +
#         geom_point(size = 2) + 
#         geom_line(aes(y = fit), size = 2, color = "maroon") +
#         geom_ribbon(aes(ymin = lwr, ymax = upr), fill =
#                         "royalblue1", alpha = 0.3) +
#         labs(title=north_regions[state]) +
#         xlab("Days") + ylab("Daily positives")
# }
# 
# do.call(grid.arrange, c(myplots, ncol = 3, nrow=3,   top = "MARS"
# ))




