library(MASS)
library(dplyr)
library(ggplot2)
library(randomForest)
library(ranger)
library(rfinterval)


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

################################################################################

train <- final_dataframe[1:round(size*ttsr),]
test <- final_dataframe[(tsen+1):size,]

################################################################################

rfrf_ci <- rfinterval(
    deltapos ~
        date +
        urbanh +
        ruralh +
        Urbanization +
        Density +
        Females +
        #Density:Urbanization  +
        #Males:Urbanization +
        lagged_deltapos +
        deltaswabs_3d +
        deltaswabs_4d +
        #deltaswabs_34 +
        #lagged_avgage +
        Males,
    train_data = train,
    test_data = lagged_data,
    method = c("oob"),
    alpha = 0.05,
    symmetry = FALSE,
    params_ranger = c(num.trees = 850, mtry=5)
)

################################################################################
north_regions <- unique(test$state)
################################################################################

trueval <- as.numeric(test$deltapos)
predval <- as.numeric(rfrf_ci$testPred)

mreloss <- function(true, pred)
{
    mean(abs(true - pred)/true)
}


################################################################################

#print(mreloss(trueval, predval))

# ##############################################################################


final_dataframe$pred_rf <- rfrf_ci$testPred
final_dataframe$rf_lwr <- rfrf_ci$oob_interval$lower
final_dataframe$rf_upr <- rfrf_ci$oob_interval$upper

final_dataframe$rf_lwr[final_dataframe$rf_lwr < 0] <- 0  # Respect physics




myplots_rf_ci <- list()  # new empty list

for (state in 1:length(north_regions)) {

    statewise_rf_ci <- final_dataframe[final_dataframe$state==north_regions[state],]
    myplots_rf_ci[[state]] <- ggplot(statewise_rf_ci, aes(x = date, y = deltapos)) +
        geom_point(size = 1) +
        geom_line(aes(y = pred_rf), size = 1, color = "maroon") +
        geom_ribbon(aes(ymin = rf_lwr, ymax = rf_upr), fill =
                        "royalblue1", alpha = 0.3) +
        labs(title=north_regions[state]) +
        xlab("Days") + ylab("Daily positives")
}

do.call(grid.arrange, c(myplots_rf_ci, ncol = 3, nrow=3,   top = "MARS"
))
