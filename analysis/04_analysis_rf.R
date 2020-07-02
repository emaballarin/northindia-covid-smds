library(MASS)
library(dplyr)
library(ggplot2)
library(randomForest)
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

rfrf <- randomForest(
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
    data = train,

    ntree=850,
    mtry=5
)

################################################################################

XYZ <- predict(object = rfrf,
               newdata = test,
               type = "response")
north_regions <- unique(test$state)
################################################################################

trueval <- as.numeric(test$deltapos)
predval <- as.numeric(XYZ)

mreloss <- function(true, pred)
{
    mean(abs(true - pred)/true)
}

################################################################################

#mreloss(trueval, predval)

################################################################################

run_upb <- 100

for (state in 1:length(north_regions))
{

    statewise_rf_looped <- final_dataframe[final_dataframe$state==north_regions[state],]

    statepreds_rf <- array(NA, nrow(statewise_rf_looped$date))
    for (run_nr in 1:run_upb)
    {
        predout <- predict(object = rfrf,
                           newdata = test,
                           type = "response")

        statepreds_rf <- rbind(predout)
    }

    statepreds_rf_df <- as_data_frame(statepreds_rf)
    statepreds_rf_df <- tidyr::drop_na(statepreds_rf_df)

    #
    med_pred <- apply(statepreds_rf_df, FUN = mean, 2)    # Median of RFs

    #


}








final_dataframe$pred_mars <- graph_pred_mars$fit
final_dataframe$mars_lwr <- graph_pred_mars$lwr
final_dataframe$mars_upr <- graph_pred_mars$upr

final_dataframe$mars_lwr[final_dataframe$mars_lwr < 0] <- 0  # Respect physics

myplots_mars <- list()  # new empty list





for (state in 1:length(north_regions)) {

    statewise_mars <- final_dataframe[final_dataframe$state==north_regions[state],]

    myplots_mars[[state]] <- ggplot(statewise_mars, aes(x = date, y = deltapos)) +
        geom_point(size = 1) +
        geom_line(aes(y = pred_mars), size = 1, color = "maroon") +
        geom_ribbon(aes(ymin = mars_lwr, ymax = mars_upr), fill =
                        "royalblue1", alpha = 0.3) +
        labs(title=north_regions[state]) +
        xlab("Days") + ylab("Daily positives")
}

do.call(grid.arrange, c(myplots_mars, ncol = 3, nrow=3,   top = "MARS: predictions and P.I.s"
))












































































for (run_nr in 1:run_upb)
{
    predout <- predict(object = rfrf,
                   newdata = test,
                   type = "response")

}



################################################################################
