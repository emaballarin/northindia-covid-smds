library(MASS)
library(dplyr)
library(ggplot2)
library(earth)
library(ciTools)
library(gridExtra)
library(ggplot2)
library(ranger)
library(rfinterval)
library(gt)

################################################################################

setwd("/home/emaballarin/DSSC/statmeth/EXAM/ANALYSIS/")

################################################################################

#load("final_dataframe.Rda")
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

polydeg <- 3

################################################################################
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
    varmod.method="earth", nfold=10, ncross=30
)

################################################################################

rfrf_ev <- ranger(
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
    num.trees = 850,
    mtry=5
)

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

glm_qpois  <- predict.glm(qpglm, type = "response", newdata=test)
mars_reg   <- predict(marsmars, type = "response", newdata=test)
rf_reg_ev  <- as.numeric(predict(rfrf_ev, type = "response", data=test)$predictions)
#rf_reg_ci <- as.numeric(rfrf_ci$testPred)

################################################################################

trueval <- as.numeric(test$deltapos)

mreloss <- function(true, pred)
{
    mean(abs(true - pred)/true)
}

################################################################################

test_acc_qpois  <- mreloss(trueval, glm_qpois)
test_acc_mars   <- mreloss(trueval, mars_reg)
test_acc_rf_ev  <- mreloss(trueval, rf_reg_ev)
#test_acc_rf_ci  <- mreloss(trueval, rf_reg_ci)

################################################################################
################################################################################

print(test_acc_qpois)
print(test_acc_mars)
print(test_acc_rf_ev)

################################################################################

# C.I.s for all models
north_regions <- unique(test$state)

################################################################################

# P.I. for GLM

final_dataframe <- ciTools::add_pi(tb = final_dataframe,
                                   qpglm, names=c("qp_lwr", "qp_upr")
)
final_dataframe <- rename(final_dataframe, pred_qp = pred)
myplots_qp <- list()  # new empty list

for (state in 1:length(north_regions)) {

    statewise_qp <- final_dataframe[final_dataframe$state==north_regions[state],]
    myplots_qp[[state]] <- ggplot(statewise_qp, aes(x = date, y = deltapos)) +
        theme(plot.title = element_text(hjust = 0.5))+
        geom_point(size = 1) +
        geom_line(aes(y = pred_qp), size = 1, color = "maroon") +
        geom_ribbon(aes(ymin = qp_lwr, ymax = qp_upr), fill =
                        "royalblue1", alpha = 0.3) +
        labs(title=north_regions[state]) +
        xlab("Days") + ylab("Daily positives")
}

g<-do.call(grid.arrange, c(myplots_qp, ncol = 3, nrow=3,   top = "Quasi-Poisson: predictions and P.I.s"
))
ggsave("PLOTS/qp_ppi.png", plot=g, dpi = 300, width = 20, height = 20, units = "cm")


################################################################################

# P.I. for MARS

graph_pred_mars <- predict(object = marsmars,
                           newdata = final_dataframe,
                           type = "response",
                           interval = "pint",
                           level=.95)

final_dataframe$pred_mars <- graph_pred_mars$fit
final_dataframe$mars_lwr <- graph_pred_mars$lwr
final_dataframe$mars_upr <- graph_pred_mars$upr

final_dataframe$mars_lwr[final_dataframe$mars_lwr < 0] <- 0  # Respect physics

myplots_mars <- list()  # new empty list

for (state in 1:length(north_regions)) {

    statewise_mars <- final_dataframe[final_dataframe$state==north_regions[state],]

    myplots_mars[[state]] <- ggplot(statewise_mars, aes(x = date, y = deltapos)) +
        theme(plot.title = element_text(hjust = 0.5))+
        geom_point(size = 1) +
        geom_line(aes(y = pred_mars), size = 1, color = "maroon") +
        geom_ribbon(aes(ymin = mars_lwr, ymax = mars_upr), fill =
                        "royalblue1", alpha = 0.3) +
        labs(title=north_regions[state]) +
        xlab("Days") + ylab("Daily positives")
}


g<-do.call(grid.arrange, c(myplots_mars, ncol = 3, nrow=3,   top = "MARS: predictions and P.I.s"
))
ggsave("PLOTS/mars_ppi.png", plot=g, dpi = 300, width = 20, height = 20, units = "cm")

################################################################################

# Pseudo-P.I. for RF

final_dataframe$pred_rf_ci <- rfrf_ci$testPred
final_dataframe$rf_lwr_ci <- rfrf_ci$oob_interval$lower
final_dataframe$rf_upr_ci <- rfrf_ci$oob_interval$upper

final_dataframe$rf_lwr_ci[final_dataframe$rf_lwr_ci < 0] <- 0  # Respect physics


myplots_rf_ci <- list()  # new empty list

for (state in 1:length(north_regions)) {

    statewise_rf_ci <- final_dataframe[final_dataframe$state==north_regions[state],]
    myplots_rf_ci[[state]] <- ggplot(statewise_rf_ci, aes(x = date, y = deltapos)) +
        theme(plot.title = element_text(hjust = 0.5))+
        geom_point(size = 1) +
        geom_line(aes(y = pred_rf_ci), size = 1, color = "maroon") +
        geom_ribbon(aes(ymin = rf_lwr_ci, ymax = rf_upr_ci), fill =
                        "royalblue1", alpha = 0.3) +
        labs(title=north_regions[state]) +
        xlab("Days") + ylab("Daily positives")
}

g<-do.call(grid.arrange, c(myplots_rf_ci, ncol = 3, nrow=3,   top = "Random Forest: predictions and P.I.s"
))
ggsave("PLOTS/rf_ppi.png", plot=g, dpi = 300, width = 20, height = 20, units = "cm")


#########################################################################################################

test_chan = subset(test, state == "Chandigarh")
test_del = subset(test, state == "Delhi")
test_har = subset(test, state == "Haryana")
test_him = subset(test, state == "HimachalPradesh")
test_jamm = subset(test, state == "JammuKashmir")
test_pun = subset(test, state == "Punjab")
test_raj= subset(test, state == "Rajasthan")
test_uttk = subset(test, state == "Uttarakhand")
test_uttp = subset(test, state == "UttarPradesh")

rmse <- function(true, pred)
{
    format(round(sqrt((sum((true - pred)^2))/length(true)),2), nsmall = 2)
}


state_preds_chan_qp <- predict.glm(qpglm, type = "response", newdata = test_chan)
test_chan$deltapos_pred_qp <- state_preds_chan_qp
state_preds_del_qp <- predict.glm(qpglm, type = "response", newdata = test_del)
test_del$deltapos_pred_qp <- state_preds_del_qp
state_preds_har_qp <- predict.glm(qpglm, type = "response", newdata = test_har)
test_har$deltapos_pred_qp <- state_preds_har_qp
state_preds_him_qp <- predict.glm(qpglm, type = "response", newdata = test_him)
test_him$deltapos_pred_qp <- state_preds_him_qp
state_preds_jamm_qp <- predict.glm(qpglm, type = "response", newdata = test_jamm)
test_jamm$deltapos_pred_qp <- state_preds_jamm_qp
state_preds_pun_qp <- predict.glm(qpglm, type = "response", newdata = test_pun)
test_pun$deltapos_pred_qp <- state_preds_pun_qp
state_preds_raj_qp <- predict.glm(qpglm, type = "response", newdata = test_raj)
test_raj$deltapos_pred_qp <- state_preds_raj_qp
state_preds_uttk_qp <- predict.glm(qpglm, type = "response", newdata = test_uttk)
test_uttk$deltapos_pred_qp <- state_preds_uttk_qp
state_preds_uttp_qp <- predict.glm(qpglm, type = "response", newdata = test_uttp)
test_uttp$deltapos_pred_qp <- state_preds_uttp_qp

err_chan_qp <- rmse(test_chan$deltapos, test_chan$deltapos_pred_qp)
err_del_qp <- rmse(test_del$deltapos, test_del$deltapos_pred_qp)
err_har_qp <- rmse(test_har$deltapos, test_har$deltapos_pred_qp)
err_him_qp <- rmse(test_him$deltapos, test_him$deltapos_pred_qp)
err_jamm_qp <- rmse(test_jamm$deltapos, test_jamm$deltapos_pred_qp)
err_pun_qp <- rmse(test_pun$deltapos, test_pun$deltapos_pred_qp)
err_raj_qp <- rmse(test_raj$deltapos, test_raj$deltapos_pred_qp)
err_uttk_qp <- rmse(test_uttk$deltapos, test_uttk$deltapos_pred_qp)
err_uttp_qp <- rmse(test_uttp$deltapos, test_uttp$deltapos_pred_qp)


RMSE_QP <- c(err_chan_qp, err_del_qp, err_har_qp, err_him_qp, err_jamm_qp, err_pun_qp, err_raj_qp, err_uttk_qp, err_uttp_qp)


state_preds_chan_mars <- predict(marsmars, type = "response", newdata = test_chan)
test_chan$deltapos_pred_mars <- state_preds_chan_mars
state_preds_del_mars <- predict(marsmars, type = "response", newdata = test_del)
test_del$deltapos_pred_mars <- state_preds_del_mars
state_preds_har_mars <- predict(marsmars, type = "response", newdata = test_har)
test_har$deltapos_pred_mars <- state_preds_har_mars
state_preds_him_mars <- predict(marsmars, type = "response", newdata = test_him)
test_him$deltapos_pred_mars <- state_preds_him_mars
state_preds_jamm_mars <- predict(marsmars, type = "response", newdata = test_jamm)
test_jamm$deltapos_pred_mars <- state_preds_jamm_mars
state_preds_pun_mars <- predict(marsmars, type = "response", newdata = test_pun)
test_pun$deltapos_pred_mars <- state_preds_pun_mars
state_preds_raj_mars <- predict(marsmars, type = "response", newdata = test_raj)
test_raj$deltapos_pred_mars <- state_preds_raj_mars
state_preds_uttk_mars <- predict(marsmars, type = "response", newdata = test_uttk)
test_uttk$deltapos_pred_mars <- state_preds_uttk_mars
state_preds_uttp_mars <- predict(marsmars, type = "response", newdata = test_uttp)
test_uttp$deltapos_pred_mars <- state_preds_uttp_mars

err_chan_mars <- rmse(test_chan$deltapos, test_chan$deltapos_pred_mars)
err_del_mars <- rmse(test_del$deltapos, test_del$deltapos_pred_mars)
err_har_mars <- rmse(test_har$deltapos, test_har$deltapos_pred_mars)
err_him_mars <- rmse(test_him$deltapos, test_him$deltapos_pred_mars)
err_jamm_mars <- rmse(test_jamm$deltapos, test_jamm$deltapos_pred_mars)
err_pun_mars <- rmse(test_pun$deltapos, test_pun$deltapos_pred_mars)
err_raj_mars <- rmse(test_raj$deltapos, test_raj$deltapos_pred_mars)
err_uttk_mars <- rmse(test_uttk$deltapos, test_uttk$deltapos_pred_mars)
err_uttp_mars <- rmse(test_uttp$deltapos, test_uttp$deltapos_pred_mars)

RMSE_MARS <- c(err_chan_mars, err_del_mars, err_har_mars, err_him_mars, err_jamm_mars, err_pun_mars, err_raj_mars, err_uttk_mars, err_uttp_mars)


state_preds_chan_rf <- predict(rfrf_ev, type = "response", data = test_chan)$predictions
test_chan$deltapos_pred_rf <- state_preds_chan_rf
state_preds_del_rf <- predict(rfrf_ev, type = "response", data = test_del)$predictions
test_del$deltapos_pred_rf <- state_preds_del_rf
state_preds_har_rf <- predict(rfrf_ev, type = "response", data = test_har)$predictions
test_har$deltapos_pred_rf <- state_preds_har_rf
state_preds_him_rf <- predict(rfrf_ev, type = "response", data = test_him)$predictions
test_him$deltapos_pred_rf <- state_preds_him_rf
state_preds_jamm_rf <- predict(rfrf_ev, type = "response", data = test_jamm)$predictions
test_jamm$deltapos_pred_rf <- state_preds_jamm_rf
state_preds_pun_rf <- predict(rfrf_ev, type = "response", data = test_pun)$predictions
test_pun$deltapos_pred_rf <- state_preds_pun_rf
state_preds_raj_rf <- predict(rfrf_ev, type = "response", data = test_raj)$predictions
test_raj$deltapos_pred_rf <- state_preds_raj_rf
state_preds_uttk_rf <- predict(rfrf_ev, type = "response", data = test_uttk)$predictions
test_uttk$deltapos_pred_rf <- state_preds_uttk_rf
state_preds_uttp_rf <- predict(rfrf_ev, type = "response", data = test_uttp)$predictions
test_uttp$deltapos_pred_rf <- state_preds_uttp_rf



err_chan_rf <- rmse(test_chan$deltapos, test_chan$deltapos_pred_rf)
err_del_rf <- rmse(test_del$deltapos, test_del$deltapos_pred_rf)
err_har_rf <- rmse(test_har$deltapos, test_har$deltapos_pred_rf)
err_him_rf <- rmse(test_him$deltapos, test_him$deltapos_pred_rf)
err_jamm_rf <- rmse(test_jamm$deltapos, test_jamm$deltapos_pred_rf)
err_pun_rf <- rmse(test_pun$deltapos, test_pun$deltapos_pred_rf)
err_raj_rf <- rmse(test_raj$deltapos, test_raj$deltapos_pred_rf)
err_uttk_rf <- rmse(test_uttk$deltapos, test_uttk$deltapos_pred_rf)
err_uttp_rf <- rmse(test_uttp$deltapos, test_uttp$deltapos_pred_rf)


RMSE_RF <- c(err_chan_rf, err_del_rf, err_har_rf, err_him_rf, err_jamm_rf, err_pun_rf, err_raj_rf, err_uttk_rf, err_uttp_rf)

################################################################################
rmse <- cbind(RMSE_QP,RMSE_MARS,RMSE_RF)
State <- c("Chandigarh","Delhi","Haryana", "Himachal Pradesh", "Jammu and Kashmir", "Punjab", "Rajasthan", "Uttarakhand", "Uttar Pradesh")
rmse <- as.data.frame(cbind(State, RMSE_QP,RMSE_MARS,RMSE_RF))
rmse <- rename(rmse, QuasiPoisson=RMSE_QP, MARS=RMSE_MARS, RandomForest=RMSE_RF)

gt_tbl <- gt(data = rmse)
gt_tbl <-
    gt_tbl %>%
    tab_header(
        title = md("**RMSE of each model**"),
        subtitle = "COVID 19 North India")
gt_tbl

gtsave(gt_tbl, "acc_tab.png", path = "PLOTS")
