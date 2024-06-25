library(ranger)
library(ROSE)
library(parallel)
library(MASS)

# Assign folder for the data input (oFol) and where the results should be saved (resultFol)

numCores <- detectCores()

#creates classification and regression random forest model (isea3h grid)
ll_train <- read.table(file=paste0(oFol,"ll_train.txt"),header = TRUE,sep =",")
ll_val <- read.table(file=paste0(oFol,"ll_val.txt"),header = TRUE,sep =",")
ll_test <- read.table(file=paste0(oFol,"ll_test.txt"),header = TRUE,sep =",")

ll_train$Irr_Obs <- 0
ll_train$Irr_Obs[which(ll_train$Irrigation != 0)] <- 1
ll_test$Irr_Obs <- 0
ll_test$Irr_Obs[which(ll_test$Irrigation != 0)] <- 1
ll_val$Irr_Obs <- 0
ll_val$Irr_Obs[which(ll_val$Irrigation != 0)] <- 1

ll_List <- list(ll_train, ll_test)


nrmse_func <-  function(obs, pred, type = "sd") {
  
  squared_sums <- sum((obs - pred)^2)
  mse <- squared_sums/length(obs)
  rmse <- sqrt(mse)
  if (type == "sd") nrmse <- rmse/sd(obs)
  if (type == "mean") nrmse <- rmse/mean(obs)
  if (type == "maxmin") nrmse <- rmse/ (max(obs) - min(obs))
  if (type == "iq") nrmse <- rmse/ (quantile(obs, 0.75) - quantile(obs, 0.25))
  if (!type %in% c("mean", "sd", "maxmin", "iq")) message("Type not recognized")
  nrmse <- round(nrmse, 3)
  return(nrmse)
  
}



randomforest_ll <- function(data){
  ll_train <- ll_List[[1]]
  ll_test <- ll_List[[2]]
  e7 <- vector()
  e8 <- vector()
  e9 <- vector()
  e10 <- vector()
  e11 <- vector()
  e12 <- vector()
  # ==== Classification Forest ======= #
  for(i in 1:500){
    
    ### Draw a balanced sample - irrigated/not irrigated 
    samp <- ROSE::ovun.sample(Irr_Obs ~ Discharge+PopDens+MedInc+Evaporation+Precipitation+Irrigation+GDP, 
                              data = ll_train, method="both", N = nrow(dgg_train)*0.4, p = 0.5, 
                              subset=options("subset")$subset, na.action=options("na.action")$na.action)
    train_data <- as.data.frame(samp$data)
    train_data$Irr_Obs <- as.factor(train_data$Irr_Obs)
    train_data$PopDens <- as.numeric(train_data$PopDens)
    train_data$MedInc <- as.numeric(train_data$MedInc)
    train_data$Discharge <- as.numeric(train_data$Discharge)
    train_data$Precipitation <- as.numeric(train_data$Precipitation)
    train_data$Evaporation <- as.numeric(train_data$Evaporation)
    train_data$GDP <- as.factor(train_data$GDP)
    
    ### Number of trees and mtry from cv results
    rf6 <<- ranger(Irr_Obs ~  Discharge + PopDens + Evaporation + MedInc + Precipitation + GDP, 
                   data = train_data, y = Irr_Obs ,num.trees = 1000, classification = TRUE, probability = TRUE, 
                   min.node.size = 10, save.memory = TRUE ,write.forest = TRUE, mtry = 5, importance = "impurity")
    
    
    
    ll_test$Irr_Obs <- as.factor(ll_test$Irr_Obs)
    ll_test$PopDens <- as.numeric(ll_test$PopDens)
    ll_test$MedInc <- as.numeric(ll_test$MedInc)
    ll_test$Discharge <- as.numeric(ll_test$Discharge)
    ll_test$Precipitation <- as.numeric(ll_test$Precipitation)
    ll_test$Evaporation <- as.numeric(ll_test$Evaporation)
    ll_test$GDP <- as.factor(ll_test$GDP)
    
    ### Predict on test data
    pred_rf6 <- predict(rf6, ll_test , num.trees = 1000)
    mean_pred <- mean(pred_rf6$predictions[,2])
    pred <- rep(0, nrow(pred_rf6$predictions))
    pred[which(pred_rf6$predictions[,2]>mean_pred)] <- 1

    # ========== Regression Forest ============ #
    
    ### Define training data 
    train_reg <- ll_train[which(ll_train$Irrigation>0),]
    
    samples <- train_reg
    #Number of trees and mtry from cv results
    rf7 <<- ranger(Irrigation ~ Discharge+PopDens+Evaporation+MedInc+Precipitation+GDP, 
                   data = samples, y = samples$Irrigation ,num.trees = 3000, 
                   min.node.size = 10, mtry = 5, write.forest = TRUE, importance = "impurity", save.memory = TRUE)
    
    
    ### Predict on test data
    pred_rf7 <- predict(rf7, ll_test , num.trees = 3000)
    
    pred_ll <- pred*pred_rf7$predictions

    e7 <- c(e7,nrmse_func(ll_test$Irrigation,pred_ll,type = "sd"))
    e8 <- c(e8,nrmse_func(ll_test$Irrigation,pred_ll,type = "mean"))
    e9 <-c(e9,nrmse_func(ll_test$Irrigation,pred_ll,type = "maxmin"))
    
    test_ll2 <- dgg_test[which(ll_test$Irrigation != 0),]
    pred_ll2 <- pred_ll[which(ll_test$Irrigation != 0)]
    
    e10 <- c(e10,nrmse_func(test_ll2$Irrigation,pred_ll2, type = "sd"))
    e11 <- c(e11,nrmse_func(test_ll2$Irrigation,pred_ll2, type = "mean"))
    e12 <- c(e12,nrmse_func(test_ll2$Irrigation,pred_ll2, type = "maxmin"))
    
    
  }
  error_tab <- cbind(e7,e8,e9,e10,e11,e12)
  colnames(error_tab) <- c("NRMSE - SD","NRMSE - Mean","NRMSE - Maxmin",
                           "NRMSE - SD","NRMSE - Mean","NRMSE - Maxmin")
  write.table(error_tab,paste0(resultFol,"error_bt_ll.txt"),col.names = TRUE,sep = ",",row.names = FALSE)
  
}


results <- mclapply(ll_List, randomforest_ll, mc.cores = numCores-1)


