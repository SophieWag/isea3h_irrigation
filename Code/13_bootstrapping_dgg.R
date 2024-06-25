library(ranger)
library(ROSE)
library(parallel)
library(MASS)

# Assign folder for the data input (oFol) and where the results should be saved (resultFol)
numCores <- detectCores()

#creates classification and regression random forest model (isea3h grid)
dgg_train <- read.table(file=paste0(oFol,"dgg_train9.txt"), header = TRUE,sep =",")
dgg_val <- read.table(file=paste0(oFol,"dgg_val9.txt"),header = TRUE,sep =",")
dgg_test <- read.table(file=paste0(oFol,"dgg_test.txt"),header = TRUE,sep =",")

dgg_train$Irr_Obs <- 0
dgg_train$Irr_Obs[which(dgg_train$Irrigation != 0)] <- 1
dgg_test$Irr_Obs <- 0
dgg_test$Irr_Obs[which(dgg_test$Irrigation != 0)] <- 1
dgg_val$Irr_Obs <- 0
dgg_val$Irr_Obs[which(dgg_val$Irrigation != 0)] <- 1

dgg_List <- list(dgg_train, dgg_test)


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


randomforest_dgg <- function(data){
  dgg_train <- dgg_List[[1]]
  dgg_test <- dgg_List[[2]]

  e7 <- vector()
  e8 <- vector()
  e9 <- vector()
  e10 <- vector()
  e11 <- vector()
  e12 <- vector()
  # ==== Classification Forest ======= #
  for(i in 1:1){
    ### Draw a balanced sample - irrigated/not irrigated 
    samp <- ROSE::ovun.sample(Irr_Obs ~ Discharge+PopDens+MedInc+Evaporation+Precipitation+Year+Irrigation+GDP, 
                              data = dgg_train, method="both", N = nrow(dgg_train)*0.4, p = 0.5, 
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
                   data = train_data, y = Irr_Obs ,num.trees = 500, classification = TRUE, probability = TRUE, 
                    min.node.size = 10, save.memory = TRUE ,write.forest = TRUE, mtry = 5, importance = "impurity")
      
    dgg_test$Irr_Obs <- as.factor(dgg_test$Irr_Obs)
    dgg_test$PopDens <- as.numeric(dgg_test$PopDens)
    dgg_test$MedInc <- as.numeric(dgg_test$MedInc)
    dgg_test$Discharge <- as.numeric(dgg_test$Discharge)
    dgg_test$Precipitation <- as.numeric(dgg_test$Precipitation)
    dgg_test$Evaporation <- as.numeric(dgg_test$Evaporation)
    dgg_test$GDP <- as.factor(dgg_test$GDP)
    ### Predict on test data
    pred_rf6 <- predict(rf6 , dgg_test,num.trees = 500)
    mean_pred <- mean(pred_rf6$predictions[,2])
    pred <- rep(0, nrow(pred_rf6$predictions))
    pred[which(pred_rf6$predictions[,2]>mean_pred)] <- 1
      
    write.table(pred,paste0(resultFol,"pred_rf6_dgg7.txt"),col.names = TRUE,sep = ",",row.names = FALSE)

      # ========== Regression Forest ============ #
      
      ### Define training data 
      train_reg <- dgg_train[which(dgg_train$Irrigation>0),]
      
      samples <- train_reg
      #Number of trees and mtry from cv results
    
      rf7 <<- ranger(Irrigation ~ Discharge+PopDens+Evaporation+MedInc+Precipitation+GDP, 
                       data = samples, y = samples$Irrigation ,num.trees = 500, 
                       min.node.size = 10, mtry = 5, write.forest = TRUE, importance = "impurity", save.memory = TRUE)
        
        
      ### Predict on test data
      pred_rf7 <- predict(rf7,dgg_test , num.trees = 500)
      
      pred_dgg <- pred*pred_rf7$predictions
      write.table(pred_dgg,paste0(resultFol,"pred_dgg9.txt"),col.names = TRUE,sep = ",",row.names = FALSE)
      
      e7 <- c(e7,nrmse_func(dgg_test$Irrigation,pred_dgg,type = "sd"))
      e8 <- c(e8,nrmse_func(dgg_test$Irrigation,pred_dgg,type = "mean"))
      e9 <-c(e9,nrmse_func(dgg_test$Irrigation,pred_dgg,type = "maxmin"))
      
      test_dgg2 <- dgg_test[which(dgg_test$Irrigation != 0),]
      pred_dgg2 <- pred_dgg[which(dgg_test$Irrigation != 0)]
      
      e10 <- c(e10,nrmse_func(test_dgg2$Irrigation,pred_dgg2, type = "sd"))
      e11 <- c(e11,nrmse_func(test_dgg2$Irrigation,pred_dgg2, type = "mean"))
      e12 <- c(e12,nrmse_func(test_dgg2$Irrigation,pred_dgg2, type = "maxmin"))
      
      print(i)
    
  }
  error_tab <- cbind(e7,e8,e9,e10,e11,e12)
  print(error_tab)
  colnames(error_tab) <- c("NRMSE - SD","NRMSE - Mean","NRMSE - Maxmin",
                           "NRMSE - SD","NRMSE - Mean","NRMSE - Maxmin")
  write.table(error_tab,paste0(resultFol,"error_bt_res8.txt"),col.names = TRUE,sep = ",",row.names = FALSE)

}

  results <- lapply(dgg_List, randomforest_dgg)


