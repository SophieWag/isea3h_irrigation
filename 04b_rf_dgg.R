#creates classification and regression random forest model (isea3h grid)
dgg_train <- read.table(file=paste0(oFol,"dgg_train.txt"),header = TRUE,sep =",")
dgg_val <- read.table(file=paste0(oFol,"dgg_val.txt"),header = TRUE,sep =",")
dgg_test <- read.table(file=paste0(oFol,"dgg_test.txt"),header = TRUE,sep =",")


randomforest_dgg <- function(){
  
  # ==== Classification Forest ======= #
  for(i in 1:1){
    oob_nam <- paste("oob_dgg_", i, sep="")
    pred_nam <- paste("pred_dgg_", i, sep="")
    
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
    write.table(train_data,paste0(resultFol,"sample_class_isea3h.txt"),col.names = TRUE,sep = ",",row.names = FALSE)
    
    
    ### Number of trees and mtry from cv results
    index <- c(1000)
    oob <- vector()
    error <- vector()
    for(j in index){
      
      rf6 <<- ranger(Irr_Obs ~  Discharge + PopDens + Evaporation + MedInc + Precipitation + GDP, 
                    data = train_data, y = Irr_Obs ,num.trees = j, classification = TRUE, probability = TRUE, 
                    min.node.size = 10, write.forest = TRUE, mtry = 5, importance = "impurity")
      
      ### Out-Of-Bag error
      oob <- c(oob, rf6$prediction.error)
      
      ### Predict on test data
      pred_rf6 <- predict(rf6, dgg_val , num.trees = j)
      mean_pred <- mean(pred_rf6$predictions[,2])
      pred <- rep(0, nrow(pred_rf6$predictions))
      pred[which(pred_rf6$predictions[,2]>mean_pred)] <- 1
      
      ### Calculate prediction error
      error <- c(error, BrierScore(dgg_val$Irr_Obs, pred, scale = FALSE))
      
    }
    assign(oob_nam, oob)
    assign(pred_nam, error)
  }
  
  class_oob_dgg <- vector()
  for(i in 1:length(oob_dgg_1)){
    class_oob_dgg <- c(class_oob_dgg,
                        mean(oob_dgg_1[i]
                        )
    )
  }
  class_pred_dgg <- vector()
  for(i in 1:length(oob_dgg_1)){
    class_pred_dgg <- c(class_pred_dgg,
                             mean(pred_dgg_1[i]
                             )
    )
  }
  ts <- cbind(index,class_oob_dgg,class_pred_dgg)
  ts <- as.data.frame(ts)
  write.table(ts,paste0(resultFol,"error_classification_isea3h.txt"),col.names = TRUE,sep = ",",row.names = FALSE)
  
  # ========== Regression Forest ============ #
  
  ### Define training data 
  train_reg <- dgg_train[which(dgg_train$Irrigation>0),]
  write.table(train_reg,paste0(resultFol,"sample_reg_isea3h.txt"),col.names = TRUE,sep = ",",row.names = FALSE)
  val_reg <- dgg_val[which(dgg_val$Irrigation>0),]
  val_reg$Irrigation <- as.numeric(val_reg$Irrigation)
  val_reg$Irr_Obs <- as.factor(val_reg$Irr_Obs)
  val_reg$PopDens <- as.numeric(val_reg$PopDens)
  val_reg$MedInc <- as.numeric(val_reg$MedInc)
  val_reg$Discharge <- as.numeric(val_reg$Discharge)
  val_reg$Precipitation <- as.numeric(val_reg$Precipitation)
  val_reg$Evaporation <- as.numeric(val_reg$Evaporation)
  val_reg$GDP <- as.factor(val_reg$GDP)
  
 
  for(i in 1:1){
    oob_nam <- paste("oob_reg_dgg_", i, sep="")
    pred_nam <- paste("pred_reg_dgg_", i, sep="")
    samples <- train_reg
    
    #Number of trees and mtry from cv results
    index <- c(4000)
    oob <- vector()
    error <- vector()
    for(j in index){
      rf7 <<- ranger(Irrigation ~ Discharge+PopDens+Evaporation+MedInc+Precipitation+GDP, 
                    data = samples, y = samples$Irrigation ,num.trees = j, 
                    min.node.size = 10, mtry = 5, write.forest = TRUE, importance = "impurity")
      
      ### Compute out-of-bag error
      oob <- c(oob, rf7$prediction.error)
      
      ### Predict on test data
      pred_rf7 <- predict(rf7, val_reg , num.trees = j)
      
      ### Compute prediction error
      error <- c(error,mean((val_reg$Irrigation-pred_rf7$predictions)^2))
    }
    assign(oob_nam, oob)
    assign(pred_nam, error)
  }
  
  ### Compute cross validation error
  reg_oob_dgg <- vector()
  for(i in 1:length(oob_reg_dgg_1)){
    reg_oob_dgg <- c(reg_oob_dgg,
                      mean(oob_reg_dgg_1[i]
                      )
    )
  }
  reg_pred_dgg <- vector()
  for(i in 1:length(pred_reg_dgg_1)){
    reg_pred_dgg <- c(reg_pred_dgg,
                           mean(pred_reg_dgg_1[i]
                           )
    )
  }
  
  ### Plot the results
  ts <- cbind(index, reg_oob_dgg, reg_pred_dgg)
  ts <- as.data.frame(ts)
  write.table(ts,paste0(resultFol,"error_regression_isea3h.txt"),col.names = TRUE,sep = ",",row.names = FALSE)
  
}



