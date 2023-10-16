#creates classification and regression random forest model (isea3h grid)
ll_train <- read.table(file=paste0(oFol,"ll_train.txt"),header = TRUE,sep =",")
ll_train<- as.data.frame(ll_train)
ll_val <- read.table(file=paste0(oFol,"ll_val.txt"),header = TRUE,sep =",")
ll_test <- read.table(file=paste0(oFol,"ll_test.txt"),header = TRUE,sep =",")


randomforest_ll <- function(){

  # ==== Classification Forest ======= #
  for(i in 1:1){
    oob_nam <- paste("oob_", i, sep="")
    pred_nam <- paste("pred_", i, sep="")
    
    ### Draw a balanced sample - irrigated/not irrigated 
    samp <- ROSE::ovun.sample(Irr_Obs ~ Discharge+PopDens+MedInc+Evaporation+Precipitation+Time+Irrigation+GDP, 
                              data = ll_train, method="both", N = nrow(ll_train)*0.4, p = 0.5, 
                              subset=options("subset")$subset, na.action=options("na.action")$na.action)
    train_data <- as.data.frame(samp$data)
    train_data$Irr_Obs <- as.factor(train_data$Irr_Obs)
    train_data$PopDens <- as.numeric(train_data$PopDens)
    train_data$MedInc <- as.numeric(train_data$MedInc)
    train_data$Discharge <- as.numeric(train_data$Discharge)
    train_data$Precipitation <- as.numeric(train_data$Precipitation)
    train_data$Evaporation <- as.numeric(train_data$Evaporation)
    train_data$GDP <- as.factor(train_data$GDP)
    write.table(train_data,paste0(resultFol,"sample_class_ll.txt"),col.names = TRUE,sep = ",",row.names = FALSE)
    
    
    index <- c(1000)
    oob <- vector()
    error <- vector()
    for(j in index){
      
      rf4 <<- ranger(Irr_Obs ~ Discharge + PopDens + Evaporation + MedInc + Precipitation + GDP, 
                    data = train_data, y = Irr_Obs ,num.trees = j, classification = TRUE, probability = TRUE, 
                    min.node.size = 10, write.forest = TRUE, mtry = 1.5, importance = "impurity")
      
      ### Out-Of-Bag error
      oob <- c(oob, rf4$prediction.error)
      
      ### Predict on validation data -Y sample randomly from the validation data set
      validation_data <- ll_val[sample(nrow(ll_val), nrow(ll_val)*0.4), ]
      validation_data$Irr_Obs <- as.numeric(validation_data$Irr_Obs)
      pred_rf4 <- predict(rf4, validation_data , num.trees = j)
      
      mean_pred <- mean(pred_rf4$predictions[,2])
      pred <- rep(0, nrow(pred_rf4$predictions))
      pred[which(pred_rf4$predictions[,2]>mean_pred)] <- 1
      
      ### Calculate prediction error
      error <- c(error, BrierScore(validation_data$Irr_Obs, pred, scale = FALSE))
      
    }
    assign(oob_nam, oob)
    assign(pred_nam, error)
  }
  
  class_oob <- vector()
  for(i in 1:length(oob_1)){
    class_oob <- c(class_oob,
                        mean(oob_1[i]
                             ))
  }
  class_pred <- vector()
  for(i in 1:length(oob_1)){
    class_pred <- c(class_pred,
                             mean(pred_1[i]
                             ))
  }
  ts <- cbind(index,class_oob,class_pred)
  ts <- as.data.frame(ts)
  write.table(ts,paste0(resultFol,"error_classification_ll.txt"),col.names = TRUE,sep = ",",row.names = FALSE)
  
  # ========== Regression Forest ============ #
  
  ### Define data - all data points with large than zero irrigation
  train_reg <- ll_train[which(ll_train$Irrigation>0),]
  write.table(train_reg,paste0(resultFol,"sample_reg_ll.txt"),col.names = TRUE,sep = ",",row.names = FALSE)
  val_reg <- ll_val[which(ll_val$Irrigation>0),]
  
  val_reg$Irrigation <- as.numeric(val_reg$Irrigation)
  val_reg$Irr_Obs <- as.factor(val_reg$Irr_Obs)
  val_reg$PopDens <- as.numeric(val_reg$PopDens)
  val_reg$MedInc <- as.numeric(val_reg$MedInc)
  val_reg$Discharge <- as.numeric(val_reg$Discharge)
  val_reg$Precipitation <- as.numeric(val_reg$Precipitation)
  val_reg$Evaporation <- as.numeric(val_reg$Evaporation)
  
  for(i in 1:1){
    oob_nam <- paste("oob_reg", i, sep="")
    pred_nam <- paste("pred_reg", i, sep="")
    samples <- train_reg
    
    samples <- as.data.frame(samples)
    samples$Irrigation <- as.numeric(samples$Irrigation)
    samples$Irr_Obs <- as.factor(samples$Irr_Obs)
    samples$PopDens <- as.numeric(samples$PopDens)
    samples$MedInc <- as.numeric(samples$MedInc)
    samples$Discharge <- as.numeric(samples$Discharge)
    samples$Precipitation <- as.numeric(samples$Precipitation)
    samples$Evaporation <- as.numeric(samples$Evaporation)
    
    #Number of trees and mtry from cv results
    index <- c(300)
    oob <- vector()
    error <- vector()
    for(j in index){
      rf5 <<- ranger(Irrigation ~ Discharge+PopDens+Evaporation+MedInc+Precipitation+GDP, 
                    data = samples, y = samples$Irrigation ,num.trees = j, 
                    min.node.size = 10, mtry = 3, write.forest = TRUE, importance = "impurity")
      
      ### Compute out-of-bag error
      oob <- c(oob, rf5$prediction.error)
      
      ### Predict on test data
      pred_rf5 <- predict(rf5, val_reg , num.trees = j)
      
      ### Compute prediction error
      error <- c(error,mean((val_reg$Irrigation-pred_rf5$predictions)^2))
    }
    assign(oob_nam, oob)
    assign(pred_nam, error)
  }
  
  reg_oob <- vector()
  for(i in 1:length(oob_reg1)){
    reg_oob <- c(reg_oob,
                      mean(oob_reg1[i])
    ) 
  }
  reg_pred <- vector()
  for(i in 1:length(pred_reg1)){
    reg_pred <- c(reg_pred,
                           mean(pred_reg1[i]))
    
    
  }
  ts <- cbind(index, reg_oob, reg_pred)
  ts <- as.data.frame(ts)
  write.table(ts,paste0(resultFol,"error_regression_ll.txt"),col.names = TRUE,sep = ",",row.names = FALSE)
}



