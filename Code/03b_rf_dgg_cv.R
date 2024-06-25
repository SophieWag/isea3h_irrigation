#performs cross-validation for the classification and regression random forests (isea3h grid)
dgg_train <- read.table(file=paste0(oFol,"dgg_train7.txt"),header = TRUE,sep =",")
dgg_val <- read.table(file=paste0(oFol,"dgg_val7.txt"),header = TRUE,sep =",")
dgg_test <- read.table(file=paste0(oFol,"dgg_test7.txt"),header = TRUE,sep =",")


randomforest_dgg_cv <- function(){

  dgg_train 
  # ==== Hyperparameter: NTREE ======= #
  #
  # ==== Classification Forest ======= #
  for(i in 1:50){
    oob_nam <- paste("oob_dgg_", i, sep="")
    pred_nam <- paste("pred_dgg_", i, sep="")
    
    ### Draw a balanced sample - irrigated/not irrigated 
    samp <- ROSE::ovun.sample(Irr_Obs ~ Discharge+PopDens+MedInc+Evaporation+Precipitation+Year+Irrigation+GDP, 
                              data = dgg_train, method="both", N = nrow(dgg_train)*0.1, p = 0.5, 
                              subset=options("subset")$subset, na.action=options("na.action")$na.action)
    train_data <- as.data.frame(samp$data)
    train_data$Irr_Obs <- as.factor(train_data$Irr_Obs)
    train_data$PopDens <- as.numeric(train_data$PopDens)
    train_data$MedInc <- as.numeric(train_data$MedInc)
    train_data$Discharge <- as.numeric(train_data$Discharge)
    train_data$Precipitation <- as.numeric(train_data$Precipitation)
    train_data$Evaporation <- as.numeric(train_data$Evaporation)
    train_data$GDP <- as.factor(train_data$GDP)
    
    index <- c(50,300, 500, 800, 1000, 2000, 3000, 4000, 5000)
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
                       mean(oob_1[i],oob_2[i],oob_3[i],oob_4[i],oob_5[i],oob_6[i],oob_7[i],oob_8[i],oob_9[i],oob_10[i],
                            oob_11[i],oob_12[i],oob_13[i],oob_14[i],oob_15[i],oob_16[i],oob_17[i],oob_18[i],oob_19[i],oob_20[i],
                            oob_21[i],oob_22[i],oob_23[i],oob_24[i],oob_25[i],oob_26[i],oob_27[i],oob_28[i],oob_29[i],oob_30[i],
                            oob_31[i],oob_32[i],oob_33[i],oob_34[i],oob_35[i],oob_36[i],oob_37[i],oob_38[i],oob_39[i],oob_40[i],
                            oob_41[i],oob_42[i],oob_43[i],oob_44[i],oob_45[i],oob_46[i],oob_47[i],oob_48[i],oob_49[i],oob_50[i]
                       )
    )
  }
  class_pred_dgg <- vector()
  for(i in 1:length(oob_dgg_1)){
    class_pred_dgg <- c(class_pred_dgg,
                        mean(pred_1[i],pred_2[i],pred_3[i],pred_4[i],pred_5[i],pred_6[i],pred_7[i],pred_8[i],pred_9[i],pred_10[i],
                             pred_11[i],pred_12[i],pred_13[i],pred_14[i],pred_15[i],pred_16[i],pred_17[i],pred_18[i],pred_19[i],pred_20[i],
                             pred_21[i],pred_22[i],pred_23[i],pred_24[i],pred_25[i],pred_26[i],pred_27[i],pred_28[i],pred_29[i],pred_30[i],
                             pred_31[i],pred_32[i],pred_33[i],pred_34[i],pred_35[i],pred_36[i],pred_37[i],pred_38[i],pred_39[i],pred_40[i],
                             pred_41[i],pred_42[i],pred_43[i],pred_44[i],pred_45[i],pred_46[i],pred_47[i],pred_48[i],pred_49[i],pred_50[i]
                        ))
  }
  ts <- cbind(index,class_oob_dgg,class_pred_dgg)
  ts <- as.data.frame(ts)
  write.table(ts,paste0(resultFol,"cv_classification_ntree_isea3h.txt"),col.names = TRUE,sep = ",",row.names = FALSE)
  

  # ========== Regression Forest ============ #
  
  ### Define training data 
  train_reg <- dgg_train[which(dgg_train$Irrigation>0),]
  val_reg <- dgg_val[which(dgg_val$Irrigation>0),]
  val_reg$Irrigation <- as.numeric(val_reg$Irrigation)
  val_reg$Irr_Obs <- as.factor(val_reg$Irr_Obs)
  val_reg$PopDens <- as.numeric(val_reg$PopDens)
  val_reg$MedInc <- as.numeric(val_reg$MedInc)
  val_reg$Discharge <- as.numeric(val_reg$Discharge)
  val_reg$Precipitation <- as.numeric(val_reg$Precipitation)
  val_reg$Evaporation <- as.numeric(val_reg$Evaporation)
  val_reg$GDP <- as.factor(val_reg$GDP)
  
  for(i in 1:50){
    oob_nam <- paste("oob_reg_dgg_", i, sep="")
    pred_nam <- paste("pred_reg_dgg_", i, sep="")
    samples <- train_reg
    
    index <- c(50,100,200,300,400,500, 1000, 2000, 4000)
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
                     mean(oob_reg1[i],oob_reg2[i],oob_reg3[i],oob_reg4[i],oob_reg5[i],oob_reg6[i],oob_reg7[i],oob_reg8[i],oob_reg9[i],oob_reg10[i],
                          oob_reg11[i],oob_reg12[i],oob_reg13[i],oob_reg14[i],oob_reg15[i],oob_reg16[i],oob_reg17[i],oob_reg18[i],oob_reg19[i],oob_reg20[i],
                          oob_reg21[i],oob_reg22[i],oob_reg23[i],oob_reg24[i],oob_reg25[i],oob_reg26[i],oob_reg27[i],oob_reg28[i],oob_reg29[i],oob_reg30[i],
                          oob_reg31[i],oob_reg32[i],oob_reg33[i],oob_reg34[i],oob_reg35[i],oob_reg36[i],oob_reg37[i],oob_reg38[i],oob_reg39[i],oob_reg40[i],
                          oob_reg41[i],oob_reg42[i],oob_reg43[i],oob_reg44[i],oob_reg45[i],oob_reg46[i],oob_reg47[i],oob_reg48[i],oob_reg49[i],oob_reg50[i]
                     )) 
  }
  reg_pred_dgg <- vector()
  for(i in 1:length(pred_reg_dgg_1)){
    reg_pred_dgg <- c(reg_pred_dgg,
                      mean(pred_reg1[i],pred_reg2[i],pred_reg3[i],pred_reg4[i],pred_reg5[i],pred_reg6[i],pred_reg7[i],pred_reg8[i],pred_reg9[i],pred_reg10[i],
                           pred_reg11[i],pred_reg12[i],pred_reg13[i],pred_reg14[i],pred_reg15[i],pred_reg16[i],pred_reg17[i],pred_reg18[i],pred_reg19[i],pred_reg20[i],
                           pred_reg21[i],pred_reg22[i],pred_reg23[i],pred_reg24[i],pred_reg25[i],pred_reg26[i],pred_reg27[i],pred_reg28[i],pred_reg29[i],pred_reg30[i],
                           pred_reg31[i],pred_reg32[i],pred_reg33[i],pred_reg34[i],pred_reg35[i],pred_reg36[i],pred_reg37[i],pred_reg38[i],pred_reg39[i],pred_reg40[i],
                           pred_reg41[i],pred_reg42[i],pred_reg43[i],pred_reg44[i],pred_reg45[i],pred_reg46[i],pred_reg47[i],pred_reg48[i],pred_reg49[i],pred_reg50[i]
                      ))
  }
  
  ts <- cbind(index, reg_oob_dgg, reg_pred_dgg)
  ts <- as.data.frame(ts)
  write.table(ts,paste0(resultFol,"cv_regression_ntree_isea3h.txt"),col.names = TRUE,sep = ",",row.names = FALSE)
  
  
  
  # ==== Hyperparameter: MTRY ======= #
  #
  # ==== Classification Forest ======= #
  for(i in 1:50){
    oob_nam <- paste("oob_dgg_", i, sep="")
    pred_nam <- paste("pred_dgg_", i, sep="")
    
    ### Draw a balanced sample - irrigated/not irrigated 
    samp <- ROSE::ovun.sample(Irr_Obs ~ Discharge+PopDens+MedInc+Evaporation+Precipitation+Year+Irrigation+GDP, 
                              data = dgg_train, method="both", N = nrow(dgg_train)*0.1, p = 0.5, 
                              subset=options("subset")$subset, na.action=options("na.action")$na.action)
    train_data <- as.data.frame(samp$data)
    train_data$Irr_Obs <- as.factor(train_data$Irr_Obs)
    train_data$PopDens <- as.numeric(train_data$PopDens)
    train_data$MedInc <- as.numeric(train_data$MedInc)
    train_data$Discharge <- as.numeric(train_data$Discharge)
    train_data$Precipitation <- as.numeric(train_data$Precipitation)
    train_data$Evaporation <- as.numeric(train_data$Evaporation)
    train_data$GDP <- as.factor(train_data$GDP)
    
    index <- seq(1,5, 0.5)
    oob <- vector()
    error <- vector()
    for(j in index){
      
      rf6 <<- ranger(Irr_Obs ~  Discharge + PopDens + Evaporation + MedInc + Precipitation + GDP, 
                     data = train_data, y = Irr_Obs ,num.trees = 500, classification = TRUE, probability = TRUE, 
                     min.node.size = 10, write.forest = TRUE, mtry = j, importance = "impurity")
      
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
                       mean(oob_1[i],oob_2[i],oob_3[i],oob_4[i],oob_5[i],oob_6[i],oob_7[i],oob_8[i],oob_9[i],oob_10[i],
                            oob_11[i],oob_12[i],oob_13[i],oob_14[i],oob_15[i],oob_16[i],oob_17[i],oob_18[i],oob_19[i],oob_20[i],
                            oob_21[i],oob_22[i],oob_23[i],oob_24[i],oob_25[i],oob_26[i],oob_27[i],oob_28[i],oob_29[i],oob_30[i],
                            oob_31[i],oob_32[i],oob_33[i],oob_34[i],oob_35[i],oob_36[i],oob_37[i],oob_38[i],oob_39[i],oob_40[i],
                            oob_41[i],oob_42[i],oob_43[i],oob_44[i],oob_45[i],oob_46[i],oob_47[i],oob_48[i],oob_49[i],oob_50[i]
                       )
    )
  }
  class_pred_dgg <- vector()
  for(i in 1:length(oob_dgg_1)){
    class_pred_dgg <- c(class_pred_dgg,
                        mean(pred_1[i],pred_2[i],pred_3[i],pred_4[i],pred_5[i],pred_6[i],pred_7[i],pred_8[i],pred_9[i],pred_10[i],
                             pred_11[i],pred_12[i],pred_13[i],pred_14[i],pred_15[i],pred_16[i],pred_17[i],pred_18[i],pred_19[i],pred_20[i],
                             pred_21[i],pred_22[i],pred_23[i],pred_24[i],pred_25[i],pred_26[i],pred_27[i],pred_28[i],pred_29[i],pred_30[i],
                             pred_31[i],pred_32[i],pred_33[i],pred_34[i],pred_35[i],pred_36[i],pred_37[i],pred_38[i],pred_39[i],pred_40[i],
                             pred_41[i],pred_42[i],pred_43[i],pred_44[i],pred_45[i],pred_46[i],pred_47[i],pred_48[i],pred_49[i],pred_50[i]
                        ))
  }
  ts <- cbind(index,class_oob_dgg,class_pred_dgg)
  ts <- as.data.frame(ts)
  write.table(ts,paste0(resultFol,"cv_classification_mtry_isea3h.txt"),col.names = TRUE,sep = ",",row.names = FALSE)
  
  
  # ========== Regression Forest ============ #
  
  ### Define training data 
  train_reg <- dgg_train[which(dgg_train$Irrigation>0),]
  val_reg <- dgg_val[which(dgg_val$Irrigation>0),]
  val_reg$Irrigation <- as.numeric(val_reg$Irrigation)
  val_reg$Irr_Obs <- as.factor(val_reg$Irr_Obs)
  val_reg$PopDens <- as.numeric(val_reg$PopDens)
  val_reg$MedInc <- as.numeric(val_reg$MedInc)
  val_reg$Discharge <- as.numeric(val_reg$Discharge)
  val_reg$Precipitation <- as.numeric(val_reg$Precipitation)
  val_reg$Evaporation <- as.numeric(val_reg$Evaporation)
  val_reg$GDP <- as.factor(val_reg$GDP)
  
  for(i in 1:50){
    oob_nam <- paste("oob_reg_dgg_", i, sep="")
    pred_nam <- paste("pred_reg_dgg_", i, sep="")
    samples <- train_reg
    
    index <- seq(1,5, 0.5)
    oob <- vector()
    error <- vector()
    for(j in index){
      rf7 <<- ranger(Irrigation ~ Discharge+PopDens+Evaporation+MedInc+Precipitation+GDP, 
                     data = samples, y = samples$Irrigation ,num.trees = 500, 
                     min.node.size = 10, mtry = j, write.forest = TRUE, importance = "impurity")
      
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
                     mean(oob_reg1[i],oob_reg2[i],oob_reg3[i],oob_reg4[i],oob_reg5[i],oob_reg6[i],oob_reg7[i],oob_reg8[i],oob_reg9[i],oob_reg10[i],
                          oob_reg11[i],oob_reg12[i],oob_reg13[i],oob_reg14[i],oob_reg15[i],oob_reg16[i],oob_reg17[i],oob_reg18[i],oob_reg19[i],oob_reg20[i],
                          oob_reg21[i],oob_reg22[i],oob_reg23[i],oob_reg24[i],oob_reg25[i],oob_reg26[i],oob_reg27[i],oob_reg28[i],oob_reg29[i],oob_reg30[i],
                          oob_reg31[i],oob_reg32[i],oob_reg33[i],oob_reg34[i],oob_reg35[i],oob_reg36[i],oob_reg37[i],oob_reg38[i],oob_reg39[i],oob_reg40[i],
                          oob_reg41[i],oob_reg42[i],oob_reg43[i],oob_reg44[i],oob_reg45[i],oob_reg46[i],oob_reg47[i],oob_reg48[i],oob_reg49[i],oob_reg50[i]
                     )) 
  }
  reg_pred_dgg <- vector()
  for(i in 1:length(pred_reg_dgg_1)){
    reg_pred_dgg <- c(reg_pred_dgg,
                      mean(pred_reg1[i],pred_reg2[i],pred_reg3[i],pred_reg4[i],pred_reg5[i],pred_reg6[i],pred_reg7[i],pred_reg8[i],pred_reg9[i],pred_reg10[i],
                           pred_reg11[i],pred_reg12[i],pred_reg13[i],pred_reg14[i],pred_reg15[i],pred_reg16[i],pred_reg17[i],pred_reg18[i],pred_reg19[i],pred_reg20[i],
                           pred_reg21[i],pred_reg22[i],pred_reg23[i],pred_reg24[i],pred_reg25[i],pred_reg26[i],pred_reg27[i],pred_reg28[i],pred_reg29[i],pred_reg30[i],
                           pred_reg31[i],pred_reg32[i],pred_reg33[i],pred_reg34[i],pred_reg35[i],pred_reg36[i],pred_reg37[i],pred_reg38[i],pred_reg39[i],pred_reg40[i],
                           pred_reg41[i],pred_reg42[i],pred_reg43[i],pred_reg44[i],pred_reg45[i],pred_reg46[i],pred_reg47[i],pred_reg48[i],pred_reg49[i],pred_reg50[i]
                      ))
  }
  
  ts <- cbind(index, reg_oob_dgg, reg_pred_dgg)
  ts <- as.data.frame(ts)
  write.table(ts,paste0(resultFol,"cv_regression_mtry_isea3h.txt"),col.names = TRUE,sep = ",",row.names = FALSE)
  

}




