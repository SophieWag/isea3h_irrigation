#predicts irrigation fraction on the test dataframe based on the ranfom forest models
#creates plot predicted vs. actual irrigation fraction
ll_train <- read.table(file=paste0(oFol,"ll_train.txt"),header = TRUE,sep =",")
ll_train<- as.data.frame(ll_train)
ll_val <- read.table(file=paste0(oFol,"ll_val.txt"),header = TRUE,sep =",")
ll_test <- read.table(file=paste0(oFol,"ll_test.txt"),header = TRUE,sep =",")

dgg_train <- read.table(file=paste0(oFol,"dgg_train.txt"),header = TRUE,sep =",")
dgg_val <- read.table(file=paste0(oFol,"dgg_val.txt"),header = TRUE,sep =",")
dgg_test <- read.table(file=paste0(oFol,"dgg_test.txt"),header = TRUE,sep =",")


# ====== Longitude-Latitude ===== #
#creates classification and regression random forest models (rf4 and rf5)
randomforest_ll()

#Predict on test data - classification
pred_rf4 <- predict(rf4, ll_test , num.trees = 1000)
mean_pred <- mean(pred_rf4$predictions[,2])
pred <- rep(0, nrow(pred_rf4$predictions))
pred[which(pred_rf4$predictions[,2]>mean_pred)] <- 1

#Predict on test data - regression
pred_rf5 <- predict(rf5, ll_test , num.trees = 300)

#Multiply predictions to obtain final prediction
pred_ll <- pred*pred_rf5$predictions
write.table(pred_ll,paste0(resultFol,"pred_ll.txt"),col.names = TRUE,sep = ",",row.names = FALSE)


#Compute the difference between actual and predicted irrigation
diff_pred <- ll_test$Irrigation - pred_ll
diff_pred <- as.data.frame(diff_pred)
write.table(diff_pred,paste0(resultFol,"diff_pred_ll.txt"),col.names = TRUE,sep = ",",row.names = FALSE)



# ====== ISEA3H ===== #
#creates classification and regression random forest models (rf6 and rf7)
randomforest_dgg()

#Predict on test data - classification
pred_rf6 <- predict(rf6, dgg_test , num.trees = 1000)
mean_pred <- mean(pred_rf6$predictions[,2])
pred <- rep(0, nrow(pred_rf6$predictions))
pred[which(pred_rf6$predictions[,2]>mean_pred)] <- 1

#Predict on test data - regression
pred_rf7 <- predict(rf7, dgg_test, num.trees = 4000)

#Multiply results to obtain final prediction
pred_dgg <- pred*pred_rf7$predictions
write.table(pred_dgg,paste0(resultFol,"pred_dgg.txt"),col.names = TRUE,sep = ",",row.names = FALSE)

#Compute the difference between actual and predicted irrigation
diff_dgg <- dgg_test$Irrigation-pred_dgg
write.table(diff_dgg,paste0(resultFol,"diff_pred_dgg.txt"),col.names = TRUE,sep = ",",row.names = FALSE)


# Figure 3:  Observed vs Predicted 
pred_ll <- read.table(file=paste0(resultFol,"pred_ll.txt"),header = TRUE,sep =",")
pred_dgg <- read.table(file=paste0(resultFol,"pred_dgg.txt"),header = TRUE,sep =",")

# Figure 3a
K <- cbind(ll_test,pred_ll)

png(paste0(resultFol,paste0("fig3a.png")))
p<- scatterPlot(K, x = "x", y = "Irrigation", method = "hexbin", cols = "inferno",
            xlab = "Predicted values", ylab = "Actual values")
p
dev.off()

# Figure 3b
K <- cbind(dgg_test,pred_dgg)

png(paste0(resultFol,paste0("fig3b.png")))
p<- scatterPlot(K, x = "x", y = "Irrigation", method = "hexbin", cols = "inferno",
                xlab = "Predicted values", ylab = "Actual values")
p
dev.off()

