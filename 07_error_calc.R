#computes error based on the random forest models
oFol = "/Users/sophiewagner/Dropbox/HESS_Paper/Test/output/"
resultFol = "/Users/sophiewagner/Dropbox/HESS_Paper/Test/results/"

dgg_train <- read.table(file=paste0(oFol,"dgg_train.txt"),header = TRUE,sep =",")
dgg_val <- read.table(file=paste0(oFol,"dgg_val.txt"),header = TRUE,sep =",")
dgg_test <- read.table(file=paste0(oFol,"dgg_test.txt"),header = TRUE,sep =",")

ll_train <- read.table(file=paste0(oFol,"ll_train.txt"),header = TRUE,sep =",")
ll_train<- as.data.frame(ll_train)
ll_val <- read.table(file=paste0(oFol,"ll_val.txt"),header = TRUE,sep =",")
ll_test <- read.table(file=paste0(oFol,"ll_test.txt"),header = TRUE,sep =",")

pred_ll <- read.table(file=paste0(resultFol,"pred_ll.txt"),header = TRUE,sep =",")
pred_dgg <- read.table(file=paste0(resultFol,"pred_dgg.txt"),header = TRUE,sep =",")

#Table 2
#defines error function
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


# ==== Longitude-Latitude ======= #
#all observations
m1 <- mean(ll_test$Irrigation)
sd1 <- sd(ll_test$Irrigation)

e1 <- nrmse_func(ll_test$Irrigation,pred_ll,type = "sd")
e2 <- nrmse_func(ll_test$Irrigation,pred_ll,type = "mean")
e3 <- nrmse_func(ll_test$Irrigation,pred_ll,type = "maxmin")

#non-zero observations
test_ll2 <- ll_test[which(ll_test$Irr_Obs != 0),]
pred_ll2 <- pred_ll$x[which(ll_test$Irr_Obs != 0)]

m2 <- mean(test_ll2$Irrigation)
sd2 <- sd(test_ll2$Irrigation)

e4 <- nrmse_func(test_ll2$Irrigation,pred_ll2, type = "sd")
e5 <- nrmse_func(test_ll2$Irrigation,pred_ll2, type = "mean")
e6 <- nrmse_func(test_ll2$Irrigation,pred_ll2, type = "maxmin")

# ==== ISEA3H ======= #
#all observations
m3 <- mean(dgg_test$Irrigation)
sd3 <- sd(dgg_test$Irrigation)

e7 <- nrmse_func(dgg_test$Irrigation,pred_dgg,type = "sd")
e8 <- nrmse_func(dgg_test$Irrigation,pred_dgg,type = "mean")
e9 <- nrmse_func(dgg_test$Irrigation,pred_dgg,type = "maxmin")

#non-zero observations
test_dgg2 <- dgg_test[which(dgg_test$Irrigation != 0),]
pred_dgg2 <- pred_dgg$x[which(dgg_test$Irrigation != 0)]

m4 <- mean(test_dgg2$Irrigation)
sd4 <- sd(test_dgg2$Irrigation)

e10 <- nrmse_func(test_dgg2$Irrigation,pred_dgg2, type = "sd")
e11 <- nrmse_func(test_dgg2$Irrigation,pred_dgg2, type = "mean")
e12 <- nrmse_func(test_dgg2$Irrigation,pred_dgg2, type = "maxmin")

error_tab <- cbind(c(m1,sd1,e1,e2,e3,m2,sd2,e4,e5,e6),c(m3,sd3,e7,e8,e9,m4,sd4,e10,e11,e12))
colnames(error_tab) <- c("Longitude-Latitude","ISEA3H")
rownames(error_tab) <- c("Mean","SD","NRMSE - SD","NRMSE - Mean","NRMSE - Maxmin",
                         "Mean","SD","NRMSE - SD","NRMSE - Mean","NRMSE - Maxmin")
write.table(error_tab,paste0(resultFol,"tab02.txt"),col.names = TRUE,sep = ",",row.names = FALSE)

