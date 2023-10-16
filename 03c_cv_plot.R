#graphs the CV results 
cv_class_mtry_ll <- read.table(file=paste0(resultFol,"cv_classification__mtry_ll.txt"),header = TRUE,sep =",")
cv_class_ntree_ll <- read.table(file=paste0(resultFol,"cv_classification_ntree_ll.txt"),header = TRUE,sep =",")
cv_reg_mtry_ll <- read.table(file=paste0(resultFol,"cv_regression_mtry_ll.txt"),header = TRUE,sep =",")
cv_reg_ntree_ll <- read.table(file=paste0(resultFol,"cv_regression_ntree_ll.txt"),header = TRUE,sep =",")

cv_class_mtry_dgg <- read.table(file=paste0(resultFol,"cv_classification_mtry_isea3h.txt"),header = TRUE,sep =",")
cv_class_ntree_dgg <- read.table(file=paste0(resultFol,"cv_classification_ntree_isea3h.txt"),header = TRUE,sep =",")
cv_reg_mtry_dgg <- read.table(file=paste0(resultFol,"cv_regression_mtry_isea3h.txt"),header = TRUE,sep =",")
cv_reg_ntree_dgg <- read.table(file=paste0(resultFol,"cv_regression_ntree_isea3h.txt"),header = TRUE,sep =",")


#Figure A6: Classification Longitude-Latitude
ts <- cv_class_ntree_ll
index <- ts$index
p<- ggplot(data=ts, aes(x=index, y=cv_ntree_class)) +
  geom_line(color = "#9E2961")+theme_minimal()+xlab("ntree")+ylab("CV Out-Of-Bag Error")+scale_x_continuous(breaks= index)+
  theme(axis.text.x = element_text(size = 10),axis.text.y = element_text(size = 14),
        axis.title=element_text(size=12,face="bold"))+
scale_color_manual(name = "Longitude-Latitude")
q <- ggplot(data=ts, aes(x=index, y=cv_ntree_class_pred)) +
  geom_line(color = "#9E2961")+theme_minimal()+xlab("ntree")+ylab("CV Prediction Error")+
  scale_x_continuous(breaks= index)+theme(axis.text.x = element_text(size = 10),axis.text.y = element_text(size = 14),
                                          axis.title=element_text(size=14,face="bold"))
png(paste0(resultFol,"figA6a.png"), width = 801, height = 464)
g <- ggarrange(p,q, ncol = 1, nrow = 2)
print(g)
g
dev.off()


ts <- cv_class_mtry_ll
index <- ts$ind
p<- ggplot(data=ts, aes(x=index, y=cv_mtry_class)) +
  geom_line(color = "#9E2961")+theme_minimal()+xlab("mtry")+ylab("CV Out-Of-Bag Error")+scale_x_continuous(breaks= index)+
  theme(axis.text.x = element_text(size = 10),axis.text.y = element_text(size = 14),
        axis.title=element_text(size=12,face="bold"))
q <- ggplot(data=ts, aes(x=index, y=cv_mtry_class_pred)) +
  geom_line(color = "#9E2961")+theme_minimal()+xlab("mtry")+ylab("CV Prediction Error")+
  scale_x_continuous(breaks= index)+theme(axis.text.x = element_text(size = 10),axis.text.y = element_text(size = 14),
                                          axis.title=element_text(size=14,face="bold"))
png(paste0(resultFol,"figA6b.png"),width = 801, height = 464)
g <- ggarrange(p,q, ncol = 1, nrow = 2)
print(g)
g
dev.off()



# Regression Longitude-Latitude
ts <- cv_reg_ntree_ll
index <- ts$index
p<- ggplot(data=ts, aes(x=index, y=cv_ntree_reg)) +
  geom_line(color = "#9E2961")+theme_minimal()+xlab("ntree")+ylab("CV Out-Of-Bag Error")+scale_x_continuous(breaks= index)+
  theme(axis.text.x = element_text(size = 10),axis.text.y = element_text(size = 14),
        axis.title=element_text(size=12,face="bold"))
q <- ggplot(data=ts, aes(x=index, y=cv_ntree_reg_pred)) +
  geom_line(color = "#9E2961")+theme_minimal()+xlab("ntree")+ylab("CV Prediction Error")+
  scale_x_continuous(breaks= index)+theme(axis.text.x = element_text(size = 10),axis.text.y = element_text(size = 14),
                                          axis.title=element_text(size=14,face="bold"))
png(paste0(resultFol,"figA6c.png"),width = 801, height = 464)
g <- ggarrange(p,q, ncol = 1, nrow = 2)
print(g)
g
dev.off()


ts <- cv_reg_mtry_ll
index <- ts$ind
p<- ggplot(data=ts, aes(x=index, y=cv_mtry_reg)) +
  geom_line(color = "#9E2961")+theme_minimal()+xlab("mtry")+ylab("CV Out-Of-Bag Error")+scale_x_continuous(breaks= index)+
  theme(axis.text.x = element_text(size = 10),axis.text.y = element_text(size = 14),
        axis.title=element_text(size=12,face="bold"))
q <- ggplot(data=ts, aes(x=index, y=cv_mtry_reg_pred)) +
  geom_line(color = "#9E2961")+theme_minimal()+xlab("mtry")+ylab("CV Prediction Error")+
  scale_x_continuous(breaks= index)+theme(axis.text.x = element_text(size = 10),axis.text.y = element_text(size = 14),
                                          axis.title=element_text(size=14,face="bold"))
png(paste0(resultFol,"figA6d.png"),width = 801, height = 464)
g <- ggarrange(p,q, ncol = 1, nrow = 2)
print(g)
g
dev.off()



#Figure A7: Classiffication ISEA3H  
ts <- cv_class_ntree_dgg
index <- ts$index
### Plot the result
p<- ggplot(data=ts, aes(x=index, y=cv_ntree_class)) +
  geom_line(color = "#280C4D")+theme_minimal()+xlab("ntree")+ylab("CV Out-Of-Bag Error")+scale_x_continuous(breaks= index)+
  theme(axis.text.x = element_text(size = 10),axis.text.y = element_text(size = 14),
        axis.title=element_text(size=12,face="bold"))
q <- ggplot(data=ts, aes(x=index, y=cv_ntree_class_pred)) +
  geom_line(color = "#280C4D")+theme_minimal()+xlab("ntree")+ylab("CV Prediction Error")+
  scale_x_continuous(breaks= index)+theme(axis.text.x = element_text(size = 10),axis.text.y = element_text(size = 14),
                                          axis.title=element_text(size=14,face="bold"))
png(paste0(resultFol,"figA7a.png"),width = 801, height = 464)
g <- ggarrange(p,q, ncol = 1, nrow = 2)
print(g)
g
dev.off()


ts <- cv_class_mtry_dgg
index <- ts$ind
p<- ggplot(data=ts, aes(x=index, y=cv_mtry_class)) +
  geom_line(color = "#280C4D")+theme_minimal()+xlab("mtry")+ylab("CV Out-Of-Bag Error")+scale_x_continuous(breaks= index)+
  theme(axis.text.x = element_text(size = 10),axis.text.y = element_text(size = 14),
        axis.title=element_text(size=12,face="bold"))
q <- ggplot(data=ts, aes(x=index, y=cv_mtry_class_pred)) +
  geom_line(color = "#280C4D")+theme_minimal()+xlab("mtry")+ylab("CV Prediction Error")+
  scale_x_continuous(breaks= index)+theme(axis.text.x = element_text(size = 10),axis.text.y = element_text(size = 14),
                                          axis.title=element_text(size=14,face="bold"))
png(paste0(resultFol,"figA7b.png"),width = 801, height = 464)
g <- ggarrange(p,q, ncol = 1, nrow = 2)
print(g)
g
dev.off()



# Regression ISEA3H
ts <- cv_reg_ntree_dgg
index <- ts$index
p<- ggplot(data=ts, aes(x=index, y=cv_ntree_reg)) +
  geom_line(color = "#280C4D")+theme_minimal()+xlab("ntree")+ylab("CV Out-Of-Bag Error")+scale_x_continuous(breaks= index)+
  theme(axis.text.x = element_text(size = 6),axis.text.y = element_text(size = 14),
        axis.title=element_text(size=12,face="bold"))
q <- ggplot(data=ts, aes(x=index, y=cv_ntree_reg_pred)) +
  geom_line(color = "#280C4D")+theme_minimal()+xlab("ntree")+ylab("CV Prediction Error")+
  scale_x_continuous(breaks= index)+theme(axis.text.x = element_text(size = 6),axis.text.y = element_text(size = 14),
                                          axis.title=element_text(size=14,face="bold"))
png(paste0(resultFol,"figA7c.png"),width = 801, height = 464)
g <- ggarrange(p,q, ncol = 1, nrow = 2)
print(g)
g
dev.off()


ts <- cv_reg_mtry_dgg
index <- ts$ind
p<- ggplot(data=ts, aes(x=index, y=cv_mtry_reg)) +
  geom_line(color = "#280C4D")+theme_minimal()+xlab("mtry")+ylab("CV Out_Of-Bag Error")+scale_x_continuous(breaks= index)+
  theme(axis.text.x = element_text(size = 10),axis.text.y = element_text(size = 14),
        axis.title=element_text(size=12,face="bold"))
q <- ggplot(data=ts, aes(x=index, y=cv_mtry_reg_pred)) +
  geom_line(color = "#280C4D")+theme_minimal()+xlab("mtry")+ylab("CV Prediction Error")+
  scale_x_continuous(breaks= index)+theme(axis.text.x = element_text(size = 10),axis.text.y = element_text(size = 14),
                                          axis.title=element_text(size=14,face="bold"))
png(paste0(resultFol,"figA7d.png"),width = 801, height = 464)
g <- ggarrange(p,q, ncol = 1, nrow = 2)
print(g)
g
dev.off()


