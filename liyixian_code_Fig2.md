setwd('')
library(gbm)
library(plyr)
library(dplyr)
library(ggplot2)
library(lattice)
library(caret)
library(hydroGOF)
library(gvlma)
library(car)

aquatic<-read.csv("Data_share_AP.csv")
set.seed(100)
train_aquatic<- sample(nrow(aquatic), 0.9*nrow(aquatic))
aquatic_train <- aquatic[train_aquatic,] 
aquatic_test <- aquatic[-train_aquatic,]

sub_aquatic_train <- aquatic_train %>%
  select(ISO,Elevation,Evaporation,
         PDM,Channel,TP,TNFux,
         TN_nitrite1,He)
sub_aquatic_test <- aquatic_test %>%
  select(ISO,Elevation,Evaporation,
         PDM,Channel,TP,TNFux,
         TN_nitrite1,He)

set.seed(100)
brt_aquatic<-gbm(He ~ ISO+Elevation+Evaporation+
                   PDM+Channel+TP+TNFux+TN_nitrite1,
                 data = sub_aquatic_train, distribution = "gaussian", n.trees = 3000,
                 interaction.depth = 9, shrinkage = 0.01, 
                 bag.fraction = 0.5,cv.folds = 10)
brt_aquatic_recyle<- NULL
brt_aquatic_recyle[[1]]<-brt_aquatic
print(brt_aquatic)


best_inter_aquatic<-gbm.perf(brt_aquatic,method = "cv")

best_inter_aquatic_recyle<-NULL
best_inter_aquatic_recyle[[1]]<-best_inter_aquatic

summary_record<-summary(brt_aquatic,n.trees=best_inter_aquatic)
summary_recyle<-NULL
summary_recyle[[1]] <-summary_record
summary_recyle

sub_aquatic_test$He_pred_1<-predict(brt_aquatic,sub_aquatic_test)
head(sub_aquatic_test[, 9:10])

plot(sub_aquatic_test$He_pred_1,sub_aquatic_test$He, xlim=c(0,0.6),ylim=c(0,0.6),
     xlab="Predicted GD", ylab="Real GD")
abline(a=0,b=1)
fitline<-lm(He~He_pred_1,sub_aquatic_test)
abline(fitline,lty=2)
summary(fitline)


lm_fit1_aquatic<-lm(He ~ ISO+Elevation+Evaporation+PDM+Channel+TP+TNFux+TN_nitrite1,
                    data = sub_aquatic_train)
summary(lm_fit1_aquatic)
sub_aquatic_test$He_pred_2<-predict(lm_fit1_aquatic,sub_aquatic_test)
plot(sub_aquatic_test$He_pred_2,sub_aquatic_test$He, xlim=c(0,0.6),ylim=c(0,0.6),
     xlab="Predicted GB", ylab="Observed GD")
abline(a=0,b=1)
fitline2<-lm(He~He_pred_2,sub_aquatic_test)
abline(fitline2,lty=2)
summary(fitline2)


cor_1 <- cor(sub_aquatic_test$He_pred_1,sub_aquatic_test$He)
R2_1 <- R2(sub_aquatic_test$He_pred_1,sub_aquatic_test$He)
RMSE_1 <- RMSE(sub_aquatic_test$He_pred_1,sub_aquatic_test$He)
nRMSE_1<-RMSE_1/mean(sub_aquatic_test$He)*100
ME_1<-me(sub_aquatic_test$He_pred_1,sub_aquatic_test$He)
P_value_1<-t.test(sub_aquatic_test$He_pred_1-sub_aquatic_test$He)$p.value

cor_1_recyle <- NULL
R2_1_recyle <- NULL
RMSE_1_recyle <- NULL
nRMSE_1_recyle<- NULL
ME_1_recyle <- NULL
P_value_1_recyle<-NULL

cor_1_recyle[[1]] <- cor_1
R2_1_recyle[[1]] <- R2_1
RMSE_1_recyle[[1]] <- RMSE_1
nRMSE_1_recyle[[1]] <- nRMSE_1
ME_1_recyle[[1]] <- ME_1
P_value_1_recyle[[1]]<-P_value_1



cor_2_recyle <- NULL
R2_2_recyle <- NULL
RMSE_2_recyle <- NULL
nRMSE_2_recyle<- NULL
ME_2_recyle <- NULL
P_value_2_recyle<-NULL

cor_2 <- cor(sub_aquatic_test$He_pred_2,sub_aquatic_test$He)
R2_2 <- R2(sub_aquatic_test$He_pred_2,sub_aquatic_test$He)
RMSE_2 <- RMSE(sub_aquatic_test$He_pred_2,sub_aquatic_test$He)
nRMSE_2<-RMSE_2/mean(sub_aquatic_test$He)*100
ME_2<-me(sub_aquatic_test$He_pred_2,sub_aquatic_test$He)
P_value_2<-t.test(sub_aquatic_test$He_pred_2-sub_aquatic_test$He)$p.value

cor_2_recyle[[1]] <- cor_2
R2_2_recyle[[1]] <- R2_2
RMSE_2_recyle[[1]] <- RMSE_2
nRMSE_2_recyle[[1]] <- nRMSE_2
ME_2_recyle[[1]] <- ME_2
P_value_2_recyle[[1]]<-P_value_2


RMSE_1_recyle[[1]] <- RMSE_1; RMSE_1_recyle#1是brt，2是lm，越小越好
RMSE_2_recyle[[1]] <- RMSE_2; RMSE_2_recyle

nRMSE_1_recyle[[1]] <- nRMSE_1; nRMSE_1_recyle
nRMSE_2_recyle[[1]] <- nRMSE_2; nRMSE_2_recyle


dev.off()

