library(mlbench)
data("BostonHousing")
#ดูข้อมูล
head(BostonHousing)
#check missing value
sum(is.na(BostonHousing))

#1 split data Train and Test
set.seed(1)
sample<-sample(c(TRUE,FALSE),nrow(BostonHousing),replace=TRUE,prob =c(0.8,0.2))
House_train <-BostonHousing[sample,] #Training set
House_test<-BostonHousing[!sample,] #Test set

#2 Building Model by trainnig set
House_model <-lm(medv~.,data=House_train)
House_model


#3 Predict
House_train$pred<-predict(House_model)
House_test$pred<-predict(House_model,newdata =House_test)

library(ggplot2)
ggplot(House_train,aes(x=pred,y=medv))+geom_point()+geom_abline()

ggplot(House_test,aes(x=pred,y=medv))+geom_point()+geom_abline()

summary(House_model)
