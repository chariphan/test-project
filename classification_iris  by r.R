library(caret)
#Load Data
data("iris")
dataset <- iris

#dimension of dataset
dim(dataset)
#list type foe each attribute
sapply(dataset,class)
#peek data
head(dataset)
#list level for the species class
levels(dataset$Species)
#summarize 
summary(dataset)

# split input and output
x <- dataset[,1:4]
y <- dataset[,5]
# boxplot for each attribute on one image
par(mfrow=c(1,4))
for(i in 1:4) {
    boxplot(x[,i], main=names(iris)[i])
}
# barplot for class breakdown
plot(y)

# box and whisker plots for each attribute
featurePlot(x=x, y=y, plot="box")

# density plots for each attribute by class value
scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=x, y=y, plot="density", scales=scales)

#split data 10-fold cross validation Training data 9 and test data 1
# Run algorithms using 10-fold cross validation
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"

# 1 linear Linear Discriminant Analysis
set.seed(12)
fit.lda <- train(Species~., data=dataset, method="lda", metric=metric, trControl=control)
fit.lda
#2 Classification and Regression Trees
set.seed(12)
fit.cart<- train(Species~., data=dataset, method="rpart", metric=metric, trControl=control)
fit.cart
# 3 k-Nearest Neighbors 
set.seed(12)
fit.knn <- train(Species~., data=dataset, method="knn", metric=metric, trControl=control)
fit.knn

results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn))
results
summary(results)

# plot to compare accuracy of models
dotplot(results)
# the most accurate model in this case was LDA



