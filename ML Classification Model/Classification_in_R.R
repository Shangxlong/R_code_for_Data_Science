############################
#Using R                   #
#to build                  #
#a classification model    #
############################


#importing libraries
library(datasets)
if (!require(caret)) {install.packages("caret"); library(caret)}
library(ggplot2)

data(iris)

#To see if there is any missing data in the iris dataset
sum(is.na(iris))

#To put the stability of the model or it will give different result each time
set.seed(100)

#To split the data (stratified random split)
TrainingIndex <- createDataPartition(iris$Species, p=0.8, list = FALSE) #p-80% will go into the trainingset
TrainingSet <- iris[TrainingIndex,]
TestingSet <- iris[-TrainingIndex,]

#To draw the scatter plots and see the two distributions are similar or not
ggplot(TrainingSet, aes(x=Sepal.Width, y=Sepal.Length)) + 
 geom_point(aes(color = Species))

ggplot(TestingSet, aes(x=Sepal.Width, y=Sepal.Length)) + 
 geom_point(aes(color = Species))

############################################
# SVM model (polynomial kernel)
Model <- train(Species ~ ., data = TrainingSet,
               method = 'svmPoly',
               na.action = na.omit,
               preProcess = c('scale', 'center'),
               trControl = trainControl(method = 'none'),
               tuneGrid = data.frame(degree=1, scale=1, C=1))

#Build CV (cross-validation) model
ModelCV <- train(Species ~ ., data = TrainingSet,
                 method = 'svmPoly',
                 na.action = na.omit,
                 preProcess = c('scale', 'center'),
                 trControl = trainControl(method = 'cv', number = 10),
                 tuneGrid = data.frame(degree=1, scale=1, C=1))

#Apply model for prediction
ModelTraining <- predict(Model, TrainingSet)
ModelTesting <- predict(Model, TestingSet)
ModelCrossv <- predict(ModelCV, TestingSet)

#Model performance (Display confusion matrix and statistics) *Don't use it when dataset is imbalanced
ModelTrainingConfusion <- confusionMatrix(ModelTraining, TrainingSet$Species)
print(ModelTrainingConfusion)

ModelTestingConfusion <- confusionMatrix(ModelTesting, TestingSet$Species)
print(ModelTestingConfusion)

ModelCvConfusion <- confusionMatrix(ModelCrossv, TestingSet$Species)
print(ModelCvConfusion)

#Feature importance
Importance <- varImp(Model)
plot(Importance, col='red')

ImportanceCV <- varImp(ModelCV)
plot(ImportanceCV, col='red')



