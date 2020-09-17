############################
#Using R                   #
#for drug effectiveness    #
#data mining               #
############################


library(datasets)
if (!require(skimr)) {install.packages("skimr"); library(skimr)} #expands on summary() by providing larger set of statistics
if (!require(caret)) {install.packages("caret"); library(caret)}
data("dhfr")  #229variables

####Understand the data####
#Display summary statistics
summary(dhfr) #for each variable
summary(dhfr$Y)

#Check if there are missing values
sum(is.na(dhfr))

skim(dhfr)

#Group data by Y (biological activity) then perform skim
dhfr %>% dplyr::group_by(Y) %>% skim() #You can even use t test after this

#if you wanna see histogram (here using a column to be the example)
hist(dhfr$moe2D_zagreb, col = "red")

#Feature importance plots
featurePlot(x = dhfr[,2:21],
            y = dhfr$Y,
            plot = "box",
            strip=strip.custom(par.strip.text=list(cex=.7)),
            scales=list(x = list(relation="free"),
                        y = list(relation="free"))
            )
#As you can see for moe2D_FCharge there are no significant difference between active and inactive drug molecule


####Data Mining####
#To put the stability of the model or it will give different result each time
set.seed(100)

#To split the data (stratified random split)
TrainingIndex <- createDataPartition(dhfr$Y, p=0.8, list = FALSE) #p-80% will go into the trainingset
TrainingSet <- dhfr[TrainingIndex,]
TestingSet <- dhfr[-TrainingIndex,]


#############################################
# SVM model (polynomial kernel)
Model <- train(Y ~ ., data = TrainingSet,
               method = 'svmPoly',
               na.action = na.omit,
               preProcess = c('scale', 'center'),
               trControl = trainControl(method = 'none'),
               tuneGrid = data.frame(degree=1, scale=1, C=1))

#Build CV (cross-validation) model
ModelCV <- train(Y ~ ., data = TrainingSet,
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
ModelTrainingConfusion <- confusionMatrix(ModelTraining, TrainingSet$Y)
print(ModelTrainingConfusion)

ModelTestingConfusion <- confusionMatrix(ModelTesting, TestingSet$Y)
print(ModelTestingConfusion)

ModelCvConfusion <- confusionMatrix(ModelCrossv, TestingSet$Y)
print(ModelCvConfusion)

#Feature importance
Importance <- varImp(Model)
plot(Importance, col='red')
#too many variables, hard to see, so to show only top 25 
plot(Importance, top=25, col="red")


