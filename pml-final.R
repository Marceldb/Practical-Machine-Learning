# pml final project

# Objective: The goal of your project is to predict the manner in which they did the exercise. This is the "classe" variable in the training set. You may use any of the other variables to predict with. You should create a report describing how you built your model, how you used cross validation, what you think the expected out of sample error is, and why you made the choices you did. You will also use your prediction model to predict 20 different test cases.

# Data.The data for this project come from this source: http://groupware.les.inf.puc-rio.br/har. 

#loading libraries
library(caret)
library(rpart)
library(randomForest)
library(readr)
library(rattle)
library(dplyr)



#loading data with NA=NA, "" and DIV0
pml_training <- read_csv("pml-training.csv")
pml_testing <- read_csv("pml-testing.csv")

# pre-processing
names(pml_testing)
names(pml_training)
# 160 variables in testing and trainig 
# Deleting the first 7 variables (user name, x1, etc) of the training  set because not valuable as predictors
pml_training <- pml_training[, -(1:7)]
pml_testing <- pml_testing[, -(1:7)]

#deleting columns with nas
pml_training<-pml_training[, colSums(is.na(pml_training)) == 0]
pml_testing <- pml_testing[, colSums(is.na(pml_testing)) == 0]

#keeping for testing only columns still available in training , except for "classe" column, used for predictions 

select_columns <- names(pml_training)
select_columns<-select_columns[select_columns != "classe"]
pml_testing<-pml_testing %>% 
  select(one_of(select_columns)) 

set.seed(1327)
# splitting training into train+validation
TRAIN.PERCENT<- 0.7
inTrain <- createDataPartition(y=pml_training$classe, p=TRAIN.PERCENT, list=F)
training <- pml_training[inTrain, ]
validation <- pml_training[-inTrain, ]

# Exploration

dim(training); dim(validation); dim(pml_testing)

ggplot(data=training, aes(x = classe))+  geom_histogram(stat="count",color="black", fill="red")+
  theme_classic()+
  ggtitle("Observations left in training set after preprocessing")
names(training)

# Prediction Models

# Tree

modFit<-train(classe ~.,method="rpart", data=training)

print(modFit$finalModel)

png(file="tree.png",width=800,height=800)
fancyRpartPlot(modFit$finalModel)
dev.off()

prediction_tree<-predict(modFit,validation)

confMatrix_Tree <- confusionMatrix(prediction_tree, as.factor(validation$classe))

confMatrix_Tree
# Not a good model, let's try with Random Forest

# Random Forest
modFit_RF<-randomForest(as.factor(classe) ~ ., data=training, method="class")

prediction_forest <- predict(modFit_RF, validation, type="class")

confMatrix_rf <- confusionMatrix(prediction_forest, as.factor(validation$classe))
confMatrix_rf

# Very high accuracy (.9935) for the random forest model on the validation set.  

# Let's predict the 20 answers

answers <- predict(modFit_RF, pml_testing, type="class")

answers







