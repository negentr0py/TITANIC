
titanic<-bind_rows(train,test)
trainDim <- dim(train)
testDim <- dim(test)
trainR <- trainDim[1];testR <- testDim[1];titanicC <- dim(titanic)[2]

glimpse(titanic)

library(knitr)
library(ggthemes)
library(gridExtra)
library(scales)
library(qdap)
library(tm)
library(wordcloud)

kable(head(titanic))

titanic$Survived <- as.factor(titanic$Survived)
titanic$Pclass <- as.factor(titanic$Pclass)
titanic$Sex <- as.factor(titanic$Sex)
titanic$Cabin <- as.factor(titanic$Cabin)
titanic$Embarked <- as.factor(titanic$Embarked)

ifelse(length(unique(titanic[,1])) == nrow(titanic),"No duplicates","Duplicates detected!")

for (i in 1:titanicC){
  titanic[,i][titanic[,i]== ""] <- NA
}

# define a function to get number of NAs in each feature
getNA <- function(dt,NumCol){
  varsNames <- names(dt)
  NAs <- 0
  
  for (i in 1:NumCol){
    NAs <- c(NAs, sum(is.na(dt[,i])))
  }
  
  NAs <- NAs[-1]
  names(NAs)<- varsNames # make a vector of variable name and count of NAs
  
  NAs <- NAs[NAs > 0]
  NAs 
}

getNA(titanic,titanicC)

titanic[,c("PassengerId","Pclass","Fare","Embarked")] %>% filter(is.na(Embarked))
FareClassComp <- titanic %>% filter(!is.na(Embarked))

# plot embarkation ports versus fare mapped by passenger class
FareClassComp %>% 
  ggplot(aes(x = Embarked, y = Fare, fill = Pclass))+
  geom_boxplot()+
  geom_hline(aes(yintercept = 80),
             colour = "red", linetype = "dashed", lwd = 2)+
  scale_y_continuous(labels = dollar_format())+
  theme_few()

titanic$Embarked[is.na(titanic$Embarked)] <- "C"

titanic[,c("PassengerId","Pclass","Fare","Embarked")] %>% filter(is.na(Fare))
titanic$Fare[titanic$PassengerId == 1044] <-  median(titanic$Fare[titanic$Pclass == 3 & titanic$Embarked == "S"], na.rm = T)

titanic$Title <- gsub("(.*, )|(\\..*)","",titanic$Name)

# tabulate titles versus sex
table(titanic$Sex, titanic$Title)

MrTitles <- c("Capt", "Col", "Don", "Jonkheer", "Major", "Master", "Mr", "Rev", "Sir")
MrsTitles <- c("Dona", "Lady", "Mme", "Mrs", "the Countess")
MissTitles <- c("Miss", "Mlle", "Ms")

titanic$Title[titanic$Title %in% MrTitles] <- "Mr"
titanic$Title[titanic$Title %in% MrsTitles] <- "Mrs"
titanic$Title[titanic$Title %in% MissTitles] <- "Miss"
titanic$Title[titanic$Title == "Dr" & titanic$Sex == "female"] <- "Mrs"
titanic$Title[titanic$Title == "Dr" & titanic$Sex == "male"] <- "Mr"

titanic$Title <- as.factor(titanic$Title)
table(titanic$Title)

titanic$FamSz <- titanic$SibSp + titanic$Parch + 1

titanic$FamSzCat[titanic$FamSz == 1] <- "Singles"
titanic$FamSzCat[titanic$FamSz > 1 & titanic$FamSz <5] <- "Small"
titanic$FamSzCat[titanic$FamSz > 4] <- "Large"

titanic$FamSzCat <- as.factor(titanic$FamSzCat)

titanic$Surname <- sapply(titanic$Name, function(x) strsplit(x, split = "[,]")[[1]][1])
paste(nlevels(factor(titanic$Surname)), "families were onboard Titanic")

titanic$AgeStg[titanic$Age < 18 & !is.na(titanic$Age)] <- "Child"
titanic$AgeStg[titanic$Age >= 18 & !is.na(titanic$Age)] <- "Adult"

length(titanic$AgeStg[is.na(titanic$AgeStg)])

varsNames <- c("PassengerId","Pclass", "Sex", "SibSp", "Parch", "Fare", "FamSz", "FamSzCat", "AgeStg")

titanicAgeStg <- titanic[,varsNames]

# subset into two sets: one with age stage complete, and one with age stage missing
titanicAgeStgComp <- titanicAgeStg[!is.na(titanicAgeStg$AgeStg),]
titanicAgeStgMiss <- titanicAgeStg[is.na(titanicAgeStg$AgeStg),]

nTrain <- 0.75 * nrow(titanicAgeStgComp)

## sample row IDs
set.seed(3030)
sampleTrain <- sample(nrow(titanicAgeStgComp),nTrain)

## create train and test data sets
AgeStgTrain <- titanicAgeStgComp[sampleTrain,]
AgeStgTest <- titanicAgeStgComp[-sampleTrain,]

# use the glm Logistic Regression model to predict the age stage. Use Forward Stepwise algorithm to select the best predictors.

# build the null model with no predictors
set.seed(3030)
null_model <- glm(factor(AgeStg)~1, data = AgeStgTrain, family = "binomial")

# build the full model with all predictors
set.seed(3030)
full_model <- glm(factor(AgeStg)~Pclass+Sex+SibSp+Parch+Fare+FamSz+FamSzCat, data = AgeStgTrain, family = "binomial")

# perform forward stepwise algorithm to get an economic model with best predictors
step_model <- step(null_model, scope = list(lower= null_model,upper = full_model),direction = "forward")

train <- titanic[!is.na(titanic$Survived),]
test <- titanic[is.na(titanic$Survived),]

train %>% 
  ggplot(aes(x=Survived, fill = Survived))+
  geom_histogram(stat = "count")+
  labs(x = "Survival in the Titanic tragedy")+
  geom_label(stat='count',aes(label=..count..))+
  labs(fill = "Survival (0 = died, 1 = survived)")

survSumy <- summary(train$Survived)
survSumy

AgeStgTrain$stepProb <- predict(step_model, data = AgeStgTrain, type = "response")
AgeStgTest$stepProb <- predict(step_model, newdata = AgeStgTest, type = "response")
library(pROC)
# create the ROC curve of the stepwise for training and testing data
ROC_train <- roc(AgeStgTrain$AgeStg,AgeStgTrain$stepProb)
ROC_test <- roc(AgeStgTest$AgeStg,AgeStgTest$stepProb)

# Plot the ROC of the stepwise model: training and testing
plot(ROC_train,col = "red")
AvgChildCount <- mean(AgeStgTrain$AgeStg == "Child")

# Predict Age Stage in testing data if its probability is greater than the average 
AgeStgTest$AgeStgPred <- ifelse(AgeStgTest$stepProb > AvgChildCount,"Child", "Adult")

# check accuracy of prediction in testing data
acc <- percent(mean(AgeStgTest$AgeStg == AgeStgTest$AgeStgPred))
acc

# predicting missing Age Stage using the stepwise, logistic regression model
titanicAgeStgMiss$stepProb <- predict(step_model, newdata = titanicAgeStgMiss, type = "response")

titanicAgeStgMiss$AgeStg <- ifelse(titanicAgeStgMiss$stepProb > AvgChildCount,"Child", "Adult")

# update missing Age Stage in the full data
titanic <- left_join(titanic,titanicAgeStgMiss[,c("PassengerId","AgeStg")], by = "PassengerId", titanic.x = TRUE, titanic.y = FALSE)

titanic$AgeStg <- ifelse(is.na(titanic$AgeStg.x),titanic$AgeStg.y,titanic$AgeStg.x)
titanic <- titanic[,!colnames(titanic) %in% c("AgeStg.x","AgeStg.y")]   

titanic$AgeStg <- as.factor(titanic$AgeStg)

getNA(titanic,length(titanic))

train <- titanic[!is.na(titanic$Survived),]
test <- titanic[is.na(titanic$Survived),]

p1 <- ggplot(train,aes(x=Survived, fill=Pclass))+
  geom_histogram(stat = "count")+
  labs(x = "P1: Survival vs Class")

p2 <- ggplot(train,aes(x=Survived, fill=Sex))+
  geom_histogram(stat = "count")+
  labs(x = "P2: Survival vs Sex")

p3 <- ggplot(train,aes(x= Survived, fill = AgeStg))+
  geom_histogram(stat = "count", position = "dodge")+
  labs(x = "P3: Survival vs Age Stage")

p4 <- ggplot(train,aes(x=Survived, fill=Embarked))+
  geom_histogram(stat = "count")+
  labs(x = "P4: Survival vs Embarkment Port")

p5 <- ggplot(train,aes(x= Survived, y = Fare))+
  geom_boxplot()+
  labs(x = "P5: Survival vs Fare")

p6 <- ggplot(train,aes(x= Survived, fill = FamSzCat))+
  geom_histogram(stat = "count")+
  labs(x = "P6: Survival vs Category of Family Size")

p7 <- ggplot(train, aes(x = FamSz, fill = Survived)) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) +
  labs(x = 'P7: Survival vs Family Size')

grid.arrange(p1,p2,p3,p4,p5,p6,p7,ncol=2)

selVarsNames <- c("Pclass", "Sex", "SibSp", "Parch", "Fare","Embarked" ,"Title", "FamSzCat", "AgeStg")
nearZeroVar(train[,selVarsNames], saveMetrics = TRUE)

#train <- dplyr::filter(train,  !is.na(Age))
#train <- dplyr::filter(train,  !is.na(Survived))
#test <- dplyr::filter(test,  !is.na(Age))

nTrain <- round(0.75 * nrow(train))

## sample row IDs
sampleTrain <- sample(nrow(train),nTrain)

## create trainTemp and testTemp data sets
trainTemp <- train[sampleTrain,]
testTemp <- train[-sampleTrain,]

library(caret)
library(e1071)

set.seed(2020)

control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
modelRF <- train(Survived~Pclass+Sex+SibSp+Parch+Fare+Embarked+Title+FamSzCat+AgeStg, data = trainTemp, method = "rf", trControl = control)

print(modelRF)

set.seed(2020)

control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
modelGBM <- train(Survived~Pclass+Sex+SibSp+Parch+Fare+Embarked+Title+FamSzCat+AgeStg, data = trainTemp, method = "gbm", trControl = control, verbose = FALSE)

print(modelGBM)

set.seed(2020)

control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
modelSVM <- train(Survived~Pclass+Sex+SibSp+Parch+Fare+Embarked+Title+FamSzCat+AgeStg, data = trainTemp, method = "svmRadial", trControl = control)

print(modelSVM)

results <- resamples(list(RF=modelRF, GBM=modelGBM, SVM = modelSVM))

# summarize the distributions
compSummary <- summary(results)
compSummary

modelRFAcc <- percent(median(compSummary$values$`RF~Accuracy`))
modelGBMAcc <- percent(median(compSummary$values$`GBM~Accuracy`))
modelSVMAcc <- percent(median(compSummary$values$`SVM~Accuracy`))

# boxplots of results
bwplot(results)
dotplot(results)

rfPred<-predict(modelRF,trainTemp)
rfCM<-confusionMatrix(rfPred,trainTemp$Survived)
rfCM

modelRFacc<-percent(as.numeric(rfCM$overall[1]))
modelRFerr<-percent(1-(as.numeric(rfCM$overall[1])))

modelRFacc;modelRFerr

gbmPred<-predict(modelGBM,testTemp)
gbmCM<-confusionMatrix(gbmPred,testTemp$Survived)
gbmCM
modelGBMacc<-percent(as.numeric(gbmCM$overall[1]))
modelGBMerr<-percent(1-(as.numeric(gbmCM$overall[1])))

modelGBMacc;modelGBMerr

svmPred<-predict(modelSVM,testTemp)
svmCM<-confusionMatrix(svmPred,testTemp$Survived)
svmCM
modelSVMacc<-percent(as.numeric(svmCM$overall[1]))
modelSVMerr<-percent(1-(as.numeric(svmCM$overall[1])))

modelSVMacc;modelSVMerr

tblAcc <- data.frame("Accuracy"=c(modelRFacc, modelGBMacc, modelSVMacc), "Error"= c(modelRFerr,modelGBMerr,modelSVMerr), row.names = c("RF","GBM","SVM"))

tblAcc

kaggle_test <- predict(modelRF, testTemp)

kaggle_Submit <- data.frame(PassengerId = testTemp$PassengerId, Survived = kaggle_test)

subSummary <- table(kaggle_Submit$Survived)
subSummary

survPerc <- percent(subSummary[2]/(subSummary[1]+subSummary[2]))
write.csv(kaggle_Submit, file = "Titanic.csv", row.names = FALSE)

library(rpart)

# grow tree 
fit <- rpart(Survived ~ Age + Sex + Pclass + Fare,
             method="class", data=filtered_train)

printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits

# plot tree 
plot(fit, uniform=TRUE, 
     main="Classification Tree for Survivors of Titanic")
text(fit, use.n=TRUE, all=TRUE, cex=.75)

pfit<- prune(fit, cp=   fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])

plot(pfit, uniform=TRUE, 
     main="Pruned Classification Tree for Survivors of Titanic")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)

# grow tree 
fit <- rpart(Survived ~ Age.Group + Sex + Pclass + Fare.Bin + Embarked, 
             method="anova", data=train)

printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits

# create additional plots 
par(mfrow=c(1,2)) # two plots on one page 
rsq.rpart(fit) # visualize cross-validation results    

# plot tree 
plot(fit, uniform=TRUE, 
     main="Regression Tree for Survivors of Titanic")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

pfit<- prune(fit, cp=) # from cptable   

# plot the pruned tree 
plot(pfit, uniform=TRUE, 
     main="Pruned Regression Tree for Survivors of Titanic")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)

library(party)
fit <- party::ctree(Survived ~ Age + Sex + Pclass + Fare, 
             data=filtered_train)
nodes(fit, unique(where(fit)))
pmean <- sapply(weights(fit), function(w) weighted.mean(filtered_train$Survived, w))
drop(Predict(fit))
unlist(treeresponse(fit))
pmedian <- sapply(weights(fit), function(w)
  median(filtered_train$Survived[rep(1:nrow(filtered_train), w)]))
plot(filtered_train$Survived, pmean, col = "red")
points(filtered_train$Survived, pmedian, col = "blue")
plot(fit, main="Conditional Inference Tree for Survivors of Titanic")

library(partykit)
fit2 <- partykit::ctree(Survived ~ Age + Sex + Pclass + Fare, data=train)
fit2
plot(fit2, main="Conditional Inference Tree for Survivors of Titanic")
set.seed(754)
library(randomForest)
library(caret)
library(ggplot2)

filtered_train$Pclass<-factor(filtered_train$Pclass)
filtered_train$Sex<-factor(filtered_train$Sex)
filtered_train$Age<-factor(filtered_train$Age)
filtered_train$Fare<-factor(filtered_train$Fare)

levels(test$Survived) <- levels(filtered_train$Survived)
levels(test$Age) <- levels(filtered_train$Age)
levels(test$Sex) <- levels(filtered_train$Sex)
levels(test$Pclass) <- levels(filtered_train$Pclass)
levels(test$Fare) <- levels(filtered_train$Fare)

fit3 <- cforest(Survived ~ Age + Sex + Pclass + Fare,   data=train)
print(fit3) # view results 
plot(fit3)
importance(fit3)

fit4 <- glm(Survived ~ Age + Sex + Pclass + Fare, data=filtered_train, family=binomial("logit"))
summary(fit4)
#not going to use

prediction <- predict(fit, test)
prediction

prediction2 <- predict(fit2, test)
prediction2

prediction3 <- predict(fit3, test)
prediction3

Output<- data.frame(PassengerID = test$PassengerId, Survived = prediction)
Output

write.csv(Output, file = 'AY_titanic_output.csv', row.names = F)

Output2<- data.frame(PassengerID = test$PassengerId, Survived = prediction2)
Output2

write.csv(Output2, file = 'AY_titanic_output2.csv', row.names = F)

Output3<- data.frame(PassengerID = test$PassengerId, Survived = prediction3)
Output3

write.csv(Output3, file = 'AY_titanic_output3.csv', row.names = F)