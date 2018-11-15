library(ggplot2)
library(dplyr)
library(ggthemes)
library(scales)
library(mice)

full <- read.csv('/Users/yingjunpan/Desktop/PM&BIA/BIA656/Final Project/titanic.csv',stringsAsFactors= FALSE)

#PClass
full$Survived <- factor(full$Survived)
ggplot(data = full, mapping = aes(x = Pclass, y = ..count.., fill=Survived)) + 
  geom_bar(stat = 'count', position='dodge') + 
  xlab('Ticket Class') + 
  ylab('Count') + 
  ggtitle('Pclass Influence') + 
  geom_text(stat = "count", aes(label = ..count..), position=position_dodge(width=1),vjust=-0.5)

#Sex
full$Sex <- factor(full$Sex)
ggplot(data = full, mapping = aes(x = Sex, y = ..count.., fill=Survived)) + 
  geom_bar(stat = 'count', position='dodge') + 
  xlab('Sex') + 
  ylab('Count') + 
  ggtitle('Sex Influence') + 
  geom_text(stat = "count", aes(label = ..count..), position=position_dodge(width=1),vjust=-0.5)

#Age
ggplot(full[!is.na(full$Survived),],aes(Age,color=Survived))+
  geom_line(aes(label=..count..), stat = 'bin', binwidth=5)  + 
  labs(title = 'Age Influence', x = 'Age', y = 'Count', fill = 'Survived')

#SibSp
ggplot(data = full, mapping = aes(x = SibSp, y = ..count.., fill=Survived)) + 
  geom_bar(stat = 'count', position='dodge') + 
  labs(title = 'Sibling and Spouse Influence', x = "Number of Sibling and Spouse", y = 'Count', fill = 'Survived') + 
  geom_text(stat = 'count', aes(label = ..count..), position=position_dodge(width=0.65), vjust=-0.5)

#Parch
ggplot(data = full, mapping = aes(x = Parch, y = ..count.., fill=Survived)) + 
  geom_bar(stat = 'count', position='dodge') + 
  labs(title = "Parent and Children Influence", x = "Number of Parent and Children", y = "Count", fill = "Survived") + 
  geom_text(stat = "count", aes(label = ..count..), position=position_dodge(width=0.65), vjust=-0.5)

#Fare
ggplot(data = full, aes(x = Fare, color=Survived)) + 
  geom_line(aes(label=..count..), stat = 'bin', binwidth=10)  + 
  labs(title = "Fare Influence", x = "Fare", y = "Count", fill = "Survived")

#Embarked
ggplot(data = full, mapping = aes(x = Embarked, y = ..count.., fill=Survived)) + 
  geom_bar(stat = 'count', position='dodge') + 
  labs(title = "Port of Embarkation Influence", x = "Port of Embarkation", y = "Count", fill = "Survived") + 
  geom_text(stat = "count", aes(label = ..count..), position=position_dodge(width=1), vjust=-0.5)

#deal with name
full$Title <- gsub("(.*, )|(\\..*)","",full$Name)
table(full$Sex, full$Title)
rare_title <- c('Dona','the Countess','Capt', 'Col', 'Don', 
                'Dr', 'Major', 'Rev','Jonkheer')
full$Title[full$Title == 'Lady'] <- 'Miss'
full$Title[full$Title == 'Mlle'] <- 'Miss' 
full$Title[full$Title == 'Ms'] <- 'Miss'
full$Title[full$Title == 'Mme'] <- 'Mrs'
full$Title[full$Title == 'Sir'] <- 'Mr'
full$Title[full$Title %in% rare_title] <- 'Rare Title'
table(full$Sex, full$Title)
ggplot(data = full, mapping = aes(x = Title, y = ..count.., fill=Survived)) + 
  geom_bar(stat = "count", position='stack') + 
  xlab('Title') + 
  ylab('Count') + 
  ggtitle('Title Influence') + 
  geom_text(stat = "count", aes(label = ..count..), position=position_stack(vjust = 0.5))

#Family size
full$Fsize <- full$SibSp + full$Parch + 1
ggplot(data = full, mapping = aes(x = Fsize, y = ..count.., fill=Survived)) + 
  geom_bar(stat = 'count', position='dodge') + 
  xlab('Family Size') + 
  ylab('Count') + 
  ggtitle('Family Size Influence') + 
  geom_text(stat = "count", aes(label = ..count..), position=position_dodge(width=1), vjust=-0.5)

#TicketCount
ticket.count <- aggregate(full$Ticket, by = list(full$Ticket), function(x) sum(!is.na(x)))
full$TicketCount <- apply(full, 1, function(x) ticket.count[which(ticket.count[, 1] == x['Ticket']), 2])
full$TicketCount <- factor(sapply(full$TicketCount, function(x) ifelse(x > 1, 'Shared Ticket', 'Single Ticket')))
ggplot(data = full, mapping = aes(x = TicketCount, y = ..count.., fill=Survived)) + 
  geom_bar(stat = 'count', position='dodge') + 
  xlab('Ticket Type') + 
  ylab('Count') + 
  ggtitle('Ticket Type Influence') + 
  geom_text(stat = "count", aes(label = ..count..), position=position_dodge(width=1), vjust=-0.5)

#Missing Value
sapply(full, function(x) sum(is.na(x)))
sapply(full, function(x) sum(x == ''))
full$PassengerId[is.na(full$Fare)]
full[1044, ]
full$Fare[1044] <- median(full[full$Pclass == '3' & full$Embarked == 'S', ]$Fare, na.rm = TRUE)
full[which(full$Embarked == ""),"Embarked"] <- "S"

#Age-Mice
set.seed(123)
mice_mod <- mice(full)
mice_output <- complete(mice_mod)
full$Age <- mice_output$Age
par(mfrow=c(1,2))
hist(full$Age, freq=F, main='Original Age',
     col='grey', ylim=c(0,0.04))
hist(mice_output$Age, freq=F, main='MICE Age',
     col='grey', ylim=c(0,0.04))

#AgeBox
full$AgeBox[full$Age < 18] <- 'child'
full$AgeBox[full$Age >= 18 & full$Age <= 50] <- 'middleAge'
full$AgeBox[full$Age > 50] <- 'old'
ggplot(data = full, mapping = aes(x = AgeBox, y = ..count.., fill=Survived)) + 
  geom_bar(stat = 'count', position='dodge') + 
  xlab('Age Group') + 
  ylab('Count') + 
  ggtitle('Age Group Influence') + 
  geom_text(stat = "count", aes(label = ..count..), position=position_dodge(width=1), vjust=-0.5)
full$Sex <- factor(full$Sex)
full$Title <- factor(full$Title)
full$Pclass <- factor(full$Pclass)
full$Fsize <- factor(full$Fsize)
full$AgeBox <- factor(full$AgeBox)
full$Embarked <- factor(full$Embarked)
full$Survived <- factor(full$Survived)

sapply(full, function(x) sum(is.na(x)))

train <- full[1:1047,] #80% data as train
test <- full[1048:1309,]


#random Forest
library(randomForest)
rf <- randomForest(Survived ~ Pclass + Title + Sex + Age + SibSp + Parch + Fsize + 
                     TicketCount + Fare + Embarked + AgeBox, data = train)
plot(rf, ylim=c(0,0.36))
varImpPlot(rf)
prediction <- predict(rf, test)
table(prediction,test$Survived)
rf.accuracy <- (82 + 156)/(262)
rf.accuracy
#random Forest Refine remove AgeBox, TicketCount, Parch, Embarked, SibSp
rf.1 <- randomForest(Survived ~ Pclass + Title + Sex + Age + Fsize + 
                       Fare, data = train)
plot(rf.1, ylim=c(0,0.36))
varImpPlot(rf.1)
prediction <- predict(rf.1, test)
table(prediction,test$Survived)
rf1.accuracy <- (83 + 157)/(262)
rf1.accuracy

# random forest
library(party)
cf <- cforest(Survived ~ Pclass + Title + Sex + Age + SibSp + Parch + Fsize + 
                 TicketCount + Fare + Embarked + AgeBox, data = train)
cf.Prediction <- predict(cf, test, OOB=TRUE, type = "response")
table(cf.Prediction,test$Survived)
cf.accuracy <- (90 + 158)/(262)
cf.accuracy

# Decision Trees
library(rpart)
dt <- rpart(Survived ~ Pclass + Title + Sex + Age + SibSp + Parch + Fsize + 
              TicketCount + Fare + Embarked + AgeBox, data=train, method="class")
prediction <- predict(dt, test, type='class') 
table(prediction,test$Survived)
dt.accuracy <- (91 + 155)/(262)
dt.accuracy

dt.1 <- ctree(Survived ~ Pclass + Title + Sex + Age + SibSp + Parch + Fsize + 
              TicketCount + Fare + Embarked + AgeBox, data=train)
dt1.Prediction <- predict(dt.1, test, OOB=TRUE, type = "response")
table(dt1.Prediction,test$Survived)
dt1.accuracy <- (91 + 155)/(262)
dt1.accuracy

# logstic Regression
lr <- glm(Survived ~ Pclass  + Sex + Age + SibSp + Parch + 
      Fare + Embarked + Title + Fsize + TicketCount +  AgeBox, family=binomial(link='logit'),data=train)
lr.prediction <- predict(lr,type='response', newdata=subset(test,select=c(2,3,5,6,7,8,10,12,13,14,15,16)))
lr.prediction <-  ifelse(lr.prediction>0.5,1,0)
error = mean(lr.prediction != test$Survived)
Accuracy <- 1-error
Accuracy

#svm
library(e1071)
svm <- svm(Survived ~ Pclass + Title + Sex + Age + SibSp + Parch + Fsize + 
              TicketCount + Fare + Embarked + AgeBox, data = train,cost = 100, gamma = 1)
Prediction <- predict(svm, test)
error = mean(Prediction != test$Survived)
Accuracy <- 1-error
Accuracy
