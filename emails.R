remove(list=ls())
setwd('/Documents/email')

library(Hmisc)
library(tm)
library(caTools)
library(rpart)
library(randomForest)
library(rpart.plot)
library(ROCR)


#Read csv file
emails <- read.csv('emails.csv', stringsAsFactors = FALSE)

#number of emails in the dataset
nrow(emails)

#number of emails that are spam
sum(emails$spam)

#Extracting first word of any email
first<- emails$text[[1]]
first[1]
tolower(substring(first[1],1, 7))

#Alternative extraction of first word using Hmisc
tolower(first.word(first[1]))

#max number of characters in any email: ~43952
maxchr <- max(nchar(emails$text))

#Row that contains minimum characters
which(nchar(emails$text) == min(nchar(emails$text)))

#Building document term matrix
corpus <- Corpus(VectorSource(emails$text))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords('english'))
corpus <- tm_map(corpus, stemDocument)
corpus <- tm_map(corpus, PlainTextDocument)
dtm <- DocumentTermMatrix(corpus)

#Create spdtm that contains words that appear atleast in 5% of documents
#: ~264
spdtm <- removeSparseTerms(dtm, 0.95)
spdtm

emailSparse <- as.data.frame(as.matrix(spdtm))
colnames(emailSparse) <- make.names(colnames(emailSparse))

#Finding the most frequent word
which.max(colSums(emailSparse))

#Adding spam to our existing data frame
emailSparse$spam <- emails$spam

#Finding number of words that appear more than 5,000 times in the ham
length(which(colSums(emailSparse[emailSparse$spam == 0,]) >5000))

#Stem words that appear atleast 1000 times in the spam emails
length(which(colSums(emailSparse[emailSparse$spam == 1,])>999))

set.seed(123)
emailSparse$spam <- as.factor(emailSparse$spam)
espl <- sample.split(emailSparse$spam, 0.7)
train <- subset(emailSparse, espl == TRUE)
test <- subset(emailSparse, espl == FALSE)

#training models on the 3 machine learning models

spamLog <- glm(spam ~., data =train, family ='binomial')
mytrainpredLog <- predict(spamLog, type = 'response')

spamCART <- rpart(spam~., data = train, method ='class')
mytrainpredCART <- predict(spamCART)[,2]

spamRF <- randomForest(spam~., data = train)
mytrainpredRF <- predict(spamRF, type ='prob')[,2]

#Finding probabilites that are less than 'x'
table(mytrainpredLog<0.00001)
table(mytrainpredLog >0.99999) #or reverse the previous results
table(mytrainpredLog >0.00001 & mytrainpredLog<= 0.99999)

#Finding variables that are significant in the logistic regression 
summary(spamLog) #Inference Zero

prp(spamCART)

#Checking accuracy of logistic model: ~0.965
table(train$spam, mytrainpredLog >0.5)

#Training set AUC of spamLog
newPredLog <- prediction(mytrainpredLog, train$spam)
as.numeric(performance(newPredLog, 'auc')@y.values)

table(train$spam, mytrainpredCART >0.5)

newPredCART <- prediction(mytrainpredCART, train$spam )
as.numeric(performance(newPredCART,'auc')@y.values)


table(train$spam, mytrainpredRF >0.5)
newPredRF <- prediction(mytrainpredRF, train$spam)
as.numeric(performance(newPredRF, 'auc')@y.values)

#Evaluating the test set
myPred_log <- predict(spamLog, newdata = test, type = 'response')
myPred_cart <- predict(spamCART, newdata =test)[,2]
myPred_rf <- predict(spamRF, newdata= test, type='prob')[,2]

table(test$spam, myPred_log>0.5)
predictionTestLog <- prediction(myPred_log, test$spam)
as.numeric(performance(predictionTestLog, 'auc')@y.values)

table(test$spam, myPred_cart >0.5)
predictionTestcart <- prediction(myPred_cart, test$spam)
as.numeric(performance(predictionTestcart, 'auc')@y.values)

table(test$spam, myPred_rf >0.5)
predictionTestrf <- prediction(myPred_rf, test$spam)
as.numeric(performance(predictionTestrf, 'auc')@y.values)

#Random forest is the most accurate in terms of accuracy and auc
