setwd('/Documents/Clinical')
library(tm)
library(caTools)
library(rpart)
library(rpart.plot)
library(ROCR)


#Reading file
trials <- read.csv('clinical_trial.csv', stringsAsFactors = FALSE)
str(trials)

#Finding maximum numbers of character
max(nchar(trials$abstract))

#Finding number of abstract with zero characters
a <- nchar(trials$abstract)
length(a[a == 0])

#Finding shortest title
which(nchar(trials$title) == min(nchar(trials$title)))
trials$title[1258]

#Building Corpera; corpusTitle and corpusAbstract
corpusTitle <- Corpus(VectorSource(trials$title))
corpusTitle <- tm_map(corpusTitle, tolower)
corpusTitle <- tm_map(corpusTitle, PlainTextDocument)
corpusTitle <- tm_map(corpusTitle, removePunctuation)
corpusTitle <- tm_map(corpusTitle, removeWords, stopwords('english'))
corpusTitle <- tm_map(corpusTitle, stemDocument)

corpusAbstract <- Corpus(VectorSource(trials$abstract))
corpusAbstract <- tm_map(corpusAbstract, tolower)
corpusAbstract <- tm_map(corpusAbstract, PlainTextDocument)
corpusAbstract <- tm_map(corpusAbstract, removePunctuation)
corpusAbstract <- tm_map(corpusAbstract, removeWords, stopwords('english'))
corpusAbstract <- tm_map(corpusAbstract, stemDocument)

#Building document term matrix for corpusTitle and corpusAbstract
dtmTitle <- DocumentTermMatrix(corpusTitle)
dtmAbstract <- DocumentTermMatrix(corpusAbstract)

#Limitng sparseness to 95%
dtmTitle <- removeSparseTerms(dtmTitle, 0.95)
dtmAbstract <- removeSparseTerms(dtmAbstract, 0.95)

#Converting to data frames
dtmTitle <- as.data.frame(as.matrix(dtmTitle))
dtmAbstract <- as.data.frame(as.matrix(dtmAbstract))

#Number of items after removing sparse terms
ncol(dtmTitle)
ncol(dtmAbstract)

#Finding the most frequent words: 'breast' in dtmTitle and 'patient' in dtmAbstract
which(colSums(dtmTitle) ==max(colSums(dtmTitle)))
which(colSums(dtmAbstract) == max(colSums(dtmAbstract)))


#Adding "T" to variable names
colnames(dtmTitle) <- paste0('T', colnames(dtmTitle))
colnames(dtmAbstract) <- paste0('T', colnames(dtmAbstract))

dtm <- cbind(dtmTitle, dtmAbstract)
dtm$trial <- trials$trial

#Splitting our dataset
spl = sample.split(dtm$trial, 0.7)
train <- subset(dtm, spl == TRUE)
test <- subset(dtm, spl == FALSE)

#Building a model
trialCart <- rpart(trial~., data = train, method ='class')

prp(trialCart)

#Training set predictions
predTrain <- predict(trialCart)[,2]
table(train$trial, predTrain >=0.5)

#Evaluating model on test set
predTest <- predict(trialCart, newdata = test)[,2]

table(test$trial, predTest>=0.5)

#Using ROCR, calculating performace: ~0.81
pred = prediction(predTest, test$trial)
as.numeric(performance(pred, 'auc')@y.values)
