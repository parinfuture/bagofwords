
setwd("C:/Users/Parikshit/Documents/Wiki")

#Reading the file from source
wiki <- read.csv('wiki.csv', stringsAsFactors = FALSE)
str(wiki)
head(wiki)

#Calculating the total number of times the article has been vandalised
sum(wiki$Vandal)

library(tm)

#Making a corpus
doc.vec <- VectorSource(wiki$Added)
corpusAdded <- Corpus(doc.vec)


#Removing stopwords, stem 
corpusAdded <- tm_map(corpusAdded, removeWords, stopwords('english'))
corpusAdded <- tm_map(corpusAdded, stemDocument)
dtmAdded <- DocumentTermMatrix(corpusAdded)

#Number of terms in Document Term Matrix
dtmAdded

#Removing sparse terms
sparseAdded <- removeSparseTerms(dtmAdded, 0.997)
sparseAdded

#Creating data frame
wordsAdded <- as.data.frame(as.matrix(sparseAdded))

colnames(wordsAdded) <- paste("A", colnames(wordsAdded))

#Corpus removed
corpusRemoved = Corpus(VectorSource(wiki$Removed))
corpusRemoved = tm_map(corpusRemoved, removeWords, stopwords("english"))
corpusRemoved = tm_map(corpusRemoved, stemDocument)
dtmRemoved = DocumentTermMatrix(corpusRemoved)
sparseRemoved = removeSparseTerms(dtmRemoved, 0.997)
wordsRemoved = as.data.frame(as.matrix(sparseRemoved))
colnames(wordsRemoved) = paste("R", colnames(wordsRemoved))
str(wordsRemoved)

#Combine the two df into single data frame
wikiWords <- cbind(wordsAdded, wordsRemoved)
str(wikiWords)

wikiWords$Vandal <- wiki$Vandal
library(caTools)
set.seed(111)
spl = sample.split(wikiWords$Vandal, 0.7)
spl

wikiTrain = subset(wikiWords, spl == TRUE)
wikiTest = subset(wikiWords, spl == FALSE)

#Accuracy on the test set of a baseline method
table(wikiTest$Vandal)

library(rpart)
mdl <- rpart(Vandal ~., data = wikiTrain, method = 'class')

#Predictions
my_pred <- predict(mdl, newdata = wikiTest, type = 'class')

#Accuracy of predictions
table(wikiTest$Vandal, my_pred)
#~0.54

#Plot your tree
plot(mdl)
text(mdl)

#Accuracy was found to be low, so we need to move on to another approach

wikiWords2 <- wikiWords
wikiWords2$HTTP <- ifelse(grepl('http', wiki$Added, fixed = TRUE),1,0)

#How many revisions added a link
sum(wikiWords2$HTTP)

#New training and testing sets
wikiTrain2 <- subset(wikiWords2, spl == TRUE)
wikiTest2 <- subset(wikiWords2, spl == FALSE)

#Generating a CART model based on the new sets
mdl2 <- rpart(Vandal ~., wikiTrain2, method = 'class')

my_pred2 <- predict(mdl2, newdata = wikiTest2, type = 'class')

#Check accuracy: ~0.58
table(wikiTest2$Vandal, my_pred2)

#Average words added: ~4.05
wikiWords2$NumWordsAdded <- rowSums(as.matrix(dtmAdded))
wikiWords2$NumWordsRemoved <- rowSums(as.matrix(dtmRemoved))
mean(wikiWords2$NumWordsAdded)

#Making new sets for Model 3 and developing model for wikiWords2

wikiTrain3 <- subset(wikiWords2, spl == TRUE)
wikiTest3 <- subset(wikiWords2, spl == FALSE)

mdl3 <- rpart(Vandal ~., wikiTrain3, method = 'class')
my_pred3 <- predict(mdl3, newdata = wikiTest3, type = 'class')

#Accuracy for model 3: ~ 0.66
table(wikiTest3$Vandal, my_pred3)

#Including new metadata into our model
wikiWords3 = wikiWords2
wikiWords3$Minor = wiki$Minor
wikiWords3$Loggedin = wiki$Loggedin

wikiTrain4 <- subset(wikiWords3, spl == TRUE)
wikiTest4 <- subset(wikiWords3, spl == FALSE)

mdl4 <- rpart(Vandal ~., wikiTrain4, method = 'class')
my_pred4 <- predict(mdl4, newdata = wikiTest4, type = 'class')

#Accuracy check: ~0.72
table(wikiTest4$Vandal, my_pred4)

#plot tree for our fourth model
plot(mdl4)
text(mdl4)



