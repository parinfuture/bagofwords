
setwd("/Documents/Wiki")

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
