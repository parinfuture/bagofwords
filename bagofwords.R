setwd('C:/Users/Parikshit/Documents/popcorn')
library(RTextTools)
library(e1071)
library(plyr)
library(stringr)

data = read.delim("labeledTrainData.tsv", quote="", sep="\t")
testdata = read.delim('testData.tsv', quote="", sep ="\t", stringsAsFactors = FALSE)
str(data)
(data$sentiment[1:50])
View(data)

pvedata = data[which(data$sentiment ==1),]
negdata = data[which(data$sentiment==0),]
str(pvedata)
str(negdata)


affinn_list <- read.delim(file= 'AFINN-111.txt', header = FALSE, stringsAsFactors = FALSE)
names(affinn_list) <- c('word', 'score')
affinn_list$word <-tolower(affinn_list$word)

#Categorizing words
vNegTerms <- affinn_list$word[affinn_list$score == -5 | affinn_list$score == -4]
negTerms <- c(affinn_list$word[affinn_list$score == -3| affinn_list$score == -2| affinn_list ==-1], "second-rate", "moronic", "third-rate", "flawed", "juvenile", "boring", "distasteful", "ordinary", "disgusting", "senseless", "static", "brutal", "confused", "disappointing", "bloody", "silly", "tired", "predictable", "stupid", "uninteresting", "trite", "uneven", "outdated", "dreadful", "bland")
posTerms <- c(affinn_list$word[affinn_list$score ==3 | affinn_list$score== 2| affinn_list$score ==1], "first-rate", "insightful", "clever", "charming", "comical", "charismatic", "enjoyable", "absorbing", "sensitive", "intriguing", "powerful", "pleasant", "surprising", "thought-provoking", "imaginative", "unpretentious")
vPosTerms <- c(affinn_list$word[affinn_list$score == 5| affinn_list$score == 4], "uproarious", "riveting", "fascinating", "dazzling", "legendary")

sentimentScore <- function(sentences, vNegTerms, negTerms, posTerms, vPosTerms){
  final_scores <- matrix('', 0, 5)
  scores <- laply(sentences, function(sentence, vNegTerms, negTerms, posTerms, vPosTerms){
    initial_sentence <- sentence
    #remove unnecessary characters and split up by word 
    sentence <- gsub('[[:punct:]]', '', sentence)
    sentence <- gsub('[[:cntrl:]]', '', sentence)
    sentence <- gsub('\\d+', '', sentence)
    sentence <- tolower(sentence)
    wordList <- str_split(sentence, '\\s+')
    words <- unlist(wordList)
    #build vector with matches between sentence and each category
    vPosMatches <- match(words, vPosTerms)
    posMatches <- match(words, posTerms)
    vNegMatches <- match(words, vNegTerms)
    negMatches <- match(words, negTerms)
    #sum up number of words in each category
    vPosMatches <- sum(!is.na(vPosMatches))
    posMatches <- sum(!is.na(posMatches))
    vNegMatches <- sum(!is.na(vNegMatches))
    negMatches <- sum(!is.na(negMatches))
    score <- c(vNegMatches, negMatches, posMatches, vPosMatches)
    #add row to scores table
    newrow <- c(initial_sentence, score)
    final_scores <- rbind(final_scores, newrow)
    return(final_scores)
  }, vNegTerms, negTerms, posTerms, vPosTerms)
  return(scores)
}    

#build tables of positive and negative sentiments with their scores
posResult <- as.data.frame(sentimentScore(pvedata$review, vNegTerms, negTerms, posTerms, vPosTerms))
negResult <- as.data.frame(sentimentScore(negdata$review, vNegTerms, negTerms, posTerms, vPosTerms))

posResult <- cbind(posResult, 'positive')
colnames(posResult) <- c('sentence', 'vNeg', 'neg', 'pos', 'vPos', 'sentiment')

negResult <- cbind(negResult, 'negative')
colnames(negResult) <- c('sentence', 'vNeg', 'neg', 'pos', 'vPos', 'sentiment')

results <- rbind(posResult, negResult)

library(e1071)

classifi = naiveBayes(sentiment ~., data = results)

testDataSet <- as.data.frame(sentimentScore(testdata$review, vNegTerms, negTerms, posTerms, vPosTerms))
colnames(testDataSet) <- c('sentence', 'vNeg' ,'neg', 'pos', 'vPos')

m <- predict(classifi, data.frame(dt =testDataSet[,2:5]), type = 'raw')

submit <- data.frame(id = testdata$id, 'sentiment'  = m)
