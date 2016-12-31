# General settings
options(scipen=999)
set.seed(1999)
library(tm)
library(SnowballC)
library(data.table)
library(dplyr)
library(slam)
library(reshape2)
library(scales)
library(stringi)
library(stringr)

# Load necessary functions
source("cleanText.R")
source("wordGram.R")
source("frequencyTable.R")
source("saveFreqTable.R")
source("prepareFreqTable.R")

# Load all source data
if (!exists("dSource")) dSource <- suppressWarnings(suppressMessages(list(
    twitter=readLines('dataset/final/en_US/en_US.twitter.txt'),
    news=readLines('dataset/final/en_US/en_US.news.txt'),
    blog=readLines('dataset/final/en_US/en_US.blogs.txt'))))

# Take a sample
dSourceSample <- lapply(dSource,function(x) sample(x,length(x)*0.02))

# Prepare Kneser-Ney Model
# saveFreqTable(dSourceSample,10)
# ft <- prepareFreqTable()

# Next word model - Pass a list of n-gram frequency tables prepared for the Kneser-Ney model (prepareFreqTable) and a sentence for next word prediction
nextWords <- function(freqTable,history = "i am going to",words=list()) {
    history <- gsub(pattern="'",replacement="",x=history)
    n <- ifelse(str_count(history," ") >=2,4,str_count(history," ")+1)
    hist <- list()
    hist[[1]] <- 0
    for (i in 2:length(freqTable)) {
        hist[[i]] <- paste0(tail(unlist(strsplit(history," ")),(i-1)),collapse=" ")
    }
    if (length(words) == 0) {
        words <- list()
        words[[1]] <- head(freqTable[[1]]$term,15)
        for (i in 2:length(freqTable)) {
            if (n > (i-1)) { words[[i]] <- head(freqTable[[i]][history %like% paste0("^",hist[[i]],"$")]$term,5) }
        }
    }
    wordList<-unique(unlist(words))
    wordList
    getProbability <- function(word) {
        pkn <- list()
        g <- list()
        for (i in 1:length(freqTable)) {
            row <- freqTable[[i]][history == hist[[i]] & term == word]
            
            pkn[[i]] <- ifelse(nrow(row) > 0,row$pkn,0)
            g[[i]] <- ifelse(nrow(row) > 0,row$g,1)
        }
        p <- pkn[[4]]+g[[4]]*(pkn[[3]]+g[[3]]*(pkn[[2]]+g[[2]]*pkn[[1]]))
        p
    }
    wordListProbs <- data.table(term=wordList,prob=sapply(wordList,function(x) getProbability(x)))
    wordListProbs <- wordListProbs[order(-prob),]
    print(wordListProbs)
}

# Test
quizOne <- function(ft) {
    quizAnswers <- list()
    quizAnswers[[1]] <- nextWords(ft,"The guy in front of me just bought a pound of bacon, a bouquet, and a case of",words=list("beer","cheese","pretzels","soda"))
    quizAnswers[[2]] <- nextWords(ft,"You're the reason why I smile everyday. Can you follow me please? It would mean the",words=list("universe","most","world","best"))
    quizAnswers[[3]] <- nextWords(ft,"Hey sunshine, can you follow me and make me the",words=list("bluest","smelliest","saddest","happiest"))
    quizAnswers[[4]] <- nextWords(ft,"Very early observations on the Bills game: Offense still struggling but the",words=list("crowd","defense","players","referees"))
    quizAnswers[[5]] <- nextWords(ft,"Go on a romantic date at the",words=list("beach","movies","groceries","mall"))
    quizAnswers[[6]] <- nextWords(ft,"Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my",words=list("motorcycle","horse","phone","way"))
    quizAnswers[[7]] <- nextWords(ft,"Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some",words=list("years","thing","time","weeks"))
    quizAnswers[[8]] <- nextWords(ft,"After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little",words=list("toes","fingers","ears","eyes"))
    quizAnswers[[9]] <- nextWords(ft,"Be grateful for the good times and keep the faith during the",words=list("hard","bad","sad","worse"))
    quizAnswers[[10]] <- nextWords(ft,"If this isn't the cutest thing you've ever seen, then you must be",words=list("insensitive","asleep","callous","insane"))
    quizAnswers
}
quizTwo <- function(ft) {
    quizAnswers <- list()
    quizAnswers[[1]] <- nextWords(ft,"When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd",words=list("eat","die","sleep","give"))
    quizAnswers[[2]] <- nextWords(ft,"Guy at my table's wife got up to go to the bathroom and I asked about dessert and he started telling me about his",words=list("marital","spiritual","horticultural","financial"))
    quizAnswers[[3]] <- nextWords(ft,"I'd give anything to see arctic monkeys this",words=list("month","decade","weekend","morning"))
    quizAnswers[[4]] <- nextWords(ft,"Talking to your mom has the same effect as a hug and helps reduce your",words=list("happiness","sleepiness","stress","hunger"))
    quizAnswers[[5]] <- nextWords(ft,"When you were in Holland you were like 1 inch away from me but you hadn't time to take a",words=list("look","minute","picture","walk"))
    quizAnswers[[6]] <- nextWords(ft,"I'd just like all of these questions answered, a presentation of evidence, and a jury to settle the",words=list("case","matter","incident","account"))
    quizAnswers[[7]] <- nextWords(ft,"I can't deal with unsymetrical things. I can't even hold an uneven number of bags of groceries in each",words=list("toe","arm","hand","finger"))
    quizAnswers[[8]] <- nextWords(ft,"Every inch of you is perfect from the bottom to the",words=list("top","center","middle","side"))
    quizAnswers[[9]] <- nextWords(ft,"I’m thankful my childhood was filled with imagination and bruises from playing",words=list("inside","daily","weekly","outside"))
    quizAnswers[[10]] <- nextWords(ft,"I like how the same people are in almost all of Adam Sandler's",words=list("novels","movies","pictures","stories"))
    quizAnswers
}