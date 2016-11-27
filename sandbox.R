set.seed(03031988)
library(tm)
library(SnowballC)
message("1. Seed & Packages set")

suppressWarnings(suppressMessages(
    twitterData <- readLines('dataset/final/en_US/en_US.twitter.txt')))
suppressWarnings(suppressMessages(
    newsData <- readLines('dataset/final/en_US/en_US.news.txt')))
suppressWarnings(suppressMessages(
    blogData <- readLines('dataset/final/en_US/en_US.blogs.txt')))
message("2. Large Datasets loaded")

twitterSample <- sample(twitterData,5000)
newsSample <- sample(newsData,1000)
blogSample <- sample(blogData,1000)
message("3. Samples generated")

twitterSampleCorpus <- Corpus(VectorSource(twitterSample))
newsSampleCorpus <- Corpus(VectorSource(newsSample))
blogSampleCorpus <- Corpus(VectorSource(blogSample))
message("4. Corpuses created")

cleanText <- function(data) {
    # Removal of extra whitespaces
    data <- tm_map(data,stripWhitespace)
    # Stemming
    data <- tm_map(data,stemDocument)
    # Transformation to lowercapse
    data <- tm_map(data,content_transformer(tolower))
}

twitterSampleCorpus <- cleanText(twitterSampleCorpus)
newsSampleCorpus <- cleanText(newsSampleCorpus)
blogSampleCorpus <- cleanText(blogSampleCorpus)
message("5. Text cleaned")

wordGram <- function(C,n=1) {
    
    # http://tm.r-forge.r-project.org/faq.html
    BigramTokenizer <-
        function(x)
            unlist(lapply(ngrams(words(x), n), paste, collapse = " "), use.names = FALSE)
    
    tdm <- TermDocumentMatrix(C, control = list(tokenize = BigramTokenizer))
}

twitterSampleDTM <- wordGram(twitterSampleCorpus,1)
newsSampleDTM <- wordGram(newsSampleCorpus,1)
blogSampleDTM <- wordGram(blogSampleCorpus,1)
message("6. Wordgrams generated")

wordCountMatrix <- function(data,colname) {
    data <- data.frame(sort(rowSums(as.matrix(data)),decreasing=T))
    data <- cbind(rownames(data),data)
    colnames(data) <- c('word',colname)
    data
}
twitterSampleWordCount <- wordCountMatrix(twitterSampleDTM,'Twitter')
newsSampleWordCount <- wordCountMatrix(newsSampleDTM,'News')
blogSampleWordCount <- wordCountMatrix(blogSampleDTM,'Blog')
message("7. Wordcountmatrices generated")

stops <- data.frame(stopwords())
colnames(stops) <- 'word'

mergeStopWords <- function(data,stopwordsset = stops) {
    data <- merge(x=stopwordsset,y=data,by.x='word',by.y='word',all.x=TRUE,all.y=FALSE)
}

stopWordsWordCount <- mergeStopWords(twitterSampleWordCount)
stopWordsWordCount <- mergeStopWords(newsSampleWordCount,stopWordsWordCount)
stopWordsWordCount <- mergeStopWords(blogSampleWordCount,stopWordsWordCount)
stopWordsWordCount <- stopWordsWordCount[order(-stopWordsWordCount$Twitter),]

stopWordsWordCountShare <- stopWordsWordCount
stopWordsWordCountShare$word <- stopWordsWordCount$word
stopWordsWordCountShare$Twitter <- stopWordsWordCountShare$Twitter / sum(twitterSampleWordCount$Twitter)
stopWordsWordCountShare$News <- stopWordsWordCountShare$News / sum(newsSampleWordCount$News)
stopWordsWordCountShare$Blog <- stopWordsWordCountShare$Blog / sum(blogSampleWordCount$Blog)

twitterSampleCorpus <- tm_map(twitterSampleCorpus, removeWords, stopwords("english"))
newsSampleCorpus <- tm_map(newsSampleCorpus, removeWords, stopwords("english"))
blogSampleCorpus <- tm_map(blogSampleCorpus, removeWords, stopwords("english"))

twitterSampleCorpus <- tm_map(twitterSampleCorpus, removePunctuation, preserve_intra_word_dashes = TRUE)
newsSampleCorpus <- tm_map(newsSampleCorpus, removePunctuation, preserve_intra_word_dashes = TRUE)
blogSampleCorpus <- tm_map(blogSampleCorpus, removePunctuation, preserve_intra_word_dashes = TRUE)

twitterSample1Gram <- wordGram(twitterSampleCorpus,1)
twitterSample2Gram <- wordGram(twitterSampleCorpus,2)
twitterSample3Gram <- wordGram(twitterSampleCorpus,3)

newsSample1Gram <- wordGram(newsSampleCorpus,1)
newsSample2Gram <- wordGram(newsSampleCorpus,2)
newsSample3Gram <- wordGram(newsSampleCorpus,3)

blogSample1Gram <- wordGram(blogSampleCorpus,1)
blogSample2Gram <- wordGram(blogSampleCorpus,2)
blogSample3Gram <- wordGram(blogSampleCorpus,3)

twitter1GCount <- wordCountMatrix(twitterSample1Gram,'count')
twitter2GCount <- wordCountMatrix(twitterSample2Gram,'count')
twitter3GCount <- wordCountMatrix(twitterSample3Gram,'count')

news1GCount <- wordCountMatrix(newsSample1Gram,'count')
news2GCount <- wordCountMatrix(newsSample2Gram,'count')
news3GCount <- wordCountMatrix(newsSample3Gram,'count')

blog1GCount <- wordCountMatrix(blogSample1Gram,'count')
blog2GCount <- wordCountMatrix(blogSample2Gram,'count')
blog3GCount <- wordCountMatrix(blogSample3Gram,'count')

full1GCount <- Reduce(function(...) merge(...,by="word",all=TRUE),list(blog1GCount,news1GCount,twitter1GCount))
colnames(full1GCount) <- c('word','blog','news','twitter')
full2GCount <- Reduce(function(...) merge(...,by="word",all=TRUE),list(blog2GCount,news2GCount,twitter2GCount))
colnames(full2GCount) <- c('word','blog','news','twitter')
full3GCount <- Reduce(function(...) merge(...,by="word",all=TRUE),list(blog3GCount,news3GCount,twitter3GCount))
colnames(full3GCount) <- c('word','blog','news','twitter')


createTransitionMatrix <- function(data,skip) {
    data <- data %>% mutate(total=twitter+blog+news)
    splitWords <- str_split_fixed(as.character(data)," ",2)[,1]
    minGrams <- paste(splitWords[,1:skip]," ")
    data <- cbind(minGrams,data$word,data$total)
    colnames(data) <- c('mingrams','word','count')
    data <- dcast(data,mingrams~word,sum)
    data
}
head(createTransitionMatrix(full3GCount,2))