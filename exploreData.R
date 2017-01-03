set.seed(03031988)
library(tm)

#Load data if it does not exist
if (!exists("twitterData")) {
    suppressWarnings(suppressMessages(
        twitterData <- readLines('dataset/final/en_US/en_US.twitter.txt')
    ))
}
if (!exists("newsData")) {
    suppressWarnings(suppressMessages(
        newsData <- readLines('dataset/final/en_US/en_US.news.txt')
    ))
}
if (!exists("blogData")) {
    suppressWarnings(suppressMessages(
        blogData <- readLines('dataset/final/en_US/en_US.blogs.txt')
    ))
}

# Create sample for testing purposes
testData <- sample(twitterData,2000)

# Create Corpus
testCorpus <- Corpus(VectorSource(testData))

# Removal of extra whitespaces
testCorpus <- tm_map(testCorpus,stripWhitespace)

# Stemming
testCorpus <- tm_map(testCorpus,stemDocument)

# Lowercase
testCorpus <- tm_map(testCorpus,content_transformer(tolower))


# Generate TDM with X-gram terms
wordGram <- function(C = testCorpus,n=1) {
    
    # http://tm.r-forge.r-project.org/faq.html
    BigramTokenizer <-
        function(x)
            unlist(lapply(ngrams(words(x), n), paste, collapse = " "), use.names = FALSE)
    
    tdm <- TermDocumentMatrix(C, control = list(tokenize = BigramTokenizer))
}

test <- wordGram(testCorpus)
wordCountMatrix <- function(data,colname) {
    data <- data.frame(sort(rowSums(as.matrix(data)),decreasing=T))
    data <- cbind(rownames(data),data)
    colnames(data) <- c('word',colname)
    data
}

testSampleWordCount <- wordCountMatrix(test,'test')

stops <- data.frame(stopwords())
colnames(stops) <- 'word'

mergeStopWords <- function(data,stopwordsset = stops) {
    data <- merge(x=stopwordsset,y=data,by.x='word',by.y='word',all.x=TRUE,all.y=FALSE)
}

stopWordsWordCount <- mergeStopWords(testSampleWordCount)


head(stopWordsWordCount)






