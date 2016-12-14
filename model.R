# General settings
options(scipen=999)
set.seed(1789)
library(tm)
library(SnowballC)
library(data.table)
library(dplyr)
library(slam)
library(reshape2)
library(scales)
library(stringi)
library(stringr)

# Function to clean the tdm
cleanText <- function(data) {
    # Removal of extra whitespaces
    data <- tm_map(data,stripWhitespace)
    # Transformation to lowercapse
    data <- tm_map(data,content_transformer(tolower))
    # Remove punctuation
    data <- tm_map(data,content_transformer(removePunctuation))
    # Remove numbers
    data <- tm_map(data,content_transformer(removeNumbers))           
}
# Function to generate an n-gram
wordGram <- function(C,n=1) {
    # Documentation: http://tm.r-forge.r-project.org/faq.html
    BigramTokenizer <-
        function(x)
            unlist(lapply(ngrams(words(x), n), paste, collapse = " "), use.names = FALSE)
    
    tdm <- TermDocumentMatrix(C, control = list(tokenize = BigramTokenizer))
    tdm
}
# Function to generate a frequency table
frequencyTable <- function(dtm) {
    dtmFreq <- data.table(term=row.names(dtm),freq=slam::row_sums(dtm))
    dtmFreq$term <- as.character(dtmFreq$term)
    dtmFreq <- dtmFreq[order(dtmFreq$freq,decreasing=T),]
    dtmFreq
}

# Load all source data
# if (!exists("dSource")) dSource <- suppressWarnings(suppressMessages(list(
#                twitter=readLines('dataset/final/en_US/en_US.twitter.txt'),
#                news=readLines('dataset/final/en_US/en_US.news.txt'),
#                blog=readLines('dataset/final/en_US/en_US.blogs.txt'))))

# Sample for testing purposes
dSourceSample <- lapply(dSource,function(x) sample(x,length(x)*0.5))
saveBatchedFreqTable <- function(sourceList,batches) {
    gc()
    ptm <- proc.time()[1]
    ptmAvg <- c()
    sources <- c()
    fullFreqTable <- list()
    # Create necessary directories for temp files
    suppressWarnings(suppressMessages(dir.create(paste0(getwd(),"/batches"))))
    suppressWarnings(suppressMessages(dir.create(paste0(getwd(),"/batches/1gram"))))
    suppressWarnings(suppressMessages(dir.create(paste0(getwd(),"/batches/2gram"))))
    suppressWarnings(suppressMessages(dir.create(paste0(getwd(),"/batches/3gram"))))
    suppressWarnings(suppressMessages(dir.create(paste0(getwd(),"/batches/4gram"))))
    
    # Aggregate all sources
    for (i in 1:length(sourceList)) {
        sources <- c(sources,sourceList[[i]])
    }
    
    # Randomize order of all documents
    sources <- sample(sources)
    
    # Calculate size of batches
    batchSize <- length(sources)/batches
    
    for (i in 1:batches) {
        # Create batch
        ptmBatch <- proc.time()[1]
        batchStart <- (i-1)*batchSize+1
        batchEnd <- i*batchSize
        batch <- sources[batchStart:batchEnd]
        
        # Create corpus
        dCorpus <- Corpus(VectorSource(batch))
        
        # Clean corpus
        cleanCorpus <- cleanText(dCorpus)
        
        # Create n-grams
        d1Gram <- wordGram(cleanCorpus,1)
        d2Gram <- wordGram(cleanCorpus,2)
        d3Gram <- wordGram(cleanCorpus,3)
        d4Gram <- wordGram(cleanCorpus,4)
        allGrams <- list(d1Gram,d2Gram,d3Gram,d4Gram)
        
        # Create frequency table
        freqTable <- lapply(allGrams, function(x) frequencyTable(x))
        
        # Save frequency tables to temporary files
        for (j in 1:length(freqTable)) {
            saveRDS(freqTable[[j]],file=paste0(getwd(),"/batches/",(j),"gram/batch",i,".Rda"))
        }
        
        ptmAvg <- c(ptmAvg,proc.time()[1] - ptmBatch)
        message(paste0("Batch ",i," of ",batches," saved as temporary file. This batch took ",round(proc.time()[1] - ptmBatch,2)," sec to process. Total time elapsed: ",round(proc.time()[1] - ptm,2)," secs. Total batching time estimated: ",round(mean(ptmAvg),2)*batches," sec."))
    }
    # Merge all temporary files to one big frequency table
    message("Merging all temporary files. Please be patient.")
    for (j in 1:4) {
        ngramTempNames <- paste0(getwd(),"/batches/",j,"gram/batch",seq(1,batches),".Rda")
        ngramBatches <- lapply(ngramTempNames,function (x) readRDS(x))
        fullFreqTable <- rbindlist(ngramBatches)
        fullFreqTable <- fullFreqTable[,.(freq=sum(freq)),by=term]

        saveRDS(fullFreqTable,file=paste0(getwd(),"/batches/",j,"gram.Rda"))
        rm(fullFreqTable)
        gc()
        message(paste0("Success! ",j,"-gram saved."))
    }
    message(paste0("Completed. Time elapsed: ",round(proc.time()[1] - ptm,2)," sec."))
}
prepareFreqTable <- function() {
    fullFreqTable <- list()
    splitTerms <- function() {
        fft <- list()
        for (j in 1:4) {
            fft[[j]] <- readRDS(paste0("batches/",j,"gram.Rda"))
            
            # Split n-grams into different columns for each word
            # fft[[j]] <- fft[[j]][,paste0('word',seq(1:(j+1))) :=tstrsplit(term,' ',fixed=T)]
            # if (j != 1) {
            #     patt = sprintf("\\w+( \\w+){0,%d}$", 0)
            #     fft[[j]] <- fft[[j]][,first := gsub("\\s*\\w*$", "", term)]
            #     fft[[j]] <- fft[[j]][,last := stri_extract_last_words(term)]
            #     fft[[j]] <- fft[[j]][,c('term'):=NULL]
            #     fft[[j]] <- setcolorder(fft[[j]],c('first','last','freq'))
            # }
            gc()
            message(paste0("Success! ",j,"-gram frequency table created & ready for action!"))
        }
        fft
    }
    if (file.exists("batches/1gram.Rda") & file.exists("batches/2gram.Rda") & file.exists("batches/3gram.Rda") & file.exists("batches/4gram.Rda")) {
        fullFreqTable <- splitTerms()
    } else {
        message("No batches exist.")
    }
    fullFreqTable
}


# Next word model
nextWords <- function(freqTable,history = "going to") {
    # Parameter 1 - frequency - Term Count
    # Parameter 2 - D - Modified n1/(n1 + 2*n2)
    # Parameter 3 - historyCount - Count of history
    # Parameter 4 - gamma - D*Nn/historyCount for each D & Nn with Nn as the count of unique words following the history that have a frequency > n
    n <- ifelse(str_count(history," ") >=3,4,str_count(history," ")+1)
    
    # Parameter 2
    ns <- list()
    ns[[1]] <- nrow(freqTable[[n]][freq==1,])
    ns[[2]] <- nrow(freqTable[[n]][freq==2,])
    ns[[3]] <- nrow(freqTable[[n]][freq==3,])
    ns[[4]] <- nrow(freqTable[[n]][freq==4,])
    
    Y <- ns[[1]]/(ns[[1]]+2*ns[[2]])
    D <- list()
    D[[1]] <- 1 - 2 * Y *(ns[[2]]/ns[[1]])
    D[[2]] <- 1 - 2 * Y *(ns[[3]]/ns[[2]])
    D[[3]] <- 1 - 2 * Y *(ns[[4]]/ns[[3]])
    
    # Parameter 3
    history <- paste(tail(unlist(strsplit(history," ")),n-1),collapse=" ")
    ftHistoryOnly <- freqTable[[n]][grep(paste0("^",history," "),freqTable[[n]]$term),]
    historyCount <- sum(ftHistoryOnly$freq)
    lastWords <- stri_extract_last_words(ftHistoryOnly$term)
    
    
    lastWordsLower <- c()
    if (n >= 3) { 
        historyLower <- paste(tail(unlist(strsplit(history," ")),n-2),collapse=" ") 
        ftHistoryOnlyLower <- freqTable[[n-1]][grep(paste0("^",historyLower," "),freqTable[[n-1]]$term),]
        historyLowerCount <- sum(ftHistoryOnlyLower$freq)
        lastWordsLower <- stri_extract_last_words(ftHistoryOnlyLower$term)
    }
    
    Encoding(ftHistoryOnly$term) <- "latin1"
    Encoding(lastWords) <- "latin1"
    Encoding(ftHistoryOnlyLower$term) <- "latin1"
    Encoding(lastWordsLower) <- "latin1"
    
    wordList <- unique(c(lastWords,lastWordsLower))
    
    # Parameter 4
    Nn <- list()
    Nn[[1]] <- length(ftHistoryOnly[freq==1,]$term)
    Nn[[2]] <- length(ftHistoryOnly[freq==2,]$term)
    Nn[[3]] <- length(ftHistoryOnly[freq==3,]$term)
    
    if (length(ftHistoryOnly$term) == 0) {
        g <- 1
    } else {
            g <- (D[[1]]*Nn[[1]]+D[[2]]*Nn[[2]]+D[[3]]*Nn[[3]])/historyCount
    }
    
    getProbability <- function(word) {
        #Parameter 1
        frequency <- ftHistoryOnly[term==paste(history,word,sep=" "),]$freq
        if (length(frequency)==0) {
            frequency <- 0
            D <- 0
        }
        if (frequency == 1) { D <- D[[1]] }
        if (frequency == 2) { D <- D[[2]] }
        if (frequency >= 3) { D <- D[[3]] }
        
        result <- as.numeric((frequency - D)/historyCount)
        if(is.nan(result)) {
            0
        } else {
            result
        }
    }
    getLowerProbability <- function(word) {
        #Parameter 1
        frequency <- ftHistoryOnlyLower[term==paste(historyLower,word,sep=" "),]$freq
        if (length(frequency)==0) {
            frequency <- 0
            D <- 0
        }
        if (frequency == 1) { D <- D[[1]] }
        if (frequency == 2) { D <- D[[2]] }
        if (frequency >= 3) { D <- D[[3]] }
        
        result <- as.numeric((frequency - D * frequency)/historyLowerCount)
        if(is.nan(result)) {
            0
        } else {
            result
        }
    }
    freqs <- sapply(wordList,function(x) getProbability(x)+g*getLowerProbability(x))
    freqs <- data.table(cbind(names(freqs),freqs))
    colnames(freqs) <- c("term","freq")
    freqs <- freqs[order(-freq),]
    freqs
}


loadSampleFreqTable <- function(sourceList = dSource,sampleSize = 0.01) {
    ptm <- proc.time()[1]
    
    # Create sample
    message(paste0("Creating samples. Time elapsed: ",round((proc.time()[1] - ptm),2)," sec."))
    dSample <- lapply(sourceList,function(x) sample(x,length(x)*sampleSize))
    for (i in 1:length(dSample)) {
        dSamples <- c()
        dSamples <- c(dSamples,dSample[i])
    }
    # Create corpus
    message(paste0("Making corpuses. Time elapsed: ",round((proc.time()[1] - ptm),2)," sec."))
    dCorpus <- Corpus(VectorSource(dSamples))               
    
    # Clean the corpus
    message(paste0("Cleaning text. Time elapsed: ",round((proc.time()[1] - ptm),2)," sec."))
    if (!exists("cleanCorpus")) cleanCorpus <- cleanText(dCorpus)
    
    # Make n-Grams
    message(paste0("Making 2-grams. Time elapsed: ",round((proc.time()[1] - ptm),2)," sec."))
    if (!exists("d2Gram")) d2Gram <- wordGram(cleanCorpus,2)
    message(paste0("Making 3-grams. Time elapsed: ",round((proc.time()[1] - ptm),2)," sec."))
    if (!exists("d3Gram")) d3Gram <- wordGram(cleanCorpus,3)
    message(paste0("Making 4-grams. Time elapsed: ",round((proc.time()[1] - ptm),2)," sec."))
    if (!exists("d4Gram")) d4Gram <- wordGram(cleanCorpus,4)
    
    message(paste0("Building Model. Time elapsed: ",round((proc.time()[1] - ptm),2)," sec."))
    freqTable <- list()
    for (i in 1:length(list(d2Gram,d3Gram,d4Gram))) {
        # Create frequency table
        freqTable[[i]] <- frequencyTable(l[[i]])
    }
    
    freqTable <- buildModel()
    
    message(paste0("Finished. Time elapsed: ",round((proc.time()[1] - ptm),2)," sec."))
    freqTable
}
