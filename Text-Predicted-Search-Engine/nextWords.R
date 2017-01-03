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
        words[[1]] <- head(freqTable[[1]]$term,5)
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
    wordListProbs
}