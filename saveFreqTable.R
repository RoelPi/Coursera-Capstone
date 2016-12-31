# This function takes a list of text data and a numeric that determines in how many files the list is split. 
# Splitting the files is done for RAM conservation.
# Each batch is a piece of a cleaned n-gram frequency table.
# In the end, all batches are put back together and saved into a large R object.
# This function preceeds the prepareFreqTable function in preparing the Kneser-Ney model.
saveFreqTable <- function(sourceList,batches) {
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
        tdm <<- wordGram(cleanCorpus,1)
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