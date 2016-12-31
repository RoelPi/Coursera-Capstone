# This function loads the saved n-gram frequency tables into the RAM and prepares the data so it can be used in a Kneser-Ney prediction model.
# The function loads n-gram freq tables from the /batches/ folder and needs to be preceded by saveFreqTable.
# Source: http://u.cs.biu.ac.il/~yogo/courses/mt2013/papers/chen-goodman-99.pdf
prepareFreqTable <- function() {
    freqTable <- list()
    preCalc <- function() {
        freqTable <- list()
        for (j in 1:4) {
            n <- j
            freqTable[[j]] <- readRDS(paste0("batches/",j,"gram.Rda"))
            
            # Parameter Y
            ns <- list()
            ns[[1]] <- nrow(freqTable[[n]][freq==1])
            ns[[2]] <- nrow(freqTable[[n]][freq==2])
            ns[[3]] <- nrow(freqTable[[n]][freq==3])
            ns[[4]] <- nrow(freqTable[[n]][freq==4])
            Y <- ns[[1]]/(ns[[1]]+2*ns[[2]])
            freqTable[[n]]$Y <- Y
            
            # Parameter D - Modified n1/(n1 + 2*n2)
            D <- list()
            D[[1]] <- 1 - 2 * Y *(ns[[2]]/ns[[1]])
            D[[2]] <- 2 - 3 * Y *(ns[[3]]/ns[[2]])
            D[[3]] <- 3 - 4 * Y *(ns[[4]]/ns[[3]])
            
            if (n == 1) {
                freqTable[[n]]$g <- 0
                freqTable[[n]]$history <- as.character(0)
                freqTable[[n]]$histCount <- sum(freqTable[[n]]$freq)
            }
            freqTable[[n]]$D <- 0
            freqTable[[n]][freq==1]$D <- D[[1]]
            freqTable[[n]][freq==2]$D <- D[[2]]
            freqTable[[n]][freq==3]$D <- D[[3]]
            
            # Temporarily create all the required variables for pkn calculation
            if (n > 1) {
                freqTable[[n]]$history <- sapply(freqTable[[n]]$term,function(x)paste(stri_extract_all_words(x,"\\s+")[1:(n-1)],collapse=" "))
                setkey(freqTable[[n]],history)
                freqTable[[n]]$term <- sapply(freqTable[[n]]$term,function(x)paste(stri_extract_last_words(x)))
                freqTable[[n]] <- freqTable[[n]][,.(term,freq,D,histCount = sum(freq)),by=history]
                freqTable[[n]] <- freqTable[[n]][,.(term,freq,D,histCount,N1 = sum(freq == 1)),by=history]
                freqTable[[n]] <- freqTable[[n]][,.(term,freq,D,histCount,N1,N2 = sum(freq == 2)),by=history]
                freqTable[[n]] <- freqTable[[n]][,.(term,freq,D,histCount,N1,N2,N3 = sum(freq == 3)),by=history]
                freqTable[[n]]$g <- (D[[1]]*freqTable[[n]]$N1+D[[2]]*freqTable[[n]]$N2+D[[3]]*freqTable[[n]]$N3)/freqTable[[n]]$histCount
            }
            rm(D)
            # Calculate probabilities
            freqTable[[n]] <- freqTable[[n]][,pkn := (freq-D)/histCount]
            freqTable[[n]] <- freqTable[[n]][,c("history","term","pkn","g"),with=F]
            
            # Set key for fast filtering & reordering
            setkeyv(freqTable[[n]],c("history","term"))
            setorder(freqTable[[n]],-pkn)
            
            gc()
            message(paste0("Success! ",j,"-gram frequency table with preparation for Kneser-Ney model created."))
        }
        freqTable
    }
    if (file.exists("batches/1gram.Rda") & file.exists("batches/2gram.Rda") & file.exists("batches/3gram.Rda") & file.exists("batches/4gram.Rda")) {
        # Calculate pkn's
        fullFreqTable <- preCalc()
    } else {
        message("No batches exist. Please run saveFreqTable(source,batches) first.")
    }
    fullFreqTable
}