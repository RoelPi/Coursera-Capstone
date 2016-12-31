# Function to generate a frequency table, input of documentTermMatrix, output of data table ordered by decreasing frequency.
# This function is called by the saveFreqTable function.
frequencyTable <- function(dtm) {
    dtmFreq <- data.table(term=row.names(dtm),freq=slam::row_sums(dtm))
    dtmFreq$term <- as.character(dtmFreq$term)
    dtmFreq <- dtmFreq[order(dtmFreq$freq,decreasing=T),]
    dtmFreq
}