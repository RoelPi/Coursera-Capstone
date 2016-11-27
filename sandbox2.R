createTransitionMatrix <- function(data,skip) {
    data[is.na(data)] <- 0
    data <- data %>% mutate(total=twitter+blog+news)
    data <- subset(data,total>1)
    splitWords <- str_split_fixed(as.character(data$word)," ",10)[,1:skip]
    minGrams <- data.frame(paste(splitWords[,1],splitWords[,2]," "))
    data <- cbind(minGrams,data$word,data$total)
    colnames(data) <- c('mingrams','word','count')
    data <- arrange(data,desc(count))
    data <- dcast(data,mingrams~word,value.var='count')
    data[is.na(data)] <- 0
    data <- sapply(data[,2:ncol(data)],function(x) x/rowSums(data[2:ncol(data)]))
    data
}
head(createTransitionMatrix(full3GCount,2))