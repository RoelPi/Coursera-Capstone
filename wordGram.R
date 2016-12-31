# Function to generate an n-gram with removel of terms with a sparsity of 0.98
# This function is called by the saveFreqTable function.
wordGram <- function(C,n=1) {
    # Documentation: http://tm.r-forge.r-project.org/faq.html
    BigramTokenizer <-
        function(x)
            unlist(lapply(ngrams(words(x), n), paste, collapse = " "), use.names = FALSE)
    
    tdm <- TermDocumentMatrix(C, control = list(tokenize = BigramTokenizer))
    removeSparseTerms(tdm,0.98)
    tdm
}