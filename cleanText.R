# This function takes a TermDocumentMatrix or a DocumentTermMatrix and cleans it in the following ways:
# - Transformation to lowercase
# - Remove numbers
# - Remove hashtags
# - Remove URLs
# - Remove punctuation
# - Remove redundant spaces
# - Removal of extra whitespaces
# This function is called by the saveFreqTable function.
cleanText <- function(data) {

    data <- tm_map(data,content_transformer(tolower))
    data <- tm_map(data,content_transformer(removeNumbers))
    data <- tm_map(data, function(x) gsub("#\\S+", "", x))
    data <- tm_map(data, function(x) gsub("http\\S+", "", x))
    data <- tm_map(data, function(x) gsub("[?!,.]", ".", x))
    data <- tm_map(data, function(x) gsub("[^[:alnum:][:space:].]", "", x))
    data <- tm_map(data, function(x) gsub(" \\.", ".", x))
    data <- tm_map(data, function(x) gsub("\\. ", ".", x))
    data <- tm_map(data, function(x) strsplit(x, "\\."))
    data <- tm_map(data, PlainTextDocument)
    data <- tm_map(data, stripWhitespace)
    data <- tm_map(data,content_transformer(removePunctuation))
}