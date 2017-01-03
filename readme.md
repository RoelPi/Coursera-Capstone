
# The Coursera/John Hopkins University Data Science Specialization Capstone
This Specialization covers the concepts and tools you'll need throughout the entire data science pipeline, from asking the right kinds of questions to making inferences and publishing results. In the final Capstone Project, you’ll apply the skills learned by building a data product using real-world data. At completion, students will have a portfolio demonstrating their mastery of the material. [Source|https://www.coursera.org/specializations/jhu-data-science]

Around the world, people are spending an increasing amount of time on their mobile devices for email, social networking, banking and a whole range of other activities. But typing on mobile devices can be a serious pain. SwiftKey, our corporate partner in this capstone, builds a smart keyboard that makes it easier for people to type on their mobile devices. One cornerstone of their smart keyboard is predictive text models. When someone types:

> I went to the

the keyboard presents three options for what the next word might be. For example, the three words might be gym, store, restaurant. In this capstone you will work on understanding and building predictive text models like those used by SwiftKey.

This course will start with the basics, analyzing a large corpus of text documents to discover the structure in the data and how words are put together. It will cover cleaning and analyzing text data, then building and sampling from a predictive text model. Finally, you will use the knowledge you gained in data products to build a predictive text product you can show off to your family, friends, and potential employers.

## loadData.R
This file downloads and unzips the text files that are provided for the Capstone project.

## saveFreqTable.R
This function takes a list of text data and a numeric that determines in how many files the list is split. Splitting the files is done for RAM conservation. Each batch is a piece of a cleaned n-gram frequency table. In the end, all batches are put back together and saved into a large R object. This function preceeds the prepareFreqTable function in preparing the **Kneser-Ney model**. This function calls on the wordGram and the frequencyTable function to generate n-grams and corresponding frequency tables. The cleanText function is used to perform cleaning transformations on the n-grams.

## prepareFreqTable.R
This function loads the saved n-gram frequency tables into the RAM and prepares the data so it can be used in a Kneser-Ney prediction model. The function loads n-gram freq tables from the /batches/ folder and needs to be preceded by saveFreqTable. Documentation can be found in the the wonderful overview paper "[An empirical study of smoothing techniques for language modeling|http://u.cs.biu.ac.il/~yogo/courses/mt2013/papers/chen-goodman-99.pdf]" (1999) by Chen & Goodman.

## model.R
This file calls on all the relevant functions to generate a Kneser-Ney probability table that needs to be passed to the nextWords function in the nextWords.R file. While it is technically possible to use the complete dataset for prediction, a small sample is used for speed & user experience purposes.

## nextWords.R
This is the function that should be used by the end user to predict the next word of a provided string.


