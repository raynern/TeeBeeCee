#Required packages
library(rjson)
library(tm)
library(dplyr)
library(tidytext)
library(data.table)

setwd("~/Desktop/Datascience/EPIC Datathon 2019")

#Reading json and using a sample
result <- fromJSON(file = "articles.json")
sample <- result[1:10]

#Generating stopwords
stopWords <- stopwords("en")
class(stopWords)

#Processing full text
text_clean <- function(string) {
  workingstring <- (lapply(strsplit(gsub("[^[:alnum:] ]", " ", string), " "), tolower))
  non_stop <- !unlist(workingstring) %in% stopWords
  return(workingstring[[1]][non_stop])
}

text_clean(sample[[1]]$contents)
