#load required libraries, data, and created functions
library(dplyr)
library(tm)
library(tidyr)
library(RWeka)
source("Functions.R")
setwd("/Users/andersswanson/Desktop/comment\ mining")

reports <- read.csv("testers/sample db.csv")

#get student names (remove punctuation and uppercase) and tokenize them
students <- levels(reports$Student.Name) %>%
        tolower %>%
        removePunctuation
students.tokenized <- lapply(students, NGramTokenizer)

#create doc corpus of student comments
corpus <- collect(select(reports, Comment))[[1]] %>%
        VectorSource %>%
        Corpus

#clean the corpus
corpus <- corpus %>% 
        #tm_map(removeWords, student.names) %>%
        tm_map(removePunctuation) %>%
        tm_map(stripWhitespace) %>%
        tm_map(content_transformer(tolower), lazy=TRUE)

#manage metadata for subsetting later
#tag each "document" (i.e. comment) in corpus using report information
for (i in 1:nrow(reports)) {
        meta(corpus[[i]], tag = "teacher") <- reports[i,"Teacher"]
        meta(corpus[[i]], tag = "subject") <- reports[i, "Subject"]
        meta(corpus[[i]], tag = "student name") <- reports[i,"Student.Name"]
}
