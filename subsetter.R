#load required libraries, data, and created functions
library(dplyr)
library(tm)
library(tidyr)
library(SnowballC)
library(RWeka)

setwd("/Users/andersswanson/Desktop/comment\ mining")
source("Functions.R")

#get corpus of T1 and T2 comments, c() T1 and T2 corpora
t1.corpus <- GetReportsDFfromMBcsv("data/t1 comments.csv") %>%
        GetCorpusFromReportDF
t2.corpus <- GetReportsDFfromMBcsv("data/t2 comments.csv") %>%
        GetCorpusFromReportDF
t12.corpus <- c(t1.corpus,t2.corpus)
rm(t1.corpus, t2.corpus)

#test
idx <- meta(t12.corpus, "teacher") == "Anders Swanson"
t1.corpus[idx]

DersTokenizer <- function(x) {
        NGramTokenizer(x, Weka_control(min = 3, max = 3))
}

indv.dtm <- t1.corpus[idx] %>%
        DocumentTermMatrix %>%
        as.matrix

indv.dtm <- t1.corpus[idx] %>%
        DocumentTermMatrix(control=list(tokenize = DersTokenizer)) %>%
        as.matrix

Comp2All(t12.corpus, "teacher", "Anders Swanson",2,2)


#strip student names from each comment
student.names <- reports$First.Name %>%
  stripWhitespace() %>%
  removePunctuation() %>%
  tolower()



