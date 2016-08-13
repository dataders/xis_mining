#load required libraries, data, and created functions
library(dplyr)
library(tm)
library(tidyr)
library(reshape2)
library(SnowballC)
library(RWeka)
library(xlsx)

setwd("~/Dropbox/ds/XIS data")
source("Functions.R")
report.path <- "t1 comments 4.csv"


#read in reports csv and functions
reports <- read.csv(report.path,
                    stringsAsFactors = FALSE,
                    encoding = "UTF-8")

teacher.comments <- GetComments("teachers")

# Make a corpus and clean it w/ pre-defined functions
tch.corpus <- ToCorpus(teacher.comments)
tch.corpus <- CorpusClean(tch.corpus)


#define controls for DocumentTermMatrix
tfidf <- function(x) {weightTfIdf(x, normalize = FALSE)}
DersTokenizer <- function(x) {NGramTokenizer(x, Weka_control(min = 3, max = 3))}

#make document term matrix
a.dtm <- DocumentTermMatrix(tch.corpus, control=list(
        tokenize = DersTokenizer,
        weighting = tfidf))
a.mat <- a.dtm[,] %>% as.matrix

a.freq <- a.mat %>%
        colSums() %>%
        sort(decreasing = TRUE) %>%
        as.matrix() %>%
        as.data.frame()
a.freq$Words <- row.names(a.freq)
a.freq <- rename(a.freq, freq = V1)
View(a.freq)

