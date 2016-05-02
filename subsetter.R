#load required libraries, data, and created functions
library(dplyr)
library(tm)
library(tidyr)
library(SnowballC)
library(RWeka)
source("Functions.R")
setwd("/Users/andersswanson/Desktop/comment\ mining")

t1.report.path <-"data/t1 comments.csv"
t2.report.path <-"data/t2 comments.csv"

t1.report <- ReportsDFfromMBcsv(t1.report.path)
t2.report <- ReportsDFfromMBcsv(t2.report.path)

t12.report <- c(t1.report,t2.report)

paths <- c(t1.report.path, t2.report.path)

reports <- lapply(paths,ReportsDFfromMBcsv)

corpora <- lapply(reports, CorpusFromReportDF)

t12.corpus <- c(corpora, corpora)

#test to check new functions
t1.report.path <-"data/t1 comments.csv"
t1.report <- ReportsDFfromMBcsv(t1.report.path)
t1.corpus <- CorpusFromReportDF(t1.report)

t2.report.path <-"data/t2 comments.csv"
t2.report <- ReportsDFfromMBcsv(t2.report.path)
t2.corpus <- CorpusFromReportDF(t2.report)

t12.corpus <- c(t1.corpus,t2.corpus)

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

Comp2All(t1.corpus, "teacher", "Anders Swanson",2,2)


#strip student names from each comment
student.names <- reports$First.Name %>%
  stripWhitespace() %>%
  removePunctuation() %>%
  tolower()



