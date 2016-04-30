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

paths <- c(t1.report.path, t2.report.path)

reports <- lapply(paths,ReportsDFfromMBcsv)

corpora <- lapply(reports, CorpusFromReportDF)


#test to check new functions
t1.report.path <-"data/t1 comments.csv"
t1.report <- ReportsDFfromMBcsv(t1.report.path)
t1.corpus <- CorpusFromReportDF(t1.report)

idx <- meta(t1.corpus, "teacher") == "Anders Swanson"
t1.corpus[!idx]



indv.dtm <- t1.corpus[idx] %>%
        DocumentTermMatrix(control=list(tokenize = DersTokenizer)) %>%
        as.matrix

Comp2All(t1.corpus, "teacher", "Anders Swanson",2,2)


#strip student names from each comment
student.names <- reports$First.Name %>%
  stripWhitespace() %>%
  removePunctuation() %>%
  tolower()



