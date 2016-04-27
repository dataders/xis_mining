#load required libraries, data, and created functions
library(dplyr)
library(tm)
library(tidyr)
library(SnowballC)
library(RWeka)
#library(quanteda)
source("Functions.R")
setwd("~/Dropbox/ds/XIS data")
report.path <- "t1 comments 4.csv"


#read in reports csv and functions
reports <- read.csv(report.path,
                    stringsAsFactors = FALSE,
                    encoding = "UTF-8")

all.corpus <- reports$Student.Comment %>%
  ToCorpus() %>%
  CorpusClean()

for (i in 1:nrow(reports)) {
  meta(all.corpus[[i]], tag = "teacher") <- reports$Teacher[i]
  meta(all.corpus[[i]], tag = "student ID") <- reports$Student.ID[i]
  meta(all.corpus[[i]], tag = "subject") <- reports$Subject[i]
  meta(all.corpus[[i]], tag = "grade") <- reports$Grade.Level[i]
}

#strip student names from each comment
student.names <- reports$First.Name %>%
  stripWhitespace() %>%
  removePunctuation() %>%
  tolower()

all.corpus <- all.corpus %>%
  tm_map(removeWords, student.names)

for (i in length(all.corpus)) {
  removeWords(all.corpus[i],)
}

Comp2All("teacher","Anders Swanson")
Comp2All("student ID","10001901")

