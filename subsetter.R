#load required libraries, data, and created functions
library(dplyr)
library(tm)
library(tidyr)
library(SnowballC)
library(RWeka)
source("Functions.R")
setwd("/Users/andersswanson/Desktop/comment\ mining")

t1.report.path <-"data/t2 comments.csv"
t2.report.path <-"data/t1 comments.csv"

t1.report <- ReportsDFfromMBcsv(t1.report.path)
t2.report <- ReportsDFfromMBcsv(t2.report.path)

#read in reports csv and functions
t1.report <- read.csv(report1.path,
                    stringsAsFactors = FALSE,
                    encoding = "UTF-8")
#rip Student.Comment column, turn it into a corpus and clean it 
all.corpus <- reports$Student.Comment %>%
  ToCorpus() %>%
  CorpusClean()

#tag each "document" (i.e. comment) in corpus using report information
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



