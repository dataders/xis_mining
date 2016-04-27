#load required libraries, data, and created functions
library(dplyr)
library(tm)
library(tidyr)
library(SnowballC)
setwd("~/Dropbox/ds/XIS data")

#read in reports csv and functions
reports <- read.csv("t1 comments 4.csv",
                    stringsAsFactors = FALSE,
                    encoding = "UTF-8")
source("Functions.R")

# get students names as factors
reports2 <- read.csv("t1 comments 4.csv",
                     stringsAsFactors = TRUE,
                     encoding = "UTF-8")
students <- levels(reports2$Student.ID)
students <- students[-1] #complete hack... remove empty first element
rm(reports2)

#create empty list and name each item after name of student
student.comments <- vector("list", length(students))
names(student.comments) <- students

#loop through each student to subset grouped df to concat all comments into one string
for (i in students) {
  sub <- reports %>%
    filter(Student.ID == i)
  student.comments[i] <- sub$Student.Comment %>%
    paste(collapse = " ")
}

# Make a corpus and clean it w/ pre-defined functions
sdt.corpus <- ToCorpus(student.comments) %>%
  CorpusClean()

# change set author tag of indv documents in corpus
for (i in 1:length(students)) {
  meta(sdt.corpus[[i]], tag = "author") <- students[i]
}

CompIndv2All("student", "10001598")
head(comp.freq$Words,10)
comp.freq <- comp.freq %>% arrange(least.typical.dist)
head(comp.freq$Words,10)