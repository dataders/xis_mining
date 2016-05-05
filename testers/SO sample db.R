#load required libraries, data, and created functions
library(dplyr)
library(tm)
source("Functions.R")
setwd("/Users/andersswanson/Desktop/comment\ mining")

reports <- read.csv("testers/sample db.csv")

data.table(reports)

#get student names (remove punctuation and uppercase) and tokenize them


students.raw  <- levels(reports$Student.Name)

students <- levels(reports$Student.Name) %>%
    tolower %>%
    removePunctuation

students.tokenized <- NGramTokenizer(students)

#create doc corpus of student comments
corpus <- collect(select(reports, Comment))[[1]] %>%
        VectorSource %>%
        Corpus

#clean the corpus
corpus <- corpus %>% 
        tm_map(removePunctuation) %>%
        tm_map(stripWhitespace) %>%
        tm_map(content_transformer(tolower), lazy=TRUE) %>%
        tm_map(removeWords, students.tokenized)

#manage metadata for subsetting later
#tag each "document" (i.e. comment) in corpus using report information
for (i in 1:nrow(reports)) {
        meta(corpus[[i]], tag = "teacher") <- reports[i,"Teacher"]
        meta(corpus[[i]], tag = "subject") <- reports[i, "Subject"]
        meta(corpus[[i]], tag = "student name") <- reports[i,"Student.Name"]
}



for (i in students.raw) {
        idx <- meta(corpus, tag = "student name") == i
        corpus[idx]
        }
        
idx <- corpus[meta(corpus, tag = "student name") == "Elizabeth (Bett"]


reports <- structure(list(Teacher = structure(c(1L, 1L, 1L, 3L, 3L, 3L, 
2L, 2L, 2L), .Label = c("Black", "Blue", "Brown"), class = "factor"), 
Subject = structure(c(2L, 2L, 2L, 1L, 1L, 1L, 3L, 3L, 3L), .Label = c("English", 
"Math", "P.E."), class = "factor"), Student.Name = structure(c(3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L), .Label = c("Elizabeth (Betty)", "Mary Grace (MG)", "Richard (Dick)"), class = "factor"), Comment = structure(c(3L, 2L, 1L, 9L, 5L, 7L, 8L, 4L, 6L), .Label = c("As her teacher, I think MG is my favorite.", "Betty procrastinates, but does good work.", "Dick is a terrible student-- why hasn't he been kicked out yet?", "Elizabeth (Betty) needs to work to communicate on the field.", "Elizabeth's work is intefering with her studies.", "Mary Grace (MG) needs to stop insulting the teacher", "Mary Grace should be a teacher someday.", "Richard (Dick) kicked more field goals than any other student.", "Richard is terrible at turning in homework"), class = "factor")), .Names = c("Teacher", "Subject", "Student.Name", "Comment"), class = "data.frame", row.names = c(NA, -9L))

reports <- read.table(
Teacher SubjectStudent.Name                                                         Comment
1   Black    Math    Richard (Dick) Dick is a terrible student-- why hasn't he been kicked out yet?
2   Black    Math Elizabeth (Betty)                       Betty procrastinates, but does good work.
3   Black    Math   Mary Grace (MG)                      As her teacher, I think MG is my favorite.
4   Brown English    Richard (Dick)                      Richard is terrible at turning in homework
5   Brown English Elizabeth (Betty)                Elizabeth's work is intefering with her studies.
6   Brown English   Mary Grace (MG)                         Mary Grace should be a teacher someday.
7    Blue    P.E.    Richard (Dick)  Richard (Dick) kicked more field goals than any other student.
8    Blue    P.E. Elizabeth (Betty)    Elizabeth (Betty) needs to work to communicate on the field.
9    Blue    P.E.   Mary Grace (MG)             Mary Grace (MG) needs to stop insulting the teacher
)
