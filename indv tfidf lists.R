#set wd and paths and source functions
source("Functions.R")
setwd("/Users/andersswanson/Desktop/comment\ mining")

#load MB reports
t1.report <- GetReportsDFfromMBcsv("data/t1 comments.csv")
t2.report <- GetReportsDFfromMBcsv("data/t2 comments.csv")

t12.corpus <- c(GetCorpusFromReportDF(t1.report), GetCorpusFromReportDF(t2.report))

teacher.corpus <- GetGroupCorpusfromCommentCorpus(t12.corpus, "teacher")
student.corpus <- GetGroupCorpusfromCommentCorpus(t12.corpus, "student ID")


anders.top100 <- GetIndvTfIdfMatrixFromGroupedCorpus(teacher.corpus, "Anders Swanson", 2, 5, TRUE) %>%
        head(n = 100)

hong.top100 <- GetIndvTfIdfMatrixFromGroupedCorpus(student.corpus, "10001901", 2, 5, TRUE) %>%
        head(n = 100)

