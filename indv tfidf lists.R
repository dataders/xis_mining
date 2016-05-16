#set wd and paths and source functions
source("Functions.R")
setwd("/Users/andersswanson/Desktop/comment\ mining")

#load MB reports
t1.report <- GetReportsDFfromMBcsv("data/t1 comments.csv")
t2.report <- GetReportsDFfromMBcsv("data/t2 comments.csv")

t1.corpus <- GetCorpusFromReportDF(t1.report)
t2.corpus <- GetCorpusFromReportDF(t2.report)

t12.corpus <- c(GetCorpusFromReportDF(t1.report), GetCorpusFromReportDF(t2.report))

