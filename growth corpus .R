#set wd and paths and source functions
source("Functions.R")
setwd("/Users/andersswanson/Desktop/comment\ mining")

#load MB reports
t1.report <- GetReportsDFfromMBcsv("data/t1 comments.csv")
t1.corpus <- GetCorpusFromReportDF(t1.report)
t1.corpus.student <- GetGroupCorpusfromCommentCorpus(t1.corpus, "student")




