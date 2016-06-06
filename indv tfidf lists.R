#set wd and paths and source functions
source("Functions.R")
setwd("/Users/andersswanson/Desktop/comment\ mining")

#load MB reports
t1.report <- GetReportsDFfromMBcsv("data/t1 comments.csv")
t2.report <- GetReportsDFfromMBcsv("data/t2 comments.csv")
t3.report <- GetReportsDFfromMBcsv("data/t3 comments.csv")

year.corpus <- c(GetCorpusFromReportDF(t1.report), GetCorpusFromReportDF(t2.report),GetCorpusFromReportDF(t3.report))

#teachers
teacher.corpus <- GetGroupCorpusfromCommentCorpus(year.corpus, "teacher")



#subjects

subject.corpus <- GetGroupCorpusfromCommentCorpus(t12.corpus, "subject")

math.top1k <- GetIndvTfIdfMatrixFromGroupedCorpus(subject.corpus, "Standard mathematics", 2,8, TRUE) %>%
        head(n = 1000) %>%
        select(ngrams = Words, tfidfXlength = LenNorm)
math.pruned <- GetPrunedList(math.top1k, 1000)

english.top1k <- GetIndvTfIdfMatrixFromGroupedCorpus(subject.corpus, "English", 2,8, TRUE) %>%
        head(n = 1000) %>%
        select(ngrams = Words, tfidfXlength = LenNorm)
english.pruned <- GetPrunedList(english.top1k, 1000)

        




