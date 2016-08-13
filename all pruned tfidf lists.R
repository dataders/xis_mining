#set wd and paths and source functions
source("Functions.R")
setwd("/Users/andersswanson/Desktop/comment\ mining")

#load MB reports
t1.report <- GetReportsDFfromMBcsv("data/t1 comments.csv")
t2.report <- GetReportsDFfromMBcsv("data/t2 comments.csv")
t3.report <- GetReportsDFfromMBcsv("data/t3 comments.csv")

#make a corpus of all the reports for the year
year.corpus <- c(GetCorpusFromReportDF(t1.report), GetCorpusFromReportDF(t2.report),GetCorpusFromReportDF(t3.report))


# Teachers ----------------------------------------------------------------

#get a corpus where each doc is all words written by one teacher
teacher.corpus <- GetGroupCorpusfromCommentCorpus(year.corpus, "teacher")

#make an ngram Tf-Idf matrix for each teacher where 2≤n≤7
system.time(all.tfidf <- GetAllTfIdfMatricesFromGroupedCorpus(teacher.corpus, 2, 7, TRUE))



# Students ----------------------------------------------------------------

#get a corpus where each doc is all words written about one student
student.corpus <- GetGroupCorpusfromCommentCorpus(year.corpus, "student")

#make an ngram Tf-Idf matrix for each student where 2≤n≤7
system.time(
        all.pruned <- GetMemberPrunedList(student.corpus, 2, 7, TRUE, 200)
)

sortable.html.table(all.pruned[["Anders Swanson"]], "figures/anders phrases.html")

lapply()