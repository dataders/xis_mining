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
system.time(all.tfidf <- GetAllTfIdfMatricesFromGroupedCorpus(teacher.corpus, 2, 7, TRUE))


#students
student.corpus <- GetGroupCorpusfromCommentCorpus(year.corpus, "student")

system.time(
        all.pruned <- GetMemberPrunedList(student.corpus, 2, 7, TRUE, 200)
)

sortable.html.table(all.pruned[["Anders Swanson"]], "figures/anders phrases.html")

lapply()