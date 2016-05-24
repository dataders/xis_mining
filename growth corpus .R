#set wd and paths and source functions
source("Functions.R")
setwd("/Users/andersswanson/Desktop/comment\ mining")

#load MB reports
t1.report <- GetReportsDFfromMBcsv("data/t1 comments.csv")
t2.report <- GetReportsDFfromMBcsv("data/t2 comments.csv")
t1.corpus <- GetCorpusFromReportDF(t1.report)
t1.corpus.student <- GetGroupCorpusfromCommentCorpus(t1.corpus, "student")

#finding subset comments where student made improvement
by.cols <- c("Student.ID", "Last.Name", "First.Name",
             "Grade.Level", "Subject", "Teacher")
t12.report <- merge(t1.report, t2.report,
                    by = by.cols, suffixes = c(".t1", ".t2")) %>%
        mutate(class.growth = CriMean.t2 - CriMean.t1) %>%
        within(class.growth.quartile <- as.integer(cut(class.growth,
                                                       quantile(class.growth, probs=0:4/4,
                                                                na.rm = TRUE),
                                                       include.lowest=TRUE)))


#add ID.SUB column
t12.report <- t12.report %>% mutate(ID.SUB = paste(Student.ID, Subject))

#get 4 quartiles of ID.SUB's 
quarts <- c(1,2,3,4)
quartiles <- lapply(quarts, function(x) {
        t12.report %>% filter(class.growth.quartile == x) %>%
                .$ID.SUB
})

#paste each quartile's comments into one comment
quartile.comments <- lapply(quartiles, function(x) {
        idx <- t1.corpus %>% meta(tag = "ID.SUB") %in% x
        do.call(paste,content(t1.corpus[idx]))
})


#make corpus (1 quartile = 1 document)
quartile.corpus <- VectorSource(quartile.comments) %>% Corpus

#make dtc from corpus
quart.dtm <- GetDocumentTermMatrix(quartile.corpus, 2,5, norm = TRUE)

#take quartiles get freq list and sorted by LenNorm
idx <- 4
indv.ngrams <- quart.dtm[idx,] %>% CollapseAndSortDTM 
indv.ngrams <- indv.ngrams %>%
        mutate(length = CountWords(Words)) %>%
        mutate(LenNorm = length * freq) %>%
        arrange(desc(LenNorm))


works <- GetPrunedList(indv.ngrams, 100)
