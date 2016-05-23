#set wd and paths and source functions
source("Functions.R")
setwd("/Users/andersswanson/Desktop/comment\ mining")

#load MB reports
t1.report <- GetReportsDFfromMBcsv("data/t1 comments.csv")
t1.corpus <- GetCorpusFromReportDF(t1.report)
t1.corpus.student <- GetGroupCorpusfromCommentCorpus(t1.corpus, "student")


idx <- t1.corpus %>% meta(tag = "ID.SUB") %in% quartiles[[1]]
t1.corpus[idx]

t12.report <- t12.report %>% mutate(ID.SUB = paste(Student.ID, Subject))

quarts <- c(1,2,3,4)
quartiles <- lapply(quarts, function(x) {
        t12.report %>% filter(class.growth.quartile == x) %>%
                .$ID.SUB
})

member.comments <- lapply(quartiles, function(x) {
        idx <- t1.corpus %>% meta(tag = "ID.SUB") %in% x
        do.call(paste,content(t1.corpus[idx]))
})



member.corpus <- VectorSource(member.comments) %>% Corpus

quart.dtm <- GetDocumentTermMatrix(member.corpus, 2,2, norm = TRUE)


indv.ngrams <- quart.dtm[3:4,] %>% CollapseAndSortDTM 
indv.ngrams <- indv.ngrams %>%
        mutate(length = CountWords(Words)) %>%
        mutate(LenNorm = length * freq) %>%
        arrange(desc(LenNorm))


t1.dtm <- GetDocumentTermMatrix(t1.corpus, 2,2, norm = TRUE)

indv.ngrams <- quart.dtm[3:4,] %>% CollapseAndSortDTM 
indv.ngrams <- indv.ngrams %>%
        mutate(length = CountWords(Words)) %>%
        mutate(LenNorm = length * freq) %>%
        arrange(desc(LenNorm))