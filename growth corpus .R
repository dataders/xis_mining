#set wd and paths and source functions
source("Functions.R")
setwd("/Users/andersswanson/Desktop/comment\ mining")

#load MB reports
t1.report <- GetReportsDFfromMBcsv("data/t1 comments.csv")
t1.corpus <- GetCorpusFromReportDF(t1.report)
t1.corpus.student <- GetGroupCorpusfromCommentCorpus(t1.corpus, "student")




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
quart.dtm <- GetDocumentTermMatrix(quartile.corpus, 2,2, norm = TRUE)

#take quartiles get freq list and sorted by LenNorm
idx <- 3:4
indv.ngrams <- quart.dtm[idx,] %>% CollapseAndSortDTM 
indv.ngrams <- indv.ngrams %>%
        mutate(length = CountWords(Words)) %>%
        mutate(LenNorm = length * freq) %>%
        arrange(desc(LenNorm))
