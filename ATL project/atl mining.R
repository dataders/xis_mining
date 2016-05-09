setwd("/Users/andersswanson/Desktop/comment\ mining")
source("ATL\ Project/atl data.R")
source("Functions.R")

#get corpus of T1 and T2 comments
t1.report.path <-"data/t1 comments.csv"
t1.report <- GetReportsDFfromMBcsv(t1.report.path)
t1.corpus <- GetCorpusFromReportDF(t1.report)

t2.report.path <-"data/t2 comments.csv"
t2.report <- GetReportsDFfromMBcsv(t2.report.path)
t2.corpus <- GetCorpusFromReportDF(t2.report)

t12.corpus <- c(t1.corpus,t2.corpus)

# rm(t1.corpus, t2.corpus, t1.report, t2.report, t1.report.path,t2.report.path)

#Get corpus of ATL Clusters
atl.corpus <- GetCorpusFromClusters(atls.vector)
atl.corpus2 <- GetCorpusFromClusters_mod(atls.vector)

#possible weighting functions
tfidf <- function(x) {
        weightTfIdf(x, normalize = TRUE)
}

#make weighted TDM, extract top 10 words for each cluster (i.e. document) 
cluster.top10 <- TermDocumentMatrix(atl.corpus, control=list(
        weighting = tfidf)) %>%
        topn(10)

cluster.top10.words <- setNames(vector(mode = "list", length = length(cluster.top10)),
                                names(cluster.top10))
for (i in 1:length(cluster.top10)) {
        cluster.top10.words[[i]] <- names(cluster.top10[[i]])
}



GetDictTotalsfromCorpus <- function(corpus, dict) {
        ref <- DocumentTermMatrix(corpus, list(dictionary = dict))
        sums <- ref %>% as.matrix %>% colSums
        sums
}


cluster.names.stemmed <- c("communic", "collabor", "organ", "affect", "reflect", "inform", 
  "media", "critic", "creativ", "transfer")


GetDictTotalsfromCorpus(t12.corpus, cluster.names.stemmed)

atl.freq.totals <- GetDictTotalsfromCorpus(t12.corpus, cluster.names.stemmed) %>%
        sort(decreasing = TRUE)

names(atl.freq.totals) <- stemCompletion(names(atl.freq.totals), cluster.names, type = "prevalent")

cluster.top10.words

