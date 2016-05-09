setwd("/Users/andersswanson/Desktop/comment\ mining")
source("ATL\ Project/atl data.R")
source("Functions.R")

#get corpus of T1 and T2 comments, c() T1 and T2 corpora
t1.corpus <- GetReportsDFfromMBcsv("data/t1 comments.csv") %>%
        GetCorpusFromReportDF
t2.corpus <- GetReportsDFfromMBcsv("data/t2 comments.csv") %>%
        GetCorpusFromReportDF
t12.corpus <- c(t1.corpus,t2.corpus)
rm(t1.corpus, t2.corpus)

#Get corpus of ATL Clusters
atl.corpus <- GetCorpusFromClusters(atls.vector)
atl.corpus2 <- GetCorpusFromClusters_mod(atls.vector)

#count occurences from the operative word from each ATL cluster then complete stem
atl.freq.totals <- GetDictTotalsfromCorpus(t12.corpus, cluster.names.stemmed) %>%
        sort(decreasing = TRUE)

names(atl.freq.totals) <- stemCompletion(names(atl.freq.totals), cluster.names, type = "prevalent")
atl.freq.totals

#get top 10 ngrams for each skill cluster, then search for their occurence 
DersTokenizer <- function(x) {NGramTokenizer(x, Weka_control(min = 1, max = 2))}

# Sets the default number of threads to use
options(mc.cores=1)

#make weighted TDM, extract top 10 words for each cluster (i.e. document) 
cluster.top10 <- TermDocumentMatrix(atl.corpus, control=list(
        weighting = tfidf,
        tokenize = DersTokenizer)) %>%
        topn(10)

cluster.top10.words <- setNames(vector(mode = "list", length = length(cluster.top10)),
                                names(cluster.top10))
for (i in 1:length(cluster.top10)) {
        cluster.top10.words[[i]] <- names(cluster.top10[[i]])
}


# kill anything ending in "and" "or" "of" "with"
run.ons <- c("and", "or", "of", "with")

transfer.phrases <- cluster.top10.words$Transfer
idx <- word(transfer.phrases,-1) %in% run.ons | word(transfer.phrases,1) %in% run.ons
transfer.phrases[!idx]

m <- str_match_all( transfer.phrases, "\\S+" )  # Sequences of non-spaces
length(m[[1]])

m <- str_match_all( c("sube a nacer conmigo hermano","el eterno retorno"), "\\S+" )