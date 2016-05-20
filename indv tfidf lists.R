#set wd and paths and source functions
source("Functions.R")
setwd("/Users/andersswanson/Desktop/comment\ mining")

#load MB reports
t1.report <- GetReportsDFfromMBcsv("data/t1 comments.csv")
t2.report <- GetReportsDFfromMBcsv("data/t2 comments.csv")

t12.corpus <- c(GetCorpusFromReportDF(t1.report), GetCorpusFromReportDF(t2.report))

subject.corpus <- GetGroupCorpusfromCommentCorpus(t12.corpus, "subject")

science.top1k <- GetIndvTfIdfMatrixFromGroupedCorpus(subject.corpus, "Sciences", 2,5, FALSE) %>%
        head(n = 1000)
science.top30 <- science.top1k %>%
        slice(1:30) %>% 
        select(ngrams = Words, tfidfXlength = LenNorm)

write.csv(science.top30, "stack_overflow/example2.csv")

#make list of scores with ngrams as name
ngram_rankings <- setNames(example$tfidfXlength, example$ngrams)
#get list of ngrams
ngrams_preprune <- names(ngram_rankings)
#pre-fill so_far list since 1st-ranked ngram is automatically in list  
ngrams <- ngrams_preprune[-(1)]
so_far <- ngrams_preprune[1]

#loop through all ngrams (except for the 1st)
lapply(ngrams, function(x) {
        #loop through all "keeper" ngrams
        lapply(so_far, function(y) {
                #check for overlap
                ifelse(overlap(x,y),
                       {
                               #if overlap
                               print("OVERLAP!")
                               ngram_rankings[x] <<-0
                       },
                       {
                               #if no overlap
                               print("no overlap!")
                               #THIS ISN'T POSSIBLE, RIGHT?
                               so_far <<- append(so_far, x)
                       })
        })
})


