library(dplyr)
library(tm)
#library(tidyr)
#library(RWeka)

data(crude)
crude.corpus <- VCorpus(VectorSource(crude)) 


GetFreqTablefromCorpus <- function(corpus, norm.bool) {
        # Sets the default number of threads to use
        options(mc.cores=1)
        
        #tokenizes corpus into ngrams(n=1:4)
        MonoQuadTokenizer <- function(x) {
                NGramTokenizer(x, Weka_control(min = 1, max = 4))
        } 
        #calculates tf*idf score
        tfidf <- function(x) {
                weightTfIdf(x, normalize = norm.bool)
        }        
        a.dtm <- DocumentTermMatrix(corpus, control=list(
                tokenize = MonoQuadTokenizer,
                weighting = tfidf))
        
        a.freq <- a.dtm %>%
                as.matrix() %>%
                colSums() %>%
                sort(decreasing = TRUE) %>%
                as.data.frame()
        
        a.freq$Words <- row.names(a.freq)
        a.freq <- rename(a.freq, freq = .)
}

freq <- GetFreqTablefromCorpus(crude.corpus,FALSE)
freq.normed <- GetFreqTablefromCorpus(crude.corpus,TRUE)
crude.compare <- inner_join(freq, freq.normed, by = "Words")

crude.freq <- crude.dtm %>%
        as.matrix() %>%
        colSums() %>%
        sort(decreasing = TRUE) %>%
        as.data.frame()

crude.freq$Words <- row.names(crude.freq)
crude.freq <- rename(crude.freq, freq = .)

crude.freq <- colSums(crude.dtm)

str(crude.dtm)

crude.compare <- inner_join(crude.freq,crude.freq.norm, by = "Words")
crude.compare <- inner_join(crude.freq,crude.freq.norm)                  