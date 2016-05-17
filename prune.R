library(stringi)


ngrams_sorted <- anders.top1000$Words

compare2top <- function(ngram) {
        top <- "the question what can i do to become a more efficient and effective learner"
        overlap(ngram, top)
}
thing <- sapply(ngrams_sorted, compare2top)
        
