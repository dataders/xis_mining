GetPrunedList <- function(wordfreqdf, prune_thru = 100) {
        #take only first n items in list
        tmp <- head(wordfreqdf, n = prune_thru) %>%
                select(ngrams = Words, tfidfXlength = LenNorm)
        #for each ngram in list:
        t <- (lapply(1:nrow(tmp), function(x) {
                #find overlap between ngram and all items in list (overlap = TRUE)
                idx <- overlap(tmp[x, "ngrams"], tmp$ngrams)
                #set overlap as false for itself and higher-scoring ngrams
                idx[1:x] <- FALSE
                idx
        }))
        
        #bind each ngram's overlap vector together to make a matrix
        t2 <- do.call(cbind, t)   
        
        #find rows(i.e. ngrams) that do not overlap with those below
        idx <- rowSums(t2) == 0
        pruned <- tmp[idx,]
        rownames(pruned) <- NULL
        pruned
}

GetPrunedList(all.tfidf[[1]])

GetPrunedListsFromAllTfidfMatrices <- function(all.tfidf, prune_thru) {
        list.pruned <- lapply(all.tfidf, function(x) {
                x %>%
                        select(ngrams = Words, tfidfXlength = LenNorm) %>%
                        GetPrunedList(prune_thru)
        })
}

all.pruned <- GetPrunedListsFromAllTfidfMatrices(all.tfidf, prune_thru = 100)


all.pruned <- lapply(all.tfidf, GetPrunedList, prune_thru = 200)

