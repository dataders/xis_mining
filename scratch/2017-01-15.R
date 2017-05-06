# D) quintile Corpus ------------------------------------------------------------

#get 4 quintiles of ID.SUB's 
quints <- c(1,2,3,4,5)
quintiles <- lapply(quints, function(x) {
        year.report.byclass %>% filter(t12.zgrowth.quintile == x) %>%
                .$ID.SUB
})

#paste each quintile's comments into one comment
quintile.comments <- lapply(quintiles, function(x) {
        idx <- t1.corpus %>% meta(tag = "ID.SUB") %in% x
        do.call(paste,content(t1.corpus[idx]))
})


#make corpus (1 quintile = 1 document)
quintile.corpus <- VectorSource(quintile.comments) %>% Corpus


# E) Analysis -------------------------------------------------------------

#make dtc from corpus
all.tfidf <- GetAllTfIdfMatricesFromCorpus(quintile.corpus, 1,3, norm = TRUE)

all.pruned <- lapply(all.tfidf, GetPrunedList, prune_thru = 200)


test <- cbind(unlist(sapply(1:5, function(x) {all.pruned[[x]]}), recursive = FALSE))


test <- lapply(1:4, function(x) {
        tmp <- all.pruned[[x]]
        tmp[1:20,"ngrams"]
})

test2 <- do.call(cbind, test)