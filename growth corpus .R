
# TO FIX ------------------------------------------------------------------

#'
#'  1) year report has to use name-scrubbed t1 comments, so...
#'         2) scrubber has to output in same format as t1.report
#'  B) take only the first and last n-tiles and compare them
#'  C) collect stats on improvement for write-up

# - OUTLINE ---------------------------------------------------------------

#' STRUCTURE OF PROGRAM
#' A) Get 


# A) Source ---------------------------------------------------------------

#set wd and paths and source functions
setwd("/Users/andersswanson/Desktop/comment\ mining")
source("Functions.R")
source("MB - t12 growth.R")


# B) t1 Comments & Corpus ---------------------------------------

t1.report <- GetReportsDFfromMBcsv("data/t1 comments.csv") %>% AnonymizeReport()
t1.corpus <- GetCorpusFromReportDF(t1.report)
t1.corpus.student <- GetGroupCorpusfromCommentCorpus(t1.corpus, "student")


# C) Year Report w/ z-score ------------------------------------------

# get year report with added z-scored t12.growth score appended
year.report.byclass <- GetT12StdGrowthfromYearReport() %>%
        #make quintiles of t12.zgrowth variable
        within(t12.zgrowth.quintile <- as.integer(cut(t12.zgrowth,
                                                       quantile(t12.zgrowth, probs=0:5/5,
                                                                na.rm = TRUE),
                                                       include.lowest=TRUE))) %>%
        #add ID.SUB column (for cross-ref w/ t1 corpus)
        mutate(ID.SUB = paste(Student.ID, Subject))


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