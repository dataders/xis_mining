
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
#source("MB - t12 growth.R")


# B) t1 Comments & Corpus ---------------------------------------

t1.report <- GetReportsDFfromMBcsv("data/t1 comments.csv") %>% 
        AnonymizeReport()
t1.corpus <- GetCorpusFromReportDF(t1.report)
t1.corpus.student <- GetGroupCorpusfromCommentCorpus(t1.corpus, "student")


# C) Year Report w/ centered mean ------------------------------------------


#wrappers for mean and sd with na.rm = TRUE
av <- function(x) {mean(x, na.rm = TRUE)}
s <- function(x) {sd(x, na.rm = TRUE)}

year.report <- GetYearReport() %>%
        select(Student.ID, Class.ID:Teacher, CriMean.t1, CriMean.t2, t12.growth) %>%
        group_by(Class.ID) %>%
        mutate(t12.growth.center = round(t12.growth - av(t12.growth), 22)) %>%
        ungroup() %>%
        within(t12.growth.center.quartile <- as.integer(cut(t12.growth.center,
                                                      quantile(t12.growth.center, probs=0:4/4,
                                                               na.rm = TRUE),
                                                      include.lowest=TRUE))) %>%
        mutate(ID.SUB = paste(Student.ID, Subject))


# D) quartile Corpus ------------------------------------------------------------

#get 4 quartiles of ID.SUB's 
quarts <- c(1,2,3,4)
quartiles <- lapply(quarts, function(x) {
        year.report %>% filter(t12.growth.center.quartile == x) %>%
                .$ID.SUB
})

#paste each quartile's comments into one comment
quartile.comments <- lapply(quartiles, function(x) {
        idx <- t1.corpus %>% meta(tag = "ID.SUB") %in% x
        do.call(paste,content(t1.corpus[idx]))
})

top.bottom.comments <- quartile.comments[-c(2,3)]

#make corpus (1 quartile = 1 document)
quartile.corpus <- VectorSource(quartile.comments) %>% Corpus


# E) Analysis -------------------------------------------------------------

#make dtc from corpus
all.tfidf <- GetAllTfIdfMatricesFromCorpus(quartile.corpus, 1,3, norm = TRUE)

all.pruned <- lapply(all.tfidf, GetPrunedList, prune_thru = 100)


test <- cbind(unlist(sapply(1:5, function(x) {all.pruned[[x]]}), recursive = FALSE))


test <- lapply(1:4, function(x) {
        tmp <- all.pruned[[x]]
        tmp[1:20,"ngrams"]
                          })

test2 <- do.call(cbind, test)