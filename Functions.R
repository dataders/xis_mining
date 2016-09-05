#author = Anders Swanson
# Dependencies ------------------------------------------------------------

dyn.load('/Library/Java/JavaVirtualMachines/jdk1.8.0_101.jdk/Contents/Home/jre/lib/server/libjvm.dylib')


#load required libraries, data, and created functions
library(dplyr)
library(tm)
library(tidyr)
library(RWeka)
library(xlsx)
library(lubridate)
library(ggplot2)
library(stringr)
library(devtools)
#source_gist("524eade46135f6348140")

# ManageBac Reports---------------------------------------------------------


#takes file path of MB reports csv and returns a df w/empty columns removed;
#splits: 1) comment in class and comments split into class and student
#
GetReportsDFfromMBcsv <- function(csv.path) {
        
        #read report in from csv
        df <- read.csv(csv.path,
                       stringsAsFactors = FALSE,
                       encoding = "UTF-8",
                       na.strings = c("N/A", NA))
        
        #filter out:
        #1 all entirely-NA columns
        df <- Filter(function(x) !all(is.na(x)), df)
        #2 rows with empty Teacher of Student.ID 
        idx1 <- nchar(df$Teacher) == 0
        idx2 <- is.na(df$Student.ID)
        df <- df[!(idx1 | idx2),]
        

        
        #parse 2 paragraph comment into "Class-" and "Student-" comment columns
        df <- df %>% separate(Comments,
                                     c("Class.Comment", "Student.Comment"),
                                     sep = "\n", extra = "merge",
                                     remove = FALSE)
        #to avoid Java NullPointer exception, convert NA's to ""
        idx <- is.na(df$Student.Comment)
        df$Student.Comment[idx] <- ""
        
        #separate "Criteria" column by Criterion
        cols <- c("Cri.A","Cri.B","Cri.C","Cri.D")
        df <- df %>% separate(Criteria,
                 cols, sep=", ",
                 remove = TRUE, convert = TRUE)
        
        #convert Criteria columns to integer and fix NA's 
        for (i in 1:length(cols)) {
                #get vectors from df columns
                col.tmp <- collect(select_(df, cols[i]))[[1]]
                
                #index vector elements that are "N/A" and replace them with NA
                idx <- col.tmp == "N/A"
                col.tmp[idx] <- NA
                #convert to integer
                col.tmp <- as.integer(col.tmp)
                #put temp col back into report
                df[,cols[i]] <- col.tmp
        }
        
        df$CriMean <- rowMeans(select(df, Cri.A:Cri.D), na.rm = TRUE)
        df <- df %>% select(Student.ID, Last.Name, First.Name, Class.ID,
                            Grade.Level, Subject, Teacher,
                            Cri.A, Cri.B, Cri.C, Cri.D,
                            Sum, CriMean, Student.Comment) %>%
                mutate_each(funs(factor), Student.ID)
        df$Student.ID <- as.integer(levels(df$Student.ID))[df$Student.ID]
        df
}

GetMeanBreakdownFromReport <- function(report) {
        groupings <- c("Student.ID","Subject","Teacher",
                       "Grade.Level", "class.growth")
        report.stats <- setNames(vector(mode = "list",
                                        length = length(groupings)),
                                 groupings)
        for (i in groupings) {
                report.stats[[i]] <- report %>% group_by_(i) %>%
                        summarize( avg = mean(CriMean, na.rm = TRUE))  
        }
        report.stats
}

# Corpora: Creation and Cleaning ------------------------------------------

# change to all lowercase, strip whitespace numbers and punctuation,
# THEN remove some number of the following:
#   stopwordssuffixes (with option to complete)
CorpusClean <- function(corpus, stop = FALSE, stem = FALSE, complete = FALSE) {
        corpus <- corpus %>% 
                tm_map(removePunctuation) %>%
                tm_map(removeNumbers) %>%
                tm_map(stripWhitespace) %>%
                tm_map(content_transformer(tolower), lazy=TRUE)
                
        if (stop == TRUE) {
                corpus <- corpus %>% tm_map(removeWords, stopwords("en"))
        }        
        if (stem == TRUE) {
                corpus <- corpus %>% tm_map(stemDocument, language = "en")
        }
        if (complete == TRUE) {
                corpus <- corpus %>% 
                        tm_map(stemCompletion, dictionary = corpus.copy, type = "prevalent")
        }
        corpus
}

#rip Student.Comment column, turn it into a corpus and clean it 
GetCorpusFromReportDF <- function(reportdf) {
        
        reportdf <- reportdf %>% mutate(ID.SUB = paste(Student.ID, Subject))
        corpus <- collect(select(reportdf, Student.Comment))[[1]] %>%
                  VectorSource %>%
                  Corpus %>%
                  CorpusClean
        
        #tag each "document" (i.e. comment) in corpus using report information
        for (i in 1:nrow(reportdf)) {
                meta(corpus[[i]], tag = "teacher") <- reportdf[i,"Teacher"]
                meta(corpus[[i]], tag = "student") <- reportdf[i,"Student.ID"]
                meta(corpus[[i]], tag = "subject") <- reportdf[i, "Subject"]
                meta(corpus[[i]], tag = "grade") <- reportdf[i,"Grade.Level"]
                meta(corpus[[i]], tag = "ID.SUB") <- reportdf[i,"ID.SUB"]
        }

        corpus
}

GetCorpusFromReportDF2 <- function(reportdf) {
        
        reportdf <- reportdf %>% mutate(ID.SUB = paste(Student.ID, Subject))
        corpus <- collect(select(reportdf, Student.Comment))[[1]] %>%
                VectorSource %>%
                Corpus %>%
                CorpusClean
        
        #tag each "document" (i.e. comment) in corpus using report information
        lapply(nrow(reportdf), function(x) {
                meta(corpus[[x]], tag = "teacher") <- reportdf[x,"Teacher"]
                meta(corpus[[x]], tag = "student") <- reportdf[x,"Student.ID"]
                meta(corpus[[x]], tag = "subject") <- reportdf[x, "Subject"]
                meta(corpus[[x]], tag = "grade") <- reportdf[x,"Grade.Level"]
                meta(corpus[[x]], tag = "ID.SUB") <- reportdf[x,"ID.SUB"]
        })
        
        corpus
}

#' GetGroupCorpusfromCommentCorpus
#'      (revamp of GetCommentsGrouped)
#' 
#' ARGS corpus:a comment corpus where each document 
#'              (e.g. the output of GetCorpusFromReportDF)
#'      group: which grouping that you would like to try
#' OUTPUT: get corpus where each document is a member of a group
#' (e.g. "English" is a member of "subject")
GetGroupCorpusfromCommentCorpus <- function(corpus, group) {
        
        #get members of given group and initialize comments vector
        members <- unique(meta(corpus, group))
        
        #for each group member:
        #       1) get idx of all comments attributed to member
        #       2) assign a paste above member comments to comments vector
        member.comments <- lapply(members, function(x) {
                idx <- corpus %>% meta(tag = group) == x
                do.call(paste,content(corpus[idx]))
        })
        #set names of member.comments to members
        names(member.comments) <- members
        
        #make a corpus out of member.comments
        member.corpus <- VectorSource(member.comments) %>% Corpus
        
        #add corpus-level tag with list of members
        meta(member.corpus, tag = "members", type = "corpus") <- members
        
        #retag corpus with group member ID
        i <- 0
        member.corpus <- tm_map(member.corpus, function(x) {
                i <<- i +1
                meta(x, tag = group) <- members[[i]]
                x
        })
        
        member.corpus
}

# Corpora: text analytics -------------------------------------------------

#format dtm of all student/teacher corpus as matrix
#       w/ columns: ngram, tf-idf score, and
#       sorted by tf-idf score
CollapseAndSortDTM <- function(dtm) {
        
        freq.mat <- dtm %>%
                as.matrix %>%
                colSums() %>%
                sort(decreasing = TRUE) %>%
                as.matrix() %>%
                as.data.frame()
        freq.mat$Words <- row.names(freq.mat)
        freq.mat <- rename(freq.mat, freq = V1)
}

PlotIndvVSMembers <- function(corpus, tag, identifier, ngram.min, ngram.max) {
        idx <- corpus %>% meta(tag) == identifier
        
        # Sets the default number of threads to use
        options(mc.cores=1)
        
        DersTokenizer <- function(x, ngram.min, ngram.max) {
                NGramTokenizer(x, Weka_control(min = ngram.min, max = ngram.max))
        }
        #bug going on right here
        indv.dtm <- corpus[idx] %>%
                DocumentTermMatrix(control=list(tokenize = DersTokenizer)) %>%
                as.matrix
        
        all.dtm <- corpus[!idx] %>%
                DocumentTermMatrix(control=list(tokenize = DersTokenizer)) %>%
                as.matrix()
        
        
        # Make df of indv student words & Frequencies
        indv.freq <- CollapseAndSortDTM(indv.dtm)
        
        
        #format dtm of all student/teacher corpus as sorted 2C table
        all.freq <- CollapseAndSortDTM(all.dtm)
        
        #create a comparison table using only words indv used at least once
        comp.freq <- indv.freq %>%
                right_join(all.freq, by = "Words") %>%
                select(Words, freq.indv = freq.x, freq.all = freq.y)
        #replace any N/A's in freq.indv with zeroes
        comp.freq[is.na(comp.freq)] <- 0
        
        # add relative proportions as columns
        totals <-
                comp.freq %>% summarise(sum.indv = sum(freq.indv),
                                        sum.all = sum(freq.all))
        comp.freq <-
                comp.freq %>% mutate(
                        prop.indv = 1000 * freq.indv / totals$sum.indv,
                        prop.all = 1000 * freq.all / totals$sum.all)
        
        #most/least typical individual words
        max.all <- comp.freq %>% select(prop.all) %>% max()
        max.indv <- comp.freq %>% select(prop.indv) %>% max()
        comp.freq <- comp.freq %>%
                mutate(most.typical.dist = sqrt(prop.all^2 + (max.indv-prop.indv)^2),
                       least.typical.dist = sqrt(prop.indv^2 + (max.all-prop.all)^2)) %>%
                arrange(most.typical.dist)
        comp.freq <<- comp.freq
        # plot all student freq (x) vs. indv freq (y)
        plot(comp.freq$prop.all, comp.freq$prop.indv, xlab= "All student Frequency (Per 10^3 words)", ylab = "Individual Frequency (Per 10^3 words)")
        abline(a = 0, b = 1)
        # open comparison plot
        View(comp.freq)
}

#'GetDocumentTextMatrix
#'  INPUTS:
#'      group.corpus: corpus object where each doc is a member of a group
#'      nmin: minimum ngram length
#'      nmax: maximum ngram length
#'      norm: normalize ngram score for each document?
#' OUTPUT: Document Term Matrix with given contraints
GetDocumentTermMatrix <- function(corpus, nmin, nmax, norm) {
        options(mc.cores=1)
        
        tfidf <- function(x) {
                weightTfIdf(x, normalize = norm)
        }
        DersTokenizer <- function(x) {
                NGramTokenizer(x, Weka_control(min = nmin, max = nmax))
        }
        
        dtm <- DocumentTermMatrix(corpus, control=list(
                tokenize = DersTokenizer,
                weighting = tfidf))
}


# Grouped Corpora ---------------------------------------------------------



#' INPUTS:
#'      group.corpus: a group corpus object from GetGroupCorpusfromCommentCorpus
#'      identifier: group identifier (e.g. teacher:"May Shen")
#'      nmin: minimum ngram length
#'      nmax: maximum ngram length
#'      norm: normalize ngram score for each document?
#' OUTPUT: Document Term Matrix with given contraintst
#' STEPS:
#'      1) store list of members from corpus and find match given identifier
#'      2) Make document-term matrix (DTM)
#'      3) subset DTM to get only identifier
#'      4) use CollapseAndSortDTM to return only ngrams for individual
GetIndvTfIdfMatrixFromGroupedCorpus <- function(group.corpus, identifier, nmin, nmax, norm) {
        
        #rip corpus-level tag of "members" from corpus metadata
        #index the identifier in list of members
        members <- unlist(meta(group.corpus, type = "corpus", tag = "members"))
        idx <- which(members == identifier)
        
        dtm <- GetDocumentTermMatrix(group.corpus, nmin, nmax, norm)
        indv.ngrams <- dtm[idx,] %>% CollapseAndSortDTM 
        indv.ngrams <- indv.ngrams %>%
                mutate(length = CountWords(Words)) %>%
                mutate(LenNorm = length * freq) %>%
                arrange(desc(LenNorm))
}

#' GetAllTfIdfMatricesFromGroupedCorpus
#' PURPOSE: FOR GROUPED CORPORA
#' INPUTS:
#'      @param group.corpus: a group corpus object from GetGroupCorpusfromCommentCorpus
#'      @param nmin: minimum ngram length
#'      @paramn max: maximum ngram length
#'      @param norm: normalize ngram score for each document?
#' OUTPUT: A list of Document Term Matrices with given contraintst
#' STEPS:
#'      1) store list of members from corpus
#'      2) Make document-term matrix (DTM)
#'      3) for each member :
#'              subset DTM to get only identifier
#'      4) use CollapseAndSortDTM to return only ngrams for individual
#'      
GetAllTfIdfMatricesFromGroupedCorpus <- function(group.corpus, nmin, nmax, norm) {
        
        #rip corpus-level tag of "members" from corpus metadata
        #index the identifier in list of members
        members <- unlist(meta(group.corpus, type = "corpus", tag = "members"))
        
        dtm <- GetDocumentTermMatrix(group.corpus, nmin, nmax, norm)
        
        setNames(lapply(members, function(x) {
                idx <- which(members == x)
                indv.ngrams <- dtm[idx,] %>% CollapseAndSortDTM 
                indv.ngrams <- indv.ngrams %>%
                        mutate(length = CountWords(Words)) %>%
                        mutate(LenNorm = length * freq) %>%
                        arrange(desc(LenNorm))
        }),members)
}

#'GetPrunedListsFromGroupTfidfMatrices
#' 
#' INPUTS: 
#'      @param all.tfidf: a list of tfidx matrixes
#'      @param prune_thru: 
#' OUTPUT: a list of pruned matrixes of unique ngrams for each member
GetPrunedListsFromGroupTfidfMatrices <- function(all.tfidf, prune) {

        #for each table in all.tfidf, select some of the columns and prune what's there
        all.pruned <- lapply(all.tfidf, GetPrunedList, prune_thru = prune)
}

# Admin Plus --------------------------------------------------------------


#takes AdminPlus database export and strips unecessary columns
#lubridates, and filters out MYP and DP students
GetStudentDBfromAPxlsx <- function(xlsxpath) {
        #read in xlsx admin plus database, assign NA to empty chr, rm empty columns
        xis.db <- read.xlsx2(xlsxpath, sheetIndex = 1, startRow = 4,
                             header= TRUE, colIndex = 1:250)
        xis.db[ xis.db == "" ] <- NA
        xis.db <- Filter(function(x)!all(is.na(x)), xis.db)
        
        #lubridate and make "Age" and "Years.at.XIS" columns
        xis.db$BIRTH.DATE <- gsub("?","",xis.db$BIRTH.DATE, fixed = TRUE)  %>% dmy()
        xis.db$ENTRY.DAY.1 <- ymd(xis.db$ENTRY.DAY.1, quiet = TRUE)
        xis.db <- xis.db  %>% 
                mutate(Age = as.period(interval(start = BIRTH.DATE,
                                                end = today())),
                       Years.at.XIS = as.period(interval(start = ENTRY.DAY.1,
                                                         end = today())))
        
        
        #take out unneeded columns
        xis.db <- xis.db %>% select(Student.ID = UNIQUE.ID, LAST.NAME, FIRST.NAME,
                                    GRADE.LEVEL, HOMEROOM, HOUSE, GENDER, NATIONALITY,
                                    BIRTH.DATE, Age, ENTRY.DAY.1, Years.at.XIS, X1st.Language,
                                    X2nd.Language, Language..home, Mother.speaks,
                                    Father.speaks, SCHOOL.BUS, Language.Suppor,
                                    Conditional.Pla, Program, LAST.SCHOOL.ATT,Tuition.Paid.by)
        
        #1) filter out PK and K, 2) convert GRADE.LEVEL to numeric,
        #3) FILTER OUT PYP and DP
        # xis.db <- xis.db %>% filter(GRADE.LEVEL != "PK" & GRADE.LEVEL != "0K")
        # grade <- xis.db$GRADE.LEVEL
        # xis.db$GRADE.LEVEL <- suppressWarnings(as.numeric(levels(grade))[grade])
        # xis.db <- xis.db %>% filter(GRADE.LEVEL <= 10 & GRADE.LEVEL >= 6)
        
        xis.db$Student.ID <- as.integer(levels(xis.db$Student.ID))[xis.db$Student.ID]
        xis.db
}


# MAP ---------------------------------------------------------------------


#' GetMAPbyID
#' 
#' Takes csv of MAP test results (1 obs = 1 test) and:
#' 1) removes incomplete tests
#' 2) removes s's 1st attempt if 2nd complete attempt exists
#' 3) spreads the data so 1 obs = 1 student
GetMAPbyID <- function(map.path) {
        #test names that don't include all goals
        discipline.repeats <- c("PRI-READ-Survey w/ Goals CC Intl (Lit/Info,Vocab)V1",
                                "PRI-READ-Survey w/ Goals CC Intl (FoundSkills,LangWriting)V1",
                                "PRI-MATH-Survey w/ Goals CC Intl (Meas/Data,Geometry)V1")
        
        #get column of only "TestRIT
        cols <- c("RITScore", "Percentile","Duration.min")
        
        #read in data and named columns below
        MAP.Raw <- read.csv(map.path) %>%
                select(Student.ID = StudentID, Discipline, TestName,
                       RITScore= TestRITScore, Percentile =TestPercentile,
                       Duration.min = TestDurationInMinutes) 
        
        MAP.Clean <- MAP.Raw %>%
                #remove incomplete tests
                filter(!(TestName %in% discipline.repeats)) %>%
                select(everything(), -TestName) %>%
                arrange(Student.ID, Discipline, desc(RITScore)) %>%
                mutate(key = paste(Student.ID, Discipline)) %>%
                distinct(key) %>%
                select(everything(), -key)
        
        
        MAP.Spread <- MAP.Clean %>% 
                unite("temp", RITScore:Duration.min, sep = "_") %>%
                spread(Discipline, temp) %>%
                separate("Language Usage", paste("Lang", cols, sep = "."),
                         sep = "_", remove = TRUE, convert = TRUE) %>%
                separate("Mathematics", paste("Math", cols, sep = "."),
                         sep = "_", remove = TRUE, convert = TRUE) %>%
                separate("Reading", paste("Read", cols, sep = "."),
                         sep = "_", remove = TRUE, convert = TRUE)
}


# Helpers (Other) -------------------------------------------------------------------

#' GetPrunedList
#' 
#' takes a word freq df returned from CollapseAndSortDTM, returns pruned table
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

#INPUT: group.corpus and 
GetAllTfIdfMatricesFromCorpus <- function(corpus, nmin, nmax, norm) {
        dtm <- GetDocumentTermMatrix(corpus, nmin, nmax, norm)
        
        idx <- 1:nrow(as.matrix(dtm))
        
        all.tfidf <- lapply(idx, function(x) {
                indv.ngrams <- dtm[x,] %>% CollapseAndSortDTM 
                indv.ngrams <- indv.ngrams %>%
                        mutate(length = CountWords(Words)) %>%
                        mutate(LenNorm = length * freq) %>%
                        arrange(desc(LenNorm))
        })
}

#' CountWords
#' OBJ: count number of words in ngram
#' INPUT: an ngram as a string or list of strings
#' OUTPUT: # of words as an int of int list
CountWords <- function(ngram) {
        str_count(ngram, "\\S+")
}

#' overlap
#' OBJ: takes two ngrams (as strings) and to see if they overlap
#' INPUT: a,b ngrams as strings
#' OUTPUT: TRUE if overlap
overlap <- function(a, b) {
        max_overlap <- min(3, CountWords(a), CountWords(b))
        
        a.beg <- word(a, start = 1L, end = max_overlap)
        a.end <- word(a, start = -max_overlap, end = -1L)
        b.beg <- word(b, start = 1L, end = max_overlap)
        b.end <- word(b, start = -max_overlap, end = -1L)
        
        # b contains a's beginning
        w <- str_detect(b, coll(a.beg, TRUE))
        # b contains a's end
        x <- str_detect(b, coll(a.end, TRUE))
        # a contains b's beginning
        y <- str_detect(a, coll(b.beg, TRUE))
        # a contains b's end
        z <- str_detect(a, coll(b.end, TRUE))
        
        #return TRUE if any of above are true
        (w | x | y | z)
}

#' plotting library from Stack Overflow
#' takes a linear model and plots it on the graph w/ R^s values
lm_eqn <- function(m) {
        
        l <- c(a = format(coef(m)[1], digits = 2),
               b = format(abs(coef(m)[2]), digits = 2),
               r2 = format(summary(m)$r.squared, digits = 3) )
        ifelse(
                {coef(m)[2] >= 0},
               {eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,l)},
               {eq <- substitute(italic(y) == a - b %.% italic(x)*","~~italic(r)^2~"="~r2,l)}
               ) 
        as.character(as.expression(eq))
}

#' PlotScatterFaceted
#'
#' Makes scatterpolot faceted by chosen variable
PlotScatterFaceted <- function(data, ind, dep, facet) {
        ggplot(data, aes_string(x = ind, y = dep)) +
                geom_point() +
                facet_wrap(as.formula(sprintf('~%s',facet)))
}

#' specify_decimal
#'
#' takes a number,x, and rounds and truncates to k digits
specify_decimal <- function(x, k) {
        format(round(x, k), nsmall=k)
}

# stat_smooth_func --------------------------------------------------------
# from https://gist.github.com/kdauria/524eade46135f6348140

source("stat_smooth_func.R")