#author = Anders Swanson
# Dependencies ------------------------------------------------------------

#load required libraries, data, and created functions
library(dplyr)
library(tm)
library(tidyr)
library(SnowballC)
library(RWeka)
library(xlsx)
library(lubridate)
library(ggplot2)
library(stringr)

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
        
        #filter out all entirely-NA columns
        df <- Filter(function(x) !all(is.na(x)), df)
        
        #filter out rows with empty Teacher of Student.ID 
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
        df <- df %>% select(Student.ID, Last.Name, First.Name,
                            Grade.Level, Subject, Teacher,
                            Cri.A, Cri.B, Cri.C, Cri.D,
                            Sum, CriMean, Student.Comment) %>%
                mutate_each(funs(factor), Student.ID)
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

#remove uppercase, punctuation, whitespace, & stopwords
CorpusClean <- function(corpus, stop = FALSE, stem = FALSE, complete = FALSE) {
        corpus <- corpus %>% 
                tm_map(removePunctuation) %>%
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
        corpus <- collect(select(reportdf, Student.Comment))[[1]] %>%
                  VectorSource %>%
                  Corpus %>%
                  CorpusClean
        
        #tag each "document" (i.e. comment) in corpus using report information
        for (i in 1:nrow(reportdf)) {
                meta(corpus[[i]], tag = "teacher") <- reportdf[i,"Teacher"]
                meta(corpus[[i]], tag = "student ID") <- reportdf[i,"Student.ID"]
                meta(corpus[[i]], tag = "subject") <- reportdf[i, "Subject"]
                meta(corpus[[i]], tag = "grade") <- reportdf[i,"Grade.Level"]
        }
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
        member.comments <- setNames(vector("list", length(members)),
                                    members)
        #for each group member:
        #       1) get idx of all comments attributed to member
        #       2) assign a paste above member comments to comments vector
        for (i in members) {
                idx <- corpus %>% meta(group) == i
                member.comments[[i]] <- do.call(paste, content(corpus[idx]))
        }

        #make a corpus out of member.comments
        member.corpus <- VectorSource(member.comments) %>% Corpus
        
        #add corpus-level tag with list of members
        meta(member.corpus, tag = "members", type = "corpus") <- members
        #retag corpus with group member ID
        for (i in 1:length(member.corpus)) {
                meta(member.corpus[[i]], tag = group) <- members[[i]]
        }
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
        
        teacher.dtm <- DocumentTermMatrix(corpus, control=list(
                tokenize = DersTokenizer,
                weighting = tfidf))
}

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

# Admin Plus --------------------------------------------------------------

#' Calculate age
#' 
#' By default, calculates the typical "age in years", with a
#' \code{floor} applied so that you are, e.g., 5 years old from
#' 5th birthday through the day before your 6th birthday. Set
#' \code{floor = FALSE} to return decimal ages, and change \code{units}
#' for units other than years.
#' @param dob date-of-birth, the day to start calculating age.
#' @param age.day the date on which age is to be calculated.
#' @param units unit to measure age in. Defaults to \code{"years"}. Passed to \link{\code{duration}}.
#' @param floor boolean for whether or not to floor the result. Defaults to \code{TRUE}.
#' @return Age in \code{units}. Will be an integer if \code{floor = TRUE}.
#' @examples
#' my.dob <- as.Date('1983-10-20')
#' age(my.dob)
#' age(my.dob, units = "minutes")
#' age(my.dob, floor = FALSE)
age <- function(dob, age.day = today(), units = "years", floor = TRUE) {
        calc.age = new_interval(dob, age.day) / duration(num = 1, units = units)
        if (floor) return(as.integer(floor(calc.age)))
        return(calc.age)
}

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
}


# MAP ---------------------------------------------------------------------

GetMAPbyID <- function(map.path) {
        map.df <- read.csv(map.path) %>%
                select(Student.ID = StudentID, Discipline, RITScore= TestRITScore,
                       StandardError = TestStandardError, Percentile =TestPercentile,
                       TypicalFallToFallGrowth,
                       Goal1Name, Goal1RitScore, Goal1StdErr,
                       Goal2Name, Goal2RitScore, Goal2StdErr,
                       Goal3Name, Goal3RitScore, Goal3StdErr,
                       Goal4Name, Goal4RitScore, Goal4StdErr) %>%
                #convert Student.ID column to factor
                mutate_each(funs(factor), Student.ID) %>%
                #hack to take out duplicate tests and "uncombined" test results
                slice(-c(9,34,473)) %>%
                slice(-c(365, 366, 519, 520, 637, 638, 655, 656, 904, 905))
        
        #get column of only "TestRIT
        cols <- colnames(map.df)[3:6]
        
        map.df <- map.df %>% 
                select(Student.ID:TypicalFallToFallGrowth) %>%
                unite(temp, RITScore:TypicalFallToFallGrowth, sep = "_") %>%
                spread(Discipline, temp) %>%
                separate("Language Usage", paste("Lang", cols, sep = "_"),
                         sep = "_", remove = TRUE) %>%
                separate("Mathematics", paste("Math", cols, sep = "_"),
                         sep = "_", remove = TRUE) %>%
                separate("Reading", paste("Read", cols, sep = "_"),
                         sep = "_", remove = TRUE)
}


# Helpers (Other) -------------------------------------------------------------------

#' CountWords
#' OBJ: count number of words in ngram
#' INPUT: an ngram as a string or list of strings
#' OUTPUT: # of words as an int of int list
CountWords <- function(ngram) {
        str_count(ngram, "\\S+")
}

#' plotting library from Stack Overflow
#' takes a linear model and plots it on the graph w/ R^s values
lm_eqn = function(m) {
        l <- list(a = format(coef(m)[1], digits = 2),
                  b = format(abs(coef(m)[2]), digits = 2),
                  r2 = format(summary(m)$r.squared, digits = 3))
        
        if (coef(m)[2] >= 0)  {
                eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,l)
        } 
        else {
                eq <- substitute(italic(y) == a - b %.% italic(x)*","~~italic(r)^2~"="~r2,l)
        }
        as.character(as.expression(eq))              
}


