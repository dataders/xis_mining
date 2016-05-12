
# Dependencies ------------------------------------------------------------

#load required libraries, data, and created functions
library(dplyr)
library(tm)
library(tidyr)
library(SnowballC)
library(RWeka)
library(xlsx)

# ManageBac ---------------------------------------------------------------



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
                            Sum, CriMean, Student.Comment)
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

#remove uppercase, punctuation, whitespace, & stopwords
CorpusCleanStem <- function(corpus) {
        corpus.copy <- corpus
        corpus %>% 
                #tm_map(removeWords, student.names) %>%
                tm_map(removePunctuation) %>%
                tm_map(stripWhitespace) %>%
                tm_map(content_transformer(tolower), lazy=TRUE) %>%
                tm_map(removeWords, stopwords("en")) %>%
                tm_map(stemDocument, language = "en")
        #tm_map(stemCompletion, dictionary = corpus.copy, type = "prevalent")
}

#rip Student.Comment column, turn it into a corpus and clean it 
GetCorpusFromReportDF <- function(reportdf) {
        corpus <- collect(select(reportdf, Student.Comment))[[1]] %>%
                  VectorSource %>%
                  Corpus %>%
                  CorpusCleanStem
        
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
        #retag corpus with group member ID
        for (i in 1:length(member.corpus)) {
                meta(member.corpus[[i]], tag = group) <- members[[i]]
        }
        member.corpus
}



#format dtm of all student/teacher corpus as matrix
#       w/ columns: ngram, tf-idf score, and
#       sorted by tf-idf score
GetNgramWeightMatrixfromDTM <- function(dtm) {
        
        freq.mat <- dtm %>%
                colSums() %>%
                sort(decreasing = TRUE) %>%
                as.matrix() %>%
                as.data.frame()
        freq.mat$Words <- row.names(freq.mat)
        freq.mat <- rename(freq.mat, freq = V1)
}

Comp2All <- function(corpus, tag, identifier, ngram.min, ngram.max) {
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
        indv.freq <- GetNgramWeightMatrixfromDTM(indv.dtm)
        
        
        #format dtm of all student/teacher corpus as sorted 2C table
        all.freq <- GetNgramWeightMatrixfromDTM(all.dtm)
        
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

GetIndvTfIdf <- function(group, identifier, nmin, nmax) {
        
        group.comments <- GetCommentsGrouped(group)
        
        # Make a corpus and clean it w/ pre-defined functions
        group.corpus <- ToCorpus(group.comments)
        group.corpus <- CorpusClean(group.corpus)
        
        tfidf <- function(x) {
                weightTfIdf(x, normalize = FALSE)
        }
        DersTokenizer <- function(x) {
                NGramTokenizer(x, Weka_control(min = nmin, max = nmax))
        }
        
        idx <- match(identifier, names(group.comments))
        
        # Sets the default number of threads to use
        options(mc.cores=1)
        
        a.dtm <- DocumentTermMatrix(group.corpus, control=list(
                tokenize = DersTokenizer,
                weighting = tfidf))
        
        a.mat <- a.dtm[idx,] %>% as.matrix
        
        a.freq <-  GetNgramWeightMatrixfromDTM(a.mat) 
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
                                    BIRTH.DATE, Age, ENTRY.DAY.1, X1st.Language,
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


