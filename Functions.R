#given "teachers" or "students"
#returns vector of comments for teacher or student in question
GetComments <- function(group) {
        if (group == "students") {
                column.name <- "Student.ID"	
        }
        if (group == "teachers") {
                column.name <- "Teacher"
        }
        if (group == "subject") {
                column.name <- "Subject"
        }
        if (group == "grade") {
                column.name <- "Grade.Level"
        }
        
        reports2 <- read.csv(report.path, stringsAsFactors=TRUE)
        members <- levels(reports2[[column.name]])
        member.comments <- vector("list", length(members))
        names(member.comments) <- members
        
        for (i in members) {

                sub <- reports %>% filter(
                        get(column.name, envir=as.environment(reports))==i)

                member.comments[i] <- sub$Student.Comment %>%
                        paste(collapse = " ")
        }
        member.comments
}

#turn subset into a corpus
ToCorpus <- function(comment.subset) {
        comment.subset %>%
        VectorSource() %>%
        Corpus()
}

#remove uppercase, punctuation, whitespace, & stopwords
CorpusClean <- function(corpus) {
  corpus.copy <- corpus
  corpus %>% 
    #tm_map(removeWords, student.names) %>%
    tm_map(removePunctuation) %>%
    tm_map(stripWhitespace) %>%
    tm_map(content_transformer(tolower), lazy=TRUE)
    #tm_map(removeWords, stopwords("en"))
    #tm_map(stemDocument)
    #tm_map(stemCompletion, dictionary = corpus.copy, type = "prevalent")
}

#take teacher name as string and produce table and plot
TeacherComp <- function(name) {
  
  # make a dtm for individual (indv) teacher in question
  indv <- teachers == name
  indv.dtm <- tch.corpus[indv] %>%
    DocumentTermMatrix %>%
    as.matrix
  
  # Make df of indv Teacher words & Frequencies
  indv.freq <- indv.dtm %>%
    colSums() %>%
    sort(decreasing = TRUE) %>%
    as.matrix() %>%
    as.data.frame()
  indv.freq$Words <- row.names(indv.freq)
  indv.freq <- rename(indv.freq, freq = V1)
  
  
  #create dtm from all teacher corpus
  all.dtm <- tch.corpus %>%
    DocumentTermMatrix() %>%
    as.matrix()
  
  #format dtm of all teacher corpus as sorted 2C table
  all.freq <- all.dtm %>%
    colSums() %>%
    sort(decreasing = TRUE) %>%
    as.matrix() %>%
    as.data.frame()
  all.freq$Words <- row.names(all.freq)
  all.freq <- rename(all.freq, freq = V1)
  
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
  
  # plot all teacher freq (x) vs. indv freq (y)
  plot(comp.freq$prop.all, comp.freq$prop.indv, xlab= "All Teacher Frequency (Per 10^3 words)", ylab = "Individual Frequency (Per 10^3 words)")
  abline(a = 0, b = 1)
  # open comparison plot
  View(comp.freq)
}

CompIndv2All <- function(role, identifier) {
  if (role == "teacher") {
    
    indv <- teachers == identifier
    indv.dtm <- tch.corpus[indv] %>%
      DocumentTermMatrix %>%
      as.matrix
    
    all.dtm <- tch.corpus %>%
      DocumentTermMatrix() %>%
      as.matrix()
    
  }
  if (role == "student") {
    
    indv <- students == identifier
    indv.dtm <- sdt.corpus[indv] %>%
      DocumentTermMatrix %>%
      as.matrix
    
    all.dtm <- sdt.corpus %>%
      DocumentTermMatrix() %>%
      as.matrix()
  }
  
  # Make df of indv student words & Frequencies
  indv.freq <- indv.dtm %>%
    colSums() %>%
    sort(decreasing = TRUE) %>%
    as.matrix() %>%
    as.data.frame()
  indv.freq$Words <- row.names(indv.freq)
  indv.freq <- rename(indv.freq, freq = V1)
  
  #format dtm of all student/teacher corpus as sorted 2C table
  all.freq <- all.dtm %>%
    colSums() %>%
    sort(decreasing = TRUE) %>%
    as.matrix() %>%
    as.data.frame()
  all.freq$Words <- row.names(all.freq)
  all.freq <- rename(all.freq, freq = V1)
  
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

Comp2All <- function(tag, identifier, ngram.min, ngram.max) {
        idx <- all.corpus %>% meta(tag) == identifier
        
        DersTokenizer <- function(x) {
                NGramTokenizer(x, Weka_control(min = ngram.min, max = ngram.max))
        }
  indv.dtm <- all.corpus[idx] %>%
          DocumentTermMatrix(control=list(tokenize = DersTokenizer)) %>%
          as.matrix
  
  all.dtm <- all.corpus[!idx] %>%
          DocumentTermMatrix(control=list(tokenize = DersTokenizer)) %>%
    as.matrix()
  
  
  # Make df of indv student words & Frequencies
  indv.freq <- indv.dtm %>%
    colSums() %>%
    sort(decreasing = TRUE) %>%
    as.matrix() %>%
    as.data.frame()
  indv.freq$Words <- row.names(indv.freq)
  indv.freq <- rename(indv.freq, freq = V1)
  
  #format dtm of all student/teacher corpus as sorted 2C table
  all.freq <- all.dtm %>%
    colSums() %>%
    sort(decreasing = TRUE) %>%
    as.matrix() %>%
    as.data.frame()
  all.freq$Words <- row.names(all.freq)
  all.freq <- rename(all.freq, freq = V1)
  
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
        
        group.comments <- GetComments(group)
        
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
        
        a.dtm <- DocumentTermMatrix(group.corpus, control=list(
                tokenize = DersTokenizer,
                weighting = tfidf))
        
        a.mat <- a.dtm[idx,] %>% as.matrix
        
        a.freq <- a.mat %>%
                colSums() %>%
                sort(decreasing = TRUE) %>%
                as.matrix() %>%
                as.data.frame()
        a.freq$Words <- row.names(a.freq)
        a.freq <- rename(a.freq, freq = V1)
        a.freq
}
