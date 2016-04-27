library(tm,tidyr)
library(dplyr)
setwd("~/Dropbox/ds/XIS data")
reports <- read.csv("t1 comments.csv", stringsAsFactors=FALSE)

#get a vector of teacher comments where each string is a giant string of the teacher's comments
reports2 <- read.csv("t1 comments.csv", stringsAsFactors=TRUE)
teachers <- levels(reports2$Teacher)
#create empty list and name each item after name of teacher
teacher.comments <- vector("list", length(teachers))
names(teacher.comments) <- teachers

#uses group_by() and select() to create df of 
reports.grouped <- group_by(reports, Teacher) %>%
    select(Student.Comment)
#loop through each teacher to supset grouped df to concat all comments into one string
for(i in teachers) {
    teacher.comments[i] <- reports %>% 
        filter(Teacher == i) %>%
        select(Student.Comment, -) %>%
        paste(collapse=" ", sep = " ")
}

filter()
select()

all.comments <- reports$Student.Comment
anders.subset <- reports[reports$Teacher == "Anders Swanson",]
anders.comments <- anders.subset$Student.Comment
anders.paste <- paste(anders.comments, collapse = " ")



#turn subset into a corpus
ToCorpus <- function(comment.subset) {
    comment.subset %>%
          paste(collapse=" ") %>%
          VectorSource() %>%
          Corpus()
  }
#remove uppercase, punctuation, whitespace, & stopwords
CorpusClean <- function(corpus) {
    corpus %>% 
      tm_map(content_transformer(tolower)) %>% 
      tm_map(removePunctuation) %>% 
      tm_map(stripWhitespace) %>% 
      tm_map(removeWords, stopwords("english")) %>%
      tm_map(stemDocument)
  }
#frequency lists
CorpusToFreq <- function(corpus) {
      dtm <- corpus  %>%
          DocumentTermMatrix()  %>%
          as.matrix()  %>%
     frequency <- colSums(dtm)
     frequency <- sort(frequency, decreasing=TRUE)
     frequency
}

