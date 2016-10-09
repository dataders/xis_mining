#' WHAT I DID TODAY
#' 1) hard-coded out nickname usage in "t1 comments.csv"
#' 2) built function to clean
#' WHAT IS STILL FUCKED UP
#' 1) last names "he" and "li" remove more than wanted.
#'    FIX: add regex for non a-z on either side of variable
#' 2) poly ngram 1st names need to be tokenized then scrubbed
#' 3) 29 obs of scrubbed names remaining
#' 4) match output structure of Anonymize report to match std t1.report struc.

#search string pattern for one word in parentheses
# [(][A-Za-z]+[)]

# Action plan
# 1st pass remove names

#set wd and paths and source functions
setwd("/Users/andersswanson/Desktop/comment\ mining")
source("Functions.R")
source("MB - t12 growth.R")

t1.report <- GetReportsDFfromMBcsv("data/t1 comments.csv")


AnonymizeReport <- function(report) {
        
        
        regex.parenth <- "[ ]*[(][a-z]+[)]"
        
        #Makes a regex for searching name instances where there are spaces
        # beforehand (optional), and
        # after        
        RegexName <- function(names) {
                non <- "\b"
                str_c("/",non,names,non,"/g/", sep = "")
        }
        
        report %>% 
                #make lowercase vars below
                mutate_each(funs(tolower), Last.Name, First.Name, Student.Comment) %>%
                #remove newline chr
                mutate(Student.Comment = str_replace_all(Student.Comment, "\n", "")) %>%
                # remove empty comments
                filter(Student.Comment != "") %>%
                #Modify
                mutate(
                        #take off nickname from First.Name
                        Legal.Name = str_replace(First.Name, regex.parenth, ""),
                        #remove hyphen from First.Name
                        Unpunct.Name = str_replace(Legal.Name, "-", " ")) %>%
                        
                #scrub ____ from comment w/ regex:
                mutate(Comment.Clean = 
                        #1) Legal Name
                        str_replace_all(Student.Comment, Legal.Name, "@@@") %>%
                        #2) Last Name        
                        str_replace_all(RegexName(Last.Name), "@@@") %>%
                        #3) Unpunctuated name
                        str_replace_all(Unpunct.Name, "@@@"),
                       
                        #tests for broken comments
                        nickname = str_detect(Comment.Clean, regex.parenth),
                        missed = !str_detect(Comment.Clean, "@@@")) %>%
                
                #restructure to match input's columns i.e. drop added columns
                select(Student.ID:CriMean, Student.Comment = Comment.Clean)
}

t1.report2 <- AnonymizeReport(t1.report)

unanoned <- t1.report2 %>% filter(missed == TRUE)

