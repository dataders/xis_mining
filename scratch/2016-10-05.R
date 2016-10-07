#' WHAT I DID TODAY
#' 1) hard-coded out nickname usage in "t1 comments.csv"
#' 2) built function to clean
#' WHAT IS STILL FUCKED UP
#' 1) last names "he" and "li" remove more than wanted.
#'    FIX: add regex for non a-z on either side of variable
#' 2) poly ngram 1st names need to be tokenized then scrubbed
#' 3) 36 obs of scrubbed names remaining
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

regex.parenth <- "[ ]*[(][a-z]+[)]"

AnonymizeReport <- function(report) {
        report %>% 
                #make lowercase vars below
                mutate_each(funs(tolower), Last.Name, First.Name, Student.Comment) %>%
                #remove newline chr
                mutate(Student.Comment = str_replace_all(Student.Comment, "\n", "")) %>%
                # remove empty comments
                filter(Student.Comment != "") %>%
                #drop nicknames and punctuation from First.Name
                mutate(Legal.Name = str_replace(First.Name, regex.parenth, ""),
                Unpunct.Name = str_replace(First.Name, "-", " ")) %>%
                        
                mutate(
                        #remove Legal name and Last name from Student Comments
                        Comment.Clean = str_replace_all(Student.Comment, Legal.Name, "@@@") %>% 
                                str_replace_all(Last.Name, "@@@") %>%
                                str_replace_all(Unpunct.Name, "@@@"),
                        #tests for broken
                        nickname = str_detect(Comment.Clean, regex.parenth),
                        missed = !str_detect(Comment.Clean, "@@@"))
}

t1.report2 <- AnonymizeReport(t1.report)

unanoned <- t1.report2 %>% filter(missed == TRUE)


# test case for chloe -----------------------------------------------------

comment <- "overall, on tim's work is consistently of a high standard.  her ted talk on gender equality was most impressive: she paid great attention to detail, was well organized, and engaged her audience.  her attention to detail when creating the stage for the ted talk was remarkable.  on tim's demonstrates strong self-management by asking for guidance as she is working as well as managing time. she seems enthusiastic about the work we are doing and is inquisitive'making connections and insightful observations about literature.  i look forward to seeing what she produces for our current unit on human rights. 
   nickname missed"
name <- "on tim"

?str_replace_all

str_replace_all(comment, name, "@@@")
