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

test <- t1.report %>% select(Last.Name, First.Name, Student.Comment) %>%
        #lower case all columns
        mutate_each(funs(tolower)) %>%
        # remove empty comments
        filter(Student.Comment != "") %>%
        #remove newline chr
        
        #drop nicknames from First.Name
        mutate(Legal.Name = str_replace(First.Name, regex.parenth, ""),
               Unpunct.Name = str_replace(First.Name, "-", " ")) %>%
        mutate(
               #remove Legal name from Student Comments
               Comment.Clean = str_replace_all(Student.Comment, Legal.Name, "@@@"),
               Comment.Clean2 = str_replace_all(Comment.Clean, Last.Name, "@@@"),
               nickname = str_detect(Comment.Clean2, regex.parenth),
               missed = !str_detect(Comment.Clean2, "@@@")) %>%
        select(Last.Name, First.Name, Legal.Name, Unpunct.Name, Student.Comment, Comment.Clean, Comment.Clean2, nickname, missed)

nicknames <- test %>% filter(nickname == TRUE)
unanoned <- test %>% filter(missed == TRUE)

test4 <- mutate(test3, Student.Comment = str_replace_all(Student.Comment, "\n", ""))
