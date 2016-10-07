#set wd and paths and source functions
setwd("/Users/andersswanson/Desktop/comment\ mining")
source("Functions.R")
source("MB - t12 growth.R")

t1.report <- GetReportsDFfromMBcsv("data/t1 comments.csv")

test <- t1.report %>% select(Last.Name, First.Name, Student.Comment) %>%
        mutate_each(funs(tolower))

#gsub(pattern, replacement, x, ignore.case = FALSE, perl = FALSE,
#     fixed = FALSE, useBytes = FALSE)


# base case 1 -------------------------------------------------------------

first <- NGramTokenizer(test[3, "First.Name"])
comment <- test[3, "Student.Comment"]

str_replace(comment, first, "@@@")


# base case 2 -------------------------------------------------------------


first <- test[1:3, "First.Name"]
first <- lapply(test[1:3, "First.Name"], NGramTokenizer)
comment <- test[1:3, "Student.Comment"]

lapply(1:3, function(x) {
        gsub(first[x], "@@@", comment[x], fixed = TRUE)
})

lapply(1:3, function(x) {
        str_replace(comment[x], first[x], "@@@")
})



gsub(test$First.Name, "@@@", test$Student.Comment, ignore.case = FALSE, perl = FALSE,
     fixed = FALSE, useBytes = FALSE)