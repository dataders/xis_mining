#test things to figure out why juk wang jan wong's last name isn't being removed from one comment!!!!

non <- "[^a-z]"
name <- c("he","li")
regex <- paste("/",non,"*",name,non,"/g/", sep = "")



RegexName <- function(names) {
        non <- "\b"
        str_c("/",non,"*",names,non,"/g/", sep = "")
}

RegexName("anders")


juk <- t1.report2 %>% filter(Student.ID == "10002301") %>%
        select(First.Name, Teacher,  Student.Comment:missed)

comments <- juk$Student.Comment
comments.clean <- comments %>%
        #1) Legal Name
        str_replace_all("yuk wang jan", "@@@") %>%
        #2) Last Name        
        str_replace_all(RegexName("wong"), "@@@") %>%
        #3) Unpunctuated name
        str_replace_all("yuk wang jan", "@@@")

check <- cbind(comments, comments.clean)
