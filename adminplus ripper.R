#author: Anders Swanson
#load required libraries, data, and created functions
library(dplyr)
library(tidyr)
library(xlsx)
library(lubridate)

setwd("/Users/andersswanson/Desktop/comment\ mining")
db.path <- "data/xis db.xlsx"
source("Functions.R")



xis.db <- GetStudentDBfromAPxlsx(db.path)


xis.db <- xis.db  %>% 
        mutate(Age = as.period(interval(start = BIRTH.DATE,
                                        end = today())),
               Years.at.XIS = as.period(interval(start = ENTRY.DAY.1,
                                                 end = today())))

xis.db <- xis.db %>% separate(Language..home,
                              c("language.home1", "language.home2", "language.home3"),
                              sep = "/",
                              remove = FALSE)


#separate language at home
xis.lang <- xis.db %>% separate(Language..Home, c("language.home1", "language.home2", "language.home3"),
                                sep = "/", remove = TRUE) %>%
        select(Student.ID = UNIQUE.ID, LAST.NAME, FIRST.NAME, NATIONALITY,
                              X1st.Language, X2nd.Language, Language..home,
                              language.home1, language.home2, language.home3)
