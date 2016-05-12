#author: Anders Swanson
#load required libraries, data, and created functions
library(dplyr)
library(tidyr)
library(xlsx)
library(lubridate)

setwd("/Users/andersswanson/Desktop/comment\ mining")
db.path <- "data/xis db.xlsx"
source("Functions.R")

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
