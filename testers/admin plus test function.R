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

#read in xlsx admin plus database, assign NA to empty chr, rm empty columns
xis.db <- read.xlsx2(db.path, sheetIndex = 1, startRow = 4,
                     header= TRUE, colIndex = 1:250)
xis.db[ xis.db == "" ] <- NA
xis.db <- Filter(function(x)!all(is.na(x)), xis.db)


#take out unneeded columns
xis.db2 <- xis.db %>% select(Student.ID = UNIQUE.ID, LAST.NAME, FIRST.NAME,
                            GRADE.LEVEL, HOMEROOM, HOUSE, GENDER, NATIONALITY,
                            BIRTH.DATE, ENTRY.DAY.1, X1st.Language,
                            X2nd.Language, Language..home, Mother.speaks,
                            Father.speaks, SCHOOL.BUS, Language.Suppor,
                            Conditional.Pla, Program, LAST.SCHOOL.ATT,Tuition.Paid.by)

#1) filter out PK and K, 2) convert GRADE.LEVEL to numeric,
#3) FILTER OUT PYP and DP
xis.db3 <- xis.db2 %>% filter(GRADE.LEVEL != "PK" & GRADE.LEVEL != "0K")
grade <- xis.db3$GRADE.LEVEL
xis.db3$GRADE.LEVEL <- suppressWarnings(as.numeric(levels(grade))[grade])
xis.db3 <- xis.db3 %>% filter(GRADE.LEVEL <= 10 & GRADE.LEVEL >= 6)
        
#lubridate and make "Age" and "Years.at.XIS" columns
xis.db3$BIRTH.DATE <- gsub("?","",xis.db3$BIRTH.DATE, fixed = TRUE)  %>% dmy()
xis.db3$ENTRY.DAY.1 <- ymd(xis.db3$ENTRY.DAY.1, quiet = TRUE)
mayday <- ymd(20160506)
xis.db3$Age <- as.period(interval(start = xis.db3$BIRTH.DATE,
                                        end = ymd(20160506)))
               Years.at.XIS = as.period(interval(start = ENTRY.DAY.1,
                                                 end = ymd(20160506))))

View(xis.db4)

