#load required libraries, data, and created functions
library(dplyr)
library(tidyr)

setwd("~/Dropbox/ds/XIS data")
source("Functions.R")

db.path <- "xis db.xlsx"

xis.db <- read.xlsx2(db.path, sheetIndex = 1, startRow = 4,
                     header= TRUE, colIndex = 1:250)
xis.db[ xis.db == "" ] <- NA
xis.db <- Filter(function(x)!all(is.na(x)), xis.db)

xis.db <- xis.db %>% separate(Language..home,
                              c("language.home1", "language.home2", "language.home3"),
                              sep = "/",
                              remove = FALSE)

xis.lang <- xis.db %>% select(FORMAL.NAME, UNIQUE.ID, NATIONALITY,
                              X1st.Language, X2nd.Language, Language..home,
                              language.home1, language.home2, language.home3)

lang.sep <- lang %>% separate(Language.Home,
                              c("language.home1", "language.home2", "language.home3"),
                              sep = "/",
                              remove = FALSE)

d <- xis.lang %>% filter(language.home1== "English" |
                                 language.home2== "English" | language.home3 == "English")

lang.df <- structure(list(Nationality = structure(c(4L, 3L, 7L, 6L, 8L, 
4L, 1L, 4L, 2L, 5L), .Label = c("Australian", "Brazilian", "German", 
"HK", "Indian", "Norwegian", "Saudi", "UK"), class = "factor"), 
`Language.Home` = structure(c(4L, 6L, 1L, 7L, 2L, 5L, 4L, 
4L, 8L, 3L), .Label = c("Arabic", "English", "Hindi/English", 
"Mandarin", "Mandarin/ Min Nan dialect", "Mandarin/English/German", 
"Norwegian", "Portuguese/English"), class = "factor")), row.names = c(NA, 
10L), .Names = c("Nationality", "Language.Home"), class = "data.frame")

library(dplyr)
library(tidyr)
#separate Langauge.Home into three new columns
lang.df <- lang.df %>% separate(Language.Home,
        c("language.home1", "language.home2", "language.home3"),
        sep = "/",
        remove = FALSE)
#find distinct languages & remove NAs
langs <- unique(c(lang.df$language.home1,
    lang.df$language.home2,
    lang.df$language.home3))
langs <- langs[!is.na(langs)]
#create boolean column for each unique language in new columns
for (i in langs) {
lang.df[,paste(i)] <- grepl(i, lang.df$Language.Home) 
}

