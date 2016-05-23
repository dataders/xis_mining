setwd("/Users/andersswanson/Desktop/comment\ mining")
source("Functions.R")

MAP.testdate <- c("2015Fall", "2016Spring")
MAP.path <- paste("data/", MAP.testdate,".Map.Results.csv", sep = "")

#Load MAP score database
MAP <- lapply(MAP.path, GetMAPbyID)
MAP.DIFF <- merge(MAP[1], MAP[2],
                    by = "Student.ID", suffixes = c(".FALL", ".SPRING")) %>%
        mutate(Lang.RITGrowth = Lang.RITScore.SPRING - Lang.RITScore.FALL) %>%
        mutate(Read.RITGrowth = Read.RITScore.SPRING - Read.RITScore.FALL) %>%
        mutate(Math.RITGrowth = Math.RITScore.SPRING - Math.RITScore.FALL) %>%
        select(Student.ID, starts_with("Math."), starts_with("Read."),starts_with("Lang."))
