setwd("/Users/andersswanson/Desktop/comment\ mining")
source("Functions.R")

GetMAPbyID2 <- function(map.path) {
        map.df <- read.csv(map.path) %>%
                select(Student.ID = StudentID, Discipline, RITScore= TestRITScore,
                       StandardError = TestStandardError, Percentile =TestPercentile,
                       TypicalFallToFallGrowth,
                       Goal1Name, Goal1RitScore, Goal1StdErr,
                       Goal2Name, Goal2RitScore, Goal2StdErr,
                       Goal3Name, Goal3RitScore, Goal3StdErr,
                       Goal4Name, Goal4RitScore, Goal4StdErr) %>%
                #convert Student.ID column to factor
                mutate_each(funs(factor), Student.ID) %>%
                #hack to take out duplicate tests and "uncombined" test results
                slice(-c(9,34,473)) %>%
                slice(-c(365, 366, 519, 520, 637, 638, 655, 656, 904, 905))
        
        #get column of only "TestRIT
        cols <- colnames(map.df)[3:6]
        
        map.df <- map.df %>% 
                select(Student.ID:TypicalFallToFallGrowth) %>%
                unite(temp, RITScore:TypicalFallToFallGrowth, sep = "_") %>%
                spread(Discipline, temp) %>%
                separate("Language Usage", paste("Lang", cols, sep = "_"),
                         sep = "_", remove = TRUE) %>%
                separate("Mathematics", paste("Math", cols, sep = "_"),
                         sep = "_", remove = TRUE) %>%
                separate("Reading", paste("Read", cols, sep = "_"),
                         sep = "_", remove = TRUE)
}


MAP.testdate <- c("2015Fall", "2016Spring")
MAP.path <- paste("data/", MAP.testdate,".Map.Results.csv", sep = "")

#Load MAP score database
MAP.dfs <- lapply(MAP.path, GetMAPbyID)

MAP.og <- read.csv(MAP.path[1])

MAP.df <- GetMAPbyID(MAP.path[1])

#test names that don't include all goals
discipline.repeats <- c("PRI-READ-Survey w/ Goals CC Intl (Lit/Info,Vocab)V1",
                        "PRI-READ-Survey w/ Goals CC Intl (FoundSkills,LangWriting)V1",
                        "PRI-MATH-Survey w/ Goals CC Intl (Meas/Data,Geometry)V1")

Spring.MAP.df <- read.csv(MAP.path[2]) %>%
        select(Student.ID = StudentID, Discipline, TestName,
               RITScore= TestRITScore, Percentile =TestPercentile,
               Duration.min = TestDurationInMinutes) %>%
        filter(!(TestName %in% discipline.repeats)) %>%
        mutate(key = paste(Student.ID, Discipline)) %>%
        slice(-duplicated(key,fromLast=TRUE)) %>%
        select(everything(), -key, -TestName)
        
#get column of only "TestRIT
cols <- c("RITScore", "Percentile","Duration.min")


Spring.MAP.df2 <- Spring.MAP.df %>% 
        unite("temp", RITScore:Duration.min, sep = "_") %>%
        spread(Discipline, temp) %>%
        separate("Language Usage", paste("Lang", cols, sep = "_"),
                 sep = "_", remove = TRUE, convert = TRUE) %>%
        separate("Mathematics", paste("Math", cols, sep = "_"),
                 sep = "_", remove = TRUE, convert = TRUE) %>%
        separate("Reading", paste("Read", cols, sep = "_"),
                 sep = "_", remove = TRUE, convert = TRUE)


