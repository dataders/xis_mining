
map.path <- "AssessmentResults.csv"

map.df <- read.csv(map.path) %>%
Filter(function(x)!all(is.na(x)), map.df)

map.df <- map.df %>% select(StudentID, Discipline, TestName,
                            TestRITScore, TestStandardError, TestPercentile,
                            TypicalFallToFallGrowth,
                            Goal1Name, Goal1RitScore, Goal1StdErr,
                            Goal2Name, Goal2RitScore, Goal2StdErr,
                            Goal3Name, Goal3RitScore, Goal3StdErr,
                            Goal4Name, Goal4RitScore, Goal4StdErr)