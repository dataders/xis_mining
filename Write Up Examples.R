library(knitr)
t1.report.ex <- GetReportsDFfromMBcsv("data/t1 comments.csv") %>%
        select(-Student.Comment) 
t1.report.ex$Student.ID <- "1000****"
t1.report.ex$First.Name <- "****"
t1.report.ex$Last.Name <- "****"
t1.report.table <- sample_n(t1.report.ex, 8)
kable(t1.report.table)