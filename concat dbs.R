setwd("/Users/andersswanson/Desktop/comment\ mining")
source("Functions.R")

#load Admin Plus Datebase
xis.db <- GetStudentDBfromAPxlsx("data/xis db.xlsx")


#load MB reports
t1.report <- GetReportsDFfromMBcsv("data/t1 comments.csv")
t2.report <- GetReportsDFfromMBcsv("data/t2 comments.csv")

#Load MAP score database
MAP.df <- GetMAPbyID("data/AssessmentResults.csv")


#finding subset comments where student made improvement
by.cols <- c("Student.ID", "Last.Name", "First.Name",
             "Grade.Level", "Subject", "Teacher")
t12.report <- merge(t1.report, t2.report,
                    by = by.cols, suffixes = c(".t1", ".t2")) %>%
        mutate(class.growth = CriMean.t2 - CriMean.t1) %>%
        within(class.growth.quartile <- as.integer(cut(class.growth,
                                                       quantile(class.growth, probs=0:4/4,
                                                                na.rm = TRUE),
                                                       include.lowest=TRUE)))
t12.report.byID <- t12.report %>% group_by(Student.ID) %>%
        summarize( avg.CriMean.t1 = mean(CriMean.t1, na.rm = TRUE),
                   avg.CriMean.t2 = mean(CriMean.t2, na.rm = TRUE),
                   growth.avg = mean(class.growth, na.rm = TRUE)) %>%
        within(growth.quartile <- as.integer(cut(growth.avg,
                                                 quantile(growth.avg, probs=0:4/4,
                                                          na.rm = TRUE),
                                                 include.lowest=TRUE)))

#joining!
MB.MAP.db <- inner_join(t12.report.byID, MAP.df, by="Student.ID")
sec.y <- 60*60*24*365
all <- right_join(xis.db, MB.MAP.db, by="Student.ID") %>%
        mutate(
                Age = as.period(interval(start = BIRTH.DATE,
                                         end = today())),
                Years.at.XIS = as.period(interval(start = ENTRY.DAY.1,
                                                  end = today()))) %>%
        mutate_each(funs(as.numeric), GRADE.LEVEL) %>%
        mutate_each(funs(as.numeric), starts_with("Math")) %>%
        mutate_each(funs(as.numeric), starts_with("Lang")) %>%
        mutate_each(funs(as.numeric), starts_with("Read")) %>%
        mutate(career_pct = 100 * Years.at.XIS / (Age - 4),
               Years.XIS.int = period_to_seconds(Years.at.XIS)/sec.y)
               



