#load MB reports
t1.report <- GetReportsDFfromMBcsv("data/t1 comments.csv")
t2.report <- GetReportsDFfromMBcsv("data/t2 comments.csv")

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
        summarize( class.growth.avg = mean(class.growth, na.rm = TRUE))
        


#finding student mean Cri score increase from T1 to T2
t1.grades.stats <- GetMeanBreakdownFromReport(t1.report)
t2.grades.stats <- GetMeanBreakdownFromReport(t2.report)
t3.grades.stats <- GetMeanBreakdownFromReport(t3.report)

t12.grades.stats <- right_join(t1.grades.stats[[1]], t2.grades.stats[[1]],
                               by = "Student.ID") %>%
                        select(Student.ID, t1.avg = avg.x, t2.avg = avg.y) %>%
                        mutate(overall.growth = t2.avg - t1.avg)

by.teacher <- t12.report %>% group_by(Teacher) %>%
        summarise(CriMean.t1 = mean(CriMean.t1, na.rm = TRUE), CriMean.t2 = mean(CriMean.t2, na.rm = TRUE),
                  avg.growth =mean(class.growth, na.rm = TRUE))
by.subject <- t12.report %>% group_by(Subject) %>%
        summarise(CriMean.t1 = mean(CriMean.t1, na.rm = TRUE), CriMean.t2 = mean(CriMean.t2, na.rm = TRUE),
                  avg.growth =mean(class.growth, na.rm = TRUE))
by.grade <- t12.report %>% group_by(Grade.Level) %>%
        summarise(CriMean.t1 = mean(CriMean.t1, na.rm = TRUE), CriMean.t2 = mean(CriMean.t2, na.rm = TRUE),
                  avg.growth =mean(class.growth, na.rm = TRUE))
