setwd("/Users/andersswanson/Desktop/comment\ mining")
source("Functions.R")


#wrappers for mean and sd with na.rm = TRUE
av <- function(x) {
        mean(x, na.rm = TRUE)
}
s <- function(x) {
        sd(x, na.rm = TRUE)
}


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

#load MB reports
t1.report <- GetReportsDFfromMBcsv("data/t1 comments.csv")
t2.report <- GetReportsDFfromMBcsv("data/t2 comments.csv")
t3.report <- GetReportsDFfromMBcsv("data/t3 comments.csv")

#name columns how I want despite ugly double join 
by.cols <- c("Student.ID", "Last.Name", "First.Name", "Class.ID",
             "Grade.Level", "Subject", "Teacher")
s <- c("Cri.A", "Cri.B", "Cri.C", "Cri.D", "Sum", "CriMean", "Student.Comment")
t <-c(".t1", ".t2", ".t3")
t.stats <- unlist(lapply(t,function(x) {paste(s,x, sep = "")}))
year.report.cols <- c(by.cols, t.stats)


year.report <- setNames(
        #join t1 to t2 then join result to t2
        left_join(t1.report, t2.report, by = by.cols) %>% 
                left_join(t3.report, by = by.cols),
        #name columns based on above
        year.report.cols)

year.report <- year.report %>%
        #make an "idx column to find and remove duplicates
        mutate(idx = paste(Student.ID, Subject, Teacher)) %>%
        distinct(idx) %>%
        select(everything(), -idx) %>%
        #add improvement from t1->t3
        mutate(t12.growth = CriMean.t3 - CriMean.t1,
               t23.growth = CriMean.t3 - CriMean.t2,
               t13.growth = CriMean.t3 - CriMean.t1)



math.MB.report <- year.report %>% 
        filter(Subject == "Standard mathematics" | 
                       Subject == "Extended mathematics")

write.csv(math.MB.report, "share/15-16.Math.MB.year.report.csv")

xis.data <- xis.db %>% select(Student.ID:Father.speaks)





year.report.byID <- year.report %>% group_by(Student.ID, Last.Name, First.Name, Grade.Level) %>%
        summarize( avg.CriMean.t1 = av(CriMean.t1),
                   avg.CriMean.t2 = av(CriMean.t2),
                   avg.CriMean.t3 = av(CriMean.t3),
                   growth.avg = av(t13.growth)) %>%

MAP.tara <- year.report.byID %>% inner_join(MAP.DIFF, by = "Student.ID")

write.csv(MAP.tara, "share/MBCriMean+MAP.csv")


