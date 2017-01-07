
GetT12StdGrowthfromYearReport <- function() {

        #load MB reports
        t1.report <- GetReportsDFfromMBcsv("data/t1 comments.csv")
        t2.report <- GetReportsDFfromMBcsv("data/t2 comments.csv")
        t3.report <- GetReportsDFfromMBcsv("data/t3 comments.csv")
        
        #create vector of columnnames to counteract ugly double join 
        by.cols <- c("Student.ID", "Last.Name", "First.Name", "Class.ID",
                     "Grade.Level", "Subject", "Teacher")
        s <- c("Cri.A", "Cri.B", "Cri.C", "Cri.D", "Sum", "CriMean", "Student.Comment")
        t <-c(".t1", ".t2", ".t3")
        t.stats <- unlist(lapply(t,function(x) {paste(s,x, sep = "")}))
        year.report.cols <- c(by.cols, t.stats)
        
        #join twice, setnames to desired column names
        year.report <- setNames(
                #join t1 to t2 then join result to t2
                left_join(t1.report, t2.report, by = by.cols) %>% 
                        left_join(t3.report, by = by.cols),
                #name columns based on above
                year.report.cols)
        
        #remove duplicate and add improvement metrics
        year.report <- year.report %>%
                #make an "idx column to find and remove duplicates
                mutate(idx = paste(Student.ID, Subject, Teacher)) %>%
                distinct(idx, .keep_all = TRUE) %>%
                select(everything(), -idx) %>%
                #add improvement from t1->t3
                mutate(t12.growth = CriMean.t2 - CriMean.t1,
                       t23.growth = CriMean.t3 - CriMean.t2,
                       t13.growth = CriMean.t3 - CriMean.t1) %>%
                mutate_each(funs(round(.,3)), starts_with("CriMean.t"), ends_with(".growth"))
        
        #wrappers for mean and sd with na.rm = TRUE
        av <- function(x) {
                mean(x, na.rm = TRUE)
        }
        s <- function(x) {
                sd(x, na.rm = TRUE)
        }
        
        
        # # Teacher Stats -----------------------------------------------------------
        # 
        # #get teacher mean&sd for t1, t2, t3,
        # #t1 to t2, t2 to t3, and t1 to t3 growth
        # by_teacher <- year.report %>%
        #         group_by(Teacher) %>%
        #         summarize(t1.m = av(CriMean.t1), t1.s = s(CriMean.t1),
        #                   t2.m = av(CriMean.t2), t2.s = s(CriMean.t2),
        #                   t3.m = av(CriMean.t3), t3.s = s(CriMean.t3),
        #                   t12.m = av(t12.growth), t12.s = s(t12.growth),
        #                   t23.m = av(t23.growth), t23.s = s(t23.growth),
        #                   t13.m = av(t13.growth), t13.s = s(t13.growth)) %>%
        #         mutate_each(funs(round(.,2)), -Teacher)
        # 
        # #select only year growth mean&sd columns
        # by_teacher.t13 <- by_teacher %>%
        #         select(Teacher, starts_with("t13."))
        # 
        # #add year growth mean&sd columns to year.report
        # year.report.byteacher <- left_join(year.report, by_teacher.t13, by = "Teacher") %>%
        #         #normalize the t13.growth by mean & sd of teacher t13.growth
        #         mutate(t13.zgrowth = (t13.growth - t13.m)/t13.s)
        
        
        # Class Stats -------------------------------------------------------------
        
        by_ClassID <- year.report %>%
                group_by(Class.ID) %>%
                summarize(t1.m = av(CriMean.t1), t1.s = s(CriMean.t1),
                          t2.m = av(CriMean.t2), t2.s = s(CriMean.t2),
                          t3.m = av(CriMean.t3), t3.s = s(CriMean.t3),
                          t12.m = av(t12.growth), t12.s = s(t12.growth),
                          t23.m = av(t23.growth), t23.s = s(t23.growth),
                          t13.m = av(t13.growth), t13.s = s(t13.growth)) %>%
                mutate_each(funs(round(.,2)), -Class.ID)
        
        #select only year growth mean&sd columns
        by_class.t12 <- by_ClassID %>%
                select(Class.ID, starts_with("t12."))
        
        year.report.byclass <- left_join(year.report, by_class.t12, by = "Class.ID") %>%
                #normalize the t12.growth by mean & sd of teacher t12.growth
                mutate(t12.zgrowth = (t12.growth - t12.m)/t12.s)
        
        
        # # Subject Stats -----------------------------------------------------------
        # 
        # by_subject <- year.report %>%
        #         group_by(Subject) %>%
        #         summarize(t1.m = av(CriMean.t1), 
        #                   t2.m = av(CriMean.t2),
        #                   av(CriMean.t3))
        
        year.report.byclass

}