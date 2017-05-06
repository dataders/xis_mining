library(dplyr)

norm.ex <- structure(list(Student = structure(1:8, .Label = c("Amy", "Becca", 
        "Carol", "Denise", "Emily", "Fiona", "Gabrielle", "Hailey"), class = "factor"), 
        Teacher = structure(c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L), .Label = c("Blue", 
        "Green"), class = "factor"), S1.Grade = c(57.5, 17.5, 27.5, 
        40, 40, 42.5, 65, 70), S2.Grade = c(75, 32.5, 47.5, 52.5, 
        52.5, 27.5, 60, 52.5), Improvement = c(17.5, 15, 20, 12.5, 
        12.5, -15, -5, -17.5)), .Names = c("Student", "Teacher", 
        "S1.Grade", "S2.Grade", "Improvement"), class = "data.frame", row.names = c(NA, 
        -8L))

by_teacher <- norm.ex %>%
        group_by(Teacher) %>%
        summarize(Improve.m = mean(Improvement), Improve.sd = sd(Improvement))

norm.ex.by_teacher <- left_join(norm.ex, by_teacher, by = "Teacher") %>%
        #normalize the t12.growth by mean & sd of teacher t12.growth
        mutate(Improve.zgrowth = (Improvement - Improve.m)/Improve.sd) %>%
        mutate_each(funs(round(.,3)), -Student, -Teacher) %>%
        select(Student, Teacher, Improvement:Improve.zgrowth)