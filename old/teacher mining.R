
LoadItUp()

#create empty list and name each item after name of teacher
teacher.comments <- vector("list", length(teachers))
names(teacher.comments) <- teachers

#loop through each teacher to subset grouped df to concat all comments into one string
for (i in teachers) {
  sub <- reports %>%
    filter(Teacher == i)
  teacher.comments[i] <- sub$Student.Comment %>%
    paste(collapse = " ")
}

# Make a corpus and clean it w/ pre-defined functions
tch.corpus <- ToCorpus(teacher.comments)
tch.corpus <- CorpusClean(tch.corpus)

# change set author tag of indv documents in corpus
for (i in 1:length(teachers)) {
  meta(tch.corpus[[i]], tag = "author") <- teachers[i]
}

idx <- tch.corpus %>% meta("author") == "Anders Swanson"
10001828

CompIndv2All("teacher", "Tara Lee")