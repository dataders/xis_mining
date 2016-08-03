#set wd and paths and source functions
source("Functions.R")
setwd("/Users/andersswanson/Desktop/comment\ mining")

tmp <- read.csv("stack_overflow/example2.csv", stringsAsFactors = FALSE)

works <- GetPrunedList(tmp, 30)