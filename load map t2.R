setwd("/Users/andersswanson/Desktop/comment\ mining")
source("Functions.R")

MAP.testdate <- c("2015Fall", "2016Spring")
MAP.path <- paste("data/", MAP.testdate,".Map.Results.csv", sep = "")


#Load MAP score database

MAP.og <- read.csv(MAP.path[1])

MAPs <- lapply(MAP.path, GetMAPbyID)
Spring.MAP.df <- GetMAPbyID(MAP.path[2])

