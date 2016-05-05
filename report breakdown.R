#load required libraries, data, and created functions
library(dplyr)
library(tm)
library(tidyr)
library(SnowballC)
library(RWeka)
source("Functions.R")
setwd("/Users/andersswanson/Desktop/comment\ mining")

t1.report.path <-"data/t1 comments.csv"
t2.report.path <-"data/t2 comments.csv"

t1.report <- GetReportsDFfromMBcsv(t1.report.path)
t2.report <- GetReportsDFfromMBcsv(t2.report.path)

t1.grades.stats <- GetMeanBreakdownFromReport(t1.report)
t2.grades.stats <- GetMeanBreakdownFromReport(t2.report)
