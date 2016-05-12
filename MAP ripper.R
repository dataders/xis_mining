#author: Anders Swanson
#load required libraries, data, and created functions
library(dplyr)
library(tidyr)
library(xlsx)
library(lubridate)

setwd("/Users/andersswanson/Desktop/comment\ mining")
source("Functions.R")

map.path <- "data/AssessmentResults.csv"

df <- GetMAPbyID(map.path)