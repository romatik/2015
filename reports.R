library(dplyr)
library(likert)
library(scales)
library(psych)
library(reshape)
library(grid)
library(RColorBrewer)

#http://blog.revolutionanalytics.com/2012/09/how-to-use-your-favorite-fonts-in-r-charts.html
library(extrafont)
loadfonts()
Sys.setenv(R_GSCMD = "C:/Program Files/gs/gs9.16/bin/gswin64c.exe")

setwd("C:/Users/Misha/Dropbox/Projects/EM Internship/Quantitative team/2015")
source("functions.R")

dataset <- read.csv("../Media/2015/Master_tables/bigtable.csv", na.strings = c("", " ", "No answer", "N/A", "NA"), header = TRUE)
dataset$X <- NULL

### ordered levels that were used in the survey
likert_levels <- c("Very unsatisfied", "Somewhat unsatisfied", "Somewhat satisfied", "Very satisfied")
agree_levels <- c("Disagree", "Somewhat disagree", "Somewhat agree", "Agree")

### questions that need to be printed out
questions <- c('B.1.1', 'B.1.3', 'B.2.1', 'B.2.2', 'C.1', #overall program satisfaction
               "L.4", "L.5", "L.6", 'L.3.a', 'L.2.a', #internship/field experience
               "N.1.1", "N.1.3", "N.2.1", "N.2.2", "N.3.1", "N.4.1", #satisfaction in first university
               "O.1.1", "O.1.3", "O.2.1", "O.2.2", "O.3.1", "O.4.1", #satisfaction in second university
               "P.1.1", "P.1.3", "P.2.1", "P.2.2", "P.3.1", "P.4.1", #satisfaction in third university
               "Q.1.1", "Q.1.3", "Q.2.1", "Q.2.2", "Q.3.1", "Q.4.1") #satisfaction in fourth university

### finding out courses with 10 or more respondents in the dataset
tenormore <- dataset %>%
  select(A.2.name.of.Erasmus.Mundus.master.course.) %>%
  group_by(A.2.name.of.Erasmus.Mundus.master.course.) %>%
  summarise(respondents = n()) %>%
  filter(respondents >= 10)
colnames(tenormore) <- c("Course", "Respondents")
#dataset <- dataset[(dataset$A.2.name.of.Erasmus.Mundus.master.course. %in% tenormore$Course),]

source("functions.R")
#http://reed.edu/data-at-reed/software/R/markdown_multiple_reports.html
for (i in seq_along(1:2)){
  course_dataset <- dataset[dataset$A.2.name.of.Erasmus.Mundus.master.course. == tenormore$Course[i],] 

  rmarkdown::render('report_script.Rmd',  # file 2
                    output_format = "pdf_document",
                    output_file =  paste("report_", as.character(tenormore$Course[i]), '_', Sys.Date(), ".pdf", sep=''), 
                    output_dir = './test/reports')
  #embed_fonts(paste("report_", as.character(tenormore$Course[i]), '_', Sys.Date(), ".pdf", sep=''))
}