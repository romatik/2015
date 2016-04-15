library(plyr)
library(extrafont)

#http://blog.revolutionanalytics.com/2012/09/how-to-use-your-favorite-fonts-in-r-charts.html
loadfonts()
Sys.setenv(R_GSCMD = "C:/Program Files/gs/gs9.16/bin/gswin64c.exe")

source("functions.R")
text_data <- read.csv("./Reports/text_data.csv", encoding = "UTF-8")
text_data$Proofreader <- NULL
names(text_data)[1] <- "Course"

today_date <- as.character(format(Sys.Date(), "%d %b %Y"))

#http://reed.edu/data-at-reed/software/R/markdown_multiple_reports.html
for (i in 16:16){
  course_dataset <- dataset[dataset$A.2.name.of.Erasmus.Mundus.master.course. == tenormore$Course[i],]
  text_dataset <- text_data[text_data$Course == as.character(tenormore$Course)[i],]

  rmarkdown::render('./Reports/report_script.Rmd',  # file 2
                    output_format = "pdf_document",
                    output_file =  paste(as.character(tenormore$Course[i]), '_', Sys.Date(), ".pdf", sep=''), 
                    output_dir = './Reports/courses')
  #embed_fonts(paste("report_", as.character(tenormore$Course[i]), '_', Sys.Date(), ".pdf", sep=''))
}


#####################################################################
### for the second phase of the application process
rmarkdown::render('./Reports/second_phase.Rmd',
                  output_format = "pdf_document",
                  output_file =  paste("second_phase.pdf", sep=''))
