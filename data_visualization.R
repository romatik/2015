library(dplyr)
library(likert)
library(scales)
library(psych)
library(reshape)
library(grid)
library(RColorBrewer)
library(tidyr)
library(rworldmap)
library(countrycode)
library(classInt)
library(RColorBrewer)
library(stringr)

setwd("C:/Users/Misha/Dropbox/Projects/EM Internship/Quantitative team/2015")
source("functions.R")
dataset <- read.csv("../Media/2015/Master_tables/bigtable.csv", na.strings = c("", " ", "No answer", "N/A", "NA", "Other (please specify)"), header = TRUE)
dataset$X <- NULL
dataset$B.2.2.a.If.you.feel.comfortable.describe.any.inappropriate.conduct.or.sexual.harassment.issues.you.have.witnessed.or.have.been.the.subject.of.and.the.support.you.have.received.The.answers.to.this.question.will.not.be.shared.with.Erasmus.Mundus.course._Open.Ended.Response <- NULL

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
  
### taking only those entries further for analysis
#dataset <- dataset[(dataset$A.2.name.of.Erasmus.Mundus.master.course. %in% tenormore$Course),]

overall <- select(dataset,
                  RespondentID_,
                  starts_with("A."),
                  starts_with("X."),
                  starts_with("B."),
                  starts_with("C."),
                  starts_with("L."),
                  starts_with("N"),
                  starts_with("O"),
                  starts_with("P"),
                  starts_with("Q"),
                  I.am.currently._Response)

#########################################################################################################################################
### creating summary plots for every question in the dataset
for (i  in seq_along(questions))
  questionprint(questions[i])

#########################################################################################################################################
### creating boxplots for every question in the dataset
for (i  in seq_along(questions))
  means_printing(questions[i])


#########################################################################################################################################
### creating summary plots for every course and every question in the dataset
mainDir <- "C:/Users/Misha/Dropbox/Projects/EM Internship/Quantitative team/2015/Course_statistics"
for (i in seq_along(tenormore$Course)){
  setwd(file.path(mainDir)) # setting the directory
  
  #subsetting to relevant course
  course_dataset <- dataset[dataset$A.2.name.of.Erasmus.Mundus.master.course. == tenormore$Course[i],] 
  
  #storing the name of the course as a character
  subDir <- make.names(as.character(tenormore$Course[i])) 
  
  #reating a directory for a course
  ifelse(!dir.exists(file.path(mainDir, subDir)), dir.create(file.path(mainDir, subDir)), FALSE) 
  setwd(file.path(mainDir, subDir))
  
  #creating a directory for questions statistics
  ifelse(!dir.exists("./Question_statistics/"), dir.create(file.path("./Question_statistics/")), FALSE) 
  for (i  in seq_along(questions))
    questionprint(questions[i], dataset = course_dataset)
}

#########################################################################################################################################
### creating heatmaps for means of every question and every course
setwd("C:/Users/Misha/Dropbox/Projects/EM Internship/Quantitative team/2015")

scaled <- FALSE

for (i in seq_along(questions)){
  means <- means_prepare(questions[i])[[1]]
  vector <- means_prepare(questions[i])[[2]]
  
  heatmap_printing(means, vector, scaled = scaled)
}

# plot_ly(z = means_matrix, x = colnames(means_matrix), y = rownames(means_matrix), type = "heatmap")
# https://plot.ly/~mikhail.balyasin/26.embed

#########################################################################################################################################
### example of printing with a grouping
questionprint_grouping("B.1.1", grouping = overall$A.9.Gender._Response, grouping_levels = c("Male", "Female"))


#########################################################################################################################################
### plotting the overall satisfaction questions
z <- c("C.3.Please.rate.academic.satisfaction.with.EM.course._", 
       "C.4.Please.rate.overall.satisfaction.with.EM.course._",
       "N.5.1.Please.rate.overall.satisfaction.at.the.first.university._",
       "O.5.1.Please.rate.overall.satisfaction.at.the.second.university._",
       "P.5.1.Please.rate.overall.satisfaction.at.the.third.university._",
       "Q.5.1.Please.rate.overall.satisfaction.at.the.fourth.university._")
question <- dataset[, z]
for(i in seq_along(question)) {
  ### this step will also reduce all other answers to NA's
  question[,i] <- factor(question[,i], levels = likert_levels)
}
names(question) <- c("Overall academic satisfaction", 
                     "Overall satisfaction",
                     "Overall satisfaction in 1st university",
                     "Overall satisfaction in 2nd university",
                     "Overall satisfaction in 3rd university",
                     "Overall satisfaction in 4th university")
question <- question[, colSums(!is.na(question)) >= 10]

p <- plot_question(question, "Overall satisfaction")
plot(p)

#########################################################################################################################################
### plotting maps with respondents

#reading dataset about IP addresses
ip <- read.csv("../Media/2015/Master_tables/ip.csv")
#combining data to have countries-respondents pairs
df <- ip %>% select(country_name) %>% group_by(country_name) %>%  summarise(respondents = n())
#pre-processing for plotting
jdf <- joinCountryData2Map(df, joinCode = "NAME", nameJoinColumn = "country_name")

#dividing data in 5 categories with equal number of countries
classInt <- classIntervals(df[["respondents"]], n = 5, style = "jenks")
catMethod = classInt[["brks"]]
colourPalette <- brewer.pal(5, "YlGn")
mapParams <- mapCountryData(jdf, nameColumnToPlot = "respondents", colourPalette = colourPalette, catMethod = catMethod, addLegend = FALSE)
do.call(addMapLegend, c(mapParams, legendLabels = "all", legendWidth = 0.5, legendIntervals = "data", legendMar = 6.5)) #adding legend to a plot

### plotting nationalities
overalldf <- overall %>%
  select(A.7.What.is.nationality.please.choose.one.only._Response) %>%
  group_by(A.7.What.is.nationality.please.choose.one.only._Response) %>%
  summarise(respondents = n())
names(overalldf) <- c("Country", "respondents")

overalldf$iso3 <- countrycode(overalldf$Country, "country.name", "iso3c")
overalljdf <- joinCountryData2Map(overalldf, joinCode = "ISO3", nameJoinColumn = "iso3")
classInt <- classIntervals(overalldf[["respondents"]], n = 5, style = "jenks")
catMethod = classInt[["brks"]]
colourPalette <- brewer.pal(5, "YlGn")
mapParams <- mapCountryData(overalljdf, nameColumnToPlot = "respondents", colourPalette = colourPalette, catMethod = catMethod, addLegend = FALSE)
do.call(addMapLegend, c(mapParams, legendLabels = "all", legendWidth = 0.5, legendIntervals = "data", legendMar = 6.5))
#plotting with gvis
Geo <- gvisGeoChart(overalldf, colorvar = "respondents", locationvar = "Country", options = list(projection = "kavrayskiy-vii"))
plot(Geo)

###creating table with regions
overalldf$iso3 <- countrycode(overalldf$Country, "country.name", "iso3c")
data("countryExData")
z <- merge(countryExData, overalldf, by.x = "ISO3V10", by.y = "iso3")
subcontinents <- country2Region(z, nameDataColumn = "respondents", joinCode = "ISO3", nameJoinColumn = "ISO3V10", regionType = "Stern", FUN = "sum")
subcontinents$percent <- subcontinents$sumrespondentsbyStern*100/sum(subcontinents$sumrespondentsbyStern)

course_dataset <- dataset[dataset$A.2.Select.the.name.of.Erasmus.Mundus.master.course._Response. == tenormore$Course[1],] 
df <- comparative_df(questions[1], course_dataset)
z <- xtable(df, caption = sprintf("Summary statistics for %s question", questions[1]), digits = c(0,0,2,2,2,2,2,2,2), type = "html")
align(z) <- "|p{5.5cm}|cc|c|lllll|"
print(z, type = "html")
