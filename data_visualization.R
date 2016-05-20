source("functions.R")

### taking only those entries further for analysis
#dataset <- dataset[(dataset$A.2.name.of.Erasmus.Mundus.master.course. %in% tenormore$Course),]

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
questionprint_grouping("B.1.1", overall, grouping = overall$A.9.Gender._Response, grouping_levels = c("Male", "Female"))


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

#########################################################################################################################################
#dataset with number of programs in different universities
universities <- dataset %>%
  select(A.2.name.of.Erasmus.Mundus.master.course., University.1, University.2, University.3, University.4) %>%
  melt(, id.vars = c("A.2.name.of.Erasmus.Mundus.master.course.")) %>%
  select(A.2.name.of.Erasmus.Mundus.master.course., value) %>%
  unique() %>%
  group_by(value) %>%
  tally() %>%
  filter(n > 2) #change here to change how many programs should be present for further analysis

#dataset to check what programs were hosted by chosen university
programs <- dataset %>%
  select(A.2.name.of.Erasmus.Mundus.master.course., University.1, University.2, University.3, University.4) %>%
  melt(, id.vars = c("A.2.name.of.Erasmus.Mundus.master.course.")) %>%
  select(A.2.name.of.Erasmus.Mundus.master.course., value) %>%
  unique() 

#########################################################################################################################################
#text analysis
require(stringi)
text_data <- read.csv("./Reports/text_data.csv", encoding = "UTF-8")
text <- text_data %>% select(starts_with("X"))
word_counts <- as.data.frame(apply(text, 2, function(x) stri_count(x, regex = "\\S+")))

#########################################################################################################################################
z <- dataset %>%
  select(A.2.name.of.Erasmus.Mundus.master.course., University.1, University.2, University.3, University.4)%>%
  melt(na.rm = TRUE, id = "A.2.name.of.Erasmus.Mundus.master.course.") %>%
  group_by(A.2.name.of.Erasmus.Mundus.master.course.) %>%
  distinct(value)

z$variable <- NULL
temp <- dcast(z, A.2.name.of.Erasmus.Mundus.master.course. ~ value)
df_args <- c(temp, sep ="; ")
f <- do.call(paste, df_args)
f <- gsub(" NA;", "", f)
write.csv(file = "universities10.csv", f)
