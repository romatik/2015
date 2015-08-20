library(dplyr)
library(likert)
library(scales)
library(psych)
library(reshape)
library(grid)

dataset <- read.csv("../Media/2015/Master_tables/bigtable.csv", na.strings = c("", " ", "No answer", "N/A", "NA"), header = TRUE)
dataset$X <- NULL
dataset$B.2.2.Rate.the.support.received.on.the.following.issues._Inappropriate.conduct.or.sexual.harassment.issues <- NULL
dataset$B.2.2.a.If.you.feel.comfortable.describe.any.inappropriate.conduct.or.sexual.harassment.issues.you.have.witnessed.or.have.been.the.subject.of.and.the.support.you.have.received.The.answers.to.this.question.will.not.be.shared.with.Erasmus.Mundus.course._Open.Ended.Response <- NULL

### ordered levels that were used in the survey
likert_levels <- c("Very unsatisfied", "Somewhat unsatisfied", "Somewhat satisfied", "Very satisfied")
agree_levels <- c("Disagree", "Somewhat disagree", "Somewhat agree", "Agree")

### finding out courses with 10 or more respondents in the dataset
tenormore <- dataset %>%
  select(A.2.Select.the.name.of.Erasmus.Mundus.master.course._Response.) %>%
  group_by(A.2.Select.the.name.of.Erasmus.Mundus.master.course._Response.) %>%
  summarise(respondents = n()) %>%
  filter(respondents > 10)
colnames(tenormore) <- c("Course", "Respondents")
  
### taking only those entries further for analysis
#dataset <- dataset[(dataset$A.2.Select.the.name.of.Erasmus.Mundus.master.course._Response. %in% tenormore$Course),]

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


### function for printing out the likert plot about each individual section of a survey. It also prints out information about 
### Cronbach's alpha level. Can be used further to create similar plots for each individual course.

### x = name of the question to be printed
### dataset = which dataset should be used to extract data from. Default = overall.
questionprint <- function(x, dataset = overall){
  question <- dataset[, substr(names(dataset), 1, nchar(x)) == x]
  colnames(question) <- gsub("\\.", " ", colnames(question)) #making names of questions readable
  name_of_the_question <- gsub("(.*)_(.*)", "\\1", colnames(question)[1]) #storing the name of the section for title of the plot
  name_of_the_question <- substring(name_of_the_question, nchar(x)+1)
  colnames(question) <- gsub("(.*?)_(.*)", "\\2", colnames(question)) #leaving just the dimension name
  
  levels = likert_levels # default is likert_levels
  if (x == "L.6" || x == "L.5" || x == "L.4")
    levels = agree_levels # using agree levels only for relevant questions
  
  ### making sure that levels go in order they were meant to go
  for(i in seq_along(question)) {
    ### this step will also reduce all other answers to NA's
    question[,i] <- factor(question[,i], levels = levels)
  }
  
  ### checking to see to have more than 10 answers in each column, otherwise delete it
  question <- question[, colSums(!is.na(question)) > 10]
  
  ### checking to see if question has more than 1 dimension with 10 or more respondents to proceed. 
  ### Otherwise it doesn't make sense to calculate Cronbach's alpha and plot
  if (dim(question)[2] > 1) {  
  ### calculating Cronbach's alpha
    if (sum(complete.cases(question)) > 0){ # calculating Cronbach's alpha only for cases where complete cases exist
      question_alpha <- psych::alpha(data.matrix(question))
      question_alpha_head <- xtable(question_alpha$total, caption = sprintf("Statistics for %s question", x))
      print.xtable(question_alpha_head, type = "html", file = sprintf("./Question_statistics/%s_alpha.html", x))
      question_alpha_drop <- xtable(question_alpha$alpha.drop, caption = sprintf("What if each individual dimension is dropped for %s question", x))
      print.xtable(question_alpha_drop, type = "html", file = sprintf("./Question_statistics/%s_drop.html", x))
      question_alpha_stats <- xtable(question_alpha$item.stats, caption = sprintf("Summary statistics for %s question", x))
      print.xtable(question_alpha_stats, type = "html", file = sprintf("./Question_statistics/%s_stats.html", x))
    }
    
    ### creating likert-type variable to print it out
    questionl <- likert(question) #creating likert-type variable for plotting
    wrap_function <- wrap_format(120) #wrap-function to print question correctly
    name_of_the_question <- wrap_function(name_of_the_question)
  
    
    ### printing out the file
    p <- plot(questionl, 
              plot.percents = TRUE, # displaying percents for each answer
              plot.percent.low = FALSE,  # displaying cummulative percents for negative answers
              plot.percent.high = FALSE, # displaying cummulative percents for positive answers
              centered = FALSE, # stretcthing the bar from left to right
              text.size = 1.5, 
              wrap = 50, # wrap statement for dimension names
              legend.position = "top") + 
      ggtitle(name_of_the_question) + # title of the question
      theme(text = element_text(size = 7), # setting the text size of the plot
            plot.margin = unit(c(0, 0.8, 0.3, 0), "lines"), # decreasing white space around the plot
            legend.margin = unit(0, "lines"), # deleting space around legend
            legend.key.size = unit(0.5, "lines"), # decreasing size of legend elements
            legend.background = element_rect(colour = "gray", fill = NA, size = 0.1)) + # adding a frame around the legend
      geom_hline(yintercept=seq(25, 75, by=25), linetype = "dashed", size = 0.2) + # adding dashed lines at 25, 50, 75% to make it more clear
      coord_flip(ylim = c(-1,101)) #reducing white space left to 0 and right to 100
    ggsave(filename = sprintf("./Question_statistics/%s.png", x), plot = p, units = "mm", width = 180, height = (25 + dim(question)[2]*8)) #making graph a little rubbery
  }
}

### questions that need to be printed out
questions <- c('B.1.1', 'B.1.3', 'B.2.1', 'B.2.2', 'C.1', #overall program satisfaction
               "L.4", "L.5", "L.6", 'L.3.a', 'L.2.a', #internship/field experience
               "N.1.1", "N.1.3", "N.2.1", "N.2.2", "N.3.1", "N.4.1", #satisfaction in first university
               "O.1.1", "O.1.3", "O.2.1", "O.2.2", "O.3.1", "O.4.1", #satisfaction in second university
               "P.1.1", "P.1.3", "P.2.1", "P.2.2", "P.3.1", "P.4.1", #satisfaction in third university
               "Q.1.1", "Q.1.3", "Q.2.1", "Q.2.2", "Q.3.1", "Q.4.1") #satisfaction in fourth university

for (i  in seq_along(questions))
  questionprint(questions[i])

mainDir <- "C:/Users/Misha/Dropbox/Projects/EM Internship/Quantitative team/2015/Course_statistics"
for (i in seq(1,5)){
  setwd(file.path(mainDir)) # setting the directory
  
  #subsetting to relevant course
  course_dataset <- dataset[dataset$A.2.Select.the.name.of.Erasmus.Mundus.master.course._Response. == tenormore$Course[i],] 
  
  #storing the name of the course as a character
  subDir <- as.character(tenormore$Course[i]) 
  
  #reating a directory for a course
  ifelse(!dir.exists(file.path(mainDir, subDir)), dir.create(file.path(mainDir, subDir)), FALSE) 
  setwd(file.path(mainDir, subDir))
  
  #creating a directory for questions statistics
  ifelse(!dir.exists("./Question_statistics/"), dir.create(file.path("./Question_statistics/")), FALSE) 
  for (i  in seq_along(questions))
    questionprint(questions[i], dataset = course_dataset)
}