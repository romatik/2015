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
#setwd("~/Dropbox/Projects/EM Internship/Quantitative team/2015")

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
dataset <- dataset[(dataset$A.2.name.of.Erasmus.Mundus.master.course. %in% tenormore$Course),]

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


# http://stackoverflow.com/questions/32136304/conditional-calculation-of-mean
# function calculates the mean only if there are 10 or more respondents to each individual question
f1 <- function(x) if(sum(!is.na(x)) >= 10) mean(as.numeric(x), na.rm=TRUE) else NA_real_

#function calculates number of respondents (not NA)
f2 <- function(x) sum(!is.na(x))

questionprint <- function(x, dataset = overall, save = TRUE, name_of_the_question = NULL){
  ### function for printing out the likert plot about each individual section of a survey. It also prints out information about 
  ### Cronbach's alpha level. Can be used further to create similar plots for each individual course.
  
  ### x = name of the question to be printed
  ### dataset = which dataset should be used to extract data from. Default = overall.
  ### save = flag to indicate whether to save or print the plot. Default is TRUE (plot will be saved in the current directory in sub-folder 
  ### "Question_statistics")
  
  z <- question_prepare(x, dataset)
  question <- z[[1]]
  name_of_the_question <- paste0(z[[2]], name_of_the_question)
#   if(is.null(name_of_the_question))
#     name_of_the_question <- z[[2]] #name of the question if no title is provided
  
  ### checking to see if question has more than 1 dimension with 10 or more respondents to proceed. 
  ### Otherwise it doesn't make sense to calculate Cronbach's alpha and plot
  if (!is.null(dim(question)[2])){
    if (dim(question)[2] > 1) {
      
      ### calculating Cronbach's alpha. If there is an error it won't print out anything
      #try(printing_alpha(x, question))
      
      wrap_function <- wrap_format(85) #wrap-function to print question correctly
      name_of_the_question <- wrap_function(name_of_the_question)
      
      p <- plot_question(question, name_of_the_question)
      if(save){
        ggsave(filename = sprintf("./Question_statistics/%s.png", x), plot = p, units = "mm", width = 180, height = (25 + dim(question)[2]*8)) #making graph a little rubbery
      } else {
        return(p)
      }
    } else
      return (NULL)
  }
}

questionprint_grouping <- function(x, dataset = overall, grouping, grouping_levels){
  ### x = name of the question to be printed
  ### dataset = which dataset should be used to extract data from. Default = overall.
  ### grouping = grouping variable to be used to display the results. MUST be the same length as the dataset itself.
  ### grouping_levels = grouping levels to be used to display. Function automatically deletes rows with levels not present in grouping_levels variable.
  
  ### returns nothing, prints the plot directly to the window.
  z <- question_prepare(x, dataset)
  question <- z[[1]]
  name_of_the_question <- z[[2]]
  
  grouping_variable <- factor(grouping, levels = grouping_levels)
  question <- question[complete.cases(grouping_variable), ]
  grouping_variable <- grouping_variable[complete.cases(grouping_variable)]
  questionl <- likert(question, grouping = grouping_variable)
  
  plot(questionl) +
    ggtitle(name_of_the_question)
}

question_prepare <- function(x, dataset = overall){
  # function to prepare a dataset for future use
  # x = name of the question
  # dataset = dataset that needs to be transformed (by default - overall)

  # returns a list with 2 elements:
  # First element is a transformed dataset. All column names are readable. All columns are transformed into respective levels.
  #   Each column has at least 10 answers in them.
  # Second element is the name of the question that will be used as a title for the plot.
  question <- dataset[, substr(names(dataset), 1, nchar(x)) == x]
  colnames(question) <- gsub("\\.", " ", colnames(question)) #making names of questions readable
  name_of_the_question <- extract_name(x, dataset)
  colnames(question) <- gsub("(.*?)_(.*)", "\\2", colnames(question)) #leaving just the dimension name
  
  
  levels <- likert_levels # default is likert_levels
  if (x == "L.6" || x == "L.5" || x == "L.4")
    levels <- agree_levels # using agree levels only for relevant questions
  
  ### making sure that levels go in order they were meant to go
  for(i in seq_along(question)) {
    ### this step will also reduce all other answers to NA's
    question[,i] <- factor(question[,i], levels = levels)
  }
  
  ### checking to see to have 10 or more answers in each column, otherwise delete it
  question <- question[, colSums(!is.na(question)) >= 10]
  output <- list(question, name_of_the_question)
  return(output)
}

extract_name <- function(x, dataset = overall){
  #extracts name of the question and returns it
  
  ### x = string containing the identifier of the question (e.g. "B.1.3")
  question <- dataset[, substr(names(dataset), 1, nchar(x)) == x]
  
  name_of_the_question <- gsub("(.*)_(.*)", "\\1", colnames(question)[1]) #storing the name of the section for title of the plot
  name_of_the_question <- substring(name_of_the_question, nchar(x)+1)
  name_of_the_question <- gsub("\\.", " ", name_of_the_question)
  name_of_the_question <- gsub("the first", "this", name_of_the_question)
  
  return(name_of_the_question)
}

printing_alpha <- function(x, question_dataset, saved = TRUE){
  ### Function takes in name of the quetions, dataset containing information about the question and prints out information about Cronbach alpha
  ### either to a specified folder "Question_statistics" or directly to a prompt. Later can be used to generate reports.
  
  ### x = name of the question
  ### question_dataset = dataset containing information only about given question
  ### saved = logical flag indicating whether to just save the tables in a folder, or print them
  if (sum(complete.cases(question_dataset)) > 0){ # calculating Cronbach's alpha only for cases where complete cases exist
    question_alpha <- psych::alpha(data.matrix(question_dataset))
    question_alpha_head <- xtable(question_alpha$total, caption = sprintf("Statistics for %s question", x), digits = 2)
    question_alpha_drop <- xtable(question_alpha$alpha.drop, caption = sprintf("What if each individual dimension is dropped for %s question", x), digits = 2)
    question_alpha_stats <- xtable(question_alpha$item.stats, caption = sprintf("Summary statistics for %s question", x), digits = 2)
    if(saved){
      print.xtable(question_alpha_head, type = "html", file = sprintf("./Question_statistics/%s_alpha.html", x))
      print.xtable(question_alpha_drop, type = "html", file = sprintf("./Question_statistics/%s_drop.html", x))
      print.xtable(question_alpha_stats, type = "html", file = sprintf("./Question_statistics/%s_stats.html", x))
    } else {
      xtable(question_alpha$item.stats, caption = sprintf("Summary statistics for %s question", x), digits = 2, type = "html")
    }
  }
}


comparative_df <- function(x, course_dataset){
  ### function to create a dataframe to compare course with all other courses on a given question. Takes in a question and returns a data frame
  ### with summary statistics for both a course and overall for all courses.
  
  ### x = name of the question.
  overall_dataset <- question_prepare(x, dataset)[[1]]
  question_dataset <- question_prepare(x, course_dataset)[[1]]
  
  #this step is needed to properly calculate the height of the plot for individual universities
  names(overall_dataset) <- gsub(pattern = "the first", replacement = "this", names(overall_dataset))

  #calculating means for a specific course
  means_question <- question_dataset %>% 
    summarise_each(funs(f1, f2)) %>%
    gather(variable, value) %>%
    separate(variable, c("var", "stat"), sep = "\\_f") %>%
    spread(var, value)%>%
    t() %>%
    as.data.frame()
  
  #cleaning the means_question dataset to use it in a table
  names(means_question) <- c("Mean", "Respondents")
  means_question <- means_question[2:nrow(means_question),]
  means_question$Mean <- round(as.numeric(as.character(means_question$Mean)), 2)
  means_question$Respondents <- as.numeric(as.character(means_question$Respondents))
  
  #calculating means for the entire dataset
  means_overall <- overall_dataset %>%
    summarise_each(funs(f1))%>%
    t()%>%
    as.data.frame()
  names(means_overall) <- c("Mean for all courses")
  
  #merging the 
  df <- merge(means_question, means_overall, by = 0, all = TRUE)
  rownames(df) <- df$Row.names
  df$Row.names <- NULL

  #calculating means for each course and each question to create a quantile variable
  means_overall_each <- means_prepare(x)[[1]]
  #this step is needed to properly calculate the height of the plot for individual universities
  names(means_overall_each) <- gsub(pattern = "the first", replacement = "this", names(means_overall_each)) 
  quantiles <- as.data.frame(t(apply(X = means_overall_each, FUN = function(x) quantile(x, na.rm = TRUE), MARGIN = 2)))
  quartile_info <- merge(quantiles, means_question, by = 0, all = TRUE)
  rownames(quartile_info) <- quartile_info$Row.names
  quartile_info$Row.names <- NULL
  quartile_info$`100%` <- round(quartile_info$`100%`, 2) + 0.01 #just to make sure that highest mean is recognized by cut
  quartile_info$Mean <- quartile_info$Mean + 0.01 #just to make sure that lowest means are also recognized by cut
  quartile_info$quartile <- apply(quartile_info, 1, function(x) cut(x[6], x[1:5], include.lowest = TRUE, labels = FALSE))
  quartile_info <- quartile_info["quartile"]
  
  clean_quantiles <- quantiles #copying quantiles to a different data frame to create ranges
  clean_quantiles$first <- apply(quantiles, 1, function(x) sprintf("%.2f - %.2f", x[1], x[2])) #creating ranges
  clean_quantiles$second <- apply(quantiles, 1, function(x) sprintf("%.2f - %.2f", x[2] + 0.01, x[3])) #+0.01 to create non-overlapping range.
  clean_quantiles$third <- apply(quantiles, 1, function(x) sprintf("%.2f - %.2f", x[3] + 0.01, x[4]))
  clean_quantiles$fourth <- apply(quantiles, 1, function(x) sprintf("%.2f - %.2f", x[4] + 0.01, x[5]))
  clean_quantiles <- clean_quantiles[c("first", "second", "third", "fourth")] #deleting unnecessary columns

  df <- merge(df, clean_quantiles, by = 0, all = TRUE)
  rownames(df) <- df$Row.names
  df$Row.names <- NULL
  
  df <- merge(df, quartile_info, by = 0, all = TRUE)
  rownames(df) <- df$Row.names
  df$Row.names <- NULL
  
  
  names(df) <- c("Mean", "n", "EM mean", "0\\% - 25\\%", "25\\% - 50\\%", "50\\% - 75\\%", "75\\% - 100\\%", "quartile")
  
  df <- df[complete.cases(df),] #deleting all rows with NA in the mean
  
  #http://stackoverflow.com/questions/24553963/xtable-for-conditional-cell-formatting-significant-p-values-of-table
  for(i in seq(1:nrow(df))){
    df[i, df$quartile[i] + 3] <- paste0("\\colorbox{table-hl}{", df[i, df$quartile[i] + 3], "}")
    
    #highlighting every other row with gray color
    if(i %% 2 == 0)
      rownames(df)[i] <- paste("\\rowcolor{table-hl!40}", rownames(df)[i])
  }
  
  df$quartile <- NULL
  
  
  return(df[,c(2,1,3:ncol(df))])
}

means_prepare <- function(x){
  ### calculates means for the given question. Returns prepared means matrix.
  
  ### x = name of the question.
  
  overall_dataset <- question_prepare(x, dataset)[[1]]

  #calculating means for each course and each question to create a quantile variable
  overall_dataset <- cbind(overall_dataset, dataset$A.2.name.of.Erasmus.Mundus.master.course.)
  names(overall_dataset)[ncol(overall_dataset)] <- "Course.name"
  means <- overall_dataset %>%
    group_by(Course.name) %>%
    summarise_each(funs(f1))
  
  
  #storing the names of the courses for future use
  rownames_store <- means$Course.name
  means$Course.name <- NULL
  
  #logical vector to find out rows with all NA's
  vector <- !!rowSums(!is.na(means)) 
  
  means <- means[vector,] #deleting the rows with all NA's
  rownames(means) <- rownames_store[vector] #writing down the names of the courses
  colnames(means) <- gsub("\\.", " ", colnames(means)) #making names of questions readable
  colnames(means) <- gsub("(.*?)_(.*)", "\\2", colnames(means)) #leaving just the dimension name
#   wrap_function_x <- wrap_format(35)
#   colnames(means) <- wrap_function_x(colnames(means))
#   wrap_function_y <- wrap_format(50)
#   rownames(means) <- wrap_function_y(rownames(means))
  
  return(list(means, vector))
}

heatmap_printing <- function(means, vector, scaled = TRUE, saved = TRUE){
  ### function to print out a heatmap for any given question. Used in conjuction with means_prepare.
  
  ### means = data frame containing means for any given question. Comes from means_prepare function.
  ### vector = vector that is used to calculate the height and width of the resulting plot.
  ### scaled = logical flag to indicate whether heatmap should be scaled or not. Default is TRUE.
  ### saved = logical flag indicating whether to save or print the graph directly. Default is TRUE.
  
  # different colors depending on scaled/not scaled
  if (scaled){
    mycolors <- brewer.pal(length(c(-Inf,-2:2,Inf)), "BrBG")
  } else {
    mycolors <- brewer.pal(length(seq(1,4, by = 0.5)), "BrBG")
  }
  # scaling the matrix or not
  if (scaled){
    means_matrix <- scale(data.matrix(means))
  } else {
    means_matrix <- data.matrix(means)
  }
  # melted matrix is easier to use with ggplot
  means_matrix_melted <- melt(means_matrix)
  
  
  ### different ways to cut values depending if scaled/not scaled
  if (scaled) {
    means_matrix_melted$value1 <- cut(means_matrix_melted$value, c(-Inf,-2:2,Inf), right = FALSE) 
  } else {
    means_matrix_melted$value1 <- cut(means_matrix_melted$value, seq(1,4, by = 0.5), right = FALSE)
  }
  
  #plotting the heatmap
  p <- ggplot(means_matrix_melted, aes(x = X2, y = X1)) + 
    ggtitle(as.character(questions[i])) +
    geom_tile(aes(fill = value1)) + 
    scale_fill_manual(name = levels(means_matrix_melted$value1), 
                      values = mycolors,
                      na.value = "black", # default color for NA values
                      drop = FALSE) + # not dropping levels with 0 values in them
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),          
          axis.title.x=element_blank(),
          axis.title.y=element_blank()) 
  
  if (saved){
    if(scaled){
    ggsave(filename = sprintf("./Heatmaps_scaled/%s.png", questions[i]), plot = p, units = "mm", width = 250, height = (70 + sum(vector)*4))
    } else { 
      ggsave(filename = sprintf("./Heatmaps/%s.png", questions[i]), plot = p, units = "mm", width = 250, height = (70 + sum(vector)*4))
    }
  } else {print(p)}
}

report_question <- function(question, course_dataset, text_data = NULL){
  ## Function to print out the question in the individual report.
  
  ## question = string, containing the question to be printed.
  ## course_dataset = course dataset to produce graphs and tables.
  
  switch(question,
         B.1.1 = {
           first_heading <- '###Consortia\n'
           intro_text <- 'Some introductory text about this particular question. Likely to be the same for all courses.\n'
           graph_text <- 'Some supporting text explaining the graph and highlighting some of the issues. Should be individual for each course.\n'
           table_text <- 'Some supporting about the table and highlighting some of the issues. Should be individual for each course.\n'
         },
         B.1.3 = {
           first_heading <- '###Consortia\n'
           intro_text <- 'Some introductory text about this particular question. Likely to be the same for all courses.\n'
           graph_text <- 'Some supporting text explaining the graph and highlighting some of the issues. Should be individual for each course.\n'
           table_text <- 'Some supporting about the table and highlighting some of the issues. Should be individual for each course.\n'
         },
         B.2.1 = {
           first_heading <- '###Consortia\n'
           intro_text <- 'Some introductory text about this particular question. Likely to be the same for all courses.\n'
           graph_text <- 'Some supporting text explaining the graph and highlighting some of the issues. Should be individual for each course.\n'
           table_text <- 'Some supporting about the table and highlighting some of the issues. Should be individual for each course.\n'
         },
         B.2.2 = {
           first_heading <- '###Consortia\n'
           intro_text <- 'Some introductory text about this particular question. Likely to be the same for all courses.\n'
           graph_text <- 'Some supporting text explaining the graph and highlighting some of the issues. Should be individual for each course.\n'
           table_text <- 'Some supporting about the table and highlighting some of the issues. Should be individual for each course.\n'
         },
         C.1 = {
           first_heading <- '###Consortia\n'
           intro_text <- 'Some introductory text about this particular question. Likely to be the same for all courses.\n'
           graph_text <- 'Some supporting text explaining the graph and highlighting some of the issues. Should be individual for each course.\n'
           table_text <- 'Some supporting about the table and highlighting some of the issues. Should be individual for each course.\n'
         },
         L.2.a = {
           first_heading <- '##Internship experience.\n'
           intro_text <- 'Some introductory text about this particular question. Likely to be the same for all courses.\n'
           graph_text <- 'Some supporting text explaining the graph and highlighting some of the issues. Should be individual for each course.\n'
           table_text <- 'Some supporting about the table and highlighting some of the issues. Should be individual for each course.\n'
         },
         L.3.a = {
           first_heading <- '##Field experience.\n'
           intro_text <- 'Some introductory text about this particular question. Likely to be the same for all courses.\n'
           graph_text <- 'Some supporting text explaining the graph and highlighting some of the issues. Should be individual for each course.\n'
           table_text <- 'Some supporting about the table and highlighting some of the issues. Should be individual for each course.\n'
         },
         L.4 = {
           first_heading <- '##First supervisor.\n'
           intro_text <- 'Some introductory text about this particular question. Likely to be the same for all courses.\n'
           graph_text <- 'Some supporting text explaining the graph and highlighting some of the issues. Should be individual for each course.\n'
           table_text <- 'Some supporting about the table and highlighting some of the issues. Should be individual for each course.\n'
         },
         L.5 = {
           first_heading <- '##Second supervisor.\n'
           intro_text <- 'Some introductory text about this particular question. Likely to be the same for all courses.\n'
           graph_text <- 'Some supporting text explaining the graph and highlighting some of the issues. Should be individual for each course.\n'
           table_text <- 'Some supporting about the table and highlighting some of the issues. Should be individual for each course.\n'
         },
         L.6 = {
           first_heading <- '##Personal development.\n'
           intro_text <- 'Some introductory text about this particular question. Likely to be the same for all courses.\n'
           graph_text <- 'Some supporting text explaining the graph and highlighting some of the issues. Should be individual for each course.\n'
           table_text <- 'Some supporting about the table and highlighting some of the issues. Should be individual for each course.\n'
         }
  )
  
  #logical flag to see if there is any data on a given question.
  #It will evaluate to TRUE if there is any error or will return the value otherwise.
  try_flag <- tryCatch(comparative_df(question, course_dataset), error = function(err) return(TRUE)) 
  
  if(!is.logical(try_flag)){ #checking if try_flag is logical. If it is, then do nothing. Otherwise print out the information about the question.
    cat(sprintf("\n%s\n", text_data))
    
    cat(sprintf("\n%s\n\n", first_heading))
    #cat(intro_text)
    
    #prtinting out the question
    print(questionprint(question, dataset = course_dataset, save = FALSE))
    
    #cat(graph_text)
    cat("\n")
    
    not_print <- c("N", "O", "P", "Q") #not printing comparative tables for questions on specific university
    
    if (!(substr(question, 1, 1) %in% not_print)) {  
      #preparing and printing table
      df <- comparative_df(question, course_dataset)
      z <- xtable(df, caption = sprintf("Summary statistics"), digits = c(0,0,2,2,2,2,2,2), type = "html")
      align(z) <- "|p{5cm}|cc|c|cccc|"
      print(z, NA.string = "NA", sanitize.text.function = function(x) x)
    }  
    
    cat("\n")
    #cat(table_text)
  }
}

means_printing <- function(x){
  ### function to print out a boxplot for any given question. Used in conjuction with means_prepare.
  
  # getting the means table for a given question
  means <- means_prepare(x)[[1]]
  # melting the means table to use it in plotting
  melted_means <- as.data.frame(melt(as.matrix(means)))

  #getting name of the question
  name_of_the_question <- extract_name(x)
  #wrap statement to have new lines after a given character
  wrap_function_title <- wrap_format(80)
  name_of_the_question <- wrap_function_title(name_of_the_question)
  #same for dimension names
  wrap_function_label <- wrap_format(40)

  #plotting
  p <- ggplot(melted_means, aes(y = value, x = X2)) + 
    ggtitle(name_of_the_question) +
    geom_boxplot() + 
    coord_flip() + 
    ylab("Distribution of means") +
    #http://stackoverflow.com/questions/21878974/auto-wrapping-of-labels-via-labeller-label-wrap-in-ggplot2
    scale_x_discrete(labels = function(x) wrap_function_label(x))+
    theme(axis.title.y=element_blank())

  #saving the resulting graph
  ggsave(filename = sprintf("./Mean_plots/%s.png", x), plot = p, units = "mm", width = 250, height = (70 + length(levels(factor(melted_means$X2)))*10))
}

figure_height <- function(question, course_dataset){
  # function to determine the height of the figure for a given question. Returns height if there is something to return, returns 0 
  # if comparative_df returns error. Function is needed since otherwise it would be impossible to determine the height and make sure
  # that script runs properly when there is no data to print out
  
  try_flag <- tryCatch(comparative_df(question, course_dataset), error = function(err) return(TRUE)) 
  if(!is.logical(try_flag)){ #checking if try_flag is logical. If it is, then do nothing. Otherwise print out the information about the question.
     return (1 + 0.42*nrow(comparative_df(question, course_dataset)))
  } else
    return (1) #return 0 height in case there is nothing to print
}
  
plot_question <- function(question, name_of_the_question){
  #function to create a graph of a question
  #question - dataset containing prepared question data
  #name_of_the_question - string, containing the name of the question to be used in title
  
  questionl <- likert(question) #creating likert-type variable for plotting
  ### printing out the file
  p <- plot(questionl, 
            plot.percents = TRUE, # displaying percents for each answer
            plot.percent.low = FALSE,  # displaying cummulative percents for negative answers
            plot.percent.high = FALSE, # displaying cummulative percents for positive answers
            centered = FALSE, # stretcthing the bar from left to right
            text.size = 2,
            wrap = 40, # wrap statement for dimension names
            legend.position = "top",
            group.order = sort(names(question))) + 
    ggtitle(name_of_the_question) + # title of the question
    theme(text = element_text(size = 10, family = "Times New Roman"), # setting the text size of the plot
          plot.margin = unit(c(0, 0.8, 0.3, 0), "lines"), # decreasing white space around the plot
          legend.margin = unit(0, "lines"), # deleting space around legend
          legend.key.size = unit(0.5, "lines"), # decreasing size of legend elements
          legend.background = element_rect(colour = "gray", fill = NA, size = 0.1), # adding a frame around the legend
          axis.title.x=element_blank(), #deleting x-label 
          plot.title = element_text(size = 10)) + #size of the text in the title
    geom_hline(yintercept=seq(25, 75, by=25), linetype = "dashed", size = 0.2) + # adding dashed lines at 25, 50, 75% to make it more clear
    coord_fixed() +
    coord_flip(ylim = c(3,97)) #reducing white space left to 0 and right to 100
  return(p)
}

prepare_university <- function(x, course_dataset){
  # function to prepare a dataset for future use
  # x = name of the question
  # course_dataset = dataset that needs to be prepared
  
  # returns a dataset:
  # universities with 10 or more respondents and answers to questions by respondents
  
  questions_uni <- c("N.", "O.", "P.", "Q.") #first letters for questions about specific universities
  x <- substr(x, 3, 5) #updating x to use it in a function. x in the beginning is used to make calls to functions consistent
  
  #creating four datasets to merge them leter. Name of the university is used as an ID.
  first_university <- course_dataset %>%
    select(University.1,
           starts_with(paste0(questions_uni[1], x)))
  second_university <- course_dataset %>%
    select(University.2,
           starts_with(paste0(questions_uni[2], x)))
  third_university <- course_dataset %>%
    select(University.3,
           starts_with(paste0(questions_uni[3], x)))
  fourth_university <- course_dataset %>%
    select(University.4,
           starts_with(paste0(questions_uni[4], x)))
  
  #since questions are always the same, binding four datasets together
  z <- rbind(first_university, 
             setNames(second_university, names(first_university)),
             setNames(third_university, names(first_university)),
             setNames(fourth_university, names(first_university)))
  
  names(z) <- gsub("the.first", "this", names(z)) #substituting "the.first" to "this" to make it gramatically correct
  z <- z[!is.na(z$University.1),] #removing empty lines
  
  #finding out names of universities with 10 or more respondents
  university_names <- z %>%
    select(University.1) %>%
    group_by(University.1) %>%
    summarise(respondents = n()) %>%
    filter(respondents >= 10)
  
  #returning only rows with 10 or more respondents 
  z <- z[z$University.1 %in% as.character(university_names$University.1),]
  return(z)
}

universityprint <- function(x, course_dataset){
  #function to print out information about specific university
  #x = name of the question to print out
  #course_dataset = dataset containing information about one course
  
  #preparing data to have only universities with at least 10 answers
  z <- prepare_university(x, course_dataset)
  
  #checking if there are any university to print out
  if (length(as.character(unique(z$University.1))) > 0) {
    #sorting university names alphabetically
    university_names <- sort(as.character(unique(z$University.1)))
    counts <- z %>% group_by(University.1) %>% summarise_each(funs(f2))
    row.names(counts) <- counts$University.1
    counts$University.1 <- NULL
    counts$rows <- rowSums(counts > 10)
    
    
    for(i in seq_along(university_names)){
      slice <- z[z$University.1 == university_names[i], ]
      n <- as.numeric(counts[rownames(counts) == university_names[i],"rows"])
      figheight <- 1 + 0.42 * n #calculating height of a figure to print out
      .q <- questionprint(x, slice, save = FALSE, name_of_the_question = paste0(" (n = ", nrow(slice), ")")) #preparing the plot

      #workaround to have next step. Chooses a random number from 1 to 1 million to have as a name of a chunk. 
      #Otherwise knitr throws an error since there are chunks with the same name
      cap <- sample(1:1e6, 1)
      
      if(!is.null(.q)){
        university_name <- paste0("\n###", university_names[i], "\n\n")
        kexpand(.q, figheight, cap, university_name)
      }
    }
  }
}

kexpand <- function(.q, figheight, cap, university_name){
  #function to dynamically update figheight inside of the loop
  
  cat(knit(
    text=knit_expand(text=
                    "```{r-{{cap}}, fig.height = {{figheight}}, echo=FALSE, message = FALSE, warning = FALSE}\n cat(university_name)\n .q\n```\n"
    )
  ))
}

cbind.fill <- function(...){
  #http://stackoverflow.com/questions/7962267/cbind-a-df-with-an-empty-df-cbind-fill
  nm <- list(...) 
  nm <- lapply(nm, as.matrix)
  n <- max(sapply(nm, nrow)) 
  do.call(cbind, lapply(nm, function (x) 
    rbind(x, matrix(, n-nrow(x), ncol(x)))))
}


prepare_university_eair <- function(x, course_dataset, university){
  # function to prepare a dataset to use in EAIR paper
  # x = name of the question
  # course_dataset = dataset that needs to be prepared
  
  # returns a dataset:
  # universities with 10 or more respondents and answers to questions by respondents
  
  questions_uni <- c("N.", "O.", "P.", "Q.") #first letters for questions about specific universities
  x <- substr(x, 3, 5) #updating x to use it in a function. x in the beginning is used to make calls to functions consistent
  
  #creating four datasets to merge them leter. Name of the university is used as an ID.
  
  #### for paper: can add A.2.name.of.Erasmus.Mundus.master.course. to chunks and then use group_by on that variable to create other chunks
  first_university <- course_dataset %>%
    select(University.1, A.2.name.of.Erasmus.Mundus.master.course.,
           starts_with(paste0(questions_uni[1], x)))
  second_university <- course_dataset %>%
    select(University.2, A.2.name.of.Erasmus.Mundus.master.course.,
           starts_with(paste0(questions_uni[2], x)))
  third_university <- course_dataset %>%
    select(University.3, A.2.name.of.Erasmus.Mundus.master.course.,
           starts_with(paste0(questions_uni[3], x)))
  fourth_university <- course_dataset %>%
    select(University.4, A.2.name.of.Erasmus.Mundus.master.course.,
           starts_with(paste0(questions_uni[4], x)))
  
  #since questions are always the same, binding four datasets together
  z <- rbind(first_university, 
             setNames(second_university, names(first_university)),
             setNames(third_university, names(first_university)),
             setNames(fourth_university, names(first_university)))
  
  #deleting dimensions that can be different in different programs
  if (x == "1.1"){
    z$N.1.1.Rate.the.following.items.regarding.the.logistic.information.and.support.received.before.the.beginning.of.studies.in.the.first.university._Language.courses <- NULL
  }
  if (x == "1.3"){
    z$N.1.3.Rate.the.introduction.process.to.the.following.units.or.people.as.part.of.the.orientation.program.at.the.first.university._Academic.staff <- NULL
    z$N.1.3.Rate.the.introduction.process.to.the.following.units.or.people.as.part.of.the.orientation.program.at.the.first.university._Administrative.staff <- NULL
    z$N.1.3.Rate.the.introduction.process.to.the.following.units.or.people.as.part.of.the.orientation.program.at.the.first.university._Other.students <- NULL
  }
  if (x == "2.1"){
    z$N.2.1.Rate.the.helpfulness.of.the.following.units.of.people.at.the.first.university._Other.students <- NULL
  }

  
  names(z) <- gsub("the.first", "this", names(z)) #substituting "the.first" to "this" to make it gramatically correct
  z <- z[!is.na(z$University.1) & z$University.1 == university,] #removing empty lines and subsetting to only chosen university
  
  #finding out names of programs with 10 or more respondents
  program_names <- z %>%
    select(A.2.name.of.Erasmus.Mundus.master.course.) %>%
    group_by(A.2.name.of.Erasmus.Mundus.master.course.) %>%
    summarise(respondents = n()) %>%
    filter(respondents >= 10)
  
  #deleting column with university since we don't need it anymore
  z$University.1 <- NULL
  #leaving only names of programs with 10 or more respondents
  z <- z[z$A.2.name.of.Erasmus.Mundus.master.course. %in% as.character(program_names$A.2.name.of.Erasmus.Mundus.master.course.),]
  
  names(z)[1] <- "name"
  #cleaning up names
  colnames(z) <- gsub("\\.", " ", colnames(z)) #making names of questions readable
  colnames(z) <- gsub("(.*?)_(.*)", "\\2", colnames(z)) #leaving just the dimension name
  
  if(x == "5.1") 
    names(z) <- c("name", "Overall satisfaction in this university")
  levels <- likert_levels # default is likert_levels
  
  ### making sure that levels go in order they were meant to go
  for(i in 2:ncol(z)) {
    ### this step will also reduce all other answers to NA's
    z[,i] <- factor(z[,i], levels = levels)
  }
  
  return(z)
}

calculatep <- function(x, dataset, university, method = "pvalue"){
  # function to assign types to comparisons
  # x = name of the question
  # dataset = dataset containing necessary information
  # university = string with a name university
  # pvalue = if TRUE, will return pvalue, if FALSE will return difference in means
  
  # returns a vector of given length with calculated types.
  
  #preparing dataset to have information about only a given university
  z <- prepare_university_eair(x, dataset, university)
  
  #calculating means for all dimensions
  means_question <- z %>% 
    group_by(name) %>%
    summarise_each(funs(f1))
  
  #preparing empty vector of given length
  pvalues <- data.frame(matrix(ncol = ncol(z)))
  pvalues[1] <- as.character(university) #pasting name of a university
  
  for (i in 2:ncol(z)){
    names(pvalues)[i] <- names(z)[i] #pasting name of a dimension
    
    #calculating location of a minimum and a maximum in a column
    minimum <- which.min(as.numeric(unlist(means_question[,i])))
    maximum <- which.max(as.numeric(unlist(means_question[,i])))
    
    #slicing prepared dataset to have information about program with minimal and maximum values
    x <- z[z$name == means_question$name[minimum], ]
    y <- z[z$name == means_question$name[maximum], ]
    
    #catch-all condition to make sure that we calculate means only when it makes sense
    #if there are less than 2 programs, if there are less than 10 observations in any of the programs, NA will be kept in a column
    if(nrow(means_question) < 2 || nrow(x) < 10 || nrow(y) < 10 || sum(!is.na(means_question[,i])) == 1) 
      next
    else {
#       if(method == "pvalue") 
#         pvalues[1, i] <- t.test(as.numeric(x[,i]), as.numeric(y[,i]))$p.value
#       if (method == "mean")
#         pvalues[1, i] <- abs(mean(as.numeric(x[,i]), na.rm = TRUE) - mean(as.numeric(y[,i]), na.rm = TRUE))
      temp <- t.test(as.numeric(x[,i]), as.numeric(y[,i]))$p.value
      if(temp < 0.05)
        pvalues[1, i] <- 3
      else if (mean(as.numeric(x[,i]), na.rm = TRUE) > 3 & mean(as.numeric(y[,i]), na.rm = TRUE) > 3)
        pvalues[1, i] <- 1
      else if (mean(as.numeric(x[,i]), na.rm = TRUE) < 2.5 & mean(as.numeric(y[,i]), na.rm = TRUE) < 2.5)
        pvalues[1, i] <- 2
      else
        pvalues[1, i] <- 4
    }
  }
  return (pvalues)
}


calculateallp <- function(x, dataset, universities, method){
  # function to calculate p-values for all universities in a dataset for a given question
  # x = name of the question
  # dataset = dataset with information
  # universities = data frame containing names of universities with 3 or more programs
  # pvalue = if TRUE, will return pvalue, if FALSE will return difference in means
  
  
  # returns a data frame with p-values for every dimension and every university. If p-value couldn't have been calculated, cell will have NA
  
  #calculating sample vector to initialize returning data frame 
  z <- calculatep(x, dataset, universities$value[1], method = method)
  pvalues <- data.frame(matrix(ncol = ncol(z), nrow = nrow(universities)))
  names(pvalues) <- names(z) #setting names to have names of dimensions
  
  for (i in 1:nrow(universities)){
    pvalues[i,] <- calculatep(x, dataset, universities$value[i], method = method)
  }
  return(pvalues)
}