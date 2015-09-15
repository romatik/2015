# http://stackoverflow.com/questions/32136304/conditional-calculation-of-mean
# function calculates the mean only if there are 10 or more respondents to each individual question
f1 <- function(x) if(sum(!is.na(x))>9) mean(as.numeric(x), na.rm=TRUE) else NA_real_

questionprint <- function(x, dataset = overall, save = TRUE){
  ### function for printing out the likert plot about each individual section of a survey. It also prints out information about 
  ### Cronbach's alpha level. Can be used further to create similar plots for each individual course.
  
  ### x = name of the question to be printed
  ### dataset = which dataset should be used to extract data from. Default = overall.
  ### save = flag to indicate whether to save or print the plot. Default is TRUE (plot will be saved in the current directory in sub-folder 
  ### "Question_statistics")
  
  z <- question_prepare(x, dataset)
  question <- z[[1]]
  name_of_the_question <- z[[2]]
  
  ### checking to see if question has more than 1 dimension with 10 or more respondents to proceed. 
  ### Otherwise it doesn't make sense to calculate Cronbach's alpha and plot
  if (!is.null(dim(question)[2])){
    if (dim(question)[2] > 1) {
      
      ### calculating Cronbach's alpha. If there is an error it won't print out anything
      #try(printing_alpha(x, question))
      
      ### creating likert-type variable to print it out
      questionl <- likert(question) #creating likert-type variable for plotting
      wrap_function <- wrap_format(60) #wrap-function to print question correctly
      name_of_the_question <- wrap_function(name_of_the_question)
      
      
      ### printing out the file
      p <- plot(questionl, 
                plot.percents = TRUE, # displaying percents for each answer
                plot.percent.low = FALSE,  # displaying cummulative percents for negative answers
                plot.percent.high = FALSE, # displaying cummulative percents for positive answers
                centered = FALSE, # stretcthing the bar from left to right
                text.size = 2,
                wrap = 40, # wrap statement for dimension names
                legend.position = "top") + 
        ggtitle(name_of_the_question) + # title of the question
        theme(text = element_text(size = 10, family = "Source Sans Pro"), # setting the text size of the plot
              plot.margin = unit(c(0, 0.8, 0.3, 0), "lines"), # decreasing white space around the plot
              legend.margin = unit(0, "lines"), # deleting space around legend
              legend.key.size = unit(0.5, "lines"), # decreasing size of legend elements
              legend.background = element_rect(colour = "gray", fill = NA, size = 0.1)) +# adding a frame around the legend
        geom_hline(yintercept=seq(25, 75, by=25), linetype = "dashed", size = 0.2) + # adding dashed lines at 25, 50, 75% to make it more clear
        coord_fixed() +
        coord_flip(ylim = c(-1,101)) #reducing white space left to 0 and right to 100
      if(save){
        ggsave(filename = sprintf("./Question_statistics/%s.png", x), plot = p, units = "mm", width = 180, height = (25 + dim(question)[2]*8)) #making graph a little rubbery
      } else {
        plot(p)
      }
    }
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
  name_of_the_question <- extract_name(question, x)
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
  question <- question[, colSums(!is.na(question)) > 9]
  output <- list(question, name_of_the_question)
  return(output)
}

extract_name <- function(question, x){
  #extracts name of the question and returns it
  
  ### x = string containing the identifier of the question (e.g. "B.1.3")
  ### question = dataset with the respective questions
  name_of_the_question <- gsub("(.*)_(.*)", "\\1", colnames(question)[1]) #storing the name of the section for title of the plot
  name_of_the_question <- substring(name_of_the_question, nchar(x)+1)
  
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

  #calculating means for a specific course
  means_question <- question_dataset %>% 
    summarise_each(funs(f1, n())) %>%
    gather(variable, value) %>%
    separate(variable, c("var", "stat"), sep = "\\_") %>%
    spread(var, value)%>%
    t() %>%
    as.data.frame()
  
  #cleaning the means_question dataset to use it in a table
  names(means_question) <- c("Mean", "Respondents")
  means_question <- means_question[2:nrow(means_question),]
  means_question$Mean <- as.numeric(as.character(means_question$Mean))
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
  quantiles <- as.data.frame(t(apply(X = means_overall_each, FUN = function(x) quantile(x, na.rm = TRUE), MARGIN = 2)))
  
    
  df <- merge(df, quantiles, by = 0, all = TRUE)
  rownames(df) <- df$Row.names
  df$Row.names <- NULL
  
  return(df[,c(2,1,3:ncol(df))])
}

means_prepare <- function(x){
  ### calculates means for the given question. Returns prepared means matrix.
  
  ### x = name of the question.
  
  overall_dataset <- question_prepare(x, dataset)[[1]]

  #calculating means for each course and each question to create a quantile variable
  overall_dataset <- cbind(overall_dataset, dataset$A.2.Select.the.name.of.Erasmus.Mundus.master.course._Response.)
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
                      drop = FALSE) + # not dropping levels with 0 
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

report_question <- function(question, course_dataset){
  ## Function to print out the question in the individual report.
  
  ## question = string, containing the question to be printed.
  ## course_dataset = course dataset to produce graphs and tables.
  
  switch(question,
         B.1.1 = {
           first_heading <- '##Support received before the start of the Erasmus Mundus course\n'
           intro_text <- 'Some introductory text about this particular question. Likely to be the same for all courses.\n'
           graph_text <- 'Some supporting text explaining the graph and highlighting some of the issues. Should be individual for each course.\n'
           table_text <- 'Some supporting about the table and highlighting some of the issues. Should be individual for each course.\n'
         },
         B.1.3 = {
           first_heading <- '##Support received during the orientaion program.\n'
           intro_text <- 'Some introductory text about this particular question. Likely to be the same for all courses.\n'
           graph_text <- 'Some supporting text explaining the graph and highlighting some of the issues. Should be individual for each course.\n'
           table_text <- 'Some supporting about the table and highlighting some of the issues. Should be individual for each course.\n'
         },
         B.2.1 = {
           first_heading <- '##Helpfulness of units and people\n'
           intro_text <- 'Some introductory text about this particular question. Likely to be the same for all courses.\n'
           graph_text <- 'Some supporting text explaining the graph and highlighting some of the issues. Should be individual for each course.\n'
           table_text <- 'Some supporting about the table and highlighting some of the issues. Should be individual for each course.\n'
         },
         B.2.2 = {
           first_heading <- '##Support received on various issues.\n'
           intro_text <- 'Some introductory text about this particular question. Likely to be the same for all courses.\n'
           graph_text <- 'Some supporting text explaining the graph and highlighting some of the issues. Should be individual for each course.\n'
           table_text <- 'Some supporting about the table and highlighting some of the issues. Should be individual for each course.\n'
         },
         C.1 = {
           first_heading <- '##Module assessment.\n'
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
         },
         N.1.1 = {
           first_heading <- '###Support before the beginning of studies.\n'
           intro_text <- 'Some introductory text about this particular question. Likely to be the same for all courses.\n'
           graph_text <- 'Some supporting text explaining the graph and highlighting some of the issues. Should be individual for each course.\n'
           table_text <- 'Some supporting about the table and highlighting some of the issues. Should be individual for each course.\n'
         },
         N.1.3 = {
           first_heading <- '###Orientation program.\n'
           intro_text <- 'Some introductory text about this particular question. Likely to be the same for all courses.\n'
           graph_text <- 'Some supporting text explaining the graph and highlighting some of the issues. Should be individual for each course.\n'
           table_text <- 'Some supporting about the table and highlighting some of the issues. Should be individual for each course.\n'
         },
         N.2.1 = {
           first_heading <- '###Helpfulness of units/people.\n'
           intro_text <- 'Some introductory text about this particular question. Likely to be the same for all courses.\n'
           graph_text <- 'Some supporting text explaining the graph and highlighting some of the issues. Should be individual for each course.\n'
           table_text <- 'Some supporting about the table and highlighting some of the issues. Should be individual for each course.\n'
         },
         N.2.2 = {
           first_heading <- '###Extracurricular activities\n'
           intro_text <- 'Some introductory text about this particular question. Likely to be the same for all courses.\n'
           graph_text <- 'Some supporting text explaining the graph and highlighting some of the issues. Should be individual for each course.\n'
           table_text <- 'Some supporting about the table and highlighting some of the issues. Should be individual for each course.\n'
         },
         N.3.1 = {
           first_heading <- '###Assessment and feedback channels.\n'
           intro_text <- 'Some introductory text about this particular question. Likely to be the same for all courses.\n'
           graph_text <- 'Some supporting text explaining the graph and highlighting some of the issues. Should be individual for each course.\n'
           table_text <- 'Some supporting about the table and highlighting some of the issues. Should be individual for each course.\n'
         },
         N.4.1 = {
           first_heading <- '###Teaching and learning.\n'
           intro_text <- 'Some introductory text about this particular question. Likely to be the same for all courses.\n'
           graph_text <- 'Some supporting text explaining the graph and highlighting some of the issues. Should be individual for each course.\n'
           table_text <- 'Some supporting about the table and highlighting some of the issues. Should be individual for each course.\n'
         },
         O.1.1 = {
           first_heading <- '###Support before the beginning of studies.\n'
           intro_text <- 'Some introductory text about this particular question. Likely to be the same for all courses.\n'
           graph_text <- 'Some supporting text explaining the graph and highlighting some of the issues. Should be individual for each course.\n'
           table_text <- 'Some supporting about the table and highlighting some of the issues. Should be individual for each course.\n'
         },
         O.1.3 = {
           first_heading <- '###Orientation program.\n'
           intro_text <- 'Some introductory text about this particular question. Likely to be the same for all courses.\n'
           graph_text <- 'Some supporting text explaining the graph and highlighting some of the issues. Should be individual for each course.\n'
           table_text <- 'Some supporting about the table and highlighting some of the issues. Should be individual for each course.\n'
         },
         O.2.1 = {
           first_heading <- '###Helpfulness of units/people.\n'
           intro_text <- 'Some introductory text about this particular question. Likely to be the same for all courses.\n'
           graph_text <- 'Some supporting text explaining the graph and highlighting some of the issues. Should be individual for each course.\n'
           table_text <- 'Some supporting about the table and highlighting some of the issues. Should be individual for each course.\n'
         },
         O.2.2 = {
           first_heading <- '###Extracurricular activities\n'
           intro_text <- 'Some introductory text about this particular question. Likely to be the same for all courses.\n'
           graph_text <- 'Some supporting text explaining the graph and highlighting some of the issues. Should be individual for each course.\n'
           table_text <- 'Some supporting about the table and highlighting some of the issues. Should be individual for each course.\n'
         },
         O.3.1 = {
           first_heading <- '###Assessment and feedback channels.\n'
           intro_text <- 'Some introductory text about this particular question. Likely to be the same for all courses.\n'
           graph_text <- 'Some supporting text explaining the graph and highlighting some of the issues. Should be individual for each course.\n'
           table_text <- 'Some supporting about the table and highlighting some of the issues. Should be individual for each course.\n'
         },
         O.4.1 = {
           first_heading <- '###Teaching and learning.\n'
           intro_text <- 'Some introductory text about this particular question. Likely to be the same for all courses.\n'
           graph_text <- 'Some supporting text explaining the graph and highlighting some of the issues. Should be individual for each course.\n'
           table_text <- 'Some supporting about the table and highlighting some of the issues. Should be individual for each course.\n'
         },
         P.1.1 = {
           first_heading <- '###Support before the beginning of studies.\n'
           intro_text <- 'Some introductory text about this particular question. Likely to be the same for all courses.\n'
           graph_text <- 'Some supporting text explaining the graph and highlighting some of the issues. Should be individual for each course.\n'
           table_text <- 'Some supporting about the table and highlighting some of the issues. Should be individual for each course.\n'
         },
         P.1.3 = {
           first_heading <- '###Orientation program.\n'
           intro_text <- 'Some introductory text about this particular question. Likely to be the same for all courses.\n'
           graph_text <- 'Some supporting text explaining the graph and highlighting some of the issues. Should be individual for each course.\n'
           table_text <- 'Some supporting about the table and highlighting some of the issues. Should be individual for each course.\n'
         },
         P.2.1 = {
           first_heading <- '###Helpfulness of units/people.\n'
           intro_text <- 'Some introductory text about this particular question. Likely to be the same for all courses.\n'
           graph_text <- 'Some supporting text explaining the graph and highlighting some of the issues. Should be individual for each course.\n'
           table_text <- 'Some supporting about the table and highlighting some of the issues. Should be individual for each course.\n'
         },
         P.2.2 = {
           first_heading <- '###Extracurricular activities\n'
           intro_text <- 'Some introductory text about this particular question. Likely to be the same for all courses.\n'
           graph_text <- 'Some supporting text explaining the graph and highlighting some of the issues. Should be individual for each course.\n'
           table_text <- 'Some supporting about the table and highlighting some of the issues. Should be individual for each course.\n'
         },
         P.3.1 = {
           first_heading <- '###Assessment and feedback channels.\n'
           intro_text <- 'Some introductory text about this particular question. Likely to be the same for all courses.\n'
           graph_text <- 'Some supporting text explaining the graph and highlighting some of the issues. Should be individual for each course.\n'
           table_text <- 'Some supporting about the table and highlighting some of the issues. Should be individual for each course.\n'
         },
         P.4.1 = {
           first_heading <- '###Teaching and learning.\n'
           intro_text <- 'Some introductory text about this particular question. Likely to be the same for all courses.\n'
           graph_text <- 'Some supporting text explaining the graph and highlighting some of the issues. Should be individual for each course.\n'
           table_text <- 'Some supporting about the table and highlighting some of the issues. Should be individual for each course.\n'
         },
         Q.1.1 = {
           first_heading <- '###Support before the beginning of studies.\n'
           intro_text <- 'Some introductory text about this particular question. Likely to be the same for all courses.\n'
           graph_text <- 'Some supporting text explaining the graph and highlighting some of the issues. Should be individual for each course.\n'
           table_text <- 'Some supporting about the table and highlighting some of the issues. Should be individual for each course.\n'
         },
         Q.1.3 = {
           first_heading <- '###Orientation program.\n'
           intro_text <- 'Some introductory text about this particular question. Likely to be the same for all courses.\n'
           graph_text <- 'Some supporting text explaining the graph and highlighting some of the issues. Should be individual for each course.\n'
           table_text <- 'Some supporting about the table and highlighting some of the issues. Should be individual for each course.\n'
         },
         Q.2.1 = {
           first_heading <- '###Helpfulness of units/people.\n'
           intro_text <- 'Some introductory text about this particular question. Likely to be the same for all courses.\n'
           graph_text <- 'Some supporting text explaining the graph and highlighting some of the issues. Should be individual for each course.\n'
           table_text <- 'Some supporting about the table and highlighting some of the issues. Should be individual for each course.\n'
         },
         Q.2.2 = {
           first_heading <- '###Extracurricular activities\n'
           intro_text <- 'Some introductory text about this particular question. Likely to be the same for all courses.\n'
           graph_text <- 'Some supporting text explaining the graph and highlighting some of the issues. Should be individual for each course.\n'
           table_text <- 'Some supporting about the table and highlighting some of the issues. Should be individual for each course.\n'
         },
         Q.3.1 = {
           first_heading <- '###Assessment and feedback channels.\n'
           intro_text <- 'Some introductory text about this particular question. Likely to be the same for all courses.\n'
           graph_text <- 'Some supporting text explaining the graph and highlighting some of the issues. Should be individual for each course.\n'
           table_text <- 'Some supporting about the table and highlighting some of the issues. Should be individual for each course.\n'
         },
         Q.4.1 = {
           first_heading <- '###Teaching and learning.\n'
           intro_text <- 'Some introductory text about this particular question. Likely to be the same for all courses.\n'
           graph_text <- 'Some supporting text explaining the graph and highlighting some of the issues. Should be individual for each course.\n'
           table_text <- 'Some supporting about the table and highlighting some of the issues. Should be individual for each course.\n'
         }
  )
  
  #logical flag to see if there is any data on a given question.
  #It will evaluate to TRUE if there is any error or will return the value otherwise.
  try_flag <- tryCatch(comparative_df(question, course_dataset), error = function(err) return(TRUE)) 
  
  if(!is.logical(try_flag)){ #checking if try_flag is logical. If it is, then do nothing. Otherwise print out the information about the question.
    temp <- sprintf("%s%s%s\n", first_heading, "Question:", question)
    cat(temp)
    cat(intro_text)
    questionprint(question, dataset = course_dataset, save = FALSE)
    cat(graph_text)
    
    df <- comparative_df(question, course_dataset)
    z <- xtable(df, caption = sprintf("Summary statistics for %s question", question), digits = c(0,0,2,2,2,2,2,2,2), type = "html")
    align(z) <- "|p{5.5cm}|cc|c|lllll|"
    print(z, table.placement="h", floating = FALSE, NA.string = "NA")
    
    cat(table_text)
  }
}
