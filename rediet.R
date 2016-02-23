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
    select(RespondentID_,
           A.2.name.of.Erasmus.Mundus.master.course.,
           University.1,
           starts_with(paste0(questions_uni[1], x)))
  second_university <- course_dataset %>%
    select(RespondentID_,
           A.2.name.of.Erasmus.Mundus.master.course.,
           University.1,
           starts_with(paste0(questions_uni[2], x)))
  third_university <- course_dataset %>%
    select(RespondentID_,
           A.2.name.of.Erasmus.Mundus.master.course.,
           University.1,
           starts_with(paste0(questions_uni[3], x)))
  fourth_university <- course_dataset %>%
    select(RespondentID_,
           A.2.name.of.Erasmus.Mundus.master.course.,
           University.1,
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
#  z <- z[z$University.1 %in% as.character(university_names$University.1),]
  return(z)
}

rmarkdown::render('./Reports/rediet.Rmd',  # file 2
                  output_format = "html_document",
                  output_file =  "rediet.html", 
                  output_dir = './Reports')
