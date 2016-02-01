source("functions.R")

####
universities <- dataset %>%
  select(A.2.name.of.Erasmus.Mundus.master.course., University.1, University.2, University.3, University.4) %>%
  melt(, id.vars = c("A.2.name.of.Erasmus.Mundus.master.course.")) %>%
  select(A.2.name.of.Erasmus.Mundus.master.course., value) %>%
  unique() %>%
  group_by(value) %>%
  tally() %>%
  filter(n > 2) #change here to change how many programs should be present for further analysis

#deleting NA's
universities <- universities[complete.cases(universities),]


#dataset to check what programs were hosted by chosen university
programs <- dataset %>%
  select(A.2.name.of.Erasmus.Mundus.master.course., University.1, University.2, University.3, University.4) %>%
  melt(, id.vars = c("A.2.name.of.Erasmus.Mundus.master.course.")) %>%
  select(A.2.name.of.Erasmus.Mundus.master.course., value) %>%
  unique() 

question1 <- calculateallp("N.1.1", dataset, universities, method = "pvalue")
question1_melt <- melt(question1, id.vars = "X1")

question2 <- calculateallp("N.1.3", dataset, universities, method = "pvalue")
question2_melt <- melt(question2, id.vars = "X1")

question3 <- calculateallp("N.2.1", dataset, universities, method = "pvalue")
question3_melt <- melt(question3, id.vars = "X1")

question4 <- calculateallp("N.2.2", dataset, universities, method = "pvalue")
question4_melt <- melt(question4, id.vars = "X1")

everydim <- rbind(question1_melt, question2_melt, question3_melt, question4_melt)
table(everydim$value)

overallsatisf <- calculateallp("N.5.1", dataset, universities, method = "mean")
cor(question1$sum[!is.na(overallsatisf[,2])], overallsatisf[!is.na(overallsatisf[,2]),2])
