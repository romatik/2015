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

x <- 'B.1.1'
question <- dataset[, substr(names(dataset), 1, nchar(x)) == x]
colnames(question) <- gsub("\\.", " ", colnames(question)) #making names of questions readable
name_of_the_question <- gsub("(.*)_(.*)", "\\1", colnames(question)[1]) #storing the name of the section for title of the plot
name_of_the_question <- substring(name_of_the_question, nchar(x)+1)
colnames(question) <- gsub("(.*)_(.*)", "\\2", colnames(question)) #leaving just the dimension name

levels <- likert_levels # default is likert_levels

for(i in seq_along(question)) {
  question[,i] <- factor(question[,i], levels = levels)
}

### creating likert-type variable to print it out
grouping <- factor(dataset$A.5.When.did.you.start.EM.Course._Response, levels = c("2009", "2010", "2011", "2012", "2013", "2014"))
question <- question[!is.na(grouping),]
grouping <- grouping[!is.na(grouping)]
questionl <- likert(question, grouping = grouping) #creating likert-type variable for plotting
wrap_function <- wrap_format(130) #wrap-function to print question correctly
name_of_the_question <- wrap_function(name_of_the_question)

### printing out the file
z <- 110
p <- plot(questionl, plot.percents = TRUE, plot.percent.low = FALSE, plot.percent.high = FALSE, 
          centered = FALSE, text.size = 1.5, wrap = 70, legend.position = "top") + 
   ggtitle(name_of_the_question) #+ 
#   theme(text = element_text(size = 7), 
#         plot.margin = unit(c(0, 0.3, 0.3, 0), "lines"), 
#         legend.margin = unit(0, "lines"),
#         plot.title = element_text(vjust = 0, hjust = 3 ), 
#         legend.key.size = unit(0.5, "lines"),
#         legend.background = element_rect(colour = "gray", fill = NA, size = 0.1)) +
#   geom_hline(yintercept=seq(25, 75, by=25), linetype = "dashed", size = 0.2) +
#   coord_flip(ylim = c(-1,101))

png(filename = sprintf("./Likert_example/%i.png", z), width = 1024)
plot(p)
dev.off()
ggsave(filename = sprintf("./Likert_example/%i.png", z), plot = p, units = "mm", width = 180, height = 150)
