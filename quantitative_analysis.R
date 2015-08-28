library(likert)
library(scales)
library(psych)
library(reshape)
library(grid)
library(RColorBrewer)
library(dplyr)

dataset <- read.csv("../Media/2015/Master_tables/bigtable.csv", na.strings = c("", " ", "No answer", "N/A", "NA"), header = TRUE)
dataset$X <- NULL
dataset$B.2.2.Rate.the.support.received.on.the.following.issues._Inappropriate.conduct.or.sexual.harassment.issues <- NULL
dataset$B.2.2.a.If.you.feel.comfortable.describe.any.inappropriate.conduct.or.sexual.harassment.issues.you.have.witnessed.or.have.been.the.subject.of.and.the.support.you.have.received.The.answers.to.this.question.will.not.be.shared.with.Erasmus.Mundus.course._Open.Ended.Response <- NULL

### ordered levels that were used in the survey
likert_levels <- c("Very unsatisfied", "Somewhat unsatisfied", "Somewhat satisfied", "Very satisfied")
agree_levels <- c("Disagree", "Somewhat disagree", "Somewhat agree", "Agree")

z <- dataset %>%
  dplyr::select(starts_with("B.1.1"),
         starts_with("B.1.3"),
         starts_with("B.2.1"),
         starts_with("B.2.2"),
         starts_with("C.1"), 
         C.3.Please.rate.academic.satisfaction.with.EM.course._,
         C.4.Please.rate.overall.satisfaction.with.EM.course._)
for(i in seq_along(z)) {
  ### this step will also reduce all other answers to NA's
  z[,i] <- factor(z[,i], levels = likert_levels)
}
colnames(z) <- gsub("Rate.the.information.and.support.received.before.the.start.of.Erasmus.Mundus.master.course.on.the.following.aspects.", "", colnames(z))
colnames(z) <- gsub("Rate.the.introduction.process.to.the.following.units.or.people.as.part.of.the.orientation.program.", "", colnames(z))
colnames(z) <- gsub("Rate.the.helpfulness.of.the.following.units.of.people.", "", colnames(z))
colnames(z) <- gsub("Rate.the.following.items", "", colnames(z))
colnames(z) <- gsub("Rate.the.support.received.on.the.following.issues.", "", colnames(z))

z <- z[complete.cases(z),]

z <- data.matrix(z)

z[,36] <- factor(z[,36])

#PCA
fa.parallel(z, fa = "pc", n.iter = 100, show.legend = FALSE)
pc <- principal(z, nfactors = 4)

#LFA
covariances <- cov(z)
correlations <- cov2cor(covariances)
fa <- fa(correlations, nfactors = 10, rotate = "none", fm = "pa")

library(MASS)
library(caTools)
#LDA
spl <- sample.split(z[,36], SplitRatio = 0.7)
train <- z[spl,]
test <- z[!spl,]
lda.fit <- lda(C.4.Please.rate.overall.satisfaction.with.EM.course._ ~ ., data = train)
lda.class <- predict(lda.fit, test$C.4.Please.rate.overall.satisfaction.with.EM.course._)$class


#kmeans
kmeans_z <- kmeans(z, 5)