source("functions.R")
library(ltm)

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
names(z) <- c(1:37)

fit1 <- grm(z, constrained = TRUE)
fit2 <- grm(z)
margins(fit1)
plot(fit2, type = "IIC")

plot(fit2, category = 4, lwd = 2, cex = 1.2, legend = TRUE, cx = -4.5, cy = 0.85, cex.main = 1.5, cex.lab = 1.3, cex.axis = 1.1)

infor <- c(1:37)
for (i in 1:37){
  t <- information(fit1, c(-4,4), items = c(i))
  infor[i] <- t$InfoTotal
}