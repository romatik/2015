library(reshape2)
library(dplyr)
library(stringr)


#### auxilary function to read in csv files. Since SurveyMonkey separates the name of the column on two lines I need to read it in
#### this way. Two lines are divided by "_" to make it easier to extract it later.
readingcsv <- function(pathtofile){
  header <- read.csv(pathtofile, nrows = 1)
  header2 <- scan(pathtofile, skip = 1, nlines = 1, what = character(), sep = ",")
  data <- read.csv(pathtofile, na.strings = c(" ", ""), skip = 2, header = FALSE)
  names(data) <- paste(colnames(header), header2, sep = "_")
  colnames(data) <- make.names(colnames(data))
  return (data)
}

##### reading in lists one by one. All of the lists contain duplicate information about ID and such. Deleting from all to later combine
##### them without issues
list1 <- readingcsv("../Media/2015/CSV/Sheet_1.csv")
list2 <- readingcsv("../Media/2015/CSV/Sheet_2.csv")
namesdrop <- list2 %>%
  select(starts_with("Email.Address_"), 
         starts_with("First.Name_"), 
         starts_with("Lastname_"), 
         starts_with("Custom.Data_"),
         starts_with("StartDate_"),
         starts_with("EndDate_"),
         starts_with("IP.Address_"),
         starts_with("CollectorID_")) %>%
  colnames()
list2 <- list2[, !(names(list2) %in% namesdrop)]

list3 <- readingcsv("../Media/2015/CSV/Sheet_3.csv")
namesdrop <- list3 %>%
  select(starts_with("Email.Address_"), 
         starts_with("First.Name_"), 
         starts_with("Lastname_"), 
         starts_with("Custom.Data_"),
         starts_with("StartDate_"),
         starts_with("EndDate_"),
         starts_with("IP.Address_"),
         starts_with("CollectorID_")) %>%
  colnames()
list3 <- list3[, !(names(list3) %in% namesdrop)]

list4 <- readingcsv("../Media/2015/CSV/Sheet_4.csv")
namesdrop <- list4 %>%
  select(starts_with("Email.Address_"), 
         starts_with("First.Name_"), 
         starts_with("Lastname_"), 
         starts_with("Custom.Data_"),
         starts_with("StartDate_"),
         starts_with("EndDate_"),
         starts_with("IP.Address_"),
         starts_with("CollectorID_")) %>%
  colnames()
list4 <- list4[, !(names(list4) %in% namesdrop)]

list5 <- readingcsv("../Media/2015/CSV/Sheet_5.csv")
namesdrop <- list5 %>%
  select(starts_with("Email.Address_"), 
         starts_with("First.Name_"), 
         starts_with("Lastname_"), 
         starts_with("Custom.Data_"),
         starts_with("StartDate_"),
         starts_with("EndDate_"),
         starts_with("IP.Address_"),
         starts_with("CollectorID_")) %>%
  colnames()
list5 <- list5[, !(names(list5) %in% namesdrop)]

##### inner joining all lists by RespondentID
bigtable <- inner_join(list1, list2, by = "RespondentID_")
bigtable <- inner_join(bigtable, list3, by = "RespondentID_")
bigtable <- inner_join(bigtable, list4, by = "RespondentID_")
bigtable <- inner_join(bigtable, list5, by = "RespondentID_")

##### processing the columns with Universities. Since we had complicated logic, SurveyMonkey recorder each logic step in separate
##### columns. It makes it complicated.
bigtable_processed <- bigtable %>% 
  select(RespondentID_, starts_with("University."), starts_with("A.3..")) %>%
  melt(id = "RespondentID_", na.rm = TRUE)  

bigtable_processed$variable <- gsub("(University.\\d{1}).+\\d{0,1}$", "\\1", bigtable_processed$variable)  
bigtable_processed$variable <- str_sub(bigtable_processed$variable, start = -12)
bigtable_processed <- reshape(bigtable_processed, direction = "wide", idvar = "RespondentID_", timevar = "variable")


##### Deleting extra columns from the table
namesdrop <- bigtable %>%
  select(starts_with("University."), 
         starts_with("A.3..")) %>%
  colnames()
bigtable <- bigtable[, !(names(bigtable) %in% namesdrop)]
bigtable <- inner_join(bigtable, bigtable_processed, by = "RespondentID_")

##### function to extract information from an IP address of a respondent. Possibly will only use it for visualizations and such
##### Result of a function is stored in ip.csv.
# freegeoip <- function(ip, format = ifelse(length(ip)==1,'list','dataframe'))
# {
#   if (1 == length(ip))
#   {
#     # a single IP address
#     require(rjson)
#     url <- paste(c("http://freegeoip.net/json/", ip), collapse='')
#     ret <- fromJSON(readLines(url, warn=FALSE))
#     if (format == 'dataframe')
#       ret <- data.frame(t(unlist(ret)))
#     return(ret)
#   } else {
#     ret <- data.frame()
#     for (i in 1:length(ip))
#     {
#       r <- freegeoip(ip[i], format="dataframe")
#       ret <- rbind(ret, r)
#     }
#     return(ret)
#   }
# }   
# #ip <- freegeoip(as.character(bigtable$IP.Address_))
# ip_addresses <- as.data.frame(ip)
# bigtable <- inner_join(bigtable, ip_addresses, by = c("IP.Address_" = "ip"))
# write.csv(x = ip, file = "ip.csv")


###### just a crutch to rename the columns in a way that will make it possible to analyze it later. SurveyMonkey didn't append
###### the names of each column with a corresponding question. 
# for (i in 152:713){
#   if(!(gsub(pattern = "(^.)+.*", replacement = "\\1", colnames(bigtable)[i]) == "X")){
#     start_of_the_line <- gsub(pattern = "(^.*_)+.*", replacement = "\\1", colnames(bigtable)[i])
#   }
#   else {
#     colnames(bigtable)[i] <- paste0(start_of_the_line, gsub(pattern = "(^.*_)+(.*)", replacement = "\\2", colnames(bigtable)[i]))
#   }
# }
##### created backup bigtable.csv at this point
#bigtable$X <- NULL

##### deleting empty columns E-mail... 
namesdrop <- bigtable %>%
  select(starts_with("Email.Address_"), 
         starts_with("First.Name_"), 
         starts_with("Lastname_"), 
         starts_with("Custom.Data_")) %>%
  colnames()
bigtable <- bigtable[, !(names(bigtable) %in% namesdrop)]

##### turning columns with dates in appropriate format
bigtable$StartDate_ <- as.POSIXct(bigtable$StartDate_, format = "%m/%d/%Y %H:%M:%S")
bigtable$EndDate_ <- as.POSIXct(bigtable$EndDate_, format = "%m/%d/%Y %H:%M:%S")

##### cleaning up the "other (please specify)" answer
university_other <- bigtable[,9:127]
namesdrop <- colnames(university_other)
university_other <- cbind(university_other, bigtable$RespondentID_)
university_other <- melt(university_other, id = "bigtable$RespondentID_", na.rm = TRUE)
university_other$variable <- "University_other"
university_other <- reshape(university_other, direction = "wide", idvar = "bigtable$RespondentID_", timevar = "variable")
rownames(university_other) <- NULL

bigtable_temp <- left_join(x = bigtable, y = university_other, by = c("RespondentID_" = "bigtable$RespondentID_"))
bigtable <- bigtable_temp
bigtable <- bigtable[, !(names(bigtable) %in% namesdrop)]

to_comb <- bigtable %>%
  select(RespondentID_, X_Other..please.specify..x, 
         value.University_other, value.University.1,value.University.2,value.University.3, value.University.4)
bigtable$X <- NULL

#### the data from combed database is transferred into the main table
bigtable_temp <- left_join(x = bigtable, y = combed, by = "RespondentID_")

compare <- bigtable_temp %>%
  select(RespondentID_, A.2..Select.the.name.of.your.Erasmus.Mundus.master.course._Response, Program_other)
compare$Program_other <- as.character(compare$Program_other)
compare$A.2..Select.the.name.of.your.Erasmus.Mundus.master.course._Response <- as.character(compare$A.2..Select.the.name.of.your.Erasmus.Mundus.master.course._Response)

#### rewritting the answers from "Other..." column into the main column with other courses.
for (i in 1:length(compare$A.2..Select.the.name.of.your.Erasmus.Mundus.master.course._Response)){
  if (is.na(compare$A.2..Select.the.name.of.your.Erasmus.Mundus.master.course._Response[i])){
    compare$A.2..Select.the.name.of.your.Erasmus.Mundus.master.course._Response[i] <- compare$Program_other[i]}
}
compare$A.2..Select.the.name.of.your.Erasmus.Mundus.master.course._Response <- as.factor(compare$A.2..Select.the.name.of.your.Erasmus.Mundus.master.course._Response)
compare$Program_other <- NULL
bigtable_temp <- left_join(x = bigtable_temp, y = compare, by = "RespondentID_")
bigtable_temp$A.2..Select.the.name.of.your.Erasmus.Mundus.master.course._Response.x <- NULL
bigtable <- bigtable_temp
bigtable$Program_other <- NULL

### written the table out on 20150802
write.csv(x = colnames(bigtable), file = "columnnames.csv")

### deleting duplicates based on a unique identifier created by the person in A.1 question. Only the latest record is kept
duplicated(bigtable$A.1..Please.create.a.unique.identification.code..Just.type.in.your.birthday.in.the.format.DD.MO.YY.and.the.first.two.letters.of.your.first.name.followed.by.the.first.two.letters.of.your.first.name.and.first.two.letter.of.your.last.name..For.example..Maris.Miller.was.born.on.October.9..1975..So.her.code.would.be.091075MAMI._Open.Ended.Response)
to_delete <- c(4003978291, 4003914308, 4002127235, 4004768338, 4002327575, 4008453547, 4005483888)
bigtable <- bigtable[!(bigtable$RespondentID_ %in% to_delete),]
write.csv(x = bigtable, file = "bigtable.csv")


### working with questions about each individual universities. Deleting multiple columns corresponding to various situations that students
### chose. For example, columns F correspond to situation when student chose to evaluate only one university, G - 2, H - 3, and I - 4.
### Therefore, there are multiple columns corresponding to the same information that needs to be merged.
columnnames <- read.csv("C:/Users/Misha/Downloads/columnnames.csv", stringsAsFactors = FALSE)
bigtable <- read.csv("../Media/2015/Master_tables/20150802_bigtable_archive.csv")
bigtable$X <- NULL
bigtable_temp <- bigtable
colnames(bigtable_temp) <- columnnames$Name.of.the.column
names_to_drop <- bigtable_temp %>%
  select(RespondentID_,
         starts_with("F."),
         starts_with("G."),
         starts_with("H."),
         starts_with("I.")) 

names_to_drop$I.am.currently._Response <- NULL
names_to_drop$I.am.currently._Other.please.specify. <- NULL
names_ <- colnames(names_to_drop)

names_to_drop <- melt(names_to_drop, id = "RespondentID_", na.rm = TRUE)  
names_to_drop$variable <- substring(names_to_drop$variable, 3)
names_to_drop <- reshape(names_to_drop, direction = "wide", idvar = "RespondentID_", timevar = "variable")

write.csv(x = names_to_drop, file = "questions_individ.csv")

bigtable_temp <- left_join(x = bigtable_temp, y = names_to_drop, by = "RespondentID_")
bigtable_temp <- bigtable_temp[, !(names(bigtable_temp) %in% names_[2:length(names_)])]
write.csv(x = bigtable_temp, file = "bigtable_temp.csv")

bigtable <- bigtable_temp
write.csv(x = bigtable, file = "bigtable.csv")
#written out the table on 6th of Aubust

write.csv(x = colnames(bigtable), file = "colnames.csv")


############ 16th of August 2015
### rename the columns starting with "value" into F.
bigtable <- read.csv("../Media/2015/Master_tables/bigtable.csv", na.strings = c("", " ", "No answer", "N/A", "NA"), header = TRUE)
bigtable$X <- NULL
bigtable_temp <- bigtable
colnames(bigtable_temp) <- gsub("^value", "F", colnames(bigtable_temp))

### merge E.1. columns in one
names_to_drop <- bigtable_temp %>%
  select(RespondentID_,
         starts_with("E.1.What"))

names_to_drop <- melt(names_to_drop, id = "RespondentID_", na.rm = TRUE)  
names_to_drop$variable <- "E.1.What.is.your.current.occupational.status"
names_to_drop <- reshape(names_to_drop, direction = "wide", idvar = "RespondentID_", timevar = "variable")
# lost 817-791 = 26 values (possibly due to people filling in the survey on multiple occassions)


colnames(names_to_drop) <- c("RespondentID_", "E.1.What.is.your.current.occupational.status")
bigtable_temp <- left_join(x = bigtable_temp, y = names_to_drop, by = "RespondentID_")
bigtable_temp$E.1.What.is.current.occupational.status._student <- NULL
bigtable_temp$E.1.What.is.current.occupational.status._employed <- NULL
bigtable_temp$E.1.What.is.current.occupational.status._unemployed <- NULL


### Merge D and E columns in one (both contain information about internship/field experience)
names_to_drop <- bigtable_temp %>%
  select(RespondentID_,
         starts_with("D.")) 
columnnames <- colnames(names_to_drop)

# vector of corrected integer values to insert into the columnnames
to_insert <- as.character(as.integer(substring(columnnames[2:length(columnnames)], 3,4)) + 1)
substring(columnnames[2:length(columnnames)], 3, 4) <- to_insert
colnames(bigtable_temp)[74:100] <- columnnames[2:28]

names_to_drop <- bigtable_temp %>%
  select(RespondentID_,
         starts_with("D."),
         starts_with("E."))

names_to_drop$E.1.What.is.your.current.occupational.status <- NULL

names_ <- colnames(names_to_drop)[2:62]

names_to_drop <- melt(names_to_drop, id = "RespondentID_", na.rm = TRUE)  #25282 values
names_to_drop$variable <- substring(names_to_drop$variable, 3)
names_to_drop <- reshape(names_to_drop, direction = "wide", idvar = "RespondentID_", timevar = "variable")
# didn't lose any data after that step

colnames(names_to_drop) <- gsub("^value", "L", colnames(names_to_drop))
bigtable_temp <- left_join(x = bigtable_temp, y = names_to_drop, by = "RespondentID_")
bigtable_temp <- bigtable_temp[, !(names(bigtable_temp) %in% names_)]
colnames(bigtable_temp)[253] <- "L.1.What.is.your.current.occupational.status"
write.csv(x = colnames(bigtable_temp), file = "columnnames.csv")
bigtable <- bigtable_temp

#######################################################################################
write.csv(x = bigtable, file = "bigtable.csv") #"2015-08-16 14:13:29 CEST"


#######################################################################################
bigtable <- read.csv("../Media/2015/Master_tables/bigtable.csv", na.strings = c("", " ", "No answer", "N/A", "NA"), header = TRUE)
bigtable$X <- NULL
bigtable_temp <- bigtable

### renaming questions about first, second, third, fourth university to have different letters in the beginning for easier processing
colnames(bigtable_temp)[85:126] <- gsub("^.", "N", colnames(bigtable_temp)[85:126])
colnames(bigtable_temp)[127:168] <- gsub("^.", "O", colnames(bigtable_temp)[127:168])
colnames(bigtable_temp)[169:210] <- gsub("^.", "P", colnames(bigtable_temp)[169:210])
colnames(bigtable_temp)[211:252] <- gsub("^.", "Q", colnames(bigtable_temp)[211:252])
write.csv(x = colnames(bigtable_temp), file = "colnames.csv")
bigtable <- bigtable_temp

write.csv(x = bigtable, file = "bigtable.csv") #"2015-08-19 15:26:49 CEST"

#######################################################################################
#deleting _first.university, _second.university and such from the name of the question since now they can be identified with letters
bigtable <- read.csv("../Media/2015/Master_tables/bigtable.csv", na.strings = c("", " ", "No answer", "N/A", "NA"), header = TRUE)
bigtable$X <- NULL
bigtable_temp <- bigtable

colnames(bigtable_temp) <- gsub("_first.university", "", colnames(bigtable_temp))
colnames(bigtable_temp) <- gsub("_second.university", "", colnames(bigtable_temp))
colnames(bigtable_temp) <- gsub("_third.university", "", colnames(bigtable_temp))
colnames(bigtable_temp) <- gsub("_fourth.university", "", colnames(bigtable_temp))
write.csv(x = colnames(bigtable_temp), file = "columnnames.csv")
bigtable <- bigtable_temp

write.csv(x = bigtable, file = "bigtable.csv") #"2015-08-20 10:05:03 CEST"

#######################################################################################
### cleaning the names of courses. there are many courses with wrong names.
bigtable <- read.csv("../Media/2015/Master_tables/bigtable.csv", na.strings = c("", " ", "No answer", "N/A", "NA"), header = TRUE, fileEncoding="latin1")
bigtable$X <- NULL
bigtable_temp <- bigtable

to_remove_duplicates <- bigtable_temp %>% select(starts_with("A.2."), RespondentID_)
write.csv(x = to_remove_duplicates, file = "to_remove_duplicates.csv") #"2015-10-10 14:47:11 CEST"
z <- read.csv("to_remove_duplicates.csv", header = TRUE, fileEncoding="latin1")
bigtable_temp <- left_join(x = bigtable_temp, y = z, by = "RespondentID_")
bigtable_temp$A.2.Select.the.name.of.Erasmus.Mundus.master.course._Response. <- NULL
bigtable <- bigtable_temp

write.csv(x = bigtable, file = "bigtable.csv") #"2015-10-10 16:48:22 CEST"

#######################################################################################
### cleaning names of some courses
bigtable <- read.csv("../Media/2015/Master_tables/bigtable.csv", na.strings = c("", " ", "No answer", "N/A", "NA"), header = TRUE)
bigtable$X <- NULL
bigtable_temp <- bigtable

bigtable_temp$A.2.name.of.Erasmus.Mundus.master.course. <- gsub("GEMMA-Master`s Degree in Women`s and Gender Studies", "GEMMA-Master's Degree in Women's and Gender Studies", bigtable_temp$A.2.name.of.Erasmus.Mundus.master.course.)
bigtable_temp$A.2.name.of.Erasmus.Mundus.master.course. <- gsub("CEMACUBE-Common European Master`s Course in Biomedical Engineering", "CEMACUBE-Common European Master's Course in Biomedical Engineering", bigtable_temp$A.2.name.of.Erasmus.Mundus.master.course.)
bigtable_temp$A.2.name.of.Erasmus.Mundus.master.course. <- gsub("IMQP-International Master in Quaternary and Prehistory Master International en Quaternaire et Pr?f?f?,©histoire", "IMQP-International Master in Quaternary and Prehistory, Master International en Quaternaire et Prehistoire", bigtable_temp$A.2.name.of.Erasmus.Mundus.master.course.)
bigtable_temp$A.2.name.of.Erasmus.Mundus.master.course. <- gsub("MITRA-M?f?f?,©diation interculturelle: identit?f?f?,©s, mobilit?f?f?,©s, conflits", "MITRA-Mediation interculturelle: identites, mobilites, conflits", bigtable_temp$A.2.name.of.Erasmus.Mundus.master.course.)
bigtable_temp$A.2.name.of.Erasmus.Mundus.master.course. <- gsub("AFEPA-European Master?f¢?,?????,?Ts programme in Agricultural, Food and Environmental Policy Analysis ", "AFEPA-European Master's programme in Agricultural, Food and Environmental Policy Analysis", bigtable_temp$A.2.name.of.Erasmus.Mundus.master.course.)
bigtable_temp$A.2.name.of.Erasmus.Mundus.master.course. <- gsub("EMARO+-European Master on Advanced Robotics +", "EMARO - European Master on Advanced Robotics", bigtable_temp$A.2.name.of.Erasmus.Mundus.master.course.)
bigtable_temp$A.2.name.of.Erasmus.Mundus.master.course. <- gsub("MUNDUS JOURNALISM\t-Erasmus Mundus Masters Journalism, Media and Globalisation", "MUNDUS JOURNALISM - Erasmus Mundus Masters Journalism, Media and Globalisation", bigtable_temp$A.2.name.of.Erasmus.Mundus.master.course.)

write.csv(x = bigtable_temp, file = "bigtable.csv") #"2015-11-05 16:25:32 CET"

#######################################################################################
### cleaning up names of universities
### file from google docs
#university_names <- read.csv("C:/Users/Misha/Downloads/University names - Sheet1.csv")

z <- university_names %>%
  select(A.2.name.of.Erasmus.Mundus.master.course., University.1, University.2, University.3, University.4)%>%
  melt(na.rm = TRUE, id = "A.2.name.of.Erasmus.Mundus.master.course.") %>%
  group_by(A.2.name.of.Erasmus.Mundus.master.course.) %>%
  distinct(value)
write.csv(z, "second_sheet.csv") #shared online at "University names"

### checking if everything stayed the same
tenormore <- university_names %>%
  select(A.2.name.of.Erasmus.Mundus.master.course.) %>%
  group_by(A.2.name.of.Erasmus.Mundus.master.course.) %>%
  summarise(respondents = n()) %>%
  filter(respondents >= 10)
colnames(tenormore) <- c("Course", "Respondents")

university_names <- university_names[(university_names$A.2.name.of.Erasmus.Mundus.master.course. %in% tenormore$Course),]

#######################################################################################
### merging 
bigtable <- read.csv("../Media/2015/Master_tables/bigtable.csv", na.strings = c("", " ", "No answer", "N/A", "NA"), header = TRUE)
bigtable$X <- NULL
bigtable_temp <- bigtable
bigtable_temp$University.1 <- NULL
bigtable_temp$University.2 <- NULL
bigtable_temp$University.3 <- NULL
bigtable_temp$University.4 <- NULL

university_names$A.2.name.of.Erasmus.Mundus.master.course. <- NULL

bigtable_temp <- left_join(x = bigtable_temp, y = university_names, by = "RespondentID_")

write.csv(x = bigtable_temp, file = "bigtable.csv") #"2015-11-15 22:45:04 CET"

#######################################################################################
### cleaning typos and other things in a master table

bigtable <- read.csv("../Media/2015/Master_tables/bigtable.csv", na.strings = c("", " ", "No answer", "N/A", "NA"), header = TRUE)
bigtable$X <- NULL
bigtable_temp <- bigtable
names(bigtable_temp) <- gsub("C.1.Rate.the.following.items._Consistency.of.moduleÃƒ..s.assessment.across.universities", "C.1.Rate.the.following.items._Consistency.of.module's.assessment.across.universities", names(bigtable_temp))
names(bigtable_temp) <- gsub("Formalised.system.offered.by.the.university.consortium.through.which.students.can.share.their.opinions.and.provide.feedback.on.the.EM.course", "Formalised.system.by.university.consortium.for.students.to.share.opinions.and.feedback.on.course", names(bigtable_temp))
names(bigtable_temp) <- gsub("L.2.a.Rate.the.following.statements.about.internship._Overall.quality.of.the.internship_2", "L.2.a.Rate.the.following.statements.about.internship._Overall.quality.of.the.internship", names(bigtable_temp))
names(bigtable_temp) <- gsub("Health.Insurance", ("Health.insurance"), names(bigtable_temp))

bigtable_temp$A.2.name.of.Erasmus.Mundus.master.course. <- gsub("&", "and", bigtable_temp$A.2.name.of.Erasmus.Mundus.master.course.)
bigtable_temp$A.2.name.of.Erasmus.Mundus.master.course. <- gsub(":", ".", bigtable_temp$A.2.name.of.Erasmus.Mundus.master.course.)

#### need to find out about L.3.a.Rate.the.following.statements.about.field.experience._Overall.quality.of.field.experience_2 
# field_experience <- bigtable_temp %>%
#   select(RespondentID_, 
#          L.3.a.Rate.the.following.statements.about.field.experience._Overall.quality.of.field.experience_2,
#          L.3.a.Rate.the.following.statements.about.field.experience._Overall.quality.of.field.experience) %>%
#   melt(id = "RespondentID_")
# 
# field_experience$variable <- "L.3.a.Rate.the.following.statements.about.field.experience._Overall.quality.of.field.experience"
# sum(!is.na(field_experience$value)) #376
# 
# field_experience_cast <- reshape(field_experience, direction = "wide", idvar = "RespondentID_", timevar = "variable")
# sum(!is.na(field_experience_cast$value.L.3.a.Rate.the.following.statements.about.field.experience._Overall.quality.of.field.experience)) #200
write.csv(x = bigtable_temp, file = "bigtable.csv") #"2016-01-17 17:34:08 MSK"


#######################################################################################
### making names of universities consistent
bigtable <- read.csv("../Media/2015/Master_tables/bigtable.csv", na.strings = c("", " ", "No answer", "N/A", "NA"), header = TRUE)
bigtable$X <- NULL
bigtable_temp <- bigtable

#gsub("University of Leuven, Belgium", "Catholic University of Leuven, Belgium")
bigtable_temp$University.1 <- gsub("National Polytechnic Institute of Lorraine, France", "University of Lorraine, France", bigtable_temp$University.1)
bigtable_temp$University.2 <- gsub("National Polytechnic Institute of Lorraine, France", "University of Lorraine, France", bigtable_temp$University.2)
bigtable_temp$University.3 <- gsub("National Polytechnic Institute of Lorraine, France", "University of Lorraine, France", bigtable_temp$University.3)
bigtable_temp$University.4 <- gsub("National Polytechnic Institute of Lorraine, France", "University of Lorraine, France", bigtable_temp$University.4)

bigtable_temp$University.1 <- gsub("Institute of Santarem, Portugal", "Polytechnic Institute of Santarem, Portugal", bigtable_temp$University.1)
bigtable_temp$University.2 <- gsub("Institute of Santarem, Portugal", "Polytechnic Institute of Santarem, Portugal", bigtable_temp$University.2)
bigtable_temp$University.3 <- gsub("Institute of Santarem, Portugal", "Polytechnic Institute of Santarem, Portugal", bigtable_temp$University.3)
bigtable_temp$University.4 <- gsub("Institute of Santarem, Portugal", "Polytechnic Institute of Santarem, Portugal", bigtable_temp$University.4)

bigtable_temp$University.1 <- gsub("Technische Universitat Berlin, Germany", "Technical University of Berlin, Germany", bigtable_temp$University.1)
bigtable_temp$University.2 <- gsub("Technische Universitat Berlin, Germany", "Technical University of Berlin, Germany", bigtable_temp$University.2)
bigtable_temp$University.3 <- gsub("Technische Universitat Berlin, Germany", "Technical University of Berlin, Germany", bigtable_temp$University.3)
bigtable_temp$University.4 <- gsub("Technische Universitat Berlin, Germany", "Technical University of Berlin, Germany", bigtable_temp$University.4)

bigtable_temp$University.1 <- gsub("Universita degli Studi Palermo, Italy", "University of Palermo, Italy", bigtable_temp$University.1)
bigtable_temp$University.2 <- gsub("Universita degli Studi Palermo, Italy", "University of Palermo, Italy", bigtable_temp$University.2)
bigtable_temp$University.3 <- gsub("Universita degli Studi Palermo, Italy", "University of Palermo, Italy", bigtable_temp$University.3)
bigtable_temp$University.4 <- gsub("Universita degli Studi Palermo, Italy", "University of Palermo, Italy", bigtable_temp$University.4)

bigtable_temp$University.1 <- gsub("Vu University Amsterdam, Netherlands", "VU University Amsterdam, Netherlands", bigtable_temp$University.1)
bigtable_temp$University.2 <- gsub("Vu University Amsterdam, Netherlands", "VU University Amsterdam, Netherlands", bigtable_temp$University.2)
bigtable_temp$University.3 <- gsub("Vu University Amsterdam, Netherlands", "VU University Amsterdam, Netherlands", bigtable_temp$University.3)
bigtable_temp$University.4 <- gsub("Vu University Amsterdam, Netherlands", "VU University Amsterdam, Netherlands", bigtable_temp$University.4)

bigtable_temp$University.1 <- gsub("Universidade Catolica Portuguesa, Portugal", "Portuguese Catholic University, Portugal", bigtable_temp$University.1)
bigtable_temp$University.2 <- gsub("Universidade Catolica Portuguesa, Portugal", "Portuguese Catholic University, Portugal", bigtable_temp$University.2)
bigtable_temp$University.3 <- gsub("Universidade Catolica Portuguesa, Portugal", "Portuguese Catholic University, Portugal", bigtable_temp$University.3)
bigtable_temp$University.4 <- gsub("Universidade Catolica Portuguesa, Portugal", "Portuguese Catholic University, Portugal", bigtable_temp$University.4)

bigtable_temp$University.1 <- gsub("Instituto Superior Tecnico (IST) Lisbon, Portugal", "Technical University of Lisbon, Portugal", bigtable_temp$University.1)
bigtable_temp$University.2 <- gsub("Instituto Superior Tecnico (IST) Lisbon, Portugal", "Technical University of Lisbon, Portugal", bigtable_temp$University.2)
bigtable_temp$University.3 <- gsub("Instituto Superior Tecnico (IST) Lisbon, Portugal", "Technical University of Lisbon, Portugal", bigtable_temp$University.3)
bigtable_temp$University.4 <- gsub("Instituto Superior Tecnico (IST) Lisbon, Portugal", "Technical University of Lisbon, Portugal", bigtable_temp$University.4)

bigtable_temp$University.1 <- gsub("Ecole polytechnique federale de Lausanne, Switzerland", "Swiss Federal Institute of Technology Lausanne, Switzerland", bigtable_temp$University.1)
bigtable_temp$University.2 <- gsub("Ecole polytechnique federale de Lausanne, Switzerland", "Swiss Federal Institute of Technology Lausanne, Switzerland", bigtable_temp$University.2)
bigtable_temp$University.3 <- gsub("Ecole polytechnique federale de Lausanne, Switzerland", "Swiss Federal Institute of Technology Lausanne, Switzerland", bigtable_temp$University.3)
bigtable_temp$University.4 <- gsub("Ecole polytechnique federale de Lausanne, Switzerland", "Swiss Federal Institute of Technology Lausanne, Switzerland", bigtable_temp$University.4)

write.csv(x = bigtable_temp, file = "bigtable.csv") #"2016-01-23 18:21:39 MSK"
