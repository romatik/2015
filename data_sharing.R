library(dplyr)
### For qualitative team
bigtable <- read.csv("../Media/2015/Master_tables/bigtable.csv")
qualitative_df <- bigtable %>%
  select(RespondentID_,
         A.2.Select.the.name.of.Erasmus.Mundus.master.course._Response.,
         University.1,
         University.2,
         University.3,
         University.4,
         starts_with("A.4.."),
         A.7.What.is.nationality.please.choose.one.only._Response,
         X.21_Other.please.specify.,
         What.is.second.nationality.please.choose.one.if.applicable._Response,
         X.22_Other.please.specify.,
         A.9.Gender._Response,
         X.23_Other.,
         A.10.Have.you.been.awarded.an.Erasmus.Mundus.scholarship.for.master.studies._Response,
         contains("Open.Ended.Response"),
         C.2.Did.the.required.mobility.path.of.EM.course.meet.academic.needs._Other.please.specify.,
         I.am.currently._Response)

### deleting the question about sexual harassment
qualitative_df[19] <- NULL
qualitative_df$A.1.Please.create.a.unique.identification.code.Just.type.in.birthday.in.the.format.DD.MO.YY.and.the.first.two.letters.of.first.name.followed.by.the.first.two.letters.of.first.name.and.first.two.letter.of.last.name.For.example.Maris.Miller.was.born.on.October.9.1975.So.her.code.would.be.091075MAMI._Open.Ended.Response <- NULL
write.csv(x = qualitative_df, file = "../Media/2015/Master_tables/qualitative_team.csv")
