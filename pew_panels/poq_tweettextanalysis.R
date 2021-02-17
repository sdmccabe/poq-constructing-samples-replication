library(tidyverse)
library(survey)
library(haven)
library(pewmethods)
library(ineq)

#load behavioral data as "stats" and atp data as "dta"
stats <- read_csv("datasets/atp_profile_data_20200124.csv")
id_to_qkey <- read_csv("datasets/qkey_to_database_ID.csv")
stats <- merge(stats, id_to_qkey, by.x='id', by.y='database_id') %>% select(-id, -X1.x, -X1.y)
dta <- read_sav("datasets/ATP W57.sav") %>% as_factor()
dta_num <- read_sav("datasets/ATP W57.sav")

# generate account age from 11/1/19
stats$acct_age <- (as.Date("2019-11-01 12:00:00") - as.Date(stats$created_at))
stats$acct_age <- as.numeric(stats$acct_age)

# merge behavior and survey
dta <- left_join(stats, dta)
get_totals("is_private",dta, wt="WEIGHT_W57",include_unw = T,digits=0)

# remove private
dta <- filter(dta, is_private==FALSE)
dtadesign<-svydesign(id=~1,weights=~WEIGHT_W57, data=dta)
get_totals("F_REG",dta,wt="WEIGHT_W57")
gfk_raw <- as_factor(read_sav("datasets/Pew_Twitter Study_updated_weights.sav"))

gfk_recoded <-  gfk_raw %>%
  filter(TWITTER == "Yes") %>%
  #recode Gfk  
  mutate(F_CREGION = PPREG4, 
         F_STATE = PPSTATEN,
         F_METRO = fct_recode(PPMSACAT, 
                              "Metropolitan" = "Metro",
                              "Non-metropolitan" = "Non-Metro"
         ),
         F_AGE = PPAGE,
         F_AGECAT = cut(F_AGE, c(-Inf,29,49,64,Inf),
                        labels = c("18-29", "30-49", "50-64", "65+")
         ),
         F_AGECAT2 = cut(F_AGE, c(-Inf,49,Inf),
                         labels = c("18-49", "50+")
         ),
         YOB = 2019 - PPAGE, 
         F_SEX = PPGENDER, 
         F_EDUCCAT2 = fct_collapse(PPEDUC,
                                   "Less than high school" = c(
                                     "No formal education", "1st, 2nd, 3rd, or 4th grade", "5th or 6th grade", "7th or 8th grade",                                         
                                     "9th grade", "10th grade", "11th grade", "12th grade NO DIPLOMA" 
                                   ),
                                   "High school graduate" = "HIGH SCHOOL GRADUATE - high school DIPLOMA or the equivalent (GED)",
                                   "Some college, no degree" = "Some college, no degree",
                                   "Associate's degree" = "Associate degree",
                                   "College graduate/some post grad" = "Bachelors degree",
                                   "Postgraduate" = c("Masters degree",
                                                      "Professional or Doctorate degree")
         ),
         F_EDUCCAT = fct_collapse(F_EDUCCAT2, 
                                  "College graduate+" = c(
                                    "College graduate/some post grad", "Postgraduate" 
                                  ), 
                                  "Some College" = c(
                                    "Some college, no degree", "Associate's degree"
                                  ),
                                  "H.S. graduate or less" = c(
                                    "Less than high school", "High school graduate" 
                                  )
         ),
         COLLEGE = fct_collapse(F_EDUCCAT2, 
                                "College graduate+" = c(
                                  "College graduate/some post grad", "Postgraduate" 
                                ), 
                                "Non College" = c(
                                  "Some college, no degree", "Associate's degree",
                                  "Less than high school", "High school graduate"
                                )
         ),
         EDUCFINAL = fct_collapse(F_EDUCCAT2, 
                                  "College graduate+" = c(
                                    "College graduate/some post grad", "Postgraduate" 
                                  ), 
                                  "High school graduate" = c(
                                    "Some college, no degree", "Associate's degree",
                                    "High school graduate"
                                  ), 
                                  "Less than high school" = c(
                                    "Less than high school"
                                  )),
         F_RACETHN = fct_collapse(PPETHM, 
                                  "White non-Hispanic" = "White, Non-Hispanic",
                                  "Black non-Hispanic" = "Black, Non-Hispanic",
                                  "Hispanic" = "Hispanic",
                                  "Other" = c(
                                    "Other, Non-Hispanic", "2+ Races, Non-Hispanic"
                                  )
         ) %>% fct_relevel("White non-Hispanic","Black non-Hispanic", "Hispanic", "Other"),
         F_PARTY_FINAL = PARTY,
         F_PARTYLN_FINAL = PARTYLN,
         F_PARTYSUM_FINAL = fct_case_when(
           F_PARTY_FINAL == "Republican" | F_PARTYLN_FINAL == "The Republican Party" ~ "Rep/Lean Rep",
           F_PARTY_FINAL == "Democrat" | F_PARTYLN_FINAL == "The Democratic Party" ~ "Dem/Lean Dem",
           F_PARTY_FINAL %in% c("Independent","Something else", "Refused") & F_PARTYLN_FINAL == "Refused" ~ "DK/Refused/No lean"
         ),
         IDEOSELF_num = case_when(IDEOSELF == "Refused" ~ NA_real_, 
                                  TRUE ~ as.numeric(IDEOSELF))-1,
         F_INCOME7 = fct_collapse(PPINCIMP,
                                  ">$150k" = c("$150,000 to $174,999", "$175,000 to $199,999",
                                               "$200,000 to $249,999","$250,000 or more"
                                  ),
                                  "$100-150k" = c("$100,000 to $124,999", "$125,000 to $149,999"
                                  ),
                                  "$75-100k" = c("$75,000 to $84,999", "$85,000 to $99,999"
                                  ),
                                  "$50-75k" = c("$50,000 to $59,999", "$60,000 to $74,999"
                                  ),
                                  "$30-50k" = c("$30,000 to $34,999", "$35,000 to $39,999", 
                                                "$40,000 to $49,999"
                                  ),
                                  "$10-30k" = c("$10,000 to $12,499", "$12,500 to $14,999", "$15,000 to $19,999", 
                                                "$20,000 to $24,999", "$25,000 to $29,999"
                                  ),
                                  "<$10k" = c("Less than $5,000","$5,000 to $7,499", "$7,500 to $9,999")
         ),
         F_INCOME_RECODE = fct_collapse(F_INCOME7,
                                        "$75,000+" = c("$75-100k", "$100-150k", ">$150k"),
                                        "$30-$74,999" = c("$50-75k", "$30-50k"),
                                        "<$30,000" = c("$10-30k", "<$10k")
         ),
         F_AGE_num = as.numeric(as.character(F_AGE))
  )

# load behavioral data and merge with gfk
gfk_twitter <- read_csv("datasets/metadata_010820.csv")
####
gfk_twitter <- gfk_twitter %>%
  mutate(CaseID = as.numeric(respondent__CaseID)) %>%
  select(CaseID, favorites_count, created_at, statuses_count, followings_count, followers_count, is_private)

gfk_recoded = gfk_recoded %>%
  left_join(gfk_twitter, by="CaseID")
gfk_recoded$acct_age <- (as.Date("2019-11-01 12:00:00") - as.Date(gfk_recoded$created_at))
gfk_recoded$acct_age <- as.numeric(gfk_recoded$acct_age)

pewmethods::get_totals(var = "is_private",df = gfk_recoded,wt = 'weight',include_unw = T,digits=0)

# remove private
gfk_recoded <- filter(gfk_recoded, is_private==FALSE)


## metadata 

gfk_recoded <- gfk_recoded %>% mutate(
  lessten = case_when(statuses_count<=9 ~ 1,
                       TRUE ~ 0)
)
# set survey design
gfkdesign<-svydesign(id=~1,weights=~weight, data=gfk_recoded)
summary(gfk_recoded$lessten)
svymean(~lessten,design=gfkdesign)



## load tweets

kp_tweets <- read_csv("datasets/kp_tweets_from_20190901_to_20200101_pulled_20200128.csv")
atp_tweets <- read_csv("datasets/atp_tweets_from_20191001_to_20200101_pulled_20200128.csv")

kp_tweets <- filter(kp_tweets, created_at <= "2019-11-30 18:59:59")
kp_tweets <- filter(kp_tweets, created_at >= "2019-10-01 00:00:00")

atp_tweets <- filter(atp_tweets, created_at <= "2019-11-30 18:59:59")
atp_tweets <- filter(atp_tweets, created_at >= "2019-10-01 00:00:00")

kp_tweets <- kp_tweets %>% mutate(
  tbt = case_when(grepl("#tbt", text,ignore.case=TRUE)==T ~ 1,
                  TRUE ~ 0),
  impeach = case_when(grepl("impeach", text,ignore.case=TRUE)==T ~ 1,
                      TRUE ~ 0),
  trump = case_when(grepl("trump", text,ignore.case=TRUE)==T ~ 1,
                    TRUE ~ 0),
  rep = case_when(grepl("republican", text,ignore.case=TRUE)==T ~ 1,
                  TRUE ~ 0),
  dem = case_when(grepl("democrat", text,ignore.case=TRUE)==T ~ 1,
                  TRUE ~ 0),
  reply = case_when(grepl("^@\\S+", text,ignore.case=T)==T ~ 1,
                    TRUE ~ 0),
  retweet = case_when(is.na(retweeted_status)==F ~ 1,
                      TRUE ~ 0)
)
atp_tweets <- atp_tweets %>% mutate(
  tbt = case_when(grepl("#tbt", text,ignore.case=TRUE)==T ~ 1,
                      TRUE ~ 0),
  impeach = case_when(grepl("impeach", text,ignore.case=TRUE)==T ~ 1,
                      TRUE ~ 0),
  trump = case_when(grepl("trump", text,ignore.case=TRUE)==T ~ 1,
                      TRUE ~ 0),
  rep = case_when(grepl("republican", text,ignore.case=TRUE)==T ~ 1,
                      TRUE ~ 0),
  dem = case_when(grepl("democrat", text,ignore.case=TRUE)==T ~ 1,
                      TRUE ~ 0),
  reply = case_when(grepl("^@\\S+", text,ignore.case=T)==T ~ 1,
                    TRUE ~ 0),
  retweet = case_when(is.na(retweeted_status)==F ~ 1,
                      TRUE ~ 0)
)

kp_tweets$CaseID <- kp_tweets$profile__respondent__CaseID

# collapse by respondent, include N tweets and N political tweets

kp_collapse <- kp_tweets %>% 
  group_by(CaseID) %>% summarise(
    n_tweets = n(),
    n_RTs = sum(retweet),
    n_replies = sum(reply),
    n_impeach = sum(impeach),
    one_impeach = max(impeach),
    n_tbt = sum(tbt),
    one_tbt = max(tbt),
    n_trump = sum(trump),
    one_trump = max(trump),
    n_rep = sum(rep),
    one_rep = max(rep),
    n_dem = sum(dem),
    one_dem = max(dem),
    ctext = paste0(text, collapse = "")
  )
#join with the survey
#gfk_recoded$CaseID <- as.factor(gfk_recoded$CaseID)
gfk_recoded <- left_join(gfk_recoded,kp_collapse,by="CaseID")

gfk_recoded$n_tweets[which(is.na(gfk_recoded$n_tweets)==T)] <- 0
gfk_recoded$n_impeach[which(is.na(gfk_recoded$n_impeach)==T)] <- 0
gfk_recoded$one_impeach[which(is.na(gfk_recoded$one_impeach)==T)] <- 0
gfk_recoded$n_tbt[which(is.na(gfk_recoded$n_tbt)==T)] <- 0
gfk_recoded$one_tbt[which(is.na(gfk_recoded$one_tbt)==T)] <- 0
gfk_recoded$n_trump[which(is.na(gfk_recoded$n_trump)==T)] <- 0
gfk_recoded$one_trump[which(is.na(gfk_recoded$one_trump)==T)] <- 0
gfk_recoded$n_dem[which(is.na(gfk_recoded$n_dem)==T)] <- 0
gfk_recoded$one_dem[which(is.na(gfk_recoded$one_dem)==T)] <- 0
gfk_recoded$n_rep[which(is.na(gfk_recoded$n_rep)==T)] <- 0
gfk_recoded$one_rep[which(is.na(gfk_recoded$one_rep)==T)] <- 0

gfk_recoded$n_RTs[which(is.na(gfk_recoded$n_RTs)==T)] <- 0
gfk_recoded$share_RTs <- gfk_recoded$n_RTs /  gfk_recoded$n_tweets
gfk_recoded$share_replies <- gfk_recoded$n_replies /  gfk_recoded$n_tweets

gfk_recoded <- gfk_recoded %>% mutate(
  notweets = case_when(n_tweets==0 ~ 1,
                       TRUE ~ 0)
)

gfkdesign<-svydesign(id=~1,weights=~weight, data=gfk_recoded)
svymean(~notweets,design=gfkdesign)
summary(gfk_recoded$notweets)

gfk_recoded2<- filter(gfk_recoded, n_tweets>=1 )
gfk_recoded2<- filter(gfk_recoded, n_tweets>=1 & n_tweets <=3200)
gfkdesign2<-svydesign(id=~1,weights=~weight, data=gfk_recoded2)

svymean(~share_RTs,design=gfkdesign2,na.rm=T)
summary(gfk_recoded2$share_RTs)
svymean(~share_replies,design=gfkdesign2,na.rm=T)
summary(gfk_recoded2$share_replies)

#svymean(~n_tweets,design=gfkdesign)
svymean(~one_impeach,design=gfkdesign2)
svymean(~one_trump,design=gfkdesign2)
svymean(~one_dem,design=gfkdesign2)
svymean(~one_rep,design=gfkdesign2)
svymean(~one_tbt,design=gfkdesign2)

gfkdesign3<-svydesign(id=~1,weights=~weight, data=filter(gfk_recoded2, n_impeach>=1))
svyquantile(~n_impeach,design=gfkdesign3,quantiles=.5)
summary(filter(gfk_recoded2, n_impeach>=1)$n_impeach)
svymean(~n_impeach,design=gfkdesign3)
sum(gfk_recoded2$n_impeach)
sum(gfk_recoded2$one_impeach)

gfkdesign3<-svydesign(id=~1,weights=~weight, data=filter(gfk_recoded2, n_trump>=1))
svyquantile(~n_trump,design=gfkdesign3,quantiles=.5)
summary(filter(gfk_recoded2, n_trump>=1)$n_trump)
svymean(~n_trump,design=gfkdesign3)
sum(gfk_recoded2$n_trump)
sum(gfk_recoded2$one_trump)

gfkdesign3<-svydesign(id=~1,weights=~weight, data=filter(gfk_recoded2, n_rep>=1))
svyquantile(~n_rep,design=gfkdesign3,quantiles=.5)
summary(filter(gfk_recoded2, n_rep>=1)$n_rep)
svymean(~n_rep,design=gfkdesign3)
sum(gfk_recoded2$n_rep)
sum(gfk_recoded2$one_rep)

gfkdesign3<-svydesign(id=~1,weights=~weight, data=filter(gfk_recoded2, n_dem>=1))
svyquantile(~n_dem,design=gfkdesign3,quantiles=.5)
summary(filter(gfk_recoded2, n_dem>=1)$n_dem)
svymean(~n_dem,design=gfkdesign3)
sum(gfk_recoded2$n_dem)
sum(gfk_recoded2$one_dem)

gfkdesign3<-svydesign(id=~1,weights=~weight, data=filter(gfk_recoded2, n_tbt>=1))
svyquantile(~n_tbt,design=gfkdesign3,quantiles=.5)
summary(filter(gfk_recoded2, n_tbt>=1)$n_tbt)
svymean(~n_tbt,design=gfkdesign3)
sum(gfk_recoded2$n_tbt)
sum(gfk_recoded2$one_tbt)

gfk_recoded2 <- gfk_recoded2 %>% mutate(
  topten = case_when(n_tweets >= quantile(n_tweets, .9) ~ 1,
                     TRUE ~ 0)
)
#gfk_recoded <- filter(gfk_recoded, n_tweets<=100000)
top <- filter(gfk_recoded2, topten==1)
rest <- filter(gfk_recoded2, topten==0)
toptentotaltweets_unw <- sum(top$n_tweets)
resttotaltweets_unw <- sum(rest$n_tweets)
perc_topten_unw_kp <- round(toptentotaltweets_unw/(toptentotaltweets_unw+resttotaltweets_unw),2)*100
perc_topten_unw_kp


topten_cuttoff <- survey::svyquantile(~n_tweets,gfkdesign2,quantile=c(.9), na.rm=T) %>% pluck(1)
gfk_recoded2 <- gfk_recoded2 %>% mutate(
  topten = case_when(n_tweets>=topten_cuttoff ~ 1,
                     n_tweets<topten_cuttoff ~ 0,
                     TRUE ~ NA_real_)
)
gfk_recoded2$weightsumstatus <- gfk_recoded2$weight*gfk_recoded2$n_tweets
gfk_recoded2$sumstatus <- gfk_recoded2$n_tweets

top <- filter(gfk_recoded2, topten==1)
rest <- filter(gfk_recoded2, topten==0)
toptentotaltweets <- sum(top$weightsumstatus)
resttotaltweets <- sum(rest$weightsumstatus)
perc_topten_kp <- round(toptentotaltweets/(toptentotaltweets+resttotaltweets),2)*100
perc_topten_kp

ineq(gfk_recoded2$n_tweets,type="Gini")
ineq((gfk_recoded2$n_tweets*gfk_recoded2$weight),type="Gini")


### ATP 

atp_collapse <- atp_tweets %>% 
  group_by(profile__pk) %>% summarise(
    n_tweets = n(),
    n_RTs = sum(retweet),
    n_replies = sum(reply),
    n_impeach = sum(impeach),
    one_impeach = max(impeach),
    n_tbt = sum(tbt),
    one_tbt = max(tbt),
    n_trump = sum(trump),
    one_trump = max(trump),
    n_rep = sum(rep),
    one_rep = max(rep),
    n_dem = sum(dem),
    one_dem = max(dem),
    ctext = paste0(text, collapse = "")
  )
#join with the survey
atp_collapse <- merge(atp_collapse, id_to_qkey, by.x='profile__pk', by.y='database_id')

atp <- left_join(dta,atp_collapse,by="QKEY")
atp$n_tweets[which(is.na(atp$n_tweets)==T)] <- 0
atp$n_impeach[which(is.na(atp$n_impeach)==T)] <- 0
atp$one_impeach[which(is.na(atp$one_impeach)==T)] <- 0
atp$n_trump[which(is.na(atp$n_trump)==T)] <- 0
atp$one_trump[which(is.na(atp$one_trump)==T)] <- 0
atp$n_dem[which(is.na(atp$n_dem)==T)] <- 0
atp$one_dem[which(is.na(atp$one_dem)==T)] <- 0
atp$n_rep[which(is.na(atp$n_rep)==T)] <- 0
atp$one_rep[which(is.na(atp$one_rep)==T)] <- 0
atp$n_tbt[which(is.na(atp$n_tbt)==T)] <- 0
atp$one_tbt[which(is.na(atp$one_tbt)==T)] <- 0


atp$n_RTs[which(is.na(atp$n_RTs)==T)] <- 0
atp$share_RTs <- atp$n_RTs /  atp$n_tweets
atp$share_replies <- atp$n_replies /  atp$n_tweets

atp <- atp %>% mutate(
  notweets = case_when(n_tweets==0 ~ 1,
                       TRUE ~ 0)
)

atpdesign<-svydesign(id=~1,weights=~WEIGHT_W57, data=atp)
svymean(~notweets,design=atpdesign)
summary(atp$notweets)

atp2<- filter(atp, n_tweets>=1 & n_tweets <=3200)
atp2<- filter(atp, n_tweets>=1 )
atpdesign2<-svydesign(id=~1,weights=~WEIGHT_W57, data=atp2)
svymean(~share_RTs,design=atpdesign2,na.rm=T)
summary(atp2$share_RTs)
svymean(~share_replies,design=atpdesign2,na.rm=T)
summary(atp2$share_replies)

svymean(~one_impeach,design=atpdesign2,na.rm=T)
svymean(~one_trump,design=atpdesign2,na.rm=T)
svymean(~one_rep,design=atpdesign2,na.rm=T)
svymean(~one_dem,design=atpdesign2,na.rm=T)
svymean(~one_tbt,design=atpdesign2,na.rm=T)


atpdesign3<-svydesign(id=~1,weights=~WEIGHT_W57, data=filter(atp, n_impeach>=1))
summary(filter(atp2, n_impeach>=1)$n_impeach)
svymean(~n_impeach,design=atpdesign3)
svyquantile(~n_impeach,design=atpdesign3,quantiles=.5)
sum(atp2$n_impeach)
sum(atp2$one_impeach)

atpdesign3<-svydesign(id=~1,weights=~WEIGHT_W57, data=filter(atp, n_trump>=1))
summary(filter(atp2, n_trump>=1)$n_trump)
svymean(~n_trump,design=atpdesign3)
svyquantile(~n_trump,design=atpdesign3,quantiles=.5)
sum(atp2$n_trump)
sum(atp2$one_trump)

atpdesign3<-svydesign(id=~1,weights=~WEIGHT_W57, data=filter(atp, n_rep>=1))
summary(filter(atp2, n_rep>=1)$n_rep)
svymean(~n_rep,design=atpdesign3)
svyquantile(~n_rep,design=atpdesign3,quantiles=.5)
sum(atp2$n_rep)
sum(atp2$one_rep)

atpdesign3<-svydesign(id=~1,weights=~WEIGHT_W57, data=filter(atp, n_dem>=1))
summary(filter(atp2, n_dem>=1)$n_dem)
svymean(~n_dem,design=atpdesign3)
svyquantile(~n_dem,design=atpdesign3,quantiles=.5)
sum(atp2$n_dem)
sum(atp2$one_dem)

atpdesign3<-svydesign(id=~1,weights=~WEIGHT_W57, data=filter(atp, n_tbt>=1))
summary(filter(atp2, n_tbt>=1)$n_tbt)
svymean(~n_tbt,design=atpdesign3)
svyquantile(~n_tbt,design=atpdesign3,quantiles=.5)
sum(atp2$n_tbt)
sum(atp2$one_tbt)

sum(atp2$one_impeach)

atp2 <- atp2 %>% mutate(
  topten = case_when(n_tweets >= quantile(n_tweets, .9) ~ 1,
                     TRUE ~ 0)
)
top <- filter(atp2, topten==1)
rest <- filter(atp2, topten==0)
toptentotaltweets_unw <- sum(top$n_tweets)
resttotaltweets_unw <- sum(rest$n_tweets)
perc_topten_unw_atp <- round(toptentotaltweets_unw/(toptentotaltweets_unw+resttotaltweets_unw),2)*100
perc_topten_unw_atp




topten_cuttoff <- survey::svyquantile(~n_tweets,atpdesign2,quantile=c(.9), na.rm=T) %>% pluck(1)
atp2 <- atp2 %>% mutate(
  topten = case_when(n_tweets>=topten_cuttoff ~ 1,
                     n_tweets<topten_cuttoff ~ 0,
                     TRUE ~ NA_real_)
)
atp2$weightsumstatus <- atp2$WEIGHT_W57*atp2$n_tweets
atp2$sumstatus <- atp2$n_tweets

top <- filter(atp2, topten==1)
rest <- filter(atp2, topten==0)
toptentotaltweets <- sum(top$weightsumstatus)
resttotaltweets <- sum(rest$weightsumstatus)
perc_topten_atp <- round(toptentotaltweets/(toptentotaltweets+resttotaltweets),2)*100
perc_topten_atp

ineq(atp2$n_tweets,type="Gini")
ineq((atp2$n_tweets*atp2$WEIGHT_W57),type="Gini")


### END OF TABLE 2

kp_tweets <- kp_tweets %>% mutate(
  impeach = case_when(grepl("impeach", text,ignore.case=TRUE)==T ~ 1,
                      TRUE ~ 0),
  retweet = case_when(is.na(retweeted_status)==F ~ 1,
                      TRUE ~ 0)
)
atp_tweets <- atp_tweets %>% mutate(
  impeach = case_when(grepl("impeach", text,ignore.case=TRUE)==T ~ 1,
                      TRUE ~ 0),
  retweet = case_when(is.na(retweeted_status)==F ~ 1,
                      TRUE ~ 0)
)

kp_tweets$CaseID <- kp_tweets$profile__respondent__CaseID

# collapse by respondent, include N tweets and N political tweets
kp_tweets <- filter(kp_tweets, retweet==0)
atp_tweets <- filter(atp_tweets, retweet==0)

kp_collapse <- kp_tweets %>% 
  group_by(CaseID) %>% summarise(
    n_tweets = n(),
    n_RTs = sum(retweet),
    n_impeach = sum(impeach),
    one_impeach = max(impeach),
    ctext = paste0(text, collapse = "")
  )
#join with the survey
#gfk_recoded$CaseID <- as.factor(gfk_recoded$CaseID)
gfk_recoded <- left_join(gfk_recoded,kp_collapse,by="CaseID")


gfk_recoded <- gfk_recoded %>% mutate(
  notweets = case_when(n_tweets==0 ~ 1,
                       TRUE ~ 0)
)

gfkdesign<-svydesign(id=~1,weights=~weight, data=gfk_recoded)

### ATP 

atp_collapse <- atp_tweets %>% 
  group_by(profile__pk) %>% summarise(
    n_tweets = n(),
    n_RTs = sum(retweet),
    n_impeach = sum(impeach),
    one_impeach = max(impeach),
    ctext = paste0(text, collapse = "")
  )
#join with the survey
atp_collapse <- merge(atp_collapse, id_to_qkey, by.x='profile__pk', by.y='database_id')

atp <- left_join(dta,atp_collapse,by="QKEY")
atp <- filter(atp, n_tweets>=1 & n_tweets <=3200)
gfk_recoded<- filter(gfk_recoded, n_tweets>=1 & n_tweets <=3200)

