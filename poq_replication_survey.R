library(tidyverse)
library(survey)
library(haven)
library(pewmethods)

#load behavioral data as "stats" and atp data as "dta"
stats <- read_csv("datasets/qkey_accountstats.csv") 
dta <- read_sav("datasets/ATP W57.sav") %>% as_factor() 

dta <- dta %>% 
  mutate(age_num = case_when(
    F_AGE == "DK/Ref" ~ NA_real_,
    F_AGE == "98+" ~ 98,
    TRUE ~ as.numeric(as.character(F_AGE)
    )),
    party_three = case_when(
      F_PARTY_FINAL=="Republican" ~ "Rep",
      F_PARTY_FINAL=="Democrat" ~ "Dem",
      F_PARTY_FINAL=="Independent" ~ "IndOth",
      F_PARTY_FINAL=="Something else" ~ "IndOth",
      F_PARTY_FINAL=="Refused" ~ "IndOth",
    ))                  

# calculate demographics - all RVs
dta_rv <- filter(dta, F_REG=="You are ABSOLUTELY CERTAIN that you are registered to vote at your current address")
rvdesign<-svydesign(id=~1,weights=~WEIGHT_W57, data=dta_rv)
get_totals("F_AGECAT",dta_rv, wt="WEIGHT_W57",include_unw = T,digits=2)
svymean(~F_AGECAT, rvdesign, na.rm=T)
get_totals("F_RACETHN",dta_rv, wt="WEIGHT_W57",include_unw = T,digits=2)
svymean(~F_RACETHN, rvdesign, na.rm=T)
get_totals("F_SEX",dta_rv, wt="WEIGHT_W57",include_unw = T,digits=2)
svymean(~F_SEX, rvdesign, na.rm=T)
get_totals("F_PARTY_FINAL",dta_rv, wt="WEIGHT_W57",include_unw = T,digits=2)
get_totals("party_three",dta_rv, wt="WEIGHT_W57",include_unw = T,digits=2)
svymean(~party_three, rvdesign, na.rm=T)
get_totals("F_EDUCCAT",dta_rv, wt="WEIGHT_W57",include_unw = T,digits=2)
get_totals("F_INCOME_RECODE",dta_rv, wt="WEIGHT_W57",include_unw = T,digits=2)
get_totals("F_METRO",dta_rv, wt="WEIGHT_W57",include_unw = T,digits=2)
svymean(~F_METRO, rvdesign, na.rm=T)
tdesign<-svydesign(id=~1,weights=~WEIGHT_W57, data=dta_rv)
survey::svyquantile(~F_AGE,tdesign,quantile=c(.5), na.rm=T) %>% pluck(1)
dta_rv_pr <- filter(dta_rv, F_STATE=="CT" |
                     F_STATE=="DE" |
                     F_STATE=="FL" |
                     F_STATE=="KS" |
                     F_STATE=="KY" |
                     F_STATE=="ME" |
                     F_STATE=="MD" |
                     F_STATE=="DC" |
                     F_STATE=="NE" |
                     F_STATE=="NM" |
                     F_STATE=="NY" |
                     F_STATE=="PA" |
                     F_STATE=="WY")
rvdesig_pr<-svydesign(id=~1,weights=~WEIGHT_W57, data=dta_rv_pr)
svymean(~party_three, rvdesig_pr, na.rm=T)
get_totals("party_three",dta_rv_pr, wt="WEIGHT_W57",include_unw = T,digits=2)

dta_rv_vra <- filter(dta_rv,  F_STATE=="AL" |
                      F_STATE=="GA" |
                      F_STATE=="LA" |
                      F_STATE=="MS" |
                      F_STATE=="SC" |
                      F_STATE=="VA" |
                      F_STATE=="AK" |
                      F_STATE=="AZ" |
                      F_STATE=="TX")
rvdesig_vra<-svydesign(id=~1,weights=~WEIGHT_W57, data=dta_rv_vra)
svymean(~F_RACETHN, rvdesig_vra, na.rm=T)

get_totals("F_RACETHN",dta_rv_vra, wt="WEIGHT_W57",include_unw = T,digits=2)

dta_t <- filter(dta, TWITTER1_W57=="Yes" & F_REG=="You are ABSOLUTELY CERTAIN that you are registered to vote at your current address")
dta_t_des<-svydesign(id=~1,weights=~WEIGHT_W57, data=dta_t)
# calculate demographics - Twitter users regardless of handle provision
get_totals("F_AGECAT",dta_t, wt="WEIGHT_W57",include_unw = T,digits=2)
svymean(~F_AGECAT,dta_t_des, na.rm=T) 
get_totals("F_RACETHN",dta_t, wt="WEIGHT_W57",include_unw = T,digits=2)
svymean(~F_RACETHN,dta_t_des, na.rm=T) 
get_totals("F_SEX",dta_t, wt="WEIGHT_W57",include_unw = T,digits=2)
svymean(~F_SEX,dta_t_des, na.rm=T) 
get_totals("party_three",dta_t, wt="WEIGHT_W57",include_unw = T,digits=2)
svymean(~party_three,dta_t_des, na.rm=T) 
get_totals("F_EDUCCAT",dta_t, wt="WEIGHT_W57",include_unw = T,digits=2)
get_totals("F_INCOME_RECODE",dta_t, wt="WEIGHT_W57",include_unw = T,digits=2)
get_totals("F_METRO",dta_t, wt="WEIGHT_W57",include_unw = T,digits=2)
svymean(~F_METRO,dta_t_des, na.rm=T) 
survey::svyquantile(~F_AGE,dta_t_des,quantile=c(.5), na.rm=T) %>% pluck(1)
dta_r_pr <- filter(dta_t, F_STATE=="CT" |
                     F_STATE=="DE" |
                     F_STATE=="FL" |
                     F_STATE=="KS" |
                     F_STATE=="KY" |
                     F_STATE=="ME" |
                     F_STATE=="MD" |
                     F_STATE=="DC" |
                     F_STATE=="NE" |
                     F_STATE=="NM" |
                     F_STATE=="NY" |
                     F_STATE=="PA" |
                     F_STATE=="WY")
dta_r_pr_des<-svydesign(id=~1,weights=~WEIGHT_W57, data=dta_r_pr)
get_totals("party_three",dta_r_pr, wt="WEIGHT_W57",include_unw = T,digits=2)
svymean(~party_three,dta_r_pr_des, na.rm=T) 

dta_r_vra <- filter(dta_t,  F_STATE=="AL" |
                      F_STATE=="GA" |
                      F_STATE=="LA" |
                      F_STATE=="MS" |
                      F_STATE=="SC" |
                      F_STATE=="VA" |
                      F_STATE=="AK" |
                      F_STATE=="AZ" |
                      F_STATE=="TX")
dta_r_vra_des<-svydesign(id=~1,weights=~WEIGHT_W57, data=dta_r_vra)

get_totals("F_RACETHN",dta_r_vra, wt="WEIGHT_W57",include_unw = T,digits=2)
svymean(~F_RACETHN,dta_r_vra_des, na.rm=T) 

# generate account age from 11/1/19
stats$acct_age <- (as.Date("2019-11-01 12:00:00") - as.Date(stats$created_at))
stats$acct_age <- as.numeric(stats$acct_age)

# merge behavior and survey
dta <- left_join(stats, dta)
get_totals("is_private",dta, wt="WEIGHT_W57",include_unw = T,digits=0)

# remove private
dta <- filter(dta, is_private==FALSE)
dtadesign<-svydesign(id=~1,weights=~WEIGHT_W57, data=dta)

# calculate behavioral summary stats
survey::svyquantile(~statuses_count,dtadesign,quantile=c(.5), na.rm=T) %>% pluck(1)
summary(dta$statuses_count)
survey::svyquantile(~favorites_count,dtadesign,quantile=c(.5), na.rm=T) %>% pluck(1)
summary(dta$favorites_count)
survey::svyquantile(~followers_count,dtadesign,quantile=c(.5), na.rm=T) %>% pluck(1)
summary(dta$followers_count)
survey::svyquantile(~followings_count,dtadesign,quantile=c(.5), na.rm=T) %>% pluck(1)
summary(dta$favorites_count)
survey::svyquantile(~acct_age,dtadesign,quantile=c(.5), na.rm=T) %>% pluck(1)
summary(dta$acct_age)

# calculate demographics - HANDLE SAMPLE
get_totals("F_REG",dta, wt="WEIGHT_W57",include_unw = T,digits=2)

get_totals("F_AGECAT",dta, wt="WEIGHT_W57",include_unw = T,digits=2)
svymean(~F_AGECAT,dtadesign, na.rm=T) 
get_totals("F_RACETHN",dta, wt="WEIGHT_W57",include_unw = T,digits=2)
svymean(~F_RACETHN,dtadesign, na.rm=T) 
get_totals("F_SEX",dta, wt="WEIGHT_W57",include_unw = T,digits=2)
svymean(~F_SEX,dtadesign, na.rm=T) 
get_totals("F_PARTY_FINAL",dta, wt="WEIGHT_W57",include_unw = T,digits=2)
svymean(~party_three,dtadesign, na.rm=T) 
get_totals("F_EDUCCAT",dta, wt="WEIGHT_W57",include_unw = T,digits=2)
svymean(~F_EDUCCAT,dtadesign, na.rm=T) 
get_totals("F_INCOME_RECODE",dta, wt="WEIGHT_W57",include_unw = T,digits=2)
svymean(~F_INCOME_RECODE,dtadesign, na.rm=T) 
get_totals("F_METRO",dta, wt="WEIGHT_W57",include_unw = T,digits=2)
svymean(~F_METRO,dtadesign, na.rm=T) 
dtadesign2<-svydesign(id=~1,weights=~WEIGHT_W57, data=dta)
median_age_wt <- survey::svyquantile(~F_AGE,dtadesign2,quantile=c(.5), na.rm=T) %>% pluck(1)

# calculate demographics - REG VOTERS
dta_r <- filter(dta, F_REG=="You are ABSOLUTELY CERTAIN that you are registered to vote at your current address")
dta_r_des<-svydesign(id=~1,weights=~WEIGHT_W57, data=dta_r)
get_totals("F_AGECAT",dta_r, wt="WEIGHT_W57",include_unw = T,digits=2)
svymean(~F_AGECAT,dta_r_des, na.rm=T) 
get_totals("F_RACETHN",dta_r, wt="WEIGHT_W57",include_unw = T,digits=2)
svymean(~F_RACETHN,dta_r_des, na.rm=T) 
get_totals("party_three",dta_r, wt="WEIGHT_W57",include_unw = T,digits=2)
svymean(~party_three,dta_r_des, na.rm=T) 
get_totals("F_SEX",dta_r, wt="WEIGHT_W57",include_unw = T,digits=2)
svymean(~F_SEX,dta_r_des, na.rm=T) 
get_totals("F_METRO",dta_r, wt="WEIGHT_W57",include_unw = T,digits=2)
svymean(~F_METRO,dta_r_des, na.rm=T) 
median_age_wt <- survey::svyquantile(~F_AGE,dta_r_des,quantile=c(.5), na.rm=T) %>% pluck(1)

dta_r_pr <- filter(dta_r, F_STATE=="CT" |
                     F_STATE=="DE" |
                     F_STATE=="FL" |
                     F_STATE=="KS" |
                     F_STATE=="KY" |
                     F_STATE=="ME" |
                     F_STATE=="MD" |
                     F_STATE=="DC" |
                     F_STATE=="NE" |
                     F_STATE=="NM" |
                     F_STATE=="NY" |
                     F_STATE=="PA" |
                     F_STATE=="WY")
dta_r_pr_des<-svydesign(id=~1,weights=~WEIGHT_W57, data=dta_r_pr)
get_totals("party_three",dta_r_pr, wt="WEIGHT_W57",include_unw = T,digits=2)
svymean(~party_three,dta_r_pr_des, na.rm=T) 

dta_r_vra <- filter(dta_r,  F_STATE=="AL" |
                      F_STATE=="GA" |
                      F_STATE=="LA" |
                      F_STATE=="MS" |
                      F_STATE=="SC" |
                      F_STATE=="VA" |
                      F_STATE=="AK" |
                      F_STATE=="AZ" |
                      F_STATE=="TX")
dta_r_vra_des<-svydesign(id=~1,weights=~WEIGHT_W57, data=dta_r_vra)
get_totals("F_RACETHN",dta_r_vra, wt="WEIGHT_W57",include_unw = T,digits=2)
svymean(~F_RACETHN,dta_r_vra_des, na.rm=T) 

#calculate share of tweets from top ten the bad way
topten_cuttoff <- survey::svyquantile(~statuses_count,dtadesign,quantile=c(.9), na.rm=T) %>% pluck(1)
dta <- dta %>% mutate(
  topten = case_when(statuses_count>=topten_cuttoff ~ 1,
                     statuses_count<topten_cuttoff ~ 0,
                     TRUE ~ NA_real_)
)
dta$weightsumstatus <- dta$WEIGHT_W57*dta$statuses_count
dta$sumstatus <- dta$statuses_count
top <- filter(dta, topten==1)
rest <- filter(dta, topten==0)
toptentotaltweets <- sum(top$weightsumstatus)
resttotaltweets <- sum(rest$weightsumstatus)
perc_topten_atp <- round(toptentotaltweets/(toptentotaltweets+resttotaltweets),2)*100
perc_topten_atp

#and unweighted
toptentotaltweets_unw <- sum(top$sumstatus)
resttotaltweets_unw <- sum(rest$sumstatus)
perc_topten_unw_atp <- round(toptentotaltweets_unw/(toptentotaltweets_unw+resttotaltweets_unw),2)*100
perc_topten_unw_atp


#### ATP analysis done
# Now start KP analysis
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
         F_AGE_num = as.numeric(as.character(F_AGE)),
         party_three = case_when(
           F_PARTY_FINAL=="Republican" ~ "Rep",
           F_PARTY_FINAL=="Democrat" ~ "Dem",
           F_PARTY_FINAL=="Independent" ~ "IndOth",
           F_PARTY_FINAL=="Something else" ~ "IndOth",
           F_PARTY_FINAL=="Refused" ~ "IndOth",
         )
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

# set survey design
gfkdesign<-svydesign(id=~1,weights=~weight, data=gfk_recoded)

get_totals("REG",gfk_recoded, wt="weight",include_unw = T,digits=0)

# calculate demographics - FULL SAMPLE
get_totals("F_AGECAT",gfk_recoded, wt="weight",include_unw = T,digits=0)
svymean(~F_AGECAT, gfkdesign, na.rm=T)
get_totals("F_RACETHN",gfk_recoded, wt="weight",include_unw = T,digits=0)
svymean(~F_RACETHN, gfkdesign, na.rm=T)
get_totals("F_SEX",gfk_recoded, wt="weight",include_unw = T,digits=0)
svymean(~F_SEX, gfkdesign, na.rm=T)
get_totals("party_three",gfk_recoded, wt="weight",include_unw = T,digits=0)
svymean(~party_three, gfkdesign, na.rm=T)
get_totals("F_EDUCCAT",gfk_recoded, wt="weight",include_unw = T,digits=0)
svymean(~F_EDUCCAT, gfkdesign, na.rm=T)
get_totals("F_INCOME_RECODE",gfk_recoded, wt="weight",include_unw = T,digits=0)
svymean(~F_INCOME_RECODE, gfkdesign, na.rm=T)
get_totals("F_METRO",gfk_recoded, wt="weight",include_unw = T,digits=0)
svymean(~F_METRO, gfkdesign, na.rm=T)
gfkdesign2<-svydesign(id=~1,weights=~weight, data=gfk_recoded)
median_age_wt <- survey::svyquantile(~F_AGE,gfkdesign2,quantile=c(.5), na.rm=T) %>% pluck(1)

# calculate demographics - REG VOTERS
gfk_r <- filter(gfk_recoded, REG=="You are ABSOLUTELY CERTAIN that you are registered to vote at your current address")
gfk_r_design<-svydesign(id=~1,weights=~weight, data=gfk_r)

get_totals("F_AGECAT",gfk_r, wt="weight",include_unw = T,digits=2)
svymean(~F_AGECAT,gfk_r_design, na.rm=T) 
get_totals("F_RACETHN",gfk_r, wt="weight",include_unw = T,digits=2)
svymean(~F_RACETHN,gfk_r_design, na.rm=T) 
get_totals("F_SEX",gfk_r, wt="weight",include_unw = T,digits=0)
svymean(~F_SEX,gfk_r_design, na.rm=T) 
get_totals("party_three",gfk_r, wt="weight",include_unw = T,digits=0)
svymean(~party_three,gfk_r_design, na.rm=T) 
get_totals("F_METRO",gfk_r, wt="weight",include_unw = T,digits=0)
svymean(~F_METRO,gfk_r_design, na.rm=T) 

median_age_wt <- survey::svyquantile(~F_AGE,gfk_r_design,quantile=c(.5), na.rm=T) %>% pluck(1)

gfk_r_pr <- filter(gfk_r, F_STATE=="CT" |
                     F_STATE=="DE" |
                     F_STATE=="FL" |
                     F_STATE=="KS" |
                     F_STATE=="KY" |
                     F_STATE=="ME" |
                     F_STATE=="MD" |
                     F_STATE=="DC" |
                     F_STATE=="NE" |
                     F_STATE=="NM" |
                     F_STATE=="NY" |
                     F_STATE=="PA" |
                     F_STATE=="WY")
gfk_r_pr_des<-svydesign(id=~1,weights=~weight, data=gfk_r_pr)
get_totals("party_three",gfk_r_pr, wt="weight",include_unw = T,digits=2)
svymean(~party_three,gfk_r_pr_des, na.rm=T) 


gfk_r_vra <- filter(gfk_r,  F_STATE=="AL" |
                      F_STATE=="GA" |
                      F_STATE=="LA" |
                      F_STATE=="MS" |
                      F_STATE=="SC" |
                      F_STATE=="VA" |
                      F_STATE=="AK" |
                      F_STATE=="AZ" |
                      F_STATE=="TX")
gfk_r_vra_des<-svydesign(id=~1,weights=~weight, data=gfk_r_vra)
get_totals("F_RACETHN",gfk_r_vra, wt="weight",include_unw = T,digits=2)
svymean(~F_RACETHN,gfk_r_vra_des, na.rm=T) 

# Produce Demographics for Table 1 ----------------------------------------

median_age_wt <- survey::svyquantile(~F_AGE,gfkdesign,quantile=c(.5)) %>% pluck(1)
median_tweets_wt <- survey::svyquantile(~statuses_count,gfkdesign,quantile=c(.5)) %>% pluck(1)
median_followers_wt <- survey::svyquantile(~followers_count,gfkdesign,quantile=c(.5)) %>% pluck(1)
median_followings_wt <- survey::svyquantile(~followings_count,gfkdesign,quantile=c(.5)) %>% pluck(1)
median_likes_wt <- survey::svyquantile(~favorites_count,gfkdesign,quantile=c(.5)) %>% pluck(1)
median_accountage_wt <- survey::svyquantile(~acct_age,gfkdesign,quantile=c(.5)) %>% pluck(1)


# There's probably a better way to do this.
df_median_age <- gfk_recoded %>% 
  summarize(unweighted = median(F_AGE)) %>%
  mutate(demos = "Age") %>% 
  mutate(estimate = "Median Age",weight = median_age_wt) %>% 
  select(demos,estimate, weight, unweighted)
df_median_tweets <- gfk_recoded %>% 
  summarize(unweighted = median(statuses_count)) %>%
  mutate(demos = "Tweets") %>% 
  mutate(estimate = "Median Tweets",weight = median_tweets_wt) %>% 
  select(demos,estimate, weight, unweighted)
df_median_followers <- gfk_recoded %>% 
  summarize(unweighted = median(followers_count)) %>%
  mutate(demos = "Followers") %>% 
  mutate(estimate = "Median Followers",weight = median_followers_wt) %>% 
  select(demos,estimate, weight, unweighted)
df_median_followings <- gfk_recoded %>% 
  summarize(unweighted = median(followings_count)) %>%
  mutate(demos = "Followings") %>% 
  mutate(estimate = "Median Followings",weight = median_followings_wt) %>% 
  select(demos,estimate, weight, unweighted)
df_median_likes <- gfk_recoded %>% 
  summarize(unweighted = median(favorites_count)) %>%
  mutate(demos = "Likes") %>% 
  mutate(estimate = "Median Likes",weight = median_likes_wt) %>% 
  select(demos,estimate, weight, unweighted)
df_median_acct_age <- gfk_recoded %>% 
  summarize(unweighted = median(acct_age)) %>%
  mutate(demos = "Account Age") %>% 
  mutate(estimate = "Median Account Age",weight = median_accountage_wt) %>% 
  select(demos,estimate, weight, unweighted)


df_agebin <- pewmethods::get_totals(var = "F_AGECAT",df = gfk_recoded,wt = 'weight',include_unw = T,digits=0) %>% rename(estimate = F_AGECAT) %>% mutate(demos = "Age Bins")
df_racethn <- pewmethods::get_totals(var = "F_RACETHN",df = gfk_recoded,wt = 'weight',include_unw = T,digits=0) %>% rename(estimate = F_RACETHN) %>% mutate(demos = "Race/Ethnicity")
df_sex <- pewmethods::get_totals(var = "F_SEX",df = gfk_recoded,wt = 'weight',include_unw = T,digits=0) %>% rename(estimate = F_SEX) %>% mutate(demos = "Gender")
df_party <- pewmethods::get_totals(var = "F_PARTY_FINAL",df = gfk_recoded,wt = 'weight',include_unw = T,digits=0) %>% rename(estimate = F_PARTY_FINAL) %>% mutate(demos = "Party")
df_party_lean <- pewmethods::get_totals(var = "F_PARTYSUM_FINAL",df = gfk_recoded,wt = 'weight',include_unw = T,digits=0) %>% rename(estimate = F_PARTYSUM_FINAL) %>% mutate(demos = "PartySum")
df_metro <- pewmethods::get_totals(var = "F_METRO",df = gfk_recoded,wt = 'weight',include_unw = T,digits=0) %>% rename(estimate = F_METRO) %>% mutate(demos = "Urban")
df_educ <- pewmethods::get_totals(var = "F_EDUCCAT",df = gfk_recoded,wt = 'weight',include_unw = T,digits=0) %>% rename(estimate = F_EDUCCAT) %>% mutate(demos = "Educ")
df_inc <- pewmethods::get_totals(var = "F_INCOME_RECODE",df = gfk_recoded,wt = 'weight',include_unw = T,digits=0) %>% rename(estimate = F_INCOME_RECODE) %>% mutate(demos = "Income")

df_table1 <- rbind(df_median_age,df_agebin,df_racethn,df_sex,df_party,df_party_lean,df_metro,df_educ,df_inc,df_median_tweets,df_median_followers,df_median_followings,df_median_likes,df_median_acct_age)

df_table1 %>% write_csv("twitter-demos/table1.csv")

topten_cuttoff <- survey::svyquantile(~statuses_count,gfkdesign,quantile=c(.9), na.rm=T) %>% pluck(1)
gfk_recoded <- gfk_recoded %>% mutate(
  topten = case_when(statuses_count>=topten_cuttoff ~ 1,
                     statuses_count<topten_cuttoff ~ 0,
                     TRUE ~ NA_real_)
)
gfk_recoded$weightsumstatus <- gfk_recoded$weight*gfk_recoded$statuses_count
gfk_recoded$sumstatus <- gfk_recoded$statuses_count

top <- filter(gfk_recoded, topten==1)
rest <- filter(gfk_recoded, topten==0)
toptentotaltweets <- sum(top$weightsumstatus)
resttotaltweets <- sum(rest$weightsumstatus)
perc_topten_kp <- round(toptentotaltweets/(toptentotaltweets+resttotaltweets),2)*100
perc_topten_kp

toptentotaltweets_unw <- sum(top$sumstatus)
resttotaltweets_unw <- sum(rest$sumstatus)
perc_topten_unw_kp <- round(toptentotaltweets_unw/(toptentotaltweets_unw+resttotaltweets_unw),2)*100
perc_topten_unw_kp

#Comparative histograms

makeTransparent<-function(someColor, alpha=100)
{
  newColor<-col2rgb(someColor)
  apply(newColor, 2, function(curcoldata){rgb(red=curcoldata[1], green=curcoldata[2],
                                              blue=curcoldata[3],alpha=alpha, maxColorValue=255)})
}
atp_tweets <- svyhist(~log10(statuses_count), design=dtadesign,col=makeTransparent("blue"),lty=2,main="",xlab="log10(Total Tweets)")
kp_tweets <- svyhist(~log10(statuses_count), design=gfkdesign,add = TRUE, col = makeTransparent("red"))
atp_tweets
kp_tweets
legend(4, 0.5, title="",
       c("ATP","Knowledge\nPanel"), fill=c(makeTransparent("blue"),makeTransparent("red")), horiz=F, cex=0.7,bty = "n",y.intersp=.5)
dev.copy(jpeg,filename="tweets.jpg")
dev.off ()
atp_followers <- svyhist(~log10(followers_count), design=dtadesign,col=makeTransparent("blue"),lty=2,main="",xlab="log10(Followers)")
kp_followers <- svyhist(~log10(followers_count), design=gfkdesign,add = TRUE, col = makeTransparent("red"))
atp_followers
kp_followers 
legend(4, 0.5, title="",
       c("ATP","Knowledge\nPanel"), fill=c(makeTransparent("blue"),makeTransparent("red")), horiz=F, cex=0.7,bty = "n",y.intersp=.5)
dev.copy(jpeg,filename="followers.jpg")
dev.off ()
atp_followings <- svyhist(~log10(followings_count), design=dtadesign,col=makeTransparent("blue"),lty=2,main="",xlab="log10(Following)")
kp_followings <- svyhist(~log10(followings_count), design=gfkdesign,add = TRUE, col = makeTransparent("red"))
atp_followings
kp_followings
legend(3.4, 0.5, title="",
       c("ATP","Knowledge\nPanel"), fill=c(makeTransparent("blue"),makeTransparent("red")), cex=0.7,bty = "n",y.intersp=.5)
dev.copy(jpeg,filename="following.jpg")
dev.off ()
atp_likes<-svyhist(~log10(favorites_count), design=dtadesign,col=makeTransparent("blue"),lty=2,main="",xlab="log10(Likes)")
kp_likes<-svyhist(~log10(favorites_count), design=gfkdesign,add = TRUE, col = makeTransparent("red"))
atp_likes
kp_likes
legend(4.2, 0.42, title="",
       c("ATP","Knowledge\nPanel"), fill=c(makeTransparent("blue"),makeTransparent("red")), cex=0.7,bty = "n",y.intersp=.5)
dev.copy(jpeg,filename="likes.jpg")
dev.off ()

atp_tweets_df <- do.call("rbind", lapply(atp_tweets, as.data.frame)) 
kp_tweets_df <- do.call("rbind", lapply(kp_tweets, as.data.frame)) 

atp_followers_df <- do.call("rbind", lapply(atp_followers, as.data.frame)) 
kp_followers_df <- do.call("rbind", lapply(kp_followers, as.data.frame)) 

atp_followings_df <- do.call("rbind", lapply(atp_followings, as.data.frame)) 
kp_followings_df <- do.call("rbind", lapply(kp_followings, as.data.frame)) 

atp_likes_df <- do.call("rbind", lapply(atp_likes, as.data.frame)) 
kp_likes_df <- do.call("rbind", lapply(kp_likes, as.data.frame)) 

save(atp_tweets_df,kp_tweets_df,atp_followers_df,kp_followers_df,atp_followings_df,kp_followings_df,atp_likes_df, kp_likes_df, file = "histograms.RData")


#load phone poll and get demographics
dta <- read_sav("datasets/2019_internet.sav") %>% as_factor()
attributes(dta$web1a)
dta <- filter(dta, web1a=="Yes, do this")
calculate_deff(dta$weight)

dta$age[which(dta$age=="97 or older")] <- NA
dta$age[which(dta$age=="(VOL) Don't know")] <- NA
dta$age[which(dta$age=="(VOL) Refused")] <- NA
dta <- dta %>% mutate(
  F_EDUCCAT = fct_collapse(educ2, 
                           "College graduate+" = c(
                             "Four year college or university degree/Bachelor's degree (e.g., BS, BA, AB)", 
                             "Some postgraduate or professional schooling, no postgraduate degree (e.g. some graduate school)",
                             "Postgraduate or professional degree, including master's, doctorate, medical or law degree (e.g., MA, MS, PhD, MD, JD, gr"
                           ), 
                           "Some College" = c(
                             "Some college, no degree (includes some community college)",
                             "Two year associate degree from a college or university"
                           ),
                           "H.S. graduate or less" = c(
                             "Less than high school (Grades 1-8 or no formal schooling)", 
                             "High school incomplete (Grades 9-11 or Grade 12 with NO diploma)",
                             "High school graduate (Grade 12 with diploma or GED certificate)"
                           )
                           
  ),
  F_INCOME = case_when(inc=="Less than $10,000" ~ "<$30K",
                       inc=="10 to under $20,000" ~ "<$30K",
                       inc=="20 to under $30,000" ~ "<$30K",
                       inc=="30 to under $40,000" ~ "$30K-75K",
                       inc=="40 to under $50,000" ~ "$30K-75K",
                       inc=="50 to under $75,000" ~ "$30K-75K",
                       inc=="75 to under $100,000" ~ "$75K+",
                       inc=="100 to under $150,000, OR" ~ "$75K+",
                       inc=="$150,000 or more?" ~ "$75K+",
                       TRUE ~ NA_character_
  ),
  F_AGE = as.numeric(as.character(age)),
  F_AGECAT = cut(F_AGE, c(-Inf,29,49,64,Inf),
                 labels = c("18-29", "30-49", "50-64", "65+")
  ),
  party_three = case_when(
    party=="Republican" ~ "Rep",
    party=="Democrat" ~ "Dem",
    party=="Independent" ~ "IndOth",
    party=="(VOL) No preference" ~ "IndOth",
    party=="(VOL) Don't know" ~ "IndOth",
    party=="(VOL) Don't know" ~ "IndOth",
    party=="(VOL) Refused" ~ "IndOth",
  )    
)
dtadesign<-svydesign(id=~1,weights=~weight, data=dta)
summary(dta_num$F_AGE)
survey::svyquantile(~F_AGE,dtadesign,quantile=c(.5), na.rm=T) %>% pluck(1)

get_totals("F_AGECAT",dta,wt="weight",include_unw = T,digits=2)
svymean(~F_AGECAT,dtadesign,na.rm=T)
get_totals("racethn",dta,wt="weight",include_unw = T,digits=2)
svymean(~racethn,dtadesign,na.rm=T)
get_totals("sex",dta,wt="weight",include_unw = T,digits=2)
svymean(~sex,dtadesign,na.rm=T)
get_totals("party",dta,wt="weight",include_unw = T,digits=2)
get_totals("party_three",dta,wt="weight",include_unw = T,digits=2)
svymean(~party_three,dtadesign,na.rm=T)
get_totals("F_EDUCCAT",dta,wt="weight",include_unw = T,digits=2)
svymean(~F_EDUCCAT,dtadesign,na.rm=T)
get_totals("inc",dta,wt="weight",include_unw = T,digits=2)
get_totals("F_INCOME",dta,wt="weight",include_unw = T,digits=2)
svymean(~F_INCOME,dtadesign,na.rm=T)
metro <- read.csv("datasets/ruralurbancodes2013.csv")
metro$fips <- as.numeric(metro$FIPS)
dta <- dta %>% left_join(metro, by="fips")
get_totals("Description",dta,wt="weight",include_unw = T,digits=2)
dtadesign<-svydesign(id=~1,weights=~weight, data=dta)
svymean(~Description,dtadesign,na.rm=T)