library(tidyverse)
library(survey)
library(haven)
library(pewmethods)

#load phone poll - can be obtained from: https://www.pewresearch.org/internet/dataset/core-trends-survey/ 
dta <- read_sav("datasets/2019_internet.sav") %>% as_factor()
attributes(dta$web1a)
dta <- filter(dta, web1a=="Yes, do this")
calculate_deff(dta$weight)

#There are no people 97 or older so NA all of these
dta$age[which(dta$age=="97 or older")] <- NA
dta$age[which(dta$age=="(VOL) Don't know")] <- NA
dta$age[which(dta$age=="(VOL) Refused")] <- NA

#recode variables
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
  )
)
#set design and get median age
dtadesign<-svydesign(id=~1,weights=~weight, data=dta)
summary(dta_num$F_AGE)
survey::svyquantile(~F_AGE,dtadesign,quantile=c(.5), na.rm=T) %>% pluck(1)

#report demographics
get_totals("F_AGECAT",dta,wt="weight",include_unw = T,digits=2)
get_totals("racethn",dta,wt="weight",include_unw = T,digits=2)
get_totals("sex",dta,wt="weight",include_unw = T,digits=2)

get_totals("party",dta,wt="weight",include_unw = T,digits=2)
get_totals("partysum",dta,wt="weight",include_unw = T,digits=2)

get_totals("F_EDUCCAT",dta,wt="weight",include_unw = T,digits=2)
get_totals("inc",dta,wt="weight",include_unw = T,digits=2)
get_totals("F_INCOME",dta,wt="weight",include_unw = T,digits=2)

#load metro classifications
metro <- read.csv("datasets/ruralurbancodes2013.csv")
metro$fips <- as.numeric(metro$FIPS)
dta <- dta %>% left_join(metro, by="fips")
get_totals("Description",dta,wt="weight",include_unw = T,digits=2)
