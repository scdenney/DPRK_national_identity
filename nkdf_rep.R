#### Replication code for "North Korean Patriotism: Assessing the Successes and Failures of a Nation" ####
#### Korea Journal, vol. 61, no. 1 (spring 2021): 154–185. ####


library(tidyverse)
library(sjPlot)
library(ggeffects)
library(stargazer)

##---------------------------##
#### load and prepare data ####
##---------------------------##

#load the rda file
#load(file="nkdf.rda")

## subset by those who spent >= 15 years in the DPRK (ie, fully socialized there) ##
nkdf <- subset(main_nkdf, years.nk >= 15)

## may be necessary to redefine some variables as numeric
#nkdf[9:22] <- lapply(nkdf[9:22], function(x) as.numeric(as.integer(x)))

## variable recoding, renaming
nkdf$female <- ifelse(nkdf$gender == "Female", c(1), c(nkdf$gender))

nkdf$eduhigh <- ifelse(nkdf$edu.nk == "University+" | nkdf$edu.nk == "Technical Training",1,0)

nkdf$class.high <- ifelse(nkdf$class.nk == "High" | nkdf$class.nk == "Middle-high",1,0)
nkdf$class.low <- ifelse(nkdf$class.nk == "Low" | nkdf$class.nk == "Middle-low",1,0)

nkdf$borderlands <- ifelse(nkdf$origins == "Ryanggang" | nkdf$origins == "North Hamgyong" | 
                             nkdf$origins == "Jagang" | nkdf$origins == "North Pyongan", 1,0)

nkdf$capital <- ifelse(nkdf$origins == "Pyongyang", 1,.0)

nkdf$kisgen <- ifelse(nkdf$birth.year <= 1979,1,0)
nkdf$kisgen <- factor(nkdf$kisgen, levels=c(0,1), labels=c("Not KIS gen.", "KIS gen."))

nkdf$year.survey <- as.factor(nkdf$year.survey)

##--------------##
#### imputing ####
##--------------##
#tempData <- mice(nkdf,m=5,maxit=50,meth="pmm")
#summary(tempData)
#nkdf <- complete(tempData,1)


##---recoding proud/not proud
nkdf$Socialism <- ifelse(nkdf$Socialism > 2, 1, 0)
nkdf$Politics <- ifelse(nkdf$Politics > 2, 1, 0)
nkdf$Economics <- ifelse(nkdf$Economics > 2, 1, 0)
nkdf$Science <- ifelse(nkdf$Science > 2, 1, 0)
nkdf$Sports <- ifelse(nkdf$Sports > 2, 1, 0)
nkdf$Art <- ifelse(nkdf$Art > 2, 1, 0)
nkdf$`Baekdu Bloodline` <- ifelse(nkdf$`Baekdu Bloodline` > 2, 1, 0)
nkdf$Military <- ifelse(nkdf$Military > 2, 1, 0)
nkdf$`Revolutionary History` <- ifelse(nkdf$`Revolutionary History` > 2, 1, 0)
nkdf$`Social Equality` <- ifelse(nkdf$`Social Equality` > 2, 1, 0)
nkdf$Juche <- ifelse(nkdf$Juche > 2, 1, 0)
nkdf$`Social Security` <- ifelse(nkdf$`Social Security` > 2, 1, 0)


##--------------------------------##
#### baseline regression models ####
##--------------------------------##
m.socialism <- glm(Socialism ~ kisgen + female + eduhigh + class.high + class.low + borderlands + capital + year.survey, family="gaussian", data=nkdf)
m.politics <- glm(Politics ~ kisgen + female + eduhigh + class.high + class.low + borderlands + capital + year.survey, family="gaussian", data=nkdf)
m.economics <- glm(Economics ~ kisgen + female + eduhigh + class.high + class.low + borderlands + capital + year.survey, family="gaussian", data=nkdf)
m.science <- glm(Science ~ kisgen + female + eduhigh + class.high + class.low + borderlands + capital + year.survey, family="gaussian", data=nkdf)
m.sports <- glm(Sports ~ kisgen + female + eduhigh + class.high + class.low + borderlands + capital + year.survey, family="gaussian", data=nkdf)
m.art <- glm(Art ~ kisgen + female + eduhigh + class.high + class.low + borderlands + capital + year.survey, family="gaussian", data=nkdf)
m.baekdu <- glm(`Baekdu Bloodline` ~ kisgen + female + eduhigh + class.high + class.low + borderlands + capital + year.survey, family="gaussian", data=nkdf)
m.military <- glm(Military ~ kisgen + female + eduhigh + class.high + class.low + borderlands + capital + year.survey, family="gaussian", data=nkdf)
m.history <- glm(`Revolutionary History` ~ kisgen + female + eduhigh + class.high + class.low + borderlands + capital + year.survey, family="gaussian", data=nkdf)
m.social.e <- glm(`Social Equality` ~ kisgen + female + eduhigh + class.high + class.low + borderlands + capital + year.survey, family="gaussian", data=nkdf)
m.juche <- glm(Juche ~ kisgen + female + eduhigh + class.high + class.low + borderlands + capital + year.survey, family="gaussian", data=nkdf)
m.social.s <- glm(`Social Security` ~ kisgen + female + eduhigh + class.high + class.low + borderlands + capital + year.survey, family="gaussian", data=nkdf)


## robust SEs
m.socialism.r <- coeftest(m.socialism, vcov = vcovHC(m.socialism, type="HC3"))
m.politics.r <- coeftest(m.politics, vcov = vcovHC(m.politics, type="HC3"))
m.economics.r <- coeftest(m.economics, vcov = vcovHC(m.economics, type="HC3"))
m.science.r <- coeftest(m.science, vcov = vcovHC(m.science, type="HC3"))
m.sports.r <- coeftest(m.sports, vcov = vcovHC(m.sports, type="HC3"))
m.art.r <- coeftest(m.art, vcov = vcovHC(m.art, type="HC3"))
m.baekdu.r <- coeftest(m.baekdu, vcov = vcovHC(m.baekdu, type="HC3"))
m.military.r <- coeftest(m.military, vcov = vcovHC(m.military, type="HC3"))
m.history.r <- coeftest(m.history, vcov = vcovHC(m.history, type="HC3"))
m.social.e.r <- coeftest(m.social.e, vcov = vcovHC(m.social.e, type="HC3"))
m.juche.r <- coeftest(m.juche, vcov = vcovHC(m.juche, type="HC3"))
m.social.s.r <- coeftest(m.social.s, vcov = vcovHC(m.social.s, type="HC3"))


##-------------------------------------------##
#### subgroup analysis by time of exposure ####
##-------------------------------------------##

## exposure levels 
nkdf$exposure3[nkdf$years.sk<=2]=1
nkdf$exposure3[nkdf$years.sk>2 & nkdf$years.sk<=5]=2
nkdf$exposure3[nkdf$years.sk>5]=3
nkdf$exposure3=factor(nkdf$exposure3, levels=c(1,2,3), labels=c('Newly Arrived',
                                                                    'Newly Resettled',
                                                                    "Fully Resettled"))

## newly arrived
m2.socialism <- subset(nkdf, exposure3 == "Newly Arrived") %>% glm(Socialism ~ kisgen + eduhigh + class.high + class.low + borderlands + year.survey, family="gaussian", data=.)
m2.politics <- subset(nkdf, exposure3 == "Newly Arrived") %>% glm(Politics ~ kisgen + eduhigh + class.high + class.low  + borderlands + year.survey, family="gaussian", data=.)
m2.economics <- subset(nkdf, exposure3 == "Newly Arrived") %>% glm(Economics ~ kisgen + eduhigh + class.high + class.low  + borderlands + year.survey, family="gaussian", data=.)
m2.science <- subset(nkdf, exposure3 == "Newly Arrived") %>% glm(Science ~ kisgen + eduhigh + class.high + class.low  + borderlands + year.survey, family="gaussian", data=.)
m2.sports <- subset(nkdf, exposure3 == "Newly Arrived") %>% glm(Sports ~ kisgen + eduhigh + class.high + class.low  + borderlands + year.survey, family="gaussian", data=.)
m2.art <- subset(nkdf, exposure3 == "Newly Arrived") %>% glm(Art ~ kisgen + eduhigh + class.high + class.low  + borderlands + year.survey, family="gaussian", data=.)
m2.baekdu <- subset(nkdf, exposure3 == "Newly Arrived") %>% glm(`Baekdu Bloodline` ~ kisgen + eduhigh + class.high + class.low  + borderlands + year.survey, family="gaussian", data=.)
m2.military <- subset(nkdf, exposure3 == "Newly Arrived") %>% glm(Military ~ kisgen + eduhigh + class.high + class.low  + borderlands + year.survey, family="gaussian", data=.)
m2.history <- subset(nkdf, exposure3 == "Newly Arrived") %>% glm(`Revolutionary History` ~ kisgen + eduhigh + class.high + class.low  + borderlands + year.survey, family="gaussian", data=.)
m2.social.e <- subset(nkdf, exposure3 == "Newly Arrived") %>% glm(`Social Equality` ~ kisgen + eduhigh + class.high + class.low  + borderlands + year.survey, family="gaussian", data=.)
m2.juche <- subset(nkdf, exposure3 == "Newly Arrived") %>% glm(Juche ~ kisgen + eduhigh + class.high + class.low  + borderlands + year.survey, family="gaussian", data=.)
m2.social.s <- subset(nkdf, exposure3 == "Newly Arrived") %>% glm(`Social Security` ~ kisgen + eduhigh + class.high + class.low  + borderlands + year.survey, family="gaussian", data=.)

# robust SEs
m2.socialism2.r <- coeftest(m2.socialism, vcov = vcovHC(m2.socialism, type="HC3"))
m2.politics.r <- coeftest(m2.politics, vcov = vcovHC(m2.politics, type="HC3"))
m2.economics.r <- coeftest(m2.economics, vcov = vcovHC(m2.economics, type="HC3"))
m2.science.r <- coeftest(m2.science, vcov = vcovHC(m2.science, type="HC3"))
m2.sports.r <- coeftest(m2.sports, vcov = vcovHC(m2.sports, type="HC3"))
m2.art.r <- coeftest(m2.art, vcov = vcovHC(m2.art, type="HC3"))
m2.baekdu.r <- coeftest(m2.baekdu, vcov = vcovHC(m2.baekdu, type="HC3"))
m2.military.r <- coeftest(m2.military, vcov = vcovHC(m2.military, type="HC3"))
m2.history.r <- coeftest(m2.history, vcov = vcovHC(m2.history, type="HC3"))
m2.social.e.r <- coeftest(m2.social.e, vcov = vcovHC(m2.social.e, type="HC3"))
m2.juche.r <- coeftest(m2.juche, vcov = vcovHC(m2.juche, type="HC3"))
m2.social.s.r <- coeftest(m2.social.s, vcov = vcovHC(m2.social.s, type="HC3"))


## newly resettled
m3.socialism <- subset(nkdf, exposure3 == "Newly Resettled") %>% glm(Socialism ~ kisgen + female + eduhigh + class.high + class.low + borderlands + capital + year.survey, family="gaussian", data=.)
m3.politics <- subset(nkdf, exposure3 == "Newly Resettled") %>% glm(Politics ~ kisgen + female + eduhigh + class.high + class.low + borderlands + capital + year.survey, family="gaussian", data=.)
m3.economics <- subset(nkdf, exposure3 == "Newly Resettled") %>% glm(Economics ~ kisgen + female + eduhigh + class.high + class.low + borderlands + capital + year.survey, family="gaussian", data=.)
m3.science <- subset(nkdf, exposure3 == "Newly Resettled") %>% glm(Science ~ kisgen + female + eduhigh + class.high + class.low + borderlands + capital + year.survey, family="gaussian", data=.)
m3.sports <- subset(nkdf, exposure3 == "Newly Resettled") %>% glm(Sports ~ kisgen + female + eduhigh + class.high + class.low + borderlands + capital + year.survey, family="gaussian", data=.)
m3.art <- subset(nkdf, exposure3 == "Newly Resettled") %>% glm(Art ~ kisgen + female + eduhigh + class.high + class.low + borderlands + capital + year.survey, family="gaussian", data=.)
m3.baekdu <- subset(nkdf, exposure3 == "Newly Resettled") %>% glm(`Baekdu Bloodline` ~ kisgen + female + eduhigh + class.high + class.low + borderlands + capital + year.survey, family="gaussian", data=.)
m3.military <- subset(nkdf, exposure3 == "Newly Resettled") %>% glm(Military ~ kisgen + female + eduhigh + class.high + class.low + borderlands + capital + year.survey, family="gaussian", data=.)
m3.history <- subset(nkdf, exposure3 == "Newly Resettled") %>% glm(`Revolutionary History` ~ kisgen + female + eduhigh + class.high + class.low + borderlands + capital + year.survey, family="gaussian", data=.)
m3.social.e <- subset(nkdf, exposure3 == "Newly Resettled") %>% glm(`Social Equality` ~ kisgen + female + eduhigh + class.high + class.low + borderlands + capital + year.survey, family="gaussian", data=.)
m3.juche <- subset(nkdf, exposure3 == "Newly Resettled") %>% glm(Juche ~ kisgen + female + eduhigh + class.high + class.low + borderlands + capital + year.survey, family="gaussian", data=.)
m3.social.s <- subset(nkdf, exposure3 == "Newly Resettled") %>% glm(`Social Security` ~ kisgen + female + eduhigh + class.high + class.low + borderlands + capital + year.survey, family="gaussian", data=.)

# robust SEs
m3.socialism3.r <- coeftest(m3.socialism, vcov = vcovHC(m3.socialism, type="HC3"))
m3.politics.r <- coeftest(m3.politics, vcov = vcovHC(m3.politics, type="HC3"))
m3.economics.r <- coeftest(m3.economics, vcov = vcovHC(m3.economics, type="HC3"))
m3.science.r <- coeftest(m3.science, vcov = vcovHC(m3.science, type="HC3"))
m3.sports.r <- coeftest(m3.sports, vcov = vcovHC(m3.sports, type="HC3"))
m3.art.r <- coeftest(m3.art, vcov = vcovHC(m3.art, type="HC3"))
m3.baekdu.r <- coeftest(m3.baekdu, vcov = vcovHC(m3.baekdu, type="HC3"))
m3.military.r <- coeftest(m3.military, vcov = vcovHC(m3.military, type="HC3"))
m3.history.r <- coeftest(m3.history, vcov = vcovHC(m3.history, type="HC3"))
m3.social.e.r <- coeftest(m3.social.e, vcov = vcovHC(m3.social.e, type="HC3"))
m3.juche.r <- coeftest(m3.juche, vcov = vcovHC(m3.juche, type="HC3"))
m3.social.s.r <- coeftest(m3.social.s, vcov = vcovHC(m3.social.s, type="HC3"))

## newly resettled
m4.socialism <- subset(nkdf, exposure3 == "Fully Resettled") %>% glm(Socialism ~ kisgen + female + eduhigh + class.high + class.low + borderlands + capital + year.survey, family="gaussian", data=.)
m4.politics <- subset(nkdf, exposure3 == "Fully Resettled") %>% glm(Politics ~ kisgen + female + eduhigh + class.high + class.low + borderlands + capital + year.survey, family="gaussian", data=.)
m4.economics <- subset(nkdf, exposure3 == "Fully Resettled") %>% glm(Economics ~ kisgen + female + eduhigh + class.high + class.low + borderlands + capital + year.survey, family="gaussian", data=.)
m4.science <- subset(nkdf, exposure3 == "Fully Resettled") %>% glm(Science ~ kisgen + female + eduhigh + class.high + class.low + borderlands + capital + year.survey, family="gaussian", data=.)
m4.sports <- subset(nkdf, exposure3 == "Fully Resettled") %>% glm(Sports ~ kisgen + female + eduhigh + class.high + class.low + borderlands + capital + year.survey, family="gaussian", data=.)
m4.art <- subset(nkdf, exposure3 == "Fully Resettled") %>% glm(Art ~ kisgen + female + eduhigh + class.high + class.low + borderlands + capital + year.survey, family="gaussian", data=.)
m4.baekdu <- subset(nkdf, exposure3 == "Fully Resettled") %>% glm(`Baekdu Bloodline` ~ kisgen + female + eduhigh + class.high + class.low + borderlands + capital + year.survey, family="gaussian", data=.)
m4.military <- subset(nkdf, exposure3 == "Fully Resettled") %>% glm(Military ~ kisgen + female + eduhigh + class.high + class.low + borderlands + capital + year.survey, family="gaussian", data=.)
m4.history <- subset(nkdf, exposure3 == "Fully Resettled") %>% glm(`Revolutionary History` ~ kisgen + female + eduhigh + class.high + class.low + borderlands + capital + year.survey, family="gaussian", data=.)
m4.social.e <- subset(nkdf, exposure3 == "Fully Resettled") %>% glm(`Social Equality` ~ kisgen + female + eduhigh + class.high + class.low + borderlands + capital + year.survey, family="gaussian", data=.)
m4.juche <- subset(nkdf, exposure3 == "Fully Resettled") %>% glm(Juche ~ kisgen + female + eduhigh + class.high + class.low + borderlands + capital + year.survey, family="gaussian", data=.)
m4.social.s <- subset(nkdf, exposure3 == "Fully Resettled") %>% glm(`Social Security` ~ kisgen + female + eduhigh + class.high + class.low + borderlands + capital + year.survey, family="gaussian", data=.)

# robust SEs
m4.socialism4.r <- coeftest(m4.socialism, vcov = vcovHC(m4.socialism, type="HC3"))
m4.politics.r <- coeftest(m4.politics, vcov = vcovHC(m4.politics, type="HC3"))
m4.economics.r <- coeftest(m4.economics, vcov = vcovHC(m4.economics, type="HC3"))
m4.science.r <- coeftest(m4.science, vcov = vcovHC(m4.science, type="HC3"))
m4.sports.r <- coeftest(m4.sports, vcov = vcovHC(m4.sports, type="HC3"))
m4.art.r <- coeftest(m4.art, vcov = vcovHC(m4.art, type="HC3"))
m4.baekdu.r <- coeftest(m4.baekdu, vcov = vcovHC(m4.baekdu, type="HC3"))
m4.military.r <- coeftest(m4.military, vcov = vcovHC(m4.military, type="HC3"))
m4.history.r <- coeftest(m4.history, vcov = vcovHC(m4.history, type="HC3"))
m4.social.e.r <- coeftest(m4.social.e, vcov = vcovHC(m4.social.e, type="HC3"))
m4.juche.r <- coeftest(m4.juche, vcov = vcovHC(m4.juche, type="HC3"))
m4.social.s.r <- coeftest(m4.social.s, vcov = vcovHC(m4.social.s, type="HC3"))


