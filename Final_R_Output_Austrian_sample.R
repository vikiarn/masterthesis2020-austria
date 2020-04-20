
########AUSTRIAN SAMPLE############

# Packages
#install.packages("haven")
#install.packages("stargazer")
#install.packages("descr")
#install.packages("ggplot2")
#install.packages("questionr")
#install.packages("tidyverse")
#install.packages("margins")
#install.packages("sjPlot")
#install.packages("effects")
#install.packages("nnet")
#install.packages("reshape2")
#install.packages("RColorBrewer")

#Libraries
library(haven)
library(descr)
library(stargazer)
library(ggplot2)
library(questionr)
library(tidyverse)
library(margins) 
library(sjPlot)
library(haven)
library(nnet)
library(effects)
library(reshape2)
library(RColorBrewer)

#Set Working Directory
setwd("~/OneDrive/Master Thesis/Data/Austria/Daten_AES_2011") 

###########################################################################
########AUSTRIA 2011: Read in data & Select sample for AUT AES 2011########
AES2011_AUT <- read.csv2(file = "AES 2011_12 Mikrodaten.csv") 
SUBSET01a_AUT_2011<-subset(AES2011_AUT)

#FILTER: limit samples to potential AE participants and are out of their primary education
attributes(SUBSET01a_AUT_2011$MAINSTAT)
table(SUBSET01a_AUT_2011$MAINSTAT)
SUBSET01_AUT_2011 <- subset(SUBSET01a_AUT_2011, SUBSET01a_AUT_2011$MAINSTAT==11 | SUBSET01a_AUT_2011$MAINSTAT==12 | SUBSET01a_AUT_2011$MAINSTAT==20 | SUBSET01a_AUT_2011$MAINSTAT==32 | SUBSET01a_AUT_2011$MAINSTAT==33 | SUBSET01a_AUT_2011$MAINSTAT==35 | SUBSET01a_AUT_2011$MAINSTAT==36 | SUBSET01a_AUT_2011$MAINSTAT==-1)
table(SUBSET01_AUT_2011$MAINSTAT)

#DV: Participation in non-formal (NFE) or formal adult education (FED)
#Participation NFE
SUBSET01_AUT_2011$NFE <- -1
SUBSET01_AUT_2011$NFE[SUBSET01_AUT_2011$NFENUM>0 & SUBSET01_AUT_2011$NFENUM<33] <- "1" 
SUBSET01_AUT_2011$NFE[SUBSET01_AUT_2011$NFENUM==0] <- "0"
SUBSET01_AUT_2011$NFE <-factor(SUBSET01_AUT_2011$NFE)
table(SUBSET01_AUT_2011$NFE)
freq(SUBSET01_AUT_2011$NFE)

#Combination of FED und NFE
SUBSET01_AUT_2011$NFE_FED <- -1
SUBSET01_AUT_2011$NFE_FED[SUBSET01_AUT_2011$FED==1 | SUBSET01_AUT_2011$NFE==1] <- "1"
SUBSET01_AUT_2011$NFE_FED[SUBSET01_AUT_2011$FED==2 & SUBSET01_AUT_2011$NFE==0] <- "0"
SUBSET01_AUT_2011$NFE_FED <-factor(SUBSET01_AUT_2011$NFE_FED)
table(SUBSET01_AUT_2011$NFE_FED)

#Creation of Independent Variables (IV)
#new variable: GENDER
attributes(SUBSET01_AUT_2011$SEX)
table(SUBSET01_AUT_2011$SEX)
SUBSET01_AUT_2011$GENDER[SUBSET01_AUT_2011$SEX==1] <-"Male"
SUBSET01_AUT_2011$GENDER[SUBSET01_AUT_2011$SEX==2] <-"Female"
SUBSET01_AUT_2011$GENDER <-factor(SUBSET01_AUT_2011$GENDER)
table(SUBSET01_AUT_2011$GENDER)
freq(SUBSET01_AUT_2011$GENDER)

#new variable: AGE (ALTER)
attributes(SUBSET01_AUT_2011$AGE)
table(SUBSET01_AUT_2011$AGE)
freq(SUBSET01_AUT_2011$AGE)
SUBSET01_AUT_2011$ALTER <- "NA"
SUBSET01_AUT_2011$ALTER[SUBSET01_AUT_2011$AGE>17 & SUBSET01_AUT_2011$AGE<35] <-"1_young"
SUBSET01_AUT_2011$ALTER[SUBSET01_AUT_2011$AGE>35 & SUBSET01_AUT_2011$AGE<53] <-"2_medium"
SUBSET01_AUT_2011$ALTER[SUBSET01_AUT_2011$AGE>54 & SUBSET01_AUT_2011$AGE<65] <-"3_old"
SUBSET01_AUT_2011$ALTER <-factor(SUBSET01_AUT_2011$ALTER)
table(SUBSET01_AUT_2011$ALTER)
freq(SUBSET01_AUT_2011$ALTER)

#delete missing values
SUBSET02_AUT_2011 <-subset(SUBSET01_AUT_2011, ALTER!="NA")
SUBSET02_AUT_2011$ALTER <- droplevels(SUBSET02_AUT_2011$ALTER)
SUBSET02_AUT_2011$ALTER <-factor(SUBSET02_AUT_2011$ALTER)
table(SUBSET02_AUT_2011$ALTER)
freq(SUBSET02_AUT_2011$ALTER)

#new variable: migration background (MIGR)
attributes(SUBSET02_AUT_2011$BIRTHPLACE)
table(SUBSET02_AUT_2011$BIRTHPLACE)
#delete the ones with missing values
SUBSET03_AUT_2011 <- subset(SUBSET02_AUT_2011, SUBSET02_AUT_2011$BIRTHPLACE>0)
SUBSET03_AUT_2011$MIGR[SUBSET03_AUT_2011$BIRTHPLACE==1] <- "AUT" #no migrant background
SUBSET03_AUT_2011$MIGR[SUBSET03_AUT_2011$BIRTHPLACE==2 | SUBSET03_AUT_2011$BIRTHPLACE==3] <- "non-AUT" #migrant background
SUBSET03_AUT_2011$MIGR <-factor(SUBSET03_AUT_2011$MIGR)
table(SUBSET03_AUT_2011$MIGR)
freq(SUBSET03_AUT_2011$MIGR)

#new variable: highest primary education degree (EDUC)
attributes(SUBSET03_AUT_2011$HATLEVEL)
SUBSET03_AUT_2011$EDUC <- "NA"
SUBSET03_AUT_2011$EDUC[SUBSET03_AUT_2011$HATLEVEL==11 | SUBSET03_AUT_2011$HATLEVEL==21] <- "3_low"
SUBSET03_AUT_2011$EDUC[SUBSET03_AUT_2011$HATLEVEL==22 | SUBSET03_AUT_2011$HATLEVEL==32 | SUBSET03_AUT_2011$HATLEVEL==40] <- "2_middle"
SUBSET03_AUT_2011$EDUC[SUBSET03_AUT_2011$HATLEVEL==50] <- "1_high"
attributes(SUBSET03_AUT_2011$EDUC)
table(SUBSET03_AUT_2011$EDUC)
freq(SUBSET03_AUT_2011$EDUC)

#new variable: part-time/full-time work (TIME)
attributes(SUBSET03_AUT_2011$MAINSTAT)
SUBSET03_AUT_2011$TIME[SUBSET03_AUT_2011$MAINSTAT==11] <-"1_FT" #full-time
SUBSET03_AUT_2011$TIME[SUBSET03_AUT_2011$MAINSTAT==12] <-"2_PT" #part-time
SUBSET03_AUT_2011$TIME[SUBSET03_AUT_2011$MAINSTAT>12] <-"3_OT" #unemployed or other inactive on the labor market  (including domestic tasks)
SUBSET03_AUT_2011$TIME <-factor(SUBSET03_AUT_2011$TIME)
table(SUBSET03_AUT_2011$TIME)
freq(SUBSET03_AUT_2011$TIME)

#new variable: establishment size (ORG)
attributes(SUBSET03_AUT_2011$LOCSIZEFIRM)
SUBSET03_AUT_2011$ORG <- "NA"
SUBSET03_AUT_2011$ORG[SUBSET03_AUT_2011$LOCSIZEFIRM==1 | SUBSET03_AUT_2011$LOCSIZEFIRM==2 | SUBSET03_AUT_2011$LOCSIZEFIRM==3] <-"1_small"
SUBSET03_AUT_2011$ORG[SUBSET03_AUT_2011$LOCSIZEFIRM==4] <-"2_medium" 
SUBSET03_AUT_2011$ORG[SUBSET03_AUT_2011$LOCSIZEFIRM==5] <-"3_large"
SUBSET03_AUT_2011$ORG[SUBSET03_AUT_2011$LOCSIZEFIRM==7 | SUBSET03_AUT_2011$LOCSIZEFIRM==-2 | SUBSET03_AUT_2011$LOCSIZEFIRM==-1] <-"4_NA"
SUBSET03_AUT_2011$ORG <-factor(SUBSET03_AUT_2011$ORG)
table(SUBSET03_AUT_2011$ORG)
freq(SUBSET03_AUT_2011$ORG)

#new variable: employment status (EMPL)
attributes(SUBSET03_AUT_2011$JOBSTAT)
SUBSET03_AUT_2011$EMPL <- -1
SUBSET03_AUT_2011$EMPL[SUBSET03_AUT_2011$JOBSTAT==-2 | SUBSET03_AUT_2011$JOBSTAT==30] <-"3_not-empl" 
SUBSET03_AUT_2011$EMPL[SUBSET03_AUT_2011$JOBSTAT==11 | SUBSET03_AUT_2011$JOBSTAT==12] <-"2_self-empl
SUBSET03_AUT_2011$EMPL[SUBSET03_AUT_2011$JOBSTAT==21 | SUBSET03_AUT_2011$JOBSTAT==22] <-"1_employee" 
SUBSET03_AUT_2011$EMPL[SUBSET03_AUT_2011$JOBSTAT==-1] <-"NA"
SUBSET03_AUT_2011$EMPL <-factor(SUBSET03_AUT_2011$EMPL)
table(SUBSET03_AUT_2011$EMPL)
freq(SUBSET03_AUT_2011$EMPL)

#delete the ones who did not answer
SUBSET04_AUT_2011 <- subset(SUBSET03_AUT_2011, EMPL!="NA")
SUBSET04_AUT_2011$EMPL <- droplevels(SUBSET04_AUT_2011$EMPL)
SUBSET04_AUT_2011$EMPL <- factor(SUBSET04_AUT_2011$EMPL)
table(SUBSET04_AUT_2011$EMPL)
freq(SUBSET04_AUT_2011$EMPL)

#new variable: skill level (Occupational Group / ISCO08) (SKILL)
attributes(SUBSET04_AUT_2011$JOBISCO)
#relabel into five categories 
SUBSET04_AUT_2011$SKILL <- "5_NA"
SUBSET04_AUT_2011$SKILL[SUBSET04_AUT_2011$JOBISCO=="OC01" | SUBSET04_AUT_2011$JOBISCO=="OC11" | SUBSET04_AUT_2011$JOBISCO=="OC12" | SUBSET04_AUT_2011$JOBISCO=="OC13" | SUBSET04_AUT_2011$JOBISCO=="OC14" | SUBSET04_AUT_2011$JOBISCO=="OC21" | SUBSET04_AUT_2011$JOBISCO=="OC22" | SUBSET04_AUT_2011$JOBISCO=="OC23" | SUBSET04_AUT_2011$JOBISCO=="OC24" | SUBSET04_AUT_2011$JOBISCO=="OC25" | SUBSET04_AUT_2011$JOBISCO=="OC26"] <-"1_Managers" 
SUBSET04_AUT_2011$SKILL[SUBSET04_AUT_2011$JOBISCO=="OC31" | SUBSET04_AUT_2011$JOBISCO=="OC32" | SUBSET04_AUT_2011$JOBISCO=="OC33" | SUBSET04_AUT_2011$JOBISCO=="OC34" | SUBSET04_AUT_2011$JOBISCO=="OC35"] <-"2_Technicians" 
SUBSET04_AUT_2011$SKILL[SUBSET04_AUT_2011$JOBISCO=="OC41" | SUBSET04_AUT_2011$JOBISCO=="OC42" | SUBSET04_AUT_2011$JOBISCO=="OC43" | SUBSET04_AUT_2011$JOBISCO=="OC44" | SUBSET04_AUT_2011$JOBISCO=="OC51" | SUBSET04_AUT_2011$JOBISCO=="OC52" | SUBSET04_AUT_2011$JOBISCO=="OC53" | SUBSET04_AUT_2011$JOBISCO=="OC54" | SUBSET04_AUT_2011$JOBISCO=="OC61" | SUBSET04_AUT_2011$JOBISCO=="OC62" | SUBSET04_AUT_2011$JOBISCO=="OC71" | SUBSET04_AUT_2011$JOBISCO=="OC72" | SUBSET04_AUT_2011$JOBISCO=="OC73" | SUBSET04_AUT_2011$JOBISCO=="OC74" | SUBSET04_AUT_2011$JOBISCO=="OC75" | SUBSET04_AUT_2011$JOBISCO=="OC81" | SUBSET04_AUT_2011$JOBISCO=="OC82" | SUBSET04_AUT_2011$JOBISCO=="OC83"] <-"3_Clerks_Workers"
SUBSET04_AUT_2011$SKILL[SUBSET04_AUT_2011$JOBISCO=="OC91" | SUBSET04_AUT_2011$JOBISCO=="OC92" | SUBSET04_AUT_2011$JOBISCO=="OC93" | SUBSET04_AUT_2011$JOBISCO=="OC94" | SUBSET04_AUT_2011$JOBISCO=="OC95" | SUBSET04_AUT_2011$JOBISCO=="OC96"] <-"4_Elementary" 
SUBSET04_AUT_2011$SKILL[SUBSET04_AUT_2011$JOBISCO==-1 | SUBSET04_AUT_2011$JOBISCO==-2] <-"5_NA"
table(SUBSET04_AUT_2011$SKILL)
freq(SUBSET04_AUT_2011$SKILL)

#new variable: barriers to participation in NFE and FED (BARRIER)
attributes(SUBSET01_AUT_2011$DIFFMAIN)
SUBSET04_AUT_2011$BARRIER <- 15
SUBSET04_AUT_2011$BARRIER[SUBSET04_AUT_2011$DIFFMAIN==1] <-"1.1_prerequisits"
SUBSET04_AUT_2011$BARRIER[SUBSET04_AUT_2011$DIFFMAIN==2] <-"3.2_costs"
SUBSET04_AUT_2011$BARRIER[SUBSET04_AUT_2011$DIFFMAIN==3] <-"4.1_lack_support" 
SUBSET04_AUT_2011$BARRIER[SUBSET04_AUT_2011$DIFFMAIN==4] <-"4.2_time_conflict" 
SUBSET04_AUT_2011$BARRIER[SUBSET04_AUT_2011$DIFFMAIN==5] <-"3.3_distance" 
SUBSET04_AUT_2011$BARRIER[SUBSET04_AUT_2011$DIFFMAIN==6] <-"1.2_computer" 
SUBSET04_AUT_2011$BARRIER[SUBSET04_AUT_2011$DIFFMAIN==7] <-"1.4_family" 
SUBSET04_AUT_2011$BARRIER[SUBSET04_AUT_2011$DIFFMAIN==8] <-"1.3_age_health" 
SUBSET04_AUT_2011$BARRIER[SUBSET04_AUT_2011$DIFFMAIN==9] <-"2.1_personal" 
SUBSET04_AUT_2011$BARRIER[SUBSET04_AUT_2011$DIFFMAIN==10] <-"3.1_offer" 
SUBSET04_AUT_2011$BARRIER[SUBSET04_AUT_2011$DIFFMAIN==-2 | SUBSET04_AUT_2011$DIFFMAIN==-1] <-"NA" 

#delete missing values
SUBSET05_AUT_2011 <-subset(SUBSET04_AUT_2011, BARRIER!= "NA")
SUBSET05_AUT_2011$BARRIER <- droplevels(SUBSET05_AUT_2011$BARRIER)
table(SUBSET05_AUT_2011$BARRIER)
freq(SUBSET05_AUT_2011$BARRIER)

#RE-CODE TO BINARY 0/1
SUBSET05_AUT_2011$BARRIER2 <- "NA"
SUBSET05_AUT_2011$BARRIER2[SUBSET05_AUT_2011$DIFFMAIN==1 | SUBSET05_AUT_2011$DIFFMAIN==7 | SUBSET05_AUT_2011$DIFFMAIN==6 | SUBSET05_AUT_2011$DIFFMAIN==9 | SUBSET05_AUT_2011$DIFFMAIN==8] <- "1_IND_LEVEL"
SUBSET05_AUT_2011$BARRIER2[SUBSET05_AUT_2011$DIFFMAIN==11 | SUBSET05_AUT_2011$DIFFMAIN==2 | SUBSET05_AUT_2011$DIFFMAIN==5 | SUBSET05_AUT_2011$DIFFMAIN==3 | SUBSET05_AUT_2011$DIFFMAIN==4 | SUBSET05_AUT_2011$DIFFMAIN==10] <- "0_INST_LEVEL" 
SUBSET05_AUT_2011$BARRIER2[SUBSET05_AUT_2011$DIFFMAIN==-1 | SUBSET05_AUT_2011$DIFFMAIN==-2] <- "N_A" #no answer or no reason
SUBSET05_AUT_2011$BARRIER2 <-factor(SUBSET05_AUT_2011$BARRIER2)
table(SUBSET05_AUT_2011$BARRIER2)
freq(SUBSET05_AUT_2011$BARRIER2)

#delete missing values
SUBSET06_AUT_2011 <-subset(SUBSET05_AUT_2011, BARRIER2!= "N_A")
SUBSET06_AUT_2011$BARRIER2 <- droplevels(SUBSET06_AUT_2011$BARRIER2)
table(SUBSET06_AUT_2011$BARRIER2)
freq(SUBSET06_AUT_2011$BARRIER2)

SUBSET06_AUT_2011$BARRIER2 <- "NA"
SUBSET06_AUT_2011$BARRIER2[SUBSET06_AUT_2011$DIFFMAIN==1 | SUBSET06_AUT_2011$DIFFMAIN==7 | SUBSET06_AUT_2011$DIFFMAIN==6 | SUBSET06_AUT_2011$DIFFMAIN==9 | SUBSET06_AUT_2011$DIFFMAIN==8] <- "1_IND_LEVEL" 
SUBSET06_AUT_2011$BARRIER2[SUBSET06_AUT_2011$DIFFMAIN==11 | SUBSET06_AUT_2011$DIFFMAIN==2 | SUBSET06_AUT_2011$DIFFMAIN==5 | SUBSET06_AUT_2011$DIFFMAIN==3 | SUBSET06_AUT_2011$DIFFMAIN==4 | SUBSET06_AUT_2011$DIFFMAIN==10] <- "0_INST_LEVEL" 
SUBSET06_AUT_2011$BARRIER2 <-factor(SUBSET06_AUT_2011$BARRIER2)
table(SUBSET06_AUT_2011$BARRIER2)
freq(SUBSET06_AUT_2011$BARRIER2)

#RE-CODE TO 4 Categories
SUBSET06_AUT_2011$BARRIER4 <- "NA"
SUBSET06_AUT_2011$BARRIER4[SUBSET06_AUT_2011$DIFFMAIN==1 | SUBSET06_AUT_2011$DIFFMAIN==6 | SUBSET06_AUT_2011$DIFFMAIN==8 | SUBSET06_AUT_2011$DIFFMAIN==7] <-"1_socio_econ_dem"
SUBSET06_AUT_2011$BARRIER4[SUBSET06_AUT_2011$DIFFMAIN==9] <-"2_psych" 
SUBSET06_AUT_2011$BARRIER4[SUBSET06_AUT_2011$DIFFMAIN==2 | SUBSET06_AUT_2011$DIFFMAIN==5 | SUBSET06_AUT_2011$DIFFMAIN==10] <-"3_direct"
SUBSET06_AUT_2011$BARRIER4[SUBSET06_AUT_2011$DIFFMAIN==3 | SUBSET06_AUT_2011$DIFFMAIN==4] <-"4_indirect" 
SUBSET06_AUT_2011$BARRIER4[SUBSET06_AUT_2011$DIFFMAIN==-1 | SUBSET06_AUT_2011$DIFFMAIN==-2 | SUBSET06_AUT_2011$DIFFMAIN==11] <- "N_A"
SUBSET06_AUT_2011$BARRIER4 <-factor(SUBSET06_AUT_2011$BARRIER4)
table(SUBSET06_AUT_2011$BARRIER4)
freq(SUBSET06_AUT_2011$BARRIER4)

#delete missing values
SUBSET07_AUT_2011 <-subset(SUBSET06_AUT_2011, BARRIER4!= "N_A")
SUBSET07_AUT_2016$BARRIER4 <- droplevels(SUBSET07_AUT_2011$BARRIER4)
table(SUBSET07_AUT_2011$BARRIER4)
freq(SUBSET07_AUT_2011$BARRIER4)

#SAMPLE STATISTICS AUT
table1<-table(SUBSET04_AUT_2011$GENDER, SUBSET04_AUT_2011$NFE_FED)
table2<-table(SUBSET04_AUT_2011$ALTER, SUBSET04_AUT_2011$NFE_FED)
table3<-table(SUBSET04_AUT_2011$MIGR, SUBSET04_AUT_2011$NFE_FED)
table4<-table(SUBSET04_AUT_2011$EDUC, SUBSET04_AUT_2011$NFE_FED)
table5<-table(SUBSET04_AUT_2011$EMPL, SUBSET04_AUT_2011$NFE_FED)
table6<-table(SUBSET04_AUT_2011$SKILL, SUBSET04_AUT_2011$NFE_FED)
table7<-table(SUBSET04_AUT_2011$TIME, SUBSET04_AUT_2011$NFE_FED)
table8<-table(SUBSET06_AUT_2011$BARRIER2, SUBSET06_AUT_2011$NFE_FED)
round(prop.table(table1,2), digits=2)
round(prop.table(table2,2), digits=2)
round(prop.table(table3,2), digits=2)
round(prop.table(table4,2), digits=2)
round(prop.table(table5,2), digits=2)
round(prop.table(table6,2), digits=2)
round(prop.table(table7,2), digits=2)
round(prop.table(table8,2), digits=2)

########Descriptive Statistics#############################################
#Cross-Table // previous level of education and Adult education, 2011
crosstab(SUBSET04_AUT_2011$NFE_FED, SUBSET04_AUT_2011$SKILL, plot=FALSE,prop.c = TRUE)
#Chi2 Test // previous level of education and Adult education
chisq.test(SUBSET04_AUT_2011$NFE_FED, SUBSET04_AUT_2011$SKILL, correct= F,)

########Logistic Regressions: HYPOTHESIS 1#################################
########MODEL 1: Socio-demographics########################################
OUTPUT02 <-glm(NFE_FED~ GENDER+ALTER+MIGR+EDUC, data=SUBSET04_AUT_2011,family=binomial())
OR.vector02 <-exp(coef(OUTPUT02))
CI.vector02 <-exp(confint(OUTPUT02))
p.values02 <-list(summary(OUTPUT02)$coefficients[,4])

stargazer(OUTPUT02,
          coef= list(OR.vector02), ci = T,
          ci.custom= list(CI.vector02),
          single.row= F,
          type = "text",
          p=c(p.values02))

########MODEL 2: Employment/skill-related variable#########################
OUTPUT03 <-glm(NFE_FED~ EMPL+SKILL+ORG+TIME, data=SUBSET04_AUT_2011,family=binomial())
OR.vector03 <-exp(coef(OUTPUT03))
CI.vector03 <-exp(confint(OUTPUT03))
p.values03 <-list(summary(OUTPUT03)$coefficients[,4])

stargazer(OUTPUT03,
          coef= list(OR.vector03), ci = T,
          ci.custom= list(CI.vector03),
          single.row= F,
          type = "text",
          p=c(p.values03))

########MODEL3: ALL IVs####################################################
OUTPUT04 <-glm(NFE_FED~ GENDER+ALTER+MIGR+EDUC+EMPL+SKILL+ORG+TIME, data=SUBSET04_AUT_2011,family=binomial())
OR.vector04 <-exp(coef(OUTPUT04))
CI.vector04 <-exp(confint(OUTPUT04))
p.values04 <-list(summary(OUTPUT04)$coefficients[,4])

stargazer(OUTPUT04,
          coef= list(OR.vector04), ci = T,
          ci.custom= list(CI.vector04),
          single.row= F,
          type = "text",
          p=c(p.values04))

########BARPLOT HYPOTHESIS 1###############################################
# Calculate a cross table (only absolute values)
TABLE1 <- table(SUBSET04_AUT_2011$NFE_FED, SUBSET04_AUT_2011$SKILL)
# Use the cross table (TABLE1) to calculate the column percent
TABLE2 <- prop.table(TABLE1,2)
# Use the calculated column percent (TABLE2) and insert it into a data frame (TABLE3)
TABLE3 <- as.data.frame(TABLE2)
ggplot(TABLE3, aes(fill=Var1, x=Var2, Var3, y=Freq)) + 
  geom_bar(stat="identity") + 
  ggtitle("Austria, 2011") + ylim(0,1) + ylab("%")+ xlab("") + 
  scale_fill_manual(name = "Participated in adult education?", labels = c("No", "Yes"), values = c("#032F5E", "#B4C6E7") + 
theme_bw())

########HYPOTHESIS 2: INCENTIVES#################################
#new variable: reasons to participate in NFE (REASON)
attributes(SUBSET04_AUT_2011$NFEREASON1_01)
table(SUBSET04_AUT_2011$NFEREASON1_01)
SUBSET04_AUT_2011$INCENTIVENFE <- "NA"
SUBSET04_AUT_2011$INCENTIVENFE[SUBSET04_AUT_2011$NFEREASON1_06==1 | SUBSET04_AUT_2011$NFEREASON1_07==1 | SUBSET04_AUT_2011$NFEREASON1_09==1] <- "2_personal"
SUBSET04_AUT_2011$INCENTIVENFE[(SUBSET04_AUT_2011$NFEREASON1_08==1 | SUBSET04_AUT_2011$NFEREASON1_05==1 | SUBSET04_AUT_2011$NFEREASON1_04==1 | SUBSET04_AUT_2011$NFEREASON1_03==1 | SUBSET04_AUT_2011$NFEREASON1_02==1 | SUBSET04_AUT_2011$NFEREASON1_01==1) & (SUBSET04_AUT_2011$INCENTIVENFE=="2_personal")] <- "1_job-rel + personal"
SUBSET04_AUT_2011$INCENTIVENFE[(SUBSET04_AUT_2011$NFEREASON1_08==1 | SUBSET04_AUT_2011$NFEREASON1_05==1 | SUBSET04_AUT_2011$NFEREASON1_04==1 | SUBSET04_AUT_2011$NFEREASON1_03==1 | SUBSET04_AUT_2011$NFEREASON1_02==1 | SUBSET04_AUT_2011$NFEREASON1_01==1) & (SUBSET04_AUT_2011$INCENTIVENFE!="2_personal" & SUBSET04_AUT_2011$INCENTIVENFE!="1_job-rel + personal")] <- "3_job-rel"
SUBSET04m_AUT_2011 <- subset(SUBSET04_AUT_2011, INCENTIVENFE!= "NA")
SUBSET04m_AUT_2011$INCENTIVENFE <- droplevels(SUBSET04m_AUT_2011$INCENTIVENFE)
table(SUBSET04m_AUT_2011$INCENTIVENFE)
freq(SUBSET04m_AUT_2011$INCENTIVENFE)

#Descriptive Statistics
table9<-table(SUBSET04m_AUT_2011$INCENTIVENFE, SUBSET04m_AUT_2011$NFE_FED)
round(prop.table(table9,2), digits=2)

#MULTINOMINAL
OUTPUT01m <- multinom(INCENTIVENFE ~ SKILL + GENDER + ALTER + ORG + EDUC + TIME + MIGR + EMPL, data = SUBSET04m_AUT_2011)
# to see output
summary(OUTPUT01m)
#Calculation of Significance
## z value:
z <- summary(OUTPUT01m)$coefficients/summary(OUTPUT01m)$standard.errors
## 2-tailed z test
p <- (1-pnorm(abs(z), 0, 1))*2
p
#Display of results
stargazer(OUTPUT01m, type="text", p.auto=F)
#Table with RRR
OUTPUT01rrr=exp(coef(OUTPUT01m))
stargazer(OUTPUT01m, coef=list(OUTPUT01rrr), type="text", p.auto=F)
#Calculation of predicted probabilities using the effects package
OUTPUT01PP <- Effect("SKILL", OUTPUT01m)
OUTPUT01PP$model.matrix
#Preparation for Graph
pp <- data.frame(OUTPUT01PP$prob)
#Assign the labels in the order of the rows that you identified with the model matrix
pp$labels <- c("1_Managers", "2_Technicians", "3_Clerks_Workers", "4_Elementary", "5_NA")
pp <- melt(pp, id="labels")
names(pp)[names(pp)=="value"] <- "pp"

#Extraction Names DV
pp$INCENTIVENFE <-substring(pp[,2],6)
lp <- data.frame(OUTPUT01PP$lower.prob)
lp <- melt(lp)
names(lp)[names(lp)=="value"] <- "l"
up <- data.frame(OUTPUT01PP$upper.prob)
up <- melt(up)
names(up)[names(up)=="value"] <- "u"
toplot <- cbind(pp, lp[,2], up[,2])

#Organisation DV
toplot$INCENTIVENFE <- factor(toplot$INCENTIVENFE )
levels(toplot$INCENTIVENFE )
toplot$INCENTIVENFE= factor(toplot$INCENTIVENFE, levels(toplot$INCENTIVENFE)[c(1,2,3)])

#Organisation IV
toplot$labels <- factor(toplot$labels)
levels(toplot$labels)
toplot$labels = factor(toplot$labels, levels(toplot$labels)[c(1,2,3,4,5)])

#Plotting the Multinominals
getPalette = colorRampPalette(brewer.pal(5, "RdBu"))
ggplot(toplot, aes(x=INCENTIVENFE, y=pp, fill=labels)) + 
  geom_bar( stat="identity", position = "dodge") +
  geom_errorbar( aes( ymin=lp[,2], ymax=up[,2]), width=0.2, position = position_dodge(0.9)) +
  scale_fill_manual(values = getPalette(5)) +
  theme_bw() +
  ggtitle("Incentives to participate in AE in Austria, 2011") +
  xlab("Incentive")

#Cross-tabs
crosstab(SUBSET04m_AUT_2011$INCENTIVENFE, SUBSET04m_AUT_2011$SKILL, plot=FALSE,prop.c = TRUE)
#Chi-Square test
chisq.test(SUBSET04m_AUT_2011$INCENTIVENFE, SUBSET04m_AUT_2011$SKILL, correct= F,)



########Logistic Regressions: HYPOTHESIS 3#################################
########MODEL 1: Socio-demographics########################################
OUTPUT06 <-glm(BARRIER2~ GENDER+ALTER+MIGR+EDUC, data=SUBSET06_AUT_2011,family=binomial())
OR.vector06 <-exp(coef(OUTPUT06))
CI.vector06 <-exp(confint(OUTPUT06))
p.values06 <-list(summary(OUTPUT06)$coefficients[,4])

stargazer(OUTPUT06,
          coef= list(OR.vector06), ci = T,
          ci.custom= list(CI.vector06),
          single.row= F,
          type = "text",
          p=c(p.values06))

########MODEL 2: Employment/skill-related variable#########################
OUTPUT07 <-glm(BARRIER2~ EMPL+SKILL+ORG+TIME, data=SUBSET06_AUT_2011,family=binomial())
OR.vector07 <-exp(coef(OUTPUT07))
CI.vector07 <-exp(confint(OUTPUT07))
p.values07 <-list(summary(OUTPUT07)$coefficients[,4])

stargazer(OUTPUT07,
          coef= list(OR.vector07), ci = T,
          ci.custom= list(CI.vector07),
          single.row= F,
          type = "text",
          p=c(p.values07))

########MODEL 3: ALL IVs###################################################
OUTPUT08 <-glm(BARRIER2~ GENDER+ALTER+MIGR+EDUC+SKILL+TIME+ORG+NFE_FED, data=SUBSET06_AUT_2011,family=binomial())
OR.vector08 <-exp(coef(OUTPUT08))
CI.vector08 <-exp(confint(OUTPUT08))
p.values08 <-list(summary(OUTPUT08)$coefficients[,4])

stargazer(OUTPUT08,
          coef= list(OR.vector08), ci = T,
          ci.custom= list(CI.vector08),
          single.row= F,
          type = "text",
          p=c(p.values08))

#Chi2 test to check for multicollinearity
chisq.test(SUBSET06_AUT_2011$TIME, SUBSET06_AUT_2011$EMPL, correct= F,)
crosstab(SUBSET06_AUT_2011$BARRIER2, SUBSET06_AUT_2011$TIME, plot=FALSE,prop.c = TRUE)
crosstab(SUBSET06_AUT_2011$BARRIER2, SUBSET06_AUT_2011$EMPL, plot=FALSE,prop.c = TRUE)
crosstab(SUBSET03_AUT_2011$NFE_FED, SUBSET03_AUT_2011$EMPL, plot=FALSE,prop.c = TRUE)

########DESCRIPTIVE STATISTICS#############################################
crosstab(SUBSET06_AUT_2011$BARRIER4, SUBSET06_AUT_2011$SKILL, plot=FALSE,prop.c = TRUE)
chisq.test(SUBSET06_AUT_2011$BARRIER4, SUBSET06_AUT_2011$SKILL, correct= F,)

###########################################################################
##############AUSTRIA 2016: Read in data & Select sample for AUT AES 2016##
#Set Working Directory
setwd("~/OneDrive/Master Thesis/Data/Austria/Daten_AES_2016") 

AES2016_AUT <- read.csv2(file = "AES 2016_final_bearbeitet.csv")
SUBSET01a_AUT_2016<-subset(AES2016_AUT)

#FILTER: limit samples to potential AE participants and are out of their primary education
attributes(SUBSET01a_AUT_2016$MAINSTAT)
table(SUBSET01a_AUT_2016$MAINSTAT)
SUBSET01_AUT_2016 <- subset(SUBSET01a_AUT_2016, SUBSET01a_AUT_2016$MAINSTAT==11 | SUBSET01a_AUT_2016$MAINSTAT==12 | SUBSET01a_AUT_2016$MAINSTAT==20 | SUBSET01a_AUT_2016$MAINSTAT==32 | SUBSET01a_AUT_2016$MAINSTAT==33 | SUBSET01a_AUT_2016$MAINSTAT==35 | SUBSET01a_AUT_2016$MAINSTAT==36 | SUBSET01a_AUT_2016$MAINSTAT==-1)
table(SUBSET01_AUT_2016$MAINSTAT)

#DV: Participation in non-formal (NFE) or formal adult education (FED) 
#Participation NFE
attributes(SUBSET01_AUT_2016$NFENUM)
SUBSET01_AUT_2016$NFE <- -1
SUBSET01_AUT_2016$NFE[SUBSET01_AUT_2016$NFENUM>0 & SUBSET01_AUT_2016$NFENUM<33] <- "1" #yes
SUBSET01_AUT_2016$NFE[SUBSET01_AUT_2016$NFENUM==0] <- "0" #no
SUBSET01_AUT_2016$NFE <-factor(SUBSET01_AUT_2016$NFE)
table(SUBSET01_AUT_2016$NFE)
freq(SUBSET01_AUT_2016$NFE)

#Combination of FED und NFE
SUBSET01_AUT_2016$NFE_FED <- -1
SUBSET01_AUT_2016$NFE_FED[SUBSET01_AUT_2016$FED==1 | SUBSET01_AUT_2016$NFE==1] <- "1"
SUBSET01_AUT_2016$NFE_FED[SUBSET01_AUT_2016$FED==2 & SUBSET01_AUT_2016$NFE==0] <- "0"
SUBSET01_AUT_2016$NFE_FED <-factor(SUBSET01_AUT_2016$NFE_FED)
table(SUBSET01_AUT_2016$NFE_FED)
freq(SUBSET01_AUT_2016$NFE_FED)

#new variable: GENDER
attributes(SUBSET01_AUT_2016$SEX)
SUBSET01_AUT_2016$GENDER[SUBSET01_AUT_2016$SEX==1] <-"Male"
SUBSET01_AUT_2016$GENDER[SUBSET01_AUT_2016$SEX==2] <-"Female"
SUBSET01_AUT_2016$GENDER <-factor(SUBSET01_AUT_2016$GENDER)
table(SUBSET01_AUT_2016$GENDER)
freq(SUBSET01_AUT_2016$GENDER)

#new variable: AGE
attributes(SUBSET01_AUT_2016$AGE)
SUBSET01_AUT_2016$ALTER <- "NA"
SUBSET01_AUT_2016$ALTER[SUBSET01_AUT_2016$AGE>17 & SUBSET01_AUT_2016$AGE<35] <-"1_young"
SUBSET01_AUT_2016$ALTER[SUBSET01_AUT_2016$AGE>35 & SUBSET01_AUT_2016$AGE<53] <-"2_medium"
SUBSET01_AUT_2016$ALTER[SUBSET01_AUT_2016$AGE>54 & SUBSET01_AUT_2016$AGE<65] <-"3_old"
SUBSET01_AUT_2016$ALTER <-factor(SUBSET01_AUT_2016$ALTER)
table(SUBSET01_AUT_2016$ALTER)
freq(SUBSET01_AUT_2016$ALTER)
#delete missing values
SUBSET01b_AUT_2016 <-subset(SUBSET01_AUT_2016, ALTER!="NA")
SUBSET01b_AUT_2016$ALTER <- droplevels(SUBSET01b_AUT_2016$ALTER)
SUBSET01b_AUT_2016$ALTER <-factor(SUBSET01b_AUT_2016$ALTER)
table(SUBSET01b_AUT_2016$ALTER)
freq(SUBSET01b_AUT_2016$ALTER)

#new variable: migrant background (MIGR)
attributes(SUBSET01b_AUT_2016$BIRTHPLACE)
#delete missing values
SUBSET02_AUT_2016 <- subset(SUBSET01b_AUT_2016, SUBSET01b_AUT_2016$BIRTHPLACE>0)
SUBSET02_AUT_2016$MIGR <- "NA"
SUBSET02_AUT_2016$MIGR[SUBSET02_AUT_2016$BIRTHPLACE==1] <- "AUT" 
SUBSET02_AUT_2016$MIGR[SUBSET02_AUT_2016$BIRTHPLACE==2 | SUBSET02_AUT_2016$BIRTHPLACE==3] <- "non-AUT" 
SUBSET02_AUT_2016$MIGR <-factor(SUBSET02_AUT_2016$MIGR)
table(SUBSET02_AUT_2016$MIGR)
freq(SUBSET02_AUT_2016$MIGR)

#new variable: highest primary education degree (EDUC)
attributes(SUBSET02_AUT_2016$HATLEVEL)
table(SUBSET02_AUT_2016$HATLEVEL)
freq(SUBSET02_AUT_2016$HATLEVEL)
SUBSET02_AUT_2016$EDUC <- "NA"
SUBSET02_AUT_2016$EDUC[SUBSET02_AUT_2016$HATLEVEL==1] <- "3_low"
SUBSET02_AUT_2016$EDUC[SUBSET02_AUT_2016$HATLEVEL==2] <- "2_middle"
SUBSET02_AUT_2016$EDUC[SUBSET02_AUT_2016$HATLEVEL==3 | SUBSET02_AUT_2016$HATLEVEL==4] <- "1_high"
attributes(SUBSET02_AUT_2016$EDUC)
table(SUBSET02_AUT_2016$EDUC)
freq(SUBSET02_AUT_2016$EDUC)

#new variable: part-time/full-time work (TIME)
attributes(SUBSET02_AUT_2016$MAINSTAT)
SUBSET02_AUT_2016$TIME <- "NA"
SUBSET02_AUT_2016$TIME[SUBSET02_AUT_2016$MAINSTAT==11] <-"1_FT" 
SUBSET02_AUT_2016$TIME[SUBSET02_AUT_2016$MAINSTAT==12] <-"2_PT" 
SUBSET02_AUT_2016$TIME[SUBSET02_AUT_2016$MAINSTAT>12 | SUBSET02_AUT_2016$MAINSTAT==-1 | SUBSET02_AUT_2016$MAINSTAT=="NA"] <-"3_OT" 
SUBSET02_AUT_2016$TIME <-factor(SUBSET02_AUT_2016$TIME)
table(SUBSET02_AUT_2016$TIME)
freq(SUBSET02_AUT_2016$TIME)

#new variable: establishment size (ORG)
attributes(SUBSET02_AUT_2016$LOCSIZEFIRM)
SUBSET02_AUT_2016$ORG <- "NA"
SUBSET02_AUT_2016$ORG[SUBSET02_AUT_2016$LOCSIZEFIRM==1 | SUBSET02_AUT_2016$LOCSIZEFIRM==2 | SUBSET02_AUT_2016$LOCSIZEFIRM==3] <-"1_small"
SUBSET02_AUT_2016$ORG[SUBSET02_AUT_2016$LOCSIZEFIRM==4] <-"2_medium" 
SUBSET02_AUT_2016$ORG[SUBSET02_AUT_2016$LOCSIZEFIRM==5] <-"3_large"
SUBSET02_AUT_2016$ORG[SUBSET02_AUT_2016$LOCSIZEFIRM==7 | SUBSET02_AUT_2016$LOCSIZEFIRM==-2 | SUBSET02_AUT_2016$LOCSIZEFIRM==-1] <-"NA"
SUBSET02_AUT_2016$ORG <-factor(SUBSET02_AUT_2016$ORG)
table(SUBSET02_AUT_2016$ORG)
freq(SUBSET02_AUT_2016$ORG)

#new variable: employment status (EMPL)
attributes(SUBSET02_AUT_2016$JOBSTAT)
SUBSET02_AUT_2016$EMPL <- -1
SUBSET02_AUT_2016$EMPL[SUBSET02_AUT_2016$JOBSTAT==-2 | SUBSET02_AUT_2016$JOBSTAT==-1 | SUBSET02_AUT_2016$JOBSTAT==30] <-"3_not-empl" 
SUBSET02_AUT_2016$EMPL[SUBSET02_AUT_2016$JOBSTAT==11 | SUBSET02_AUT_2016$JOBSTAT==12] <-"2_self-empl" 
SUBSET02_AUT_2016$EMPL[SUBSET02_AUT_2016$JOBSTAT==21 | SUBSET02_AUT_2016$JOBSTAT==22] <-"1_employee" 
SUBSET02_AUT_2016$EMPL <-factor(SUBSET02_AUT_2016$EMPL)
table(SUBSET02_AUT_2016$EMPL)
freq(SUBSET02_AUT_2016$EMPL)

#new variable: skill level (Occupational Group / ISCO08) (SKILL)
attributes(SUBSET02_AUT_2016$JOBISCO)
#relabel into five categories
SUBSET02_AUT_2016$SKILL <- "5_NA"
SUBSET02_AUT_2016$SKILL[SUBSET02_AUT_2016$JOBISCO=="OC01" | SUBSET02_AUT_2016$JOBISCO=="OC11" | SUBSET02_AUT_2016$JOBISCO=="OC12" | SUBSET02_AUT_2016$JOBISCO=="OC13" | SUBSET02_AUT_2016$JOBISCO=="OC14" | SUBSET02_AUT_2016$JOBISCO=="OC21" | SUBSET02_AUT_2016$JOBISCO=="OC22" | SUBSET02_AUT_2016$JOBISCO=="OC23" | SUBSET02_AUT_2016$JOBISCO=="OC24" | SUBSET02_AUT_2016$JOBISCO=="OC25" | SUBSET02_AUT_2016$JOBISCO=="OC26"] <-"1_Managers" 
SUBSET02_AUT_2016$SKILL[SUBSET02_AUT_2016$JOBISCO=="OC31" | SUBSET02_AUT_2016$JOBISCO=="OC32" | SUBSET02_AUT_2016$JOBISCO=="OC33" | SUBSET02_AUT_2016$JOBISCO=="OC34" | SUBSET02_AUT_2016$JOBISCO=="OC35"] <-"2_Technicians" 
SUBSET02_AUT_2016$SKILL[SUBSET02_AUT_2016$JOBISCO=="OC41" | SUBSET02_AUT_2016$JOBISCO=="OC42" | SUBSET02_AUT_2016$JOBISCO=="OC43"  | SUBSET02_AUT_2016$JOBISCO=="OC44" | SUBSET02_AUT_2016$JOBISCO=="OC51" | SUBSET02_AUT_2016$JOBISCO=="OC52" | SUBSET02_AUT_2016$JOBISCO=="OC53" | SUBSET02_AUT_2016$JOBISCO=="OC54" | SUBSET02_AUT_2016$JOBISCO=="OC61" | SUBSET02_AUT_2016$JOBISCO=="OC62" | SUBSET02_AUT_2016$JOBISCO=="OC71" | SUBSET02_AUT_2016$JOBISCO=="OC72" | SUBSET02_AUT_2016$JOBISCO=="OC73" | SUBSET02_AUT_2016$JOBISCO=="OC74" | SUBSET02_AUT_2016$JOBISCO=="OC75" | SUBSET02_AUT_2016$JOBISCO=="OC81" | SUBSET02_AUT_2016$JOBISCO=="OC82" | SUBSET02_AUT_2016$JOBISCO=="OC83"] <-"3_Clerks_Workers" 
SUBSET02_AUT_2016$SKILL[SUBSET02_AUT_2016$JOBISCO=="OC91" | SUBSET02_AUT_2016$JOBISCO=="OC92" | SUBSET02_AUT_2016$JOBISCO=="OC93" | SUBSET02_AUT_2016$JOBISCO=="OC94" | SUBSET02_AUT_2016$JOBISCO=="OC95" | SUBSET02_AUT_2016$JOBISCO=="OC96"] <-"4_Elementary" 
SUBSET02_AUT_2016$SKILL[SUBSET02_AUT_2016$JOBISCO==-1 | SUBSET02_AUT_2016$JOBISCO==-2] <-"5_NA" 
table(SUBSET02_AUT_2016$SKILL)
freq(SUBSET02_AUT_2016$SKILL)

#new variable: barriers to participation in NFE and FED (BARRIER)
attributes(SUBSET02_AUT_2016$DIFFMAIN)
SUBSET03_AUT_2016 <- subset(SUBSET02_AUT_2016)
SUBSET03_AUT_2016$BARRIER <- "NA"
SUBSET03_AUT_2016$BARRIER[SUBSET03_AUT_2016$DIFFMAIN==1] <-"1.1_prerequisits" 
SUBSET03_AUT_2016$BARRIER[SUBSET03_AUT_2016$DIFFMAIN==2] <-"3.2_costs" 
SUBSET03_AUT_2016$BARRIER[SUBSET03_AUT_2016$DIFFMAIN==3] <-"4.1_lack_support" 
SUBSET03_AUT_2016$BARRIER[SUBSET03_AUT_2016$DIFFMAIN==4] <-"4.2_time_conflict" 
SUBSET03_AUT_2016$BARRIER[SUBSET03_AUT_2016$DIFFMAIN==5] <-"3.3_distance" 
SUBSET03_AUT_2016$BARRIER[SUBSET03_AUT_2016$DIFFMAIN==6] <-"1.2_computer" 
SUBSET03_AUT_2016$BARRIER[SUBSET03_AUT_2016$DIFFMAIN==7] <-"1.4_family" 
SUBSET03_AUT_2016$BARRIER[SUBSET03_AUT_2016$DIFFMAIN==81 | SUBSET03_AUT_2016$DIFFMAIN==82] <-"1.3_age_health" 
SUBSET03_AUT_2016$BARRIER[SUBSET03_AUT_2016$DIFFMAIN==9] <-"2.1_personal" 
SUBSET03_AUT_2016$BARRIER[SUBSET03_AUT_2016$DIFFMAIN==10] <-"3.1_offer" 
SUBSET03_AUT_2016$BARRIER[SUBSET03_AUT_2016$DIFFMAIN==12] <-"2.2_bad_exp" 
SUBSET03_AUT_2016$BARRIER[SUBSET03_AUT_2016$DIFFMAIN==-2 | SUBSET03_AUT_2016$DIFFMAIN==-1] <-"NA"
table(SUBSET03_AUT_2016$BARRIER)
freq(SUBSET03_AUT_2016$BARRIER)

#delete the missing values
SUBSET03a_AUT_2016 <-subset(SUBSET03_AUT_2016, BARRIER!= "NA")
SUBSET03a_AUT_2016$BARRIER2 <- droplevels(SUBSET03a_AUT_2016$BARRIER)
table(SUBSET03a_AUT_2016$BARRIER)
freq(SUBSET03a_AUT_2016$BARRIER)

#RE-CODE TO 4 Categories
SUBSET03a_AUT_2016$BARRIER4 <- "NA"
SUBSET03a_AUT_2016$BARRIER4[SUBSET03a_AUT_2016$DIFFMAIN==1 | SUBSET03a_AUT_2016$DIFFMAIN==6 | SUBSET03a_AUT_2016$DIFFMAIN==81 | SUBSET03a_AUT_2016$DIFFMAIN==82 | SUBSET03a_AUT_2016$DIFFMAIN==7] <-"1_socio_econ_dem" 
SUBSET03a_AUT_2016$BARRIER4[SUBSET03a_AUT_2016$DIFFMAIN==12 | SUBSET03a_AUT_2016$DIFFMAIN==9] <-"2_psych 
SUBSET03a_AUT_2016$BARRIER4[SUBSET03a_AUT_2016$DIFFMAIN==2 | SUBSET03a_AUT_2016$DIFFMAIN==5 | SUBSET03a_AUT_2016$DIFFMAIN==10] <-"3_direct" 
SUBSET03a_AUT_2016$BARRIER4[SUBSET03a_AUT_2016$DIFFMAIN==3 | SUBSET03a_AUT_2016$DIFFMAIN==4] <-"4_indirect" 
SUBSET03a_AUT_2016$BARRIER4[SUBSET03a_AUT_2016$DIFFMAIN==-1 | SUBSET03a_AUT_2016$DIFFMAIN==-2] <- "N_A" 
SUBSET03a_AUT_2016$BARRIER4 <-factor(SUBSET03a_AUT_2016$BARRIER4)
table(SUBSET03a_AUT_2016$BARRIER4)
freq(SUBSET03a_AUT_2016$BARRIER4)

#delete missing values
SUBSET04_AUT_2016 <-subset(SUBSET03a_AUT_2016, BARRIER4!= "N_A")
SUBSET04_AUT_2016$BARRIER4 <- droplevels(SUBSET04_AUT_2016$BARRIER4)
table(SUBSET04_AUT_2016$BARRIER4)
freq(SUBSET04_AUT_2016$BARRIER4)

#RE-CODE TO BINARY 0/1
attributes(SUBSET04_AUT_2016$DIFFMAIN)
SUBSET05_AUT_2016 <-subset(SUBSET04_AUT_2016)
SUBSET05_AUT_2016$BARRIER2 <- "NA"
SUBSET05_AUT_2016$BARRIER2[SUBSET05_AUT_2016$DIFFMAIN==1 | SUBSET05_AUT_2016$DIFFMAIN==7 | SUBSET05_AUT_2016$DIFFMAIN==6 | SUBSET05_AUT_2016$DIFFMAIN==81 | SUBSET05_AUT_2016$DIFFMAIN==82 | SUBSET05_AUT_2016$DIFFMAIN==12 | SUBSET05_AUT_2016$DIFFMAIN==9] <- "1_IND_LEVEL" 
SUBSET05_AUT_2016$BARRIER2[SUBSET05_AUT_2016$DIFFMAIN==10 | SUBSET05_AUT_2016$DIFFMAIN==2 | SUBSET05_AUT_2016$DIFFMAIN==5 | SUBSET05_AUT_2016$DIFFMAIN==3 | SUBSET05_AUT_2016$DIFFMAIN==4] <- "0_INST_LEVEL" 
SUBSET05_AUT_2016$BARRIER2[SUBSET05_AUT_2016$DIFFMAIN==-1 | SUBSET05_AUT_2016$DIFFMAIN==-2] <- "N_A" #no answer or no reason
SUBSET05_AUT_2016$BARRIER2 <-factor(SUBSET05_AUT_2016$BARRIER2)
table(SUBSET05_AUT_2016$BARRIER2)
freq(SUBSET05_AUT_2016$BARRIER2)
#delete missing values
SUBSET05_AUT_2016 <-subset(SUBSET05_AUT_2016, BARRIER2!= "N_A")
SUBSET05_AUT_2016$BARRIER2 <- droplevels(SUBSET05_AUT_2016$BARRIER2)
table(SUBSET05_AUT_2016$BARRIER2)
freq(SUBSET05_AUT_2016$BARRIER2)

########SAMPLE STATISTICS AUT##############################################
table1<-table(SUBSET02_AUT_2016$GENDER, SUBSET02_AUT_2016$NFE_FED)
table2<-table(SUBSET02_AUT_2016$ALTER, SUBSET02_AUT_2016$NFE_FED)
table3<-table(SUBSET02_AUT_2016$MIGR, SUBSET02_AUT_2016$NFE_FED)
table4<-table(SUBSET02_AUT_2016$EDUC, SUBSET02_AUT_2016$NFE_FED)
table5<-table(SUBSET02_AUT_2016$EMPL, SUBSET02_AUT_2016$NFE_FED)
table6<-table(SUBSET02_AUT_2016$SKILL, SUBSET02_AUT_2016$NFE_FED)
table7<-table(SUBSET02_AUT_2016$TIME, SUBSET02_AUT_2016$NFE_FED)
table8<-table(SUBSET05_AUT_2016$BARRIER2, SUBSET05_AUT_2016$NFE_FED)
round(prop.table(table1,2), digits=2)
round(prop.table(table2,2), digits=2)
round(prop.table(table3,2), digits=2)
round(prop.table(table4,2), digits=2)
round(prop.table(table5,2), digits=2)
round(prop.table(table6,2), digits=2)
round(prop.table(table7,2), digits=2)
round(prop.table(table8,2), digits=2)

########Descriptive Statistics#############################################
#Cross-Table // occupational skill level and Adult education, 2016
crosstab(SUBSET02_AUT_2016$NFE_FED, SUBSET02_AUT_2016$SKILL, plot=FALSE,prop.c = TRUE)

#Chi2 Test // occupational skill level and Adult education
chisq.test(SUBSET02_AUT_2016$NFE_FED, SUBSET02_AUT_2016$SKILL, correct= F,)

#Cross-Table // occupational skill level and Barrier, 2016
crosstab(SUBSET04_AUT_2016$BARRIER4, SUBSET04_AUT_2016$SKILL, plot=FALSE,prop.c = TRUE)

#Chi2 Test // occupational skill level and barrier
chisq.test(SUBSET04_AUT_2016$BARRIER4, SUBSET04_AUT_2016$SKILL, correct= F,)

#new variable: reasons to participate in NFE (REASON)
attributes(SUBSET05_AUT_2016$NFEREASON1)
attributes(SUBSET05_AUT_2016$NFEREASON1_01a)
table(SUBSET05_AUT_2016$NFEREASON1_01a)
SUBSET05_AUT_2016$INCENTIVENFE <- "NA"
SUBSET05_AUT_2016$INCENTIVENFE[SUBSET05_AUT_2016$NFEREASON1_06==1 | SUBSET05_AUT_2016$NFEREASON1_07==1 | SUBSET05_AUT_2016$NFEREASON1_09==1 | SUBSET05_AUT_2016$NFEREASON1_10==1 | SUBSET05_AUT_2016$NFEREASON1_12==1] <- "2_personal"
SUBSET05_AUT_2016$INCENTIVENFE[(SUBSET05_AUT_2016$NFEREASON1_08==1 | SUBSET05_AUT_2016$NFEREASON1_13==1 | SUBSET05_AUT_2016$NFEREASON1_04==1 | SUBSET05_AUT_2016$NFEREASON1_03==1 | SUBSET05_AUT_2016$NFEREASON1_02==1 | SUBSET05_AUT_2016$NFEREASON1_01a==1 | SUBSET05_AUT_2016$NFEREASON1_01b==1 | SUBSET05_AUT_2016$NFEREASON1_11==1) & (SUBSET05_AUT_2016$INCENTIVENFE=="2_personal")] <- "1_job-rel + personal"
SUBSET05_AUT_2016$INCENTIVENFE[(SUBSET05_AUT_2016$NFEREASON1_08==1 | SUBSET05_AUT_2016$NFEREASON1_13==1 | SUBSET05_AUT_2016$NFEREASON1_04==1 | SUBSET05_AUT_2016$NFEREASON1_03==1 | SUBSET05_AUT_2016$NFEREASON1_02==1 | SUBSET05_AUT_2016$NFEREASON1_01a==1 | SUBSET05_AUT_2016$NFEREASON1_01b==1 | SUBSET05_AUT_2016$NFEREASON1_11==1) & (SUBSET05_AUT_2016$INCENTIVENFE!="2_personal" & SUBSET05_AUT_2016$INCENTIVENFE!="1_job-rel + personal")] <- "3_job-rel"
table(SUBSET05_AUT_2016$INCENTIVENFE)
#delete missing values
SUBSET05m_AUT_2016 <- subset(SUBSET05_AUT_2016, INCENTIVENFE!= "NA")
SUBSET05m_AUT_2016$INCENTIVENFE <- droplevels(SUBSET05m_AUT_2016$INCENTIVENFE)
table(SUBSET05m_AUT_2016$INCENTIVENFE)
freq(SUBSET05m_AUT_2016$INCENTIVENFE)

#########SAMPLE STATISTICS#################################################
table9<-table(SUBSET05m_AUT_2016$INCENTIVENFE, SUBSET05m_AUT_2016$NFE_FED)
round(prop.table(table9,2), digits=2)

##########Hypothesis 3#####################################################
##########MULTINOMINAL#####################################################
OUTPUT01m <- multinom(INCENTIVENFE ~ SKILL + GENDER + ALTER + ORG + EDUC + TIME + MIGR + EMPL, data = SUBSET05m_AUT_2016)
#to see output
summary(OUTPUT01m)
#Calculation of Significance
## z value:
z <- summary(OUTPUT01m)$coefficients/summary(OUTPUT01m)$standard.errors
## 2-tailed z test
p <- (1-pnorm(abs(z), 0, 1))*2
p
#Display of results
stargazer(OUTPUT01m, type="text", p.auto=F)
#Table with RRR
OUTPUT01rrr=exp(coef(OUTPUT01m))
stargazer(OUTPUT01m, coef=list(OUTPUT01rrr), type="text", p.auto=F)
#Calculation of predicted probabilities using the effects package
OUTPUT01PP <- Effect("SKILL", OUTPUT01m)
OUTPUT01PP$model.matrix
#Preparation for Graph
pp <- data.frame(OUTPUT01PP$prob)
# Assign the labels in the order of the rows that you identified with the model matrix
pp$labels <- c("1_Managers", "2_Technicians", "3_Clerks_Workers", "4_Elementary", "5_NA")
pp <- melt(pp, id="labels")
names(pp)[names(pp)=="value"] <- "pp"
#Extraction Names DV
pp$INCENTIVENFE <-substring(pp[,2],6)
lp <- data.frame(OUTPUT01PP$lower.prob)
lp <- melt(lp)
names(lp)[names(lp)=="value"] <- "l"
up <- data.frame(OUTPUT01PP$upper.prob)
up <- melt(up)
names(up)[names(up)=="value"] <- "u"
toplot <- cbind(pp, lp[,2], up[,2])

#Organisation DV
toplot$INCENTIVENFE <- factor(toplot$INCENTIVENFE )
levels(toplot$INCENTIVENFE )
toplot$INCENTIVENFE= factor(toplot$INCENTIVENFE, levels(toplot$INCENTIVENFE)[c(1,2,3)])

#Organisation IV
toplot$labels <- factor(toplot$labels)
levels(toplot$labels)
toplot$labels = factor(toplot$labels, levels(toplot$labels)[c(1,2,3,4,5)])

#Plotting the Multinominals
getPalette = colorRampPalette(brewer.pal(5, "RdBu"))
ggplot(toplot, aes(x=INCENTIVENFE, y=pp, fill=labels)) + 
  geom_bar( stat="identity", position = "dodge") +
  geom_errorbar( aes( ymin=lp[,2], ymax=up[,2]), width=0.2, position = position_dodge(0.9)) +
  scale_fill_manual(values = getPalette(5)) +
  theme_bw() +
  ggtitle("Incentives to participate in AE in Austria, 2016") +
  xlab("Incentives")

#Cross-tabs
crosstab(SUBSET05m_AUT_2016$INCENTIVENFE, SUBSET05m_AUT_2016$SKILL, plot=FALSE,prop.c = TRUE)
#Chi-Square test
chisq.test(SUBSET05m_AUT_2016$INCENTIVENFE, SUBSET05m_AUT_2016$SKILL, correct= F,)

##########Logistic Regression: Hypothesis 1################################
##########MODEL 1: Socio-demographics######################################
OUTPUT02 <-glm(NFE_FED~ GENDER+ALTER+MIGR+EDUC, data=SUBSET03_AUT_2016,family=binomial())
OR.vector02 <-exp(coef(OUTPUT02))
CI.vector02 <-exp(confint(OUTPUT02))
p.values02 <-list(summary(OUTPUT02)$coefficients[,4])

stargazer(OUTPUT02,
          coef= list(OR.vector02), ci = T,
          ci.custom= list(CI.vector02),
          single.row= F,
          type = "text",
          p=c(p.values02))

#######MODEL 2: Employment/skill-related variable##########################
OUTPUT03 <-glm(NFE_FED~ EMPL+SKILL+ORG+TIME, data=SUBSET03_AUT_2016,family=binomial())
OR.vector03 <-exp(coef(OUTPUT03))
CI.vector03 <-exp(confint(OUTPUT03))
p.values03 <-list(summary(OUTPUT03)$coefficients[,4])

stargazer(OUTPUT03,
          coef= list(OR.vector03), ci = T,
          ci.custom= list(CI.vector03),
          single.row= F,
          type = "text",
          p=c(p.values03)

#######MODEL3: ALL IVs#####################################################
OUTPUT04 <-glm(NFE_FED~ GENDER+ALTER+MIGR+EDUC+EMPL+SKILL+ORG+TIME, data=SUBSET03_AUT_2016,family=binomial())
OR.vector04 <-exp(coef(OUTPUT04))
CI.vector04 <-exp(confint(OUTPUT04))
p.values04 <-list(summary(OUTPUT04)$coefficients[,4])

stargazer(OUTPUT04,
          coef= list(OR.vector04), ci = T,
          ci.custom= list(CI.vector04),
          single.row= F,
          type = "text",
          p=c(p.values04))

#######BARPLOT HYPOTHESIS 1################################################
TABLE1 <- table(SUBSET03_AUT_2016$NFE_FED, SUBSET03_AUT_2016$SKILL)
TABLE2 <- prop.table(TABLE1,2)
TABLE3 <- as.data.frame(TABLE2)
ggplot(TABLE3, aes(fill=Var1, x=Var2, Var3, y=Freq)) + geom_bar(stat="identity") + ggtitle("Austria, 2016") + ylim(0,1) + ylab("%")+ xlab("") + scale_fill_manual(name = "Participated in adult education?", labels = c("No", "Yes"), values = c("#032F5E", "#B4C6E7") + theme_bw())

#######Hypothesis 3########################################################
#######MODEL1: Socio-demographics##########################################
OUTPUT06 <-glm(BARRIER2~ GENDER+ALTER+MIGR+EDUC, data=SUBSET05_AUT_2016,family=binomial())
OR.vector06 <-exp(coef(OUTPUT06))
CI.vector06 <-exp(confint(OUTPUT06))
p.values06 <-list(summary(OUTPUT06)$coefficients[,4])

stargazer(OUTPUT06,
          coef= list(OR.vector06), ci = T,
          ci.custom= list(CI.vector06),
          single.row= F,
          type = "text",
          p=c(p.values06))

#######MODEL2: Employment/skill-related variable###########################
OUTPUT07 <-glm(BARRIER2~ EMPL+SKILL+ORG+TIME, data=SUBSET05_AUT_2016,family=binomial())
OR.vector07 <-exp(coef(OUTPUT07))
CI.vector07 <-exp(confint(OUTPUT07))
p.values07 <-list(summary(OUTPUT07)$coefficients[,4])

stargazer(OUTPUT07,
          coef= list(OR.vector07), ci = T,
          ci.custom= list(CI.vector07),
          single.row= F,
          type = "text",
          p=c(p.values07))

#######MODEL3: ALL IVs#####################################################
OUTPUT08 <-glm(BARRIER2~ GENDER+ALTER+MIGR+EDUC+SKILL+ORG+TIME+NFE_FED, data=SUBSET05_AUT_2016,family=binomial())
OR.vector08 <-exp(coef(OUTPUT08))
CI.vector08 <-exp(confint(OUTPUT08))
p.values08 <-list(summary(OUTPUT08)$coefficients[,4])

stargazer(OUTPUT08,
          coef= list(OR.vector08), ci = T,
          ci.custom= list(CI.vector08),
          single.row= F,
          type = "text",
          p=c(p.values08))

#######COMBINE DATASETS FROM 2011 AND 2016#################################
#New Variable: YEAR 2011 
SUBSET04_AUT_2011$YEAR <- "2011"
SUBSET04_AUT_2011$YEAR <- factor(SUBSET04_AUT_2011$YEAR)

#Select variables & save data for 2011
SUBSET_AUT_2011 <-subset(SUBSET04_AUT_2011, select=c(NFE_FED, GENDER, ALTER, MIGR, EDUC, EMPL, SKILL, ORG, TIME, YEAR))
saveRDS(SUBSET_AUT_2011, file= "AES_2011_AUT_NEU.rds")

#New Variable: YEAR 2016
SUBSET03_AUT_2016$YEAR <- "2016"
SUBSET03_AUT_2016$YEAR <- factor(SUBSET03_AUT_2016$YEAR)

#Select variables & save data for 2016
SUBSET_AUT_2016 <-subset(SUBSET03_AUT_2016, select=c(NFE_FED, GENDER, ALTER, MIGR, EDUC, EMPL, SKILL, ORG, TIME, YEAR))
saveRDS(SUBSET_AUT_2016, file= "AES_2016_AUT_NEU.rds")

#Combining Data Sets of 2012 and 2016 for AUSTRIA with new variables
AES_2011_AUT <-readRDS("AES_2011_AUT_NEU.rds")
AES_2016_AUT <-readRDS("AES_2016_AUT_NEU.rds")
AES_2011_2016_AUT <-rbind(AES_2011_AUT,AES_2016_AUT)

#Set Reference Category to 2011
AES_2011_2016_AUT$YEAR <-relevel(AES_2011_2016_AUT$YEAR, ref="2011")

#######BARPLOT: PARTICIPATION RATE#########################################
TABLE1 <- table(AES_2011_2016_AUT$NFE_FED, AES_2011_2016_AUT$YEAR)
TABLE1
TABLE2 <- prop.table(TABLE1,2)
TABLE3 <- as.data.frame(TABLE2)
ggplot(TABLE3, aes(fill=Var1, x=Var2, Var3, y=Freq)) + geom_bar(stat="identity") +
  ggtitle("Austria, 2011/2016") + ylim(0,1) + ylab("%")+ xlab("") + 
  scale_fill_manual(name = "Participated in adult education?", labels = c("No", "Yes"), values = c("#032F5E", "#B4C6E7")) + 
  theme_bw()

freq(SUBSET04_AUT_2011$NFE_FED)
freq(SUBSET03_AUT_2016$NFE_FED)

#######BARPLOT: HYPOTHESIS 1###############################################
TABLE1 <- table(AES_2011_2016_AUT$NFE_FED, AES_2011_2016_AUT$SKILL)
TABLE1
TABLE2 <- prop.table(TABLE1,2)
TABLE3 <- as.data.frame(TABLE2)
ggplot(TABLE3, aes(fill=Var1, x=Var2, Var3, y=Freq)) + geom_bar(stat="identity") +
  ggtitle("Austria, 2011/2016") + ylim(0,1) + ylab("%")+ xlab("") + 
  scale_fill_manual(name = "Participated in adult education?", labels = c("No", "Yes"), values = c("#032F5E", "#B4C6E7")) + 
  theme_bw()

#######COMBINE DATASETS FROM 2012 AND 2016 for Hypothesis 2################
#New Variable: YEAR 2012  
SUBSET07_AUT_2011$YEAR <- "2011" 
SUBSET07_AUT_2011$YEAR <- factor(SUBSET07_AUT_2011$YEAR) 

#Select variables & save data FOR 2012 
SUBSET08_AUT_2011 <-subset(SUBSET07_AUT_2011, select=c(NFE_FED, GENDER, ALTER, MIGR, EDUC, EMPL, SKILL, ORG, TIME, YEAR, BARRIER2)) 
saveRDS(SUBSET08_AUT_2011, file= "AES_2011_AUT_01.rds") 

#New Variable: YEAR 2016 
SUBSET05_AUT_2016$YEAR <- "2016" 
SUBSET05_AUT_2016$YEAR <- factor(SUBSET05_AUT_2016$YEAR) 

#Select variables & save data FOR 2016 
SUBSET06_AUT_2016 <-subset(SUBSET05_AUT_2016, select=c(NFE_FED, GENDER, ALTER, MIGR, EDUC, EMPL, SKILL, ORG, TIME, YEAR, BARRIER2)) 
saveRDS(SUBSET06_AUT_2016, file= "AES_2016_AUT_01.rds") 

#Combining Data Sets of 2012 and 2016 for GERMANY with new variables 
AES_2011_AUT_01 <-readRDS("AES_2011_AUT_01.rds") 
AES_2016_AUT_01 <-readRDS("AES_2016_AUT_01.rds") 
AES_2011_2016_AUT_01 <-rbind(AES_2011_AUT_01,AES_2016_AUT_01) 

#######BARPLOT: HYPOTHESIS 3###############################################
TABLE1 <- table(AES_2011_2016_AUT_01$BARRIER2, AES_2011_2016_AUT_01$SKILL)
TABLE2 <- prop.table(TABLE1,2)
TABLE3 <- as.data.frame(TABLE2)
ggplot(TABLE3, aes(fill=Var1, x=Var2, Var3, y=Freq)) + 
  geom_bar(stat="identity") + ggtitle("Austria, 2011/2016") + ylim(0,1) + ylab("%")+ xlab("") + 
  scale_fill_manual(name = "Barriers on institutional or individual level?", labels = c("Institutional-level", "Individual-level"), values = c("#032F5E", "#B4C6E7")) + 
  theme_bw()

########BARPLOT: HYPOTHESIS 4##############################################
#COMBINE DATASETS FROM 2011 AND 2016
#New Variable: YEAR 2011 
SUBSET05_AUT_2011$YEAR <- "2011"
SUBSET05_AUT_2011$YEAR <- factor(SUBSET05_AUT_2011$YEAR)

#Select variables & save data FOR 2012
SUBSET_AUT_2011 <-subset(SUBSET05_AUT_2011, select=c(NFE_FED, GENDER, ALTER, MIGR, EDUC, EMPL, SKILL, ORG, TIME, YEAR, BARRIER))
saveRDS(SUBSET_AUT_2011, file= "AES_2011_AUT_NEU.rds")

#New Variable: YEAR 2016
SUBSET03a_AUT_2016$YEAR <- "2016"
SUBSET03a_AUT_2016$YEAR <- factor(SUBSET03a_AUT_2016$YEAR)

#Select variables & save data FOR 2016
SUBSET_AUT_2016 <-subset(SUBSET03a_AUT_2016, select=c(NFE_FED, GENDER, ALTER, MIGR, EDUC, EMPL, SKILL, ORG, TIME, YEAR, BARRIER))
saveRDS(SUBSET_AUT_2016, file= "AES_2016_AUT_NEU.rds")

#Combining Data Sets of 2012 and 2016 for AUSTRIA with new variables
AES_2011_AUT <-readRDS("AES_2011_AUT_NEU.rds")
AES_2016_AUT <-readRDS("AES_2016_AUT_NEU.rds")
AES_2011_2016_AUT <-rbind(AES_2011_AUT,AES_2016_AUT)

getPalette = colorRampPalette(brewer.pal(12, "RdBu"))

ggplot(AES_2011_2016_AUT, aes(x=YEAR, fill=BARRIER)) +
  geom_bar(stat="count", position="fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  ggtitle("Austria") +
  xlab("Year") +
  ylab("Percentage [%]") +
  scale_fill_manual(values = getPalette(12)) +
  theme_bw()

########COMBINE DATASETS FROM 2012 AND 2016 for Hypothesis 2###############
#New Variable: YEAR 2011 
SUBSET04m_AUT_2011$YEAR <- "2011" 
SUBSET04m_AUT_2011$YEAR <- factor(SUBSET04m_AUT_2011$YEAR) 

#Select variables & save data FOR 2012 
SUBSET08z_AUT_2011 <-subset(SUBSET04m_AUT_2011, select=c(NFE_FED, GENDER, ALTER, MIGR, EDUC, EMPL, SKILL, ORG, TIME, YEAR, INCENTIVENFE)) 
saveRDS(SUBSET08z_AUT_2011, file= "AES_2011_AUT_02.rds") 

#New Variable: YEAR 2016 
SUBSET05m_AUT_2016$YEAR <- "2016" 
SUBSET05m_AUT_2016$YEAR <- factor(SUBSET05m_AUT_2016$YEAR) 

#Select variables & save data FOR 2016 
SUBSET06z_AUT_2016 <-subset(SUBSET05m_AUT_2016, select=c(NFE_FED, GENDER, ALTER, MIGR, EDUC, EMPL, SKILL, ORG, TIME, YEAR, INCENTIVENFE)) 
saveRDS(SUBSET06z_AUT_2016, file= "AES_2016_AUT_02.rds") 

#Combining Data Sets of 2012 and 2016 for GERMANY with new variables 
AES_2011_AUT_02 <-readRDS("AES_2011_AUT_02.rds") 
AES_2016_AUT_02 <-readRDS("AES_2016_AUT_02.rds") 
AES_2011_2016_AUT_02 <-rbind(AES_2011_AUT_02,AES_2016_AUT_02) 

#Barplot
getPalette = colorRampPalette(brewer.pal(3, "Blues"))

ggplot(AES_2011_2016_AUT_02, aes(x=YEAR, fill=INCENTIVENFE)) +
  geom_bar(stat="count", position="fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  ggtitle("Austria") +
  xlab("Year") +
  ylab("Percentage [%]") +
  scale_fill_manual(values = getPalette(3), 
                    name="Incentives",
                    labels=c("both", "personal", "job-related")) +
  theme_bw()

