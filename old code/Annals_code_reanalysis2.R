#### Load and organize packages ####
Packages <- c("dplyr", "stats", "psych", "ggplot2", "lme4","lm.beta", "lmerTest","Jmisc","lavaan",
              "gridExtra","olsrr",'relaimpo','BayesFactor','MASS','psych','mice','VIM','psych',"rstatix","pastecs","sjmisc","bayestestR")

lapply(Packages, library, character.only = TRUE)
setwd("~/Dropbox (MIT)/Annals_SVR") #Set working directory to correct file location

source("reviews/bf_function.R")

#### Adult organization ####

#Load files

abcd=read.csv("~/Dropbox (MIT)/Annals_SVR/ABCD.csv")
abcd_raw=read.csv('ABCD_raw.csv')
abcd=merge(abcd,abcd_raw)
#beh=read.csv("beh.csv")
#abcd<-merge(abcd,beh,'ID')
#Clean data

#Rename files

names(abcd)[names(abcd)=="wrmt_lc_ss_2"] <- "LC"
names(abcd)[names(abcd)=="gort_comp_ss_2"] <- "RC"
names(abcd)[names(abcd)=="wrmt_wa_ss_2"] <- "WA"
names(abcd)[names(abcd)=="ran_letters_ss_2"] <- "RAN_Letters"
names(abcd)[names(abcd)=="wrmt_id_ss_2"] <- "WID"
names(abcd)[names(abcd)=="towre_sw_ss_2"] <- "SWE"
names(abcd)[names(abcd)=="towre_pde_ss_2"] <- "PDE"
names(abcd)[names(abcd)=="ppvt_vocab_ss_2"] <- "Vocabulary"
names(abcd)[names(abcd)=="gort_ori_ss_2"] <- "ORI"
names(abcd)[names(abcd)=="ctopp_blending_ss_2"] <- "Blending"
names(abcd)[names(abcd)=="ctopp_elision_ss_2"] <- "Elision"
names(abcd)[names(abcd)=="ctopp_nonword_ss_2"] <- "Nonword"
names(abcd)[names(abcd)=="ran_2set_ss_2"] <- "RAN_2Set"
names(abcd)[names(abcd)=="ran_objects_ss_2"] <- "RAN_Objects"
names(abcd)[names(abcd)=="wais_dsb_ss_2"] <- "DigitsBackward"
names(abcd)[names(abcd)=="wais_dsf_ss_2"] <- "DigitsForward"
names(abcd)[names(abcd)=="kbit_ss_2"] <- "IQ"
#abcd$DigitsForward<-rescale(abcd$DigitsForward, to = c(40, 160))

abcd$SWE<-as.numeric(abcd$SWE)
abcd$PDE<-as.numeric(abcd$PDE)
abcd$WA<-as.numeric(abcd$WA)
abcd$WID<-as.numeric(abcd$WID)
abcd$DigitsForward=ifelse(abcd$DigitsForward==88,8,abcd$DigitsForward)#correct one typo

abcd$WR<-rowMeans(abcd[,c(37,39)],na.rm= TRUE)
#abcd$WR<-abcd$WA

#abcd$Cog<-rowMeans(abcd[,c(45,23)],na.rm= TRUE)

abcd$WR<-as.integer(abcd$WR)
## calculate dyslexia 
abcd$DD<-ifelse(abcd$WA < 90 & abcd$WID < 90 | abcd$WA < 90 & abcd$SWE < 90|
                  abcd$WA < 90 & abcd$PDE < 90 |
                  abcd$WID < 90 & abcd$SWE < 90 | abcd$WID <90 & abcd$PDE < 90 |
                  abcd$SWE < 90 & abcd$PDE < 90,1,
                ifelse (abcd$WA >= 90 & abcd$WID >= 90 & abcd$PDE >= 90 & abcd$SWE >= 90,0,NA))

# abcd$DD<-ifelse(abcd$WA < 81 & abcd$WID < 81 | abcd$WA < 81 & abcd$SWE < 81|
#                   abcd$WA < 81 & abcd$PDE < 81 |
#                   abcd$WID < 81 & abcd$SWE < 81 | abcd$WID <90 & abcd$PDE < 81 |
#                   abcd$SWE < 81 & abcd$PDE < 81,1,
#                 ifelse (abcd$WA >= 90 & abcd$WID >= 90 & abcd$PDE >= 90 & abcd$SWE >= 90,0,NA))

#abcd$DD2=ifelse(abcd$subgroup==1 & abcd$DD==0,0,ifelse(abcd$subgroup==2 & abcd$DD==1,1,"NA"))

#participation description
#abcd1<-na.omit(abcd%>%dplyr::select(Sex,Age,RC,WR,LC,RAN_Letters,DigitsForward))
#abcd_jasp<-na.omit(abcd%>%dplyr::select(DD,Sex,Age,RC,WR,LC,RAN_Letters,DigitsForward,DigitsBackward,Vocabulary,IQ,Nonword))
#abcd_jasp_dys<-abcd_jasp%>%filter(DD=='1')
#abcd_jasp_typ<-abcd_jasp%>%filter(DD=='0')

#write.csv(abcd_jasp,"andy_adult_jasp.csv")
#write.csv(abcd_jasp_dys,"andy_adult_jasp_dys.csv")
#write.csv(abcd_jasp_typ,"andy_adult_jasp_typ.csv")

####READ Y1 organization  ####

read_1=read.csv("~/Dropbox (MIT)/Annals_SVR/READ.csv",na.strings=c("NA","NaN", "9999","8888","999","7777"))
#read_1$WR<-rowMeans(read_1[,c(92,95)],na.rm= TRUE) #WA and WID

names(read_1)[names(read_1)=="CELFss_T3"] <- "LC"
names(read_1)[names(read_1)=="GORTCOMPss_T3"] <- "RC"
names(read_1)[names(read_1)=="RANLss_T3"] <- "RAN_Letters"
names(read_1)[names(read_1)=="W3WIss_T3"] <- "WID"
names(read_1)[names(read_1)=="W3WAss_T3"] <- "WA"
names(read_1)[names(read_1)=="TSWEss_T3"] <- "SWE"
names(read_1)[names(read_1)=="TPDEss_T3"] <- "PDE"
names(read_1)[names(read_1)=="GORTORIss_T3"] <- "ORI"
names(read_1)[names(read_1)=="GORTCOMPss_T3"] <- "RC"
names(read_1)[names(read_1)=="CTBWss_T3"] <- "Blending"
names(read_1)[names(read_1)=="CTELss_T3"] <- "Elision"
names(read_1)[names(read_1)=="CTNRss_T3"] <- "Nonword"
names(read_1)[names(read_1)=="CTMDss_T3"] <- "MemoryDigits"
names(read_1)[names(read_1)=="RAN2ss_T3"] <- "RAN_2Set"
names(read_1)[names(read_1)=="RANOss_T3"] <- "RAN_Objects"
names(read_1)[names(read_1)=="WISCss_T3"] <- "ProcSpeed"
names(read_1)[names(read_1)=="CELFss_T3"] <- "LC"
names(read_1)[names(read_1)=="TOCHCss_T3"] <- "Ortho"
names(read_1)[names(read_1)=="TWSss_T3"] <- "Spell"
read_1$T3_age_SPSS<-as.integer(read_1$T3_age_SPSS)
read_1$Age<-read_1$T3_age_SPSS/12
read_1$WR<-read_1$WA

read_1$DD=ifelse(read_1$W3WAss_T4 < 90 & read_1$W3WIss_T4 < 90 | read_1$W3WAss_T4 < 90 & read_1$TSWEss_T4 < 90|
                   read_1$W3WAss_T4 < 90 & read_1$TPDEss_T4 < 90 |
                   read_1$W3WIss_T4 < 90 & read_1$TSWEss_T4 < 90 | read_1$W3WIss_T4 <90 & read_1$TPDEss_T4 < 90 |
                   read_1$TSWEss_T4 < 90 & read_1$TPDEss_T4 < 90,"1", 
                 ifelse (read_1$W3WAss_T4 >= 90 & read_1$W3WIss_T4 >= 90 & read_1$W3WAss_T4 >= 90 & read_1$TSWEss_T4 >= 90,"0","NA"))

# read_1$DD=ifelse(read_1$W3WAss_T4 < 81 & read_1$W3WIss_T4 < 81 | read_1$W3WAss_T4 < 81 & read_1$TSWEss_T4 < 81|
#                    read_1$W3WAss_T4 < 81 & read_1$TPDEss_T4 < 81 |
#                    read_1$W3WIss_T4 < 81 & read_1$TSWEss_T4 < 81 | read_1$W3WIss_T4 <81 & read_1$TPDEss_T4 < 81 |
#                    read_1$TSWEss_T4 < 81 & read_1$TPDEss_T4 < 81,"1",
#                  ifelse (read_1$W3WAss_T4 >= 90 & read_1$W3WIss_T4 >= 90 & read_1$W3WAss_T4 >= 90 & read_1$TSWEss_T4 >= 90,"0","NA"))

read_1$DD<-as.factor(read_1$DD)

read_1_2<-na.omit(read_1%>%dplyr::select(Sex,RC,WR,RAN_Letters,LC,MemoryDigits))
read_1_2$Sex<-as.factor(read_1_2$Sex)

#### Organize READ 2nd Grade ####
read=read.csv("~/Dropbox (MIT)/Annals_SVR/READ.csv",na.strings=c("NA","NaN", "9999","8888","999","7777"))
#demo<-read.csv("demo_read.csv",na.strings=c("NA","NaN", "9999","8888","999","7777"))

read$DD=ifelse(read$W3WAss_T4 < 90 & read$W3WIss_T4 < 90 | read$W3WAss_T4 < 90 & read$TSWEss_T4 < 90|
                 read$W3WAss_T4 < 90 & read$TPDEss_T4 < 90 |
                 read$W3WIss_T4 < 90 & read$TSWEss_T4 < 90 | read$W3WIss_T4 <90 & read$TPDEss_T4 < 90 |
                 read$TSWEss_T4 < 90 & read$TPDEss_T4 < 90,"1", 
               ifelse (read$W3WAss_T4 >= 90 & read$W3WIss_T4 >= 90 & read$W3WAss_T4 >= 90 & read$TSWEss_T4 >= 90,"0","NA"))

# read$DD=ifelse(read$W3WAss_T4 < 81 & read$W3WIss_T4 < 81 | read$W3WAss_T4 < 81 & read$TSWEss_T4 < 81|
#                  read$W3WAss_T4 < 81 & read$TPDEss_T4 < 81 |
#                  read$W3WIss_T4 < 81 & read$TSWEss_T4 < 81 | read$W3WIss_T4 <81 & read$TPDEss_T4 < 81 |
#                  read$TSWEss_T4 < 81 & read$TPDEss_T4 < 81,"1", 
#                ifelse (read$W3WAss_T4 >= 90 & read$W3WIss_T4 >= 90 & read$W3WAss_T4 >= 90 & read$TSWEss_T4 >= 90,"0","NA"))
# 


read$Sex=ifelse(read$Sex=="Male",1,2)

read$T4mos<-as.integer(read$T4mos)
read$Age<-read$T4mos/12


#read2<-read%>%dplyr::select(ID,Age,DD,Group,RC,Sex,
#                            RANL,WA,LC,'IQ'


names(read)[names(read)=="CELF5ss_T4"] <- "LC"
names(read)[names(read)=="KBITss_T4"] <- "IQ"
names(read)[names(read)=="GORTACOMPss_T4"] <- "RC"
names(read)[names(read)=="RANLss_T4"] <- "RAN_Letters"
names(read)[names(read)=="W3WIss_T4"] <- "WID"
names(read)[names(read)=="W3WAss_T4"] <- "WA"
names(read)[names(read)=="TSWEss_T4"] <- "SWE"
names(read)[names(read)=="TPDEss_T4"] <- "PDE"
names(read)[names(read)=="GORTORIss_T4"] <- "ORI"
names(read)[names(read)=="GORTCOMPss_T4"] <- "RC"
names(read)[names(read)=="CTBWss_T4"] <- "Blending"
names(read)[names(read)=="CTELss_T4"] <- "Elision"
names(read)[names(read)=="CTNRss_T4"] <- "Nonword"
names(read)[names(read)=="CTMDss_T4"] <- "MemoryDigits"
names(read)[names(read)=="RAN2ss_T4"] <- "RAN_2Set"
names(read)[names(read)=="RANOss_T4"] <- "RAN_Objects"
names(read)[names(read)=="WISCBss_T4"] <- "ProcSpeed"
names(read)[names(read)=="CELFss_T4"] <- "LC"
names(read)[names(read)=="TOCHCss_T4"] <- "Ortho"
names(read)[names(read)=="TWSss_T4"] <- "Spell"
names(read)[names(read)=="PPVTss_T4"] <- "Vocabulary"

#read$WR<-rowMeans(read[,c(186,189)],na.rm= TRUE)
read$WR<-read$WA
read_2_2<-na.omit(read%>%dplyr::select(Sex,RC,WR,RAN_Letters,LC,MemoryDigits))
#read_2_2<-read_2_2%>%filter(DD==1 | DD==0)

#### Correlations ####
source("~/Dropbox (MIT)/GitHub/speech_specific_deficit_paper/corstars_function.R")

#Adults
abcd2<-na.omit(abcd%>%dplyr::select(RC,WR,LC,RAN_Letters,DigitsForward))
cor_a<-corstars(abcd2,method="pearson")
#write.csv(cor_a,'cor_a.csv')
correlationBF(abcd2$RC, abcd2$WR)
correlationBF(abcd2$RC, abcd2$LC)
correlationBF(abcd2$RC, abcd2$RAN_Letters)
correlationBF(abcd2$RC, abcd2$DigitsForward)
correlationBF(abcd2$WR, abcd2$LC)
correlationBF(abcd2$WR, abcd2$RAN_Letters)
correlationBF(abcd2$WR, abcd2$DigitsForward)
correlationBF(abcd2$LC, abcd2$RAN_Letters)
correlationBF(abcd2$LC, abcd2$DigitsForward)
correlationBF(abcd2$RAN_Letters, abcd2$DigitsForward)

#1st Grade
read_1_22<-read_1%>%dplyr::select(RC,WR,RAN_Letters,LC,MemoryDigits)
cor_1<-corstars(read_1_22,method="pearson")
#write.csv(cor_1,'cor_1.csv')
correlationBF(read_1_22$RC, read_1_22$WR)
correlationBF(read_1_22$RC, read_1_22$RAN_Letters)
correlationBF(read_1_22$RC, read_1_22$LC)
correlationBF(read_1_22$RC, read_1_22$MemoryDigits)
correlationBF(read_1_22$WR, read_1_22$RAN_Letters)
correlationBF(read_1_22$WR, read_1_22$LC)
correlationBF(read_1_22$WR, read_1_22$MemoryDigits)
correlationBF(read_1_22$LC, read_1_22$RAN_Letters)
correlationBF(read_1_22$LC, read_1_22$MemoryDigits)
correlationBF(read_1_22$RAN_Letters, read_1_22$MemoryDigits)

#2nd Grade
read_2_22<-read%>%dplyr::select(RC,WR,RAN_Letters,LC,MemoryDigits)
cor_2<-corstars(read_2_22,method="pearson")
#write.csv(cor_2,'cor_2.csv')
correlationBF(read_2_22$RC, read_2_22$WR)
correlationBF(read_2_22$RC, read_2_22$RAN_Letters)
correlationBF(read_2_22$RC, read_2_22$LC)
correlationBF(read_2_22$RC, read_2_22$MemoryDigits)
correlationBF(read_2_22$WR, read_2_22$RAN_Letters)
correlationBF(read_2_22$WR, read_2_22$LC)
correlationBF(read_2_22$WR, read_2_22$MemoryDigits)
correlationBF(read_2_22$LC, read_2_22$RAN_Letters)
correlationBF(read_2_22$LC, read_2_22$MemoryDigits)
correlationBF(read_2_22$RAN_Letters, read_2_22$MemoryDigits)


#### Descriptives ####

adult_table<-abcd%>%ungroup%>%dplyr::select(DD,IQ,WID,WA,SWE,PDE,
                                            Vocabulary,ORI,Blending,Elision,RC, ORI,Nonword,RAN_Letters,RAN_Objects,
                                            LC,RAN_2Set,DigitsForward,DigitsBackward)
abcd_b<-na.omit(adult_table)
#table(abcd$DD,abcd$Sex)
chisq.test(abcd$DD,abcd$Sex)


# Make table 
library(arsenal)
at<-summary(tableby(DD ~ ., data = adult_table,
                    control=tableby.control(numeric.stats="meansd", total=FALSE)),title = "Adult Descriptives",text=TRUE,digits=2, digits.p=3)
#arsenal::write2word(at, "adult_descriptives_new.doc", title="Adult Descriptives")



#### Reading History ####

read_rd<-read.csv("read_dys_diag.csv")
read_t<-na.omit(read%>%dplyr::select(ID,DD,RC,WR,RAN_Letters,LC,MemoryDigits))

read_rd<-merge(read_t,read_rd,"ID")

table(read_rd$TeacherSugg,read_rd$DD)

#### Adult regressions ####

abcd$RC<-as.integer(abcd$RC)

abcd2<-na.omit(abcd%>%dplyr::select(RC,WR,LC,RAN_Letters,DigitsForward))
abcd2$DD<-NULL
fit_adult<-lm(RC~WR+LC+RAN_Letters+DigitsForward, data=abcd2)
anova(fit_adult)

#abcd2<-na.omit(abcd%>%dplyr::select(RC,WR,LC,RAN_Letters,DigitsForward))
fit_adult<-lm(RC~WR*(LC+RAN_Letters+DigitsForward), data=abcd2)
fit_adult2<-lm(RC~WR+LC+RAN_Letters+DigitsForward, data=abcd2)

anova(fit_adult,fit_adult2)
library(lmtest)

lrtest(fit_adult,fit_adult2)
anovaBF(fit_adult,fit_adult2)

out<-tidy(fit_adult)
knitr::kable(out)
lm.beta(fit_adult)
fit_a <- stepAIC(fit_adult, direction = "both",steps = 1000)
fit_a$anova
eta_squared(fit_a)

calc.relimp(fit_adult, type = c("lmg"),
            rela = TRUE)
boot_a<-boot.relimp(
  fit_adult,
  b = 1000,
  typesel = c("lmg"),
  rank = TRUE,
  diff = TRUE,
  rela = TRUE
)

booteval.relimp(boot_a) # print result
results_a <- (booteval.relimp(boot_a, sort = TRUE)) # plot result
plot(results_a, level = 0.8, names.abbrev = 10,cex=1.5,
     main = "Adult Relative Importance")

### Bayesian Regression ###
tab <- runRegOnAll(abcd2)
#write.csv(tab,"reviews/data/adult_all_N62.csv")
#write.csv(abcd2,'jasp_abcd2.csv')

####1st grade Regressions ####
#write.csv(read_1_2,"jasp_read1.csv")
#write.csv(read_2_2,"jasp_read2.csv")

#fit_child1<-lm(RC~DD*(WR+LC+RAN_Letters+MemoryDigits), data=read_1_2)
#fit_chil1_2<-lm(DD+RC~WR+LC+RAN_Letters+MemoryDigits, data=read_1_2)

library(lmtest)

lrtest(fit_child1,fit_chil1_2)

fit_1<-lm(RC~WR+LC+RAN_Letters+MemoryDigits, data=read_1_2)
out<-tidy(fit_1)
knitr::kable(out)
summary(fit_1)
eta_squared(fit_1)
lm.beta::lm.beta(fit_1) #estimates

fit_1_2 <- stepAIC(fit_1, direction = "both",steps = 1000)
fit_1_2$anova
calc.relimp(fit_1, type = c("lmg"),
            rela = TRUE)
boot_1<-boot.relimp(
  fit_1,
  b = 1000,
  typesel = c("lmg"),
  rank = TRUE,
  diff = TRUE,
  rela = TRUE
)

booteval.relimp(boot_1) # print result


### Bayesian Regression 
tab_1 <- runRegOnAll(read_1_2)
#write.csv(tab,"reviews/data/1st_all_N150.csv")

###2nd Grade Regression ####

read_2_3=na.omit(read_2_2$DD)

fit_child2<-lm(RC~DD*(WR+LC+RAN_Letters+MemoryDigits), data=read_2_3)
fit_child2_2<-lm(DD+RC~WR+LC+RAN_Letters+MemoryDigits, data=read_2_3)

library(lmtest)

lrtest(fit_child2,fit_child2_2)

fit_2<-lm(RC~WR+LC+RAN_Letters+MemoryDigits, data=read_2_2)
summary(fit_2)
out2<-tidy(fit_2)
knitr::kable(out2)
summary(fit_2)
lm.beta::lm.beta(fit_2) #estimates

#fit_2_2 <- stepAIC(fit_2, direction = "both",steps = 1000)
#fit_2_2$anova
calc.relimp(fit_2, type = c("lmg"),
            rela = TRUE)
boot_2<-boot.relimp(
  fit_2,
  b = 1000,
  typesel = c("lmg"),
  rank = TRUE,
  diff = TRUE,
  rela = TRUE
)

booteval.relimp(boot_2) # print result
results_1 <- (booteval.relimp(boot_1, sort = TRUE)) # plot result

### Bayesian Regression 
tab_2 <- runRegOnAll(read_2_2)
#write.csv(tab,"reviews/data/2nd_all_N155.csv")

#### By Group ####

# Define groups

abcd2<-na.omit(abcd%>%dplyr::select(Sex,DD,RC,WR,RAN_Letters,LC,DigitsForward))
abcd2<-na.omit(abcd2%>%filter(DD==1|DD==0))

da_dys<-abcd2%>%filter(DD==1)%>%
  select(DD,RC,WR,RAN_Letters,LC,DigitsForward)
da_typ<-abcd2%>%filter(DD==0)%>%
  select(DD,RC,WR,RAN_Letters,LC,DigitsForward)


read_1<-read_1%>%filter(DD==1 | DD==0)
read_1_2<-na.omit(read_1%>%dplyr::select(DD,RC,WR,RAN_Letters,LC,MemoryDigits))

dc_dys_1<-read_1_2%>%filter(DD==1)
dc_typ_1<-read_1_2%>%filter(DD==0)

read_2_2<-na.omit(read%>%dplyr::select(DD,RC,WR,RAN_Letters,LC,MemoryDigits))
dc_dys_2<-read_2_2%>%filter(DD==1)
dc_typ_2<-read_2_2%>%filter(DD==0)


#### Correlation by group ####
#Adults
da_dys$DD<-NULL
da_typ$DD<-NULL
cor_ad<-corstars(da_dys,method="pearson")
cor_at<-corstars(da_typ,method="pearson")
#write.csv(cor_ad,'cor_ad.csv')
#write.csv(cor_at,'cor_at.csv')
correlationBF(da_dys$WR, da_dys$RC)

#1st Grade
dc_dys_1$DD<-NULL
dc_typ_1$DD<-NULL
cor_1d<-corstars(dc_dys_1,method="pearson")
cor_1t<-corstars(dc_typ_1,method="pearson")
#write.csv(cor_1d,'cor_1d.csv')
#write.csv(cor_1t,'cor_1t.csv')
correlationBF(dc_typ_1$MemoryDigits, dc_typ_1$LC)
correlationBF(dc_dys_1$LC, dc_dys_1$MemoryDigits)


#2nd Grade
dc_dys_2$DD<-NULL
dc_typ_2$DD<-NULL
cor_2d<-corstars(dc_dys_2,method="pearson")
cor_2t<-corstars(dc_typ_2,method="pearson")
#write.csv(cor_2d,'cor_2d.csv')
#write.csv(cor_2t,'cor_2t.csv')
correlationBF(dc_typ_2$MemoryDigits, dc_typ_2$LC)
correlationBF(dc_dys_2$LC, dc_dys_2$MemoryDigits)

#### Regressions by group ####

#### Adult reggression by group
fit_adult_D<-lm(RC~WR+LC+RAN_Letters+DigitsForward, data=da_dys)
summary(fit_adult_D)
out<-tidy(fit_adult_D)
knitr::kable(out)
lm.beta(fit_adult_D)
fit_a_d <- stepAIC(fit_adult_D, direction = "both",steps = 1000)
fit_a_d$anova
#eta_squared(fit_a_d)
calc.relimp(fit_adult_D, type = c("lmg"),
            rela = TRUE)
fit_a_d2<-boot.relimp(
  fit_adult_D,
  b = 1000,
  typesel = c("lmg"),
  rank = TRUE,
  diff = TRUE,
  rela = TRUE
)

booteval.relimp(fit_a_d2) # print result
results_a_d2<- (booteval.relimp(fit_a_d, sort = TRUE)) # plot result
plot(results_a_d2, level = 0.8, names.abbrev = 10,cex=1.5,
     main = "Dys Adult Relative Importance")


#Typ

fit_adult_T<-lm(RC~WR+LC+RAN_Letters+DigitsForward, data=da_typ)
anova(fit_adult_T)
out<-tidy(fit_adult_T)
knitr::kable(out)
lm.beta(fit_adult_T)
fit_a_t <- stepAIC(fit_adult_T, direction = "both",steps = 1000)
fit_a_t$anova
eta_squared(fit_a_t)
calc.relimp(fit_adult_T, type = c("lmg"),
            rela = TRUE)
fit_a_t2<-boot.relimp(
  fit_adult_T,
  b = 1000,
  typesel = c("lmg"),
  rank = TRUE,
  diff = TRUE,
  rela = TRUE
)

booteval.relimp(fit_a_t2) # print result

###Bayesian

#Typ
tab_typ <- runRegOnAll(da_typ) #make sure that there there is one column before RC 
#write.csv(tab_typ,"reviews/data/adult_typ_N31.csv")

#Dys
tab_da <- runRegOnAll(da_dys)
#write.csv(tab,"reviews/data/adult_dys_N26_122721.csv")


#### 1st Grade regression by group ####

#1st Dys
fit_1st_D<-lm(RC~WR+LC+RAN_Letters+MemoryDigits, data=dc_dys_1)
anova(fit_1st_D)
out<-tidy(fit_1st_D)
knitr::kable(out)
lm.beta(fit_1st_D)
fit_1_d <- stepAIC(fit_1st_D, direction = "both",steps = 1000)
fit_1_d$anova
calc.relimp(fit_1st_D, type = c("lmg"),
            rela = TRUE)
fit_1_d_2<-boot.relimp(
  fit_1st_D,
  b = 1000,
  typesel = c("lmg"),
  rank = TRUE,
  diff = TRUE,
  rela = TRUE
)

booteval.relimp(fit_1_d_2) # print result

#1st Typ

fit_1st_T<-lm(RC~WR+LC+RAN_Letters+MemoryDigits, data=dc_typ_1)
anova(fit_1st_T)
out<-tidy(fit_1st_T)
knitr::kable(out)
lm.beta(fit_1st_T)
fit_1_t <- stepAIC(fit_1st_T, direction = "both",steps = 1000)
fit_1_t$anova
eta_squared(fit_1_t)
calc.relimp(fit_1st_T, type = c("lmg"),
            rela = TRUE)
fit_1_t2<-boot.relimp(
  fit_1st_T,
  b = 1000,
  typesel = c("lmg"),
  rank = TRUE,
  diff = TRUE,
  rela = TRUE
)

booteval.relimp(fit_1_t2) # print result

### Bayesian Regression 
dc_dys_1<-read_1_2%>%filter(DD==1)%>%
  select(DD,RC,WR,RAN_Letters,LC,MemoryDigits)

dc_typ_1<-read_1_2%>%filter(DD==0)%>%
  select(DD,RC,WR,RAN_Letters,LC,MemoryDigits)

tab_dc1 <- runRegOnAll(dc_dys_1)
#write.csv(tab_dc1,"reviews/data/1st_dys_N35_122721.csv")

tab_dt1 <- runRegOnAll(dc_typ_1)
#write.csv(tab_dt1,"reviews/data/1st_typ_N115_122721.csv")


###2nd grade regression  by Group ####

# Dys
fit_2nd_D<-lm(RC~WR+LC+RAN_Letters+MemoryDigits, data=dc_dys_2)
anova(fit_2nd_D)
out<-tidy(fit_2nd_D)
knitr::kable(out)
lm.beta(fit_2nd_D)
#fit_2_d <- stepAIC(fit_2nd_D, direction = "both",steps = 1000)
#fit_2_d$anova
calc.relimp(fit_2nd_D, type = c("lmg"),
            rela = TRUE)
fit_2_d_2<-boot.relimp(
  fit_2nd_D,
  b = 1000,
  typesel = c("lmg"),
  rank = TRUE,
  diff = TRUE,
  rela = TRUE
)

booteval.relimp(fit_2_d_2) # print result

# Typ

fit_2nd_T<-lm(RC~WR+LC+RAN_Letters+MemoryDigits, data=dc_typ_2)
summary(fit_2nd_T)
#eta_squared(fit_2nd_T)
out<-tidy(fit_2nd_T)
knitr::kable(out)
lm.beta(fit_2nd_T)
fit_2_t <- stepAIC(fit_2nd_T, direction = "both",steps = 1000)
fit_2_t$anova
calc.relimp(fit_2nd_T, type = c("lmg"),
            rela = TRUE)
fit_2_t2<-boot.relimp(
  fit_2nd_T,
  b = 1000,
  typesel = c("lmg"),
  rank = TRUE,
  diff = TRUE,
  rela = TRUE
)

booteval.relimp(fit_2_t2) # print result



### Bayesian Regression 
dc_dys_2<-read_2_2%>%filter(DD==1)%>%
  select(DD,RC,WR,RAN_Letters,LC,MemoryDigits)

dc_typ_2<-read_2_2%>%filter(DD==0)%>%
  select(DD,RC,WR,RAN_Letters,LC,MemoryDigits)

tab_dc2 <- runRegOnAll(dc_dys_2)
#write.csv(tab_dc2,"reviews/data/2nd_dys_N34_122721.csv")

tab_tc_2 <- runRegOnAll(dc_typ_2)
#write.csv(tab_tc_2,"reviews/data/2nd_typ_N111_122721.csv")


#typ
m1_ct2<-lmBF(RC~WR+RAN_Letters+LC+MemoryDigits, data=dc_typ_2)
m2_ct2<-lmBF(RC~RAN_Letters+LC+MemoryDigits, data=dc_typ_2)
m3_ct2<-lmBF(RC~WR+LC+MemoryDigits, data=dc_typ_2)
m4_ct2<-lmBF(RC~WR+RAN_Letters+LC, data=dc_typ_2)
m5_ct2<-lmBF(RC~WR+LC, data=dc_typ_2)
m6_ct2<-lmBF(RC~WR, data=dc_typ_2)
m7_ct2<-lmBF(RC~LC, data=dc_typ_2)
m8_ct2<-lmBF(RC~WR+RAN_Letters, data=dc_typ_2)

#dys
m1_cd2<-lmBF(RC~WR+RAN_Letters+LC+MemoryDigits, data=dc_dys_2)
m2_cd2<-lmBF(RC~RAN_Letters+LC+MemoryDigits, data=dc_dys_2)
m3_cd2<-lmBF(RC~WR+LC+MemoryDigits, data=dc_dys_2)
m4_cd2<-lmBF(RC~WR+RAN_Letters+LC, data=dc_dys_2)
m5_cd2<-lmBF(RC~WR+LC, data=dc_dys_2)
m6_cd2<-lmBF(RC~WR, data=dc_dys_2)
m7_cd2<-lmBF(RC~LC, data=dc_dys_2)
m8_cd2<-lmBF(RC~WR+RAN_Letters, data=dc_dys_2)

#### Plot rank of top models ####
d_all<-read.csv("plot_all_top.csv")
d_all$BF<-log10(d_all$Value)

d_group<-read.csv("/Users/olaozernov-palchik/Dropbox (MIT)/Annals_SVR/top_group.csv")
d_group$BF<-log10(d_group$adj)
hist(d_group$BF)

d_group$DD<-as.factor(d_group$DD)

d_all$Age <- factor(d_all$Age ,
                    levels = c('Adult','First','Second'),
                    labels = c("Adults", "First Grade", "Second Grade"))

theme_set(theme_bw())

p1<-ggplot(data=d_all, aes(x = BF, y = Group,color=Age)) + geom_point(size=5)+ 
  scale_y_discrete(limits=d_all$Group)+ scale_colour_grey() +
  theme(
    axis.title = element_text(family = "Times New Roman", size = 20),
    legend.position = "none",
    axis.text.x = element_text(size = 15),
    axis.title.y=element_blank(),
    axis.text.y = element_text(size = 15))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  xlab("Bayes factor (Top Model)") 

p2<-ggplot(data=d_group, aes(x = BF, y = Group,shape=DD)) + geom_point(size=5)+ 
  scale_y_discrete(limits=d_group$Group)+
  theme(
    axis.title = element_text(family = "Times New Roman", size = 20),
    legend.key.size = unit(1.5, "cm"),
    legend.text = element_text(
      size = 15,
      face = 'bold'
    ),
    legend.title = element_blank(),
    axis.title.y=element_blank(),
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15)
  )+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.position = "none",
             panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  xlab("Bayes factor (Top Model)") 

grid.arrange(p1,p2)

### Variance comparison
library(car)

leveneTest(WR ~ DD, data = abcd)
leveneTest(LC ~ DD, data = abcd)
leveneTest(WR ~ DD, data = abcd)

#how many diagnosed
diag<-read.csv("read_dys_diag.csv")
read2<-merge(diag, read)
table(read2$DD, read2$Diagnosis)
table(read2$DD, read2$TeacherSugg)
table(read2$DD, read2$Intervention)

#history

#child
d_imp<-read.csv("READ_data_final_imputed_051018.csv")
d_imp<-d_imp%>%dplyr::select(READ,Mom.ARHQ.score..percentile.,Dad.ARHQ.score..percentile.)
d_imp$ID<-paste("READ_",d_imp$READ,sep="")
read3<-merge(d_imp,read_rd)

names(read3)[names(read3) == 'Dad.ARHQ.score..percentile.'] <- 'Dad.ARHQ'
names(read3)[names(read3) == 'Mom.ARHQ.score..percentile.'] <- 'Mom.ARHQ'

#read3$FHD_mom=ifelse(read3$Mom.ARHQ>0.3,1,0)
#read3$FHD_dad=ifelse(read3$Dad.ARHQ>0.3,1,0)
read3$FHD=ifelse(read3$Dad.ARHQ>0.4|read3$Mom.ARHQ>0.4,1,0)

mytable<-table(read3$DD, read3$FHD)
prop.table(mytable,1)

#adult

a_demo<-read.csv("abcd_demo.csv")
a_demo<-merge(a_demo,abcd,all = TRUE)
mytable<-table(a_demo$DD, a_demo$read_delay)
prop.table(mytable,1)

#### READER ####

setwd("/Users/olaozernov-palchik/Dropbox (MIT)/Annals_SVR/paper/jslhr")
rdr<-read.csv("Annals_READER_dataset2.csv")
grades<-read.csv("READER_grades.csv")
rdr<-merge(rdr,grades)
names(rdr)[names(rdr)=="celf_usp_std_behav1"] <- "LC"
names(rdr)[names(rdr)=="gort_comp_std_behav1"] <- "RC"
names(rdr)[names(rdr)=="wrmt_wa_std_behav1"] <- "WA"
names(rdr)[names(rdr)=="ran_let_std_behav1"] <- "RAN_Letters"
names(rdr)[names(rdr)=="wrmt_id_std_behav1"] <- "WID"
names(rdr)[names(rdr)=="towre_sight_std_behav1"] <- "SWE"
names(rdr)[names(rdr)=="towre_phon_std_behav1"] <- "PDE"
names(rdr)[names(rdr)=="wisc_digitfwd_std_behav1"] <- "DigitsForward"

rdr$SWE<-as.numeric(rdr$SWE)

rdr$WR<-rowMeans(rdr[,c(17,19)],na.rm= TRUE)

rdr$DD=ifelse(rdr$WA < 90 & rdr$WID < 90 | rdr$WA < 90 & rdr$SWE < 90|
                rdr$WA < 90 & rdr$PDE < 90 |
                rdr$WID < 90 & rdr$SWE < 90 | rdr$WID <90 & rdr$PDE < 90 |
                rdr$SWE < 90 & rdr$PDE < 90,"DD", 
              ifelse (rdr$WA >= 90 & rdr$WID >= 90 & rdr$WA >= 90 & rdr$SWE >= 90,"TYP","NA"))

rdr2<-rdr%>%filter(rdr$DD!='NA')

rdr_all_gd3<-rdr%>%filter(rdr$grade_at_testing==3)
rdr_all_gd4<-rdr%>%filter(rdr$grade_at_testing==4)


#Grade 3
fit_rdr1<-lm(RC~DD*(WR+LC+RAN_Letters+DigitsForward), data=rdr_gd3)
fit_rdr2<-lm(RC~DD+WR+LC+RAN_Letters+DigitsForward, data=rdr_gd3)

anova(fit_rdr1,fit_rdr2)
library(lmtest)
lrtest(fit_rdr1,fit_rdr2)
anovaBF(fit_rdr1,fit_rdr2)

#Grade 4
fit_rdr3<-lm(RC~DD*(WR+LC+RAN_Letters+DigitsForward), data=df_gd4)
fit_rdr4<-lm(RC~DD+WR+LC+RAN_Letters+DigitsForward, data=df_gd4)

anova(fit_rdr3,fit_rdr4)
library(lmtest)
lrtest(fit_rdr3,fit_rdr4)
anovaBF(fit_rdr,fit_rdr2)

#SVR models

#All
rdr_3_all<-lm(RC~WR+LC+RAN_Letters+DigitsForward, data=df_all_gd3)
summary(rdr_3_all)
out_rdr3<-tidy(rdr_3_all)
knitr::kable(out_rdr3)
lm.beta(rdr_3)
fit_rdr3 <- stepAIC(rdr_3, direction = "both",steps = 1000)
fit_rdr3$anova
eta_squared(fit_rdr3)

# Grade 3
##Dys
df_dys_gd3<-df_gd3%>%filter(DD=='DD')
rdr_3_dys<-lm(RC~WR+LC+RAN_Letters+DigitsForward, data=df_dys_gd3)
summary(rdr_3_dys)


##Typ
df_typ_gd3<-df_gd3%>%filter(DD=='TYP')
rdr_3_typ<-lm(RC~WR+LC+RAN_Letters+DigitsForward, data=df_typ_gd3)
summary(rdr_3_typ)

# Grade 4
##Dys
df_dys_gd4<-df_gd4%>%filter(DD=='DD')
rdr_4_dys<-lm(RC~WR+LC+RAN_Letters+DigitsForward, data=df_dys_gd4)
summary(rdr_4_dys)

##Typ
df_typ_gd4<-df_gd4%>%filter(DD=='TYP')
rdr_4_typ<-lm(RC~WR+LC+RAN_Letters+DigitsForward, data=df_typ_gd4)
summary(rdr_4_typ)

