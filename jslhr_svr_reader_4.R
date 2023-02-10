#### Load and organize packages ####
Packages <- c("dplyr", "stats", "psych", "ggplot2", "lme4","lm.beta", "lmerTest","Jmisc","lavaan",
              "gridExtra","olsrr",'relaimpo','BayesFactor','MASS','psych','mice','VIM','psych',"rstatix","pastecs","sjmisc","bayestestR")

lapply(Packages, library, character.only = TRUE)

#### Organize ####

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Import combined dataset #
source("bf_function.R")
all<-read.csv("all_datasets.csv")
all$study<-as.factor(ifelse(grepl('READER', all$ID), 'reader', ifelse(grepl('READ', all$ID), 'READ', ifelse(grepl('ABCD',  all$ID), 'ABCD','none'))))
all2<-na.omit(all)

all2$DD<-as.factor(all2$DD) #define factor for dyslexia group
all2$study<-as.factor(all2$study) #define factor for age group

#### Analyze ####

##### Demographics #####

chisq.test(abcd$DD,abcd$Sex)
demo<-read.csv("demo_reader_jhlr.csv")
table(demo$DD,demo$rd_diag)


##### Do RC components differ based on Dys x Age? ######
# Test for DD x Grade interaction (Main analysis)
all2$grade<-as.factor(all2$grade)
fit4<-lm(RC~DD*grade*(WR+LC+RAN_Letters+Digits), data=all2)
fit5<-lm(RC~grade+DD*(WR+LC+RAN_Letters+Digits), data=all2)
lrtest(fit4,fit5)

anova(fit4)
m<-lmBF(RC ~ DD*grade*(WR+LC+RAN_Letters+Digits), data = all2, 
        progress=FALSE)

#now create seperate groups
df_1<-all2%>%filter(all2$grade==1)
df_2_read<-all2%>%dplyr::filter(grade=='2' & study=="READ")
df_3_4<-all2%>%filter(all2$grade==4|all2$grade==3)
df4<-all2%>%filter(all2$grade==4)
adult<-all2%>%filter(all2$grade=='adult')

# for exploratory analysis seperate 3rd and 4th graders
df_3<-all2%>%filter(all2$grade==3)
df_4<-all2%>%filter(all2$grade==4)

##### How do RC components predict RC seperatly by Grade and Dys? ######

### 1st Grade ####

# Create dys groups
df1_dys<-df_1%>%dplyr::filter(DD=="DD")%>%
  select(DD,RC,WR,RAN_Letters,LC,Digits)
df1_typ<-df_1%>%dplyr::filter(DD=="TYP")%>%
  select(DD,RC,WR,RAN_Letters,LC,Digits)

#Dys
fit_first_D<-lm(RC~WR+LC+RAN_Letters+Digits, data=df1_dys)
summary(fit_first_D)
out_3D<-tidy(fit_first_D)
knitr::kable(out_1D)
lm.beta(fit_first_D)
fit_first_D2 <- stepAIC(fit_first_D, direction = "both",steps = 1000)
fit_first_D2$anova
calc.relimp(fit_first_D, type = c("lmg"),
            rela = TRUE)
#Typ
fit_first_T<-lm(RC~WR+LC+RAN_Letters+Digits, data=df1_typ)
summary(fit_first_T)
out_3T<-tidy(fit_first_T)
knitr::kable(out_1T)
lm.beta(fit_first_T)
fit_first_T2 <- stepAIC(fit_first_T, direction = "both",steps = 1000)
fit_first_T2$anova
calc.relimp(fit_first_T, type = c("lmg"),
            rela = TRUE)

#2nd Grade

# Create dys groups
df2_dys<-df_2_read%>%dplyr::filter(DD=="DD")%>%
  select(DD,RC,WR,RAN_Letters,LC,Digits)
df2_typ<-df_2_read%>%dplyr::filter(DD=="TYP")%>%
  select(DD,RC,WR,RAN_Letters,LC,Digits)

#Dys
fit_second_D<-lm(RC~WR+LC+RAN_Letters+Digits, data=df2_dys)
summary(fit_second_D)
out_3D<-tidy(fit_second_D)
knitr::kable(out_2D)
lm.beta(fit_second_D)
fit_second_D2 <- stepAIC(fit_second_D, direction = "both",steps = 1000)
fit_second_D2$anova
calc.relimp(fit_second_D, type = c("lmg"),
            rela = TRUE)

#Typ
fit_second_T<-lm(RC~WR+LC+RAN_Letters+Digits, data=df2_typ)
summary(fit_second_T)
out_3T<-tidy(fit_second_T)
knitr::kable(out_2T)
lm.beta(fit_second_T)
fit_second_t2 <- stepAIC(fit_second_T, direction = "both",steps = 1000)
fit_second_t2$anova
calc.relimp(fit_second_T, type = c("lmg"),
            rela = TRUE)


# 3rd & 4th Grade

df3_dys<-df_3_4%>%dplyr::filter(DD=="DD")%>%
  select(grade,RC,WR,RAN_Letters,LC,Digits)
df3_typ<-df_3_4%>%dplyr::filter(DD=="TYP")%>%
  select(DD,RC,WR,RAN_Letters,LC,Digits)

write.csv(df3_dys,"/Users/olaozernov-palchik/Dropbox (MIT)/Annals_SVR/reviews/JASP/data_jasp/dc_345_N50.csv")
write.csv(df3_typ,"/Users/olaozernov-palchik/Dropbox (MIT)/Annals_SVR/reviews/JASP/data_jasp/tc_345_N32.csv")

#Dys
fit_third_D<-lm(RC~WR+LC+RAN_Letters+Digits, data=df3_dys)
summary(fit_third_D)
out_3D<-tidy(fit_third_D)
knitr::kable(out_3D)
lm.beta(fit_third_D)
fit_third_D2 <- stepAIC(fit_third_D, direction = "both",steps = 1000)
fit_third_D2$anova
calc.relimp(fit_third_D, type = c("lmg"),
            rela = TRUE)
#Typ
fit_third_T<-lm(RC~WR+LC+RAN_Letters+Digits, data=df3_typ)
summary(fit_third_T)
out_3T<-tidy(fit_third_T)
knitr::kable(out_3T)
lm.beta(fit_third_T)
fit_third_t <- stepAIC(fit_third_T, direction = "both",steps = 1000)
fit_third_t$anova
booteval.relimp(fit_third_T) # print result
calc.relimp(fit_third_T, type = c("lmg"),
            rela = TRUE)

# Adult

da_dys<-adult%>%filter(DD=="DD")%>%
  select(DD,RC,WR,RAN_Letters,LC,Digits)
da_typ<-adult%>%filter(DD=="TYP")%>%
  select(DD,RC,WR,RAN_Letters,LC,Digits)

fit_adult_D<-lm(RC~WR+LC+RAN_Letters+Digits, data=da_dys)
summary(fit_adult_D)
out<-tidy(fit_adult_D)
knitr::kable(out)
lm.beta(fit_adult_D)
fit_a_d <- stepAIC(fit_adult_D, direction = "both",steps = 1000)
fit_a_d$anova
#eta_squared(fit_a_d)

calc.relimp(fit_adult_D, type = c("lmg"),
            rela = TRUE)

#Typ

fit_adult_T<-lm(RC~WR+LC+RAN_Letters+Digits, data=da_typ)
anova(fit_adult_T)
out<-tidy(fit_adult_T)
knitr::kable(out)
lm.beta(fit_adult_T)
fit_a_t <- stepAIC(fit_adult_T, direction = "both",steps = 1000)
fit_a_t$anova
eta_squared(fit_a_t)

calc.relimp(fit_adult_T, type = c("lmg"),
            rela = TRUE)
source("~/Dropbox (MIT)/Annals_SVR/reviews/bf_function.R")

##Bayes
#1 Typ
b1_typ <- runRegOnAll(df1_typ) #make sure that there there is one column before RC 
write.csv(b1_typ,"/Users/olaozernov-palchik/Dropbox (MIT)/Annals_SVR/paper/jslhr/jslhr_bf_outputs/Gd1_typ115.csv")

#1 Dys
b1_dys <- runRegOnAll(df1_dys)
write.csv(b1_dys,"/Users/olaozernov-palchik/Dropbox (MIT)/Annals_SVR/paper/jslhr/jslhr_bf_outputs/Gd1_dys34.csv")

#2 Typ
b2_typ <- runRegOnAll(df2_typ) #make sure that there there is one column before RC 
write.csv(b2_typ,"/Users/olaozernov-palchik/Dropbox (MIT)/Annals_SVR/paper/jslhr/jslhr_bf_outputs/Gd2_typ.csv")

#2 Dys
b2_dys <- runRegOnAll(df2_dys)
write.csv(b2_dys,"/Users/olaozernov-palchik/Dropbox (MIT)/Annals_SVR/paper/jslhr/jslhr_bf_outputs/Gd2_dys.csv")

#3 Typ
b3_typ <- runRegOnAll(df3_typ) #make sure that there there is one column before RC 
write.csv(b3_typ,"/Users/olaozernov-palchik/Dropbox (MIT)/Annals_SVR/paper/jslhr/jslhr_bf_outputs/Gd3_4_typ.csv")

#3 Dys
b3_dys <- runRegOnAll(df3_dys)
write.csv(b3_dys,"/Users/olaozernov-palchik/Dropbox (MIT)/Annals_SVR/paper/jslhr/jslhr_bf_outputs/Gd3_4_dys.csv")

#4 Dys
b4_dys <- runRegOnAll(df4_dys)
write.csv(b4_dys,"/Users/olaozernov-palchik/Dropbox (MIT)/Annals_SVR/paper/jslhr/jslhr_bf_outputs/Gd4_dys.csv")

#4 Typ
b4_typ <- runRegOnAll(df4_typ)
write.csv(b4_typ,"/Users/olaozernov-palchik/Dropbox (MIT)/Annals_SVR/paper/jslhr/jslhr_bf_outputs/Gd4_typ.csv")

#Adult Typ
ba_typ <- runRegOnAll(da_typ) #make sure that there there is one column before RC 
write.csv(ba_typ,"/Users/olaozernov-palchik/Dropbox (MIT)/Annals_SVR/paper/jslhr/jslhr_bf_outputs/adult_typ.csv")

#Adult Dys
ba_dys <- runRegOnAll(da_dys)
write.csv(ba_dys,"/Users/olaozernov-palchik/Dropbox (MIT)/Annals_SVR/paper/jslhr/jslhr_bf_outputs/adult_dys.csv")


