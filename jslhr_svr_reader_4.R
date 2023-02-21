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

all$DD<-as.factor(all$DD) #define factor for dyslexia group
all$study<-as.factor(all$study) #define factor for age group
all<-all%>%filter(all$grade!=5)
table(all$study,all$DD)


#### Analyze ####

##### Demographic tables######


##### Do RC components differ based on Dys x Age? ######

# Test for DD interaction (Main analysis)
fit1<-lm(RC~grade+DD*(WR+LC+RAN_Letters+Digits), data=all)
fit2<-lm(RC~grade+DD+(WR+LC+RAN_Letters+Digits), data=all)
lmtest::lrtest(fit1,fit2)

# Test for DD x Grade interaction (Main analysis)
all$grade<-as.factor(all2$grade)

fit4<-lm(RC~DD*grade*(WR+LC+RAN_Letters+Digits), data=all)
fit5<-lm(RC~grade+DD*(WR+LC+RAN_Letters+Digits), data=all)
lmtest::lrtest(fit4,fit5)

anova(fit4)
m<-lmBF(RC ~ DD*grade*(WR+LC+RAN_Letters+Digits), data = all2, 
        progress=FALSE)

#now create seperate groups
df_1<-all%>%filter(all$grade==1)
df_2_read<-all%>%dplyr::filter(grade=='2' & study=="READ")
df_3_4<-all%>%filter(all$grade==4|all$grade==3)
adult<-all%>%filter(all$grade=='adult')

# for exploratory analysis seperate 3rd and 4th graders
df_3<-all2%>%filter(all2$grade==3)
df_4<-all2%>%filter(all2$grade==4)

#### Create summaries by group ####
df<-all2
# Create summary table with means and SD
summary_table <- aggregate(df[,c("RC", "WR", "RAN_Letters", "LC", "Digits")], 
                           by = list(DD = df$DD, grade = df$grade), 
                           FUN = function(x) c(mean = mean(x), sd = sd(x)))

# Rename columns
colnames(summary_table) <- c("DD", "grade", "RC_mean", "RC_sd", "WR_mean", "WR_sd",
                             "RAN_Letters_mean", "RAN_Letters_sd", "LC_mean", "LC_sd",
                             "Digits_mean", "Digits_sd")

# Print summary table
summary_table

#### Create correlations by group ####
# Select columns 4 to 8 and compute the correlation matrix
cor_matrix <- cor(df[, 5:9])

# Print the correlation matrix
print(cor_matrix)

# Assuming your data frame is named "my_data"
# Select columns 4 to 8 and compute the correlation matrix
cor_matrix <- cor(my_data[, 4:8])

# Load the psych package for corr.test()
library(psych)

# Compute the correlation matrix and p-values using corr.test()
corr_results <- corr.test(df[, 5:9])

# Extract the p-values from the results and format them
p_values <- format(round(corr_results$p, 3), nsmall = 3)

# Add the p-values to the correlation matrix
cor_matrix_with_pvalues <- paste0(format(round(cor_matrix, 2), nsmall = 2), "*\n(p =", p_values, ")")
print(cor_matrix_with_pvalues)

##### How do RC components predict RC separately by Grade and Group? ######

### Linear Models

###### 1st Grade ####

# Create dys groups
df1_dys<-df_1%>%dplyr::filter(DD=="DD")%>%
  select(DD,RC,WR,RAN_Letters,LC,Digits)
df1_typ<-df_1%>%dplyr::filter(DD=="TYP")%>%
  select(DD,RC,WR,RAN_Letters,LC,Digits)

#Dys
fit_first_D<-lm(RC~WR+LC+RAN_Letters+Digits, data=df1_dys)
summary(fit_first_D)
out_1D<-tidy(fit_first_D)
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

###### 2nd Grade #### 

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


###### 3rd & 4th Grade #### 

df3_dys<-df_3_4%>%dplyr::filter(DD=="DD")%>%
  select(grade,RC,WR,RAN_Letters,LC,Digits)
df3_typ<-df_3_4%>%dplyr::filter(DD=="TYP")%>%
  select(DD,RC,WR,RAN_Letters,LC,Digits)

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

######  Adult #### 

da_dys<-adult%>%dplyr::filter(DD=="DD")%>%
  select(DD,RC,WR,RAN_Letters,LC,Digits)
da_typ<-adult%>%dplyr::filter(DD=="TYP")%>%
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
library(BayesFactor)
### Bayesian Models ###
# Create and run Bayesian models. This part produces Bayes factors. 
#To compare best models with other models, I saved these into .csv and 
#compared divded BF for top model by BF for each subsequent model

#1 Typ
b1_typ <- runRegOnAll(df1_typ) 

#1 Dys
b1_dys <- runRegOnAll(df1_dys)

#2 Typ
b2_typ <- runRegOnAll(df2_typ) 

#2 Dys
b2_dys <- runRegOnAll(df2_dys)

#3/4 Typ
b3_typ <- runRegOnAll(df3_typ) 

#3/4 Dys
b3_dys <- runRegOnAll(df3_dys)

#Adult Typ
ba_typ <- runRegOnAll(da_typ) 

#Adult Dys
ba_dys <- runRegOnAll(da_dys)

#### Descriptive tables ####

library(arsenal)
first<-summary(tableby(DD ~ ., data = df_1,
                      control=tableby.control(numeric.stats="meansd", total=FALSE)),title = "First Grade Descriptives",text=TRUE,digits=2, digits.p=3)
second<-summary(tableby(DD ~ ., data = df_2_read,
                       control=tableby.control(numeric.stats="meansd", total=FALSE)),title = "Second Grade Descriptives",text=TRUE,digits=2, digits.p=3)

third_fourth<-summary(tableby(DD ~ ., data = df_3_4,
                        control=tableby.control(numeric.stats="meansd", total=FALSE)),title = "ThirdFourth Grade Descriptives",text=TRUE,digits=2, digits.p=3)
adult<-summary(tableby(DD ~ ., data = adult,
                              control=tableby.control(numeric.stats="meansd", total=FALSE)),title = "Adult  Descriptives",text=TRUE,digits=2, digits.p=3)

arsenal::write2word(first, "READ_1st_descriptives_new.doc", title="READ 1st Grade Descriptives")

arsenal::write2word(second, "READ_2nd_descriptives_new.doc", title="READ 2nd Grade Descriptives")

arsenal::write2word(third_fourth, "3rd4th_descriptives_new.doc", title="3rd-4th Descriptives")

arsenal::write2word(adult, "Adult_descriptives_new.doc", title="Adult Descriptives")
