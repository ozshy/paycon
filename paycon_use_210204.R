# paycon_use_210204.R replacing id
# paycon_use_200110.R 2nd revision for REIO: New Table 1 (use of PI regardless of adoption)

### The following packages are used:
library(formattable)# has percent function
#library(plotrix)# weighted histograms
library(dplyr)
#library(ineq) # for Gini coefficient
#library(mfx) marginal effects for logit
#library(broom) # for tidy lm summary
library(xtable)# for LaTeX tables
#library(ggplot2)
#library(car)# for boxCox linearity test
#library(fitdistrplus)# for Fig 1: Fitting distributions to observed HHI
#library(actuar)# add Pareto dist (and others) to the above
setwd("~/Papers/Papers_accepted/paycon/paycon_coding") # Set your working directory !!!
dir()

### Reading RDS dataset
pay1 = readRDS("paycon_16_17_18_210204.rds")# read diary transation based
#resp_adoption_3 = readRDS("resp_adoption_3.rds")# list of respondents who adopted cash, credit, debit for 3 years
#resp_adoption_5 = readRDS("resp_adoption_5.rds")# list of respondents who adopted cash, credit, debit, check, prepaid for 3 years
objects()

## Restricting to in_person trans only
table(pay1$in_person)
pay2 = subset(pay1, in_person==1)
table(pay2$in_person)

### Restricting trans to 7 merchant types only#
table(pay2$merch)
pay2 = subset(pay2, merch %in% 1:7)
table(pay2$merch)
nrow(pay2)# num trans reported in Sec. 2
length(unique(pay2$id))# num respondents

# Restricting the sample to 5pi 1=cash, 2=check, 3=credit, 4=debit, 5=prepaid
pay3 = pay2
table(pay3$pi)
nrow(pay3[pay3$pi %in% c(1:5), ])# num trans made with the 5 main PI
nrow(pay3[pay3$pi %in% c(1:5), ])/nrow(pay3)# frac trans made with the 5 main PI
nrow(pay3)-nrow(pay3[pay3$pi %in% c(1:5), ])# num trans deleted because not paid with the 5 PI
# Restricting trans to 5 pi
pay4 = subset(pay3, pi %in% c(1:5))# deleting all other PI
nrow(pay4)
length(unique(pay4$id))
nrow(pay4) - nrow(pay3)# num payments lost by restricting PI
length(unique(pay4$id)) - length(unique(pay3$id)) # num respondents lost by restricting PI
table(pay4$pi)
percent(prop.table(table(pay4$pi)))

# adding demographics ==> Not needed here b/c Table 1 focuses on use only
demog1 = subset(pay4, select = c("id", "work_employed", "work_onleave", "work_temp_unemployed", "work_looking", "work_retired", "work_disabled", "work_other", "work_occupation", "work_self", "marital_status", "highest_education", "hispaniclatino", "hispaniclatino_group", "race_white",   "race_black", "race_asian", "race_other", "hh_size", "age", "income_hh", "gender"))
names(demog1)
dim(demog1)
head(demog1, 3)
str(demog1)
#
# edit demographics
pay5 = pay4
table(pay5$work_employed)
pay5$work = pay5$work_employed # create a new variable "work"
pay5$work[pay5$work==1] = "employed"
pay5$work[pay5$work==0] = "not_employed"
table(pay5$work)
pay5$work = factor(pay5$work)
table(pay5$work)
#
table(pay5$marital_status)
str(pay5$marital_status)
pay5$marital = pay5$marital_status # create a new variable "marital"
pay5$marital[pay5$marital==1 | pay5$marital==2] = "married"
pay5$marital[pay5$marital %in% c(3,4,5,6)] = "not_married"
table(pay5$marital)
pay5$marital = factor(pay5$marital)
#
table(pay5$highest_education)
pay5$education = NA # new variable "education"
pay5$education[pay5$highest_education <= 9] = "HS_or_less"
pay5$education[pay5$highest_education == 10] = "Some_college"
pay5$education[pay5$highest_education >= 11 & pay5$highest_education <= 12] = "Assoc_degree"
pay5$education[pay5$highest_education == 13 ] = "BA_degree"
pay5$education[pay5$highest_education >= 14] = "MA_or_higher"
#
table(pay5$education)
pay5$education = factor(pay5$education, levels = c("HS_or_less", "Some_college", "Assoc_degree", "BA_degree", "MA_or_higher")) 
levels(pay5$education)
table(pay5$education)
#
table(pay5$hh_size) # no edit
table(pay5$age) # no edit
summary(pay5$income_hh) # no edit
#
table(pay5$gender)
pay5$gender[pay5$gender==0] = "female"
pay5$gender[pay5$gender==1] = "male"
table(pay5$gender)
pay5$gender = factor(pay5$gender, levels = c("female", "male"))
table(pay5$gender)
#
str(pay5)
head(pay5)
dim(pay5)
#
length(unique(pay5$id))


# Table 1 use statistics (regardless of adoption)
pay6 = pay5
(num_payments = nrow(pay6))# num of payments
(num_resp =length(unique(pay6$id)))# num respondents
(total_val = sum(pay6$amnt))# total dollar value of all payments

table(pay6$pi)
pay6[pay6$pi==1, "pi"] = "cash"
pay6[pay6$pi==2, "pi"] = "check"
pay6[pay6$pi==3, "pi"] = "credit"
pay6[pay6$pi==4, "pi"] = "debit"
pay6[pay6$pi==5, "pi"] = "prepaid"
table(pay6$pi)

# Volume share of payments by PI
(frac_cash = nrow(pay6[pay6$pi=="cash",])/num_payments)
(frac_check = nrow(pay6[pay6$pi=="check",])/num_payments)
(frac_credit = nrow(pay6[pay6$pi=="credit",])/num_payments)
(frac_debit = nrow(pay6[pay6$pi=="debit",])/num_payments)
(frac_prepaid = nrow(pay6[pay6$pi=="prepaid",])/num_payments)

# value shoare of payments by PI
(frac_val_cash = sum(subset(pay6, pi=="cash")$amnt)/total_val)
(frac_val_check = sum(subset(pay6, pi=="check")$amnt)/total_val)
(frac_val_credit = sum(subset(pay6, pi=="credit")$amnt)/total_val)
(frac_val_debit = sum(subset(pay6, pi=="debit")$amnt)/total_val)
(frac_val_prepaid = sum(subset(pay6, pi=="prepaid")$amnt)/total_val)

# Avg payment value by PI
(avg_val_cash = mean(subset(pay6, pi=="cash")$amnt))
(avg_val_check = mean(subset(pay6, pi=="check")$amnt))
(avg_val_credit = mean(subset(pay6, pi=="credit")$amnt))
(avg_val_debit = mean(subset(pay6, pi=="debit")$amnt))
(avg_val_prepaid = mean(subset(pay6, pi=="prepaid")$amnt))

# Median payment value by PI
(med_val_cash = median(subset(pay6, pi=="cash")$amnt))
(med_val_check = median(subset(pay6, pi=="check")$amnt))
(med_val_credit = median(subset(pay6, pi=="credit")$amnt))
(med_val_debit = median(subset(pay6, pi=="debit")$amnt))
(med_val_prepaid = median(subset(pay6, pi=="prepaid")$amnt))

# Highest payment value by PI
(max_val_cash = max(subset(pay6, pi=="cash")$amnt))
(max_val_check = max(subset(pay6, pi=="check")$amnt))
(max_val_credit = max(subset(pay6, pi=="credit")$amnt))
(max_val_debit = max(subset(pay6, pi=="debit")$amnt))
(max_val_prepaid = max(subset(pay6, pi=="prepaid")$amnt))

# Lowest payment value by PI
(min_val_cash = min(subset(pay6, pi=="cash")$amnt))
(min_val_check = min(subset(pay6, pi=="check")$amnt))
(min_val_credit = min(subset(pay6, pi=="credit")$amnt))
(min_val_debit = min(subset(pay6, pi=="debit")$amnt))
(min_val_prepaid = min(subset(pay6, pi=="prepaid")$amnt))

# num payments made at this merchant type
(vol_merch1 = nrow(subset(pay6, merch==1)))
(vol_merch2 = nrow(subset(pay6, merch==2)))
(vol_merch3 = nrow(subset(pay6, merch==3)))
(vol_merch4 = nrow(subset(pay6, merch==4)))
(vol_merch5 = nrow(subset(pay6, merch==5)))
(vol_merch6 = nrow(subset(pay6, merch==6)))
(vol_merch7 = nrow(subset(pay6, merch==7)))

# fraction merch type 1:7 is paid with cash
(frac_merch1_cash = nrow(subset(pay6, merch==1 & pi=="cash"))/vol_merch1)
(frac_merch2_cash = nrow(subset(pay6, merch==2 & pi=="cash"))/vol_merch2)
(frac_merch3_cash = nrow(subset(pay6, merch==3 & pi=="cash"))/vol_merch3)
(frac_merch4_cash = nrow(subset(pay6, merch==4 & pi=="cash"))/vol_merch4)
(frac_merch5_cash = nrow(subset(pay6, merch==5 & pi=="cash"))/vol_merch5)
(frac_merch6_cash = nrow(subset(pay6, merch==6 & pi=="cash"))/vol_merch6)
(frac_merch7_cash = nrow(subset(pay6, merch==7 & pi=="cash"))/vol_merch7)

# fraction merch type 1:7 is paid with check
(frac_merch1_check = nrow(subset(pay6, merch==1 & pi=="check"))/vol_merch1)
(frac_merch2_check = nrow(subset(pay6, merch==2 & pi=="check"))/vol_merch2)
(frac_merch3_check = nrow(subset(pay6, merch==3 & pi=="check"))/vol_merch3)
(frac_merch4_check = nrow(subset(pay6, merch==4 & pi=="check"))/vol_merch4)
(frac_merch5_check = nrow(subset(pay6, merch==5 & pi=="check"))/vol_merch5)
(frac_merch6_check = nrow(subset(pay6, merch==6 & pi=="check"))/vol_merch6)
(frac_merch7_check = nrow(subset(pay6, merch==7 & pi=="check"))/vol_merch7)

# fraction merch type 1:7 is paid with credit
(frac_merch1_credit = nrow(subset(pay6, merch==1 & pi=="credit"))/vol_merch1)
(frac_merch2_credit = nrow(subset(pay6, merch==2 & pi=="credit"))/vol_merch2)
(frac_merch3_credit = nrow(subset(pay6, merch==3 & pi=="credit"))/vol_merch3)
(frac_merch4_credit = nrow(subset(pay6, merch==4 & pi=="credit"))/vol_merch4)
(frac_merch5_credit = nrow(subset(pay6, merch==5 & pi=="credit"))/vol_merch5)
(frac_merch6_credit = nrow(subset(pay6, merch==6 & pi=="credit"))/vol_merch6)
(frac_merch7_credit = nrow(subset(pay6, merch==7 & pi=="credit"))/vol_merch7)

# fraction merch type 1:7 is paid with debit
(frac_merch1_debit = nrow(subset(pay6, merch==1 & pi=="debit"))/vol_merch1)
(frac_merch2_debit = nrow(subset(pay6, merch==2 & pi=="debit"))/vol_merch2)
(frac_merch3_debit = nrow(subset(pay6, merch==3 & pi=="debit"))/vol_merch3)
(frac_merch4_debit = nrow(subset(pay6, merch==4 & pi=="debit"))/vol_merch4)
(frac_merch5_debit = nrow(subset(pay6, merch==5 & pi=="debit"))/vol_merch5)
(frac_merch6_debit = nrow(subset(pay6, merch==6 & pi=="debit"))/vol_merch6)
(frac_merch7_debit = nrow(subset(pay6, merch==7 & pi=="debit"))/vol_merch7)

# fraction merch type 1:7 is paid with prepaid
(frac_merch1_prepaid = nrow(subset(pay6, merch==1 & pi=="prepaid"))/vol_merch1)
(frac_merch2_prepaid = nrow(subset(pay6, merch==2 & pi=="prepaid"))/vol_merch2)
(frac_merch3_prepaid = nrow(subset(pay6, merch==3 & pi=="prepaid"))/vol_merch3)
(frac_merch4_prepaid = nrow(subset(pay6, merch==4 & pi=="prepaid"))/vol_merch4)
(frac_merch5_prepaid = nrow(subset(pay6, merch==5 & pi=="prepaid"))/vol_merch5)
(frac_merch6_prepaid = nrow(subset(pay6, merch==6 & pi=="prepaid"))/vol_merch6)
(frac_merch7_prepaid = nrow(subset(pay6, merch==7 & pi=="prepaid"))/vol_merch7)

# finalizing Table 1
(use_var.vec = c("Volume share (%)", "Value share (%)", "Average payment amount ($)", "Median payment amount ($)", "Highest payment amount ($)", "Lowest payment amount ($)", "Volume share merchant 1 (%)", "Volume share merchant 2 (%)", "Volume share merchant 3 (%)", "Volume share merchant 4 (%)", "Volume share merchant 5 (%)", "Volume share merchant 6 (%)", "Volume share merchant 7 (%)"))
#
(use_cash.vec = c(100*frac_cash, 100*frac_val_cash, avg_val_cash, med_val_cash, max_val_cash, min_val_cash, 100*frac_merch1_cash, 100*frac_merch2_cash, 100*frac_merch3_cash, 100*frac_merch4_cash, 100*frac_merch5_cash, 100*frac_merch6_cash, 100*frac_merch7_cash))
#
(use_check.vec = c(100*frac_check, 100*frac_val_check, avg_val_check, med_val_check, max_val_check, min_val_check, 100*frac_merch1_check, 100*frac_merch2_check, 100*frac_merch3_check, 100*frac_merch4_check, 100*frac_merch5_check, 100*frac_merch6_check, 100*frac_merch7_check))
#
(use_credit.vec = c(100*frac_credit, 100*frac_val_credit, avg_val_credit, med_val_credit, max_val_credit, min_val_credit, 100*frac_merch1_credit, 100*frac_merch2_credit, 100*frac_merch3_credit, 100*frac_merch4_credit, 100*frac_merch5_credit, 100*frac_merch6_credit, 100*frac_merch7_credit))
#
(use_debit.vec = c(100*frac_debit, 100*frac_val_debit, avg_val_debit, med_val_debit, max_val_debit, min_val_debit, 100*frac_merch1_debit, 100*frac_merch2_debit, 100*frac_merch3_debit, 100*frac_merch4_debit, 100*frac_merch5_debit, 100*frac_merch6_debit, 100*frac_merch7_debit))
#
(use_prepaid.vec = c(100*frac_prepaid, 100*frac_val_prepaid, avg_val_prepaid, med_val_prepaid, max_val_prepaid, min_val_prepaid, 100*frac_merch1_prepaid, 100*frac_merch2_prepaid, 100*frac_merch3_prepaid, 100*frac_merch4_prepaid, 100*frac_merch5_prepaid, 100*frac_merch6_prepaid, 100*frac_merch7_prepaid))

# verify that shares sum up to 100%
(use_cash.vec + use_check.vec + use_credit.vec + use_debit.vec + use_prepaid.vec)

#
(use.df = data.frame(use_var.vec, use_cash.vec, use_check.vec, use_credit.vec, use_debit.vec, use_prepaid.vec))
dim(use.df)
# below, create matrix w\ 1 extra column to indicate number of digits for each row
(digitm = matrix(c(rep(1,6+1), rep(1,6+1), rep(2,6+1) , rep(2,6+1) , rep(2,6+1) , rep(2,6+1), rep(1,6+1), rep(1,6+1), rep(1,6+1), rep(1,6+1), rep(1,6+1), rep(1,6+1), rep(1,6+1)), nrow = 13, ncol = 6+1, byrow = T))
#
print(xtable(use.df, digits = digitm), include.rownames = F, hline.after = c(0,2,6,13))


#### End of paycon coding file (3 PI, Sections 2, 3, 4.2, 4.3, 4.5, 5.1 in paper) ####
###################################

