# paycon_mult_200114.R multinom using nnet package, line 431
# paycon_mult_191229.R start 2nd revision: Switching from binomial regressions to multinomial multinomial logit on hold at the end of this file
# paycon_mult_191004.R  adding log(amnt) to last 3 regressions
# paycon_mult_190926.R (version resubmitted to REIO)
# paycon_mult_190923.R 3 binomial regressions on lagged payment choice. Section 5.2 in paper
#Note: Same sample used in Section 4 and 5.1 (for 3 pi), except for removing 1st payment of each respondent. 

### The following packages are used:
library(formattable)# has percent function
#library(plotrix)# weighted histograms
library(dplyr)
library(ineq) # for Gini coefficient
#library(mfx) marginal effects for logit
library(broom) # for tidy lm summary
library(nnet) # for multinomial logit (not using mlogit b/c it requires data formatting)
#library(margins)# marginal effects for regressions, including multinom in nnet
#library(mlogit)
#library(mnlogit)
library(mfx)# binomial logit marginal effects
library(xtable)# for LaTeX tables
#library(ggplot2)
#library(car)# for boxCox linearity test
#library(fitdistrplus)# for Fig 1: Fitting distributions to observed HHI
#library(actuar)# add Pareto dist (and others) to the above
setwd("~/Papers/paycon/paycon_coding") # Set your working directory !!!
dir()

### Reading RDS dataset
pay1 = readRDS("paycon_16_17_18.rds")# read diary transation based
resp_adoption_3 = readRDS("resp_adoption_3.rds")# list of respondents (list of uasid) who adopted cash, credit, debit for 3 years
resp_adoption_5 = readRDS("resp_adoption_5.rds")# list of respondents who adopted cash, credit, debit, check, prepaid for 3 years
objects()

## Restricting to in_person trans only
table(pay1$in_person)
pay2 = subset(pay1, in_person==1)
table(pay2$in_person)

## Restricting trans to 7 merchant types only#
table(pay2$merch)
pay2 = subset(pay2, merch %in% 1:7)
table(pay2$merch)
dim(pay2)
length(unique(pay2$uasid))
length(resp_adoption_3)

### Restricting the data to resp who have (adopt) credit and debit cards [checks and prepaid cards are analyzed in paycon_5pi_190916.R]
table(pay2$pi)
temp = as.vector(percent(prop.table(table(pay2$pi))))
temp # Note: pi=0 is 1, pi=1 is 2, etc. [just for this vector position!]
temp[2]+temp[4]+temp[5] #fraction of payments made with cash, CC and DB. Reported in Section 2

# restrict to 3 pi
pay3 = subset(pay2, uasid %in% resp_adoption_3)
length(unique(pay3$uasid))# num resp
nrow(pay3)# num payments
#
nrow(pay3) - nrow(pay2)# loss of trans by restricting to CC and DC adopters
length(unique(pay3$uasid)) - length(unique(pay2$uasid))# loss of respondents by restricting to CC and DC adopters
(length(unique(pay3$uasid)) - length(unique(pay2$uasid)))/length(unique(pay2$uasid))# % loss of resp

# Restricting the sample to payments made with only 3pi 1=cash, 3=credit, 4=debit
table(pay3$pi)
nrow(pay3[pay3$pi %in% c(1,3,4), ])# num trans made with the 3 main PI  (cash, credit, debit)
nrow(pay3[pay3$pi %in% c(1,3,4), ])/nrow(pay3)# frac trans made with the 3 main PI
nrow(pay3)-nrow(pay3[pay3$pi %in% c(1,3,4), ])# num trans deleted because not paid with the 3 PI
pay4 = subset(pay3, pi %in% c(1,3,4))# deleting all other PI
nrow(pay4)
length(unique(pay4$uasid))
nrow(pay4) - nrow(pay3)# num payments lost by restricting PI
length(unique(pay4$uasid)) - length(unique(pay3$uasid)) # num respondents lost by restricting PI
table(pay4$pi)# 1=cash, 3=credit, 4=debit
percent(prop.table(table(pay4$pi)))

names(pay4)
dim(pay4)

# Number of payments by respondent ID
trans_by_resp = pay4 %>% group_by(uasid) %>% summarise("vol" = n())# Num trans made by each resp
head(trans_by_resp)
sum(trans_by_resp$vol)# total num trans
nrow(pay4)# verifying same as above
summary(trans_by_resp)
#
# Restricting the sample to respodents who made 3+ payments 
trans_by_resp3 = subset(trans_by_resp, vol >= 3)
nrow(trans_by_resp3)# num resp with 3+ trans
#
# Restricting entire sample pay4 to resp with 3+ payments
pay43 = subset(pay4, uasid %in% trans_by_resp3$uasid)
nrow(pay43)# total num trans made by resp who made 3+ trans
length(unique(pay43$uasid))

## Sorting by date and time begins:
pay44 = pay43
head(pay44$date)
str(pay44$date)
summary(pay44$date)
head(pay44$time)
str(pay44$time)

# Count NA in date and time
nrow(pay44[is.na(pay44$date), ])# num tran with missing date
nrow(pay44[!is.na(pay44$date), ])# no missing date
nrow(pay44[is.na(pay44$time), ])# num tran with missing time
nrow(pay44[!is.na(pay44$time), ])# no missing time
pay44[is.na(pay44$time), ]$year # => all 126 trans with missing "time" are from 2016
# removing 126 trans with missing "time"
nrow(pay44)
pay45 = pay44[!is.na(pay44$time), ]# removing 126 2016 trans with missing time
nrow(pay45)

# sorting by priorities: uasid, then date, and then time
pay46 = pay45[order(pay45$uasid, pay45$date, pay45$time), ]
nrow(pay46)
head(pay46[,c("uasid", "date", "time"), ])
tail(pay46[,c("uasid", "date", "time"), ])
names(pay46)

# Adding a new variable indicating a respondent's transaction number according to data an time
pay47 = pay46 %>% group_by(uasid) %>% mutate(num = 1:n())
names(pay47)
head(pay47[,c("uasid", "date", "time", "num"), ], 20)
tail(pay47[,c("uasid", "date", "time", "num"), ], 20)

## Consolidating and renaming demographic variables
pay48 = pay47
table(pay48$work_employed)
pay48$work = pay48$work_employed # create a new variable "work"
pay48$work[pay48$work==1] = "employed"
pay48$work[pay48$work==0] = "not_employed"
table(pay48$work)
pay48$work = factor(pay48$work)
table(pay48$work)
#
table(pay48$marital_status)
str(pay48$marital_status)
pay48$marital = pay48$marital_status # create a new variable "marital"
pay48$marital[pay48$marital==1 | pay48$marital==2] = "married"
pay48$marital[pay48$marital %in% c(3,4,5,6)] = "not_married"
table(pay48$marital)
pay48$marital = factor(pay48$marital)
#
table(pay48$highest_education)
pay48$education = NA # new variable "education"
pay48$education[pay48$highest_education <= 9] = "HS_or_less"
pay48$education[pay48$highest_education == 10] = "Some_college"
pay48$education[pay48$highest_education >= 11 & pay48$highest_education <= 12] = "Assoc_degree"
pay48$education[pay48$highest_education == 13 ] = "BA_degree"
pay48$education[pay48$highest_education >= 14] = "MA_or_higher"
#
table(pay48$education)
pay48$education = factor(pay48$education, levels = c("HS_or_less", "Some_college", "Assoc_degree", "BA_degree", "MA_or_higher")) 
levels(pay48$education)
table(pay48$education)
#
colnames(pay48)[colnames(pay48)=="income_hh"] = "hh_income" # make it consistent with "hh_size"
#
table(pay48$hh_size) # no edit
table(pay48$age) # no edit
summary(pay48$hh_income) # no edit
#
table(pay48$gender)
pay48$gender[pay48$gender==0] = "female"
pay48$gender[pay48$gender==1] = "male"
table(pay48$gender)
pay48$gender = factor(pay48$gender, levels = c("female", "male"))
table(pay48$gender)

# renaming merchant type
# 1 - Grocery stores, convenience stores without gas stations, pharmacies
# 2 - Gas stations
# 3 - Sit-down restaurants and bars
# 4 - Fast food restaurants, coffee shops, cafeterias, food trucks
# 5 - General merchandise stores, department stores, other stores, online shopping
# 6 - General services: hair dressers, auto repair, parking lots, laundry or dry cleaning, etc.
# 7 - Arts, entertainment, recreation
table(pay48$merch)
pay48$merch[pay48$merch==1] = "Grocery_pharma"
pay48$merch[pay48$merch==2] = "Gas"
pay48$merch[pay48$merch==3] = "Restaurant_bar"
pay48$merch[pay48$merch==4] = "Fast_food_coffee"
pay48$merch[pay48$merch==5] = "General_stores"
pay48$merch[pay48$merch==6] = "Genereal_services"
pay48$merch[pay48$merch==7] = "Arts_entertain_recreation"
table(pay48$merch)
pay48$merch = factor(pay48$merch, levels = c("Grocery_pharma", "Gas", "Restaurant_bar", "Fast_food_coffee", "General_stores", "Genereal_services", "Arts_entertain_recreation"))
table(pay48$merch)

# Year
str(pay48$year)
pay48$year = as.factor(pay48$year)
str(pay48$year)

# renaming pi
pay48 = pay48 %>% mutate(pi = replace(pi, pi==1, "cash"))
pay48 = pay48 %>% mutate(pi = replace(pi, pi==3, "credit"))
pay48 = pay48 %>% mutate(pi = replace(pi, pi==4, "debit"))
table(pay48$pi)

## Creating lagged choice of pi
pay49 = pay48
names(pay49)
colnames(pay49)[colnames(pay49)=="num"] = "serial" # renaming "num" to "serial" which is the transaction serial number of a respondent
summary(pay49$serial) # 
#
# computing num (max serial) trans for each resp
serial_max_by_resp = pay49 %>% group_by(uasid) %>% summarise("serial_max" = max(serial))
nrow(serial_max_by_resp) # should be equal num resp
head(serial_max_by_resp)
#
pay50 = merge(x=pay49, y=serial_max_by_resp, all.x = T)
dim(pay50)
length(unique(pay50$uasid))
head(pay50[ c("uasid", "serial", "serial_max")])
tail(pay50[ c("uasid", "serial", "serial_max")])

# creating new var called pi_lagged (pi chosen on previous transaction, except the 1st)
resp = unique(pay50$uasid) # list of respondents names
length(resp)
head(resp)
pay50$pi_lagged = NA # initializing 
#
for(i in 1:nrow(pay50)){
  if(pay50[i, ]$serial >= 2) pay50[i, "pi_lagged" ] = pay50[i-1, pi]
}
head(pay50[ , c("uasid", "pi", "pi_lagged", "serial")], 50) # verify
#
# delete trans with serial ==1 (first transaction for each resp)
nrow(pay50[pay50$serial==1, ])# num of first trans (= num resp)
nrow(pay50)
pay51 = subset(pay50, serial > 1)
nrow(pay51)
names(pay51)

# removeing unused var (to make it easier to display the data)
pay52 = subset(pay51, select = c(uasid, pi, pi_lagged, amnt, merch, date, time, year, hh_size, age, hh_income, gender, work, marital, education, serial, serial_max))

# changing some chr var to factor
str(pay52)
pay52$uasid = as.factor(pay52$uasid)
pay52$pi = as.factor(pay52$pi)
pay52$pi_lagged = as.factor(pay52$pi_lagged)
pay52$merch = as.factor(pay52$merch)

names(pay52)
# creating 2x3 new 1/0 variable cash_yes, cash_lagged_yes, credit_yes, credit_lagged_yes, debit_yes, debit_lagged_yes 
pay53 = pay52
#
pay53$cash_yes = 0 #initializing
pay53[pay53$pi == "cash", ]$cash_yes = 1
table(pay53$cash_yes)
#
pay53$credit_yes = 0 #initializing
pay53[pay53$pi == "credit", ]$credit_yes = 1
table(pay53$credit_yes)
#
pay53$debit_yes = 0 #initializing
pay53[pay53$pi == "debit", ]$debit_yes = 1
table(pay53$debit_yes)
#
pay53$cash_lagged_yes = 0 #initializing
pay53[pay53$pi_lagged == "cash", ]$cash_lagged_yes = 1
table(pay53$cash_lagged_yes)
#
pay53$credit_lagged_yes = 0 #initializing
pay53[pay53$pi_lagged == "credit", ]$credit_lagged_yes = 1
table(pay53$credit_lagged_yes)
#
pay53$debit_lagged_yes = 0 #initializing
pay53[pay53$pi_lagged == "debit", ]$debit_lagged_yes = 1
table(pay53$debit_yes)
#
head(pay53, 20)


### Start 3 separate binomial logit model

# binomial logit cash model: Note: "amnt" removed to avoid perfect separation
#cash_model = cash_yes ~ cash_lagged_yes + merch + year + age + hh_income + education + marital + gender + work + hh_size # basic model w/o amnt
cash_model = cash_yes ~ cash_lagged_yes + log(amnt) + merch + year + hh_income + age + gender + work + hh_size + marital + education  # basic model with amnt
# 
cash_logit = logitmfx(cash_model, data = pay53, atmean = T)
cash_logit
#
# binomial logit credit model
#credit_model = credit_yes ~ credit_lagged_yes + merch + year + age + hh_income + education + marital + gender + work + hh_size # basic model w/o amnt
#credit_model = credit_yes ~ credit_lagged_yes + log(amnt) + merch + year + age + hh_income + education + marital + gender + work + hh_size # basic model with amnt #1st revision w/0 debit lagged yes
credit_model = credit_yes ~ credit_lagged_yes + debit_lagged_yes + log(amnt) + merch + year + hh_income + age + gender + work + hh_size + marital + education # basic model with amnt

# 
credit_logit = logitmfx(credit_model, data = pay53, atmean = T)
credit_logit
# 
# binomial logit debit model
#debit_model = debit_yes ~ debit_lagged_yes + merch + year  + age + hh_income + education + marital + gender + work + hh_size # basic model w/o amnt
#debit_model = debit_yes ~ debit_lagged_yes + log(amnt) + merch + year  + age + hh_income + education + marital + gender + work + hh_size # basic model with amnt # 1st revision
debit_model = debit_yes ~ credit_lagged_yes + debit_lagged_yes + log(amnt) + merch + year + hh_income + age + gender + work + hh_size + marital + education # basic model with amnt and credit lagged 2nd revision

# 
debit_logit = logitmfx(debit_model, data = pay53, atmean = T)
debit_logit
# 

# putting 3 regression result into single data frame
# cash logit data frame begins
(cash_logit_m =  as.data.frame(cash_logit$mfxest))
#(cash_logit_m = cbind(rownames(cash_logit_m, cash_logit_m)))

#
# adding *, **, ***
names(cash_logit_m)
for (i in 1:nrow(cash_logit_m)){cash_logit_m[i,5]=' '}
for (i in 1:nrow(cash_logit_m)){
  if(cash_logit_m[i,4]<=0.1){cash_logit_m[i,5]='.'}
}
for (i in 1:nrow(cash_logit_m)){
  if(cash_logit_m[i,4]<=0.05){cash_logit_m[i,5]='*'}
}
for (i in 1:nrow(cash_logit_m)){
  if(cash_logit_m[i,4]<=0.01){cash_logit_m[i,5]='**'}
}
for (i in 1:nrow(cash_logit_m)){
  if(cash_logit_m[i,4]<=0.001){cash_logit_m[i,5]='***'}
}
names(cash_logit_m)
names(cash_logit_m)[5] = "Sig"
cash_logit_m
dim(cash_logit_m)
cash_logit_m2 = cash_logit_m[, c(1,5)]
cash_logit_m2 # ready to be merged with credit and debit logit

# credit logit data frame begins
(credit_logit_m =  as.data.frame(credit_logit$mfxest))
#(credit_logit_m = cbind(rownames(credit_logit_m, credit_logit_m)))

#
# adding *, **, ***
names(credit_logit_m)
for (i in 1:nrow(credit_logit_m)){credit_logit_m[i,5]=' '}
for (i in 1:nrow(credit_logit_m)){
  if(credit_logit_m[i,4]<=0.1){credit_logit_m[i,5]='.'}
}
for (i in 1:nrow(credit_logit_m)){
  if(credit_logit_m[i,4]<=0.05){credit_logit_m[i,5]='*'}
}
for (i in 1:nrow(credit_logit_m)){
  if(credit_logit_m[i,4]<=0.01){credit_logit_m[i,5]='**'}
}
for (i in 1:nrow(credit_logit_m)){
  if(credit_logit_m[i,4]<=0.001){credit_logit_m[i,5]='***'}
}
names(credit_logit_m)
names(credit_logit_m)[5] = "Sig"
credit_logit_m
dim(credit_logit_m)
credit_logit_m2 = credit_logit_m[, c(1,5)]
credit_logit_m2 # ready to be merged with credit and debit logit

# debit logit data frame begins
(debit_logit_m =  as.data.frame(debit_logit$mfxest))
#(debit_logit_m = cbind(rownames(debit_logit_m, debit_logit_m)))

#
# adding *, **, ***
names(debit_logit_m)
for (i in 1:nrow(debit_logit_m)){debit_logit_m[i,5]=' '}
for (i in 1:nrow(debit_logit_m)){
  if(debit_logit_m[i,4]<=0.1){debit_logit_m[i,5]='.'}
}
for (i in 1:nrow(debit_logit_m)){
  if(debit_logit_m[i,4]<=0.05){debit_logit_m[i,5]='*'}
}
for (i in 1:nrow(debit_logit_m)){
  if(debit_logit_m[i,4]<=0.01){debit_logit_m[i,5]='**'}
}
for (i in 1:nrow(debit_logit_m)){
  if(debit_logit_m[i,4]<=0.001){debit_logit_m[i,5]='***'}
}
names(debit_logit_m)
names(debit_logit_m)[5] = "Sig"
debit_logit_m
dim(debit_logit_m)
debit_logit_m2 = debit_logit_m[, c(1,5)]
debit_logit_m2 # ready to be merged with credit and debit logit

## merging 3 data frame with marginal effects
# add 2 rows to cash_logit_m2
(cash_logit_m3 = rbind(c(NA, NA, NA), cash_logit_m2))
(cash_logit_m3 = rbind(c(NA, NA, NA), cash_logit_m3))
nrow(cash_logit_m3)
(cash_logit_m4 = cash_logit_m3[c(3,1,2,4:22), ])
rownames(cash_logit_m4)[rownames(cash_logit_m4) == "1"] = "credit_lagged_yes"
rownames(cash_logit_m4)[rownames(cash_logit_m4) == "11"] = "debit_lagged_yes"
cash_logit_m4
# add 1 row to credit_logit_m2
(credit_logit_m3 = rbind(c(NA, NA, NA), credit_logit_m2))
credit_logit_m4 = credit_logit_m3
rownames(credit_logit_m4)[rownames(credit_logit_m3) == "1"] = "cash_lagged_yes"
credit_logit_m4
# add 1 row to debit_logit_m2
(debit_logit_m3 = rbind(c(NA, NA, NA), debit_logit_m2))
debit_logit_m4 = debit_logit_m3
rownames(debit_logit_m4)[rownames(debit_logit_m3) == "1"] = "cash_lagged_yes"
debit_logit_m4
dim(debit_logit_m4)
#
#(logit_m4 = cbind(rownames(cash_logit_m4), cash_logit_m4, credit_logit_m4, debit_logit_m4))
(logit_m4 = cbind(cash_logit_m4, credit_logit_m4, debit_logit_m4))
dim(logit_m4)
#colnames(logit_m4)[colnames(logit_m4)== "rownames(cash_logit_m4)"] = "Variable"
names(logit_m4)
logit_m4
# revising rownames
logit_m5 = logit_m4
dim(logit_m5)
rownames(logit_m5)
length(rownames(logit_m5))
rownames(logit_m5) =  c("Previous/Cash", "Previous/Credit", "Previous/Debit", "Log payment amount", "Merch/Gas station", "Merch/Restaurant, bar", "Merch/Fast food, coffee shop", "Merch/General store", "Merch/General service", "Merch/Arts, entertain recreation", "Year/2017", "Year/2018", "Household income", "Age", "Gender/Male", "Work/Not employed", "Household size", "Marital/Not married", "Education/Some college", "Education/Assoc degree", "Education/BA degree", "Education/MA or higher")
logit_m5
#
colnames(logit_m5)
logit_m6 =  logit_m5
(colnames(logit_m6) = c("MarEff", "Sig", "MarEff", "Sig", "MarEff", "Sig" ))

# Below, Table xx (logit_m4.t) in the paper
print(xtable(logit_m6, digits = 3), include.rownames = T) # This removes row numbers
#
nrow(pay53)# Num obs 
length(unique(pay53$uasid))

### paycon_mult_200114.R multinom using nnet package, line 431
table(pay53$pi)
names(pay53)
table(pay53$pi_lagged)

#(pay_multi =  multinom(pi ~ pi_lagged + log(amnt), data=pay53))
(pay_multi =  multinom(pi ~ pi_lagged + log(amnt) + merch + year + hh_income + age + gender + work + hh_size + marital + education, data=pay53))
summary(pay_multi)

# Follow the procedure in https://data.princeton.edu/wws509/r/mlogit
# Relative prob
(B = coef(pay_multi))
colnames(B)
exp(B[, "pi_laggedcredit"])
exp(B[, "pi_laggeddebit"])
# marginal effects (continuous)
probs = predict(pay_multi, type="probs")
dim(probs)
colnames(probs)
(b_laggedcredit = c(0,B[,"pi_laggedcredit"]))
(b_laggeddebit = c(0,B[,"pi_laggeddebit"]))
#
pb_laggedcredit = probs[,2]*b_laggedcredit[2] + probs[,3]*b_laggedcredit[3]
pb_laggeddebit = probs[,2]*b_laggeddebit[2] + probs[,3]*b_laggeddebit[3]
length(pb_laggedcredit)
#
me_laggedcredit = matrix(0, nrow(probs), ncol(probs))
me_laggeddebit = matrix(0, nrow(probs), ncol(probs))
dim(me_laggedcredit)
dim(me_laggeddebit)
#
for(j in 1:3){
  me_laggedcredit[,j] = probs[,j] * (b_laggedcredit[j] - pb_laggedcredit)
}
for(j in 1:3){
  me_laggeddebit[,j] = probs[,j] * (b_laggeddebit[j] - pb_laggeddebit)
}
#
apply(me_laggedcredit, 2, mean)
(effect_laggedcredit_on_credit = apply(me_laggedcredit, 2, mean)[2])# relative to cash!
(effect_laggedcredit_on_debit = apply(me_laggedcredit, 2, mean)[3])# relative to cash!
apply(me_laggeddebit, 2, mean)
(effect_laggeddebit_on_credit = apply(me_laggeddebit, 2, mean)[2])# relative to cash!
(effect_laggeddebit_on_debit = apply(me_laggeddebit, 2, mean)[3])# relative to cash!

## Finalizing Table 4 (multinomial)
(pay_multi_coef = B[,1:4])# in paper, display only 3 coefficients
(colnames(pay_multi_coef) = c("Intercept", "Lagged credit", "Lagged debit", "Log(amount)"))
dim(pay_multi_coef)
pay_multi_coef
# creating a row with credit marginal effects
(credit_mar_eff = c(NA , effect_laggedcredit_on_credit, effect_laggeddebit_on_credit, NA))
(debit_mar_eff = c(NA , effect_laggedcredit_on_debit, effect_laggeddebit_on_debit, NA))
#
(pay_multi_coef.df = data.frame(pay_multi_coef))
(multi_df = rbind(pay_multi_coef.df, credit_mar_eff, debit_mar_eff))
(colnames(multi_df) = c("Intercept", "Lagged credit", "Lagged debit", "Log amount"))
row.names(multi_df)
(row.names(multi_df) = c("Credit coefficients", "Debit coefficients", "Credit marginal effects", "Debit marginal effects"))
#
print(xtable(multi_df, digits = 4), include.rownames = T, hline.after = c(0,2,4))


### End of paycon_multi code ###

