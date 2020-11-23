# paycon_3pi_200112.R I HHI regression
# paycon_3pi_200111.R I added a column of trimmed income
# paycon_3pi_191230.R start 2nd revision for REIO### paycon_3pi_200108.R Table of sample stats starts Line 578 (end)
# Sections 3, 4.1, 4.2, 4.3, 4.5, and 5.1 in the paper  (all are 3 payment instruments)
### The following packages are used:
library(formattable)# has percent function
#library(plotrix)# weighted histograms
library(dplyr)
library(ineq) # for Gini coefficient
#library(mfx) marginal effects for logit
library(broom) # for tidy lm summary
library(xtable)# for LaTeX tables
#library(ggplot2)
#library(car)# for boxCox linearity test
#library(fitdistrplus)# for Fig 1: Fitting distributions to observed HHI
#library(actuar)# add Pareto dist (and others) to the above
setwd("~/Papers/paycon/paycon_coding") # Set your working directory !!!
dir()

### Reading RDS dataset
pay1 = readRDS("paycon_16_17_18.rds")# read diary transation based
resp_adoption_3 = readRDS("resp_adoption_3.rds")# list of respondents who adopted cash, credit, debit for 3 years
resp_adoption_5 = readRDS("resp_adoption_5.rds")# list of respondents who adopted cash, credit, debit, check, prepaid for 3 years
objects()

### Section 4: Introducing the data
# paycon_3pi_191230.R constructing Table with sample statistics 

## Restricting to in_person trans only
table(pay1$in_person)
pay2 = subset(pay1, in_person==1)
table(pay2$in_person)

### Restricting trans to 7 merchant types only#
table(pay2$merch)
pay2 = subset(pay2, merch %in% 1:7)
table(pay2$merch)
nrow(pay2)# num trans reported in Sec. 2
length(unique(pay2$uasid))# num respondents
length(resp_adoption_3)

### Restricting the data to resp who have (adopt) credit and debit cards [checks and prepaid cards are analyzed in paycon_5pi_190916.R]
table(pay2$pi)
temp = as.vector(percent(prop.table(table(pay2$pi))))
temp # Note: pi=0 is 1, pi=1 is 2, etc.
temp[2]+temp[4]+temp[5] #fraction of payments made with cash, CC and DB. Reported in Section 2

# restrict to resp who adopted 3 pi (not reported in Section 2)
pay3 = subset(pay2, uasid %in% resp_adoption_3)
length(unique(pay3$uasid))
nrow(pay3)
#
nrow(pay3) - nrow(pay2)# loss of trans by restricting to CC and DC adopters
length(unique(pay3$uasid)) - length(unique(pay2$uasid))# loss of respondents by restricting to CC and DC adopters
(length(unique(pay3$uasid)) - length(unique(pay2$uasid)))/length(unique(pay2$uasid))# % loss of resp

# Restricting the sample to 3pi 1=cash, 3=credit, 4=debit (not reported in Sec. 2 b/c I further restrict the data to 3+ payments)
table(pay3$pi)
nrow(pay3[pay3$pi %in% c(1,3,4), ])# num trans made with the 3 main PI  (cash, credit, debit)
nrow(pay3[pay3$pi %in% c(1,3,4), ])/nrow(pay3)# frac trans made with the 3 main PI
nrow(pay3)-nrow(pay3[pay3$pi %in% c(1,3,4), ])# num trans deleted because not paid with the 3 PI
# Restricting trans to 3 pi
pay4 = subset(pay3, pi %in% c(1,3,4))# deleting all other PI
nrow(pay4)
length(unique(pay4$uasid))
nrow(pay4) - nrow(pay3)# num payments lost by restricting PI
length(unique(pay4$uasid)) - length(unique(pay3$uasid)) # num respondents lost by restricting PI
table(pay4$pi)
percent(prop.table(table(pay4$pi)))

# Number of payments by respondent ID
trans_by_resp = pay4 %>% group_by(uasid) %>% summarise("vol" = n())# Num trans made by each resp
head(trans_by_resp)
sum(trans_by_resp$vol)# total num trans
nrow(pay4)# verifying same as above
summary(trans_by_resp)
#
# Restricting the sample to respodents who made 3+ payments NOTE: now reported in Section 2
trans_by_resp3 = subset(trans_by_resp, vol >= 3)
nrow(trans_by_resp3)# num resp with 3+ trans
#
nrow(pay4)# Num trans for any respo
names(trans_by_resp3)
length(unique(trans_by_resp3$uasid))# num resp who made 3+ payments
sum(trans_by_resp3$vol)# total num trans made by the above resp
# num of payments lost by restricting the data to 3+ payments
sum(trans_by_resp$vol)-sum(trans_by_resp3$vol)
# num of respondents lost by restricting the data to 3+ payments
length(unique(trans_by_resp$uasid))-length(unique(trans_by_resp3$uasid))

# Restricting entire sample pay4 to resp with 3+ payments (reported in Section 2)
pay43 = subset(pay4, uasid %in% trans_by_resp3$uasid)
nrow(pay43)# total num trans made by resp who made 3+ trans
length(unique(pay43$uasid))

### Section 5.2 (3 PI concentration: HHI and histogram)
# Rename pi
pay53 = pay43
pay53 = pay53 %>% mutate(pi = replace(pi, pi==1, "cash"))
pay53 = pay53 %>% mutate(pi = replace(pi, pi==3, "credit"))
pay53 = pay53 %>% mutate(pi = replace(pi, pi==4, "debit"))
str(pay53)# Problem, pi is char not factor, so
# below make it a factor with the correct levels
#pay53$pi = factor(pay53$pi, levels = c("cash", "check", "credit", "debit", "prepaid"))
pay53$pi = factor(pay53$pi, levels = c("cash", "credit", "debit"))

head(pay43$pi, 30)# verifying above mapping of pi is correct
head(pay53$pi, 30)
tail(pay43$pi, 30)
tail(pay53$pi, 30)
table(pay53$pi)# num transactions by PI
percent(prop.table(table(pay53$pi)))
names(pay53)

### Summarizing use (by trans num) of each PI, of each resp
pay_cash3 = pay53 %>%  group_by(uasid) %>% summarise(sum(pi == "cash"))
pay_credit3 = pay53 %>%  group_by(uasid) %>% summarise(sum(pi == "credit"))
pay_debit3 = pay53 %>%  group_by(uasid) %>% summarise(sum(pi == "debit"))

### Total dollar spending (amnt) by each resp
pay_amnt3 = pay53 %>% group_by(uasid) %>% summarise(sum(amnt))
head(pay_amnt3)

### Num transactions by each respondent
pay_trans3 = pay53 %>% group_by(uasid) %>% summarise(n())
head(pay_trans3)
summary(pay_trans3)
table(pay_trans3$`n()`)
sum(pay_trans3$`n()`)
nrow(pay53)
length(unique(pay_trans3$uasid))# num all respondents with both cards
length(unique(pay53$uasid))# num all respondents with both cards

### merging all the above into a single dataframe
pay_pi31a  = merge(x = pay_trans3, y = pay_cash3, all.x = T)
#pay_pi51b = merge(x = pay_pi51, y = pay_check5, all.x = T)
pay_pi31b = pay_pi31a
pay_pi32  = merge(x = pay_pi31b, y = pay_credit3, all.x = T)
pay_pi33  = merge(x = pay_pi32, y = pay_debit3, all.x = T)
#pay_pi53a = merge(x = pay_pi53, y = pay_prepaid5, all.x = T)
pay_pi33b = merge(x = pay_pi33, y = pay_amnt3, all.x = T)
dim(pay_pi33b)
head(pay_pi33b)
#
names(pay_pi33b) = c("uasid", "total","cash", "credit", "debit",   "amount")
head(pay_pi33b)

### computing the HHI concentration measure (3 PI), Section 6.2 in paper
# Turning tran num intro fractions
pay_pi34 = pay_pi33b
pay_pi34$cash_share = pay_pi34$cash/pay_pi34$total
pay_pi34$credit_share = pay_pi34$credit/pay_pi34$total
pay_pi34$debit_share = pay_pi34$debit/pay_pi34$total
head(pay_pi34)
# Adding the HHI concentration measure
pay_pi34$hhi = pay_pi34$cash_share^2 + pay_pi34$credit_share^2 + pay_pi34$debit_share^2 
head(pay_pi34)
#
nrow(pay_pi34)# num resp who made at least 3 payments
sum(pay_pi34$total)# total num trans
summary(pay_pi34$hhi)# dist of concentration 
table(pay_pi34$hhi == 1)# num respo with HHI = 1
percent(prop.table(table(pay_pi34$hhi == 1)))# the above in %
summary(pay_pi34[pay_pi34$hhi < 1,]$hhi)# dist of concentration for resp with HHI < 1

### HHI histograms: Figure 1 top (by volume) Subsection 6.2
#
# Histogram with density
histPercent <- function(x, ...) {
  H <- hist(x, plot = FALSE)
  H$density <- with(H, 100 * density* diff(breaks)[1])
  labs <- paste(round(H$density), "%", sep="")
  plot(H, freq = FALSE, labels = labs, xlim=c(0.3,1), ylim=c(0, 1.08*max(H$density)),...)# Oz added the xlim here to improve x-axis
}
par(mar = c(4.5,4,2,0))
histPercent(pay_pi34$hhi, col = "lightblue",  ylab = "Percentage of respondents", xlab = "Payment concentration (HHI) by volume with 3 payment methods", main = "")
#
# Information provided in section 3.2 (3 pi)
# num of resp and trans in the above histograms
length(unique(pay_pi34$uasid))# num resp in top of Fig 1
sum(pay_pi34$total)# num trans of the above resp
summary(pay_pi34$hhi)
length((pay_pi34[pay_pi34$hhi == 1, ]$uasid))# num resp with HHI=1
length(unique(pay_pi34[pay_pi34$hhi == 1, ]$uasid))# num resp with HHI=1 # same as above, showing unique is not needed here
# below percentage of respondents with HHI=1
percent(length((pay_pi34[pay_pi34$hhi == 1, ]$uasid)))/length(unique(pay_pi34$uasid))# 
percent(length((pay_pi34[pay_pi34$hhi >= 0.95, ]$uasid)))/length(unique(pay_pi34$uasid))# 


# some stats of the data plotted in Figure 1 (resp who made 3+ transactions)
# Info for the top panel of Figure 1 (all HHI)
length(unique(pay_pi34$uasid))# num resp
sum(pay_pi34$total)# num trans
median(pay_pi34$hhi)
mean(pay_pi34$hhi)

# Info for HHI = 1 
names(pay_pi34)
pay43_hhi1 = pay43[pay43$uasid %in% pay_pi34[pay_pi34$hhi == 1,]$uasid, ]
nrow(pay43_hhi1)# total num trans for HHI=1
length(unique(pay43_hhi1$uasid))# num resp with HHI=1
table(pay43_hhi1$pi)# distribution of trans by pi amoungr resp with hhi=1 
100*prop.table(table(pay43_hhi1$pi))
sum(100*prop.table(table(pay43_hhi1$pi)))# 100%

### Subsection 6.2 continues: Figure 2 begins
### How is concentration vary with num (volume) trans?
cor(pay_pi34$hhi, pay_pi34$total)# low correlation (reported in paper)
#
# range of total (need to decide on upperbound)
summary(pay_pi34$total)
sum(pay_pi34$total)
table(pay_pi34$total)
sum(table(pay_pi34$total))
#
nrow(pay_pi34[which(pay_pi34$total >= 29), ])# num resp who made more than 29 payments
pay_pi34_28 = subset(pay_pi34, total <= 28) # cut figure @ 28 payments (practially, the cut is at 30 or more)
sum(pay_pi34_28$total)
length(unique(pay_pi34_28$uasid))# num resp who made less than 29 payments. 
length(unique(pay_pi34$uasid))-length(unique(pay_pi34_28$uasid))# num resp who made more than 28 payments. 
#
summary(pay_pi34[pay_pi34$total == 3,]$hhi)
summary(pay_pi34[pay_pi34$total == 4,]$hhi)
summary(pay_pi34[pay_pi34$total == 5,]$hhi)
summary(pay_pi34[pay_pi34$total == 6,]$hhi)
summary(pay_pi34[pay_pi34$total == 7,]$hhi)
summary(pay_pi34[pay_pi34$total == 8,]$hhi)
summary(pay_pi34[pay_pi34$total == 9,]$hhi)
summary(pay_pi34[pay_pi34$total == 10,]$hhi)
summary(pay_pi34[pay_pi34$total == 11,]$hhi)
summary(pay_pi34[pay_pi34$total == 12,]$hhi)
summary(pay_pi34[pay_pi34$total == 13,]$hhi)
summary(pay_pi34[pay_pi34$total == 14,]$hhi)
summary(pay_pi34[pay_pi34$total == 15,]$hhi)
summary(pay_pi34[pay_pi34$total == 16,]$hhi)
summary(pay_pi34[pay_pi34$total == 17,]$hhi)
summary(pay_pi34[pay_pi34$total == 18,]$hhi)
summary(pay_pi34[pay_pi34$total == 19,]$hhi)
summary(pay_pi34[pay_pi34$total == 20,]$hhi)
summary(pay_pi34[pay_pi34$total == 21,]$hhi)
summary(pay_pi34[pay_pi34$total == 22,]$hhi)
summary(pay_pi34[pay_pi34$total == 23,]$hhi)
summary(pay_pi34[pay_pi34$total == 24,]$hhi)
summary(pay_pi34[pay_pi34$total == 25,]$hhi)
summary(pay_pi34[pay_pi34$total == 26,]$hhi)
summary(pay_pi34[pay_pi34$total == 27,]$hhi)
summary(pay_pi34[pay_pi34$total == 28,]$hhi)
#
par(mar = c(4,2,0.5,1))# plot Figure 2
plot(pay_pi34_28$hhi~pay_pi34_28$total, pch = 20, xlab = "Respondents' recorded volume (number) of payments", ylab = "Payment method concentration level (HHI)", cex = 1.2, xlim=c(0,30))
abline(lm(hhi~total, data = pay_pi34_28), col = "magenta")
hhi_medians = pay_pi34_28 %>% group_by(total) %>% summarise(median(hhi))
points(hhi_medians, col="red", pch = 8, cex = 1.5)
hhi_means = pay_pi34_28 %>% group_by(total) %>% summarise(mean(hhi))
points(hhi_means, col="blue", pch = 2, cex = 1.5)
summary(lm(hhi~total, data = pay_pi34_28))


### Computing HHI (3 pi) by dollar amount (value) (Subsection 6.3, Figure 1 (middle))
# Recall Line 141
head(pay_pi33b)
# I need to replace cash, credit, debit volumes with total dollar amount by each pi (amount column is total for each resp)
names(pay53)
#
# computing total dollar amounts by PI (to be merged below)
pay54_cash = subset(pay53, pi=="cash")
amount_cash3 = pay54_cash %>%  group_by(uasid) %>% summarise("cash_amount"=sum(amnt))
head(amount_cash3)
pay54_credit = subset(pay53, pi=="credit")
amount_credit3 = pay54_credit %>%  group_by(uasid) %>% summarise("credit_amount"=sum(amnt))
head(amount_credit3)
pay54_debit = subset(pay53, pi=="debit")
amount_debit3 = pay54_debit %>%  group_by(uasid) %>% summarise("debit_amount"=sum(amnt))
head(amount_debit3)
#
# merging with the data frame to be used to compute shares
head(pay_pi33b) # recall this by-respondent data frame
# merge total amount spent with cash by respondent
amount_pi1 = merge(x=pay_pi33b, y = amount_cash3, all.x = T)
head(amount_pi1)
# merge total amount spent with credit by respondent
amount_pi2 = merge(x=amount_pi1, y = amount_credit3, all.x = T)
head(amount_pi2)
# merge total amount spent with debit by respondent
amount_pi3 = merge(x=amount_pi2, y = amount_debit3, all.x = T)
head(amount_pi3)
# replace NA with 0
amount_pi4 = amount_pi3
amount_pi4[is.na(amount_pi4)] = 0
head(amount_pi4)

# computing the HHI Amount concentration measure (3 PI)
# Turning dollar amounts into fractions
amount_pi5 = amount_pi4
amount_pi5$cash_amount_share = amount_pi5$cash_amount/amount_pi5$amount
amount_pi5$credit_amount_share = amount_pi5$credit_amount/amount_pi5$amount
amount_pi5$debit_amount_share = amount_pi5$debit_amount/amount_pi5$amount
head(amount_pi5)

# Adding the dollar amount HHI concentration measure
amount_pi6 = amount_pi5
amount_pi6$amount_hhi = amount_pi6$cash_amount_share^2 + amount_pi6$credit_amount_share^2 + amount_pi6$debit_amount_share^2. 
head(amount_pi6)

# plotting histogram: Amount_hhi versus Amount (Fig 1 middle)
summary(amount_pi6$amount)
summary(amount_pi6$amount_hhi)

# Histogram with density
histPercent <- function(x, ...) {
  H <- hist(x, plot = FALSE)
  H$density <- with(H, 100 * density* diff(breaks)[1])
  labs <- paste(round(H$density), "%", sep="")
  plot(H, freq = FALSE, labels = labs, xlim=c(0.3,1), ylim=c(0, 1.08*max(H$density)),...)# Oz added the xlim here to improve x-axis
}
par(mar = c(4.5,4,2,0))
histPercent(amount_pi6$amount_hhi, col = "lightblue",  ylab = "Percentage of respondents", xlab = "Payment concentration (HHI) by dollar amount with 3 payment methods", main = "")
#
# information for Fig 1 middle subsection 6.3
nrow(amount_pi6)# num resp who made at least 3 payments
summary(amount_pi6$amount_hhi)# dist of concentration 
table(amount_pi6$amount_hhi == 1)# num respo with HHI = 1
percent(prop.table(table(amount_pi6$amount_hhi == 1)))# the above in %
percent(prop.table(table(amount_pi6$amount_hhi >= 0.9 & amount_pi6$amount_hhi <= 1)))# the above in %
percent(prop.table(table(amount_pi6$amount_hhi >= 0.95 & amount_pi6$amount_hhi <= 1)))# This shows on the hist!
# summary w.r.t to respondents' total dollar amounts
summary(amount_pi6$amount)# dist of concentration 

### Subsection 6.5: Other concentration measures (volume)
# Recall Line 152 (back to volume-only HHI)
head(pay_pi34)

## Adding Gini coefficient by volume
pay_pi35 = pay_pi34
for (i in 1:nrow(pay_pi35)) {
#pay_pi35[i, "Gini"] = ineq(c(pay_pi35[i, "cash"], pay_pi35[i, "credit"], pay_pi35[i, "debit"]), type = "Gini")
pay_pi35[i, "Gini"] = Gini(c(pay_pi35[i, "cash"], pay_pi35[i, "credit"], pay_pi35[i, "debit"]), corr = T)# corr=T means finite sample. 
}
head(pay_pi35)

## Adding I1: share of pi used the most (suggested by a referee)
pay_pi36 = pay_pi35
for (i in 1:nrow(pay_pi36)) {
  pay_pi36[i, "I1"] = max(pay_pi36[i, "cash_share"], pay_pi36[i, "credit_share"], pay_pi36[i, "debit_share"])
}
head(pay_pi36)

# summary stats for HHI, Gini, and I1 by volume (also appears in Table 2 bottom 4 rows)
summary(pay_pi36$hhi)
summary(pay_pi36$Gini)
summary(pay_pi36$I1)
#
cor(pay_pi36$hhi, pay_pi36$Gini)# stated at the end of Section 6.5
cor(pay_pi36$hhi, pay_pi36$I1)
cor(pay_pi36$Gini, pay_pi36$I1)
#
nrow(pay_pi36)

## Gini by value (reported in section 6.5)
head(amount_pi6) # recall from line 290
amount_pi7 = amount_pi6
nrow(amount_pi7)# num resp
#
# for (i in 1:nrow(amount_pi7)) {
#   amount_pi7[i, "amount_Gini"] = ineq(c(amount_pi7[i, "cash_amount"], amount_pi7[i, "credit_amount"], amount_pi7[i, "debit_amount"]), type = "Gini")
# }
for (i in 1:nrow(amount_pi7)) {
  amount_pi7[i, "amount_Gini"] = Gini(c(amount_pi7[i, "cash_amount"], amount_pi7[i, "credit_amount"], amount_pi7[i, "debit_amount"]), corr = T)
}

head(amount_pi7)
summary(amount_pi7$amount_Gini)# reported in section 4.5

## I1 by value (reported in section 4.5)
for (i in 1:nrow(amount_pi7)) {
  amount_pi7[i, "amount_I1"] = max(c(amount_pi7[i, "cash_amount_share"], amount_pi7[i, "credit_amount_share"], amount_pi7[i, "debit_amount_share"]))
}
head(amount_pi7)
summary(amount_pi7$amount_I1)# reported in section 4.5

### HHI, Gini, and I1 regressions Section 7 in paper
pay_pi37 = pay_pi36
head(pay_pi37)
#
# adding demographics
demog1 = subset(pay4, select = c("uasid", "work_employed", "work_onleave", "work_temp_unemployed", "work_looking", "work_retired", "work_disabled", "work_other", "work_occupation", "work_self", "marital_status", "highest_education", "hispaniclatino", "hispaniclatino_group", "race_white",   "race_black", "race_asian", "race_other", "hh_size", "age", "income_hh", "gender"))
names(demog1)
dim(demog1)
head(demog1, 3)
str(demog1)
#
# edit demographics
demog2 = demog1
table(demog2$work_employed)
demog2$work = demog2$work_employed # create a new variable "work"
demog2$work[demog2$work==1] = "employed"
demog2$work[demog2$work==0] = "not_employed"
table(demog2$work)
demog2$work = factor(demog2$work)
table(demog2$work)
#
table(demog2$marital_status)
str(demog2$marital_status)
demog2$marital = demog2$marital_status # create a new variable "marital"
demog2$marital[demog2$marital==1 | demog2$marital==2] = "married"
demog2$marital[demog2$marital %in% c(3,4,5,6)] = "not_married"
table(demog2$marital)
demog2$marital = factor(demog2$marital)
#
table(demog2$highest_education)
demog2$education = NA # new variable "education"
demog2$education[demog2$highest_education <= 9] = "HS_or_less"
demog2$education[demog2$highest_education == 10] = "Some_college"
demog2$education[demog2$highest_education >= 11 & demog2$highest_education <= 12] = "Assoc_degree"
demog2$education[demog2$highest_education == 13 ] = "BA_degree"
demog2$education[demog2$highest_education >= 14] = "MA_or_higher"
#
table(demog2$education)
demog2$education = factor(demog2$education, levels = c("HS_or_less", "Some_college", "Assoc_degree", "BA_degree", "MA_or_higher")) 
levels(demog2$education)
table(demog2$education)
#
table(demog2$hh_size) # no edit
table(demog2$age) # no edit
summary(demog2$income_hh) # no edit
#
table(demog2$gender)
demog2$gender[demog2$gender==0] = "female"
demog2$gender[demog2$gender==1] = "male"
table(demog2$gender)
demog2$gender = factor(demog2$gender, levels = c("female", "male"))
table(demog2$gender)
#
str(demog2)
head(demog2)
dim(demog2)
#
length(unique(demog2$uasid))
demog3 = distinct(demog2)
dim(demog3)
head(demog3)

## Merging demog2 into pay_pi37
dim(pay_pi37)
head(pay_pi37)
#
pay_pi38 = merge(x = pay_pi37, y = demog3, by="uasid",  all.x = T)
head(pay_pi38)
tail(pay_pi38)
nrow(pay_pi38)# num obs (respondents) in the following regressions

# renaming Amount to value and total to volume
pay_pi39 = pay_pi38
names(pay_pi39)
colnames(pay_pi39)[colnames(pay_pi39)=="amount"] = "value"
colnames(pay_pi39)[colnames(pay_pi39)=="total"] = "volume"
colnames(pay_pi39)[colnames(pay_pi39)=="income_hh"] = "hh_income" # make it consistent with "hh_size"

## HHI regression  begins
hhi_model = hhi ~ value + volume + hh_income + age + gender + relevel(work, ref = "employed") + hh_size + relevel(marital, ref = "married") + education 
#
hhi_lm = lm(hhi_model, data = pay_pi39)
summary(hhi_lm) 
names(summary(hhi_lm))# locating the adjusted-R2
(hhi_r2 = as.numeric(summary(hhi_lm)$adj.r.squared)) # adjusted-R2
(hhi_lm.df = as.data.frame(tidy(summary(hhi_lm))))# turn lm summary into df
names(hhi_lm.df)
dim(hhi_lm.df)
#
# adding sig symbol into the data frame
for (i in 1:nrow(hhi_lm.df)){hhi_lm.df[i,6]=' '}
for (i in 1:nrow(hhi_lm.df)){
  if(hhi_lm.df[i,5]<=0.1){hhi_lm.df[i,6]='.'}
}
for (i in 1:nrow(hhi_lm.df)){
  if(hhi_lm.df[i,5]<=0.05){hhi_lm.df[i,6]='*'}
}
for (i in 1:nrow(hhi_lm.df)){
  if(hhi_lm.df[i,5]<=0.01){hhi_lm.df[i,6]='**'}
}
for (i in 1:nrow(hhi_lm.df)){
  if(hhi_lm.df[i,5]<=0.001){hhi_lm.df[i,6]='***'}
}
names(hhi_lm.df)
names(hhi_lm.df)[6] = "sig"
hhi_lm.df
# Remove 3 columns
(hhi_lm2.df = subset(hhi_lm.df, select = c(term, estimate, sig)))

## Gini regression begins # Revision, Larry does not want Gini and I1 regressions
# gini_model = Gini ~ value + volume + age + hh_income + education + relevel(marital, ref = "not_married") + gender + relevel(work, ref = "not_employed") + hh_size
# #
# gini_lm = lm(gini_model, data = pay_pi39)
# summary(gini_lm) 
# (gini_r2 = as.numeric(summary(gini_lm)$adj.r.squared)) # adjusted-R2
# (gini_lm.df = as.data.frame(tidy(summary(gini_lm))))# turn lm summary into df
# names(gini_lm.df)
# dim(gini_lm.df)
# #
# # adding sig symbol into the data frame
# for (i in 1:nrow(gini_lm.df)){gini_lm.df[i,6]=' '}
# for (i in 1:nrow(gini_lm.df)){
#   if(gini_lm.df[i,5]<=0.1){gini_lm.df[i,6]='.'}
# }
# for (i in 1:nrow(gini_lm.df)){
#   if(gini_lm.df[i,5]<=0.05){gini_lm.df[i,6]='*'}
# }
# for (i in 1:nrow(gini_lm.df)){
#   if(gini_lm.df[i,5]<=0.01){gini_lm.df[i,6]='**'}
# }
# for (i in 1:nrow(gini_lm.df)){
#   if(gini_lm.df[i,5]<=0.001){gini_lm.df[i,6]='***'}
# }
# names(gini_lm.df)
# names(gini_lm.df)[6] = "sig"
# gini_lm.df
# # Remove 3 columns
# (gini_lm2.df = subset(gini_lm.df, select = c(term, estimate, sig)))
# 
# ## I1 regression
# i1_model = I1 ~ value + volume + age + hh_income + education + relevel(marital, ref = "not_married") + gender + relevel(work, ref = "not_employed") + hh_size
# #
# i1_lm = lm(i1_model, data = pay_pi39)
# summary(i1_lm) 
# (i1_r2 = as.numeric(summary(i1_lm)$adj.r.squared)) # adjusted-R2
# (i1_lm.df = as.data.frame(tidy(summary(i1_lm))))# turn lm summary into df
# names(i1_lm.df)
# dim(i1_lm.df)
# #
# # adding sig symbol into the data frame
# for (i in 1:nrow(i1_lm.df)){i1_lm.df[i,6]=' '}
# for (i in 1:nrow(i1_lm.df)){
#   if(i1_lm.df[i,5]<=0.1){i1_lm.df[i,6]='.'}
# }
# for (i in 1:nrow(i1_lm.df)){
#   if(i1_lm.df[i,5]<=0.05){i1_lm.df[i,6]='*'}
# }
# for (i in 1:nrow(i1_lm.df)){
#   if(i1_lm.df[i,5]<=0.01){i1_lm.df[i,6]='**'}
# }
# for (i in 1:nrow(i1_lm.df)){
#   if(i1_lm.df[i,5]<=0.001){i1_lm.df[i,6]='***'}
# }
# names(i1_lm.df)
# names(i1_lm.df)[6] = "sig"
# i1_lm.df
# # Remove 3 columns
# (i1_lm2.df = subset(i1_lm.df, select = c(term, estimate, sig)))

## combining 3 regressions into single df# Revision, Larry does not want Gini and I1 regressions
dim(hhi_lm2.df)
#dim(gini_lm2.df)
#dim(i1_lm2.df)
#
#(reg1 = cbind(hhi_lm2.df[, c(1:3)], gini_lm2.df[, c(2:3)], i1_lm2.df[, c(2:3)]))
(reg1 = hhi_lm2.df[, c(1:3)])
dim(reg1)
#
# adding a row with num obs =  num resp
(num_obs = nrow(pay_pi39))
#(num_obs.vec = c("No. obs", num_obs, NA, num_obs, NA, num_obs, NA))
(num_obs.vec = c("No. obs", num_obs, NA))
length((num_obs.vec))
str(num_obs.vec)
# adding adjusted R2 for each regression
#(adj_r2 = as.numeric(c("Adj. R-squared", hhi_r2, NA, gini_r2, NA, i1_r2, NA)))
(adj_r2 = as.numeric(c("Adj. R-squared", hhi_r2, NA)))
#(adj_r2 = c("Adj. R-squared", hhi_r2, NA))
length((adj_r2))
str(adj_r2)
# binding with reg1
(reg2 = rbind(reg1, num_obs, adj_r2))

# adding a 2nd HHI regression restricted to resp with volume > 10 (Larry asks 2nd rev)
dim(pay_pi39)
large_vol = subset(pay_pi39, volume > 10)
dim(large_vol)
# start HHI regression on the reduced sample 
large_hhi_lm = lm(hhi_model, data = large_vol)
summary(large_hhi_lm) 
names(summary(large_hhi_lm))# locating the adjusted-R2
(large_hhi_r2 = as.numeric(summary(large_hhi_lm)$adj.r.squared)) # adjusted-R2
(large_hhi_lm.df = as.data.frame(tidy(summary(large_hhi_lm))))# turn lm summary into df
names(large_hhi_lm.df)
dim(large_hhi_lm.df)
# adding sig symbol into the data frame
for (i in 1:nrow(large_hhi_lm.df)){large_hhi_lm.df[i,6]=' '}
for (i in 1:nrow(large_hhi_lm.df)){
  if(large_hhi_lm.df[i,5]<=0.1){large_hhi_lm.df[i,6]='.'}
}
for (i in 1:nrow(large_hhi_lm.df)){
  if(large_hhi_lm.df[i,5]<=0.05){large_hhi_lm.df[i,6]='*'}
}
for (i in 1:nrow(large_hhi_lm.df)){
  if(large_hhi_lm.df[i,5]<=0.01){large_hhi_lm.df[i,6]='**'}
}
for (i in 1:nrow(large_hhi_lm.df)){
  if(large_hhi_lm.df[i,5]<=0.001){large_hhi_lm.df[i,6]='***'}
}
names(large_hhi_lm.df)
names(large_hhi_lm.df)[6] = "sig"
large_hhi_lm.df

# Remove 4 columns (including variable names)
(large_hhi_lm2.df = subset(large_hhi_lm.df, select = c(estimate, sig)))
dim(reg2)
dim(large_hhi_lm2.df)

# adding a row with num obs =  num resp
(large_num_obs = nrow(large_vol))
#(num_obs.vec = c("No. obs", num_obs, NA, num_obs, NA, num_obs, NA))
(large_num_obs.vec = c(large_num_obs, NA))
length((large_num_obs.vec))
str(large_num_obs.vec)
# adding adjusted R2 for each regression
#(adj_r2 = as.numeric(c("Adj. R-squared", hhi_r2, NA, gini_r2, NA, i1_r2, NA)))
(large_adj_r2 = as.numeric(c(large_hhi_r2, NA)))
#(adj_r2 = c("Adj. R-squared", hhi_r2, NA))
length((large_adj_r2))
# binding with large regression
(large_hhi_lm3.df = rbind(large_hhi_lm2.df, large_num_obs, large_adj_r2))

# combining the 2 regressions
dim(reg2)
dim(large_hhi_lm3.df)
(reg3 = cbind(reg2, large_hhi_lm3.df))
# Rename col names
dim(reg3)
colnames(reg3) = c("Variable", "Coefficient", "Sig", "Coefficient", "Sig")
reg3
# Rename variable column
reg3$Variable = c("(Intercept", "Number of payments", "Dollar value of payments", "Household income", "Age", "Gener/Male", "Work/Not employed", "Household size", "Marital/Not married", "Education/Some college", "Education/Associate degree", "Education/BA degree", "Education/MA or higher", "Number of respondents", "Adjusted R2")
reg3

print(xtable(reg3, digits = 6), include.rownames = F) # paste into the paper

# Sample statistics for the Concentration OLS regressions, reported below the model, Section 7 in paper ==> Not used after 2nd revision, instead, Table 2 provides sample stats

# nrow(pay_pi39)# num obs (respondents) in the following regressions
# summary(pay_pi39$value)
# summary(pay_pi39$volume)
# summary(pay_pi39$age)
# summary(pay_pi39$hh_income)
# table(pay_pi39$education)
# table(pay_pi39$marital)
# table(pay_pi39$gender)
# table(pay_pi39$work)
# summary(pay_pi39$hh_size)

### paycon_3pi_200112.R Table of sample stats starts Line 578 (end)
names(pay_pi39)# by resp
dim(pay_pi39)
names(pay53)# by payment
length(unique(pay53$uasid)) #verify num resp = nrow pay_pi39
#
(num_payments = nrow(pay53))
(num_resp = length(unique(pay53$uasid)))
(avg_pay_val = mean(pay53$amnt))
(med_pay_val = median(pay53$amnt))
(max_pay_val = max(pay53$amnt))
(min_pay_val = min(pay53$amnt))
table(pay53$pi)
(frac_cash = nrow(pay53[pay53$pi=="cash",])/num_payments)
(frac_credit = nrow(pay53[pay53$pi=="credit",])/num_payments)
(frac_debit = nrow(pay53[pay53$pi=="debit",])/num_payments)
(med_age = median(pay_pi39$age))
(med_income = median(pay_pi39$hh_income, na.rm = T))
(max_income = max(pay_pi39$hh_income, na.rm = T))
(min_income = min(pay_pi39$hh_income, na.rm = T))
(med_age = median(pay_pi39$age))
(max_age = max(pay_pi39$age))
(min_age = min(pay_pi39$age))
(frac_male = nrow(pay_pi39[pay_pi39$gender=="male",])/nrow(pay_pi39))
(frac_not_employed = nrow(pay_pi39[pay_pi39$work=="not_employed",])/nrow(pay_pi39))
(med_size = median(pay_pi39$hh_size, na.rm = T))
(frac_not_married = nrow(pay_pi39[pay_pi39$marital=="not_married",])/nrow(pay_pi39))
table(pay_pi39$education)
(frac_hs = nrow(pay_pi39[pay_pi39$education=="HS_or_less",])/nrow(pay_pi39))
(frac_college = nrow(pay_pi39[pay_pi39$education=="Some_college",])/nrow(pay_pi39))
(frac_assoc = nrow(pay_pi39[pay_pi39$education=="Assoc_degree",])/nrow(pay_pi39))
(frac_ba = nrow(pay_pi39[pay_pi39$education=="BA_degree",])/nrow(pay_pi39))
(frac_ma = nrow(pay_pi39[pay_pi39$education=="MA_or_higher",])/nrow(pay_pi39))
(med_hhi = median(pay_pi39$hhi))
(frac_hhi_1 = nrow(pay_pi39[pay_pi39$hhi==1,])/num_resp)
(med_gini = median(pay_pi39$Gini, na.rm = T))
(frac_gini_1 = nrow(pay_pi39[pay_pi39$Gini==1,])/num_resp)
(med_I1 = median(pay_pi39$I1, na.rm = T))
(frac_I1_1 = nrow(pay_pi39[pay_pi39$I1==1,])/num_resp)

# adding a column with values for HH income trimmed 1% both directions (editor's request)
nrow(pay_pi39)# num respondents
resp_trim = pay_pi39 %>% filter(hh_income < quantile(pay_pi39$hh_income, 0.99, na.rm = T) & hh_income > quantile(pay_pi39$hh_income, 0.01, na.rm = T))
num_resp_trim = nrow(resp_trim)
# 
num_payments_trim = nrow(pay53)# num payments
pay_trim = pay53 %>% filter(income_hh < quantile(pay53$income_hh, 0.99, na.rm = T) & income_hh > quantile(pay53$income_hh, 0.01, na.rm = T))
dim(pay_trim)
#
(num_payments_trim = nrow(pay_trim))
(num_resp_trim = length(unique(pay_trim$uasid)))
(avg_pay_val_trim = mean(pay_trim$amnt))
(med_pay_val_trim = median(pay_trim$amnt))
(max_pay_val_trim = max(pay_trim$amnt))
(min_pay_val_trim = min(pay_trim$amnt))
table(pay_trim$pi)
(frac_cash_trim = nrow(pay_trim[pay_trim$pi=="cash",])/num_payments_trim)
(frac_credit_trim = nrow(pay_trim[pay_trim$pi=="credit",])/num_payments_trim)
(frac_debit_trim = nrow(pay_trim[pay_trim$pi=="debit",])/num_payments_trim)
(med_age_trim = median(pay_trim$age))
(med_income_trim = median(resp_trim$hh_income, na.rm = T))
(max_income_trim = max(resp_trim$hh_income, na.rm = T))
(min_income_trim = min(resp_trim$hh_income, na.rm = T))
(med_age_trim = median(resp_trim$age))
(max_age_trim = max(resp_trim$age))
(min_age_trim = min(resp_trim$age))
(frac_male_trim = nrow(resp_trim[resp_trim$gender=="male",])/nrow(resp_trim))
(frac_not_employed_trim = nrow(resp_trim[resp_trim$work=="not_employed",])/nrow(resp_trim))
(med_size_trim = median(resp_trim$hh_size, na.rm = T))
(frac_not_married_trim = nrow(resp_trim[resp_trim$marital=="not_married",])/nrow(resp_trim))
table(resp_trim$education)
(frac_hs_trim = nrow(resp_trim[resp_trim$education=="HS_or_less",])/nrow(resp_trim))
(frac_college_trim = nrow(resp_trim[resp_trim$education=="Some_college",])/nrow(resp_trim))
(frac_assoc_trim = nrow(resp_trim[resp_trim$education=="Assoc_degree",])/nrow(resp_trim))
(frac_ba_trim = nrow(resp_trim[resp_trim$education=="BA_degree",])/nrow(resp_trim))
(frac_ma_trim = nrow(resp_trim[resp_trim$education=="MA_or_higher",])/nrow(resp_trim))
(med_hhi_trim = median(resp_trim$hhi))
(frac_hhi_1_trim = nrow(resp_trim[resp_trim$hhi==1,])/num_resp_trim)
(med_gini_trim = median(resp_trim$Gini, na.rm = T))
(frac_gini_1_trim = nrow(resp_trim[resp_trim$Gini==1,])/num_resp_trim)
(med_I1_trim = median(resp_trim$I1, na.rm = T))
(frac_I1_1_trim = nrow(resp_trim[resp_trim$I1==1,])/num_resp_trim)

# finalizing the table
(stat_var = c("Number of payments", "Number of respondents", "Average payment amount ($)", "Median payment amount ($)", "Highest payment amount ($)", "Lowest payment amount ($)", "Cash share (%)", "Credit card share (%)", "Debit card share (%)", "Median HH income ($)", "Highest HH income ($)", "Lowest HH income ($)", "Median age", "Oldest age", "Youngest age","Male share (%)", "Not employed (%)", "Median HH size", "Not married (%)", "High school or less (%)", "Some college (%)", "Associate degree (%)", "BA degree (%)", "MA degree or higher (%)", "Median HHI", "Median Gini", "Median I1",  "HHI=Gini=I1=1 share (%)"))
length(stat_var)
#
(stat_value = c(num_payments, num_resp, avg_pay_val, med_pay_val, max_pay_val, min_pay_val, 100*frac_cash, 100*frac_credit, 100*frac_debit, med_income, max_income, min_income, med_age, max_age, min_age, 100*frac_male, 100*frac_not_employed, med_size, 100*frac_not_married, 100*frac_hs, 100*frac_college, 100*frac_assoc, 100*frac_ba, 100*frac_ma, med_hhi, med_gini, med_I1, 100*frac_hhi_1))
length(stat_value)
#
(stat_value_trim = c(num_payments_trim, num_resp_trim, avg_pay_val_trim, med_pay_val_trim, max_pay_val_trim, min_pay_val_trim, 100*frac_cash_trim, 100*frac_credit_trim, 100*frac_debit_trim, med_income_trim, max_income_trim, min_income_trim, med_age_trim, max_age_trim, min_age_trim, 100*frac_male_trim, 100*frac_not_employed_trim, med_size_trim, 100*frac_not_married_trim, 100*frac_hs_trim, 100*frac_college_trim, 100*frac_assoc_trim, 100*frac_ba_trim, 100*frac_ma_trim, med_hhi_trim, med_gini_trim, med_I1_trim, 100*frac_hhi_1_trim))
length(stat_value_trim)
#
(stat.df = data.frame(stat_var, stat_value, stat_value_trim))
dim(stat.df)
# rename columns
colnames(stat.df) = c("Variable", "Value", "Value (trimmed")
stat.df

# below, create matrix w\ 1 extra column to indicate number of digits for each row
(digitm = matrix(c(rep(rep(0,4), 2), rep(rep(2,4), 4), rep(rep(1,4), 3), rep(rep(0,4), 6), rep(rep(1,4), 2), rep(rep(0,4),1), rep(rep(1,4), 6), rep(rep(2,4), 3), rep(rep(1,4), 1)), nrow = 28, ncol = 4, byrow = T))
#
print(xtable(stat.df, digits = digitm), include.rownames = F, hline.after = c(0,9,24,28))



#### End of paycon coding file (3 PI, Sections 2, 3, 4.2, 4.3, 4.5, 5.1 in paper) ####
###################################

