#paycon_5pi_210204.R changing to new id
#paycon_5pi_190915.R Analyzing 5 payment instruments (Fig.1 bottom and Section 4.4 in paper)
### The following packages are used:
library(formattable)# has percent function
#library(plotrix)# weighted histograms
library(dplyr)
library(xtable)# for LaTeX tables
#library(ggplot2)
#library(car)# for boxCox linearity test
#library(fitdistrplus)# for Fig 1: Fitting distributions to observed HHI
#library(actuar)# add Pareto dist (and others) to the above
setwd("~/Papers/Papers_accepted/paycon/paycon_coding") # Set working directory!
dir()

### Reading RDS dataset
pay1 = readRDS("paycon_16_17_18_210204.rds")# read diary transations
resp_adoption_3 = readRDS("resp_adoption_3_210204.rds")# list of respondents who adopted cash, credit, debit for 3 years
resp_adoption_5 = readRDS("resp_adoption_5_210204.rds")# list of respondents who adopted cash, credit, debit, check, prepaid for 3 years
objects()
resp_adoption_3 = as.vector(resp_adoption_3$id)
resp_adoption_5 = as.vector(resp_adoption_5$id)


### Restricting to merchants 1:7 and in_person trans only
table(pay1$in_person)
pay2 = subset(pay1, in_person==1)
table(pay2$in_person)
#
table(pay2$merch)
pay2 = subset(pay2, merch %in% 1:7)
table(pay2$merch)

### Restricting the data to resp who have (adopt) 5 PI: credit, debit, checks, and prepaid cards
dim(pay2)
length(unique(pay2$id))
length(resp_adoption_5)
#
pay3 = subset(pay2, id %in% resp_adoption_5)
dim(pay3)
length(unique(pay3$id))

### Section 4.4 (5 PI concentration) begins
nrow(pay3)
length(unique(pay3$id))
nrow(pay3) - nrow(pay2)# loss of trans by restriction to adoption of all 5 PI
length(unique(pay3$id)) - length(unique(pay2$id))# loss of resp by restriction to adoption of all 5 PI
(length(unique(pay3$id)) - length(unique(pay2$id)))/length(unique(pay2$id))# % loss of resp
table(pay3$pi)
nrow(pay3[pay3$pi %in% c(1:5), ])# num trans made with the 5 main PI
nrow(pay3[pay3$pi %in% c(1:5), ])/nrow(pay3)# frac trans made with the 5 main PI
nrow(pay3)-nrow(pay3[pay3$pi %in% c(1:5), ])# num trans deleted because not paid with the 5 PI
# Restricting trans to pi 1:5
pay4 = subset(pay3, pi %in% c(1:5))# deleting all other PI
nrow(pay4)
length(unique(pay4$id))
nrow(pay4) - nrow(pay3)# num payments lost by restricting PI
length(unique(pay4$id)) - length(unique(pay3$id)) # num respondents lost by restricting PI
table(pay4$pi)
percent(prop.table(table(pay4$pi)))
sum(table(pay4$pi))# Number of trans 

(fivepi_vol_cash = nrow(pay4[pay4$pi == 1,])  )# num cash trans
(fivepi_vol_check = nrow(pay4[pay4$pi == 2,])  )# num check trans
(fivepi_vol_credit = nrow(pay4[pay4$pi == 3,])  )# num credit trans
(fivepi_vol_debit = nrow(pay4[pay4$pi == 4,])  )# num debit trans
(fivepi_vol_prepaid = nrow(pay4[pay4$pi == 5,])  )# num prepaid trans
(fivepi_vol_all = nrow(pay4))
#
(fivepi_perc_cash = fivepi_vol_cash/fivepi_vol_all)# vol as % of total num trans
(fivepi_perc_check = fivepi_vol_check/fivepi_vol_all)# vol as % of total num trans
(fivepi_perc_credit = fivepi_vol_credit/fivepi_vol_all)# vol as % of total num trans
(fivepi_perc_debit = fivepi_vol_debit/fivepi_vol_all)# vol as % of total num trans
(fivepi_perc_prepaid = fivepi_vol_prepaid/fivepi_vol_all)# vol as % of total num trans
(fivepi_perc_all = fivepi_vol_all/fivepi_vol_all)# vol as % of total num trans
#
(fivepi_avg_cash = mean(pay4[pay4$pi == 1,]$amnt, na.rm = T))
(fivepi_avg_check = mean(pay4[pay4$pi == 2,]$amnt, na.rm = T))
(fivepi_avg_credit = mean(pay4[pay4$pi == 3,]$amnt, na.rm = T))
(fivepi_avg_debit = mean(pay4[pay4$pi == 4,]$amnt, na.rm = T))
(fivepi_avg_prepaid = mean(pay4[pay4$pi == 5,]$amnt, na.rm = T))
(fivepi_avg_all = mean(pay4$amnt, na.rm = T))
# Respondents incidence begins
(fivepi_med_cash = median(pay4[pay4$pi == 1,]$amnt, na.rm = T))
(fivepi_med_check = median(pay4[pay4$pi == 2,]$amnt, na.rm = T))
(fivepi_med_credit = median(pay4[pay4$pi == 3,]$amnt, na.rm = T))
(fivepi_med_debit = median(pay4[pay4$pi == 4,]$amnt, na.rm = T))
(fivepi_med_prepaid = median(pay4[pay4$pi == 5,]$amnt, na.rm = T))
(fivepi_med_all = median(pay4$amnt, na.rm = T))
#
(fivepi_resp_cash = length(unique(pay4[pay4$pi == 1,]$id)))# num resp who pay with cash at least once
(fivepi_resp_check = length(unique(pay4[pay4$pi == 2,]$id)))# num resp who pay with check at least once
(fivepi_resp_credit = length(unique(pay4[pay4$pi == 3,]$id)))# num resp who pay with credit at least once
(fivepi_resp_debit = length(unique(pay4[pay4$pi == 4,]$id)))# num resp who pay with debit at least once
(fivepi_resp_prepaid = length(unique(pay4[pay4$pi == 5,]$id)))# num resp who pay with pp at least once
(fivepi_resp_all = length(unique(pay4$id)))# num resp who uses PI at least once (incidence)
#
(fivepi_resp_perc_cash = length(unique(pay4[pay4$pi == 1,]$id))/fivepi_resp_all)# % resp who pay with cash at least once
(fivepi_resp_perc_check = length(unique(pay4[pay4$pi == 2,]$id))/fivepi_resp_all)# % resp who pay with check at least once
(fivepi_resp_perc_credit = length(unique(pay4[pay4$pi == 3,]$id))/fivepi_resp_all)# % resp who pay with credit at least once
(fivepi_resp_perc_debit = length(unique(pay4[pay4$pi == 4,]$id))/fivepi_resp_all)# % resp who pay with debit at least once
(fivepi_resp_perc_prepaid = length(unique(pay4[pay4$pi == 5,]$id))/fivepi_resp_all)# % resp who pay with pp at least once
(fivepi_resp_perc_all = length(unique(pay4$id))/fivepi_resp_all)# %S resp who uses PI at least once (incidence)
# Resp incidence for 5+ trans
trans_per_resp = pay4 %>% group_by(id) %>% summarise("vol" = n())# Num trans made by each resp
head(trans_per_resp)
sum(trans_per_resp$vol)# total num trans
nrow(pay4)# verifying same as above
trans_per_resp5 = subset(trans_per_resp, vol > 4) # Restricting to resp who made 5 or more trans
nrow(trans_per_resp5)# num resp with 5+ trans
# 
# now, back to constructing Table 1 (Table 1 deleted from paper)
# below, num resp who made 5+ payments by PI
nrow(pay4)# Num trans for any respo
names(trans_per_resp5)
length(unique(trans_per_resp5$id))# num resp who made 5+ payments
sum(trans_per_resp5$vol)# total num trans made by the above resp
pay45 = subset(pay4, id %in% trans_per_resp5$id)## Restricting pay4 to resp with 5+ trans
nrow(pay45)# total num trans made by resp who made 5+ trans
head(pay45)

# Renaming payement instruments 
pay55 = pay45
pay55 = pay55 %>% mutate(pi = replace(pi, pi==1, "cash"))
pay55 = pay55 %>% mutate(pi = replace(pi, pi==2, "check"))
pay55 = pay55 %>% mutate(pi = replace(pi, pi==3, "credit"))
pay55 = pay55 %>% mutate(pi = replace(pi, pi==4, "debit"))
pay55 = pay55 %>% mutate(pi = replace(pi, pi==5, "prepaid"))
str(pay55)# Problem, pi is char not factor, so
# below make it a factor with the correct levels
pay55$pi = factor(pay55$pi, levels = c("cash", "check", "credit", "debit", "prepaid"))
head(pay45$pi, 30)# verifying above mapping of pi is correct
head(pay55$pi, 30)
tail(pay45$pi, 30)
tail(pay55$pi, 30)
table(pay55$pi)# num transactions by PI
percent(prop.table(table(pay55$pi)))
names(pay55)

### Summarizing use (by trans num) of each PI, of each resp
pay_cash5 = pay55 %>%  group_by(id) %>% summarise(sum(pi == "cash"))
pay_check5 = pay55 %>%  group_by(id) %>% summarise(sum(pi == "check"))
pay_credit5 = pay55 %>%  group_by(id) %>% summarise(sum(pi == "credit"))
pay_debit5 = pay55 %>%  group_by(id) %>% summarise(sum(pi == "debit"))
pay_prepaid5 = pay55 %>%  group_by(id) %>% summarise(sum(pi == "prepaid"))

### Total dollar spending (amnt) by each resp
pay_amnt5 = pay55 %>% group_by(id) %>% summarise(sum(amnt))
head(pay_amnt5)

### Num transactions by each respondent
pay_trans5 = pay55 %>% group_by(id) %>% summarise(n())
head(pay_trans5)
summary(pay_trans5)
table(pay_trans5$`n()`)
sum(pay_trans5$`n()`)
nrow(pay55)
length(unique(pay_trans5$id))# num all respondents with both cards
length(unique(pay55$id))# num all respondents with both cards

### merging all the above into a single dataframe
pay_pi51  = merge(x = pay_trans5, y = pay_cash5, all.x = T)
pay_pi51b = merge(x = pay_pi51, y = pay_check5, all.x = T)
pay_pi52  = merge(x = pay_pi51b, y = pay_credit5, all.x = T)
pay_pi53  = merge(x = pay_pi52, y = pay_debit5, all.x = T)
pay_pi53a = merge(x = pay_pi53, y = pay_prepaid5, all.x = T)
pay_pi53b = merge(x = pay_pi53a, y = pay_amnt5, all.x = T)
dim(pay_pi53b)
head(pay_pi53b)
#
names(pay_pi53b) = c("id", "total","cash", "check ", "credit", "debit", "prepaid",  "amount")
head(pay_pi53b)

### computing the HHI concentraition measure (5 PI)
# Turning tran num intro fractions
pay_pi54 = pay_pi53b
pay_pi54$cash    = pay_pi54$cash/pay_pi54$total
pay_pi54$check   = pay_pi54$check/pay_pi54$total
pay_pi54$credit  = pay_pi54$credit/pay_pi54$total
pay_pi54$debit   = pay_pi54$debit/pay_pi54$total
pay_pi54$prepaid = pay_pi54$prepaid/pay_pi54$total
head(pay_pi54)
# Adding the HHI concentration measure
pay_pi54$hhi = pay_pi54$cash^2 + pay_pi54$check^2 + pay_pi54$credit^2 + pay_pi54$debit^2 + pay_pi54$prepaid^2
head(pay_pi54)
#
nrow(pay_pi54)# num resp who made at least 5 payments
sum(pay_pi54$total)# total num trans
summary(pay_pi54$hhi)# dist of concentration 
table(pay_pi54$hhi == 1)# num respo with HHI = 1
percent(prop.table(table(pay_pi54$hhi == 1)))# the above in %
summary(pay_pi54[pay_pi54$hhi < 1,]$hhi)# dist of concentration for resp with HHI < 1

### HHI histogram (5 pi): Bottom Figure 1 Subsection 4.4
# 
# Histogram with density
histPercent <- function(x, ...) {
  H <- hist(x, plot = FALSE, breaks = seq(0.2, 1, 0.05))
  H$density <- with(H, 100 * density* diff(breaks)[1])
  labs <- paste(round(H$density), "%", sep="")
  plot(H, freq = FALSE, labels = labs, xlim=c(0.2,1), ylim=c(0, 1.08*max(H$density)),...)# Oz added the xlim here to improve x-axis
}
par(mar = c(4.5,4,2,0))
histPercent(pay_pi54$hhi, col = "lightblue",  ylab = "Percentage of respondents", xlab = "Payment concentration (HHI) by volume with 5 payment methods", main = "")

# Information (stats) for Figure 1 bottom
# num of resp and trans in the above histograms
length(unique(pay_pi54$id))# num resp in top of Fig 1
sum(pay_pi54$total)# num trans of the above resp
summary(pay_pi54$hhi)
length((pay_pi54[pay_pi54$hhi == 1, ]$id))# num resp with HHI=1
percent(length((pay_pi54[pay_pi54$hhi == 1, ]$id)))/length(unique(pay_pi54$id))# 

length(unique(pay_pi54$id))# num resp
sum(pay_pi54$total)# num trans
median(pay_pi54$hhi)
mean(pay_pi54$hhi)

# Info for HHI = 1 
names(pay_pi54)
pay45_hhi1 = pay45[pay45$id %in% pay_pi54[pay_pi54$hhi == 1,]$id, ]
nrow(pay45_hhi1)# total num trans for HHI=1
length(unique(pay45_hhi1$id))# num resp with HHI=1
table(pay45_hhi1$pi)# Amazing! Resp with hhi=1 concentration on 3 PI only!
100*prop.table(table(pay45_hhi1$pi)) # Amazing! All those who 100% cocentrated did not concentrate on Prepaid or Checks.

### End of coding for 5 PI (Section 4.4 and Fig 1 bottom in paper) ###
