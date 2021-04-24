## ------------------------------------
library('knitr')
library('data.table')
setwd("/Users/Carolyn/downloads")


## ------------------------------------
## Make the dates more manageable by getting rid of the timestamps and turning them into the month of the date

date = phq_all_final$date
date = substring(date, 6, 7)
date = as.numeric(date)

date_created = phq_all_final$patient_date_created
date_created = substring(date_created, 6, 7)
date_created = as.numeric(date_created)


## ------------------------------------
## Find the mean score conditional on the amount of time (# of months) elapsed from when the user was first created to when the user was assessed

sapply(split(phq_all_final$score, date - date_created), mean)


## ------------------------------------
## Add new columns to data frame that has the month created, month assessed, and months passed between creation and assessment

phq_all_final$month_created = date_created
phq_all_final$month_assessed = date

months_passed = date - date_created
phq_all_final$months_passed = months_passed


## ------------------------------------
## Find the 95% confidence interval for the score on the assessment when months passed = 0 to examine whether or not the differences in mean score between when months passed does not = 0 and when it does = 0 is statistically significant

t.test(phq_all_final$score[phq_all_final$months_passed == 0], alternative = "two.sided", paired = FALSE, conf.level = 0.95)

## ------------------------------------
## Test the same thing as above but at the 90% confidence level to see if the differences in mean are still statistically significant

t.test(phq_all_final$score[phq_all_final$months_passed == 0], alternative = "two.sided", paired = FALSE, conf.level = 0.90)


## ------------------------------------
## Count how many users fall into each value of months passed to examine if our conclusions are justified -- if "sample" size is big enough

sum(months_passed == 0)
sum(months_passed == 1)
sum(months_passed == 2)
sum(months_passed == 3)
sum(months_passed == 4)
sum(months_passed == 5)
sum(months_passed == 6)

## If months_passed = -6, the user was assessed 6 months after created (just in the next year), if months_passed = -7, the user was assessed 5 months after created (just in the next year), etc.

sum(months_passed == -6)
sum(months_passed == -7)
sum(months_passed == -8)
sum(months_passed == -9)
sum(months_passed == -10)
sum(months_passed == -11)

