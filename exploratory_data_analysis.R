library(data.table)
library(magrittr)
library(ggplot2)



# bank-full examine each feature ===============================================
dat <- fread("bank/bank-full.csv")

# age is slightly skewed
hist(dat$age)


# 0.6% missing values (unknown), cannot be filled with any value as no 
# dominant one
job_count <- sort(table(dat$job), decreasing = TRUE)
barplot(job_count)


# 27214 married, 12790 single, 5207 devorces
table(dat$marital)


# 4.1% missing (unknown), no dominant value to fill
table(dat$education)


# 1.8% have credit default history, highly unbalanced by important, stratify!!!!
table(dat$default)

# yearly balance is highly skewed and with outlier
boxplot(dat$balance)
hist(dat$balance, breaks = 100)


# balanced with and without housing loans
table(dat$housing)


# 16% have personal loans
table(dat$loan)


# phone type, 6% using telephone (landline?), 29% missing, others cell phone
table(dat$contact)


# last contact day and month, no year information so no weekday data ????????
barplot(table(dat$day))
barplot(table(dat$month))


# last contact duration, highly skewed with outlier
boxplot(dat$duration)
hist(dat$duration, breaks = 100)


# number of contact, highly skewed
table(dat$campaign)
hist(dat$campaign, breaks = 100)


# number of day from last contact in previous campain, shows 4 previous campains
# 68% are -1, means not contacted in last campain
# put into brackets
table(dat$pdays)
hist(dat$pdays, breaks = 100)
hist(dat$pdays, breaks = 100, ylim = c(0, 1000))

# previous: number of contacts performed before this campaign and for this 
# client, highly skewed 
hist(dat$previous, breaks = 100)


# poutcome: outcome of the previous marketing campaign (categorical: "unknown",
# "other","failure","success")
# 11% failure, 3.3% success, others missing (other and unknown)
table(dat$poutcome)


# target outcome, 9.8 subscribed to the term deposit, unbalanced
table(dat$y)


# .. correlations between features ====
num_select <- dat[sample(1:nrow(dat), 1000), 
                  .(age, balance, duration, campaign, pdays, previous)]
pairs(num_select)

# pdays and previous correlation coef 0.57
cor(num_select)
plot(dat$previous, dat$pdays, xlim = c(0, 50))
ggplot(dat, aes(as.factor(previous), pdays)) +
    geom_boxplot() +
    geom_jitter(width = 0.1, size = 1)


# bank-additional-full examine features ========================================


dat2 <- fread("bank-additional/bank-additional-full.csv") %>%
    .[y == "yes", y := 1] %>%
    .[y == "no", y := 0] %>%
    .[, y := as.integer(y)]


# age, skewed, sharp drop above 60
hist(dat2$age, breaks = 100)
plot_avg(dat2, "age")

# job, 0.8% missing, unemployed and student are special
job_count <- sort(table(dat2$job), decreasing = T)
barplot(job_count)
plot_avg(dat2, "job")

# marital, 0.2% missing
table(dat2$marital)
plot_avg(dat2, "marital")

# education, 4% missing 
table(dat2$education)
plot_avg(dat2, "education")

# credit default? large unknow, only 3 reported default, assuming those unknown having
# default??????
table(dat2$default)
plot_avg(dat2, "default")

# housing loan? 2.4% unknown
table(dat2$housing)
plot_avg(dat2, "housing")

# personal loan? 2.4% unknown
table(dat2$loan)
plot_avg(dat2, "loan")

# contact type: cell phone or landline
table(dat2$contact)
plot_avg(dat2, "contact")


# month of last contact
table(dat2$month)
plot_avg(dat2, "month")


# day_of_week
table(dat2$day_of_week)
plot_avg(dat2, "day_of_week")


# .. number of contact for this campain  !!!! ====
table(dat2$campaign)
plot_avg(dat2, "campaign")

# days since last contact in a previous compaign, 999 if never contacted in a 
# previous campaign
# 3.7% contacted from previous campaign and the outcome is too good to be true!!!!
table(dat2$pdays)
plot_avg(dat2, "pdays") + xlim(0, 30)

# number of contact in previous campaigns, numbers does not add up to pdays
table(dat2$previous)
plot_avg(dat2, "previous")

# outcome of previous campaign, previous success is great indicator
table(dat2$poutcome)
plot_avg(dat2, "poutcome")


# employment variation rate ????? (quarterly)
table(dat2$emp.var.rate)
plot_avg(dat2, "emp.var.rate")


# consumer price index (monthly)
table(dat2$cons.price.idx)
plot_avg(dat2, "cons.price.idx")

# consumer confidence index (monthly)
table(dat2$cons.conf.idx)
plot_avg(dat2, "cons.conf.idx")


# uribor three month rate (daily)
table(dat2$euribor3m)
plot_avg(dat2, "euribor3m")


# number of employees (quaterly)
table(dat2$nr.employed)
plot_avg(dat2, "nr.employed")
