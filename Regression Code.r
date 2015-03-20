plot(rpm)
plot(rpm$organics~rpm$paid)
abline(rpm)
hist(resid(rpm.organics))

ma <- function(arr, n = 5){   ##calculate moving average
  res = arr
  for (i in n:length(arr)){
    res[i] = mean(arr[(i-n):i])
  }
  res
}
##################################
###past 6 months dataset
rm(list = ls(all = TRUE))
rpm=read.csv("regr.csv")
ls(rpm)
dim(rpm)
names(rpm)
summary(rpm)
organics <-rpm$organics
MA <- filter(organics/5, rep(1,5))
paid <-rpm$paid
model <-lm(MA~paid)
summary(model)
plot(paid, MA)
library(car)
scatterplot(MA~paid)
qqplot(paid, MA)

##date conversion
rpm$date_test <- as.Date(rpm$Date, format='%m/%d/%Y')
rpm$date_test

##subset
test <- rpm$date_test >= as.Date("2014-11-02") & rpm$date_test <= as.Date("2014-12-10")
test

##subset
newdata <- subset (rpm, rpm$date_test > as.Date("2014-10-10"))
newdata

###############start here
rpm=read.csv("bee.csv")
rpm$Date <- as.Date(rpm$Date, format='%m/%d/%Y')
rpm$Date

model = lm(rpm$MA~rpm$paid+rpm$holiday)
summary(model)
sqrt(diag(vcov(model))  ##std error
model$coef     
     

fullmodel = lm(rpm$MA~rpm$paid+rpm$cpi+rpm$holiday)
summary(fullmodel)
sqrt(diag(vcov(fullmodel))  ##std error
fullmodel$coef
     
####cpi start from 2014-11-03
start_date = as.Date('2014-11-03')
end_date = as.Date('2015-02-08')

start_dates = list(as.Date('2014-07-01'),as.Date('2014-08-01'), as.Date('2014-09-01'),as.Date('2014-10-01'),as.Date('2014-11-01'),as.Date('2014-12-01'),as.Date('2015-01-01'),as.Date('2015-02-01'))
end_dates  = list(as.Date('2014-07-31'),as.Date('2014-08-31'),as.Date('2014-09-30'),as.Date('2014-10-31'), as.Date('2014-11-30'),as.Date('2014-12-31'), as.Date('2015-01-31'))
start_dates = list(1,2,3)
length(start_dates)
length(end_dates)

#######################################################
outputCoef <- function(start_dates, end_dates) {
  for (i in 1:length(start_dates)) {
    startdate = (start_dates[i])
    cat(sprintf("start_dates is: %s\n ",startdate))
    print(start_dates[i])
  }
  for(j in 1:length(end_dates)){
    print(end_dates[j])
  }
}
outputCoef(start_dates,end_dates)
outputCoef(start_dates, '')

###################################################this one works
rpm <- rpm[rpm$paid < 9000,]
outputCoef <- function(start_dates, end_dates) {
  for (i in 1:length(start_dates)) {
    startdate = (start_dates[[i]])
    print(sprintf("========================================"))
    cat(sprintf("start_dates is: %s\n ",startdate))
    m <- lm(MA ~ paid + holiday, data=rpm[rpm$Date >= startdate,])
    #summary(m)
    scatterplot(rpm[rpm$Date >= startdate,]$paid,rpm[rpm$Date >= startdate,]$MA, main=startdate)
    print("Coefficient")
    coefficient <- m$coef
    print(coefficient)
    print("standard error")
    stderror <- sqrt(diag(vcov(m)))
    print(stderror)
  #  return(c(coefficient,stderror))
  }
}
outputCoef(start_dates,end_dates)
summary(lm(MA ~ paid  + holiday, data=rpm[rpm$Date >= as.Date('2014-07-01'),]))
######################################################
rpm <- rpm[rpm$paid < 9000,]  ##exclude outlier
start_dates = list(as.Date('2014-11-01'),as.Date('2014-12-01'),as.Date('2015-01-01'),as.Date('2015-02-01'))
outputCoef <- function(start_dates, end_dates) {
  for (i in 1:length(start_dates)) {
    startdate = (start_dates[[i]])
    print(sprintf("========================================"))
    cat(sprintf("start_dates is: %s\n ",startdate))
    m <- lm(MA ~ paid +  holiday + UA, data=rpm[rpm$Date >= startdate,])
    #summary(m)
    scatterplot(rpm[rpm$Date >= startdate,]$paid,rpm[rpm$Date >= startdate,]$MA, main=startdate)
    print("Coefficient")
    coefficient <- m$coef
    print(coefficient)
    print("standard error")
    stderror <- sqrt(diag(vcov(m)))
    print(stderror)
    #  return(c(coefficient,stderror))
  }
}
outputCoef(start_dates,end_dates)

##from july upto now
summary(lm(MA ~ paid + holiday, data=rpm[rpm$Date >= as.Date('2014-08-01'),]))
scatterplot(rpm[rpm$Date >= as.Date('2014-08-01'),]$paid,rpm[rpm$Date >= as.Date('2014-08-01'),]$MA, main=('2014-08-01 forward'))
summary(lm(MA ~ paid + holiday, data = rpm[rpm$Date >= as.Date('2014-08-01') & rpm$Date < as.Date('2014-11-24') ,]))
scatterplot(rpm[rpm$Date >= as.Date('2014-08-01') & rpm$Date < as.Date('2014-11-24') ,]$paid,rpm[rpm$Date >= as.Date('2014-08-01') & rpm$Date < as.Date('2014-11-24') ,]$MA, main=('2014-08-01 to 2014-11-24'))
summary(lm(MA ~ paid + holiday, data = rpm[rpm$Date >= as.Date('2014-11-24') & rpm$Date < as.Date('2014-12-26') ,]))
scatterplot(rpm[rpm$Date >= as.Date('2014-11-24') & rpm$Date < as.Date('2014-12-26') ,]$paid,rpm[rpm$Date >= as.Date('2014-11-24') & rpm$Date < as.Date('2014-12-26') ,]$MA, main=('2014-11-24 to 2014-12-26'))
summary(lm(MA ~ paid + holiday, data = rpm[rpm$Date >= as.Date('2014-11-24') ,]))
##from Dec upto now
summary(lm(MA ~ paid + holiday, data=rpm[rpm$Date >= as.Date('2014-12-01'),]))
scatterplot(rpm[rpm$Date >= as.Date('2014-12-01'),]$paid,rpm[rpm$Date >= as.Date('2014-12-01'),]$MA, main=('2014-12-01 forward'))

summary(lm(holiday~cpi,data=rpm[rpm$Date >= as.Date('2014-12-01'),]))
########################################################

m <- lm(MA ~ paid, data=rpm[rpm$Date >= start_date & rpm$Date <= end_date,])

model$coef
m$estimate
rpm[rpm$Date >= start_date & rpm$Date <= end_date,]
rpm

subset[rpm$Date > = i & rpm$Date < = j, ]

names(rpm)
hist(rpm$MA)
plot(rpm$paid, rpm$MA)
qqplot(rpm$paid, rpm$MA)
