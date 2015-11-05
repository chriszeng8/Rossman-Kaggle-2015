setwd("/Users/apple1/Github_Repo/Rossman-Kaggle-2015/Script")

require(data.table)
require(h2o)
require(ggplot2)

store<-fread("../data/store.csv",stringsAsFactors = F)
test<-fread("../data/test.csv",stringsAsFactors = F)
train<-fread("../data/train.csv",stringsAsFactors = F)
sample_submission<-fread("../data/sample_submission.csv",stringsAsFactors = F)

# store test ID and delete it from original test data
testId<- test[,Id]
test[,Id:=NULL]

str(train)
head(store,5)
head(train,5)
head(test,5)

# join train and store table by "Store"
train<-merge(train,store,by="Store")
test<-merge(test,store,by="Store")

# convert date to date format
train[,Date:=as.Date(Date)]
test[,Date:=as.Date(Date)]


train[,month:=as.integer(format(Date, "%m"))]
train[,year:=as.integer(format(Date, "%y"))]

test[,month:=as.integer(format(Date, "%m"))]
test[,year:=as.integer(format(Date, "%y"))]

allDate=union(train$Date,test$Date)
allDate<- allDate-min(allDate)
allDate<-sort(allDate)
allDate<-as.Date(allDate,origin="2013-01-01")


trainDate<- data.frame(Date=train[,Date],val=0)
testDate<- data.frame(Date=test[,Date],val=1)
p <- ggplot() + geom_line(data = trainDate, aes(x=Date,y=val,color="red")) +
     geom_line(data = testDate, aes(x=Date,y=val,color="blue")) +
     xlab('date') + ylab('train(0)/test(1)')
plot(p)
# when perform cross-validation of time-series data


TestStartDate <- min(unique(test[,Date]))
TestEndDate <- max(unique(test[,Date]))
TestWindow = as.Date(TestEndDate)-as.Date(TestStartDate)
# the test data was only between "2015-08-01" and "2014-09-17"

# or shift date so that the week patterns match
shiftDate= -5
# shiftDate= 2
startDate2013 = as.Date("2013-08-01")+shiftDate
print (weekdays(startDate2013))
endDate2013 = startDate2013 + TestWindow


shiftDate= 1
startDate2014 = as.Date("2014-08-01")+shiftDate
print(weekdays(startDate2014))
endDate2014 = startDate2014 + TestWindow


subsetindex13<-((train$Date>=startDate2013)&(train$Date<=endDate2013))
sales13<- train[subsetindex13,]
dim(sales13)
unique(sales13$Date)

subsetindex14<-((train$Date>=startDate2014)&(train$Date<=endDate2014))
sales14<- train[subsetindex14,]
unique(sales14$Date)
dim(sales14)

# StoreNum = 32 for Store Num, what is causing the sales during the weekday to be 0.
# There must be some sort of variables.

date<-1:48
StoreNum = 29
logSales13<-data.frame(date = x, logSales = sales13[Store==StoreNum,log(Sales+1)],logCustomers = sales13[Store==StoreNum,log(Customers+1)])
logSales14<-data.frame(date = x, logSales = sales14[Store==StoreNum,log(Sales+1)],logCustomers = sales14[Store==StoreNum,log(Customers+1)])

# log Sales
p <- ggplot() + geom_line(data = logSales13, aes(x=date,y=logSales,color="red")) +
     geom_line(data = logSales14, aes(x=date,y=logSales,color="blue")) +
     xlab('date') + ylab('sales')
plot(p)


# find stores that open on Weekends
