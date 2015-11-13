rm(list=ls())

require(data.table)
require(ggplot2)

# read in data
store<-fread("../data/store.csv",stringsAsFactors = F)
test<-fread("../data/test.csv",stringsAsFactors = F)
train<-fread("../data/train.csv",stringsAsFactors = F)
sample_submission<-fread("../data/sample_submission.csv",stringsAsFactors = F)

# join train and store table by "Store"
train<-merge(train,store,by="Store")
test<-merge(test,store,by="Store")

# convert date to date format
train[,Date:=as.Date(Date)]
test[,Date:=as.Date(Date)]

# the test data was only between "2015-08-01" and "2015-09-17"
TestStartDate <- min(unique(test[,Date]))
print (weekdays(TestStartDate))
TestEndDate <- max(unique(test[,Date]))
TestWindow = as.Date(TestEndDate)-as.Date(TestStartDate)

# shift date so that the Sales patterns match the test data time window
shiftDate= -5
startDate2013 = TestStartDate-365*2 +shiftDate
print (weekdays(startDate2013))
endDate2013 = startDate2013 + TestWindow
cat(paste('2013 Sales from ',startDate2013,'to',endDate2013))

shiftDate= 1
startDate2014 = TestStartDate-365 +shiftDate
print(weekdays(startDate2014))
endDate2014 = startDate2014 + TestWindow
cat(paste('2014 Sales from ',startDate2014,'to',endDate2014))

# sort the data frame in reversely chronological order
subsetindex13<-((train$Date>=startDate2013)&(train$Date<=endDate2013))
sales13<- train[subsetindex13,]

subsetindex14<-((train$Date>=startDate2014)&(train$Date<=endDate2014))
sales14<- train[subsetindex14,]

# StoreNum = 32 for Store Num, what is causing the sales during the weekday to be 0.
# There must be some sort of variables.

AllStores <- store$Store
SelectStore <-sample(AllStores,6)

testdates = seq(from=TestEndDate,to=TestStartDate,by=-1)
plots <- list()
counter=0
for (StoreNum in SelectStore) {
  counter=counter+1
  print (paste('store number:',StoreNum))
  if (length(sales13[Store==StoreNum,log(Sales+1)])>0) {
    logSales13<-data.frame(date = testdates, logSales = sales13[Store==StoreNum,log(Sales+1)],logCustomers = sales13[Store==StoreNum,log(Customers+1)])
  } else {
    logSales13<-data.frame(date = testdates, logSales = 0,logCustomers = 0)
  }
  if (length(sales14[Store==StoreNum,log(Sales+1)])>0) {
    logSales14<-data.frame(date = testdates, logSales =sales14[Store==StoreNum,log(Sales+1)],logCustomers = sales14[Store==StoreNum,log(Customers+1)])
  } else {
    logSales14<-data.frame(date = testdates, logSales=0,logCustomers = 0)
  }
  
     p <- ggplot() + geom_line(data = logSales13, aes(x=date,y=logSales,color="Sales 2013")) +
       geom_line(data = logSales14, aes(x=date,y=logSales,color="Sales 2014")) +
       xlab('date') + ylab('log(sales+1)')+labs(title=paste("Store",StoreNum))
    plots[[counter]]<-p
}

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


multiplot(plotlist=plots, cols=2)








test[Id%in%ZeroSalesID,]
test[Id%in%ZeroSalesID,Sales:=0]

p<-qplot(Date,Sales,data=test)
submission<-data.frame(Id = test$Id, Sales = test$Sales)
write.csv(submission,file='submission1.csv',row.names = F)



# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#


multiplot(plotlist = plots, cols = 2)
