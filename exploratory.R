library(ggplot2)
library(sqldf)

sales <- read.csv('train.csv')
stores <- read.csv('store.csv')
test <- read.csv('test.csv')
raw_sales <- read.csv('train.csv')

sales <- sales[sales$Open==1,]
sales$Date <- as.Date(sales$Date)
sales$Month <- months(sales$Date, abbreviate = TRUE)

#some simple scatter plots. Basically, the sales are heavily influenced by months, and store definitely also has a impact
byDayofWeek <- sqldf('select DayOfWeek, avg(Sales) as average_sale from sales group by DayofWeek')
byMonth <- sqldf('select Month, avg(Sales) as average_sale from sales group by Month')
byStore <- sqldf('select Store, avg(Sales) as average_sale from sales group by Store')

qplot(DayOfWeek, average_sale, data = byDayofWeek)
qplot(Month, average_sale, data = byMonth)
qplot(Store, average_sale, data = byStore)

byStore <- sqldf('select s.*, b.average_sale from stores s join byStore b using(Store) ')

qplot(CompetitionDistance, average_sale, data = byStore)
qplot(Promo2, average_sale, data= byStore)
qplot(StoreType, average_sale, data= byStore)
qplot(Assortment, average_sale, data= byStore)

merged_data <- sqldf('select * from sales left join stores using(Store)')
merged_data$PromoInterval <- as.character(merged_data$PromoInterval)

## create a column to indicate whether the given row is in promo2 interval
merged_data$in_promo2Month =0
for(row in 1:nrow(merged_data)){
  if(merged_data$Promo2[row] == 1 ){
    interval = strsplit(as.character(merged_data$PromoInterval[row]), ',')
    for(i in 1:length(interval[[1]])){
     if(merged_data$Month[row] == interval[[1]][i]){
       merged_data$in_promo2Month[row] = 1
      } 
    }
  }
}

new_train <- merged_data[merged_data$Month == 'Aug' | merged_data$Month == 'Sep',]
#write.csv(new_train, file = 'new_train.csv')
