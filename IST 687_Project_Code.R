library(ggplot2)
library(tidyverse)
library(maps)
library(ggmap)
library(mapproj)
library(RCurl)
library(jsonlite)
library(MASS)
library(dplyr)
library(car)
library(arules)
library(arulesViz)
library(kernlab)
library(caret)
library(e1071)
library(rpart)
library(rpart.plot)
library(randomForest)
library(xgboost)
library(rworldmap)
library(viridis)
library(hrbrthemes)


hotel_data <- read_csv("https://intro-datascience.s3.us-east-2.amazonaws.com/Resort01.csv")

#Histograms of Numeric tables
hist(hotel_data$LeadTime)
hist(hotel_data$StaysInWeekendNights)
hist(hotel_data$StaysInWeekNights)
hist(hotel_data$TotalOfSpecialRequests)

#Histogram for columns with Character Strings
plot(as.factor(hotel_data$MarketSegment))
plot(as.factor(hotel_data$IsCanceled))
plot(as.factor(hotel_data$Country))
plot(as.factor(hotel_data$CustomerType))
plot(as.factor(hotel_data$DepositType))
plot(as.factor(hotel_data$AssignedRoomType))
plot(as.factor(hotel_data$ReservedRoomType))
plot(as.factor(hotel_data$IsRepeatedGuest))

#Checking for NUll and NA values
hotel_data[is.na(hotel_data$PreviousBookingsNotCanceled),]
hotel_data[is.null(hotel_data$Country),]

#Boxplot for weekend Night bookings with Deposit Type
dev.new()
hotel_data %>%
  ggplot( aes(x=DepositType,y=StaysInWeekendNights)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    
    legend.position="none",
    plot.title = element_text(size=15)
  ) +
  ggtitle("Boxplot for weekend Night bookings with Deposit Type") +
  xlab("Deposit Type") +ylab("Weekend Nights Bookings")



# Visualizing distribution of babies through Histogram 
dev.new()
babies <- ggplot(hotel_data, aes(x=Babies))+geom_histogram(binwidth = 1)+ ggtitle("Distribution of Babies Variable")+xlab("Babies")
babies
dev.off()

not_cancelled <- subset(hotel_data, IsCanceled==0)
#Q. Calculating no. of non-cancellations stays in weekend nights
sum(not_cancelled$StaysInWeekendNights) 
#32813

#Q. Calculating no. of non-cancellations stays in week nights
sum(not_cancelled$StaysInWeekNights)
#87074
#therefore we come to know that people prefer to come to hotel on week nights instead of weekend nights.


#boxplot for bookings in week nights in each market Segment with assigned room type
dev.new()
hotel_data %>%
  ggplot( aes(x=MarketSegment, y=StaysInWeekNights, fill=AssignedRoomType)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    
    legend.position="right",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Basic boxplot") +
  xlab("Market Segment") +ylab("Weekend Nights Bookings")


# Visualizing distribution of Adults through Histogram
dev.new()
adults <- ggplot(hotel_data , aes(x=Adults))+geom_histogram(binwidth = 5)+ ggtitle("Distribution of Adults Variable")+xlab("Adults")+ theme(plot.title = element_text(hjust = 0.5))
adults

#Basic Scatter plots
plot1 <- ggplot(hotel_data)
plot1 <- plot1 + aes(x=ReservedRoomType, y=AssignedRoomType)
plot1 <- plot1 + geom_point()
plot1

plot2 <- ggplot(hotel_data)
plot2 <- plot2 + aes(x=LeadTime, y=BookingChanges)
plot2 <- plot2 + geom_line()
plot2

plot3 <- ggplot(hotel_data)
plot3 <- plot3 + aes(x=ReservedRoomType, y=TotalOfSpecialRequests)
plot3 <- plot3 + geom_point()
plot3

plot4 <- ggplot(hotel_data)
plot4 <- plot4 + aes(x=Country, y=MarketSegment)
plot4 <- plot4 + geom_point()
plot4

#fetching data 
hotel_data <- read_csv("https://intro-datascience.s3.us-east-2.amazonaws.com/Resort01.csv")

view(hotel_data)
str(hotel_data)

#checking for NA values
colSums(is.na(hotel_data))


#Summary of data
summary(hotel_data)


#dividing data w.r.t cancellation 

hotel_cancelled <- subset(hotel_data, hotel_data$IsCanceled==1)
hotel_notcancelled <- subset(hotel_data, hotel_data$IsCanceled == 0)
view(hotel_cancelled)
view(hotel_notcancelled)

#basic plots for explanatory analysis
plot(as.factor(hotel_data$MarketSegment), ylab= 'Frequency')
plot(as.factor(hotel_data$DepositType), ylab= 'Frequency')
plot(as.factor(hotel_data$CustomerType), ylab= 'Frequency')

#basic boxplot to find outliers
boxplot(hotel_data$Children, ylab="Child Count", main='Boxplot of # of Childrens')
hist(hotel_data$Children)
boxplot(hotel_data$Adults, ylab="Adults Count ", main='Boxplot of # of Adults')
boxplot(hotel_data$LeadTime, ylab="No. of days", main='Boxplot of lead time')
boxplot(hotel_data$StaysInWeekNights, ylab='Stay on weekdays', main='Boxplot of Stay in week nights')
summary(hotel_data)
mean(hotel_data$StaysInWeekNights)
boxplot(hotel_data$StaysInWeekendNights, ylab='Stay on weekends', main='Boxplot of Stay on weekends')
mean(hotel_data$StaysInWeekendNights)

#room-type prerefence
table(hotel_data$ReservedRoomType)
table(hotel_data$DepositType)
table(hotel_data$CustomerType)

#repeated guest
repeated_guest <- subset(hotel_data, hotel_data$IsRepeatedGuest == 1)
nonrepeated <- subset(hotel_data, hotel_data$IsRepeatedGuest == 0)


sum(hotel_data$PreviousCancellations)
sum(repeated_guest$PreviousCancellations)
sum(nonrepeated$PreviousCancellations)

#weekdays vs. weekends
sum(hotel_data$StaysInWeekendNights)
sum(hotel_data$StaysInWeekNights)

hotel_data$totalstay <- hotel_data$StaysInWeekendNights + hotel_data$StaysInWeekNights
hist(hotel_data$totalstay, ylim = c(0,40000), xlim = c(0,20), xlab = 'Total Stay', main = 'Freq of total stay of a customer')


#boxplots

boxplot(IsCanceled ~ MarketSegment, data = hotel_data,xlab = 'Market Segment',ylab='Cancellation Data', col='green')
boxplot(IsCanceled ~ IsRepeatedGuest, data = hotel_data, xlab = 'Repeated Guest',ylab='Cancellation Data', col='red')
boxplot(IsCanceled~ CustomerType, data=hotel_data, ylab='Cancellation Data', col='blue')


#fetching data 
hotel_data <- read_csv("https://intro-datascience.s3.us-east-2.amazonaws.com/Resort01.csv")

ggplot(hotel_data,aes(x=factor(AssignedRoomType),fill=factor(IsCanceled))) + geom_bar() + theme(axis.text.x = element_text(face="bold", size=15),axis.text.y = element_text(face="bold", size=15)) +
  
  labs(
    title = "Cancellation based on the Room Type",
    x = "Room Type",
    y = "No. of Bookings",size=15
  ) +
  
  scale_fill_manual(
    name = "Booking Status",
    breaks = c("0", "1"),
    labels = c("Not Cancelled", "Cancelled"),
    values = c("0" = "red", "1"="yellow")
  )


library(ggplot2)
ggplot(hotel_data,aes(x=factor(MarketSegment),fill=factor(IsCanceled)))+
  
  geom_bar()+theme(axis.text.x = element_text(face="bold", size=15),axis.text.y = element_text(face="bold", size=15))+
  
  labs(
    title = "Cancellation based on market segment Type",
    x = "Distribution Type",
    y = "No. of Bookings",size=15) +
  
  scale_fill_manual(
    name = "Booking Status",
    breaks = c("0", "1"),
    labels = c("Not Cancelled", "Cancelled"),
    values = c("0" = "red", "1"="green")
  )


# Table cylinder - transmission type
table2 <- table(hotel_data$DepositType, hotel_data$IsCanceled)

barplot(table2,
        main = "Grouped barchart for DepositType",
        xlab = "IsCancelled", ylab = "Frequency",
        col = c("darkgrey", "darkblue", "red"),
        legend.text = rownames(table2),
        beside = TRUE) # Grouped bars



#Table cylinder - transmission type
table3 <- table(hotel_data$MarketSegment, hotel_data$DepositType)

barplot(table3,
        main = "Grouped barchart for MarketSegment",
        xlab = "Deposit Type", ylab = "Frequency",
        col = c("darkgrey", "darkblue", "red", "green", "yellow", "orange"),
        legend.text = rownames(table3),
        beside = TRUE) # Grouped bars
#
table4 <- table(hotel_data$DepositType, hotel_data$CustomerType)

barplot(table4,
        main = "Grouped barchart for Booking Changes",
        xlab = "Deposit Type", ylab = "Frequency",
        col = c("blue", "green", "yellow"),
        legend.text = rownames(table4),
        beside = TRUE) # Grouped bars

#Table cylinder - transmission type
table5 <- table(hotel_data$AssignedRoomType, hotel_data$CustomerType)

barplot(table5,
        main = "Grouped barchart for AssignedRoomType",
        xlab = "IsCancelled", ylab = "Frequency",
        col = c("yellow", "red", "orange", "cyan", "grey", "violet", "green", "brown", "slateblue", "blue","orchid"),
        legend.text = rownames(table5),
        beside = TRUE) # Grouped bars


#
library(ggplot2)
# Basic barplot
ggplot(data=hotel_data, aes(x=IsCanceled, y=PreviousCancellations)) + geom_bar(stat="identity")

comp1 <- aggregate(x= hotel_data$IsCanceled, by = list(hotel_data$LeadTime), FUN = mean)
plot(comp1, xlab = "Unique values in LeadTime", ylab = "Average of IsCanceled for each unique value in LeadTime", main = "Unique LeadTime VS Avg of IsCanceled")
View(comp1)
#This graph gives us the average value of IsCanceled vs all unique values of Lead Time. 
#From this scatter plot it can be seen that for some of the values in Lead time the average value of IsCanceled is above 0.5, this indicates that particular unique value in LeadTime has a lot of cancellations. In future it can be marked as an indicator.

comp3 <- aggregate(x= hotel_data$IsCanceled, by = list(hotel_data$StaysInWeekendNights), FUN = mean)
plot(comp3, xlab = "Unique values in StaysInWeekendNights", ylab = "Average of IsCanceled for each unique value in StaysInWeekendNights", main = "Unique StaysInWeekendNights VS Avg of IsCanceled")
View(comp3)
#Similarly in this graph we have the same scenario but in this graph we evidently see that the scale is upto 0.6 which means that we cannot deduce according to this as there are extremely low amount of unique values in StaysInWeekendNights

comp4 <- aggregate(x= hotel_data$IsCanceled, by = list(hotel_data$StaysInWeekNights), FUN = mean)
plot(comp4, xlab = "Unique values in StaysInWeekNights", ylab = "Average of IsCanceled for each unique value in StaysInWeekNights", main = "Unique StaysInWeekNights VS Avg of IsCanceled")
View(comp4)
#Similarly in this graph we have the same scenario but in this graph we evidently see that the scale is upto 1 which means that we can deduce (not certainly) according to the unique values in StaysInWeekNights for which bookings are cancelled.

comp5 <- aggregate(x= hotel_data$IsCanceled, by = list(as.factor(hotel_data$MarketSegment)), FUN = mean)
plot(comp5, xlab = "Unique values in StaysInWeekendNights", ylab = "Average of IsCanceled for each unique value in StaysInWeekendNights", main = "Unique StaysInWeekendNights VS Avg of IsCanceled", type = "p") 

#map
hotel_data_Canceled_1 <- subset(hotel_data, IsCanceled == 1)
hotel_data_Canceled <- subset(hotel_data_Canceled_1, Country !='NULL')
#dev.new()
map1_data <- aggregate(x= hotel_data_Canceled$IsCanceled, by = list(hotel_data_Canceled$Country), FUN = sum)
sPDF <- joinCountryData2Map( map1_data
                             ,joinCode = "ISO3"
                             ,nameJoinColumn = "Group.1",
                             nameCountryColumn = "Country")
mapCountryData(sPDF, nameColumnToPlot="x", colourPalette = "heat" ,addLegend = TRUE, aspect = 1, missingCountryCol = NA, add = FALSE)

dev.new()
map2_data <- aggregate(x= hotel_data_Canceled$StaysInWeekendNights, by = list(hotel_data_Canceled$Country), FUN = sum)
sPDF1 <- joinCountryData2Map( map2_data
                              ,joinCode = "ISO3"
                              ,nameJoinColumn = "Group.1",
                              nameCountryColumn = "Country")
mapCountryData(sPDF1, nameColumnToPlot="x", colourPalette = "heat" ,addLegend = TRUE, aspect = 1, missingCountryCol = NA, add = FALSE)

dev.new()
map3_data <- aggregate(x= hotel_data_Canceled$StaysInWeekNights, by = list(hotel_data_Canceled$Country), FUN = sum)
sPDF3 <- joinCountryData2Map( map3_data
                              ,joinCode = "ISO3"
                              ,nameJoinColumn = "Group.1",
                              nameCountryColumn = "Country")
mapCountryData(sPDF3, nameColumnToPlot="x", colourPalette = "heat" ,addLegend = TRUE, aspect = 1, missingCountryCol = NA, add = FALSE)

dev.new()
map4_data <- aggregate(x= hotel_data_Canceled$BookingChanges, by = list(hotel_data_Canceled$Country), FUN = sum)
sPDF4 <- joinCountryData2Map( map4_data
                              ,joinCode = "ISO3"
                              ,nameJoinColumn = "Group.1",
                              nameCountryColumn = "Country")
mapCountryData(sPDF4, nameColumnToPlot="x", colourPalette = "heat" ,addLegend = TRUE, aspect = 1, missingCountryCol = NA, add = FALSE)

#Linear Regression  for all the variables
linOut_1 <- lm(IsCanceled ~ LeadTime , data = hotel_data)
summary(linOut_1)
plot(linOut_1) #Gives various plots for regression


linOut_2 <- lm(IsCanceled ~ StaysInWeekendNights, data = hotel_data)
summary(linOut_2)

linOut_3 <- lm(IsCanceled ~ StaysInWeekNights, data = hotel_data)
summary(linOut_3)

linOut_4 <- lm(IsCanceled ~ Adults, data = hotel_data)
summary(linOut_4)

linOut_5 <- lm(IsCanceled ~ Children, data = hotel_data)
summary(linOut_5)

linOut_6 <- lm(IsCanceled ~ Babies, data = hotel_data)
summary(linOut_6)

linOut_7 <- lm(IsCanceled ~ Meal, data = hotel_data)
summary(linOut_7)

linOut_8 <- lm(IsCanceled ~ Country, data = hotel_data)
summary(linOut_8)

linOut_9 <- lm(IsCanceled ~ MarketSegment, data = hotel_data)
summary(linOut_9)

linOut_10 <- lm(IsCanceled ~ IsRepeatedGuest, data = hotel_data)
summary(linOut_10)

linOut_11 <- lm(IsCanceled ~ PreviousCancellations , data = hotel_data)
summary(linOut_11)

linOut_12 <- lm(IsCanceled ~ PreviousBookingsNotCanceled, data = hotel_data)
summary(linOut_12)

linOut_13 <- lm(IsCanceled ~ ReservedRoomType, data = hotel_data)
summary(linOut_13)

linOut_14 <- lm(IsCanceled ~ AssignedRoomType, data = hotel_data)
summary(linOut_14)

linOut_15 <- lm(IsCanceled ~ BookingChanges, data = hotel_data)
summary(linOut_15)

linOut_16 <- lm(IsCanceled ~ DepositType, data = hotel_data)
summary(linOut_16)

linOut_17 <- lm(IsCanceled ~ CustomerType, data = hotel_data)
summary(linOut_17)

linOut_18 <- lm(IsCanceled ~ RequiredCarParkingSpaces, data = hotel_data)
summary(linOut_18)

linOut_19 <- lm(IsCanceled ~ TotalOfSpecialRequests, data = hotel_data)
summary(linOut_19)

# Multiple Regression Data model with predictions

hotel_data <- read_csv("https://intro-datascience.s3.us-east-2.amazonaws.com/Resort01.csv")

linOut <- lm(IsCanceled ~ ., data = hotel_data)
summary(linOut)
# We can observe when we select all the columns for prediction the Adjusted R-Squared value is 0.3831
# which generally translates to 38.31% accuracy which is not very good, we can try to maximize the accuracy by keeping 
# the factors which are significant. In this model we can see not many countries are significant in gauging the 
# if the booking is going to be cancelled. We can eliminate counties from out next model. 
# Similarly, we can also eliminate Meals, MarketSegment, Babies and CustomerType as they are not very significant in this model.

linOut1 <- lm(IsCanceled ~ LeadTime + StaysInWeekendNights + StaysInWeekNights + Adults + Children + IsRepeatedGuest + PreviousCancellations + PreviousBookingsNotCanceled + ReservedRoomType + AssignedRoomType + BookingChanges + DepositType + RequiredCarParkingSpaces + TotalOfSpecialRequests , data = hotel_data)
summary(linOut1)
# Even though we eliminated some of the insignificant variables we still see that the accuracy keeps on dropping this is due to the linear modelling. 
# In linear/ multiple prediction models the number of inputs (predictors) with respect to the data given to the model also play a big role in the accuracy/ adjusted R-squared value. 
# As we reduce the number of inputs even though all the variables are significant we can see that the accuracy goes down considerably.
# R-squared value 0.2266 which translates to 22.66% accuracy of this model.
# In order to have better insights using linear modelling we can subset data according to categories and try to 
# maximize accuracy of the model which in turn will translate into better prediction model


hotel_data_lt <- subset(hotel_data, LeadTime > 60)
linOut_lt <- lm(IsCanceled ~ ., data = hotel_data_lt)
summary(linOut_lt)
plot(linOut_lt)
# If we subset using LeadTime > 60 we get a more efficient model which us an R-squared value of 0.49 which 
# Translates to accuracy of 49% which is considerably higher than the previous model.


hotel_data_lt1 <- subset(hotel_data, LeadTime > 200)
linOut_lt1 <- lm(IsCanceled ~ ., data = hotel_data_lt1)
summary(linOut_lt1)
plot(linOut_lt1)
# If we increase the LeadTime to above 200 in our subset we get an even higher accuracy which tells us that the more
# the LeadTime the more accurate the prediction. The accuracy is 57.78%


hotel_data_lt2 <- subset(hotel_data, StaysInWeekNights > 5)
linOut_lt2 <- lm(IsCanceled ~ ., data = hotel_data_lt2)
summary(linOut_lt2)
plot(linOut_lt2)



#svm modelling
hotel_data_fac <- data.frame(IsCanceled = as.factor(hotel_data$IsCanceled),
                             LeadTime = as.factor(hotel_data$LeadTime),
                             StaysInWeekendNights = as.factor(hotel_data$StaysInWeekendNights),
                             StaysInWeekNights = as.factor(hotel_data$StaysInWeekNights),
                             Adults = as.factor(hotel_data$Adults),
                             Children = as.factor(hotel_data$Children),
                             Babies = as.factor(hotel_data$Babies),
                             Meal = as.factor(hotel_data$Meal),
                             Country = as.factor(hotel_data$Country),
                             MarketSegment = as.factor(hotel_data$MarketSegment),
                             IsRepeatedGuest = as.factor(hotel_data$IsRepeatedGuest),
                             PreviousCancellations = as.factor(hotel_data$PreviousCancellations),
                             PreviousBookingsNotCanceled = as.factor(hotel_data$PreviousBookingsNotCanceled),
                             ReservedRoomType = as.factor(hotel_data$ReservedRoomType),
                             AssignedRoomType = as.factor(hotel_data$AssignedRoomType),
                             BookingChanges = as.factor(hotel_data$BookingChanges),
                             DepositType = as.factor(hotel_data$DepositType),
                             CustomerType = as.factor(hotel_data$CustomerType),
                             RequiredCarParkingSpaces = as.factor(hotel_data$RequiredCarParkingSpaces),
                             TotalOfSpecialRequests = as.factor(hotel_data$TotalOfSpecialRequests))

CanceledBook <- table(hotel_data_fac$IsCanceled)
prop.table(CanceledBook) #27.76335 Canceled Bookings

hotel_data_tran <- as(hotel_data_fac, "transactions")
itemFrequency(hotel_data_tran)
itemFrequencyPlot(hotel_data_tran)

set.seed(11)
trainList1 <- createDataPartition(y=hotel_data_fac$IsCanceled,p=.30,list=FALSE)

trainSet1 <- hotel_data_fac[trainList1,]
testSet1 <- hotel_data_fac[-trainList1,]

dim(trainSet1)
dim(testSet1)

#Model with train Control. This model take hours to execute which is why its not suggestable at this given point.
trctrl <- trainControl(method="repeatedcv", number = 10, repeats = 5)
svm.model1 <- train(IsCanceled  ~ ., data = trainSet1, method = "svmRadial", trControl = trctrl, preProcess = c("center", "scale"), tuneLength = 10)

#SVM model without trainControl
svmModel_1 <- ksvm(IsCanceled ~ ., data=trainSet1, C=5, cross = 3, prob.model = TRUE)
svmModel_1

svmPred_1 <- predict(svmModel_1, newdata=testSet1)
table(svmPred_1, testSet1$IsCanceled)
sum(diag(table(svmPred_1, testSet1$IsCanceled)))/sum(table(svmPred_1, testSet1$IsCanceled))
confusionMatrix(svmPred_1, testSet1$IsCanceled)

#rpart
rpart_model_1 <- rpart(IsCanceled ~ ., data = testSet1)
rpart.plot(rpart_model_1)

#randomforest
trainList2 <- createDataPartition(y=hotel_data$IsCanceled,p=.50,list=FALSE)
trainSet2 <- hotel_data[trainList2,]
testSet2 <- hotel_data[-trainList2,]
rf <- randomForest(IsCanceled ~ .,data=trainSet2)
summary(rf)
rf_pred <- predict(rf, newdata = testSet2)


#association rule mining
library("arules")


rules1 <- apriori(hotel_data_tran,
                  parameter=list(supp=0.007, conf=0.55, minlen=4, maxlen = 5),
                  control=list(verbose=F),
                  appearance=list(default="lhs",rhs=("IsCanceled=1")))
inspect(rules1[1:10])


















