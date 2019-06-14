





# ***********BIG MART SALES PREDICTION *************

# Importing the dataset
train_bigmart<-read.csv("C:\\Users\\xeadmin\\Desktop\\Big Mart Sales Prediction\\Train_BigMart.csv")












   dim(train_bigmart)
   summary(train_bigmart)
# when we look at the summary we find that only the item_weight has 'NA' values (1463)

summary(train_bigmart$Outlet_Size)
# # outlet size var has some null values ,we have to put NA there as there is only blank 



# we will add training and test data for data cleaning,visualization and data prep purpose
# for adding purpose both the dataset should have same number of col so we will add a col to the test data containing NA valuestest[,Item_Outlet_Sales := NA
test_bigmart<-read.csv("C:\\Users\\xeadmin\\Desktop\\Big Mart Sales Prediction\\Test_BigMart.csv")
str(test_bigmart)
test_bigmart$Item_Outlet_Sales<-NA


# appending the training and the test for data cleaning and prep
master<-rbind(train_bigmart,test_bigmart)
dim(master)
summary(is.na(master))






# if we look at the fat content we find that there are 2 levels low fat and regular 
# but low fat is encoded as 'LF','LOW FAT','low fat' and Regular as 'Regular' and 'reg'
# # so we have to change it in same case either upper or lower

str(train_bigmart$Item_Type)
# the item type has 16 levels ,we can do some bucketing according to the food type as making 16 dummy var will lead to more complex model


# grouping the dependent var i.e outlet sales based on the independent var
library(dplyr)
train_bigmart%>%
            select(Item_Outlet_Sales,Item_Type)%>%
            group_by(Item_Type)%>%
            summarise(sum_sales=sum(Item_Outlet_Sales))%>%arrange(desc(sum_sales))
# fruits and veggies have the highest sales followed by snack food and sea food has the lowest sales
train_bigmart%>%
            select(Outlet_Identifier,Item_Outlet_Sales,Item_Type)%>%
            group_by(Item_Type,Outlet_Identifier)%>%
            summarise(sum_sales=sum(Item_Outlet_Sales))%>%arrange(desc(sum_sales))
# outlet 27 has the highest sales i.e of fruits and veggies , snack food as well as household



hist(master$Item_Outlet_Sales)
# if we look at the hist we find that data is skewed to the right.we have greater mean thn the median
# we can use log to normalize the data

hist(master$Item_Weight)
#we see that the weights of the product are not much skewed .
hist(master$Item_Visibility)
# item visibility is also right skewed which means mean is greater thn the median

library(ggplot2)
ggplot(master,aes(Outlet_Identifier,Item_Weight))+geom_boxplot()
# we find that only the weight of outlet 19 and outlet 27 is missing

ggplot(master,aes(Item_Type,Item_Weight))+geom_boxplot()
#we find that item weights for all the item types are available.

ggplot(master,aes(Item_Type,Item_Visibility))+geom_boxplot()
# the dots show the outliers for each item type

ggplot(master,aes(x=Item_Outlet_Sales,Item_Visibility,col=Item_Type))+ geom_point()


ggplot(master,aes(x=Item_Outlet_Sales,Item_Visibility,col=Outlet_Location_Type))+ geom_point()



ggplot(master,aes(y=Item_Outlet_Sales,x=Item_MRP,col=Outlet_Location_Type))+ geom_point()
#there are 4 categories in which mrp is divided
# *************Why could be the possible reasons for this?

ggplot(master,aes(Outlet_Type))+geom_bar(stat="count")
# outlet type (sypermarket 1) has the highhest number.so it is the most popular.

ggplot(master,aes(Outlet_Establishment_Year))+geom_bar(stat="count")
#No outlets were established in the year 1987 to 1996...In 1998 lowest number of outlets were established


#replacing missing values with NA

master$Outlet_Size <- as.character(master$Outlet_Size)
master$Outlet_Size[master$Outlet_Size==""] <- "NA"

#checking the na values
table(master$Outlet_Size)


#transforming LF and low fat to Low Fat
index <- which(master$Item_Fat_Content == "LF" | 
                 master$Item_Fat_Content == "low fat")

master[index, "Item_Fat_Content"] <- "Low Fat"



#Transforming "reg" to "Regular
index2 <- which(master$Item_Fat_Content == "reg")

master[index2, "Item_Fat_Content"] <- "Regular"

ggplot(master)+geom_histogram(aes(x=Item_Fat_Content),stat='count')


#identifying ways to impute values for outlet size which have nA values
table(master$Outlet_Identifier,master$Outlet_Size)
table(master$Outlet_Identifier,master$Outlet_Type)
table(master$Outlet_Size,master$Outlet_Type)
table(master$Outlet_Type,master$Outlet_Size)
#as we can see only outlet 10 and outlet  17,outlet 45 has na values .
#oulet 10 is a grocery store  so the na values present in outlet size are prod bably of grocery store.i.e 925 NA values 
#will be converted in grocery 
index3<-which(master$Outlet_Identifier== "OUT010")

master[index3,"Outlet_Size"]<- "Small"

#similarly for outlet 17
index4<-which(master$Outlet_Identifier=="OUT017")







master[index4,"Outlet_Size"]<-"Small"

#similarly for out 45
index5<-which(master$Outlet_Identifier == "OUT045")
master[index5,"Outlet_Size"]<-"Medium"


ggplot(master)+geom_histogram(aes(Outlet_Size),stat='count')


#imputing values for iitem weighth
install.packages("VIM")
library(VIM)
master <- kNN(master, variable = c("Item_Weight"), k = 90)
sum(is.na(master$Item_Weight))
View(master)

#removing outliers for item visibility

ggplot(master) + geom_histogram(aes(Item_Visibility), bins = 100,colour="red")
summary(master$Item_Visibility)
#as we can see there are outliers in item visibility , .we can replace the 0's 
#with winsorizing technique
#Quartile3 +1.5*IQR(variable)
#here 3rd Q = 0.094 +1.5*0.067(var)outlier is any data point more than 1.5 IQRs below the first quartile or
#above the third quartile. And 3 is just 1.5 doubled.

bench <- 0.09404 + 1.5*IQR(master$Item_Visibility)
bench
master$Item_Visibility[master$Item_Visibility > bench]<-bench

boxplot(master$Item_Visibility)


#Since we are only concerned with how old the outlet is, and not the establishment year
master$Outlet_Year <- 2018 - master$Outlet_Establishment_Year
master$Outlet_Year <- as.factor(master$Outlet_Year)
summary(master)


#encoding categorical data

class(master$Item_Fat_Content)

master$Item_Fat_Content<-factor(master$Item_Fat_Content,
                                levels = c('Low Fat','Regular'),
                             labels = c(1,2))

table(master$Item_Fat_Content)

master$Item_Type<-factor(master$Item_Type,
                         levels = c('Baking Goods','Breads','Breakfast','Canned',
                                                     'Dairy','Frozen Foods','Fruits and Vegetables',
                                                      'Hard Drinks','Health and Hygiene','Household',
                                                       'Meat','Others','Seafood','Snack Foods',
                                                        'Soft Drinks','Starchy Foods'),
                        labels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16))

table(master$Item_Type)


levels(master$Outlet_Type)
master$Outlet_Type<- factor(master$Outlet_Type,
                            levels=c('Grocery Store','Supermarket Type1','Supermarket Type2','Supermarket Type3'),
                            labels = c(1,2,3,4))

table(master$Outlet_Type)



master$Outlet_Location_Type<-as.character(master$Outlet_Location_Type)
class(master$Outlet_Location_Type)
levels(master$Outlet_Location_Type)


master$Outlet_Location_Type<-factor(master$Outlet_Location_Type,
                                    levels=c('Tier 1','Tier 2','Tier 3'),
                                    labels=c(1,2,3))
table(master$Outlet_Location_Type)

ggplot(master)+geom_histogram(aes(Outlet_Size),stat='count')
master$Outlet_Size<-factor(master$Outlet_Size,
                           levels = c('High','Medium','Small'),
                           labels=c(1,2,3))






#removing the unused columns
master<-master[-c(1,7,8,13)]

#dividing the data in training and test
train= master[1:8523,] # this will put the first 100 rows into the training set</code>
test= master[8524:14204,]#  this will put the remaining 50 rows into the test set


#building the multiple regression mmodel
regressor1<-lm(formula = Item_Outlet_Sales ~ .,
               data=train)
summary(regressor1)
#many of the vars are coming insignificant

#let's try with the log of the item outlet sales
regressor2<-lm(formula = log(Item_Outlet_Sales) ~ .,
               data=train)
summary(regressor2)


#let's do the sqrt of outlet sales
regressor3<-lm(formula = sqrt(Item_Outlet_Sales) ~ .,
               data=train)
summary(regressor3)

plot(regressor1)
plot(regressor2)
plot(regressor3)


#prediction on test dataset

test<-test[-c(9)]


#Prediction on test_dataset

predicted <- predict(regressor2,newdata =test)

test$Item_Outlet_Sales <-exp(predicted)






install.packages("Metrics")

#Lets check RMSE values
library(Metrics)
rmse(train$Item_Outlet_Sales,exp(regressor2$fitted.values) )

rmse(test$Item_Outlet_Sales, predicted)
