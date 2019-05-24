





# ***********BIG MART SALES PREDICTION *************

# Importing the dataset
train_bigmart<-read.csv("C:\\Users\\xeadmin\\Desktop\\Big Mart Sales Prediction\\Train_BigMart.csv")
summary(train_bigmart)
# when we look at the summary we find that only the item_weight has 'NA' values (1463)

summary(train_bigmart$Outlet_Size)
# # outlet size var has some null values ,we have to put NA there as there is only blank 

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

# we will add training and test data for data cleaning,visualization and data prep purpose
# for adding purpose both the dataset should have same number of col so we will add a col to the test data containing NA valuestest[,Item_Outlet_Sales := NA
test_bigmart<-read.csv("C:\\Users\\xeadmin\\Desktop\\Big Mart Sales Prediction\\Test_BigMart.csv")
str(test_bigmart)
test_bigmart$Item_Outlet_Sales<-NA


# appending the training and the test for data cleaning and prep
master<-rbind(train_bigmart,test_bigmart)
dim(master)
summary(is.na(master))



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











