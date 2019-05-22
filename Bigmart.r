





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
