
getwd()
setwd("C:/Users/HP/Documents/Data_Science/Internship/BigMart")

df<-read.csv("Train.csv")
dft<-read.csv("Test.csv")
dim(df)

##[1] 8523   12

structure(df)

table(is.na(df))

colSums(is.na(df))

##1463 Nas in Item weight

dat<-na.omit(df)

summary(df)

library(ggplot2)

Item_Visibility<-df$Item_Visibility
Item_Outlet_Sales<-df$Item_Outlet_Sales

ggplot(df, aes(Item_Visibility,Item_Outlet_Sales))+
  geom_point(color='cyan')+
  ggtitle("Item Visibilty vs Item Outlet Sales")

## Item Visibiltiy 

Outlet_Identifier<-df$Outlet_Identifier
Item_Outlet_Sales<-df$Item_Outlet_Sales

ggplot(df, aes(Outlet_Identifier,Item_Outlet_Sales))+
  geom_bar(stat='identity', color='thistle2')+
  ggtitle("Outlet vs Outlet Sales")

##Outlet 27 has contributed most to the sales

Item_Type<-df$Item_Type
Item_Outlet_Sales<-df$Item_Outlet_Sales

ggplot(df, aes(Item_Type,Item_Outlet_Sales))+
  geom_bar(stat='identity', color='skyblue')+
  ggtitle("Item Type vs Outlet Sales")+
  theme(axis.text.x = element_text(angle = 90,vjust = 0.5,color = "black"))

##Highest sales is of fruits and vegetables followed by snacks and household items

Item_MRP<-df$Item_MRP

ggplot(df, aes(Item_Type,Item_MRP))+
  geom_boxplot()+
  ggtitle("Item Type vs Item MRP")+
  theme(axis.text.x = element_text(angle = 70,vjust = 0.5,color = "black"))

dft$Item_Outlet_Sales <- 1
db<- rbind(df,dft)

View(db)
median(db$Item_Weight)
db$Item_Weight [is.na(db$Item_Weight)] <-median(db$Item_Weight, na.rm = TRUE)
median(db$Item_Weight)
db$Item_Visibility [db$Item_Visibility==0] <-median(db$Item_Visibility, na.rm = TRUE)
median(db$Item_Visibility)

levels(db$Outlet_Size)[1] <- "Other"

library(plyr)
summary(db$Item_Fat_Content)

db$Item_Fat_Content<- revalue(db$Item_Fat_Content,
                              c("LF"="Low Fat", "low fat"="Low Fat","reg"="Regular"))

db$Year <- 2013- db$Outlet_Establishment_Year

library(dplyr)

dbt<- select(db, -c(Item_Identifier,Outlet_Identifier,Outlet_Establishment_Year))

View(dbt)

new_train<- dbt[1:nrow(dbt),]
new_test<- db[-(1:nrow(db)),]

str(new_test)

relation<- lm(new_train$Item_Outlet_Sales~.,data=new_train)
summary(relation)
plot<- plot(relation)
return(plot)

prediction<-predict(relation, data= new_test)
summary(prediction)

##Minimum sales of the test dataset is -923.1
##Maximum sales is 3503.5
##Average sales being 1303.3

