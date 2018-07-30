
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

ggplot(df, aes(Item_Type,Item_Outlet_Sales))+
  geom_boxplot()+
  ggtitle("Item Type vs Outlet Sales")+
  theme(axis.text.x = element_text(angle = 70,vjust = 0.5,color = "black"))

dft$Item_Outlet_Sales <- 1
db<- rbind(df,dft)

View(db)
median(db$Item_Weight)
db$Item_Weight [is.na(db$Item_Weight)] <-median(db$Item_Weight, na.rm = TRUE)
db$Item_Visibility [db$Item_Visibility==0] <-median(db$Item_Visibility, na.rm = TRUE)
median(db$Item_Visibility)

levels(db$Outlet_Size)[1] <- "Other"

#library(dplyr)

#db$Item_Fat_Content <- revalue(df$Item_Fat_content, 
#c("LF" = "Low Fat", "reg" = "Regular")

#db$Year <- 2013-db$Outlet_Establishment_Year

#library(dplyr)
#dbt<- select(db, -c(Item_Identifier,Outlet_Identifier,Outlet_Establishment_Year))
#new_train<- db[1:nrow(train),]

#relation<- lm(db$Item_Weight~db$Item_Outlet_Sales)
#print(summary(relation))

