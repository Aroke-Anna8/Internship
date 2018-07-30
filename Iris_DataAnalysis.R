getwd()
setwd("C:/Users/HP/Documents/Data_Science/Internship/iris")

#Import Dataset
read.csv("iris.csv")

#Print first three records 
print(head(iris,3))

#Dimensions of dataset
print(dim(iris))

#Find names and class of features
print(names(iris))
print(class(iris))

#Find missing values
is.na(iris)
#No NAs found


#To omit NAs
#na.omit(iris)

#To find Structure of Data
str(iris)

#Mean, Median, Quarentile, Max, Min
summary(iris)

#Boxplot of Dataset
boxplot(iris)

#Create subsets according to species
a<-subset(iris,iris$Species=="setosa")
b<-subset(iris,iris$Species=="versicolor")
c<-subset(iris,iris$Species=="virginica")
x<-c(nrow(a),nrow(b),nrow(c))
S<- c("setosa","versicolor","virginica")

#Plot boxplot for individual R Object
print(a)
boxplot(a)
boxplot(b)
boxplot(c)

#Pie chart of Species
(pie(x,labels,main="Species"))

#Histogram of Petal Length
h<-c(iris$Petal.Length)
print(class(h))
hist(h,xlab = "Petal Length", col = blues9, main = "Histogram of Petal Length")

#Histogram of Petal Length according to species
ah<-c(a$Petal.Length)
print(class(ah))
hist(ah,xlab = "Petal Length", main = "Petal length of Setosa")

bh<-c(b$Petal.Length)
print(class(bh))
hist(bh,xlab = "Petal Length", main = "Petal length of Versicolor")

ch<-c(c$Petal.Length)
print(class(ch))
hist(ch,xlab = "Petal Length", main = "Petal length of Virginica")


#Finding correlation

cor(iris[,1:4])
#Petal length and petal width show coorelation of +0.96, a uphill linear relationship
#Thus it can be concluded that Petal length is directly proportional 
#to the Petal width of the iris flower species


#ScatterPlot
colors()

plot(iris$Petal.Width,iris$Petal.Length, 
     xlab= "Petal Width", 
     ylab= "Petal Length", 
     main= "Scatterplot of Petal length versus Petal width", 
     col= 571)

#Decision Tree

library("rpart")
library("rpart.plot")
library(party)

data("iris")
str(iris)
dim(iris)
set.seed(123)
indexes = sample(150,110)
iris_train= iris[indexes,]
iris_test= iris[-indexes,]
target = Species ~ Petal.Length + Sepal.Length + Petal.Width + Sepal.Width
tree = rpart(target, data = iris, method = "class")
predict(tree,iris_test, type = "prob")
rpart.plot(tree)

#Report:
#It can be concluded that classification of iris flowers into their respective species; namely- setosa, veriscolor and virginica
#depends highly on the Length and Width of their Petals
#Iris flowers having petal length less that 2.4cm are grouped under Species setosa
#While for the ones having Petal length more than 2.4cm can be further identified using the width of their petals
#Flowers having petal width less than 1.8cm are grouped under species versicolor
#While the ones having petal width more than 1.8cm are grouped under species virginica
#Further tests 