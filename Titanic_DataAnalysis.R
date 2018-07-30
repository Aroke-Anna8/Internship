getwd()
setwd("C:/Users/HP/Documents/Data_Science/Internship")

# Import train.csv file from Titanic_dataset.
data<- (read.csv("train.csv",stringsAsFactors = FALSE))

#2. Factors and Levels
#A. Find number of Passengers according to their Group Class: 1st , 2nd , 3rd

data$Pclass <- factor(data$Pclass)
summary(Pclass)

## Report: 216 passengers travelled in 1st class, 184 in 2nd class and 491 in 3rd class

#B. Find number of Passengers according to their Group Sex: Male, Female.

summary(data)

## Report: 314 female passengers and 577 male passengers

#Find stats of Passengers Age.

is.na(data$Age)
a<- na.omit(data$Age)
a[a<1]=1
summary(a)

## Report: The youngest passenger was 1 year old while the eldest was 80years old
##        Mean of age all the passengers on board is 29
##        Median of age all the passengers on board is 28
##        1st quarentile is 20.12 and 3rd quarentile is 38

#D. Find no. of Passengers acc to Group Embarked:Cherbourg, Queenstown or Southampton.


summary(data$Embarked)
data$Embarked[data$Embarked==""]=NA
na.omit(c(data$Embarked))
data$Embarked<-as.factor(data$Embarked)
summary(data$Embarked)

## Report: 168 passengers embarked from Cherbourg, 77 from Queenstown and 644 from Southampton.

#3.A. Validate number of passengers who survived / Not Survived

survived<-as.factor(data$Survived)
summary(survived)

## Report: Assuming that 1 indicates 'survived' and 0 indicates 'not survived', 
##        549 passengers did not survive, while 342 passenger survived 

#4.A. Explore / Print first n Records from Dataset.

head(data)

#B. Find mean, median, quartile, max, min data for every feature

summary(data)

#C. Input variables : Passenger Class, Sex, Age, and Port of Embarkment. Response variable : Survived.

b<- (data[,c("Survived","Pclass","Sex","Age","Embarked")])

#Datacleaning

c<-na.omit(b)
rownames(c)

#E. Encode Data. Make Age as a categorical variable as follows:

c$Age[c$Age <= 18] <- "Child"
c$Age[18 < c$Age & c$Age <= 60] <- "Adult"
c$Age[c$Age!="Child" & c$Age!="Adult"]<-"Senior"
c$Age=as.factor(c$Age)
head(c)
View(c)

#5. Data Analysis to perform
#A. Plot the barplot of all four input variables:

c$Ages = as.integer(c$Age)
BAge= barplot(table(c$Age),main = "Number of passengers as per Age",
        ylab = "Number of Passengers",
        xlab = "Age")
text(BAge, 0, round(table(c$Age), 1),cex=1,pos=3) 

##Of all the passengers 21 were senior, that is; aged aboved 60, 
##139 were children, aged 18 and below and the rest 552 were adults.

BClass<- barplot(table(c$Pclass),main= "Barplot of Passenger class",
              ylab= "Number of Passengers",
              xlab="Passenger Class")
text(BClass, 0, round(table(c$Pclass), 1),cex=1,pos=3)

##No.of Passengers travelling in third class were the highest-355 passengers
##Followed by the first class travellers-184 passengers
##And then the second class travellers- 173 Passengers

head(c$sex)

BSex<- barplot(table(c$Sex),main = "Number of passengers as per gender",
        xlab= "Gender",
        ylab= "Number of Passengers")
text(BSex, 0, round(table(c$Sex), 1),cex=1,pos=3)

##Number of male passengers were higher- 453 as compared to female passengers-259

BEmbarked<- barplot(table(c$Embarked),main = "No. of Passengers as per port of embarkment",
            xlab= "Port of Embarkment",names=c("Cherbourg", "Queenstown", "Southampton"),
            ylab= "Number of Passengers")
text(BEmbarked, 0, round(table(c$Embarked), 1),cex=1,pos=3)

##More 2/3 of the passengers embarked from southampton-554, 
##While the rest embarked from cherbourg-130 and Queenstown-28

#B. Convert the categorical dataframe into numeric dataframe

summary(c$Pclass)
c$Pclass = as.integer(c$Pclass)

c$Age = as.integer(c$Age)

c$Embarked = as.integer(c$Embarked)

c$Survived = as.integer(c$Survived)

head(c)

#6. Statistical Analysis:
#A. Number of survivors on an average from Class & Plot a scatter plot

MPC = c(0,0,0)
MPC[1] = mean(c$Survived[c$Pclass==1])
print(MPC[1])
MPC[2] = mean(c$Survived[c$Pclass==2])
print(MPC[2])
MPC[3] = mean(c$Survived[c$Pclass==3])
print(MPC[3])

plot(MPC, type="o",
     main="Average Survival as per Passenger Class",
     xlab="Passenger Class",
     ylab="Survival",
     xaxt="n")
axis(1, at=c(1,2,3), labels=c("1st", "2nd", "3rd"))

##The average survival of the passengers travelling in 1st class is highest,
##followed by the passengers travelling 2nd class and then passengers travelling 3rd class.


MPC1 = aov(data$Survived ~ data$Pclass)
anova(MPC1)

#B. Number of survivors on an average from Gender & Plot a scatter plot

y<-as.factor(c$Sex)
x<-as.integer(y)
head(x)

MS = c(0,0)
MS[1] = mean(c$Survived[x==1])
print(MS[1])
MS[2] = mean(c$Survived[x==2])
print(MS[2])

plot(MS, type = "o",
     main= "Average Survival as per Gender",
     xlab="Gender",
     ylab= "Average Survival",
     xaxt="n")
axis(1, at=c(1,2), labels=c("female","male"))

##Average survival of females is high to that of males

MPC2 = aov(c$Survived ~ x)
anova(MPC2)

#C. Number of survivors on an average from Every Port of Embarkment & Plot a scatter plot.

ME = c(0,0,0)
ME[1] = mean(c$Survived[c$Embarked==1])
print(ME[1])
ME[2] = mean(c$Survived[c$Embarked==2])
print(ME[2])
ME[3] = mean(c$Survived[c$Embarked==3])
print(ME[3])

plot(ME, type="o",
     main="Average Survival as per Port of Embarkment",
     xlab="Port of Embarkment",
     ylab="Average Survival",
     xaxt="n")
axis(1, at=c(1,2,3), labels=c("Cherbourg", "Queenstown", "Southampton"))  

##Average survival of passengers embarked from Cherbourg is higher 
##than those who embarked from southampton and queenstown.

MPC3 = aov(c$Survived ~ c$Embarked)
anova(MPC3)

##Although the number of males tavelling was higher than the number of females
##as seen in the barplot, the average survial of females are higher than that of males
##Similarly, we can also see that the average survival of passengers travelling 1st class
##is higher than that of passengers travelling 3rd class- where the number of passengers
##travelling third class is more than double that of those travelling 1st class
##The same could be said when we consider the port of emabarkment. 
##It is very clear that passengers that embarked from Southampton were much more than
##those that embarked from cherbourg or queenstown. 
##And yet the average survival rate of passengers emabarked from Southamptom is around 35%
##while that of Queenstown is 60%
##Further analysis would be required to confirm and find, does and how these factors 
##have any effect on survival.