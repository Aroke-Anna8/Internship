d <- read.csv("https://chronicdata.cdc.gov/api/views/hn4x-zwk7/rows.csv?accessType=DOWNLOAD",
              stringsAsFactors = FALSE)

str(d)
summary(d)

#Identifing missing values and replacing them with NA
table(d$Gender, useNA = 'ifany')
d$Gender[d$Gender==""]=NA


d$Gender<-as.factor(d$Gender)
d$Gender<-as.numeric(d$Gender)
str(d$Gender)


#Find missing data
sapply(d, function(x) {
  round(sum(is.na(x) | x == "")/nrow(d), 2)
})

#Remove NA's
mean(c(1,2,NA))
mean(c(1,2,NA), na.rm = TRUE)

d1<- na.omit(d[,15:20])

