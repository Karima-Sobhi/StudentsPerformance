#load data file
ds<- read.csv("D:/3year_5thsemester/STAT/stat-labscode/StudentsPerformance.csv")
head(ds)
ds2<- ds

#/////////////////////clean data///////////////////

# for numerical columns use mean to fill data 
ds2$X[is.na(ds2$X)] <- mean(ds2$X,na.rm=TRUE) 
ds2$age[is.na(ds2$age)] <- mean(ds2$age,na.rm=TRUE) 

ds2$goout [is.na(ds2$goout)] <- mean(ds2$goout,na.rm=TRUE) 
ds2$studytime [is.na(ds2$studytime)] <- mean(ds2$studytime,na.rm=TRUE)

ds2$failures [is.na(ds2$failures)] <- mean(ds2$failures,na.rm=TRUE)
ds2$health [is.na(ds2$health)] <- mean(ds2$health,na.rm=TRUE)

ds2$absences [is.na(ds2$absences)] <- mean(ds2$absences,na.rm=TRUE)
ds2$G1 [is.na(ds2$G1)] <- mean(ds2$G1,na.rm=TRUE)

ds2$G2 [is.na(ds2$G2)] <- mean(ds2$G2,na.rm=TRUE)
ds2$G3 [is.na(ds2$G3)] <- mean(ds2$G3,na.rm=TRUE)

# for string columns use mode to fill data
ds2$Fjob [is.na(ds2$Fjob)] <- mode(ds2$Fjob)
ds2$Mjob [is.na(ds2$Mjob)] <- mode(ds2$Mjob) 
ds2$internet [is.na(ds2$internet)] <- mode(ds2$internet) 
ds2$romantic [is.na(ds2$romantic)] <- mode(ds2$romantic)


# Encoding Categorical Data to numbers
ds2$Fjob = factor(ds2$Fjob, 
                 levels = c('teacher','services','health','at_home','other'), 
                 labels = c(0 ,1 ,2 ,3 ,4 ))
ds2$Mjob = factor(ds2$Mjob, 
                  levels = c('teacher','services','health','at_home','other'), 
                  labels = c(0 ,1 ,2 ,3 ,4 ))
ds2$internet = factor(ds2$internet, 
                 levels = c('yes','no'), 
                 labels = c(0, 1  ))
ds2$romantic= factor(ds2$romantic, 
                      levels = c('yes','no'), 
                      labels = c(0, 1  ))

#remove duplicate by rows
ds2 <- ds2[!duplicated(ds2), ]

#boxplot to detect outliers
#boxplot(ds2) 

#remove outliers from failures
out <- boxplot.stats(ds2$failures)$out 
out_ind <- which(ds2$failures %in% c(out)) 
out_ind 
c<-out_ind
ds2 <- ds2[-c, ] 

#remove outliers from  absences(most column w/ outlier points)
out <- boxplot.stats(ds2$absences)$out 
out_ind <- which(ds2$absences %in% c(out)) 
out_ind 
c<-out_ind
ds2 <- ds2[-c, ] 

#remove outliers from g2 
out <- boxplot.stats(ds2$G2)$out 
out_ind <- which(ds2$G2 %in% c(out)) 
out_ind 
c<-out_ind
ds2 <- ds2[-c, ] 
#boxplot(ds2) 

#add extra column for techniques (for classification)
accepted <- sample(c(0,1), size = 10, replace = TRUE)
ds2<-cbind(ds2,accepted)
#==============================STATISTICS==============================
library(pastecs)
#calculate 
stat.desc(ds2[c('age','goout','studytime','failures','health','absences','G1','G2','G3')])
#IQR:
IQR(ds2$age)
IQR(ds2$goout)
IQR(ds2$studytime)
IQR(ds2$failures)
IQR(ds2$health)
IQR(ds2$absences)
IQR(ds2$G1)
IQR(ds2$G2)
IQR(ds2$G3)
#Mode:
library(modeest)
mfv(ds2$age)
mfv(ds2$goout)
mfv(ds2$Mjob)
mfv(ds2$internet)
mfv(ds2$romantic)
mfv(ds2$Fjob)
mfv(ds2$studytime)
mfv(ds2$failures)
mfv(ds2$health)
mfv(ds2$absences)
mfv(ds2$G1)
mfv(ds2$G2)
mfv(ds2$G3)

# correlation for all variables
round(cor(ds2[c('age','goout','studytime','health','absences','G1','G2','G3')]),
      digits = 2 # rounded to 2 decimals
)
# correlation PLOT for all variables
library(corrplot)

#the standard deviation is zero
#corrplot(cor(ds2[c('age','goout','studytime','failures','health')]),
#         method = "number",
#         type = "upper" # show only upper side
#)
corrplot(cor(ds2[c('failures','health','absences','G1','G2','G3')]),
         method = "number",
         type = "upper" # show only upper side
)
# covariance
#cov(ds2[c('age','goout','studytime','health','absences')])
cov(ds2[c('age','goout','studytime','health','absences','G1','G2','G3')])
#///////////////////////visualization//////////
# Bar Plot
barplot(table(ds2$age))
barplot(table(ds2$Fjob))
barplot(table(ds2$Mjob))
barplot(table(ds2$goout))
barplot(table(ds2$internet))
barplot(table(ds2$romantic))
barplot(table(ds2$studytime))
barplot(table(ds2$health))
barplot(table(ds2$absences))
barplot(table(ds2$G1))
# PIE CHART
pie(table(ds2$internet)[order(table(ds2$internet), decreasing=TRUE)],
    clockwise=TRUE,
    main="Pie Chart of internt", )
#BOX PLOT
boxplot(ds2$G1, ds2$G2,  ds2$G3)
#HISTOGRAM
hist (ds2$G2,
      main="Histogram for the second exam grade",
      xlab="Name List",
      border="black",
      col=c("violet","blue"),
      xlim=c (5,20),
      ylim=c(0,80),
      breaks=5)
#///////////////////////visualization//////////
library(tidyverse)
#1-histogram////////
# age by Gender plot
v<-ggplot(ds2,aes(x=age))
h<-geom_histogram(binwidth = 0.5,color="blue",aes(fill=sex),alpha=0.6)
v+h+ylab("Gender")
# grade1 by Gender plot
v1<-ggplot(ds2,aes(x=G1))
h1<-geom_histogram(binwidth = 1,color="blue",aes(fill=sex),alpha=0.6)
v1+h1+ylab("Gender")+xlab("grade1")

# grade2 by Gender plot
v2<-ggplot(ds2,aes(x=G2))
v2+h1+ylab("Gender")+xlab("grade2")

# grade3 by Gender plot
v<-ggplot(ds2,aes(x=G3))
v+h1+ylab("Gender")+xlab("grade3")

#2-boxPlot///////
#internet by Grade1 boxPlot
#ggplot(ds2,aes(x=internet,y=G1,group=age))+geom_boxplot()+geom_jitter(aes(color=sex))
ggplot(ds2,aes(x= internet,y=G1))+geom_boxplot()+ylab("Grade1")+xlab("having_internet")
#internet by Grade2 boxPlot
ggplot(ds2,aes(x= internet,y=G2))+geom_boxplot()+ylab("Grade2")+xlab("having_internet")
#internet by Grade3 boxPlot
ggplot(ds2,aes(x= internet,y=G3))+geom_boxplot()+ylab("Grade3")+xlab("having_internet")
#Gender by studytime  boxPlot
ggplot(ds2,aes(x=sex,y=studytime))+geom_boxplot()+ylab("studytime")+xlab("Gender")

#visualization
library("ggplot2")

#[1] Bar charts 
#Vertical charts
#[1.1]
value <- table(ds2$studytime)  # convert our data to table
hours <- c("1 hour","2 hours","3 hours","4 hours")
barplot(value,names.arg = hours,xlab = 'studytime levels', col ='orange')

#[1.2]
value <- table(ds2$accepted)  # convert our data to table
#xx=toString(ds2$accepted)
acceptance <- c("not accepted","accepted")
barplot(value,names.arg = acceptance ,xlab = 'acceptance chart', col ='green')

#[1.3]
value <- table(ds2$goout) # convert our data to table
barplot(value, xlab = 'goout levels', col ='blue')

#[1.4]
value <- table(ds2$absences)  # convert our data to table
barplot(value, xlab = 'abcence levels', col ='blue')

#[1.5]  # trying to concatinate tow columns
value <- table(ds2$health,ds2$goout)  # convert our data to table
barplot(value, xlab = 'health levels', col =c("yellow","red"))


# [2] Scatter Plot :
#is a type of plot used to display the relationship between two numerical variables, and plots one dot for each observation.
#It needs two vectors of same length, one for the x-axis (horizontal) and one for the y-axis (vertical):
#[2.1]
plot( ds2$age ,ds2$goout, main = "Scatter Plot 1",
      xlab = "age", ylab = "goout",
      pch = 19)
# Add regression line
abline(lm(ds2$goout ~ ds2$age, data = ds2), col = "blue")

#[2.2]
plot( ds2$G1 ,ds2$G3, main = "Scatter Plot 2",
      xlab = "G1", ylab = "G3",
      pch = 19)
# Add regression line
abline(lm(ds2$G3 ~ ds2$G1, data = ds2), col = "red")

#[2.3]
plot( ds2$studytime ,ds2$failures, main = "Scatter Plot 3",
      xlab = "studytime", ylab = "failures",
      pch = 19)
# Add regression line
abline(lm(ds2$failures ~ ds2$studytime, data = ds2), col = "blue")


#[2.4]
plot( ds2$age ,ds2$studytime, main = "Scatter Plot 4",
      xlab = "age", ylab = "studytime",pch=19)
# Add regression line
abline(lm(ds2$studytime ~ ds2$age, data = ds2), col = "red")
# remove unrelated columns
ds2$school <-NULL
ds2$sex <-NULL
#SPLITTING DATA FOR ALGORITHMES
library("caTools")# required library for data split
# returns true if observation goes to the Training set and false if observation goes to the test set.
split = sample.split(ds2$accepted, SplitRatio = 0.8)

#Creating the training set and test set separately
training_set = subset(ds2, split == TRUE)
test_set = subset(ds2, split == FALSE)
######################
#library('stats')
library('dplyr')
library('ggplot2')

library('ggfortify')
#k-means
wss=numeric(18) 
for(i in 1:18) wss[i]=sum(kmeans(ds2,i)$withinss)
#plot(1:18,wss,type="b",
#     xlab="Number of clusters K",
#     ylab="Total within-clusters sum of squares")

kc <- kmeans(ds2, 3)
kc$centers
#plot(ds2$age, ds2$Fjob, col = kc$cluster)
#points(kc$centers, col = 2:15, pch = 8, cex=2)
#plot(ds2, xaxt="n",ylab="studentperformance", xlab="data")
#plot(ds2$studytime,ds2$G1)


#logistic regression
model_logistic<-glm(accepted ~ G1+G2+G3, data=training_set
                    ,family = "binomial") 
summary(model_logistic )
confint.lm(model_logistic)
pred_logistic<-predict(model_logistic,test_set, type="response")
table(pred_logistic)
#naivebayes 
library(e1071)
model<-naiveBayes(accepted~ G1+G2+G3+absences , data=training_set)
pred<- predict(model , test_set[,-15])
pred
table(test_set$accepted,pred)
MLmetrics::Accuracy(pred,test_set$accepted)

#decision tree
library(rpart)
library(rpart.plot)

model2<-rpart(accepted~ G1+G2+G3+absences,
              data=training_set,method = "class",
              parms = list(split= "information"),control = rpart.control(minsplit=1))
#rpart.plot(model2)
library(rpart)
library(rpart.plot)

model2<-rpart(accepted~ G1+G2+G3+absences,data=training_set
              ,method = "class",parms = list(split= "information"),control = rpart.control(minsplit=1))
rpart.plot(model2)
pred<-predict(model2,newdata = test_set[,-15],type='class')
pred
table(test_set$accepted,pred)
MLmetrics::Accuracy(pred,test_set$accepted)
#158/nrow(test) accurecy