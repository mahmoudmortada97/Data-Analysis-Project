library(ggplot2)
library(gridExtra)
library(PerformanceAnalytics)
library(klaR)
library(e1071)
library(Metrics)
library(party)

#read dataset
accid <- read.csv(file="C:\\Users\\Mortada\\Documents\\R\\win-library\\3.5\\nassCDS.csv", header=TRUE, stringsAsFactors = FALSE)

#show name of coloums
names(accid)

#show details of each coloum
str(accid)

#detec missing values and remove it from dataset
NoOfNull<-apply(accid,2,function(x)sum(is.na(x)))
NoOfNull
accid<-na.omit(accid)
NoOfNull<-apply(accid,2,function(x)sum(is.na(x)))
NoOfNull

#delete wrong values of weight
accid$weight[accid$weight>100]<-0
accid$weight[accid$weight<10]<-0
accid$weight[accid$weight==0]<-NA
accid<-na.omit(accid)
accid




#barplot to show age of person in accident
Person_Age<-table(accid$ageOFocc)
barplot(Person_Age,main="Age of Person")




#piechart to show distribution of year of accident

#barplot of year accident
year<-table(accid$yearacc)
barplot(year,main="Year of accident")

slices <- c(4000,4400,4500,4400, 4100, 4800)
lbls <- c("1997","1998","1999","2000","2001","2002")
pct <- slices/26000*100
pcct<-as.integer(pct)
lbls <- paste(lbls,pcct)
lbls <- paste(lbls,"%",sep="") 
pie(slices, labels = lbls, col= rainbow (length(lbls)), main="Pie Chart of Accident Year")
           


#histogram to show the distribution model of cars
hist(accid$yearVeh, col = rainbow(6),main = "Vehicle year Histogram  ", xlab = "Year of vehicle",ylab = "No of accident",breaks = 30,lwd=3)


#densityplot show the injury severity
plot(density(accid$injSeverity),main="Density plot of injury severiy",col="red")



#barplot to determine no of alive and dead people
d<-accid$dead
barplot(table(d),main = "Dead or Alive",col = "blue")


#barplot to determine no of male and female people
s<-accid$sex
barplot(table(s),main = "Male or female ",col = "Green")


#histogram to show the distribution speed of cars
accidtemp$dvcat<-as.numeric(as.factor(accidtemp$dvcat))
hist(accidtemp$dvcat,col = rainbow(6),main = "Vehicle Speed Histogram  ", xaxt="n", ,xlab = "Distribution of speed",ylab = "No of accident",breaks = 30,lwd=3)
axis(1,labels=c("1-9","10-24","25-39","40-54","+55"),at=1:5)


#show details of age
summary(accid$ageOFocc)


#injury severity based on age
ggplot(aes(x = injSeverity), data = accid) +
geom_bar(color = "black", fill = "#993366") +
ggtitle("Distribution of  injure sevirity ")
boxplot(accid$ageOFocc ~ accid$injSeverity,main="Distribution of  injure sevirity according to age",col="red")


#determine distribution of dead and alive based on their age
p1<-plot(accid$dead,accid$ageOFocc)
by(accid$ageOFocc, accid$dead, mean)


#injury severity based on year of accident
counts<-table(accid$injSeverity,accid$yearacc)
barplot(counts,main = "YEAR OF ACCIDENT VS SEVERITY OF INJURY",
        xlab ="year of accident",ylab = "severity",col = rainbow(7),
        legend=rownames(counts),beside = T)



# Classification with decision tree

accid$dead<-as.integer(accid$dead)
accid$airbag<-as.numeric(accid$airbag)
accid$seatbelt<-as.numeric(accid$seatbelt)
accid$dvcat<-as.numeric(accid$dvcat)
accid$sex<-as.numeric(accid$sex)
accid$occRole<-as.numeric(accid$occRole)
accid$injSeverity<-as.numeric(accid$injSeverity)
accid$ageOFocc<-as.numeric(accid$ageOFocc)
accid$yearacc<-as.numeric(accid$yearacc)
accid$yearVeh<-as.numeric(accid$yearVeh)

ind<-sample(2,nrow(accid),prob=c(0.7,0.3),replace=TRUE)
train.data<-accid[ind==1,]
test.data<-accid[ind==2,]

accid.tree <- ctree(dead~ageOFocc+weight+dvcat
                      +airbag+seatbelt+yearacc+yearVeh+injSeverity ,data = train.data)

table(predict(accid.tree),train.data$dead)
testPred <-predict(accid.tree,newdata = test.data)
table(testPred,test.data$dead)

plot(accid.tree,type="simple")
accuracy(predict(accid.tree,test.data),test.data[,"dead"])



# Classification with naive bayesian
accid$dead<-as.factor(accid$dead)
accid$airbag<-as.factor(accid$airbag)
accid$seatbelt<-as.factor(accid$seatbelt)
accid$dvcat<-as.factor(accid$dvcat)
accid$sex<-as.factor(accid$sex)
accid$occRole<-as.factor(accid$occRole)
accid$injSeverity<-as.factor(accid$injSeverity)
accid$caseid<-as.integer(accid$caseid)

nb_mod<-naiveBayes(accid$dead~.,data = accid)
nb_mod

prediction=predict(nb_mod,accid)
table(prediction,accid$dead)

prop.table(table(prediction,accid$dead),1)
