#1)a)probability of obtaining a score greater than 700 on a GMAT test that has a mean of 494 and a standard deviation of 100?

library(readr)

library(caret)

library(dplyr)

pnorm(700,mean = 494,sd = 100,lower.tail = FALSE)

#without using r code

# zscore = (x - mean) / standard deviation = (700-494)/100 = 2.06 ; p(X>700)=P(Z>2.06)=1-P(Z>2.06)=1 -0.98030 = 0.0196

#1)b)probability of getting a score between 350 and 450 on the same GMAT exam?

a <- pnorm(350,mean = 494,sd = 100,lower.tail = TRUE)

b <- pnorm(450,mean = 494,sd = 100,lower.tail = TRUE)

c = b - a

# c is defined as probability of getting a score between 350 and 450 on the same GMAT exam?

# 2) what is the average per diem cost in Buenos Aires? 

RUN<-qnorm(0.8665)

449 - (RUN*36) 


# 3) Calculate the correlation (Pearson Correlation Coefficient) between the temperatures of the two cities without using any R commands 

c <-sum((a-mean(a))*(b-mean(b)))

d <- sqrt(sum((a-mean(a))^2)*sum((b-mean(b))^2))

e <-c/d # coorlation betweeen temperature of two cities without using r

cor(a,b) #  coorlation betweeen temperature of two cities with using r command


# Data wrangling
library(readr)

Online <- read_csv("C:/Users/rajendra/Desktop/Online_Retail.csv")

View(Online_Retail)

#4)Show this in total number and also in percentage. Show only countries accounting for more than 1% of the total transactions. 

g <- prop.table(table(Online$Country)) * 100

g[g>1]

#5)'TransactionValue' that is the product of the exising 'Quantity' and 'UnitPrice' variables. 

Online$TransactionValue<-Online$Quantity*Online$UnitPrice



#6)how much money in total has been spent each country. Show this in total sum of transaction values. 

Online%>%group_by(Online$Country)%>%summarise(k= sum(Online$TransactionValue))

sum(split(Online$TransactionValue,Online$Country))

k<-tapply(Online$TransactionValue,Online$Country,sum)

k[k>130000]


#7)
Temp=strptime(Online$InvoiceDate,format='%m/%d/%Y %H:%M',tz='GMT')

Online$New_Invoice_Date <- as.Date(Temp)

Online$New_Invoice_Date[20000]- Online$New_Invoice_Date[10]

Online$Invoice_Day_Week= weekdays(Online$New_Invoice_Date)

Online$New_Invoice_Hour = as.numeric(format(Temp, "%H"))

Online$New_Invoice_Month = as.numeric(format(Temp, "%m"))


# a)	Show the percentage of transactions (by numbers) by days of the week 
  
week <- tapply(Online$TransactionValue,Online$Invoice_Day_Week, sum) / sum(Online$TransactionValue) * 100
week

#b)	Show the percentage of transactions (by transaction volume) by days of the week 

day <- tapply(Online$TransactionValue,Online$Invoice_Day_Week, NROW) / NROW(Online$TransactionValue) * 100
day

# c)	Show the percentage of transactions (by transaction volume) by month of the year 
month <- tapply(Online$TransactionValue,Online$New_Invoice_Month, length) / length(Online$TransactionValue) * 100
month

# d)	What was the date with the highest number of transactions from Australia? 

Z<- max(Online$TransactionValue[Online$Country == "Australia"])
Online$New_Invoice_Date[Z]




#8)Plot the histogram of transaction values from Germany. Use the hist() function to plot. 

range(Online$TransactionValue[Online$Country=="Germany"]) # for x-limit

NROW(Online$TransactionValue[Online$Country=="Germany"]) #for y -axis

i<-hist(Online$TransactionValue[Online$Country=="Germany"],breaks = 7,xlim = c(-600,1000),ylim=c(0,10000),col="green")

text(i$mids,i$counts,labels = i$counts,adj=c(0.5,-0.5))


#9)Which customer had the highest number of transactions? Which customer is most valuable (i.e. highest total sum of transactions)? 
l<-tapply(Online$TransactionValue,Online$CustomerID,sum)
which.max(l)#for finding the position of customer id
l[1704] # for finding customer id



#10)What are the number of transactions with missing CustomerID records by countries? 

sum(is.na(Online))

misscustomer<- colMeans(is.na(Online[c(0:8)])) * 100

misscustomer 


#11)How many unique customers are represented in the dataset? You can use unique() and length() functions. 

id<-function(x){

    k<-sum(is.na(x))
 
     return(k)}
tapply(Online$CustomerID,Online$Country,id)


#12) On average, how often the costumers comeback to the website for their next shopping? 
table(Online$InvoiceNo)
qq<-Online[,c(1,7,12)]
str(qq)
summarise(group_by(qq,CustomerID),count=n())
diff(qq$CustomerID,qq$New_Invoice_Month)

#13)#Consider the cancelled transactions as those where the 'Quantity' variable has a negative value.

o<-length(Online$Quantity[Online$Quantity<0 & Online$Country=="France"])

(o/length(Online$Quantity))*100

# 14)product that has generated the highest revenue for the retailer? 

n<-(tapply(Online$TransactionValue,Online$Description,sum))

n[which.max(n)]


#15)How many unique customers are represented in the dataset? 

length((unique(Online$CustomerID)))





