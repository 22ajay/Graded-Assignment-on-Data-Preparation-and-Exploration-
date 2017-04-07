#setting the path and loading data
getwd()
setwd("D:/DataScienceWithR/Assignments/Graded Assignments/Topic 8.2 -  Data Preparation")
getwd()

#loading data

read.delim("Campaign_File.txt")->campaign
View(campaign)
read.delim("Customers_File.txt")->customer
read.delim("Products_File.txt")->product
read.delim("Transactions_File.txt")->transaction
View(customer)
View(product)
View(transaction)

#checking null values
colSums(is.na(campaign))
colSums(is.na(customer))
colSums(is.na(product))
colSums(is.na(transaction))

#doing the summary check
summary(campaign)
summary(customer)
summary(product)
summary(transaction)

#loading dplyr and cheking max value 
library(dplyr)


#1)Based on the transactions, which product category dominates
#in terms of $ amount? (Hint: You will need to merge Transactions and Product data sets
                      # and then look at Product category) 
#solution:

#checking intitial values of product and transaction
head(product)
head(transaction)
#merging transaction and product table(outer join)
merge(x=transaction,y=product,by="Product_Code",all = T)->case1
#checking with dimension of resultant of tables
dim(case1)
dim(transaction)
dim(product)
#querying the objective of our case study
case1%>%group_by(Product_Category)%>%summarise(revenue=sum(Items_Amount),no_items=sum(Items_Number))->case1_sum
#check with the summarise data set
View(case1_sum)
#descending order with respect to revenue
case1_sum%>%arrange(desc(revenue))
#the category with row number 1 will be dominating

#Answer:
#So we can infer from the below observation ,the product category Entertainment is dominating


#2.Perform a suitable age grouping and find out contribution of each of the age group 
#in terms of $ amount spent. (Hint: A merge between Customer and Transaction table will be required) 

#solution:
#merge the table transaction and customer
merge(x=transaction,y=customer,by="Card_ID",all=T)->case2
#check the initial rows of the data set
head(case2)
#chek for missing values in Birthdate
sum(is.na(case2$Birth_Date))
#from birthdat we need to create a coulmn of age
case2%>%mutate(age=(Sys.Date()-as.Date(Birth_Date))/365)->case2_sum
case2%>%filter(is.na(Transaction_ID)==T)->case2_na_check
dim(case2_na_check)
#so there are 80655 customers whose transcation details is not given
#for further analysis we can omit them
dim(na.omit(case2_sum))
dim(case2_sum)
summary(case2_sum)
#filtering out alll the transaction with valid id
case2_sum%>%filter(is.na(Transaction_ID)==F)->case2_sum
summary(as.numeric(case2_sum$age))
#divide the age group in group of 10
case2_sum%>%mutate(quantile=ntile(as.numeric(case2_sum$age),10))->case2_sum
#summarise the data set in terms of age group and amount spent
case2_sum%>%group_by(quantile)%>%summarise(amount_spent=sum(Items_Amount))->case2_objective
quantile(round(as.numeric(case2_sum$age),digits = 2),(1:10)/10)
case2_objective%>%mutate(age=c("0-41.36","41.36-46.35","46.35-49.83 ","49.83-52.63","52.63-55.02",
                               "55.02-57.57","57.57-60.46","60.46-64.36","64.36-70.20","70.20-115.08"))->case2_objective
#ANSWER
case2_objective$age
data.frame(age=case2_objective$age,Amount_Spent=case2_objective$amount_spent)

View(case2_objective)


#Q3
#Find the response rate to the campaign. Also identify the 
#age group of customers where response rate is high. 
#Is there a consistent trend. (Hint: Add age information from customer file to the 
                              #campaign file, compute response rate by age. 
                              #This can be done by binning age either in deciles or
                              #quartiles and seeing if there is a consistent trend) 

#Solution:

#merging the campaign and customer
merge(x=campaign,y=customer,by="Card_ID",all.x = T)->case3
View(case3)
#adding age column to the table using the birth_date
case3%>%mutate(age=(Sys.Date()-as.Date(Birth_Date))/365)->case3
View(case3)
#obsrving the quartile of age column
quantile(as.numeric(case3$age),c(0,2.5,5,7.5,10)/10)
#creating quantile coulmn to group the data accoring to age group
case3%>%mutate(quantile=ntile(as.numeric(age),4))->case3

View(case3)
#now we need to create dummy variable for Campaign_response
#for TRUE=1,FALSE=0
gsub("TRUE",1,case3$Campaign_Responce)->case3$Campaign_Responce
gsub("FALSE",0,case3$Campaign_Responce)->case3$Campaign_Responce
View(case3)
#now response rate

case3%>%group_by(quantile)%>%summarise(response_rate=sum(as.numeric(Campaign_Responce)/n())*100)->case3_obj
library(ggplot2)
View(case3_obj)
#now adding a new column which will show which group belongs to which age range
quantile(round(as.numeric(case3$age),digits = 2),c(0,2.5,5,7.5,10)/10)
case3_obj%>%mutate(age_group=c("26.25-48.04","48.04-55.11","55.11-62.51","62.51-110.36"))->case3_obj
as.factor(case3_obj$age_group)->case3_obj$age_group
a<-ggplot(case3_obj,aes(x=age_group,y=response_rate,label=round(response_rate,digits = 2)))
a+geom_point()+geom_text(vjust=-0.5)

#ANSWER:
data.frame(age_group=case3_obj$age_group,response_rate=case3_obj$response_rate)
#from the above graph we can infer that between age group of 26 to 48,the response rate is high,
#after this age group there is sharp decline and agian it increaess with respect to the age.

case4
#Repeat the analysis above with "Tenure" of customer. 
#(Tenure will be defined as the time period between the Date of Registration and 31/12/2002) 
#SOLUTION:
#merging the required tables
merge(x=campaign,y=customer,by="Card_ID",all.x = T)->case4
View(case4)
#creating a new column for tenure
case4%>%mutate(tenure=(as.Date("31/12/2002","%d/%m/%Y")-as.Date(Registration_Date))/365)->case4
View(case4)
#creating quartile
quantile(as.numeric(case4$tenure),c(0,2.5,5,7.5,10)/10)
case4%>%mutate(quantile=ntile(as.numeric(tenure),4))->case4
View(case4)
#now we need to create dummy variable for Campaign_response
#for TRUE=1,FALSE=0
gsub("TRUE",1,case4$Campaign_Responce)->case4$Campaign_Responce
gsub("FALSE",0,case4$Campaign_Responce)->case4$Campaign_Responce
View(case4)

#now response rate
case4%>%group_by(quantile)%>%summarise(response_rate=sum(as.numeric(Campaign_Responce)/n())*100)->case4_obj
View(case4_obj)
#now adding a new column which will show which group belongs to which age range
quantile(round(as.numeric(case4$tenure),digits = 2),c(0,2.5,5,7.5,10)/10)
case4_obj%>%mutate(tenure_period=c("1.02-2.55","2.55-3.36","3.36-4.19"," 4.19-5.00"))->case4_obj
View(case4_obj)
as.factor(case4_obj$tenure_period)->case4_obj$tenure_period
#plotting to see the variation 
library(ggplot2)
View(case4_obj)
b<-ggplot(case4_obj,aes(x=tenure_period,y=response_rate,label=round(response_rate,digits = 2)))
b
b+geom_point()+geom_text(vjust=-0.2)+theme_bw()


#ANSWER:
data.frame(case4_obj$tenure_period,case4_obj$response_rate)
#quantile 4 i.e tenure  between 4 and 5 are active in giving response

#Q5
#Create a cross tab of response rate between Age and Tenure of customers.
#Do you observe anything? 
View(case4)
#storing the value of case4 data set into case5 data sets
case4->case5
View(case5)
#renaming the column name
colnames(case5)[7]<-"tenure_quantile" 
#check if the col name has been chnaged or not
colnames(case5)
#adding the age column using birthdate
case5%>%mutate(age=(Sys.Date()-as.Date(Birth_Date))/365)->case5
#check withthe data set
View(case5)

#create age quartile
case5%>%mutate(quantile=ntile(as.numeric(age),4))->case5
colnames(case5)[9]<-"age_quantile" 

#converting to numeric with unchanged valeu
library(varhandle)
library(dplyr)
unfactor(as.factor(case5$Campaign_Responce))->case5$Campaign_Responce
View(case5)
#check if any null values introduced
colSums(is.na(case5))
#now summarise the response rate
case5%>%group_by(tenure_quantile,age_quantile)%>%summarise(response_rate=(sum(Campaign_Responce)/n()*100))->case5_obj
View(case5_obj)
library(reshape)
cast(case5_obj,age_quantile~tenure_quantile,sum)
library(ggplot2)
z<-ggplot(case5_obj,aes(x=age_quantile,y=response_rate,fill=as.factor(tenure_quantile),label=round(response_rate,digits = 2)))
Contrib<-z+geom_bar(stat="identity",position="stack",alpha=0.8)+theme_light()+geom_text(stat = "identity",position="stack",vjust=1.999)
Contrib<-Contrib+scale_fill_discrete(c=50,h=c(1,300),h.start = 50,"tenure in bin")+theme_classic()+xlab("age in bin")+ylab("response_rate")+ggtitle("Response rate with age and tenure")

Contrib
quantile(round(as.numeric(case4$tenure),digits = 2),c(0,2.5,5,7.5,10)/10)
quantile(round(as.numeric(case5$age),digits = 2),c(0,2.5,5,7.5,10)/10)

#From the Contrib chart we can infer the  following key points 
#ANSWER:
  #1)The age group of 26.27 to 48.06 is more active in giving response as compared to other groups
  #2)there is no such trendz observed in the above chart between age in bin and response rate with respect to tenure in bin.It's randomly 
      #distributed

#case6
#Which mode of payment is most popular? 
#Is mode of payment affected by the time of transaction? 
#(Hint: Extract hour information from timestamp column by 
#using appropriate date conversion function, based on the hour of the day 
#extracted, you can do an appropriate classification and then look at the cross 
#tab between payment mode and time of day) 

summary(campaign)
summary(customer)
summary(product)
summary(transaction)
View(transaction)
summary(transaction$Payment_Method)->case6_obj1
case6_obj1
#ANSWER:Credit card payment is the most popular
#extract the hour information from time stamp
date1<-as.POSIXct(transaction$Timestamp)
date2<-date1$hour
#lubridate
library(lubridate)
library(lubridate)
hour(date1)->hr
View(hr)
transaction$Timestamp<-as.POSIXct(transaction$Timestamp)
View(transaction)
library(dplyr)
transaction%>%mutate(hour=hour(Timestamp))->case6_obj2
View(case6_obj2)
case6_obj2%>%select(Payment_Method,hour)->case6_obj3
View(case6_obj3)
table(case6_obj3$Payment_Method,case6_obj3$hour)->case6_crosstab
case6_crosstab
as.data.frame(case6_crosstab)->case6_crosstab1
View(case6_crosstab2)
case6_crosstab1%>%arrange(Freq)->case6_crosstab2
library(ggplot2)
e<-ggplot(case6_crosstab1,aes(x=Var2,y=Freq,fill=Var1,label=Freq))
f<-e+geom_bar(stat = "identity",position = "stack")+scale_fill_discrete(c=50,h=c(1,300),h.start = 50,name="Payment Mode")+geom_text(stat = "identity",position="stack",vjust=2)
f+xlab("Hours")+ylab("NUmber of transaction")
#ANSWER
#there is no such  variation in payment method with respect to time,in all hours of the day
#Credit card is popular

#Q7. Do you think, based on the data, that age and gender has any impact 
#on $ amount spent? (Hint: You'll need to merge customer and transaction 
#tables appropriately and then do an age classification, post that 
#you can create a cross tab between gender and age to arrive at an opinion) 

merge(x=transaction,y=customer,by="Card_ID",all=T)->case7
View(case7)
case7<-case7%>%filter(is.na(case7$Transaction_ID)==FALSE)
View(case7)
case7%>%mutate(age=(Sys.Date()-as.Date(Birth_Date))/365)->case7
View(case7)
case7%>%mutate(quantile=ntile(as.numeric(age),4))->case7
View(case7)
case7%>%select(quantile,Gender,Items_Amount)->case7_obj
View(case7_obj)
case7_obj%>%group_by(quantile,Gender)%>%summarise(total_spent=sum(Items_Amount))->case7_obj
View(case7_obj)
cast(case7_obj,quantile~Gender,sum)
    
f<-ggplot(case7_obj,aes(x=quantile,y=total_spent,fill=Gender,label=total_spent))
f+geom_bar(stat = "identity",position = "stack")+scale_fill_discrete(c=50,h=c(1,300),h.start = 50)+geom_text(stat = "identity",position = "stack",vjust=2)+xlab("Age Quantile")

quantile(as.numeric(case7$age),c(0,2.5,5,7.5,10)/10)

#ANSWER:
#from this plot we can infer except  quantile1 i.e age group of people between 25 and  48,in all other 
#age group from 48-115 ,men are spending more than women

#case8
# Produce a histogram for "tenure of a customer" separately for male and female customers.

case4->case8
view
View(case8)

case8%>%filter(Gender=="F")->case8_female
case8%>%filter(Gender=="M")->case8_male
library(ggplot2)
par(mfrow=c(2,1))
library(gridExtra)
plot1<-ggplot(case8_female,aes(x=quantile,y=tenure))+geom_histogram(stat = "identity",color="red")+ggtitle("tenure wrt to female")
plot2<-ggplot(case8_male,aes(x=quantile,y=tenure))+geom_histogram(stat = "identity",color="blue")+ggtitle("tenure wrt male")

              grid.arrange(plot1, plot2, nrow=1, ncol=2)
