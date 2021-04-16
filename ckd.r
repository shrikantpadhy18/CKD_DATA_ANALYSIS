library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)
library(ggplot2)
#to set the working directory
setwd("C:/Users/shrikant padhy/Desktop/ckddataset")

mydata1 <- read.csv("kidney_disease.csv")
mydata2 <- read.csv("ckd_clean.csv")
mydata1 <- merge(mydata1,mydata2,by=c('id','age','bp','sg','al','su','rbc','pc','pcc','ba','bgr','bu','sc','sod','pot','hemo','pcv','wc','rc','htn','dm','cad','appet','pad','ane','classification'))
#summary stats for all variables in dataset

summary(mydata1)
temp<- mydata1
test<- mydata1$bp
test
#is.na() will check whether a particular valu is NA OR NOT returns vector of Boolean Value
is.na(test)


#to identify the indices having NA Values we can use which() function
which(is.na(test))

#replacing the NA values with mean value
test[which(is.na(test))]<- 76.47
test
#which(is.na(test))

#we are finding all the index in bp column which contains na values and replacing them with the mean
temp$bp[which(is.na(temp$bp))]<- mean(temp$bp,na.rm = TRUE) #na.rm will actually remove the NA if it encounters it while estimating mean
temp

temp$sg[which(is.na(temp$sg))]<- mean(temp$sg,na.rm = TRUE)
temp$al[which(is.na(temp$al))]<- mean(temp$al,na.rm = TRUE)
temp$bgr[which(is.na(temp$bgr))]<- mean(temp$bgr,na.rm = TRUE)
temp$bu[which(is.na(temp$bu))]<- mean(temp$bu,na.rm = TRUE)
temp$age[which(is.na(temp$age))]<- mean(temp$age,na.rm = TRUE)

#estimate the data frame that will have all the count of  distinct ages(group by)

agesbycountwithckd<- temp %>% filter(classification==1) %>% group_by(age) %>% summarize(n())
#agesbycountwithckd<- agesbycountwithckd.frame(agesbycountwithckd)
agesbycountwithckd<- data.frame(agesbycountwithckd)
agesbycountwithckd
colnames(agesbycountwithckd)

#rename the column 
agesbycountwithckd<- agesbycountwithckd %>% rename(age=age,counts=n..)
colnames(agesbycountwithckd)

#do bar graph plotting in order to understand the hidden correlation between age and ckd patient

ggplot(data = agesbycountwithckd,mapping = aes(x=age,y=counts))+geom_bar(aes(fill=age),stat="identity")+theme(legend.position = "none")+xlab("ages")+ylab("counts")+ggtitle("Graph of no of people with ckd by different age group")

#figure out the correlations between different cause of ckd to the number of ckd affected people

#correlation between b glucose and ckd affected people


bloodglucose<- temp %>%filter(classification==1) %>% group_by(bgr) %>% summarize(n())
bloodglucose<- data.frame(bloodglucose)

bloodglucose<- bloodglucose %>% rename(bgr=bgr,counts=n..)
#par(mar=c(1,1,1,1))
ggplot(bloodglucose,aes(x=bgr,y=counts))+geom_point(aes(fill=bgr))+stat_smooth(method = "lm",col="#C42126",se=FALSE,size=1)+ggtitle("Graph of number of ckd patients with respect change in bgr value")#se=FALSE means dont display standar error,lm:linear regression
