library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)
library(ggplot2)
#to set the working directory
setwd("C:/Users/shrikant padhy/Desktop/ckddataset")

mydata1 <- read.csv("kidney_disease.csv")
mydata2 <- read.csv("ckd_clean.csv")

#mydata1 <- merge(mydata1,mydata2,by=c('age','bp','sg','al','su','rbc','pc','pcc','ba','bgr','bu','sc','sod','pot','hemo','pcv','wc','rc','htn','dm','cad','appet','pe','ane','classification'))
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
temp$pe[which(is.na(temp$pe))]<- 1


#confusion matrix

table(temp$classification)
mymodel=glm(classification~bgr+bu+ane+pe+dm+cad+age,family = binomial(link = "logit"),data=temp)
pymodel=predict(mymodel,temp)
pymodel



#confusion matrix
tab=table(pymodel>0.5,temp$classification)
tab

accuracy=sum(diag(tab))/sum(tab)*100
accuracy


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


#correlation between blood urea and CKD patients


bloodurea<- temp %>%filter(classification==1) %>% group_by(bu) %>% summarize(n())
bloodurea<- data.frame(bloodurea)

bloodurea<- bloodurea %>% rename(bu=bu,counts=n..)
ggplot(bloodurea,aes(x=bu,y=counts))+geom_point(aes(fill=bu))+stat_smooth(method = "lm",col="#C42126",se=FALSE,size=1)+ggtitle("Graph of number of ckd patients with respect change in BLOOD UREA value")#se=FALSE means dont display standar error,lm:linear regression



#now lets find the correlation between anemia and ckd patients

#in anemia column 0 represnts NO means person has no anemia

#kidney produces erythropoetin hormone which helps in the formation of rbc,lack of rbc causes anemia so person with ckd is more likely to have anemia.
anemia<- temp %>%filter(classification==1 && ane==0) %>% group_by(ane) %>% summarize(n())
anemia<- data.frame(anemia)

anemia<- anemia %>% rename(ane=ane,counts=n..)
ggplot(anemia,aes(x=ane,y=counts))+geom_point(aes(fill=ane))+stat_smooth(method = "lm",col="#C42126",se=FALSE,size=1)+ggtitle("Graph of number of ckd patients with respect to anemia")#se=FALSE means dont display standar error,lm:linear regression


# correlation between pedal edema and CKD patients
#pedaledema<- it is caused by excess fluid getting trapped in tissues,its due to damaged kidney

#
pedaledema<- temp %>%filter(classification==1 && pe==1) %>% group_by(pe) %>% summarize(n())
pedaledema<- data.frame(pedaledema)

pedaledema
#ggplot(data = pedaledema,mapping = aes(x=pe,y=counts))+geom_bar(aes(fill=pe),stat="identity")+theme(legend.position = "none")+xlab("pe")+ylab("counts")+ggtitle("Graph of no of people with ckd having pedal edema")



pedaledema<- pedaledema %>% rename(pe=pe,counts=n..)

ggplot(pedaledema,aes(x=pe,y=counts))+geom_point(aes(fill=pe))+stat_smooth(method = "lm",col="#C42126",se=FALSE,size=1)+ggtitle("Graph of number of ckd patients with respect to pedaledema")#se=FALSE means dont display standar error,lm:linear regression



#corelation between diabetes mellitus and ckd patients

diabetesmellitus<- temp %>%filter(classification==1 && dm==0) %>% group_by(dm) %>% summarize(n())
diabetesmellitus<- data.frame(diabetesmellitus)

diabetesmellitus<- diabetesmellitus %>% rename(dm=dm,counts=n..)
ggplot(diabetesmellitus,aes(x=dm,y=counts))+geom_point(aes(fill=dm))+stat_smooth(method = "lm",col="#C42126",se=FALSE,size=1)+ggtitle("Graph of number of ckd patients with diabetes mellitus")#se=FALSE means dont display standar error,lm:linear regression


#correlation between CAD(coronary artery disease and ckd patients)

cdisease<- temp %>%filter(classification==1 && cad==0) %>% group_by(cad) %>% summarize(n())
cdisease<- data.frame(cdisease)

cdisease<- cdisease %>% rename(cad=cad,counts=n..)
ggplot(cdisease,aes(x=cad,y=counts))+geom_point(aes(fill=cad))+stat_smooth(method = "lm",col="#C42126",se=FALSE,size=1)+ggtitle("Graph of number of ckd patients with CAD ")#se=FALSE means dont display standar error,lm:linear regression

#coronary artery disease is the leading cause of mortality in patients with ckd


#prediction:

#create a new matrix  with new data for testing ,we will use 6 attributes
new.data<- as.data.frame(matrix(c(120,34,1,0,0,0,10),nrow=1))
#give it column names
colnames(new.data)<- c("bgr","bu","ane","pe","dm","cad","age")
#new.data
predict.glm(mymodel,newdata=new.data,type = "response")

