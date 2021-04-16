#setwd :setting the csv file directory directory

setwd <- setwd("C:\\Users\\shrikant padhy\\Desktop\\ckddataset")
#project directory

getwd()

#read csv file
data <- read.csv("kidney_disease.csv",header=T)

#data has the csv file
#convert it to data frames

data <- data.frame(data)

#view first n row bydefault its 6 
head(data)

#view dataframe
View(data)
