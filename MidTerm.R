####################
#read data
drive0<-read.csv("./drivetest.csv",header=T, stringsAsFactors=F)
str(drive0)
####################
#clean data
drive<-unique(drive0)#remove duplicates
any(duplicated(drive$studentID))

drive<-na.omit(drive)#remove missing data
any(is.na(drive))

# remove the whitespace before or after a string of characters
install.packages("raster")
library(raster)
drive<-trim(drive)

#remove invalid values
drive$sex[!(drive$sex %in% c("M","F"))]<-NA
drive$state[!(drive$state %in% state.abb)]<-NA
drive$citySize[!(drive$citySize %in% c("S","M","L","V"))]<-NA
drive$age[drive$age<17|drive$age>100]<-NA
drive$month[!(drive$month %in% month.abb)]<-NA
drive$glasses[!(drive$glasses %in% c("TRUE","FALSE"))]<-NA
drive$firstTime[!(drive$firstTime %in% c("Y","N"))]<-NA
drive$ampm[!(drive$ampm %in% c("am","pm"))]<-NA
drive$pass[!(drive$pass %in% c("TRUE","FALSE"))]<-NA

drive<-na.omit(drive)
any(is.na(drive))

rm(drive0)#drop original dataset

attach(drive)
####################
#Question 1
#How many observations (rows) remain in your dataset?
n<-length(studentID)
n
####################
#Question 2
#Create a two-by-two table of sex vs. pass (i.e., how many men/women passed/failed)
table(sex,pass)

####################
#Question 3
#Create a four-by-two table of citySize vs. pass. 
#Make sure that the city size categories appear in an increasing order.

#change citySize to an ordinal factor
drive$citySize<-factor(citySize,levels=c("S","M","L","V"),ordered=T)
attach(drive)
table(citySize,pass)

####################
#Question 4
#Create a new variable of age category (ageCat) with the following values:
#Y for 17-24, M for 25-50, and S for 51-100.
ageCat<-rep(NA,n)
for (i in 1:n){
  ageCat[i][age[i]>=17&age[i]<=24]<-"Y"
  ageCat[i][age[i]>=25&age[i]<=50]<-"M"
  ageCat[i][age[i]>=51&age[i]<=100]<-"S"
}

head(ageCat)
####################
#Question 5
#Create a three-by-two table of ageCat vs. pass.
#Make sure that the age category appear in an increasing order.
ageCat<-factor(ageCat,levels=c("Y","M","S"),ordered=T)
table(ageCat,pass)

detach(drive)
####################
#Question 6
#download the population file
download.file("https://www2.census.gov/programs-surveys/popest/tables/2010-2016/state/totals/nst-est2016-01.xlsx",
              "nst-est2016-01.xlsx",mode="wb")

install.packages("xlsx")
library("xlsx")
pop<-read.xlsx("./nst-est2016-01.xlsx",
               sheetIndex=1,startRow=10,endRow=60,colIndex=c(1,10),
               header=F,stringsAsFactors=F)
colnames(pop)<-c("state","pop2016")

m<-length(pop$state)
l<-length(state.abb)
#change the state names in pop to state abbreviations
for (i in 1:m){
  for (j in 1:l) {
    if (pop$state[i]==paste(".",state.name[j],sep="")){
      pop$state[i]<-state.abb[j]
    }

  }
}

#merge two data set
drive2<-merge(drive,pop,by.x="state",by.y="state")

head(drive2)
#########################
#save the final dataset
write.csv(drive2,file="./Chen_Menglei.csv",row.names=F)
