setwd("C:\\project6494\\data")
##################################################################
#download daily weather from 200705 to 201710
y<-as.character(2007:2017)
m<-c(paste("0",as.character(1:9),sep=""),"10","11","12")

a<-c(2:25)
b<-2*a

for (j in y[1:11]) {
  for (i in m[1:12]) {
    if (j==y[11] & i==m[11]){
      break
    }else{}
    if (j==y[1] & i<m[5]){
      next
    }else{}
    zipn<-paste("QCLCD",j,i,".zip",sep="")
    url<-paste("https://www.ncdc.noaa.gov/orders/qclcd/",zipn,sep="")
    trydownload<-try(download.file(url,zipn),silent=T)
    if ('try-error' %in% class(trydownload)) {
      file.remove(zipn)
      rm(zipn,url,trydownload)
      next
    }else{
      dailyn<-paste(j,i,"daily.txt",sep="")
      stationn<-paste(j,i,"station.txt",sep="")
      unzip(zipn,c(dailyn,stationn),overwrite = T)
      daily<-read.csv(dailyn,header=T,stringsAsFactors = F)
      station<-read.csv(stationn,sep="|",header=T,stringsAsFactors = F)
      daily<-daily[,-b]
      daily <- daily[order(daily$WBAN),]
      station <- station[order(station$WBAN),]
      subclimate<-merge(daily,station,by.x="WBAN",by.y="WBAN",all.x=T,all.y=F,sort=T)
      if (j!=y[1] | i!=m[5]) {
        climate<-rbind(climate,subclimate)
      }else{
        climate<-subclimate
      }
      file.remove(zipn,dailyn,stationn)
      rm(daily,station,subclimate,url,zipn,dailyn,stationn,trydownload)
    }
  }
}

#save(climate,file="climate200705-201710.RData")
###########################################################
#load("climate200705-201710.RData")
#Deal with missing data and invalid data
sub=gsub("\\*|[M]|\\s","",cbind(climate$Tmax,climate$Tmin,climate$Tavg,climate$Depart))
climate$Tmax<-sub[,1]
climate$Tmin<-sub[,2]
climate$Tavg<-sub[,3]
climate$Depart<-sub[,4]
rm(sub)

mark=which(climate$Tmax=='')
climate<-climate[-mark,]

mark=which(climate$Tmin=='')
climate<-climate[-mark,]

climate$DewPoint[climate$DewPoint %in% c('-','M')]<-''
climate$WetBulb[climate$WetBulb %in% c('-','M')]<-''

climate$Heat=gsub("[M]|\\s","",climate$Heat)

climate$Cool=gsub("[M]|\\s","",climate$Cool)

climate$CodeSum[climate$CodeSum=='-']<-''

climate$Depth=gsub("\\-|[MT]|\\s","",climate$Depth)

climate<-climate[,-15]#all values of variable Water1 are missing

climate$SnowFall=gsub("\\-|[A-z]|\\s|\\*","",climate$SnowFall)

climate$PrecipTotal=gsub("\\-|[A-z]|\\s|\\*","",climate$PrecipTotal)

climate$StnPressure=gsub("\\-|[A-z]|\\s|\\*","",climate$StnPressure)

climate$SeaLevel=gsub("\\-|[A-z]|\\s|\\*","",climate$SeaLevel)

climate$ResultSpeed=gsub("\\-|[A-z]|\\s|\\*","",climate$ResultSpeed)

climate$ResultDir=gsub("\\-|[A-z]|\\s|\\*","",climate$ResultDir)

climate$AvgSpeed=gsub("\\-|[A-z]|\\s|\\*","",climate$AvgSpeed)

climate$Max5Speed=gsub("\\-|[A-z]|\\s|\\*","",climate$Max5Speed)

climate$Max5Dir=gsub("\\-|[A-z]|\\s|\\*","",climate$Max5Dir)

climate$Max2Speed=gsub("\\-|[A-z]|\\s|\\*","",climate$Max2Speed)

climate$Max2Dir=gsub("\\-|[A-z]|\\s|\\*","",climate$Max2Dir)

climate$WMO=gsub("\\-|[A-z]|\\s|\\*","",climate$WMO)

climate$State=gsub("\\s","",climate$State)
mark=which(climate$State=='')
climate<-climate[-mark,]

climate$GroundHeight[climate$GroundHeight==99999]<-NA
climate$StationHeight[climate$StationHeight==99999]<-NA
climate$Barometer[climate$Barometer==99999]<-NA
climate$Barometer[climate$Barometer==9999]<-NA

#Add date related variables
library(lubridate)
climate$YearMonthDay<-as.Date(as.character(climate$YearMonthDay),"%Y%m%d")
climate$Year<-year(climate$YearMonthDay)
climate$Month<-month(climate$YearMonthDay)
climate$Day<-day(climate$YearMonthDay)
climate$Weekday<-weekdays(climate$YearMonthDay)
climate$Quarter<-quarters(climate$YearMonthDay)

#Add Sunrise and Sunset
library(RAtmosphere)
sunrise<-suncalc(yday(climate$YearMonthDay),climate$Latitude, climate$Longitude,UTC=F)$sunrise
climate$Sunrise<-paste(sprintf("%02d",trunc(sunrise)),":",sprintf("%02d",trunc((sunrise-trunc(sunrise))*60)),sep="")
climate$Sunrise[climate$Sunrise=="NA:NA"]<-"NA" #NaNs produced cause some places don't have sunrise

sunset<-suncalc(yday(climate$YearMonthDay),climate$Latitude, climate$Longitude,UTC=F)$sunset
climate$Sunset<-paste(sprintf("%02d",trunc(sunset)),":",sprintf("%02d",trunc((sunset-trunc(sunset))*60)),sep="")
climate$Sunset[climate$Sunset=="NA:NA"]<-"NA" #NaNs produced cause some places don't have sunset

#add state names
climate$StateName<-state.name[match(climate$State,state.abb)]

#transform formats
##transform characters to numerics
climate$Tmax<-as.numeric(climate$Tmax)
climate$Tmin<-as.numeric(climate$Tmin)
climate$Tavg<-as.numeric(climate$Tavg)
climate$Depart<-as.numeric(climate$Depart)
climate$DewPoint<-as.numeric(climate$DewPoint)
climate$WetBulb<-as.numeric(climate$WetBulb)
climate$Heat<-as.numeric(climate$Heat)
climate$Cool<-as.numeric(climate$Cool)
climate$Depth<-as.numeric(climate$Depth)
climate$SnowFall<-as.numeric(climate$SnowFall)
climate$PrecipTotal<-as.numeric(climate$PrecipTotal)
climate$StnPressure<-as.numeric(climate$StnPressure)
climate$SeaLevel<-as.numeric(climate$SeaLevel)
climate$ResultSpeed<-as.numeric(climate$ResultSpeed)
climate$ResultDir=as.numeric(climate$ResultDir)
climate$AvgSpeed=as.numeric(climate$AvgSpeed)
climate$Max5Speed=as.numeric(climate$Max5Speed)
climate$Max5Dir=as.numeric(climate$Max5Dir)
climate$Max2Speed=as.numeric(climate$Max2Speed)
climate$Max2Dir=as.numeric(climate$Max2Dir)
climate$Barometer<-as.numeric(climate$Barometer)
climate$StationHeight<-as.numeric(climate$StationHeight)
##transform numerics to characters 
climate$WBAN<-sprintf("%05d",climate$WBAN)
climate$WMO<-sprintf("%07d",as.integer(climate$WMO))
climate$ClimateDivisionCode<-as.character(climate$ClimateDivisionCode)
climate$ClimateDivisionStateCode<-as.character(climate$ClimateDivisionStateCode)
climate$ClimateDivisionStationCode<-as.character(climate$ClimateDivisionStationCode)
climate$TimeZone<-as.character(climate$TimeZone)
##transform numerics to integers 
climate$Year<-as.integer(climate$Year)
climate$Month<-as.integer(climate$Month)



#check the dataset structure
str(climate)

#create the id used for merging
climate$id<-paste(climate$StateName,climate$Year,climate$Quarter,sep='')

#order the data by id
climate <- climate[order(climate$id),]

#save(climate,file="final_climate.RData")
#####################################################################
#add GDP
#load("final_climate.RData")
#unzip and read the quarterly state gdp data file
unzip("qgdpstate_all_R.zip","qgdpstate_all_R.csv",overwrite = T)
qgdpstate<-read.csv("qgdpstate_all_R.csv",header=T,stringsAsFactors = F)
#get the industry id and their description, and saved them into dataset industry
mark=which(qgdpstate$GeoFIPS=="00000")
industry<-qgdpstate[mark,c(6,8)]
#eliminate nonrelated data
mark=which(qgdpstate$GeoName %in% state.name)
qgdpstate<-qgdpstate[mark,]

mark1=grep("2005",names(qgdpstate))
mark2=grep("2006",names(qgdpstate))
qgdpstate<-qgdpstate[,-c(1,3,4,5,mark1,mark2)]

for (i in c(1:length(industry$IndustryId))){
  mark=which(qgdpstate$IndustryId==industry$IndustryId[i])
  total<-qgdpstate[mark,]
  total<-total[,-c(2:5)]
  
  #stack the data
  stotal<-data.frame(total[,1], stack(total[,2:ncol(total)]),stringsAsFactors = F)
  names(stotal)<-c("StateName",industry$Description[i],"time")
  stotal$Year<-as.integer(substr(stotal$time,2,5))
  stotal$Quarter<-substr(stotal$time,7,8)
  stotal<-stotal[,-3]

  #create the id used for merging
  stotal$id<-paste(stotal$StateName,stotal$Year,stotal$Quarter,sep='')
  stotal<-stotal[,-c(1,3,4)]
  stotal<-stotal[order(stotal$id),]
  if (i==1){
     #merge the climate with the stotal
    final<-merge(climate,stotal,by.x="id",by.y="id",all.x=T,all.y=F,sort=T)
    #transform GDP into numeric
    n=ncol(final)
    final[,n]<-as.numeric(final[,n])
    #remove useless climate dataset
    rm(climate)
  }else{
    #merge the final with the stotal
    final<-merge(final,stotal,by.x="id",by.y="id",all.x=T,all.y=F,sort=T)
    #transform GDP into numeric
    final[,n]<-as.numeric(final[,n])
  }
}

#elimate variable id
final<-final[,-1]

########################################################################
#save the final dataset into .csv file
write.csv(final,file="final.csv")
########################################################################
#Describe variables
n=length(final[,1])
k=length(final[1,])
str(final)
num=int=char=0
for (i in 1:k){
  if(class(final[,i])=="numeric") num=num+1
  else if(class(final[,i])=="integer") int=int+1
  else if(class(final[,i])=="character") char=char+1
}
c(n,k,num,int,char)
##deatiled description 
library(RColorBrewer)
library(knitr)
###numeric variables
for (i in 1:k ){
  if(class(final[,i])=="numeric"){
    hist(final[,i],xlab=paste(names(final[i])),main=paste("Histogram of",names(final[i])))
    print(summary(final[,i]))
  }
}

###integer variables
####Year
freq=table(final$Year)
freq=as.data.frame(freq)
freq<-freq[order(freq$Var1),]
barplot(freq$Freq,ylim=c(-150000,max(freq$Freq)),width=5.8,space=.2,main="Barplot of Year",ylab="Frequency",xlab="Year")
text(seq(4.3,length.out=11,by=7),rep(-100000,11),label=freq$Var1,cex=.7)  
####Month
freq=table(final$Month)
freq=as.data.frame(freq)
freq<-freq[order(freq$Var1),]
barplot(freq$Freq,ylim=c(-150000,max(freq$Freq)),width=5.8,space=.2,main="Barplot of Month",ylab="Frequency",xlab="Month")
text(seq(4.3,length.out=12,by=7),-100000,label=freq$Var1,cex=.7) 
####Day
freq=table(final$Day)
freq=as.data.frame(freq)
freq<-freq[order(freq$Var1),]
freq
barplot(freq$Freq,ylim=c(-30000,max(freq$Freq)),width=5.8,space=.2,main="Barplot of Day",ylab="Frequency",xlab="Day")
text(seq(4.3,length.out=31,by=7),-15000,label=freq$Var1,cex=.5) 

###charactor variables
####Sunrise and Sunset
hist(sunrise)
summary(sunrise)
hist(sunset)
summary(sunset)
rm(sunrise,sunset) #remove unused values
####Weekday and Quarter
for (i in 43:44){
  freq=table(final[,i])
  freq=as.data.frame(freq)
  pie(freq$Freq,labels=freq$Var1,main=paste("Pie Chart of",names(final[i])),cex=.8)
}
####State
freq=table(final$State)
freq=as.data.frame(freq)
freq<-freq[order(freq$Freq,decreasing=T),]
l=length(freq$Var1)
barplot(rev(freq$Freq),horiz=T,xlim=c(-20000,max(freq$Freq)),width=5.8,space=.2,main="Barplot of State",xlab="Frequency",ylab="State",col=rep(brewer.pal(9,'YlOrRd'),each=15))
text(x=-10000,seq(4.3,length.out=l,by=7),label=freq$Var1,cex=.5) 
####StateName
freq=table(final$StateName)
freq=as.data.frame(freq)
freq<-freq[order(freq$Freq,decreasing=T),]
l=length(freq$Var1)
barplot(rev(freq$Freq),horiz=T,xlim=c(-60000,max(freq$Freq)),width=5.8,space=.2,main="Barplot of StateName",xlab="Frequency",ylab="StateName",col=rep(brewer.pal(9,'YlOrRd'),each=15))
text(x=-20000,seq(4.3,length.out=l,by=7),label=freq$Var1,cex=.5) 
####Timezone
freq=table(final$TimeZone)
freq=as.data.frame(freq)
freq<-freq[order(freq$Freq,decreasing=T),]
l=length(freq$Var1)
barplot(rev(freq$Freq),horiz=T,xlim=c(-60000,max(freq$Freq)),width=5.8,space=.2,main="Barplot of TimeZone",xlab="Frequency",ylab="TimeZone",col=rep(brewer.pal(9,'YlOrRd'),each=2))
text(x=-20000,seq(4.3,length.out=l,by=7),label=freq$Var1,cex=.7) 
####ClimateDivisionCode
freq=table(final$ClimateDivisionCode)
freq=as.data.frame(freq)
freq<-freq[order(freq$Freq,decreasing=T),]
l=length(freq$Var1)
barplot(rev(freq$Freq),horiz=T,xlim=c(-60000,max(freq$Freq)),width=5.8,space=.2,main="Barplot of ClimateDivisionCode",xlab="Frequency",ylab="ClimateDivisionCode",col=rep(brewer.pal(9,'YlOrRd'),each=2))
text(x=-20000,seq(4.3,length.out=l,by=7),label=freq$Var1,cex=.7) 
####ClimateDivisionStateCode
freq=table(final$ClimateDivisionStateCode)
freq=as.data.frame(freq)
freq<-freq[order(freq$Freq,decreasing=T),]
l=length(freq$Var1)
barplot(rev(freq$Freq),horiz=T,xlim=c(-60000,max(freq$Freq)),width=5.8,space=.2,main="Barplot of ClimateDivisionStateCode",xlab="Frequency",ylab="ClimateDivisionStateCode",col=rep(brewer.pal(9,'YlOrRd'),each=10))
text(x=-20000,seq(4.3,length.out=l,by=7),label=freq$Var1,cex=.5) 




