#DO NOT SAVE CHANGES AFTER USING CODE
#THIS CODE WILL IMPORT RAW PROTEUS AND IRGA DATA, AND MERGE DATA BY TEMPERATURE

#SET WORKING DIRECTORY
#either with the menu Session | Set Working Directory | Choose Directory, or
#setwd("~/STA Sample Data Files/xxx")

#IMPORT PROTEUS FILE
#NOTE: file exported by Proteus needs to be opened then saved in Excel before importing
#use Tools | Import Dataset | From Text File
#select no heading, comma separator
#rename proteus raw data as 'proteus' (need to match imported filename)
proteus = ExpDat_FILENAME

#IMPORT IRGA FILE
#NOTE: first row in raw IRGA file needs to be deleted before importing
#select with heading, whitespace separator
#rename irga raw data as 'irga' (need to match the imported filename) 
irga = FILENAME

#INPUT gas flow rate here in ml/min:
flowrate = 42

#ENTER DESIRED OUTPUT FILENAME BELOW
#code will generate file called "AllDat_FILENAME"
output = "FILENAME.csv"

########################################

end=dim(proteus)[1]                               #determine the length of the proteus datafile
temp=as.vector(proteus[,1]);temp=as.numeric(temp) #these lines extract variables of interest from proteus file
time=as.vector(proteus[,2]);time=as.numeric(time) #convert to numeric: by default, proteus data is imported as factors
dsc=as.vector(proteus[,3]);dsc=as.numeric(dsc)    
mass=as.vector(proteus[,4]);mass=as.numeric(mass)
segment=proteus[,6]                               
dsctemp <-data.frame(temp,time,dsc,mass,segment)  #collect just the raw proteus data without header
irgatime=format(irga[,2],format="%H:%M:%S")       #extract time in hour : minute : second format
irgatime1=as.POSIXlt(irgatime,format="%H:%M:%S")  #convert to class POSIXlt in order to do time-based calculations
elapsedtime=irgatime1-irgatime1[1]                #elapsed time is time - first time measured
elapsedtime=as.numeric(elapsedtime)               #convert elapsed time to seconds
delay=((180*pi*(0.15875^2))/flowrate)*60          #use flowrate to determine time delay, where tube length=180cm, radius=0.16cm 
irga_etime=(elapsedtime-delay)/60                 #convert seconds to fractions of a minute. 20 second subtraction to account for time lag
co2=irga$CO2.ppm.                                 #extract CO2 from irga dataset
h2o=irga$H2O.ppt.                                 #extract water from irga dataset

#note that approx will only interpolate over the interval provided by xout; creating a new vector of the same length as the xout vector
irgatemp<-data.frame(irga_etime,co2,h2o)                                   #create temporary data frame to store elapsed time and irga data
co2FunctTime2 <- approx(x=irga_etime, y=irgatemp$co2, xout=dsctemp$time)   #get co2 function of the same time rate as dsc data
co2.new <-co2FunctTime2$y
waterFunctTime2 <- approx(x=irga_etime, y=irgatemp$h2o, xout=dsctemp$time) #get water function of the same time rate as dsc data
h2o.new <- waterFunctTime2$y
plot(temp,co2.new,xlab="Temperature (°C)",ylab="CO2 (ppm)",main= output)   #plot CO2 as a function of elapsed time

proteus.new<-data.frame(temp,time,dsc,mass,co2.new,h2o.new,segment)        #place all data in new dataframe
names(proteus.new)<-c("temp","time","dsc","mass","IRGAco2","IRGAh2o","segment")
write.csv(proteus.new,paste("AllDat_",output,sep=""),row.names=FALSE)      #export new merged file

rm(irgatemp);rm(dsctemp)    #remove temporary dataframes
rm(list=ls())               #remove all objects
