#DO NOT SAVE CHANGES AFTER USING CODE
#THIS CODE WILL EXTRACT AND EXPORT SEGMENT 3 (EXOTHERMIC REGION) WITH OR WITHOUT BLC-DSC DATA

#SET WORKING DIRECTORY
#either with the menu Session | Set Working Directory | Choose Directory, or
#setwd("~/STA Sample Data Files/xxx")

#IMPORT AllDat FILE
#use Tools | Import Dataset | From Text File
#select with heading, comma separator
#rename data by typing filename below: 
proteus.new = AllDat_FILENAME

#ENTER DESIRED OUTPUT FILENAME, code will generate "SEG3_FILENAME"
output = "FILENAME.csv"

tmax=                                   #enter maximum temperature desired for Segment 3 
tscale=                                 #enter desired temperature resolution/step
flowrate=                               #enter measured gas flowrate (mL/min)
samplemass=                             #enter sample mass (mg), REQUIRED
percentC=                               #enter either sample total C concentration (%)
sampleC=samplemass*(percentC/100)       #calculated or enter sample C mass (mg), REQUIRED

########################################

proteus.new2<-proteus.new[which(proteus.new$segment==3),1:dim(proteus.new)[2]]  
tempscale=seq(120,tmax,tscale)     #temp begins at 120ºC and goes to user input tmax
proteus.new3=matrix(0,nrow=length(tempscale),ncol=length(proteus.new2),dimnames=c(list(1:length(tempscale)),list(names(proteus.new2))))
proteus.new3[,1]=tempscale

for (i in seq(length(proteus.new2)-1)) {
    varscaled<-approx(x=proteus.new2[,1],y=proteus.new2[,i+1],xout=tempscale)
    proteus.new3[,i+1]<-varscaled$y
}

proteus.new4=data.frame(proteus.new3[,])
flowrate1=rep(flowrate,times=length(tempscale))
proteus.new4$flowrate=flowrate1
samplemass1=rep(samplemass,times=length(tempscale))
proteus.new4$samplemass=samplemass1
sampleC1=rep(sampleC,times=length(tempscale))
proteus.new4$sampleC=sampleC1
proteus.new4$segment <- NULL

write.csv(proteus.new4,paste("SEG3_",output,sep=""),row.names=FALSE)   #export seg 3 file
rm(list=ls())                                                          #remove all objects
