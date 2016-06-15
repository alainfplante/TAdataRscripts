#DO NOT SAVE CHANGES AFTER USING CODE
#THIS CODE WILL CALCULATE A SET OF SUMMARY STATISTICS FOR ONE SEG3 FILE AT A TIME
#IT CAN ETHER GENERATE A NEW SUMMARY FILE (OPTION 1), OR APPEND TO AN EXISTING ONE (OPTION 2)

#SET WORKING DIRECTORY
#either with the menu Session | Set Working Directory | Choose Directory, or
setwd("T:/STA Sample Data Files/xxx")

#IMPORT SEG3 data file
#use Tools | Import Dataset | From Text File
#select with heading, comma separator
#rename data by inputing filename below:
masterdata=SEG3_FILENAME
attach(masterdata)

#OPTION 1: CREATE NEW DATA FRAME TO STORE AND OUTPUT SUMMARY STATS
#use this option only for the first file being added to a Summary Stats file
summaryall = data.frame(matrix(vector(), 0, 15, dimnames=list(c(), c("sampleid","tstart_degC","tend_degC","energy_mJ","energyconc_mJmgsoil","energydensity_mJmgC","exoenergy_mJ","DSCT50_degC","DSCmax_degC","sumCO2_mgC","CO2conc_mgCgsoil","CO2yield_perc","CO2T50_degC","CO2max_degC","TGT50_degC"))),check.names=F,stringsAsFactors=F)
output = "summarystats_FILENAME.csv"

#OPTION 2: IMPORT AND APPEND TO AN EXISTING SUMMARY STATS FILE
#make sure the headers in this file are not altered from the code's output
summaryall = read.csv("summarystats_FILENAME.csv", stringsAsFactors=FALSE)
output = "summarystats_FILENAME.csv"

#Enter an ID for this sample
SampleID= "SAMPLEID"

#ENTER START AND END RANGE FOR DSC, CO2 and TG INTEGRATIONS
#skip and leave blank if you wish to integrate over entire temp range
setstart=
setend=
    
########################################
library(Bolstad)  # call the Bolstad package for numerical integration
masterdata[is.na(masterdata)]<-0  # convert NA values to zero

# create range for integrations
# if instart or intend are not user specified, these values default to tmin and tmax from SEG3 file
if (exists("setstart")) {
    intstart=setstart
}  else {intstart=temp[1]}  
if (exists("setend")) {
    intend=setend
}  else {intend=max(temp,na.rm=T)}
tstep=temp[2]-temp[1]  # temperature step, which in SEG3 file is typically 0.2
range=seq(from=intstart, to=intend, by=tstep)  # create sequence from start to end by temperature step
seconds=time*60; masterdata$seconds=seconds  # convert time from minutes to second and add to data
seconds.sub=subset(seconds,temp >= range[1]&temp <  max(range))
temp.sub=subset(temp,temp >= range[1]&temp <  max(range))  # subset temperature over prescribed range

#Define variables in ideal gas equation for CO2-C conversions
P=1.004  # atmospheric pressure, in bar
R=0.08206  # ideal gas constant, in L atm/mol K
T=324.25  # temperature, in Kelvin (51.1ºC)

#DSC summary stats
if (exists("BLCdsc")) {
    BLCdsc.sub=subset(BLCdsc,temp >= range[1]&temp <  max(range))  # subset BLCdsc over prescribed temp range
    energy=(sintegral(seconds.sub,BLCdsc.sub)$value)  # total dsc-seconds integral
    dsc.exo=subset(BLCdsc.sub,BLCdsc.sub > 0)  # subset for positive dsc values only
    seconds.exo=subset(seconds.sub,BLCdsc.sub > 0)  # subset for seconds associated with positive dsc values only
    temp.exo=subset(temp.sub,BLCdsc.sub > 0)  # subset temperature associated with positive dsc values
    energy.exo=sintegral(seconds.exo,dsc.exo)$value  # determine net (exothermic) dsc integral
    energy.mgsample=energy/samplemass[1]  # determine energy content on per sample mass basis
    energy.mgC=energy/sampleC[1]  # determine energy density on per sample C basis
    dsc.maxindex=which.max(dsc.exo)  # determine max positive energy flux
    dsc.max=temp.exo[dsc.maxindex]  # temperature of max positive energy flux 
    tempint=sintegral(temp.exo,dsc.exo)$cdf$x  # extract temperature values in cumulative distribution function
    dsc_cum=sintegral(temp.exo,dsc.exo)$cdf$y  # extract cumulative dsc values
    m=max(dsc_cum)*0.5  # 1/2 of cumulative distribution function
    dscindex=which(abs(dsc_cum-m)==min(abs(dsc_cum-m))) #index of cumulative dsc value closest to 1/2
    dsct50=tempint[dscindex]  # dsc t50 is the temperature occuring at the same index
}  else {print("dsc baseline does not exist")}

#CO2 summary stats
if (exists("IRGAco2")) {
    firstvalue=which(!is.na(IRGAco2))  # find first non-NA value
    zero=mean(IRGAco2[min(firstvalue):(min(firstvalue)+10)])  # set baseline as mean of first 10 NA values
    IRGAco2[is.na(IRGAco2)]<-0  # convert NA values to zero
    co2.blc=subset(IRGAco2,temp >= range[1]&temp <  max(range))-zero  # subset over prescribed temp range and perform baseline subtraction
    co2.blc[co2.blc <0]<-0  # convert negative values to zero
    V=((flowrate[1])/60)/1000  # gas flow from SEG3 data, in L/s
    c.co2=co2.blc*(((P*V)/(R*T))*12)  # convert CO2 data from ppm to grams C
    tot=max(sintegral(seconds.sub,c.co2)$cdf$y)  # total grams of co2 evolved over time
    sumCO2=tot/1000  # convert total CO2 evolved from g to mg
    co2.cconc=sumCO2/(samplemass[1]/1000)  #determine total CO2-C concentration in mgC/gsoil
    co2yield=sumCO2/sampleC[1]*100  # proportion of CO2 evolved as % of total sample C
    co2.maxindex=which.max(co2.blc)  # value of peak CO2 evolution
    co2.max=temp.sub[co2.maxindex]  # temp of peak CO2 evolution
    tempint1=sintegral(temp.sub,co2.blc,n.pts=length(temp.sub))$cdf$x  # extract temperature values in cumulative distribution function
    co2.cum=sintegral(temp.sub,co2.blc,n.pts=length(temp.sub))$cdf$y  # extract cumulative CO2 values in cumulative distribution function
    c=max(co2.cum)*0.5  # amount of CO2 at 50% of total evolved
    co2index=which(abs(co2.cum-c)==min(abs(co2.cum-c)))  # index for 50% CO2 evolved
    co2t50=tempint1[co2index]  # CO2 T50
}  else {print("co2 data does not exist")}

#TG summary stats
mass.sub=subset(mass,temp >= range[1]&temp <  max(range))
massloss50=max(mass.sub,na.rm=T)-((max(mass.sub,na.rm=T)-min(mass.sub,na.rm=T))/2)
massindex=which(abs(mass.sub-massloss50)==min(abs(mass.sub-massloss50),na.rm=T))
tgt50i=temp.sub[massindex]
tgt50=mean(tgt50i)

summary=data.frame(SampleID,intstart,intend,energy,energy.mgsample,energy.mgC,energy.exo,dsct50,dsc.max,sumCO2,co2.cconc,co2yield,co2t50,co2.max,tgt50,stringsAsFactors=F)
summaryall[nrow(summaryall)+1,]=summary         #append new row to summary file
write.csv(summaryall,output,row.names=FALSE)    #write to CSV summary file
rm(list=ls())                                   #remove all variables
