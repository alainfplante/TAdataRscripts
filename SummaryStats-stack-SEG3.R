# DO NOT SAVE CHANGES AFTER USING CODE
# THIS CODE WILL CALCULATE SUMMARY STATS IN BATCH MODE FOR MULTIPLE SAMPLES

# SET WORKING DIRECTORY
# either with the menu Session | Set Working Directory | Choose Directory, or
setwd("~/STA Sample Data Files/xxx")

# IMPORT SEG3 master file, which must include a column called "sampleno", starting at 1+i
# for each sample, and a column called "sampleid" repeated in each row of each sample.
# use Tools | Import Dataset | From Text File
# select with heading, comma separator
# rename data by inputing filename below:
masterdata=SEG3_FILENAME
attach(masterdata)

# ENTER DESIRED OUTPUT FILENAME
output = "summarystats_FILENAME.csv"  

# ENTER START AND END TEMPERATURE RANGE FOR DSC, CO2 and TG INTEGRATIONS
# skip if you wish to integrate over entire temp range of each sample
setstart=
setend=
    
########################################
library(Bolstad)  # call the Bolstad package for numerical integration
masterdata[is.na(masterdata)]<-0  # convert NA values to zero
seconds=time*60;masterdata$seconds=seconds  # convert time in minutes to seconds

# define empty vectors to store each of the summary values
name=vector(mode="character",length=max(sampleno))
tstart=vector(mode="numeric",length=max(sampleno))
tend=vector(mode="numeric",length=max(sampleno))
energy=vector(mode="numeric",length=max(sampleno))
energy.exo=vector(mode="numeric",length=max(sampleno))
energy.mgsample=vector(mode="numeric",length=max(sampleno))
energy.mgC=vector(mode="numeric",length=max(sampleno))
dsct50=vector(mode="numeric",length=max(sampleno))
dsc.max=vector(mode="numeric", length=max(sampleno))
sumCO2=vector(mode="numeric",length=max(sampleno))
co2.cconc=vector(mode="numeric",length=max(sampleno))
co2yield=vector(mode="numeric",length=max(sampleno))
co2t50=vector(mode="numeric",length=max(sampleno))
co2.max=vector(mode="numeric", length=max(sampleno))
tgt50=vector(mode="numeric",length=max(sampleno))

# define variables in idea gas equation for c-co2 conversions
P=1.004  # atmospheric pressure, in bar
R=0.08206  # ideal gas constant, in L atm/mol K
T=324.25  # temperature, in Kelvin (51.1ºC)

# start for-loop that iteratively goes through every sample in the dataset
# calculating summary values and placing those values in a summary matrix
for (i in seq(max(sampleno))) {
    name[i]=as.character(sampleid[sampleno==i][1])  # sample ID associated with the sample number
    sampletemp=temp[sampleno==i]
    if (exists("setstart")) {
        intstart=setstart  # if user specified start temp, use it
    }  else {intstart=min(sampletemp)}  # otherwise, use min temp for sample from SEG3
    if (exists("setend")) {
        intend=setend    # if user specified end temp, use it
    }  else {intend=max(sampletemp,na.rm=T)}  
    tstart[i]=intstart
    tend[i]=intend
    tstep=sampletemp[2]-sampletemp[1]  # temperature step, which in SEG3 file is typically 0.2
    range=seq(from= intstart, to = intend, by =tstep)  # create sequence from start to end by temperature step
    seconds.sub=subset(seconds,sampleno==i&temp >= range[1]&temp <  max(range))  # subset seconds for sample and for temp range
    temp.sub=subset(temp,sampleno==i&temp >= range[1]&temp <  max(range))  # subset temp for sample and for temp range
    
    if (exists("BLCdsc")) {
        BLCdsc.sub=subset(BLCdsc,sampleno==i&temp >= range[1]&temp <  max(range))  # subset BLCdsc over prescribed temp range
        energy[i]=(sintegral(seconds.sub,BLCdsc.sub)$value)  # total dsc-seconds integral
        dsc.exo=subset(BLCdsc.sub,BLCdsc.sub > 0)  # subset for positive dsc values only
        seconds.exo=subset(seconds.sub,BLCdsc.sub > 0)  # subset for seconds associated with positive dsc values only
        temp.exo=subset(temp.sub,BLCdsc.sub > 0)  # subset temperature associated with positive dsc values
        energy.exo[i]=sintegral(seconds.exo,dsc.exo)$value  # determine net (exothermic) dsc integral
        energy.mgsample[i]=energy[i]/(subset(samplemass,sampleno==i)[1])  # determine energy content on per sample mass basis
        energy.mgC[i]=energy[i]/(subset(sampleC,sampleno==i)[1])  # determine energy density on per sample C basis
        dsc.maxindex=which.max(dsc.exo)  # determine max energy flux
        dsc.max[i]=temp.exo[dsc.maxindex]  # temperature of max DSC
        tempint=sintegral(temp.exo,dsc.exo)$cdf$x  # extract temperature values in cumulative distribution function
        dsc_cum=sintegral(temp.exo,dsc.exo)$cdf$y  # extract cumulative dsc values
        m=max(dsc_cum)*0.5  # 1/2 of cumulative distribution function
        dscindex=which(abs(dsc_cum-m)==min(abs(dsc_cum-m)))  # index of cumulative dsc value closest to 1/2
        dsct50[i]=tempint[dscindex]  # dsc t50 is temperature occuring at same index
    }  else {print("dsc baseline does not exist")}
    
    if (exists("IRGAco2")) {
        co2.blc=subset(IRGAco2,sampleno==i)  # get CO2 data for sample
        firstvalue=which(!is.na(co2.blc))  # find first non-NA value
        zero=mean(co2.blc[min(firstvalue):(min(firstvalue)+10)])  # set baseline as mean of first 10 NA values
        co2.blc[is.na(co2.blc)]<-0  # convert NA values to zero
        co2.blc=subset(co2.blc,sampletemp >= range[1]&sampletemp <  max(range))-zero  # subset over prescribed temp range and perform baseline subtraction
        co2.blc[co2.blc <0]<-0  # convert negative values to zero
        V=((flowrate[sampleno==i][1])/60)/1000  # gas flow from SEG3 data, in L/s
        c.co2=co2.blc*(((P*V)/(R*T))*12)  # convert CO2 data from ppm to grams C
        tot=max(sintegral(seconds.sub,c.co2)$cdf$y)  # total grams of CO2 evolved over time
        sumCO2[i]=tot/1000  # convert total CO2 evolved from g to mg
        co2.cconc[i]=sumCO2[i]/((subset(samplemass,sampleno==i)[1])/1000)  # determine total CO2-C concentration in mgC/gsoil
        co2yield[i]=sumCO2[i]/(subset(sampleC,sampleno==i)[1])*100  # proportion of CO2 evolved as % of total sample C
        co2.maxindex=which.max(co2.blc)  # value of peak CO2 evolution
        co2.max[i]=temp.sub[co2.maxindex]  # temp of peak CO2 evolution
        tempint1=sintegral(temp.sub,co2.blc,n.pts=length(temp.sub))$cdf$x  # extract temperature values in cumulative distribution function
        co2.cum=sintegral(temp.sub,co2.blc,n.pts=length(temp.sub))$cdf$y  # extract cumulative CO2 values in cumulative distribution function
        c=max(co2.cum)*0.5  # amount of CO2 at 50% of total evolved
        co2index=which(abs(co2.cum-c)==min(abs(co2.cum-c)))  # index for 50% CO2 evolved
        co2t50[i]=tempint1[co2index]  # CO2 T50
    }  else {print("co2 data does not exist")}
    
    # TG-T50
    mass.sub=subset(mass,sampleno==i&temp >= range[1]&temp <  max(range))
    massloss50=max(mass.sub,na.rm=T)-((max(mass.sub,na.rm=T)-min(mass.sub,na.rm=T))/2)
    massindex=which(abs(mass.sub-massloss50)==min(abs(mass.sub-massloss50),na.rm=T))
    tgt50i=temp.sub[massindex]
    tgt50[i]=mean(tgt50i)
}

summary=data.frame(name,tstart,tend,energy,energy.mgsample,energy.mgC,energy.exo,dsct50,dsc.max,sumCO2,co2.cconc,co2yield,co2t50,co2.max,tgt50)
names(summary)=c("sampleid","tstart_degC","tend_degC","energy_mJ","energyconc_mJmgsoil","energydensity_mJmgC","exoenergy_mJ","DSCT50_degC","DSCmax_degC","sumCO2_mgC","CO2conc_mgCgsoil","CO2yield_perc","CO2T50_degC","CO2max_degC","TGT50_degC")
write.csv(summary,output,row.names=FALSE)  # export a comma-delimited text file
rm(list=ls())  # remove all variables
