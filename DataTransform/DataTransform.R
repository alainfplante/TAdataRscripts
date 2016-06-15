#DO NOT SAVE CHANGES AFTER USING CODE
#THIS CODE WILL COMPILE/TRANSFORM DATASETS FOR GRAPHING AND MULTIVARIATE ANALYSIS

#SET WORKING DIRECTORY
#either with the menu Session | Set Working Directory | Choose Directory, or
setwd("~/STA Sample Data Files/xxx")

#IMPORT SEG3 master data file
#Code written to compile many samples together.
#Datafile must include a column called "sampleno", starting at 1,
#and a column called "sampleid"
#use Tools | Import Dataset | From Text File
#select with heading, comma separator

#Attach imported dataset by entering filename below
data=SEG3_FILENAME
attach(data)

#Select variable of interest to create compiled datasets
#acceptable values are: dsc, BLCdsc (if exists), mass, IRGAco2, IRGAh2o
var=

#SELECT A TEMPERATURE SCALE ON WHICH TO PLACE DATA
#recommend tempscale that produces a number of intervals equal approximately 1/10 number of samples
tmin=120
tscale=0.2
tmax=800

#Enter desired name of output file
output="transform_FILENAME"

########################################
  
var_norm=var/sampleC;data$var_norm=var_norm #normalize selected variable to carbon concentration
tempscale=seq(tmin,tmax,tscale)             #build the temperature scale used
length=length(tempscale)
SampleID1=c(levels(sampleid))               #create a list of sample ids for column names instead
SampleID2=c("temp",SampleID1)
var_PCA=data.frame(tempscale)               #initialize dataframes to store PCA-ready data of interest

for (i in seq(max(sampleno))) {
  #loop through master datasheet, selecting individual sample data and adding to new dataframe
  select_var=subset(data,sampleno==i,select=var_norm);
  select_temp=subset(data,sampleno==i,select=temp)
  #get selected data on new temp scale
  select_var_scale<-approx(x=select_temp[,1],y=select_var[,1],xout=tempscale)
  #add scaled data to new matrix
  var_PCA[,i+1]<-select_var_scale$y
  name=as.character(sampleid[sampleno==i][1]) 
  colnames(var_PCA)[i+1]=name
}
 
var_PCA1=t(var_PCA)                                      #transpose dsc_scaled and co2_scaled
var_PCA1=var_PCA1[2:dim(var_PCA1)[1],]                   #remove first row which becomes temperature on transposing
tempscale1=c(tempscale);tempscale1=as.vector(tempscale1) #vectorize the temp scale
colnames(var_PCA1)[1:length]<-tempscale[1:length]        #give columns appropriate temperature names
write.csv(var_PCA1,paste(output,".csv",sep=""))
rm(list=ls())
