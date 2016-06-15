#DO NOT SAVE CHANGES AFTER USING CODE
#THIS CODE WILL MERGE BASELINE CORRECTED (BLC) DSC DATA FROM PEAKFIT TO AllDat FILE

################################################
#GO TO PEAKFIT AND PERFORM BLC USING AllDat FILE
################################################

#SET WORKING DIRECTORY
#either with the menu Session | Set Working Directory | Choose Directory, or
#setwd("~/STA Sample Data Files/xxx")

#IMPORT AllDat FILE
#use Tools | Import Dataset | From Text File
#select with heading, comma separator
#rename data by inputing filename below:
proteus.new = AllDat_FILENAME

#IMPORT BASELINE CORRECTED DSC
#use Tools | Import Dataset | From Text File
#select with heading, whitespace separator
#rename data by inputing your filename below:
dscBLC = dscBLC_FILENAME

proteus.new$BLCdsc<-dscBLC[,2]                                        #append baseline corrected dsc to proteus.new

#ENTER DESIRED OUTPUT FILENAME
write.csv(proteus.new,paste("AllDat_FILENAME.csv"),row.names=FALSE)   #export the merged AllDat with BLC-DSC data
rm(list=ls())                                                         #remove all objects
