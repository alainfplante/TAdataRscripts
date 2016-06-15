#DO NOT SAVE CHANGES AFTER USING CODE
#THIS CODE WILL EXPORT DSC DATA FOR BASELINE CORRECTION (BLC) IN PEAKFIT AND MERGE CORRECTED DATA to AllDat FILE

#SET WORKING DIRECTORY
#either with the menu Session | Set Working Directory | Choose Directory, or
#setwd("~/STA Sample Data Files/xxx")

#IMPORT AllDat FILE
#use Tools | Import Dataset | From Text File
#select with heading, comma separator
#rename data by inputing filename below:
proteus.new = AllDat_FILENAME

#ENTER DESIRED OUTPUT FILENAME, code will generate file "BLC_FILENAME"
output = "FILENAME.csv"

proteus.new2<-proteus.new[which(proteus.new$segment==3), c(1,3)]        #subset proteus.new to select only segment 3, dsc, temp
write.csv(proteus.new2,paste("BLC_",output,sep=""),row.names=FALSE)   #export temporary file for BLC (segment 3 only)
rm(proteus.new2)                                                        #remove the segment 3 dataframe

########################################
#GO TO PEAKFIT AND PERFORM BLC
########################################

#IMPORT BASELINE CORRECTED DSC
#use Tools | Import Dataset | From Text File
#select with heading, whitespace separator
#rename data by inputing your filename below:
dscBLC = dscBLC_FILENAME

seg12length <- dim(subset(proteus.new, segment == 1 | segment == 2))[1]  #determine the length of proteus.new occupied by segment 1&2
lengthtotal<-dim(proteus.new)[1]                                         #and determine length of full proteus.new
mergedata=matrix(nrow=lengthtotal,ncol=2)                                #create a new temporary matrix of same length as proteus.new to append BLC DSC data
mergedata[(seg12length+1):lengthtotal,1]<-dscBLC[,1]                     #add BLC DSC to temporary dataframe at correct position
mergedata[(seg12length+1):lengthtotal,2]<-dscBLC[,2]
proteus.new$BLCdsc<-mergedata[,2]                                        #append baseline corrected dsc to proteus.new
write.csv(proteus.new,paste("AllDat_",output,sep=""),row.names=FALSE)           #export the merged AllDat with BLC-DSC data
rm(list=ls())                                                            #remove all objects
