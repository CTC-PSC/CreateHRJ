#######################
## LOAD HRJ FUNCTION ##
#######################
source("CreateHRJFunctions.R")
#######################
## READ IN .HRJ DATA ##
#######################
#2014 ERA - 2012/2013 ISBM 
#Set user directory
 userDir=userDir=choose.dir()
#Read in HRJ files in a directory
 by=readHRJdir(userDir, nFisheries=68, straysinescap=FALSE, Age6="ignore")
#Convert to CY layout
 cy=convertHRJ_BYtoCY(by)
#Convert HRJ from R to Access format
 z.by=convertHRJ_RtoAccess(by)
 z.cy=convertHRJ_RtoAccess(cy) 
#Write 
 write.csv(z.cy$HRJ_BY, "2012 hrj, cy layout, by method.csv")
 write.csv(z.cy$HRJ_CY, "2012 hrj, cy layout, cy method.csv")
