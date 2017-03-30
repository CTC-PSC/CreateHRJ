#######################
## READ IN .HRJ DATA ##
#######################
#2016 ERA - 2014/2015 ISBM
#Set user directory
userDir=choose.dir()
#Read in HRJ files in a directory
by=readHRJdir(userDir, nFisheries=80, straysinescap=TRUE, Age6="ignore")
#Convert to CY layout
cy=convertHRJ_BYtoCY(by)
#Convert HRJ from R to Access format
z.by=convertHRJ_RtoAccess(by)
z.cy=convertHRJ_RtoAccess(cy)
#Write output for "hand calcs"
write.csv(z.cy$HRJ_BY, "2013 hrj, cy layout, by method.csv")
write.csv(z.cy$HRJ_CY, "2013 hrj, cy layout, cy method.csv")
