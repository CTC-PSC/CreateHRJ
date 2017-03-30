#######################
## READ IN .HRJ DATA ##
#######################
##########
#2017 ERA#
##########
userDir=choose.dir()
#Read in HRJ files in a directory
by=readHRJdir(userDir, nFisheries=79, straysinescap=TRUE, Age6="ignore")
#Convert to CY layout
cy=convertHRJ_BYtoCY(by)
#Convert HRJ from R to Access format
z.by=convertHRJ_RtoAccess(by)
z.cy=convertHRJ_RtoAccess(cy)
#add the 'preferred' table to the Access format
z.by = addPTableHRJ(z.by, hrjclass = "Access")
z.cy = addPTableHRJ(z.cy, hrjclass = "Access")
#Write output for "hand calcs"
write.csv(z.cy$HRJ_BY, "2017ERA hrj, cy layout, by method.csv")
write.csv(z.cy$HRJ_CY, "2017ERA hrj, cy layout, cy method.csv")
write.csv(z.cy$HRJ_P,  "2017ERA hrj, cy layout, p method.csv")
write.csv(z.cy$ESC_BY, "2017ERA hrj, cy layout, escap.csv")
write.csv(z.by$HRJ_P,  "2017ERA hrj, BY layout, p method.csv")
write.csv(z.by$ESC_BY, "2017ERA hrj, BY layout, escap.csv")
write.csv(z.by$HRJ_BY,  "2017ERA hrj, BY layout, by method.csv") #for HRJ db
write.csv(z.by$HRJ_CY,  "2017ERA hrj, BY layout, cy method.csv") #for HRJ db
