###########################################
# readHRJ
#
# Description
# ------------------
# reads in a. HRJ file and returns a list object with the n 
#
# Argument(s)
# ------------------
# filename
# nFisheries
# straysinescap
# Age6
#
# Output(s)
# ------------------
# list object 
###########################################
#readHRJ
readHRJ <- function(filename, nFisheries=69, straysinescap=TRUE, Age6=c("DNE","ignore","include")) {
  #Read in the file by line
   rawHRJ <- readLines(filename)
  #Determine if there are strays in escapement
    #best let user specify... #if((length(strsplit(rawHRJ[1]," ")[[1]][nchar(strsplit(rawHRJ[1], split=" ")[[1]])!=0])-3) %% 3 == 0) 
  #Determine the number of years of data in the HRJ
   nYears <- length(rawHRJ)/(nFisheries+1) #+1 b/c first row always contains escapement
   if(I(nYears-floor(nYears))!=0) cat("WARNNG: number of fisheries in the HRJ and what's user specified mismatch\n")
  #First, subset the data to escapRows and fishRows
   escRows <- rawHRJ[seq(1, length(rawHRJ), by=nFisheries+1)]
   fshRows <- rawHRJ[-seq(1, length(rawHRJ), by=nFisheries+1)]
  #Second, set up the vectors for the access database dependent on;
   #1)if strays are present or not
   #2)what to do with 5 age HRJs (2:6 vs. 2:5)
   hrjNamFish = c("AEQCat","AEQTot","NomCat","NomTot","Pop")
   if(straysinescap) {
     hrjNamEsc = c("All_Esc", "CA_Esc", "US_Esc")
   } else hrjNamEsc = "Esc"
   #given the straysinescap and Age6 option specified
    if(Age6=="DNE" || Age6=="ignore") {
      if(straysinescap) {
         hrjEscVec = sort(paste(hrjNamEsc, sort(rep(2:5,3)), sep=""))
      } else hrjEscVec = paste(hrjNamEsc, 2:5, sep="")
      hrjFshVec = sort(paste(hrjNamFish, sort(rep(2:5,5)), sep=""))
    } else if(Age6=="include") {
      if(straysinescap) {
        hrjEscVec = sort(paste(hrjNamEsc, sort(rep(2:6,3)), sep=""))
      } else hrjEscVec = paste(hrjNamEsc, 2:6, sep="")
      hrjFshVec = sort(paste(hrjNamFish, sort(rep(2:6,5)), sep=""))     
    }  else cat("WARNING: user-specified Age6 option not recognized\n")
  #Third, set up the number of escapement and fishery data matrices
   escMat <- as.data.frame(matrix(NA, ncol=length(hrjEscVec)+3, nrow=length(escRows)))
   names(escMat) = c("brood", "fishery", "oldestage", hrjEscVec)
   fshMat <- as.data.frame(matrix(NA, ncol=length(hrjFshVec)+3, nrow=length(fshRows)))
   names(fshMat) = c("brood", "fishery", "oldestage", hrjFshVec)
  #Fourth, load the escapement data
   for(i in 1:length(escRows)) {
    tmp = strsplit(escRows[i]," ")[[1]][nchar(strsplit(escRows[i], split=" ")[[1]])!=0]
    escMat[i,"brood"] = as.numeric(tmp[1])
    escMat[i,"fishery"] = as.numeric(tmp[2])
    escMat[i,"oldestage"] = as.numeric(tmp[3])
    #if length of the HRJ row is 3, there's no data, so do nothing
    if(length(tmp)!=3) {
      #oldestage = as.numeric(tmp[3]) (stocks that have age 5 & 6 collapsed reports the oldestage as 6, not 5, so i can't reference that value)
      oldestage = as.numeric(tmp[3]) #length(tmp[4:length(tmp)])/ifelse(straysinescap,3,1)+1
      minage = as.numeric(tmp[3]) - length(tmp[4:length(tmp)])/ifelse(straysinescap,3,1)+1
      hrjEscTmp = sort(paste(hrjNamEsc, sort(rep(minage:oldestage,ifelse(straysinescap,3,1))), sep=""))
      tmp2 = as.numeric(tmp[4:length(tmp)])
      names(tmp2) = hrjEscTmp[1:length(tmp2)]
      #IFFF YOU WANT TO IGNORE AGE6, YOU HAVE TO READ THE DATA IN, BUT IT IS THEN DROPPED HERE!!!!
      #Case only applies if age6 is present
      if(Age6=="ignore" && length(-grep(6,names(tmp2)))!=0) {
        escMat[i,names(tmp2)[-grep(6,names(tmp2))]] = tmp2[-grep(6,names(tmp2))]
      } else escMat[i,names(tmp2)] = tmp2     
    }
   }
  #Fifth, load the fishery data
   for(i in 1:length(fshRows)) {
     tmp = strsplit(fshRows[i]," ")[[1]][nchar(strsplit(fshRows[i], split=" ")[[1]])!=0]
     fshMat[i,"brood"] = as.numeric(tmp[1])
     fshMat[i,"fishery"] = as.numeric(tmp[2])
     fshMat[i,"oldestage"] = as.numeric(tmp[3])
     #if length of the HRJ row is 3, there's no data, so do nothing
     if(length(tmp)!=3) {
       #oldestage = as.numeric(tmp[3]) (stocks that have age 5 & 6 collapsed reports the oldestage as 6, not 5, so i can't reference that value)
       oldestage = as.numeric(tmp[3]) #length(tmp[4:length(tmp)])/5 + 1
       minage = as.numeric(tmp[3]) - length(tmp[4:length(tmp)])/5 + 1
       
       hrjFshTmp = sort(paste(hrjNamFish, sort(rep(minage:oldestage,5)), sep=""))
       tmp2 = as.numeric(tmp[4:length(tmp)])
       names(tmp2) = hrjFshTmp[1:length(tmp2)]
       #IFFF YOU WANT TO IGNORE AGE6, YOU HAVE TO READ THE DATA IN, BUT IT IS THEN DROPPED HERE!!!!
       #Case only applies if age6 is present
       if(Age6=="ignore" && length(-grep(6,names(tmp2)))!=0) {
         fshMat[i,names(tmp2)[-grep(6,names(tmp2))]] = tmp2[-grep(6,names(tmp2))]
       } else fshMat[i,names(tmp2)] = tmp2      
     }
   }
  #Sixth, return results to user
 return(list(ESC=escMat,HRJ=fshMat,strays=straysinescap,nFisheries=nFisheries))
}

###########################################
# .convertHRJ_BYtoCY_bytable
#
# Description
# ------------------
# Internal function that manipulates an HRJ standardized R format object
# from brood year data to calendar year data and
# determines if a brood is complete
#
# Dependent(s)
# ------------------
# .convertHRJ_BYtoCY_bytable - interal function that 
#
# Argument(s)
# ------------------
#
# Output(s)
# ------------------
# 
###########################################
.convertHRJ_BYtoCY_bytable <- function(x) {
 #subset the data by age
  headings=x[,match(c("brood","fishery","oldestage","stock"), names(x), nomatch=0)]
  x.age2 = cbind(headings,x[,c(grep("2", names(x)))])
  x.age3 = cbind(headings,x[,c(grep("3", names(x)))])
  x.age4 = cbind(headings,x[,c(grep("4", names(x)))])
  x.age5 = cbind(headings,x[,c(grep("5", names(x)))])
  x.age6 = cbind(headings,x[,c(grep("6", names(x)))])
 #add cy date to each
  x.age2$cy <- x.age2$brood+2
  x.age3$cy <- x.age3$brood+3
  x.age4$cy <- x.age4$brood+4
  x.age5$cy <- x.age5$brood+5
  x.age6$cy <- x.age6$brood+6
 #create blank matrix
  minCY <- min(x.age2$cy,x.age3$cy,x.age4$cy,x.age5$cy,x.age6$cy)
  maxCY <- max(x.age2$cy,x.age3$cy,x.age4$cy,x.age5$cy,x.age6$cy)
  cy.HRJ = matrix(NA,nrow=max(x$fishery)*(maxCY-minCY+1),ncol=ncol(x)+1)
  colnames(cy.HRJ) = c(names(x),"cy")
  cy.HRJ = as.data.frame(cy.HRJ)
  cy.HRJ$cy = sort(rep(minCY:maxCY,max(x$fishery)))
 #if the max fishery number is 0, assume that we're dealing with escapement. 
  if(max(x$fishery)==0) {
   cy.HRJ = matrix(NA,nrow=1*(maxCY-minCY+1),ncol=ncol(x)+1)
   colnames(cy.HRJ) = c(names(x),"cy")
   cy.HRJ = as.data.frame(cy.HRJ)
   cy.HRJ$cy = sort(rep(minCY:maxCY,1))
  }
 #subset data to each cy and recombine
  for(i in minCY:maxCY) {
    tmp = subset(cy.HRJ,cy==i)
    age2.temp = subset(x.age2, cy==i)
    if(nrow(age2.temp)!=0) tmp[,names(age2.temp)] = age2.temp
    age3.temp = subset(x.age3, cy==i)
    if(nrow(age3.temp)!=0) tmp[names(age3.temp)] = age3.temp
    age4.temp = subset(x.age4, cy==i)
    if(nrow(age4.temp)!=0) tmp[names(age4.temp)] = age4.temp
    age5.temp = subset(x.age5, cy==i)
    if(nrow(age5.temp)!=0) tmp[names(age5.temp)] = age5.temp
    age6.temp = subset(x.age6, cy==i)
    if(nrow(age6.temp)!=0) tmp[names(age6.temp)] = age6.temp
    cy.HRJ[cy.HRJ$cy %in% i,] = tmp
  }
 #cleanup and rearrange the matrix
  cy.HRJ = cy.HRJ[,c(1,ncol(cy.HRJ),2:(ncol(cy.HRJ)-1))] 
  cy.HRJ = cy.HRJ[,-match("brood",names(cy.HRJ),nomatch=0)] #drop column brood, and put the column cy in its place
 #determine if it's a 4 age or 5 age HRJ file
  if(sum(colSums(apply(cy.HRJ, 2, is.na))==nrow(cy.HRJ))>=5) {
    #if it's a 4 age HRJ, it'll have all blanks across all years in a given age
    cy.HRJ$inc = rowSums(apply(cy.HRJ, 2, is.na))/5>1
  } else {
    #if it's a 5 age HRJ
    cy.HRJ$inc = rowSums(apply(cy.HRJ, 2, is.na))>0   
  }
 #return object
  cy.HRJ
}

###########################################
# convertHRJ_BYtoCY
#
# Description
# ------------------
# Manipulates an HRJ standardized R format object  from brood year data to calendar year data
# Determine if a brood is complete
#
# Dependent(s)
# ------------------
# .convertHRJ_BYtoCY_bytable - interal function that 
#
# Argument(s)
# ------------------
#
# Output(s)
# ------------------
# 
###########################################
convertHRJ_BYtoCY <- function(x) {
  if(x$HRJformat=="calendar") cat("ERROR: data is already in calendar year format\n")
  for(i in 1:x$nstocks) {
    #convert fishery data
    x[[i]]$HRJ_BY = .convertHRJ_BYtoCY_bytable(x[[i]]$HRJ_BY)
    x[[i]]$HRJ_CY = .convertHRJ_BYtoCY_bytable(x[[i]]$HRJ_CY)
    #convert escapement data
    x[[i]]$ESC_BY = .convertHRJ_BYtoCY_bytable(x[[i]]$ESC_BY)
    x[[i]]$ESC_CY = .convertHRJ_BYtoCY_bytable(x[[i]]$ESC_CY)
  }
  x$HRJformat="calendar"
  x
}

###########################################
# addPTableHRJ
#
# Description
# ------------------
# Adds the "preferred" table to any HRJ object. Use the 'BY' method data when the brood is incomplete, else use the 'CY" method
#
# Dependent(s)
# ------------------
# na
#
# Argument(s)
# ------------------
#
# Output(s)
# ------------------
# 
###########################################
addPTableHRJ <- function(x, hrjclass=c("R","Access")) {
  if(hrjclass=="R") {
    for(i in 1:x$nstocks) {
      cat("not implemented\n")
      stop()
      #determine if its an incomplete brood or not
      incBroods = rowSums(apply(x[[i]]$HRJ_BY, 2, is.na))/5>1 #NOTE!!!: assumes a 4 age HRJ file

      x[[i]]$HRJ_BY[inc.broods,]
      
      ifelse(inc.broods, x[[i]]$HRJ_BY, x[[i]]$HRJ_CY)
      
      #convert fishery data
      x[[i]]$HRJ_P = x[[i]]$HRJ_CY
      
      .convertHRJ_BYtoCY_bytable(x[[i]]$HRJ_BY)
      #convert escapement data (NOTE!: there's no difference between the two methods for escap data)
      x[[i]]$ESC_P = x[[i]]$ESC_CY 
      
    } 
  }
  if(hrjclass=="Access" && !is.null(x$HRJ_BY$inc) && x$HRJformat=="calendar") {
    #use the cy method if complete,
     tmp = x$HRJ_CY
    #for rows where the brood is not complete, use the by method
     tmp[tmp$inc,] <- x$HRJ_BY[tmp$inc,]
    #in the 'p' table, update the inc field to state where the data is from
     tmp$inc <- ifelse(tmp$inc,"by","cy")
     x$HRJ_P = tmp
  }
  if(hrjclass=="Access" && x$HRJformat=="brood") {
    #use the cy method if complete,
    tmp = x$HRJ_CY
    tmp$inc = rep(NA,nrow(tmp))
    #
    for(i in 1:x$nstocks) {
      hrj_cy_tmp = subset(x$HRJ_CY, stock==i)
      #determine if it's a 4 age or 5 age HRJ file
      if(sum(colSums(apply(hrj_cy_tmp, 2, is.na))==nrow(hrj_cy_tmp))>=5) {
        #if it's a 4 age HRJ, it'll have all blanks across all years in a given age
        hrj_cy_tmp$inc = rowSums(apply(hrj_cy_tmp, 2, is.na))/5>1
      } else {
        #if it's a 5 age HRJ
        hrj_cy_tmp$inc = rowSums(apply(hrj_cy_tmp, 2, is.na))>0   
      }
      #update the "inc" field in the tmp object
      tmp[rownames(hrj_cy_tmp),grep("inc",colnames(tmp))] = hrj_cy_tmp$inc
    }
    #
    tmp2 =  x$HRJ_BY
    tmp2$inc = tmp$inc
    #for rows where the brood is not complete, use the by method
    tmp[tmp$inc,] <- tmp2[tmp$inc,]
    #in the 'p' table, update the inc field to state where the data is from
    tmp$inc <- ifelse(tmp$inc,"by","cy")
    x$HRJ_P = tmp
  }
  x
}

###########################################
# convertHRJ_RtoAccess
#
# Description
# ------------------
# Convert a R HRJ data object format into the access database format (later will be a function) by
# collapsing the by stock HRJ's B & C method (ESC & HRJ) data into individual dataframes
#
# Argument(s)
# ------------------
#
#
# Output(s)
# ------------------
# 
###########################################
convertHRJ_RtoAccess <- function(x, writeCSV=FALSE, userDir=NULL) {
 #Confirm that the first item in the vector is B because program assumes the order of B and C
  if(!(x[[1]]$imMethod[1]=="B" && x[[1]]$imMethod[2]=="C")) {
   cat("ERROR: B and C method HRJ methods are out of order. They should be in B/C Method\n")
   print(x[[1]]$imMethod)
   stop()
  }
 #Create the base BY method table
  hrj_by <- x[[1]]$HRJ_BY
  esc_by <- x[[1]]$ESC_BY
  hrj_by$stock <- 1
  esc_by$stock <- 1
 #Create the base CY method table
  hrj_cy <- x[[1]]$HRJ_CY
  esc_cy <- x[[1]]$ESC_CY
  hrj_cy$stock <- 1
  esc_cy$stock <- 1
 #Loop through and read the next 
  for(i in 2:x$nstocks) {
   #BY
    hrj_tmp <- x[[i]]$HRJ_BY
    esc_tmp <- x[[i]]$ESC_BY
    hrj_tmp$stock <- i
    esc_tmp$stock <- i
    hrj_by <- rbind(hrj_by, hrj_tmp)
    esc_by <- rbind(esc_by, esc_tmp)
   #CY
    hrj_tmp <- x[[i]]$HRJ_CY
    esc_tmp <- x[[i]]$ESC_CY
    hrj_tmp$stock <- i
    esc_tmp$stock <- i
    hrj_cy <- rbind(hrj_cy, hrj_tmp)
    esc_cy <- rbind(esc_cy, esc_tmp)
  }
  hrj_by <- hrj_by[,c(ncol(hrj_by),1:(ncol(hrj_by)-1))]
  esc_by <- esc_by[,c(ncol(esc_by),1:(ncol(esc_by)-1))]
  hrj_cy <- hrj_cy[,c(ncol(hrj_cy),1:(ncol(hrj_cy)-1))]
  esc_cy <- esc_cy[,c(ncol(esc_cy),1:(ncol(esc_cy)-1))]
  hrj=list(
    HRJ_BY=hrj_by,
    HRJ_CY=hrj_cy,
    ESC_BY=esc_by,
    ESC_CY=esc_cy,
    stknames = x$stknames,
    fshnames = x$fshnames,
    nstocks = x$nstocks,
    nfisheries = x$nfisheries,
    HRJformat = x$HRJformat
  )
 #if write CSV
  if(writeCSV) {
   #ask user to specify the directory iif userDir is equal to ask, otherwise write files to the default directory
    if(userDir=="ask") setwd(choose.dir()) 
   #write directories
   write.table(hrj_by, "hrj_by - by layout.csv")
   write.table(hrj_cy, "hrj_cy - by layout.csv")
   write.table(esc_by, "esc_by - by layout.csv")
   write.table(esc_cy, "esc_cy - by layout.csv") 
  }
 #Return output
  return(hrj)
}

