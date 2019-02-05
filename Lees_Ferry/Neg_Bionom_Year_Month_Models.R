###############################################################################
#                                                                      Feb 2019
#
#   Fit count models (neg.binom) to the drift data to look
#   at yearly and monthlyeffects
#     
#  Notes:
#  * Commented out is code for changing the sample dates (:/) & adding in
#  dischange data
#
#  To Do:
#  * 
#                                                                     
###############################################################################
rm(list = ls(all = TRUE))
library(foodbase)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(xts)     # align.time (round to 15min) library(zoo)
library(glmmTMB)
library(here) # sets up relative paths

#-----------------------------------------------------------------------------#
all.dat = readDB()

all.dat.2 = all.dat[which(all.dat$Reach == "CRLeesFerry"),]

all.dat.3 = all.dat.2[all.dat.2$GearID == 4,]

dat = sampspec(samp = all.dat.3, species = "Big9")

#--------------------------------------
# cut down the sample table
keep.col = c("BarcodeID",
             "TripID",
             # "Region",
             # "Reach",
             "Date",
             "SampleNumber",
             "RiverMile",
             "DepthTotal",
             "DepthSample",
             "DepthIntegrated",
             # "GearID",
             "TimeDay", 
             "TimeBegin", 
             "TimeElapsed",
             "Volume")
             # "Notes")


ltl.samp = dat$Samples[,which(names(dat$Samples) %in% keep.col)] 

#--------------------------------------
# only the total counts

dat$Specimens$CountTotal = rowSums(dat$Specimens[,3:23])
ltl.specs = dat$Specimens[,c(1,2,24)]

#--------------------------------------
# merge the sample table with the specimen counts

lf.dat = left_join(ltl.samp, ltl.specs, by = "BarcodeID")

lf.dat$SpeciesID = as.character(lf.dat$SpeciesID)

#--------------------------------------
# Group the life stages of midges and blackflies together
tmp.ID = ifelse(lf.dat$SpeciesID == "CHIA", "CHIL", lf.dat$SpeciesID)
tmp.ID = ifelse(tmp.ID == "CHIP", "CHIL", tmp.ID)

tmp.ID = ifelse(tmp.ID == "SIMA", "SIML", tmp.ID)
tmp.ID = ifelse(tmp.ID == "SIMP", "SIML", tmp.ID)
unique(tmp.ID)

lf.dat$SpeciesID = tmp.ID

#--------------------------------------
# add in some other variables
lf.dat$Year = as.factor(substr(lf.dat$Date, 1, 4))
lf.dat$month = as.factor(substr(lf.dat$Date, 6,7))

# make jan 2008 --> 2007 
# idx = which(substr(data4$Date,1,7) == "2008-01") 
# idx2 = which(substr(data4$Date,1,10) == "2008-03-03")
# data4$year = as.numeric(substr(data4$Date,1,4))
# data4[idx,which(names(data4) == "year")] = "2007"
# data4[idx2,which(names(data4) == "year")] = "2007"
# data3 = data4

#-----------------------------------------------------------------------------#
# adding in the discharge data 
# lf.q = read.csv('LFQ_2007_2016.csv', header = T)  

# round the drift sampling time to 15 mins 
# time = as.POSIXct(strptime(as.character(counts.data2$TimeBegin),"%H:%M"))  
# tmp.time = substr(align.time(time, n = 60 * 15), 11, 19) # n is in seconds (rounds up)
# counts.data2$date.time = as.POSIXct(paste(counts.data2$Date, tmp.time), format = "%Y-%m-%d %H:%M:%S")

# format the discharge time column to match the bug data
# lf.q$time = as.POSIXct(strptime(as.character(lf.q$time),"%m/%d/%Y  %H:%M"))
# lf.q$year<-substr(lf.q$time,1,4)

# add a new col. with the coresponding discharges.
# counts.data2$q = lf.q[match(counts.data2$date.time, lf.q$time),2]
# counts.data2$q = counts.data2$q * .028317

# counts.data3 = counts.data2[which(!is.na(counts.data2$q)),]
# counts.data4 = filter(counts.data3, Volume_Sampled_M.3>10 & Velocity_m.s>0.1)

# maybe do with dat[[1]]$FlagStrange ? 
# counts.data4 = counts.data2

#-----------------------------------------------------------------------------#
# cut out some of the flood data and some river miles
lf.dat$flood = ifelse(substr(lf.dat$Date, 1, 7) == "2008-03" | substr(lf.dat$Date, 1, 7) == "2012-11"
                            | substr(lf.dat$Date, 1, 7) == "2013-11" , 1, 0)
lf.dat.2 <- filter(lf.dat, flood == 0)

lf.dat.3 <- filter(lf.dat.2, RiverMile == -11.5 | RiverMile == -11 | RiverMile == -8 |
                       RiverMile == -4.9 | RiverMile == -3.5 | RiverMile == -3.4|
                       RiverMile == -2.1| RiverMile == -0.2 | RiverMile == 0)


dat.in = lf.dat.3
# could clean up all the temp data formatting objects here...
# rm(list = setdiff(ls(), "dat.in"))
#-----------------------------------------------------------------------------#