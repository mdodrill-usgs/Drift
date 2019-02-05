###############################################################################
#                                                                      Feb 2019 
#                          Drift at Lees Ferry for Korman
#
# Notes: 
# * See previous version: U:\Desktop\FB_DOWN\Analysis\DRIFT_DOWNSTREAM\Korman
#  'Get_Drift_New_v1'
#  
###############################################################################
# library(devtools)
# install_github(repo = 'jmuehlbauer-usgs/R-packages', subdir = 'foodbase')
library(dplyr)
library(foodbase)

d.tmp = readDB(gear = "Drift", type = "Sample", updater = FALSE)

# d.1 = d.tmp[d.tmp$RiverMile == 0,]
d.1 = d.tmp[d.tmp$Reach == "CRLeesFerry",]

d.2 = d.1[d.1$Date >= "2012-01-01",]

d.3 = d.2[d.2$GearID == 4,]

#-----------------------------------------------------------------------------#
dat = sampspec(samp = d.2, species = "Big9")

# cut down some of the columns in samples
keep.col = c("BarcodeID",
             "TripID",
             "Region",
             "Reach",
             "Date",
             "SampleNumber",
             "RiverMile",
             "DepthTotal",
             "DepthSample",
             "DepthIntegrated",
             "GearID",
             "TimeDay", 
             "TimeBegin", 
             "TimeElapsed",
             "Volume",
             "Notes")
ltl.samps = dat$Samples[,names(dat$Samples) %in% keep.col]

# get total counts
dat$Specimens$CountTotal = rowSums(dat$Specimens[,3:23])
ltl.specs = dat$Specimens[,c(1,2,24)]

dat2 = left_join(ltl.samps, ltl.specs, by = "BarcodeID")

# get total mass
dat$Biomass$MassTotal = rowSums(dat$Biomass[,3:23])

ltl.mass = dat$Biomass[,c(1,2,24)]

dat3 = left_join(dat2, ltl.mass, by = c("BarcodeID", "SpeciesID"))

#-----------------------------------------------------------------------------#