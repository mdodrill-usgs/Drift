###############################################################################
#                                                                    March 2019 
#                          Drift at Lees Ferry for Charles
#
# Notes: 
# * See previous version: U:\Desktop\FB_Git\Drift\Queries\Korman_Lees_Ferry_2019
# * Also stole some from 'Yackulic_Lees_and _4b_2019'
#  
###############################################################################
# library(devtools)
# install_github(repo = 'jmuehlbauer-usgs/R-packages', subdir = 'foodbase')
library(dplyr)
library(foodbase)

# d.tmp = readDB(gear = "Drift", type = "Sample", updater = TRUE)
d.tmp = readDB(gear = "Drift", type = "Sample", updater = FALSE)

# d.1 = d.tmp[d.tmp$RiverMile == 0,]
d.1 = d.tmp[d.tmp$Reach == "CRLeesFerry",]

d.2 = d.1[d.1$Date >= "2012-01-01",]

d.3 = d.2[d.2$GearID == 4,]

# only the Ferry 
d.4 = d.3[d.3$RiverMile %in% c(-0.2, 0),]

#-----------------------------------------------------------------------------#
dat = sampspec(samp = d.4, species = "Big9")

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

ltl.samps$Year = substr(ltl.samps$Date, 1,4)
ltl.samps$Month = substr(ltl.samps$Date, 6, 7)

#--------------------------------------
# format the biomass

bio = dat$Biomass

# collapse black flies and midges
bio.2 = collapse.taxa(bio)

bio.2$mass.tot = rowSums(bio.2[,3:23])

bio.ltl = bio.2[,c(1:2,24)]

bio.ltl.2 = spread(bio.ltl, key = SpeciesID, value = mass.tot)

#--------------------------------------
# combine with the sample data

drift = left_join(ltl.samps, bio.ltl.2, by = "BarcodeID")

# write.csv(drift, "CY_Drift_Lees_Biomass_2019_03_11.csv", row.names = F)
#-----------------------------------------------------------------------------#
# End