###############################################################################
#                                                                      Feb 2019
#                         Query for Charles Yackulic 
#     
#  Notes:
#  * Sources 'Drift_Functions.R'
#
#  To Do:
#  * Lab processing of IVb samples
#  * Format data for CY:
#    - 1 row per sample, cols for biomass of big 5
#                                                                     
###############################################################################
library(dplyr)
library(tidyr)
library(foodbase)
library(here)

source(paste0(getwd(), "/Drift_Functions.R"))

# d.tmp = readDB(gear = "Drift", type = "Sample", updater = TRUE)
d.tmp = readDB(gear = "Drift", type = "Sample", updater = FALSE)

d.1 = d.tmp[d.tmp$GearID == 4,]
#--------------------------------------
# older data during NO 
# (for some reason, GC20150624 is the original tripID I had, but this no longer
# matches the drift data, instead changed to GC20150623, which matches)
no.trips = c('GC20120419','GC20120705','GC20120913','GC20130110',
             'GC20130404','GC20130625','GC20130912','GC20140109',
             'GC20140403','GC20140626','GC20140911','GC20150108',
             'GC20150403','GC20150623','GC20150911','GC20160108',
             'GC20160331','GC20160701','GC20160909')

d.1$Site = add.no.site(d.1)

sub.1 = d.1[which(d.1$TripID %in% no.trips),]

# sub.2 = sub.1[which(sub.1$site %in% c("I", "II", "III", "IVa", "IVb")),]
sub.2 = sub.1[which(sub.1$Site %in% c("I", "IVb")),]

sub.3 = sub.2[which(sub.2$DepthIntegrated == 0),]

sub.4 = sub.3[which(sub.3$TimeDay == "Day"),]

#--------------------------------------
# After NO

# email from Jeff: JCM trips (TripIDs GC20170425, GC20170621, GC20170926, GC20180511, GC20180615, GC20181006)

tmp = d.1[which(d.1$Date >= "2017-01-01"),]

tmp.2 = tmp[which(tmp$Site %in% c("I", "IVb")),]

# without the tripID subset, you get all of the Ferry -- cut down to only around -3.5
tmp.3 = tmp.2[which(tmp.2$RiverMile %in% c(-3.5, -3.1, 63.2, 63.5, 63.6, 64.1, 64.4)),]
table(tmp.3$RiverMile)  

#-----------------------------------------------------------------------------#
# get all of the samples the were subset above
all = rbind(sub.4, tmp.3)

dat = sampspec(samp = all, species = "Big9")

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
             "Site")

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

write.csv(drift, "CY_Drift_Biomass_2019_02_07.csv", row.names = F)
#-----------------------------------------------------------------------------#
# End
