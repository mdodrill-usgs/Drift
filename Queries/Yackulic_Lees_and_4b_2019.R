###############################################################################
#                                                                      Feb 2019
#                         Query for Charles Yackulic 
#     
#  Notes:
#  * 
#
#  To Do:
#  * Lab processing of IVb samples
#  * Format data for CY:
#    - 1 row per sample, cols for biomass of big 5
#                                                                     
###############################################################################
library(dplyr)
library(foodbase)

d.tmp = readDB(gear = "Drift", type = "Sample", updater = TRUE)

d.1 = d.tmp[d.tmp$GearID == 4,]

no.trips = c('GC20120419','GC20120705','GC20120913','GC20130110',
             'GC20130404','GC20130625','GC20130912','GC20140109',
             'GC20140403','GC20140626','GC20140911','GC20150108',
             'GC20150403','GC20150624','GC20150911','GC20160108',
             'GC20160331','GC20160701','GC20160909')



d.1$site = add.no.site(d.1)

# sub = d.1[which(d.1$site == "IVb"),]
sub = d.1[which(d.1$site == "I"),]

sort(unique(sub$TripID))

test = c("GC20170425", "GC20170428", "GC20170621", "GC20170915",
         "GC20180411", "GC20180511", "GC20180615", "GC20180915", "GC20181006")

sub.2 = sub[which(sub$TripID %in% test),]


write.csv(sub.2, "CY_Query_Samples_Feb_19_IVb.csv")
#-------------------------------------
# looking at what's been processed for site I
sub$month = substr(sub$Date, 6,7)
sub$year = substr(sub$Date, 1, 4)

sub = d.1[which(d.1$site == "I"),]
# sub.2 = sub[which(sub$Date > "2016-01-01"),]
# sub.3 = sub.2[which(sub.2$RiverMile %in% c(0, -3.5, -3.4, -3.1)),]
sub.3 = sub[which(sub$RiverMile %in% c(0, -3.5, -3.4, -3.1)),]

sub.4 = sub.3[which(!is.na(sub.3$ProcessDate)),]


table(sub.4$month, sub.4$year)

write.csv(sub.3, "CY_Query_Samples_Feb_19_I.csv")

#-----------------------------------------------------------------------------#
# dat = sampspec(samp = sub.2, species = "Big9")
# dat = sampspec(samp = d.1, species = "Big9")
# 
# # cut down some of the columns in samples
# keep.col = c("BarcodeID",
#              "TripID",
#              "Region",
#              "Reach",
#              "Date",
#              "SampleNumber",
#              "RiverMile",
#              "DepthTotal",
#              "DepthSample",
#              "DepthIntegrated",
#              "GearID",
#              "TimeDay", 
#              "TimeBegin", 
#              "TimeElapsed",
#              "Volume",
#              "Notes")
# 
# ltl.samps = dat$Samples[,names(dat$Samples) %in% keep.col]
# 
# # get total counts
# dat$Specimens$CountTotal = rowSums(dat$Specimens[,3:23])
# ltl.specs = dat$Specimens[,c(1,2,24)]
# 
# dat2 = left_join(ltl.samps, ltl.specs, by = "BarcodeID")
# 
# # get total mass
# dat$Biomass$MassTotal = rowSums(dat$Biomass[,3:23])
# 
# ltl.mass = dat$Biomass[,c(1,2,24)]
# 
# dat3 = left_join(dat2, ltl.mass, by = c("BarcodeID", "SpeciesID"))

#-----------------------------------------------------------------------------#
