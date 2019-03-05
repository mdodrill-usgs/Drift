###############################################################################
#                                                                    March 2019 
#                 Gammarus Day vs Night for Korman
#
# Notes: 
# * 
#  
###############################################################################
# library(devtools)
# install_github(repo = 'jmuehlbauer-usgs/R-packages', subdir = 'foodbase')
library(dplyr)
library(foodbase)

d.tmp = readDB(gear = "Drift", type = "Sample", updater = FALSE)

# d.1 = d.tmp[d.tmp$RiverMile == 0,]
# d.1 = d.tmp[d.tmp$Reach == "CRLeesFerry",]

d.2 = d.tmp[d.tmp$Date >= "2012-01-01",]

d.3 = d.2[d.2$GearID == 4,]

# only the Ferry and around -3.5 mile
# d.4 = d.3[d.3$RiverMile %in% c(-3.5, -3.4, -0.2, 0),]

#-----------------------------------------------------------------------------#
dat = sampspec(samp = d.3, species = "Big9")

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
# some quick plots
library(ggplot2)


dat3$mass.per.vol = dat3$MassTotal / dat3$Volume

dat3$count.per.vol = dat3$CountTotal / dat3$Volume


dat3$Year = substr(dat3$Date, 1, 4)
dat3$Month = substr(dat3$Date, 6, 7)

sub = dat3[which(dat3$TimeDay == "Night"),]

key = unique(paste(sub$Year, sub$Month))


dat.in = dat3[which(paste(dat3$Year, dat3$Month) %in% key),]


sub.in = dat.in[dat.in$SpeciesID == "GAMM",]


p = ggplot(sub.in, aes(x = Month, y = mass.per.vol)) +
  geom_boxplot(aes(fill = TimeDay), alpha = .75) +
  labs(title = " Combined NO sites\n Only Gammarus\n Only Year/Month combos with night samples",
       y = "Mass / Vol. (mg/m^3)") +
  facet_wrap(~ Year, scales = "free") 
p

g = p + theme(axis.title.x = element_text(size = 16),
               axis.title.y = element_text(size = 16, vjust = 3),
               axis.text.x = element_text(size = 14, colour = "black"),
               axis.text.y = element_text(size = 14, colour = "black"),
               strip.text = element_text(size = 14),
               strip.background = element_blank(),
               plot.title = element_text(size = 16, hjust = 0),
               panel.background = element_rect(fill = "white"),
               panel.border = element_rect(color = "black", fill = NA),
               panel.grid.minor = element_line(colour = "white"),
               panel.grid.major = element_line(colour = "white"),
               legend.position = c(.06,.8))
g


p2 = ggplot(sub.in, aes(x = Month, y = count.per.vol)) +
  geom_boxplot(aes(fill = TimeDay), alpha = .75) +
  labs(title = "",
       y = "Count / Vol. (#/m^3)") +
  facet_wrap(~ Year, scales = "free") 
p2

g2 = p2 + theme(axis.title.x = element_text(size = 16),
               axis.title.y = element_text(size = 16, vjust = 3),
               axis.text.x = element_text(size = 14, colour = "black"),
               axis.text.y = element_text(size = 14, colour = "black"),
               strip.text = element_text(size = 14),
               strip.background = element_blank(),
               plot.title = element_text(size = 16, hjust = 0),
               panel.background = element_rect(fill = "white"),
               panel.border = element_rect(color = "black", fill = NA),
               panel.grid.minor = element_line(colour = "white"),
               panel.grid.major = element_line(colour = "white"),
               legend.position = c(.06,.8))
g2

library(gridExtra)

grid.arrange(g,g2,ncol = 1)


#-----------------------------------------------------------------------------#
# End