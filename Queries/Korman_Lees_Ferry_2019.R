###############################################################################
#                                                                      Feb 2019 
#                          Drift at Lees Ferry for Korman
#
# Notes: 
# * See previous version: U:\Desktop\FB_DOWN\Analysis\DRIFT_DOWNSTREAM\Korman
#  'Get_Drift_New_v1'
# * Last updated on 5/22/2019
#  
###############################################################################
# library(devtools)
# install_github(repo = 'jmuehlbauer-usgs/R-packages', subdir = 'foodbase')
library(dplyr)
library(foodbase)

d.tmp = readDB(gear = "Drift", type = "Sample", updater = TRUE)

# d.1 = d.tmp[d.tmp$RiverMile == 0,]
d.1 = d.tmp[d.tmp$Reach == "CRLeesFerry",]

d.2 = d.1[d.1$Date >= "2012-01-01",]

d.3 = d.2[d.2$GearID == 4,]

# only the Ferry and around -3.5 mile
d.4 = d.3[d.3$RiverMile %in% c(-3.5, -3.4, -0.2, 0),]

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

# get total counts
dat$Specimens$CountTotal = rowSums(dat$Specimens[,3:23])
ltl.specs = dat$Specimens[,c(1,2,24)]

dat2 = left_join(ltl.samps, ltl.specs, by = "BarcodeID")

# get total mass
dat$Biomass$MassTotal = rowSums(dat$Biomass[,3:23])

ltl.mass = dat$Biomass[,c(1,2,24)]

dat3 = left_join(dat2, ltl.mass, by = c("BarcodeID", "SpeciesID"))

# write.csv(dat3, "JK_Drift_Lees_Count_and_Biomass_2019_05_22.csv", row.names = F)
#-----------------------------------------------------------------------------#
# some quick plots
library(ggplot2)


dat3$mass.per.vol = dat3$MassTotal / dat3$Volume

# dat4 = group_by(dat3, BarcodeID) %>%
#        summarise(tot.mass.per.vol = sum(mass.per.vol))

dat4 = filter(dat3, !SpeciesID %in% c("NZMS", "OLIG")) %>%
       group_by(BarcodeID) %>%
       summarise(tot.mass.per.vol = sum(mass.per.vol))
       

dat.in = left_join(ltl.samps, dat4, by = "BarcodeID")

dat.in$Year = substr(dat.in$Date, 1, 4)
dat.in$Month = substr(dat.in$Date, 6, 7)

dat.in$Place = ifelse(dat.in$RiverMile %in% c(-3.5, -3.4), "NO", "Lees")

table(dat.in$Year, dat.in$TimeDay)
dat.in.2 = dat.in[dat.in$TimeDay == "Day",]

# table(dat.in.2$Year, dat.in.2$DepthIntegrated)
# dat.in.3 = dat.in.2[dat.in.2$DepthIntegrated == 0,]


tmp = group_by(dat.in.2, Year, Month) %>%
  summarise(n.samps = n())


# p = ggplot(dat.in[dat.in$Place == "NO",], aes(x = Month, y = tot.mass.per.vol)) +
# p = ggplot(dat.in[dat.in$Place == "Lees",], aes(x = Month, y = tot.mass.per.vol)) +
p = ggplot(dat.in.2, aes(x = Month, y = tot.mass.per.vol)) +
    geom_boxplot() +
    coord_cartesian(ylim = c(0, 2)) +
    geom_text(data = tmp, aes(x = Month, y = 2, label = n.samps)) +
    labs(title = " Lees (~ 0 RM) and NO site (~ -3.5 RM)\n Only Daytime Samples\n Only Black Flies, Midges, Gammarus\n Mix of Depth Int and Fixed",
         y = "Mass / Vol. (mg/m^3)") +
    facet_wrap(~ Year) 
p

g2 = p + theme(axis.title.x = element_text(size = 16),
                axis.title.y = element_text(size = 16, vjust = 3),
                axis.text.x = element_text(size = 14, colour = "black"),
                axis.text.y = element_text(size = 14, colour = "black"),
                strip.text = element_text(size = 14),
                strip.background = element_blank(),
                plot.title = element_text(size = 16, hjust = 0),
                panel.background = element_rect(fill = "white"),
                panel.border = element_rect(color = "black", fill = NA),
                panel.grid.minor = element_line(colour = "white"),
                panel.grid.major = element_line(colour = "white"))
g2




#---------------------------------------------

p2 = ggplot(dat.in, aes(x = as.factor(DepthIntegrated), y = tot.mass.per.vol))+
     geom_boxplot()
p2


p2 = ggplot(dat.in, aes(x = TimeDay, y = tot.mass.per.vol))+
     geom_boxplot()
p2

p2 = ggplot(dat.in, aes(x = Place, y = tot.mass.per.vol))+
     geom_boxplot()
p2



#-----------------------------------------------------------------------------#
# End