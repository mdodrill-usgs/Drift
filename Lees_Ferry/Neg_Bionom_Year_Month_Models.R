###############################################################################
#                                                                      Feb 2019
#
#   Fit count models (neg.binom) to the drift data to look
#   at yearly and monthly effects
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
# library(devtools)
# install_github(repo = 'jmuehlbauer-usgs/R-packages', subdir = 'foodbase')

library(foodbase)
library(dplyr)
library(ggplot2)
library(ggthemes)
# library(xts)     # align.time (round to 15min) library(zoo)
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
lf.dat$Month = as.factor(substr(lf.dat$Date, 6, 7))
# lf.dat$Year = substr(lf.dat$Date, 1, 4)
# lf.dat$Month = substr(lf.dat$Date, 6, 7)

# make jan 2008 --> 2007 (kinda fucked up)
# idx = which(substr(lf.dat$Date,1,7) == "2008-01")  # These samples are not showing up with the new DB (b/c)
# idx2 = which(substr(lf.dat$Date,1,10) == "2008-03-03")  # this gets dropped below...

# data4[idx,which(names(data4) == "year")] = "2007"
# lf.dat[idx2, which(names(lf.dat) == "Year")] = "2007"
# data3 = data4

# lf.dat$Year = as.factor(lf.dat$Year)
# lf.dat$Month = as.factor(lf.dat$Month)

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
# could clean up all the temp. data formatting objects here...
# rm(list = setdiff(ls(), "dat.in"))
#-----------------------------------------------------------------------------#
# run some models for each taxa looking at year effects
models = NULL  # variable to save model fits 
taxa = unique(dat.in$SpeciesID)
big.out = NULL

for(i in 1:length(taxa)){
  
  taxa.sub = filter(dat.in, SpeciesID == taxa[i])
  
  start.time <- Sys.time()  # start timer
  fm_1 <- glmmTMB(CountTotal ~ 0 + Year + (1|Month) + (1|RiverMile) + offset(log(Volume)),
                  # fm_1 <- glmmTMB(count.sum ~ 0 + year + (1|month)  + offset(log(Volume)),
                  data = taxa.sub,
                  family = nbinom2)
  
  end.time = Sys.time()
  time.taken = end.time - start.time
  print(round(time.taken, 2))
  
  tmp.len = length(fixef(fm_1)[[1]])
  
  tmp.out = data.frame(matrix(NA, tmp.len, length(unique(dat.in$SpeciesID))))
  tmp.out[,1] = rep(taxa[i], tmp.len)
  
  tmp.out[,2] = sort(as.numeric(paste(unique(dat.in$Year))))
  
  tmp.out[,3] = exp(fixef(fm_1)[[1]])
  tmp.out[,4] = exp(confint(fm_1)[1:tmp.len,1])
  tmp.out[,5] = exp(confint(fm_1)[1:tmp.len,2]) 
  
  models[[i]] = fm_1
  
  big.out[[i]] = tmp.out
}

model.counts = do.call(rbind, big.out)
colnames(model.counts) = list("taxa", "year", "est", "ll", "ul")
#-----------------------------------------------------------------------------#
# make some plots of the year effect from the fitting above 
windows(width = 10, height = 10, record = TRUE)

name.key = data.frame(n1 = c("CHIL", "GAMM", "NZMS", "SIML", "OLIG"),
                      n2 = c("Midges", "Gammarus", "New Zealand Mud Snails",
                             "Black Flies", "Worms"))

model.counts$name = name.key[match(model.counts[,1], name.key[,1]),2]

model.counts$year2 = as.character(model.counts$year)

p = ggplot(model.counts, aes(x = year2, y = est)) +
  geom_point() +
  geom_errorbar(aes(ymax = ul, ymin = ll), width = 0) +
  facet_wrap(~ name, scales = "free_y") + 
  scale_x_discrete(labels = paste0("'", substr(as.character(seq(2008,2018,1)),3,4)),
                     breaks = c(2008:2018)) +
  labs(title = "Long-Term Drift Monitoring",
       y = expression(paste('Count / m'^' 3')), x = "Year")  

G = p + theme(axis.title.x = element_text(size = 14, vjust = -.1),
              axis.title.y = element_text(size = 14, vjust = 1),
              axis.text.x = element_text(size = 12, colour = "black"),
              axis.text.y = element_text(size = 12, colour = "black"),
              title = element_text(size = 16),
              panel.background = element_rect(fill = "white"),
              panel.grid.minor = element_line(colour = "white"),
              panel.grid.major = element_line(colour = "white"),
              panel.border = element_rect(colour = "black", fill = NA),
              # panel.spacing = unit(c(1,1,1,1), "lines"),
              strip.background = element_blank(),
              strip.text = element_text(size = 14, vjust = 1),
              legend.text = element_text(size = 12),
              legend.title = element_text(size = 12),
              legend.title.align = .5)
G

#-----------------------------
# only 4 taxa on the plot
sub.model.counts = model.counts[which(model.counts$name != "Worms"),]

p = ggplot(sub.model.counts, aes(x = year2, y = est)) +
  geom_point() +
  geom_errorbar(aes(ymax = ul, ymin = ll), width = 0) +
  facet_wrap(~ name, scales = "free_y") + 
  scale_x_discrete(labels = paste0("'", substr(as.character(seq(2008,2018,1)),3,4)),
                     breaks = c(2008:2018)) +
  labs(title = "Long-Term Drift Monitoring",
       y = expression(paste('Count / m'^' 3')), x = "Year")  

G = p + theme(axis.title.x = element_text(size = 14, vjust = -.1),
              axis.title.y = element_text(size = 14, vjust = 1),
              axis.text.x = element_text(size = 12, colour = "black"),
              axis.text.y = element_text(size = 12, colour = "black"),
              title = element_text(size = 16),
              panel.background = element_rect(fill = "white"),
              panel.grid.minor = element_line(colour = "white"),
              panel.grid.major = element_line(colour = "white"),
              panel.border = element_rect(colour = "black", fill = NA),
              # panel.spacing = unit(c(1,1,1,1), "lines"),
              strip.background = element_blank(),
              strip.text = element_text(size = 14, vjust = 1),
              legend.text = element_text(size = 12),
              legend.title = element_text(size = 12),
              legend.title.align = .5)
G

#-----------------------------
# only 4 taxa on the plot, show data...and some summaries
sub.model.counts = model.counts[which(model.counts$name != "Worms"),]

dat.in$name = name.key[match(dat.in$SpeciesID, name.key[,1]),2]
sub.dat.in = dat.in[which(dat.in$name != "Worms"),]

u.name = unique(sub.model.counts$name)

lims = list(c(0,5), c(0,.5), c(0,25), c(0,.3))

for(i in 1:4){
  sub.sub = sub.dat.in[which(sub.dat.in$name == u.name[i]),]
  sub.mod = sub.model.counts[which(sub.model.counts$name == u.name[i]),]
  
  sub.mu = group_by(sub.sub, Year) %>%
           summarize(mean = mean(CountTotal / Volume),
                     median = median(CountTotal / Volume))
  
  p = ggplot(sub.mod, aes(x = year2, y = est)) +
    geom_point() +
    geom_errorbar(aes(ymax = ul, ymin = ll), width = 0) +
    geom_jitter(data = sub.sub, aes(x = Year, y = CountTotal/Volume),
                shape = 1, color = "gray50", alpha = .5) +
    geom_point() +
    geom_errorbar(aes(ymax = ul, ymin = ll), width = 0) +
    scale_x_discrete(labels = paste0("'", substr(as.character(seq(2008,2018,1)),3,4)),
                     breaks = c(2008:2018)) +
    geom_point(data = sub.mu, aes(x = Year, y = mean), color = "red") +
    geom_point(data = sub.mu, aes(x = Year, y = median), color = "blue") +
    # geom_boxplot(data = sub.sub, aes(x = Year, y = CountTotal/Volume), fill = NA, outlier.alpha = 0) +
    # geom_violin(data = sub.sub, aes(x = Year, y = CountTotal/Volume)) +
    
    labs(title = paste("Long-Term Drift Monitoring - ", u.name[i], sep = " "),
         y = expression(paste('Count / m'^' 3')), x = "Year") +
    ylim(lims[[i]])
  
  G = p + theme(axis.title.x = element_text(size = 14, vjust = -.1),
                axis.title.y = element_text(size = 14, vjust = 1),
                axis.text.x = element_text(size = 12, colour = "black"),
                axis.text.y = element_text(size = 12, colour = "black"),
                title = element_text(size = 16),
                panel.background = element_rect(fill = "white"),
                panel.grid.minor = element_line(colour = "white"),
                panel.grid.major = element_line(colour = "white"),
                panel.border = element_rect(colour = "black", fill = NA),
                # panel.spacing = unit(c(1,1,1,1), "lines"),
                strip.background = element_blank(),
                strip.text = element_text(size = 14, vjust = 1),
                legend.text = element_text(size = 12),
                legend.title = element_text(size = 12),
                legend.title.align = .5)
  print(G)
}


#-----------------------------------------------------------------------------#
# run some models for each taxa looking at month effects
models.2 = NULL  # variable to save model fits 
big.out.2 = NULL


for(i in 1:length(taxa)){
  
  taxa.sub = filter(dat.in, SpeciesID == taxa[i])
  
  start.time <- Sys.time()  # start timer
  fm_1 <- glmmTMB(CountTotal ~ 0 + (1|Year) + Month + (1|RiverMile) + offset(log(Volume)),
                  data = taxa.sub,
                  family = nbinom2)
  
  end.time = Sys.time()
  time.taken = end.time - start.time
  print(round(time.taken,2))
  
  tmp.len = length(fixef(fm_1)[[1]])
  
  tmp.out = data.frame(matrix(NA, tmp.len, length(unique(dat.in$SpeciesID))))
  tmp.out[,1] = rep(taxa[i], tmp.len)
  
  tmp.out[,2] = sort(as.numeric(paste(unique(dat.in$Month))))
  
  tmp.out[,3] = exp(fixef(fm_1)[[1]])
  tmp.out[,4] = exp(confint(fm_1)[1:tmp.len,1])
  tmp.out[,5] = exp(confint(fm_1)[1:tmp.len,2]) 
  
  models.2[[i]] = fm_1
  
  big.out.2[[i]] = tmp.out
}

model.counts.2 = do.call(rbind, big.out.2)
colnames(model.counts.2) = list("taxa", "month", "est", "ll", "ul")
#-----------------------------------------------------------------------------#
# make some month plots 
windows(width = 10, height = 10, record = TRUE)

model.counts.2$name = name.key[match(model.counts.2[,1], name.key[,1]),2]


p2 = ggplot(model.counts.2, aes(x = month, y = est)) +
  geom_point() +
  geom_errorbar(aes(ymax = ul, ymin = ll)) + 
  scale_x_continuous(breaks = c(1:12), labels = as.character(1:12)) +
  facet_wrap(~ name, scales = "free_y") + 
  labs(title = "Long-Term Drift Monitoring",
       y = expression(paste('Count / m'^' 3')), x = "Month")  #
# p + theme_base()

G2 = p2 + theme(axis.title.x = element_text(size = 14, vjust = -.1),
              axis.title.y = element_text(size = 14, vjust = 1),
              axis.text.x = element_text(size = 12, colour = "black"),
              axis.text.y = element_text(size = 12, colour = "black"),
              title = element_text(size = 16),
              panel.background = element_rect(fill = "white"),
              panel.grid.minor = element_line(colour = "white"),
              panel.grid.major = element_line(colour = "white"),
              panel.border = element_rect(colour = "black", fill = NA),
              # panel.spacing = unit(c(1,1,1,1), "lines"),
              strip.background = element_blank(),
              strip.text = element_text(size = 14, vjust = 1),
              legend.text = element_text(size = 12),
              legend.title = element_text(size = 12),
              legend.title.align = .5)
G2

#--------------------------------------
# Only 4 taxa 
sub.model.counts.2 = model.counts.2[which(model.counts.2$name != "Worms"),]

p2 = ggplot(sub.model.counts.2, aes(x = month, y = est)) +
  geom_point() +
  geom_errorbar(aes(ymax = ul, ymin = ll), width = 0) + 
  scale_x_continuous(breaks = c(1:12), labels = as.character(1:12)) +
  facet_wrap(~ name, scales = "free_y") + 
  labs(title = "Long-Term Drift Monitoring",
       y = expression(paste('Count / m'^' 3')), x = "Month")  #
# p + theme_base()

G2 = p2 + theme(axis.title.x = element_text(size = 14, vjust = -.1),
              axis.title.y = element_text(size = 14, vjust = 1),
              axis.text.x = element_text(size = 12, colour = "black"),
              axis.text.y = element_text(size = 12, colour = "black"),
              title = element_text(size = 16),
              panel.background = element_rect(fill = "white"),
              panel.grid.minor = element_line(colour = "white"),
              panel.grid.major = element_line(colour = "white"),
              panel.border = element_rect(colour = "black", fill = NA),
              # panel.spacing = unit(c(1,1,1,1), "lines"),
              strip.background = element_blank(),
              strip.text = element_text(size = 14, vjust = 1),
              legend.text = element_text(size = 12),
              legend.title = element_text(size = 12),
              legend.title.align = .5)
G2


#-----------------------------------------------------------------------------#
# End
#-----------------------------------------------------------------------------#