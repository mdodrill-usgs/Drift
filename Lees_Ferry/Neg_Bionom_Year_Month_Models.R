###############################################################################
#                                                                      Feb 2019
#
#   Fit count models (neg.binom) to the drift data to look
#   at yearly and monthlyeffects
#     
#  Notes:
#  * 
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
library(here)

#-----------------------------------------------------------------------------#