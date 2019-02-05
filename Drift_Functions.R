###############################################################################
#                                                                      Feb 2019
#                   Extra functions used for drift
#     
#  Notes:
#  * 
#
#  To Do:
#  * 
#                                                                     
###############################################################################

# given a data.frame with 'RiverMile', returns the NO site
add.no.site = function(dat){
  # these are based roughly on the excel file "Jan2016_Site Location List for Boatman.xlsx"
  
  rm.col = dat[,which(names(dat) == "RiverMile")]
  
  out = ifelse(rm.col <= 0, 'I',
               ifelse(rm.col >= 17.22 & rm.col <= 20.58, 'II', 
                      ifelse(rm.col >= 37.57 & rm.col <= 42.11, 'III',
                             ifelse(rm.col > 59 & rm.col < 62.5, 'IVa',
                                    ifelse(rm.col > 63 & rm.col < 65.6, 'IVb',
                                           ifelse(rm.col > 65.6, 'downstream', 'other'))))))
  return(out)  
}






