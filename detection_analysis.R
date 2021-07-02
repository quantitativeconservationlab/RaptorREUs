#########################################################################
###  This Rscript was started by Dr. Jen Cruz at Boise State University #
# It estimates factors that influence detection of Mexican spotted owls #
# surveyed using sound recorders at historical territories within the   #
# Grand canyon.                                                        #
# Predictors include weather data downloaded from NOAA as hourly records #
# from 01-Jan-2019 to 30-Jun-2021.                                      # 
# Also time of day and hours after sunset                                #
##########################################################################

##### Set up your workspace and load relevant packages -----------
# Clean your workspace to reset your R environment. #
rm( list = ls() )
# Check that you are in the right project folder (i.e. that you are in the 
# correct Rstudio project) #
getwd()

#load relevant packages
library( tidyverse ) #easy data manipulation
# set option to see all columns and more than 10 rows
options( dplyr.width = Inf, dplyr.print_min = 100 )
## end of package load ###############

###################################################################
#### Load or create data -----------------------------------------
#If you are in your correct Rstudio project then it should be:
workdir <- getwd()

# set your own datapath
datapath <- "C:/Users/jencruz/Google Drive/QCLabShared/Projects/REUs2021/database/"

#import records file:
all_df <- read.csv( file = paste0( datapath,"clean_records_df.csv" ),
                    header = TRUE )
#######################################################################
######## ready data for analysis #############

#############################################################################
# Saving relevant objects and data ---------------------------------
#save hourly detection dataframe with weather predictors
write.csv(x = det_df, 
          #ensure that you save it onto your datafolder
          file = paste0( datapath, 'stoc_det_df.csv'), 
          row.names = FALSE )
#save workspace in case we need to make changes
save.image( "STOCDetResultsWorkspace" )

############### END OF SCRIPT ########################################