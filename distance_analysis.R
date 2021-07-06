#########################################################################
###  This Rscript was started by Dr. Jen Cruz at Boise State University #
# It processes and analyses data from the distance trials we performed #
# at the Grand Canyon, looking at the effects of distance, recorder ID #
# and habitat type on our ability to hear sound on a given recorder #
# data was logged into survey123                                      #
########################################################################

##### Set up your workspace and load relevant packages -----------
# Clean your workspace to reset your R environment. #
rm( list = ls() )
# Check that you are in the right project folder (i.e. that you are in the 
# correct Rstudio project) #
getwd()

#load packages
library( tidyverse )
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
raw_df <- read.csv( file = paste0( datapath,"GC_distance_trials.csv" ),
                     header = TRUE )

#######################################################################
######## ready data for analysis #############
# check out raw dataframe from survey123
tail( raw_df ); dim( raw_df )
#get column names for the distance columns
distcols <- grep( "X", colnames(raw_df),value = TRUE )
# create distance dataframe by removing extra columns
dist_df <- raw_df %>%  
  select( Date.Time, Habitat.type, Recorder.ID, Trial.ID, all_of(distcols) )

#check 
head( dist_df ); dim( dist_df)

dist_df <- dist_df %>% rowwise() %>% 
  mutate( maxdist  = last(distcols[ grep( 'YES', dist_df[,distcols] ) ]) )
#check 
head( dist_df ); dim( dist_df)

#############################################################################
# Saving relevant objects and data ---------------------------------
#save workspace in case we need to make changes
save.image( "DistanceResultsWorkspace.RData" )

############### END OF SCRIPT ########################################