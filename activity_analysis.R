#########################################################################
###  This Rscript was started by Dr. Jen Cruz at Boise State University #
#     Here we visualize activity times for owls at the Grand Canyon     #
# We then analyze the overlap in their activity times.                  #
##########################################################################
##### Set up your workspace and load relevant packages -----------
# Clean your workspace to reset your R environment. #
rm( list = ls() )
# Check that you are in the right project folder (i.e. that you are in the 
# correct Rstudio project) #
getwd()
#install relevant packages that have not already been installed in your PC:
install.packages( "ggplot2" )#nice plotting

#load relevant packages
library( tidyverse ) #easy data manipulation
library( lubridate ) #easy date and time manipulation
library( ggplot2 )
## end of package load ###############

###################################################################
#### Load or create data -----------------------------------------
#If you are in your correct Rstudio project then it should be:
getwd()
# set up your working directory:
workdir <- getwd()

# set your own datapath
datapath <- "C:/Users/jencruz/Google Drive/QCLabShared/Projects/REUs2021/database/"

#import records: calls file by pasting datapath to filename:
act_df <- read.csv( file = paste0( datapath,"activity_df.csv" ),
                     header = TRUE )

#######################################################################
######## explore data #############

#view to make sure that it looks correct
head( act_df ); dim( act_df )

# we start by viewing the data using a combination of piping and ggplot:
act_df %>% 
  ggplot( ., aes( x = aftersun_h ) ) +
  labs( x = "hours after sunset", y = "counts" ) +
  theme_bw() +
  geom_histogram() +
  facet_wrap( ~record, nrow = 5 )


#############################################################################
# Saving relevant objects and data ---------------------------------

############### END OF SCRIPT ########################################