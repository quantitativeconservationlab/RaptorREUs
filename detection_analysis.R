#########################################################################
###  This Rscript was started by Dr. Jen Cruz at Boise State University #
#     Here we relate detection of Mexican spotted owls using sound      #
#   recorders at historical territories within the canyon where they are #
#  known to occur.                                                       #
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
all_df <- read.csv( file = paste0( datapath,"clean_records_df.csv" ),
                    header = TRUE )

#import station details for those we processed
stn_df <- read.csv( file = paste0( datapath,"stn_df.csv" ),
                    header = TRUE )

#######################################################################
######## ready data for analysis #############
head( all_df )
# to start with we need to know which stations had STOC?
keep <- all_df %>% 
  filter( record == 'STOC' ) %>% 
  summarise( survey_id = unique( survey_id ) )

# We use those to filter out the survey stations that we are keeping for #
# further analysis. #
dec_df <- all_df %>%  
        filter( survey_id %in% keep$survey_id )
#check
head( dec_df ); dim( dec_df )
#what does our data look like?
# how many of each record
table( dec_df$record )

#how many days for each station
sdays <- table( dec_df$date, dec_df$survey_id )
sdays
# Note that we only have records for 2020 and 2021. The dates do not overlap
# between stations. Sampling is during Mar-May
# Save the unique dates that we need to get weather info for:
sdates <- sort( rownames( sdays ) )
sdates
# convert the date strings to proper dates with lubridate:
sdates <- ymd( sdates )
# calculate day of year (julian day)
jday <- yday( sdates )
date_df <- data.frame( sdates = sdates, jday = jday )
head( date_df ); dim( date_df )
# could add stage of breeding season also 
# do it here:

#check duration of records
dec_df %>% ggplot( . ) + 
  theme_classic() +
  geom_histogram( aes( x = duration_m ) ) +
  facet_wrap( ~ record,  )



#############################################################################
# Saving relevant objects and data ---------------------------------

############### END OF SCRIPT ########################################