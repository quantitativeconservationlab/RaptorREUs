#########################################################################
###  This Rscript was started by Dr. Jen Cruz at Boise State University #
#                                                                       #
##########################################################################
##### Set up your workspace and load relevant packages -----------
# Clean your workspace to reset your R environment. #
rm( list = ls() )
# Check that you are in the right project folder (i.e. that you are in the 
# correct Rstudio project) #
getwd()
#install relevant packages that have not already been installed in your PC:
install.packages( "ggplot2" )#nice plotting
install.packages( "suncalc") #calculates sun and moon phases and positions

#load relevant packages
library( tidyverse ) #easy data manipulation
library( lubridate ) #easy date and time manipulation
library( ggplot2 )
library( suncalc )
## end of package load ###############

###################################################################
#### Load or create data -----------------------------------------
#If you are in your correct Rstudio project then it should be:
getwd()
# set up your working directory:
workdir <- getwd()

# set datapath
datapath <- "C:/Users/jencruz/Google Drive/QCLabShared/Projects/REUs2021/database/"

#import records: calls file by pasting datapath to filename:
act_df <- read.csv( file = paste0( datapath,"activity_df.csv" ),
                     header = TRUE )

#import station details
stn_df <- read.csv( file = paste0( datapath, "stn_df.csv"), header = TRUE )

#######################################################################
######## explore data #############

#join station details to our activity times

#view to make sure that it looks correct
head( act_df ); dim( act_df )
head( stn_df ); dim( stn_df )

#which database is the main one that we are appending to?
data_df <- left_join( act_df, stn_df, by = "survey_id") %>% 
            #create columns necessary for calculating sunset times
            mutate( date = as_date(start_time),
                    lon =long,
                    station_no = station_no.y ) %>% 
          dplyr::select( -coord_system, -coord_zone, -long,  -station_no.y) 
#check 
head(data_df ); dim( data_df )


#calculate sunset on that date
#need to include late and long for that location
sunsets <- getSunlightTimes( data = data_df[ ,c("date", "lat", "lon")], 
                  keep = c("sunset"),
                  tz = "US/Arizona" )

#add sunset time to dataframe
data_df$sunset <- sunsets$sunset
#now calculate minutes since sunset
data_df <- data_df %>% 
  mutate( time_only = hour( start_time ),
    min_aftersun = as.numeric( as.duration( interval( sunset, start_time) /60 )),
    hr_aftersun = min_aftersun / 60 )

#check
head( data_df ); dim( data_df )

#append to main dataframe
data_df <- left_join()

interval( data_df$sunset[1], data_df$start_time[1])

#check other objects
head( surveys ); dim( surveys )

# we start by viewing the data using a combination of piping and ggplot:
data_df %>% 
  #ggplot( ., aes( x = time_only ) ) +
  ggplot( ., aes( x = hr_aftersun ) ) +
  labs( x = "hour", y = "counts" ) +
  theme_bw() +
  geom_histogram(stat = 'count') +
  facet_wrap( ~record, nrow = 5 )

#but this approach doesn't account for sunset changes with time. 

as_hms( sub(".* ", "", act_df$start_time[1] ))
a <- strftime(act_df$start_time[1], "%H:%M:%S")
hour( act_df$start_time[1])
############### END OF SCRIPT ########################################