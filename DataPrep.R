#########################################################################
###  This Rscript was started by Dr. Jen Cruz at Boise State University #
# to help support Raptor REU students with their analysis. In this first #
# script we import data and perform basic manipulations, checks and     #
# visualizations to ready it for analysis.                              #
# Data are detections from acoustic sound recorders placed in the Grand #
# Canyon National Park during months of Mar-Jul, 2019 to 2021.          #
# Data were initially processed using RAVEN, without a filter band, which #
# allowed detections of potential sources of noise that could disrupt #
# owl detections.                                                      #
#                                                                       #
##########################################################################

# Remember to create a new Rstudio project specific to this project. #
# I called mine RaptorREUs. 

##### Set up your workspace and load relevant packages -----------
# Clean your workspace to reset your R environment. #
rm( list = ls() )
# Check that you are in the right project folder (i.e. that you are in the 
# correct Rstudio project) #
getwd()

# Install new packages from "CRAN" repository if you don't have them. # 
install.packages( "tidyverse" ) #actually a collection of packages 
install.packages( "lubridate") 
install.packages( "sf" ) #manipulate spatial data

# load packages relevant to this script:
library( tidyverse ) #easy data manipulation
library( lubridate ) #easy date and time manipulation
library( sf ) #for spatial data
## end of package load ###############

###################################################################
#### Load or create data -----------------------------------------
# Set working directory. This is the path to your Rstudio folder for this 
# project. If you are in your correct Rstudio project then it should be:
getwd()
# if so then:
workdir <- getwd()

# set path to where you can access the Access database in your computer. #
# Note that the path will be different in yours than mine.#
datapath <- "C:/Users/jencruz/Google Drive/QCLabShared/Projects/REUs2021/database/"
#import relevant tables, which have been exported from Access database as #
# .csv files# 

#import records: calls file by pasting datapath to filename:
records <- read.csv( file = paste0( datapath,"Records.csv" ),
                     #replaces those values with NA, includes column heading
                     na.strings = c(""," ","NA"), header = TRUE )

#import survey details
surveys <- read.csv( file = paste0( datapath,"Survey_details.csv" ),
                     na.strings = c(""," ","NA"),header = TRUE )

#import station details
stations <- read.csv( file = paste0( datapath,"station_details.csv" ),
                     na.strings = c(""," ","NA"), header = TRUE )
##############

#######################################################################
######## explore data #############
# start by extracting station details for those stations we have data for
head( surveys ); dim( surveys )
head( stations ); dim( stations )

#we join dataframes 
station_df <- left_join( surveys, stations, 
                by = c( "station_no" = "station_id", 
                    "year" = "year", "site" = "site" ) )
#view
station_df; dim( station_df )

#define spatial info from details in database WSG84 for zone 12
setcrs <- sf::st_crs( 32612 )
station_sp <- sf::st_as_sf( station_df, 
                            coords = c( "easting", "northing"), 
                                crs = setcrs )
#at this point the easting northings have shifted to geometry so if we want #
# to keep them in the dataframe we need to extract them
station_sp$easting <- st_coordinates( station_sp)[,1]
station_sp$northing <- st_coordinates( station_sp )[,2]
#We convert to lat longs so that we can calculate sunshine time
station_sp <- st_transform( station_sp, crs = 4326 )
#extract these values to the dataframe again:
station_sp$long <- st_coordinates( station_sp)[,1]
station_sp$lat <- st_coordinates( station_sp )[,2]

#view
head( station_sp )
#now keep the columns we need and remove spatial data
stn_df <- station_sp %>% dplyr::select( survey_id, year, recorder_no, 
            coord_system, coord_zone, station_no.y, 
            easting, northing, long, lat ) %>% st_set_geometry( NULL )
#check
head( stn_df )
# clean records table -----------------------------------------------
# View top rows and object attributes
head( records ); dim( records )
# note that the columns are in the order that they were created, not #
# in the viewing order that we had them for entering data. #
# Also that time was added to the date columns, and dates were added to #
# the time columns. We need to remove those. 

#There are intermediate steps that we don't want to keep in our dataframe
# so we just create these as separate objects
#keep everything before white space
date_keep <- sub(" .*", "", records$record_date)
#keep everything after white space
start_keep <- sub(".* ", "", records$record_starttime)
end_keep <- sub(".* ", "", records$record_endtime)
#combine correct date with correct start time
comb_start <- paste( date_keep, start_keep, sep = " " )
comb_end <- paste( date_keep, end_keep, sep = " " )

#Now we are ready to use our updated date/times and to calculate #
# duration of events
# We rely on tidyverse to do all of these using piping
#create a new dataframe for your records
records_df <- records %>% 
    #create new columns
    mutate( #create date/time columns for start and endtimes
    start_time = lubridate::dmy_hms( comb_start, tz = "US/Arizona" ),
    end_time = lubridate::dmy_hms( comb_end, tz = "US/Arizona" ),
    #calculate duration of event in minutes
    duration_m = as.numeric( as.duration( interval(start_time, end_time) )/ 60 ) ) %>% 
  # select columns you want to keep and order them 
  dplyr::select( survey_id, night_no, start_time, end_time,
                 duration_m, record, sex, call_type, noise_type )

#view that your new dataframe worked
head( records_df ); dim( records_df)
#you can see that some times were entered in the wrong format if you 
# look at the first row. How many more are there?
#here we extract rows with negative values as a way to find out:
records_df[ which( records_df$duration_m <0 ), ]
#fix those that go onto next day
#start by flagging the row ids of those that need fixing:
rowfix <- which( records_df$duration_m < 0 ) 
#view
rowfix
#loop through each row that needs fixing
for( i in rowfix ){
  #update date of end time
  newdate <- lubridate::dmy_hms( paste( date_keep[i], end_keep[i], 
                    sep = " " ), tz = "US/Arizona" ) %m+% days(1)
#check that it worked
print( newdate)
#calculate new duration
a <- as.duration( interval(records_df$start_time[i], newdate  ) ) 
#check that it worked
print( a )
#convert from seconds to minutes and numeric
b <- as.numeric( a/60 )
#check
print( b )
#now replace corresponding values in datasheet
records_df$end_time[i] <- newdate
records_df$duration_m[i] <- b
}

#Now recheck that the duration periods look ok by plotting them in a histogram:
hist( records_df$duration_m )
#check those > 200 minutes, just in case
records_df[which( records_df$duration_m > 200 ), ]

########## end of initial data cleaning #################################
#### creating project specific databases  ####################################

# Activity time dataframe --------------
# Choose only records with owls (since they are all in capital letter, #
# we rely on that attribute for our row selection )#
#here we create a new dataframe that filters out all rows except those with upper
# letters in the record column:
act_df <- records_df %>% filter( str_detect( record, '[:upper:]') )
#check that it worked
head( act_df ); dim( act_df )
#so total of 89 records to work with 


# Saving relevant objects and data ---------------------------------
# Note we don't save the workspace here. Why is that?
#save station details for relevant surveys
write.csv(x = stn_df, 
          #ensure that you save it onto your datafolder
          file = paste0( datapath, 'stn_df.csv'), 
          row.names = FALSE )
# Save activity time dataframe
write.csv(x = act_df, 
          #ensure that you save it onto your datafolder
          file = paste0( datapath, 'activity_df.csv'), 
          row.names = FALSE )

############### END OF SCRIPT ########################################