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
install.packages( "suncalc") #calculates sun and moon phases and positions

# load packages relevant to this script:
library( tidyverse ) #easy data manipulation
library( lubridate ) #easy date and time manipulation
library( sf ) #for spatial data
library( suncalc )

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
datapath <- "G:/My Drive/QCLabShared/Projects/REUs2021/database/"
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
###### start by extracting station details for those stations we have data for ####
#first view objects to ensure that they downloaded properly:
head( surveys ); dim( surveys )
head( stations ); dim( stations )

#join dataframes to extract info only for those stations that we processed #
#data for:
station_df <- left_join( surveys, stations, 
                by = c( "station_no" = "station_id", 
                    "year" = "year", "site" = "site" ) )
#view
station_df; dim( station_df )
# why is checking dimensions important?
#answer: 
#
#replacing missing coordinates from one of the survey locations, wiht
# another survey location within the same pac (site 13):
to <- which(station_df$survey_id == 31)
from <- which(station_df$survey_id == 30)
station_df$easting[to] <- station_df$easting[from]
station_df$northing[to] <- station_df$northing[from]
# to calculate hour when the sun set we need coordinates. Our spatial info is #
# in easting northings but we need lat longs. #
# if you want to learn a bit more about dealing with spatial data, go through #
# the spatial tutorial in our lab...the readme also has links to other online #
# options. #

# Here we rely on the recently created sf package for spatial manipulations. #
# rgdal and sp used to be the main options but sf is more compatible with #
# tidyverse packages so I recommend that instead. #

# We first need to define what UTM we used to collect the coordinates. #
# Details are in the station_details table: UTM WSG84 for zone 12 #
# Those details are captured as epgs. I goggled epgs WSG84 zone 12 to get #
# the epgs code. Then I create an object that defines the projection:
setcrs <- sf::st_crs( 32612 )
#view it:
setcrs
# since our object dataframe doesn't have spatial information. We first create #
# a new spatial dataframe providing the spatial details including epgs:
station_sp <- sf::st_as_sf( station_df, 
                            #which columns contain spatial data:
                            coords = c( "easting", "northing"), 
                            #what projection are we using?
                                crs = setcrs )
# the st_as_sf function converts a traditional dataframe into a spatial dataframe
# note that to do that it asks us to provide coordinates and projection. 
#view
head( station_sp )
# What differences do you note between it and the station_df?

#Note that the easting northing columns have shifted to geometry so if we want #
# to keep them in the dataframe we need to extract them:
station_sp$easting <- st_coordinates( station_sp)[,1]
station_sp$northing <- st_coordinates( station_sp )[,2]

#We then convert our spatial dataframe to lat longs, that requires a different epgs #
# google WSG84 epgs lat long to get the correct epgs:
# We do this by transforming coordinates to the new desired epgs:
station_sp <- st_transform( station_sp, crs = 4326 )
#extract lat longs as columns in the dataframe:
station_sp$lon <- st_coordinates( station_sp)[,1]
station_sp$lat <- st_coordinates( station_sp )[,2]
#check
head( station_sp )

#Convert back to non-spatial dataframe so we can save it as a csv:
# keep the columns we need and remove spatial data 
stn_df <- station_sp %>% 
            dplyr::select( survey_id, year, recorder_no, 
            coord_system, coord_zone, station_no = station_no.y, 
            easting, northing, lon, lat ) %>% 
  #remove spatial info 
  st_set_geometry( NULL )
#check
head( stn_df )
##### end station details for our survey data ---------
# Records table -----------------------------------------------
# View top rows and object attributes
head( records ); dim( records )
# note that the columns are in the order that they were created, not #
# in the viewing order that we had them for entering data. #
# Also that time was added to the date columns, and dates were added to #
# the time columns. We need to remove those as they are not correct.#
# Next time it would be easier to keep the date and time in the same entry. #

#There are intermediate steps that we don't need in our dataframe. #
# One option is to create them as separate objects that don't get added.
#extract date info only by keeping everything before white space
date_keep <- sub(" .*", "", records$record_date)
# did it work?
date_keep[1:10]
# extract time only by keeping everything after white space
start_keep <- sub(".* ", "", records$record_starttime)
end_keep <- sub(".* ", "", records$record_endtime)
#combine correct date with correct start time
comb_start <- paste( date_keep, start_keep, sep = " " )
# repeat for endtime
comb_end <- paste( date_keep, end_keep, sep = " " )
#what does that look like now?
comb_end[1:10]

#Now we are ready to use these updated date/times objects to calculate #
# duration of events
# We rely on tidyverse to do all of these using piping
#create a new dataframe in case you stuff up. This way you don't have to start#
# from scratch. Does that make sense?
records_df <- records %>% 
    #create new columns
    mutate( #create date/time columns for start and endtimes.
      #Note use of lubridate to transfor a string to a date. Defining time zone is important
    start_time = lubridate::dmy_hms( comb_start, tz = "US/Arizona" ),
    end_time = lubridate::dmy_hms( comb_end, tz = "US/Arizona" ),
    #calculate duration of event in minutes, using lubridate functions
    # note duration is in seconds and so we divide by 60 to get to minutes and turn 
    # it into a number with as.numeric
    duration_m = as.numeric( as.duration( interval(start_time, end_time) )/ 60 ) ) %>% 
  # select columns you want to keep and order them 
  dplyr::select( survey_id, start_time, end_time,
                 duration_m, record, sex, call_type, noise_type )

#view that your new dataframe worked
head( records_df ); dim( records_df)
#you can see that some times were entered in the wrong format if you 
# look at the first row. How many more are there?
#here we extract rows with negative values as a way to find out:
records_df[ which( records_df$duration_m <0 ), ]
# We fixed records back in database...so that the correct ones are accesible #
# for others.
# Remaining are those that go onto next day
#start by flagging the row ids of those that need fixing:
rowfix <- which( records_df$duration_m < 0 ) 
#view
rowfix
#loop through each row that needs fixing
for( i in rowfix ){
  #update date of end time to the following day
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

#Recheck duration by plotting a quick histogram:
hist( records_df$duration_m )
#check those > 200 minutes, just in case
records_df[which( records_df$duration_m > 200 ), ]

########calculate sunset times ---------------------------------
#add lat long to records dataframe
# We start by selecting columns of interest in stn_df
records_df <- stn_df %>%
  #extract columns of interest for station dataframe
            dplyr::select( survey_id, lat, lon ) %>%
  #Then append only those columns to records_df
            right_join( records_df, by = "survey_id") 

#check
tail(records_df ); dim( records_df)

#add year column for plotting
records_df$year <- lubridate::year(records_df$start_time) 
#add starthour column to work out which records occured in the morning
records_df$start_hour <- lubridate::hour( records_df$start_time )

#histogram of start times
ggplot( records_df, aes( x = hour(start_time) ) )+
  theme_bw() + geom_histogram() + facet_wrap( ~year, nrow = 3 )

#select those records that occurred in the morning between midnight and 7am:
rid <- which( (records_df$start_hour >= 0 ) & (records_df$start_hour <= 7) )
#we want the previous day's date for these 
#start by creating date column in the format needed by suncalc package:
records_df$date <- as_date( records_df$start_time )
#replace those morning records with previous day's date:
records_df$date[rid] <- as_date( records_df$start_time[rid] %m-% days(1) )
#check
tail(records_df ); dim( records_df)

#now calculate sunset time
sunsets <- getSunlightTimes( data = records_df[ ,c("date", "lat", "lon")], 
                             keep = c("sunset"),
                             tz = "US/Arizona" )
#check
head( sunsets )
#add sunset time to dataframe
records_df$sunset <- sunsets$sunset
#now calculate minutes and hours from sunset
records_df <- records_df %>% 
  #same approach as before using lubridate functions
  mutate( aftersun_m = as.numeric( as.duration( interval( sunset, start_time) /60 )),
          #we also calculate it in hours for easier plotting
          aftersun_h = aftersun_m / 60 )
#check
head( records_df );dim( records_df )
#plot histogram of negative records
records_df %>% filter( record == 'wind' ) %>%
  filter( duration_m < 60 ) %>% 
  ggplot(.) + theme_classic() +
  geom_histogram( aes( x = duration_m))


########## end of initial data cleaning #################################
#############################################################################
#### creating project specific databases  ####################################
# Activity time dataframe --------------
# Choose only records with owls (since they are all in capital letters, #
# we rely on that attribute for our row selection )#
#here we create a new dataframe that filters out all rows except those with upper
# letters in the record column:
act_df <- records_df %>% filter( str_detect( record, '[:upper:]') )
#check that it worked
head( act_df ); dim( act_df )
#so total of 89 records to work with 
#####end activity df #######
######
#############################################################################
# Saving relevant objects and data ---------------------------------
# Note we don't save the workspace here. Why is that?
#save all records dataframe
write.csv(x = records_df, 
          #ensure that you save it onto your datafolder
          file = paste0( datapath, 'clean_records_df.csv'), 
          row.names = FALSE )

# Save activity time dataframe
write.csv(x = act_df, 
          #ensure that you save it onto your datafolder
          file = paste0( datapath, 'activity_df.csv'), 
          row.names = FALSE )

#saving station details for those we surveyed
write.csv( x = stn_df, file = paste0( datapath, 'stn_df' ), 
           row.names = FALSE )

############### END OF SCRIPT #################################################