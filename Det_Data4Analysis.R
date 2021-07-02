#########################################################################
###  This Rscript was started by Dr. Jen Cruz at Boise State University #
# It prepares data of Mexican spotted owls #
# surveyed using sound recorders at historical territories within the   #
# Grand canyon.                                                        #
# Predictors include weather data downloaded from NOAA as hourly records #
# from 01-Jan-2019 to 30-Jun-2021.                                      # 
##########################################################################

##### Set up your workspace and load relevant packages -----------
# Clean your workspace to reset your R environment. #
rm( list = ls() )
# Check that you are in the right project folder (i.e. that you are in the 
# correct Rstudio project) #
getwd()

#install packages
install.packages( "weathermetrics" )

#load relevant packages
library( tidyverse ) #easy data manipulation
library( lubridate )#easy date manipulation
# set option to see all columns and more than 10 rows
options( dplyr.width = Inf, dplyr.print_min = 100 )
library( weathermetrics ) #convert from imperial to metric
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

#import relevant station details
stn_df <- read.csv( file = paste0( datapath,"stn_df.csv" ),
                    header = TRUE )

#import survey details
surveys <- read.csv( file = paste0( datapath,"Survey_details.csv" ),
                     na.strings = c(""," ","NA"),header = TRUE )

#import weather data 
allweather <- read.csv( file = paste0( datapath, "/predictor_data/weather2019-2021GCA.csv"),
                        header = TRUE )

#######################################################################
######## ready data for analysis #############

#which stations had STOC?
keep <- all_df %>% 
  filter( record == 'STOC' ) %>% 
  summarise( survey_id = unique( survey_id ) )
# We have 3 stations to work with

######### creating hourly detection dataframe ---------------------
# We select relevant survey details:
surv_df <- surveys %>% 
          filter( survey_id %in% keep$survey_id )
#check
surv_df
# Note that we need to fix the dates and times
#extract date info only by keeping everything before white space
date_start <- sub(" .*", "", surv_df$start_date)
date_end <- sub(" .*", "", surv_df$end_date)
# did it work?
date_start
# extract time only by keeping everything after white space
start_keep <- sub(".* ", "", surv_df$survey_start_time)
end_keep <- sub(".* ", "", surv_df$end_time)
#combine correct date with correct start time
comb_start <- paste( date_start, start_keep, sep = " " )
# repeat for endtime
comb_end <- paste( date_end, end_keep, sep = " " )

#correct start and end dates and times in survey dataframe:
surv_df <- surv_df %>% 
  mutate( start_time = lubridate::dmy_hms( comb_start, tz = "US/Arizona" ),
  end_time = lubridate::dmy_hms( comb_end, tz = "US/Arizona" ) ) %>% 
  select( survey_id, year, site, station_no, recorder_no, start_time, end_time )
#check
surv_df
# Calculate day of year and hour of day:
surv_df <- surv_df %>% 
  mutate( startday = lubridate::yday( start_time ),
          endday = lubridate::yday( end_time ),
          starthr = lubridate::hour( start_time ),
          #for end hour we round up instead of the standard down
          endhr = lubridate::hour( ceiling_date( end_time, "hour" ) ) )
#view
surv_df

# We can now use this information to create a blank hourly detection dataframe
# we will add our records to this dataframe later #

# We create a for loop that loops through each row of surv_df and uses start and #
# end of day and hour details to create a dataframe. These get appended together #
for( i in 1:length(keep$survey_id) ){
  #start by creating single row dataframe from surv_df 
  df <- surv_df  %>% filter( survey_id == keep$survey_id[i] ) %>% 
        select( survey_id, site, station_no, recorder_no, year )
  #create vector that estimates number of days surveyed and repeats entry for each of 24hrs
  jday <- rep( surv_df$startday[i]:surv_df$endday[i], each = 24 ) 
  #check that it worked
  print( length( jday ) )
  #create vector that repeat 24 hours for all days surveyed
  hr <- rep( 0:23, length( surv_df$startday[i]:surv_df$endday[i] ) )
  # it should be same length as jday vector
  print(length( hr ))
  #append surveyed days and hours to df dataframe 
  # since df is a single row, the values will repeat to match the number of cells 
  # in the jday and hr vectors
  df <- cbind( df, jday, hr) 
  #check that it worked
  print( head( df ) )
  #Now we need to remove extra hours on the 1st and last days:
  # get row ids for actual start and end hours:
  sr <- which( (df$jday == surv_df$startday[i] )& (df$hr == surv_df$starthr[i]) )
  er <- which( (df$jday == surv_df$endday[i] )& (df$hr == surv_df$endhr[i]) )
  #subset dataframe to remove unsurveyed hours on first and last survey days
  df <- df[ sr:er, ]
  #for first survey create the new hourly dataframe
  ifelse( i == 1,  
          # the <<- sign takes it out of the loop into the main workspace
          #otherwise if an object is created only inside a loop it is 
          #temporary 
          hr_df  <<- df,
          #for remaining surveys in surv_df append to existing hourly dataframe
          hr_df <- rbind( hr_df, df ) ) #close ifelse statement
  #check progress
  print( dim( df ) )
  print( dim( hr_df) )
} #end for loop

######### end of survey df --------------------------
###################### manipulating records dataframe #########################
#now we can reduce records dataframe to surveys of interest:
rec_df <- all_df %>%  
        filter( survey_id %in% keep$survey_id )
#check
head( rec_df ); dim( rec_df )
#what does our data look like?
# how many of each record
table( rec_df$record )

#how many days for each station?
sdays <- table( rec_df$date, rec_df$survey_id )
sdays; dim( sdays )
# Note that we only have records for 2020 and 2021. The dates do not overlap
# between stations. Sampling is during Mar-May
# Save the unique dates that we need to get weather info for:
sdates <- sort( rownames( sdays ) )
sdates
# convert the date strings to proper dates with lubridate:
sdates <- lubridate::ymd( sdates )
# calculate day of year (julian day)
jday <- lubridate::yday( sdates )
date_df <- data.frame( sdates = sdates, jday = jday )
head( date_df ); dim( date_df )
# could add stage of breeding season also 
# do it here:

#check duration of records
rec_df %>% ggplot( . ) + 
  theme_classic() +
  geom_histogram( aes( x = duration_m ) ) +
  facet_wrap( ~ record,  )

#how many days had owl calls?
rec_df %>% filter( record == 'STOC' ) %>% 
  group_by( survey_id ) %>% 
  count( date )

####-----------------end records maniputation ------------------------------

# Extract owl detections into a new dataframe:
owl_df <- rec_df %>% filter( record == 'STOC' ) %>% 
          select( -lat, -lon, -noise_type )
#view
head( owl_df ); dim( owl_df )
#turn records calculated as zero minutes to one minute
owl_df$duration_m[ which( owl_df$duration_m == 0 ) ] <- 1
#how long do our calls last?
hist(owl_df$duration_m )
# All owl calls last less than 1 hour. This makes it easier to assign to 
# hourly detection dataframe
#do males and females call at different times?
owl_df %>% ggplot(.) +
  theme_bw() +
  geom_histogram( aes( x = aftersun_h, fill = sex )) + 
  facet_wrap( ~call_type )
#what time was sunset on the days we recorded owls:
hour( owl_df$sunset )

# We can now join owl records onto dataframe:
head( hr_df ); dim( hr_df )
head( owl_df ); dim( owl_df )
# We create a new dataframe in case
owl_hr <- owl_df %>% 
  select( survey_id, year, hr = start_hour, duration_m, stoc = record,
          sunset, aftersun_h ) %>% 
  mutate( jday = yday( sunset ), 
    sunset_hr = hour( sunset ) )
#check
head( owl_hr,20 ); dim( owl_hr )
# Note that owl records in the same hour need to be pooled together. 
owl_hr <- owl_hr %>%
  group_by( survey_id, year, jday, hr ) %>% 
  #choose first records
  summarise( sunset_hr = first(sunset_hr ), 
             sunset = first( sunset ),
             #round hour to whole hour 
             aftersun_h = round( first( aftersun_h ), 0 ),
             #sum duration minutes for each hour
             duration_m = sum( duration_m ),
             #turn owl record to 1:
             stoc = 1 )
#check
owl_hr; dim( owl_hr )

#we are ready to join our dataframes
owl_hr <- hr_df %>% 
        left_join( owl_hr, by = c("survey_id", "year", "jday", "hr" ) )
#check
head( owl_hr ); dim( owl_hr )
unique( owl_hr$jday )
#replace missing values with zero for duration and stoc
owl_hr$stoc[ is.na( owl_hr$stoc ) ] <- 0
owl_hr$duration_m[ is.na( owl_hr$duration_m ) ] <- 0

#get sunset hour for those nearby days that were not in records
sun_df <- owl_hr %>% group_by( jday ) %>% 
  summarise( sunset_hr = last( unique( sunset_hr ) )  ) 
#check
sun_df 
#from here we can see that there are only two hours 18 or 19 for our sampling #
# days. So it is probably unncessary to recalculate sunset_hr for all our records#
#instead we can just replace those jdays <86 with 18 and those >85 with 19
owl_hr$sunset_hr[ which(owl_hr$jday < 86 ) ] <- 18 
owl_hr$sunset_hr[ which(owl_hr$jday > 85 ) ] <- 19
#check
head( owl_df )
#Calculate aftersun_h:
owl_hr$aftersun_h <- owl_hr$hr - owl_hr$sunset_hr 
owl_hr$aftersun_h[ which( (owl_hr$aftersun_h < -2) | (owl_hr$aftersun_h > 5) )] <- 
  owl_hr$aftersun_h[ which( (owl_hr$aftersun_h < -2) | (owl_hr$aftersun_h > 5) )] + 24
#<-
#check
head( owl_hr,20 );dim( owl_hr )
# We never detected owls < 2 hrs or more tan 12 hrs after sun so remove those:
owl_hr <- owl_hr[ which( (owl_hr$aftersun_h > -3 ) & ( owl_hr$aftersun_h < 13 ) ), ]

#check
head( owl_hr );dim( owl_hr )

########### end --------------------------------------
#######################################################################
### sorting out predictors ----------------------------------------
# view raw file
head( allweather ); dim( allweather )
# a lot of records and columns! 
# what columns may we be interested in?
colnames( allweather )
#  [2] "DATE", [44] "HourlyDryBulbTemperature" 
# [45] "HourlyPrecipitation",  [55] "HourlyWindDirection",
# [56] "HourlyWindGustSpeed", [57] "HourlyWindSpeed"            
#Hourly precipitation is total for hour at time of measurement in inches to 
#hundredths:
# Blank/null = no precipitation was observed/reported
# M = missing
# T = indicates trace amount of precipitation
# Temperature is in Farenheight
# Hourly windspeed is at the time of measurement 
# we start by removing columns we are not intersted in:
weather_df <- allweather %>% 
            select( DATE, HourlyPrecipitation, HourlyDryBulbTemperature,
                    HourlyWindSpeed )
#check
head( weather_df )
# Separate date coulm into date and time
weather_df <- weather_df %>% 
  separate( DATE, c("wdate", "wtime"), "T" )

#fix precipitation
unique( weather_df$HourlyPrecipitation )
#Note the empty values are reported as "","T" should probably also be zero
#replace empty and T
weather_df$HourlyPrecipitation[ which( weather_df$HourlyPrecipitation %in% 
                                         c("", "T" ) ) ] <- 0
#now those containing s to represent hundredths
weather_df$HourlyPrecipitation[ grep( "s", weather_df$HourlyPrecipitation ) ] <- 0
#check
unique( weather_df$HourlyPrecipitation )
#now convert to numeric
weather_df$HourlyPrecipitation <- as.numeric( weather_df$HourlyPrecipitation )
#check
head( weather_df )

#check temp:
unique( weather_df$HourlyDryBulbTemperature )
#remove the s from the records and turn to numeric
weather_df$HourlyDryBulbTemperature <- as.numeric(
        sub( "s.*", "", weather_df$HourlyDryBulbTemperature ))

#check wind
unique( weather_df$HourlyWindSpeed )
#replace empty with 0
weather_df$HourlyWindSpeed[ which( weather_df$HourlyWindSpeed == "" ) ] <- 0 
#turn to numeric:
weather_df$HourlyWindSpeed <- as.numeric( weather_df$HourlyWindSpeed )
#check
tail( weather_df,50 )
#convert to metric system and extract hour and day of year:
weather_df <- weather_df %>%
  mutate( HourlyTemp = fahrenheit.to.celsius( HourlyDryBulbTemperature ),
  HourlyRain = inches_to_metric( HourlyPrecipitation, "mm" ),
  hr = hour( ymd_hms( paste( wdate, wtime ), tz = "US/Arizona" ) ),
  jday = yday( ymd( wdate, tz = "US/Arizona" ) ),
  year = year( ymd_hms( paste( wdate, wtime ), tz = "US/Arizona" ) ))
#check
head( weather_df ); dim( weather_df ) 

# now calculate average temperature and total rainfall for each hour
w_df <- weather_df %>% 
   group_by( year, jday, hr ) %>% 
  summarise( HourlyWindSpeed  = mean( HourlyWindSpeed, na.rm = TRUE ),
             HourlyTemp = mean( HourlyTemp, na.rm = TRUE ),
             HourlyRain = sum( HourlyRain ) )
#view
head( w_df )
###### end of weather manipulation ################
#### combine relevant dataframes -------------------------
head( owl_hr ); dim( owl_hr )
#combine
det_df <- left_join( owl_hr, w_df, by = c("year", "jday", "hr"  ))
#check
head( det_df ); dim( det_df )
#it should have 334 rows and 15 columns 

#############################################################################
# Saving relevant objects and data ---------------------------------
#save hourly detection dataframe with weather predictors
write.csv(x = det_df, 
          #ensure that you save it onto your datafolder
          file = paste0( datapath, 'stoc_det_df.csv'), 
          row.names = FALSE )
#save workspace in case we need to make changes
save.image( "Data4AnalysisWorkspace" )

############### END OF SCRIPT ########################################