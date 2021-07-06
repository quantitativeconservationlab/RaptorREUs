############################################################################
### Script developed by Jen Cruz to assess occurrence of noise disturbance # 
# that may hinder our ability to detect owls                               #
############################################################################

##### Set up your workspace and load relevant packages -----------
# Clean your workspace to reset your R environment. #
rm( list = ls() )

# load packages relevant to this script:
library( tidyverse ) #easy data manipulation
library( lubridate ) #easy date and time manipulation
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

#import clean records dataframe
rec_df <- read.csv( file = paste0( datapath,"clean_records_df.csv" ),
          header = TRUE )
#import activity density estimates from using overlap package "delta1":
act_df <- read.csv( file = paste0( datapath, 'stoc_activitydens.csv'),
                    header = TRUE )

#import hourly detection dataframe with weather predictors:
stoc_df <- read.csv( file = paste0( datapath,"stoc_det_df.csv" ),
                     header = TRUE )

#######################################################################
######## ready data for analysis #############

# hours in act_df are 0:23 but on stoc_df, 23 is -1 and 22 is -2. #
# We edit those so that we can append activity density to our main dataframe
act_df$hrs_aftersun[ which(act_df$hrs_aftersun == 23) ] <- -1
act_df$hrs_aftersun[ which(act_df$hrs_aftersun == 22) ] <- -2

head( rec_df ); dim( rec_df )
#which records do we have
unique( rec_df$record )
# which other owls do we remove
owlrm <- c( "BUVI", "OTFL", "AEAC", "MEKE" )
# Exclude other owls
noise_df <- rec_df %>% filter( !(record %in% owlrm ) )
#check
head( noise_df ); dim( noise_df )
# We create a new column of combined record categories.
noise_df <- noise_df %>% 
  #create new column that replaces those categories listed as noise, leaves others as they are
  mutate( soundcat = ifelse( record %in% c("human", "frogs", "bird", "insect",
          "noise"), "other", record ),
          # convert 0 duration to 1 minute
          duration_m = ifelse( duration_m == 0, 1, duration_m ),
          jday = yday( lubridate::ymd(date) ) )
#check
head( noise_df ); dim( noise_df )
# some occurred in the same hour so combine those and calculate new duration
# also remove extra columns
noise_df <- noise_df %>% 
  group_by( survey_id, year, jday, start_hour, soundcat ) %>% 
  summarise( duration_m = sum( duration_m ),
             aftersun_h = first( aftersun_h ) )
#check
head( noise_df ); dim( noise_df )
#some records lasted more than 1 hour so need to add them to following hours
noise_df[ which( noise_df$duration_m >60),]
#which rows need replacing
rowid <- which( noise_df$duration_m > 60)
#what are the durations we need to fix?
noise_df$duration_m[rowid ]
#round up to calculate how many rows are required for the record
rowadd <- ceiling( noise_df$duration_m[ rowid ] / 60 ) 
#loop through each to create extended dataframe of records
for( i in 1:length(rowid) ) {
  #create new dataframe by extracting relevant row from noise_df
  df <- noise_df[rowid[i],] %>% select( -duration_m ) 
  #calculate duration minutes for each hour
  duration_m <- c( rep( 60, rowadd[i]-1), 
        noise_df$duration_m[ rowid[i] ] - sum(rep( 60, rowadd[i]-1) ) )
  #remove the first one since it's already in the noise_df
  duration_m <- duration_m[2:length(duration_m)]
  #extend dataframe
  df <- data.frame( df, duration_m )
  #check
  print( df )
        ifelse( i == 1,
        #append to dataframe and create as object outside the loop
        add_df <<- df, 
        #else if not first record then just append to existing dataframe:
        add_df <- dplyr::bind_rows( add_df, df ) )
} #close for loop
#check output
head( add_df ); dim( add_df ) 
#now append extra rows to noise_df
noise_df <- bind_rows( noise_df, add_df )
#check it
head( noise_df); dim( noise_df) 
#replace original duration_m with 60
noise_df$duration_m[ which( noise_df$duration_m > 60) ] <- 60

##### end data manipulation ##############################
#########################################################################
############## display results ------------------------------------
#plot histograms for all sound categories:
noise_df %>% 
  filter( soundcat != "STOC" ) %>% 
  ggplot( . ) + #choose your data
  theme_bw( base_size = 15 ) + #choose a preset theme
    geom_histogram( aes( x = aftersun_h, fill = as.factor( soundcat ) ),#, color = as.factor(soundcat) ),
                    bins = 10 ) 

# what are the revised durations
hist( noise_df$duration_m )
#remove those that occured for less than 20 min
noise_df %>% filter(duration_m > 20 ) %>% 
  filter( soundcat != "STOC" ) %>% 
  ggplot( . ) + #choose your data
  theme_bw( base_size = 15 ) + #choose a preset theme
  geom_histogram( aes( x = aftersun_h, fill = as.factor( soundcat ) ),#, color = as.factor(soundcat) ),
                  bins = 10 ) 
###OR
#remove those that occured for less than 20 min
noise_df %>% filter(duration_m > 20 ) %>% 
  filter( soundcat != "STOC" ) %>% 
  ggplot( . ) + #choose your data
  theme_bw( base_size = 15 ) + #choose a preset theme
  geom_histogram( aes( x = aftersun_h ),#, color = as.factor(soundcat) ),
                  bins = 10 ) +
  facet_wrap( ~as.factor( soundcat) )


# ##########################################################
########## exploring wind further ###########################
#extract only wind records recorded in RAVEN
wind_df <- noise_df %>%  filter( soundcat == 'wind' )
#check 
head( wind_df ); dim( wind_df)
#check hourly dataframe
head( stoc_df )
#add windspeed from stoc_df to wind_df
wind_df <- stoc_df %>% select( survey_id, year, jday, 
            start_hour = hr, HourlyWindSpeed ) %>% 
      right_join( wind_df, by = c('survey_id', 'year', 'jday', 'start_hour') )
#check
head(wind_df); dim( wind_df )

#what type of wind occurred on hours when owls were detected?
#select rows with owl detections
stocrow <- which(noise_df$soundcat == 'STOC' )
#what days and start times did they occur
js <- noise_df$jday[ stocrow ]
hrs <- noise_df$start_hour[ stocrow ]
#create loop that extracts all records that occured in those 
#days at those times:
for( i in 1:length(stocrow) ){
  a <- noise_df %>% filter( jday == js[i] & start_hour == hrs[i] ) 
  ifelse(
    i == 1, 
    ws_df <<- a,
    ws_df <- bind_rows( ws_df, a )
  )
}
#check
head( ws_df ); dim( ws_df)
#keep only wind
ws_df <- ws_df %>% filter( soundcat == 'wind' )
#add windspeed from stoc_df 
ws_df <- stoc_df %>% select( survey_id, year, jday, 
                               start_hour = hr, HourlyWindSpeed ) %>% 
  right_join( ws_df, by = c('survey_id', 'year', 'jday', 'start_hour') )
#check
head(ws_df); dim( ws_df )

#Now plot wind speed counts for all recorded wind records
ggplot( wind_df ) +
  theme_bw( base_size = 15 ) +
  geom_histogram( aes( HourlyWindSpeed ) )

#And now only for those wind records that occured at the same 
# time when owls were detected
ggplot( ws_df ) +
  theme_bw( base_size = 15 ) +
  geom_histogram( aes( HourlyWindSpeed ) )

# We were able to detect owls at windspeeds <15
#what about duration?
ggplot( ws_df ) +
  theme_bw( base_size = 15 ) +
  geom_histogram( aes( duration_m ) )
#duration didn't appear to matter

#############################################################################
# Saving relevant objects and data ---------------------------------
#save workspace in case we need to make changes
save.image( "NoiseResultsWorkspace.RData" )

##################     END OF SCRIPT      ##################################