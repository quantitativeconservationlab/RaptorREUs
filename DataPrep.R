################################################################################
### This script is designed by Jen Cruz to clean occupancy data collected ####
### in Spring 2025 checking for squirrels (heard or seen), active burrows ####
### or fresh scat. Protocol was a time to detection approach where survey   ###
### stopped if squirrels were detected. Otherwise, detections lasted 5min with #
### random searches. Otherwise searches were conducted by observer only (tech2)#
### along transects. #
###############################################################################
##### Set up your workspace and load relevant packages -----
# load packages relevant to this script:
library( tidyverse ) #easy data manipulation
options( dplyr.width = Inf, dplyr.print_min = 100 )
library( lubridate ) #easy date and time manipulation
## end of package load ###############

############################################################
#### Load or create data ---------------------------------------
# Clean your workspace to reset your R environment. #
rm( list = ls() )

#load workspace 
#load("DataCleanResults.RData" )

#import cleaned and formatted 2021-2022 data 
rawdf <- read.csv( "GroundSquirrelOccupancy/survey_2025.csv")

#######################################################################
######## clean  data #############
#view
head( rawdf)
colnames(rawdf)

#create new data frame where clean data will be stored
datadf <- rawdf %>% 
  #start by removing superfluous columns
  dplyr::select( -GlobalID, -CreationDate, -Creator,
                 -EditDate, -Editor, -GPS.track.recorded, -x, -y )

#view
head(datadf)
#check if the other columns have info
unique(datadf$Other...Observer.Name)
unique(datadf$Other...Site.ID)
#nope so can remove

datadf <- datadf %>% 
  dplyr::select( -Other...Site.ID, -Other...Observer.Name )

#view 
tail(datadf)

#info that may be good to summarize
unique(datadf$Dominant.Shrub) #empty
datadf$Fresh.jackrabbit.scat.samples.collected[datadf$Fresh.jackrabbit.scat.samples.collected>0]
#none???
###this is wrong as at least 1 comment reports rabbit scats collected

datadf$Fresh.squirrel.scat.samples.collected[datadf$Fresh.squirrel.scat.samples.collected>0]
#7 only?

unique(datadf$Comments.Notes)

#create new dataframe of abreviated results
ddf <- datadf %>% 
  dplyr::select( -Fresh.squirrel.scat.samples.collected, -Fresh.jackrabbit.scat.samples.collected,
                 -Dominant.Shrub, -Dominant.Understory, -ObjectID, -Bearing, -Wind..km.h.,
                 -Comments.Notes )
#view
head(ddf)

##now focus on cleaning detections
table(datadf$First.detection.type)
#### write results somewhere of raw detections by each type!

### fix dates
ddf <- ddf %>% 
  # remove 6pm from date column
    dplyr::mutate( Date = sub(" .*", "", Date),
        #combine date with start time:
        starttime = lubridate::mdy_hm( paste(Date, Start.time ), tz = "MST"),
        #combine date with time detection time:
        detecttime = lubridate::mdy_hm( paste(Date, Time.to.first.detection ),
                                        tz = "MST"),
        #now work out day of year
        jday = yday(starttime),
        #calculates difference in minutes between start time and time to detection:
        timetodetect = difftime( detecttime, starttime, units = "mins" ) )

#view
head( ddf)

#### check that our detection metrics are correct
#plot histogram of time to detection
hist( as.double( ddf$timetodetect ) )

#note negative values, with data entered wrong
#check which ones
ddf[ (ddf$timetodetect < 0), ]
ddf[ (ddf$timetodetect > 10), ]
##several records that need checking #####

#check day of year 
hist(ddf$jday )
#also looks like date was entered wrong #
#check
ddf[ ddf$jday > 125, ]
# 6 records that need fixing
#check that dates make sense
min(ddf$Date); max(ddf$Date)
#those are all fixed now
### we can continue for now. Next step is to create a column noting detection
# as 1 or 0 
ddf<- ddf %>% 
  select( Site.ID, Date, jday, starttime, detecttime, timetodetect,
          Observer.Type, Observer.Name, Survey.Type, First.detection.type ) %>% 
  mutate( detection = ifelse( First.detection.type == "", 0, 1 ) )

#check
head(ddf)

#there appears to be records assigned to the wrong type or technician:
ddf %>% filter( Observer.Type == "Lead" ) %>% 
  filter( Survey.Type == "Day 1 Transect" )
#fixed
#check how many sites had detections
ddf %>% filter( First.detection.type != "" ) %>% 
  group_by( Site.ID ) %>% 
  slice(1)
#now check sites with < 5 records
a <- ddf %>%  
  group_by( Site.ID ) %>% 
  summarize( detected = sum(detection),
  surveys = n() )

#there should be no sites with 1 survey only and no detections
a %>% filter( detected == 0 ) %>% 
  filter( surveys == 2 )

#lastly check for duplicated records
ddf[duplicated( ddf ),]
#check
ddf %>% filter( Site.ID == "74002")

#fixed as much as possible. some datasheets are missing!!!
########
#############################################################################
############ getting data ready for unmarked analysis #################
#first need to assign survey id 
widedf <- ddf %>% arrange( Site.ID, jday, Survey.Type ) %>% 
  group_by( Site.ID ) %>% 
  mutate( survey  = row_number() )
  
#check
head(widedf)

#check for sites that were surveyed more than 5 times
widedf[ widedf$survey > 5,]
#two sites! check surveys
widedf[ widedf$Site.ID == "42776", ]
widedf[ widedf$Site.ID == "POSE2", ]
widedf[ widedf$Site.ID == "81215", ]
dim(widedf)
#remove those additional surveys
widedf <- widedf[ !(widedf$survey > 5), ]
#check
dim(widedf)
#removed extra records 

#replace empty detection type with none
widedf$First.detection.type[ widedf$First.detection.type == "" ] <- "None"

table( widedf$Survey.Type, widedf$timetodetect )
head(widedf)

#modify time to detection for analysis 
widedf %>% filter(detection == 1) %>% 
  filter( timetodetect > 5 )
#modify detections to < 5 min time
widedf$timetodetect[ widedf$detection == 1 & widedf$timetodetect > 5] <- 4
#modify non-detections to 5 min
widedf$timetodetect[ widedf$detection == 0 & widedf$timetodetect > 5] <- 5

#turn wide
widedf <- widedf %>% 
  dplyr::select( -Observer.Type, -Date, -detecttime, -starttime,
                 -Survey.Type ) %>% 
  pivot_wider( names_from = survey, 
               values_from = c(jday, timetodetect, Observer.Name,
                                First.detection.type,
                               detection ) )

#view
head( widedf)



##################################### save #################################
# clean long format data
write.csv( ddf, file = "GroundSquirrelOccupancy/CleanData.csv",
           row.names = FALSE )

write.csv( widedf, file = "GroundSquirrelOccupancy/WideData.csv",
           row.names = FALSE )

#save workspace to continue working from where we left off
save.image("DataCleanResults.RData" )
############################ end of script ####################################
