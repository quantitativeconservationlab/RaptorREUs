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
#library( sf ) #for spatial data
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
                 -Comments.Notes)
#view
head(ddf)

##now focus on cleaning detections
table(datadf$First.detection.type)
#### write results somewhere of raw detections by each type
ddf <- ddf %>% 
    dplyr::mutate( Date = sub(" .*", "", Date),
        #date = lubridate::mdy(Date, tz = "MST"),
        starttime = lubridate::mdy_hm( paste(Date, Start.time ), tz = "MST"),
        detecttime = lubridate::mdy_hm( paste(Date, Time.to.first.detection ),
                                        tz = "MST"),
        jday = yday(starttime),
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
### we can continue for now. Next step is to create a column noting detection
# as 1 or 0 
ddf<- ddf %>% 
  select( Site.ID, Date, jday, starttime, detecttime, timetodetect,
          Observer.Type, Observer.Name, Survey.Type, First.detection.type ) %>% 
  mutate( detection = ifelse( First.detection.type == "", 0, 1 ) )

#check
head(ddf)

##################################### save #################################
# clean trap details
write.csv( ddf, file = "GroundSquirrelOccupancy/CleanData.csv",
           row.names = FALSE )

############################ end of script ####################################
