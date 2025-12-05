#######################################################################
#######################################################################
##     This script was created by Dr. Jen Cruz                       ##  
##                                                                   ##  
## Here we import our model results for one season of occupancy model  ##
#   for Piute ground squirrels at the NCA.                            ##
##                                                                   ##
#######################################################################
##### Set up your workspace and load relevant packages -----------
# Clean your workspace to reset your R environment. #
rm( list = ls() )
# Check that you are in the right project folder
getwd()

# load packages:
library( tidyverse )#includes dplyr, tidyr and ggplot2
options( dplyr.width = Inf, dplyr.print_min = 100 )
library( unmarked ) #
## end of package load ###############
###################################################################
#### Load or create data -----------------------------------------
# Clean your workspace to reset your R environment. #
rm( list = ls() )

#load workspace 
load("OccResults.RData" )
#### End of data load -------------
####################################################################

##### Producing model output ##############
# Now that we have evaluated the value of our model we can produce #
# some output. If our aim is inference on what drives occupancy and #
# detection of Piute ground squirrels at the NCA, we would want to #
# plot the relationships between our model predictors and responses #

# Using our global model we estimate partial prediction plots #
# for our predictors. These plot the relationship between the #
# response and predictor of interest, while keeping the remaining #
# predictors at their mean values. # 

# To create nice plots we first need to create new vectors with evenly #
# spaced values for our continuous predictors within their actual observed range: #
# Why do we not predict outside the observed range?
# Answer:
# 
###occupancy relationships ############
# we start choosing the number of values we want to predict over:
n <- 100
# start with sagebrush
# we use our data (unscaled) to extract the observed range of the predictor:
sagebrush <- seq( min( ddf[,"sagebrush"]),max( ddf[,"sagebrush"]),
                  length.out = n )
#view
sagebrush
#standardize these values
sage.std <- scale( sagebrush )
#view
sage.std

#combine standardized predictor with other predictors in the model, kept at #
# their standardized mean:
sageData <- data.frame( sagebrush = sage.std, cheatgrass = 0 )
# Note that you have to label the  columns exactly the same as the names of your
# predictors in the model

#Use predict function to predict expected probability of occupancy across the 
# range of values you determine above for your predictor of interest, while 
# keeping other predictors at their mean:
pred.occ.sage <- predict( fm.closed, type = "state", newdata = sageData, 
                          appendData = TRUE )
# Note that you have to define which submodel you want to use for the prediction
# using the type = 'state' 
#view results
head( pred.occ.sage ); dim( pred.occ.sage )


################# detection relationships ############################
#######
### In our detection submodel we have a combination of continuous and #
#now if we want to look at differences in detection for the different levels
# of the categorical variable, we create a dataframe that varies those levels
# and keep the other predictors in that submodel at their mean value:
obsvDet <- data.frame( 
  jday = 86, 
  obsv = factor( c("David Bontrager","Dylan Hendry",
                   "Jessica Hovey", "Lillie Scofield", "Mariah Hoel",    
                   "Stefanie Buxel","Sydney Smith" ), 
                 levels = c("David Bontrager","Dylan Hendry",
                            "Jessica Hovey", "Lillie Scofield", "Mariah Hoel",    
                            "Stefanie Buxel","Sydney Smith" ) ))
#view
obsvDet
#Now predict partial relationship between observer effects and detection
pred.det.obsv <- predict( fm.ttd, #fm.closed, 
                          type = "det", newdata = obsvDet, 
                          appendData = TRUE )
#view
pred.det.obsv
# Now plot observer effects:
obsvp.det <- pred.det.obsv %>%
  # define x and y values
  ggplot(., aes( y = obsv, x = Predicted, color = obsv ) ) + 
  #choose preset look
  theme_bw( base_size = 15 ) +
  #remove legend
  theme( legend.position = "none" ) +
  # add labels
  labs( y = "Observer", x = "Predicted detection" ) +
  #add mean detection for each observer
  geom_point( size = 4 ) +
  # add confidence intervals
  geom_errorbar( aes(xmin = lower, xmax = upper ), 
                 size = 1.5, width = 0.3 ) 
#view
obsvp.det

####### Now for day of year and detection 
# we use our data (unscaled) to extract the observed range of the predictor:
dayyr <- seq( min( ddf[,"jday"]),max( ddf[,"jday"]),
              by = 1 )
dateyr <- as.Date( dayyr, origin = "2025-01-01" )
dayDet <- data.frame( obsv = factor("Dylan Hendry",  
                                    levels = c("David Bontrager","Dylan Hendry",
                                               "Jessica Hovey", "Lillie Scofield", "Mariah Hoel",    
                                               "Stefanie Buxel","Sydney Smith" ) ), jday = dayyr )

#view
head( dayDet )
#can you see that we still need to provide all levels because it is a factor #
# even though only one is chosen as the output?

#predict partial relationship between sagebrush and detection probability 
# using the predict function
pred.det.day <- predict( fm.ttd, #fm.closed, 
                         type = "det", newdata = dayDet, 
                         appendData = TRUE )

# plot:
dayp <- cbind( pred.det.day[,c("Predicted", "lower", "upper") ], dateyr ) %>%
  # define x and y values
  ggplot(., aes( x = dateyr, y = Predicted ) ) + 
  #choose preset look
  theme_bw( base_size = 15 ) +
  # add labels
  labs( x = "Day of year", y = "Detection probability" ) +
  # add band of confidence intervals
  geom_smooth( aes(ymin = lower, ymax = upper ), 
               stat = "identity",
               size = 1.5, alpha = 0.5, color = "grey" ) +
  # add mean line on top
  geom_line( size = 2 ) 
#view
dayp


#### end detection relationships #############

###################extracting predicted values #############
ranef( fm.ttd)
#Estimated p for all observations
head(getP(fm.ttd))

#################################
