#######################################################################
#######################################################################
##     This script was created by Dr. Jen Cruz                       ##  
##                                                                   ##  
## Here we import our cleaned data for one season of our occurrence   ##
#  observations for Piute ground squirrels at the NCA and run a      ##
## closed population occupancy analysis. See Mackenzie et al. 2002   ##
## for details of the model. The occupancy model is hierarchical with #
# two components: (1) an ecological submodel linking occupancy to    ##
## environmental predictors at the site. (2) an observation submodel ##
## linking our detection probability to relevant predictors.         ##
##                                                                   ##
#######################################################################
##### Set up your workspace and load relevant packages -----------
# Clean your workspace to reset your R environment. #
rm( list = ls() )
# Check that you are in the right project folder
getwd()

# Install new packages from "CRAN" repository. # 
install.packages( "unmarked" ) #package for estimating occupancy, N-mixtures, 
#and some multinomial approaches for capture data
install.packages( "MuMIn") # package for model selection and evaluation
# load packages:
library( tidyverse )#includes dplyr, tidyr and ggplot2
library( unmarked ) #
library( MuMIn )
## end of package load ###############
###################################################################
#### Load or create data -----------------------------------------
# Clean your workspace to reset your R environment. #
rm( list = ls() )

#import cleaned data formatted as wide 
widedf <- read.csv( "GroundSquirrelOccupancy/WideData.csv", header = TRUE)

#import cleaned long format data for plotting
ddf <- read.csv( "GroundSquirrelOccupancy/CleanData.csv", header = TRUE)

#view
head( widedf ); dim( widedf ) 
#### End of data load -------------
####################################################################
##### Ready data for analysis --------------

# the unmarked package has several functions to make data import easy
# We need to define which predictors we will link to which responses #
# We expect detection to be influenced by observer effects, time of day,
# day of year, type of detection #

# We expect occupancy to be influenced by soils and vegetation
#let's extract columns of interest
# detection 
detidx <- grep( "detection_", colnames(widedf), value = FALSE)
typeidx <- grep( "First", colnames(widedf), value = FALSE) 
obsidx <- grep( "Obs", colnames(widedf), value = FALSE)
timeidx <- grep( "time", colnames(widedf), value = FALSE)
jidx <- grep( "jday", colnames(widedf), value = FALSE)

# Let's define our unmarked dataframe:
# Start by defining which columns represent the response (observed occurrences)
umf <- unmarkedFrameOccu( y = as.matrix( widedf[ ,detidx]),
                          # Define predictors at the site level:
                          #siteCovs = closeddf[ ,c("sagebrush", "cheatgrass")],
                          # Define predictors at the survey level as a list:
                          obsCovs = list( obsv = widedf[ ,obsidx],
                                          jday = widedf[ ,jidx] ) ) 
# View summary of unmarked dataframe:
summary( umf )
#fix time to detection for now
Tmax <- 10
yy <- as.matrix( widedf[ ,timeidx])
yy[ yy > Tmax ] <- Tmax
yy[yy < 0] <- Tmax
#view
head(yy)
# Time to detection model
umf.ttd <- unmarkedFrameOccuTTD( y = yy ,
                          # Define predictors at the site level:
                          #siteCovs = closeddf[ ,c("sagebrush", "cheatgrass")],
                          # Define predictors at the survey level as a list:
                          obsCovs = list( obsv = widedf[ ,obsidx],
                                          jday = widedf[ ,jidx] ),
                          surveyLength = Tmax, numPrimary = 1 ) 
# View summary of unmarked dataframe:
summary( umf.ttd )

### Analyze data ------------------------------------------
# We are now ready to perform our analysis. Since the number of predictors #
# is reasonable for the sample size, and there were no issues with #
# correlation, we focus on a single full, additive model:
fm.closed <- occu( ~ 1 +  jday +  obsv #(1 | obsv)
                   ~ 1, 
                   data = umf )
# Note that we start with the observation submodel, linking it to the intercept # 
# day of year and  observer id as factors influencing detection.
# We then define the ecological submodel linking predictors to occupancy.

# View model results:
fm.closed

#compare to a model with no predictors
fm.null <- occu( ~ 1 
                  ~ 1, 
                 data = umf )

### time to detection analysis
fm.ttd <- occuTTD( psiformula = ~1,
  detformula = ~ 1 +  jday +  obsv, #(1 | obsv)
                   data = umf.ttd )

fm.ttd

#Estimated p for all observations
head(getP(fm.ttd))
##########################################################################
# Model fit and evaluation -----------------------------------------------

# We assess goodness of fit (GoF) on detection frequencies, which relies on a #
# Pearson chi-square to assess fit as suggested by Mackenzie and Bailey (2004) #
# J. Agr., Bio. & Env. Stats. 9: 300-318
# using AICmodavg package
gof.boot <- AICcmodavg::mb.gof.test( fm.closed, nsim = 1000 )
#view
gof.boot

# Note that values of c-hat > 1 indicate overdispersion (variance > mean), but #
# that values much higher than 1 (i.e., > 4) probably indicate lack-of-fit. #
# In cases of moderate overdispersion, one usually multiplies the #
# variance-covariance matrix of the estimates by c-hat inflating the SE’s of the#
# estimates (c-hat is also known as a variance inflation factor). #
# In cases of underdispersion (c-hat < 1), keep the value of c-hat to 1. #
# Note that values of c-hat << 1 can also indicate lack-of-fit. #

# Is our model over- or under-dispersed?
# Answer:


# We can also evaluate how well our full model did against the null model # 
# by estimating pseudo-R^2, based on Nagelkerke, N.J.D. (1991) A Note #
# on a General Definition of the Coefficient of Determination. Biometrika 78,#
# pp. 691-692.#
# (1 - R^2) represents the proportion of unexplained variation in the model
# We create a reduced model list with only our two models of interest:
rms <- fitList( 'psi(full)p(full)' = fm.closed,
                'psi(.)p(.)' = fm.null )
# Then use model selection function from unmarked but this time we define #
# which one our null model is:
unmarked::modSel(rms, nullmod = "psi(.)p(.)" )
# What does this tell us about the fit of our model?
# Answer:
#
######## end of model evaluation ##############
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

####### Now for day of year and detection  #######
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
pred.det.day <- predict( fm.closed, type = "det", newdata = dayDet, 
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
###################


##################### end of script #######################################