#########################################################################
###  This Rscript was started by Dr. Jen Cruz at Boise State University #
# It estimates factors that influence detection of Mexican spotted owls #
# surveyed using sound recorders at historical territories within the   #
# Grand canyon.                                                        #
# Predictors include weather data downloaded from NOAA as hourly records #
# from 01-Jan-2019 to 30-Jun-2021.                                      # 
# Also time of day and hours after sunset                                #
##########################################################################

##### Set up your workspace and load relevant packages -----------
# Clean your workspace to reset your R environment. #
rm( list = ls() )
# Check that you are in the right project folder (i.e. that you are in the 
# correct Rstudio project) #
getwd()

#install packages
install.packages( "lmerTest" )
install.packages( "visreg" )
install.packages( "pbkrtest" )
install.packages( "MuMIn")
install.packages( "DHARMa")
install.packages( "gamm4" )
#load packages
library( tidyverse )
library( lmerTest ) #allows fitting of mixed effect models for better diagnostics
library( visreg ) #plotting of mixed effect models
library( pbkrtest ) #to estimate better p-values for mixed-effects models
library( MuMIn ) # for model evaluation of mixed-effects models 
library( DHARMa ) #diagnostics for residuals in mixed-effect models
library(gamm4)
# set option to see all columns and more than 10 rows
options( dplyr.width = Inf, dplyr.print_min = 100 )
## end of package load ###############

###################################################################
#### Load or create data -----------------------------------------
#If you are in your correct Rstudio project then it should be:
workdir <- getwd()

# set your own datapath
datapath <- "C:/Users/jencruz/Google Drive/QCLabShared/Projects/REUs2021/database/"

#import records file:
stoc_df <- read.csv( file = paste0( datapath,"stoc_det_df.csv" ),
                    header = TRUE )

#######################################################################
######## ready data for analysis #############
tail( stoc_df,20 ); dim( stoc_df )

# Before any formal analysis we need to check our data for outliers, 
# colinearity among predictors, sample size etc.

# We start by checking for outliers, skewed distribution etc #
# create a vector with predictor names
prednames <- c("jday", "aftersun_h", "HourlyWindSpeed",
               "HourlyTemp", "HourlyRain" )
# loop over each to create histograms for each predictor:
for( p in 1:length(prednames) ){
  # create an object with the ggplot so that you can display it 
  # in a loop 
  a <- ggplot( stoc_df ) + #choose your data
    theme_bw( base_size = 15 ) + #choose a preset theme
    labs( x = prednames[p] ) + #label x axis using our predictor names
    geom_histogram( aes( get(prednames[p]) ), bins = 10 ) #plot histogram
  # display your plot object
  print( a )
}
# What do you note? Any apparent issues with these data?
# Answer:
#
# Let's plot how predictors vary annually:
for( p in 1:length(prednames) ){
  # We can also incorporate site variability for habitat:
  bp<- ggplot( stoc_df ) +
    theme_bw( base_size = 15 ) + #choose a preset theme
    theme( legend.position = "none" ) + #remove legend display
    labs( y = prednames[p] ) + #label x axis using our predictor names
    # plot each site individually
    geom_point( aes( x = jday, y = get(prednames[p]),
                    color = as.factor( stoc ) ), size = 1.5 )
  
  # Here we rely on a smoothing spline to get mean trends #
  # across all sites:
  cp<- ggplot( stoc_df, aes( x = jday, y = get(prednames[p]) ) ) +
    theme_bw( base_size = 15 ) + #choose a preset theme
    labs( y = prednames[p] ) + #label x axis using our predictor names
    geom_smooth( size = 2 ) #smooth mean 
  #display plots
  print( bp )
  print( cp )
}
 
# Now that we are happy with no outliers, we can check for #
# correlation among predictors. Why is this important?
cor( stoc_df[ , prednames] )
# Nothing about 0.7 so we get ready for our actual analysis

# We standardise predictors so that we can compare effect sizes
sc_df <- stoc_df
sclpreds <- c( "jday", "aftersun_h", "HourlyWindSpeed", "HourlyTemp", "HourlyRain")
for( i in 1:length(sclpreds ) ){
  sc_df[,sclpreds[i]]  <- scale( sc_df[ ,sclpreds[i] ] )
}
head( sc_df )
#Why do we scale predictors?

############ end of prelim analysis ----------------------------
###########################################################################
######### running logistic regression models for stoc detection #########

# We start by running a full model including all fixed effects of interest #
# as well as a random intercept for survey 
m1 <- glmer( stoc ~ aftersun_h + 
               HourlyWindSpeed + HourlyTemp + HourlyRain +
               (1|jday), family = binomial,
             data = sc_df )
#view results
summary( m1 )
#What do the random intercepts account for?
nullm <- glmer( stoc ~ (1|jday), family = binomial,
                data = sc_df )
# We can also calculate marginal R^2 for random effects only (R2m) and the #
#  whole model including fixed and random (R2c) using the MuMIn package:
MuMIn::r.squaredGLMM( m1, nullm )
# How do we interpret these results?
# Tip: think about how much the R2c and R2m differ, and how much of that #
# is due to the fixed effects.

### model validation
model_simres <- simulateResiduals( m1 )
#you can plot the results:
plot( model_simres )
# What do these diagnostics tell us?
# Answer:
#

#### view partial relationships between response and predictors:
visreg( #which model do we want to plot results for?
    fit =  m1,  
    #transform y from logit back to probability
    scale = "response",
    ylab = "Detection probability" 
        )
# Note the different limits on the y axis. Which effect was strongest?

# if we want to stick to visreg, we could rerun the model with the unscaled 
# values and use those results for plotting
m2 <- glmer( stoc ~ aftersun_h + 
               HourlyWindSpeed + HourlyTemp + HourlyRain +
               (1|jday), family = binomial,
             data = stoc_df )
#check that we got the same p values
summary( m2 )
#plot all to check
visreg( fit =  m2,   
  #transform y from logit back to probability
  scale = "response", ylab = "Detection probability" 
)
#plot only the important variables
#readjust number of panels and increase font size
par( mfrow = c(1,1), cex = 1.7)
visreg( fit = m2, scale = "response", xvar = "aftersun_h",
        ylab = "Detection probability", xlab = "Hours after sun")

#############################################################################
# Saving relevant objects and data ---------------------------------
#save workspace in case we need to make changes
save.image( "STOCDetResultsWorkspace" )

############### END OF SCRIPT ########################################