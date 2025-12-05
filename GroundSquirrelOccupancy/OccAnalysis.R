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

# #import cleaned data formatted as wide 
# widedf <- read.csv( "GroundSquirrelOccupancy/WideData.csv", header = TRUE)
# 
widedf <- read.csv( "GroundSquirrelOccupancy/CombData.csv", header = TRUE)

# #import cleaned long format data for plotting
# ddf <- read.csv( "GroundSquirrelOccupancy/CleanData.csv", header = TRUE)

#view
head( widedf ); dim( widedf ) 
#### End of data load -------------
####################################################################
########### standardising functions  #################
#genetic function to standardise covariates using Gelman's suggestion:
standardise <- function( xmat, stdevs = 2, marg = c( 1, 2, 3 ) ) { 
  mean.xmat = mean( as.vector( xmat ), na.rm = TRUE )
  sd.xmat = sd( as.vector( xmat ), na.rm = TRUE ) 
  std.xmat = apply( xmat, marg, function( x ){
    ( x - mean.xmat ) / (stdevs * sd.xmat ) } )
  return( std.xmat )
}
##############################################################
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

#calculate raw occupancy
table(rowSums( widedf[ ,detidx], na.rm = TRUE))
68/(68+39+6) #0.6

#check soil
table(widedf$soilclass)
#combine complex and other
widedf$soilclass[widedf$soilclass == "other"] <- "complex"

# Let's define our unmarked dataframe:
# Start by defining which columns represent the response (observed occurrences)
umf <- unmarkedFrameOccu( y = as.matrix( widedf[ ,detidx]),
                          # Define predictors at the site level:
              siteCovs = widedf[ ,c("perennial_2024", "shrub_2024",
                                    "soilclass", "soiltype") ],
                          # Define predictors at the survey level as a list:
                          obsCovs = list( obsv = widedf[ ,obsidx],
                                          jday = widedf[ ,jidx] ) ) 
# View summary of unmarked dataframe:
summary( umf )
### standardize predictors
scshrub <- standardise( as.matrix(siteCovs(umf)[,"shrub_2024"]), stdevs = 2, marg = 2 )
scperen <- standardise( as.matrix(siteCovs(umf)[,"perennial_2024"]), stdevs = 2, marg = 2 )
# We replace the predictors in our unmarked dataframe with the scaled values:
siteCovs( umf )[,c("shrub_2024","perennial_2024") ] <- cbind(scshrub, scperen)

scday <- standardise( as.matrix( obsCovs(umf)[,c("jday") ] ), marg = 2 )
#replace with scaled values:
obsCovs(umf)[,"jday"] <- as.vector(scday)

#view
summary(umf)

#fix time to detection for now
Tmax <- 5
yy <- as.matrix( widedf[ ,timeidx])
# yy[ yy > Tmax ] <- Tmax
# yy[yy < 0] <- Tmax
# #view
# head(yy)
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
                   ~ 1 + soilclass + shrub_2024 + perennial_2024, 
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
  ttdDist = "exp",
                   data = umf.ttd )

fm.ttd

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
##################################### save #################################
#save workspace to continue working from where we left off
save.image("OccResults.RData" )
############################ end of script ####################################


##################### end of script #######################################