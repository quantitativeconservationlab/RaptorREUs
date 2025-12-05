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

library( tidyverse )#includes dplyr, tidyr and ggplot2
options( dplyr.width = Inf, dplyr.print_min = 100 )

## end of package load ###############
###################################################################
#### Load or create data -----------------------------------------
# Clean your workspace to reset your R environment. #
rm( list = ls() )

#import cleaned data formatted as wide 
widedf <- read.csv( "GroundSquirrelOccupancy/WideData.csv", header = TRUE)

# #import cleaned long format data for plotting
# ddf <- read.csv( "GroundSquirrelOccupancy/CleanData.csv", header = TRUE)

soilraw <- read.csv( "GroundSquirrelOccupancy/soilmetrics.csv", header = TRUE,
                     row.names = NULL)

vegraw <- read.csv( "GroundSquirrelOccupancy/RAPallsites1986-2024_cover.csv", 
                    header = TRUE, row.names = NULL)
#### End of data load -------------
####################################################################
##### Ready data for analysis --------------
head(soilraw)
head( vegraw)
#prep veg data
vegdf <- vegraw 
#create a new object and keep only relevant columns:
vegdf <- vegdf %>% 
  dplyr::select( Site.ID = sitename, annual_2024, perennial_2024, shrub_2024  )

table(soilraw$soiltype)

#prep soils
soildf <- soilraw %>% 
  dplyr::select( Site.ID = SiteID, soilclass, soiltype, compname, compaction,
                 description)

#view
head(soildf)

#combine
preddf <- left_join(soildf, vegdf, by = "Site.ID")

head(preddf)
#check correlation 
cor(preddf[,c("annual_2024", "perennial_2024", "shrub_2024")])

#add to datadf
head( widedf)

combdf <- left_join(widedf, preddf, by = "Site.ID" )
#view
head(combdf)
table( combdf$soiltype)
table(combdf$description)
##################################### save #################################
# clean long format data
write.csv( combdf, file = "GroundSquirrelOccupancy/CombData.csv",
           row.names = FALSE )

############################ end of script ####################################

