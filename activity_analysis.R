#########################################################################
###  This Rscript was started by Dr. Jen Cruz at Boise State University #
#     Here we visualize activity times for owls at the Grand Canyon     #
# We then analyze the overlap in their activity times.                  #
##########################################################################
##### Set up your workspace and load relevant packages -----------
# Clean your workspace to reset your R environment. #
rm( list = ls() )
# Check that you are in the right project folder (i.e. that you are in the 
# correct Rstudio project) #
getwd()

#install relevant packages
install.packages( "overlap" ) 

#load relevant packages
library( tidyverse ) #easy data manipulation
library( lubridate ) #easy date and time manipulation
library( overlap ) # to turn data into circular metrics
## end of package load ###############

###################################################################
#### Load or create data -----------------------------------------
#If you are in your correct Rstudio project then it should be:
getwd()
# set up your working directory:
workdir <- getwd()

# set your own datapath
datapath <- "C:/Users/jencruz/Google Drive/QCLabShared/Projects/REUs2021/database/"

#import records: calls file by pasting datapath to filename:
act_df <- read.csv( file = paste0( datapath,"activity_df.csv" ),
                     header = TRUE )

#######################################################################
######## explore data #############

#view to make sure that it looks correct
head( act_df ); dim( act_df )

# we start by viewing the data using a combination of piping and ggplot:
act_df %>% 
  ggplot( ., aes( x = aftersun_h ) ) +
  labs( x = "hours after sunset", y = "counts" ) +
  theme_bw() +
  geom_histogram() +
  facet_wrap( ~record, nrow = 5 )

#do males and females call at different times?
act_df %>% filter( record == "STOC" ) %>% 
  ggplot(.) +
  theme_bw() +
  geom_histogram( aes( x = aftersun_h, fill = sex )) + 
  facet_wrap( ~call_type )
#what time was sunset on the days we recorded owls:
hour( owl_df$sunset )
### end descriptive viewing ---------------
######### activity analyses ###################################
#turn start_hour to proportion of day
act_df$start_hour / 24

#convert hours after sun to radians
act_df$aftersun_rad <- 
ifelse( act_df$aftersun_h < 0, 
        (24 + act_df$aftersun_h)/24, 
        act_df$aftersun_h / 24 ) *2 * pi

#check
head( act_df )
#fit kernel density for each species
#extract owl species that we want to use
#here we start with all, but we can modify this object to a subset
spp <- unique( act_df$record )
# Write labels you want to use for the owls
spplabs <- c( "Mexican Spotted Owl", "Great Horned Owl", "Flammulated Owl",
              "Northern Saw-whet Pwl", "Western Screech Owl")
#create a loop that estimates and plots density for each
#set plotting parameters for base plot
par( mfrow = c(1,1), cex = 1.7, lwd = 2, bty = "l", pty = "m" )
for( i in 1:length(spp)){
  a <- act_df %>% filter( record == spp[i] ) %>% pull( aftersun_rad )
    b <- densityPlot( a, rug = TRUE, main = spplabs[i], xlab = "Time after sunset" )
    print( b )
}

# We can now calculate overlap between species of interest
# Demonstrate with mexican and flammulated
#create vectors that contain activity hours (in radians) for each species
stoc_rad <-  act_df %>% filter( record == "STOC" ) %>% pull( aftersun_rad )
otfl_rad <-  act_df %>% filter( record == "OTFL" ) %>% pull( aftersun_rad )

#create overlap plot
par( mfrow = c(1,1), cex = 1.7, lwd = 2, bty = "l", pty = "m" )
overlapPlot( stoc_rad, otfl_rad, main = "Vocal activity overlap", lty =c(1,20),
             col = c(1,4),xlab = "Time after sunset" )
legend( "top", spplabs[c(1,3)], lty =c(1,20),
        col = c(1,4), bty = "n" )

#what is the percentage overlap?
overlapEst( stoc_rad, otfl_rad, type = "Dhat1" )
# We use Dhat1 because of our smaller sample size. For details see:
# https://cran.r-project.org/web/packages/overlap/vignettes/overlap.pdf


#Create dataframe of aftersun hours that we want density estimates for
stoc_fit <- data.frame( hrs_aftersun = 0:23, rad_aftersun = ((0:23)/24)*2*pi ) 
#Extract density fit for Mexican spotted owls for use in detection models
stoc_fit$dens <- round( densityFit( stoc_rad, stoc_fit$rad_aftersun, bw = 50 ), 3)
#view
stoc_fit
#############################################################################
# Saving relevant objects and data ---------------------------------

# save vocal activity density estimates for STOCS:
write.csv(x = stoc_fit, 
          #ensure that you save it onto your datafolder
          file = paste0( datapath, 'stoc_activitydens.csv'), 
          row.names = FALSE )

# save workspace in case we want different plots
save.image( "ActivityResults" )
############### END OF SCRIPT ########################################