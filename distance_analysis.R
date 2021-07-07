#########################################################################
###  This Rscript was started by Dr. Jen Cruz at Boise State University #
# It processes and analyses data from the distance trials we performed #
# at the Grand Canyon, looking at the effects of distance, recorder ID #
# and habitat type on our ability to hear sound on a given recorder #
# data was logged into survey123                                      #
########################################################################

##### Set up your workspace and load relevant packages -----------
# Clean your workspace to reset your R environment. #
rm( list = ls() )
# Check that you are in the right project folder (i.e. that you are in the 
# correct Rstudio project) #
getwd()

#install packages
install.packages( "unmarked" ) #distance sampling and other population modeling

#load packages
library( tidyverse )
library( unmarked )
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
raw_df <- read.csv( file = paste0( datapath,"GC_distance_trials.csv" ),
                     header = TRUE )

#######################################################################
######## ready data for analysis #############
# check out raw dataframe from survey123
tail( raw_df ); dim( raw_df )
#get column names for the distance columns
distcols <- grep( "X", colnames(raw_df),value = TRUE )
# create distance dataframe by removing extra columns
dist_df <- raw_df %>%  
  select( Date.Time, Habitat.type, Recorder.ID, Trial.ID, all_of(distcols) )

#check 
head( dist_df ); dim( dist_df)
# check that recorder IDs were entered correctly
unique( dist_df$Recorder.ID )
# two version of MSO39. Fix:
dist_df$Recorder.ID[which(dist_df$Recorder.ID == "MSO039")] <- "MSO39"

#create maxdist column, which we will modify in a loop
dist_df$maxdist <- distcols[1]
#loop over distance columns for each row, replacing Yes/no wiht 1/0 and
#max dist with maximum distance where detection was YES
for( i in 1:dim(dist_df)[1]){
  dist_df$maxdist[i]<- last(distcols[ grep( 'YES', dist_df[i,distcols] ) ]) 
  dist_df[i,distcols] <- ifelse( dist_df[i,distcols] == "YES", 1, 0 )
}
#check 
head( dist_df ); dim( dist_df)
#now turn max distance into numeric
dist_df$maxdist <- parse_number( dist_df$maxdist)

#plot raw data
hist( dist_df$maxdist )

## prep dataframe for distance analysis
#extract detection dataframe into matrix
ydat <- dist_df[,distcols]
#turn into numeric values
ydat <- apply(ydat, 2, FUN = as.numeric )
#count records per distance
colSums(ydat)
#remove column names
colnames(ydat) <- NULL
#convert to matrix
ydat <- ydat %>% as.matrix() 
#view
ydat[1:5,]
class(ydat )

#now extract site level predictors 
covs <- dist_df %>% select( Habitat.type, Recorder.ID ) %>% 
      mutate( Habitat.type = as.factor(Habitat.type),
              Recorder.ID = as.factor(Recorder.ID) )
covs

#define transect lengths
tlengths <- rep( parse_number(last(distcols)), dim(ydat)[1])
#define distance breaks
brks <- parse_number( distcols ) 
brks <- c( brks[1],brks[1:length(brks)] + 2.5)
#define unmarked dataframe
umf <- unmarkedFrameDS( y = as.matrix(ydat), siteCovs = covs, survey = "line",
          dist.breaks = brks, tlength = tlengths, unitsIn = "m" )

#view dataframe
summary( umf )
#run model
m1 <- distsamp( ~ 1 + Habitat.type + Recorder.ID ~1, 
                keyfun = "halfnorm", data = umf )

m1

############ results --------------------------------------
# We start by estimating differences between recorders while keeping #
# the habitat consistent. We choose Burnt for now as it was the habitat #
# chosen as an intercept #
recDet <- data.frame( Recorder.ID = unique(covs$Recorder.ID),
                      Habitat.type = "Burnt" )
#view
recDet
#predict partial relationship between recorder effects and detection
pred.det.rec <- predict( m1, type = "det", newdata = recDet, 
                          appendData = TRUE )

# Plot:
pred.det.rec %>%
  # define x and y values
  ggplot(., aes( x = Recorder.ID, y = Predicted, color = Recorder.ID ) ) + 
  #choose preset look
  theme_bw( base_size = 15 ) +
  #remove legend
  theme( legend.position = "none" ) +
  # add labels
  labs( x = "Recorder ID", y = "Half normal scale parameter" ) +
  #add mean detection for each recorder
  geom_point( size = 4 ) +
  # add confidence intervals
  geom_errorbar( aes(ymin = lower, ymax = upper ), 
                 size = 1.5, width = 0.3 ) 

for( i in 1:dim(pred.det.rec)[1]){
  drange <- 0:150
  mdet <- gxhn( x = drange, sigma = pred.det.rec$Predicted[i] )
  ldet <- gxhn( x = drange, sigma = pred.det.rec$lower[i] )
  hdet <- gxhn( x = drange, sigma = pred.det.rec$upper[i] )
  a <- data.frame( drange, mdet, ldet, hdet, 
                   Recorder = pred.det.rec$Recorder.ID[i])
  ifelse( i == 1,
          recdf <<- a,
          recdf <- bind_rows( recdf, a) )
}
#check
head( recdf )
#plot all 
recdf %>%  ggplot(., aes( x = drange, y = mdet, color = Recorder ) ) + 
  #choose preset look
  theme_bw( base_size = 15 ) + theme( legend.position = "top" ) +
  # add labels
  labs( x = "Distance (m)", y = "Predicted detection" ) +
  # add band of confidence intervals
  geom_smooth( aes(ymin = ldet, ymax = hdet, fill = Recorder ), 
               stat = "identity",
               size = 1.5, alpha = 0.2 ) +
  # add mean line on top
  geom_line( size = 2 ) 
####----------------------------------------
# Repeat for habitat type, keeping recorder consistent. We stick to a 
# good recorder for now, but can also produce another with a poor one.
habDet <- data.frame( Recorder.ID = "MSO39",
                      Habitat.type = unique(covs$Habitat.type) )
#view
habDet
#predict partial relationship between recorder effects and detection
pred.det.hab <- predict( m1, type = "det", newdata = habDet, 
                          appendData = TRUE )

# Plot:
pred.det.hab %>%
  # define x and y values
  ggplot(., aes( x = Habitat.type, y = Predicted, color = Habitat.type ) ) + 
  #choose preset look
  theme_bw( base_size = 15 ) +
  #remove legend
  theme( legend.position = "none" ) +
  # add labels
  labs( x = "Habitat type", y = "Half normal scale parameter (m)" ) +
  #add mean detection for each recorder
  geom_point( size = 4 ) +
  # add confidence intervals
  geom_errorbar( aes(ymin = lower, ymax = upper ), 
                 size = 1.5, width = 0.3 ) 

# We now use the sigma estimates to plot the probability detection function
# We loop through each habitat to estimate the detection function
for( i in 1:dim(pred.det.hab)[1]){
  drange <- 0:150
  mdet <- gxhn( x = drange, sigma = pred.det.hab$Predicted[i] )
  ldet <- gxhn( x = drange, sigma = pred.det.hab$lower[i] )
  hdet <- gxhn( x = drange, sigma = pred.det.hab$upper[i] )
  a <- data.frame( drange, mdet, ldet, hdet, 
                  habitat = pred.det.hab$Habitat.type[i])
  ifelse( i == 1,
        habdf <<- a,
        habdf <- bind_rows( habdf, a) )
}
#check
head( habdf )
#plot all 
habdf %>%  ggplot(., aes( x = drange, y = mdet, color = habitat ) ) + 
  #choose preset look
  theme_bw( base_size = 15 ) + theme( legend.position = "top" ) +
  # add labels
  labs( x = "Distance (m)", y = "Predicted detection" ) +
  # add band of confidence intervals
  geom_smooth( aes(ymin = ldet, ymax = hdet, fill = habitat ), 
               stat = "identity",
               size = 1.5, alpha = 0.2 ) +
  # add mean line on top
  geom_line( size = 2 ) 

#############################################################################
# Saving relevant objects and data ---------------------------------
#save workspace in case we need to make changes
save.image( "DistanceResultsWorkspace.RData" )

############### END OF SCRIPT ########################################