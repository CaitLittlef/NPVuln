setwd("//main.sefs.uw.edu/main/Space/Lawler/Shared/Julia/NPVuln/Indicator_Dir")

root <- "C:"
droughtDir <- "//main.sefs.uw.edu/main/Space/Lawler/Shared/Julia/NPVuln/Indicator_Dir/Inputs/Drought"

library(readr)
#library(plyr) # n.b., some functions (e.g., rename) operate diff with plyr & dplyr
library(dplyr)
library(tidyr)

# SPEI data is list with 18 elements, read in from .mat file
install.packages("R.matlab")
library(R.matlab)
drought <- readMat("Inputs/Drought/DroughtChange_SPEI_CONUS.mat")

# extract lat and long lists; calling list member by name "Lat" and "Lon"
# longitude is still structured as 241 obs of 1 variable.
# But I want to use long as column names, therefore transpose it.
lat <- as.data.frame(drought["Lat"])
# lat <- as.matrix(drought["Lat"]) just gives list
lat <- as.matrix(lat)
long <- as.data.frame(drought["Lon"])
long.t <- (t(long)) # transpose to get 241 columns


# Create nested list of only the drought data (not lat or long).
drought_data <- drought[1:16]

# Want to create dataframes of each drought list and call it by name.
# ASSIGN enables siultaneous creation and assignment of objects
for (i in c(5:8)){ # only calling rcp 8.5 and thru 2050s
  assign(names(drought_data)[i], as.data.frame(drought_data[i]))
}

### ASSIGNING LAT & LONG AS ROWS & COLUMNS

rownames(DrghtChnge.rcp85.2006.2055.spring) <- lat[,1] # prob unecessary
rownames(DrghtChnge.rcp85.2006.2055.summer) <- lat[,1] # prob unecessary
rownames(DrghtChnge.rcp85.2006.2055.fall)   <- lat[,1] # prob unecessary
rownames(DrghtChnge.rcp85.2006.2055.winter) <- lat[,1] # prob unecessary

colnames(DrghtChnge.rcp85.2006.2055.spring) <- long.t
colnames(DrghtChnge.rcp85.2006.2055.summer) <- long.t
colnames(DrghtChnge.rcp85.2006.2055.fall)   <- long.t
colnames(DrghtChnge.rcp85.2006.2055.winter) <- long.t


##### LOOP: CHANGE SEASONS IN 1st TEMP, SPEI_all, & CSV NAME!! #####
# Instead of having each long value
# as a variable name in its own column,
# tidy all long values under single variable.
temp <- DrghtChnge.rcp85.2006.2055.winter #!!!!#
# Need to specify which columsn to gather under one variable.
# In this case, I want ALL columns into single variable.
long.values <- colnames(temp) # keeps as characters vs. long.t is num.
# To keep lat as variable, bind to dataset and gather only long.values
temp <- cbind(temp,lat)
temp <- temp %>%
  gather((paste(long.values)), key = "Long", value = "SPEI.chng")
# Long values aren't numeric
temp$Long <- as.numeric(temp$Long)
# Coerce into western hemisphere with negative
temp <- temp %>% mutate(Long.neg = Long*-1) %>%
  dplyr::select(Lat, Long.neg, SPEI.chng)

# Iteratively activate so that I can #!!!!#
# SPEI.spr <- temp$SPEI.chng
# SPEI.sum <- temp$SPEI.chng
# SPEI.fal <- temp$SPEI.chng
SPEI.win <- temp$SPEI.chng


# For writing each individual .csv
# write.csv(temp,
#           file = paste(droughtDir,
#                        "\\SPEI_chng_rcp85_2055_winter.csv", #!!!!#
#                        sep=""))


#After working thru each seasons, create all table.
SPEI.all <- temp %>%
  mutate(SPEI.spr,
         SPEI.sum,
         SPEI.fal,
         SPEI.win) %>%
  dplyr::select(Lat, Long.neg, SPEI.spr,
                SPEI.sum, SPEI.fal, SPEI.win)


write.csv(SPEI.all,
          file = paste(droughtDir,
                       "\\SPEI_chng_rcp85_2055_all.csv",
                       sep=""))





### Turn into XY points
# SPEI.chng values won't be numeric in Arc due to NaN (not a number)
# So, Add XY .csv, which makes it an "XY event source".
# Export dataset as .shp and reload. Add field.
# Compute new SPEI_chng value as numeric.

### Rasterize
# Use Arc tool Point --> Raster. Can do as batch. 
# Setting cell size at to 0.25 decimal degrees gives 1 pixel for each point.
# (Determined after experimenting to too fine and too coarse.)
# Note that default raster value is NOT drought but FID.
# Add new field to set the REAL values (SPEI_CHNG) to numeric (SPEI_CHNG_NUM).
# Then, to make sure raster value field reflects real drougth value,
# use Spatial Analyst -> Reclass -> Lookup and new rastesr should be legit.


