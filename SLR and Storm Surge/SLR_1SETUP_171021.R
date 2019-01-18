setwd("//main.sefs.uw.edu/main/Space/Lawler/Shared/Julia/NPVuln")
#

# 
# FIXME:
# In all cases, use st_transform for parks to SLW/low CRS, not vice versa >:p


# install.packages("sf")
# install.packages("rgdal") 
# install.packages("rgeos")
# install.packages("raster")
# install.packages("tidyverse")
# install.packages("dplyr")
# install.packages("mapview")
# install.packages("geosphere")

library(sf)
library(rgdal)
library(rgeos)
library(raster)
library(tidyverse)
library(dplyr)
library(mapview)
library(geosphere)

# Load SLR & Surge .gdbs
slr.db <- "//main.sefs.uw.edu/main/Space/Lawler/Shared/Julia/NPVuln/GIS/NPS/SLR/SLR_vs2.gdb"
surge.db <- "//main.sefs.uw.edu/main/Space/Lawler/Shared/Julia/NPVuln/GIS/NPS/StormSurge/Surge_vs2.gdb"

###########################
# Process park shapefiles #
###########################

# GOAL: extract (or reference) individual park boundaries from shapefile below.
parks <- st_read(dsn = slr.db, layer = "NPS_park_boundary_coast") %>%
  st_buffer(dist = 0) # fix invalid geometries (warning re: lat/long vs. dd)
class(parks) # sf: simple feature; info as data.frame
# 118 data observations at 115 levels (i.e., UNIT_CODE) with 118 obs; 23 variables
parks <- parks[,-(2:19)] # Drops all variables except FileName, shape info.

# 3 UNIT_CODES are for both park & associated reserve (GLBA, KATM, WRST)
# Dissolve those 3x2 instances into 3x1 obs --> 115 obs
parks <- parks %>%
  dplyr::group_by(UNIT_CODE) %>%  
  dplyr::summarise() # tried summarise(AREA = sum(Shape_Area)) but later got carried as num. not real area. 
# Dunno why, but parks is now sfc_GEOMETRY not MULTIPOLYGON. 
parks <- st_cast(parks) # Re-convert to MULTIPOLYGON, else probs with join

# Load in look-up table for SLR domains representing multiple parks (UNIT_CODE)...
# ...& YES/NO for precise to actual projection layers. Src: NPS file 0Readme_Domains_vs2.xlsx
lu.domains <- read.csv("//main.sefs.uw.edu/main/Space/Lawler/Shared/Julia/NPVuln/GIS/NPS/SLR/lu_unit_domains.csv")
parks <- left_join(parks, lu.domains, by = "UNIT_CODE") # now 115 obs, 4 variables

# Check for mis-matches or empty domains.
apply(is.na(parks),2,sum) # Shows 2 domains are NA
is.na(parks$DOMAIN) # shows rows 29, 75
parks[c(29,75),]
parks$DOMAIN[29] <- "EBLA" # Weirdly got dropped; EBLA=EBLA in orig NPS lookup
parks$READY[29] <- "YES" # There's only 1 projection for EBLA -- good to go in loop.
parks$DOMAIN[75] <- "NCAP" # NACC in DC so assigning to NCAP; not in orig NPS lookup
parks$READY[75] <- "NO" # Multiple projections for NCAP -- not good to go in loop.
apply(is.na(parks),2,sum) # Clear!

# Keep df of parks around
df.parks <- parks ; st_geometry(df.parks) <- NULL

##########################
# CREATE REFERENCE LISTS #
##########################

# Pull names from SLR .gdb
layer.list <- ogrListLayers(slr.db) # N.b., this is a vector, not R-style list

# Won't be using any RCP 4.5 projections
layers.rcp45 <- grep(pattern = "45", x = layer.list) # Not having value=TRUE just gives index
layer.list <- layer.list[-layers.rcp45] %>% sort(.)
remove(layers.rcp45)

# Make vectors of each type of projection (value=TRUE gives 'em); sort so names line-up
layers.MHHW <- grep(pattern = "MHHW", x = layer.list, value = TRUE) %>% sort(.)
layers.slr.MHHW <- grep(pattern = "slr", x = layers.MHHW, value = TRUE) %>% sort(.)
layers.low.MHHW <- grep(pattern = "low", x = layers.MHHW, value = TRUE) %>% sort(.)
layers.85 <- grep(pattern = "85", x = layer.list, value = TRUE) %>% sort(.)
layers.85.2050 <- grep(pattern="2050", x=layers.85, value=TRUE) %>% sort(.)
layers.slr.85.2050 <- grep(pattern = "slr", x = layers.85.2050, value = TRUE) %>% sort(.)
layers.low.85.2050 <- grep(pattern = "low", x = layers.85.2050, value = TRUE) %>% sort(.)
layers.85.2100 <- grep(pattern="2100", x=layers.85, value=TRUE) %>% sort(.)
# layers.slr.85.2100 <- grep(pattern = "slr", x = layers.85.2100, value = TRUE) %>% sort(.)
# layers.low.85.2100 <- grep(pattern = "low", x = layers.85.2100, value = TRUE) %>% sort(.)
remove(layers.MHHW, layers.85, layers.85.2050, layers.85.2100)