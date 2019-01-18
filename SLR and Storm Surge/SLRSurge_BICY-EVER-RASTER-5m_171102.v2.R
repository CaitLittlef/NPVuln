##############
### SET-UP ###
##############


setwd("//main.sefs.uw.edu/main/Space/Lawler/Shared/Julia/NPVuln")
#
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


# Rasters below are just HUGE and they run out of space in the default temp file folder.
# Set temp direcotry to new location
tmpDir(create = TRUE) # [1] "C:\\Users\\clittlef\\AppData\\Local\\Temp\\10\\RtmpUTJJ6d/raster/"
rasterOptions(tmpdir = "//main.sefs.uw.edu/main/Space/Lawler/Shared/Caitlin/temp_for_fat_files")
tmpDir(create = TRUE) # [1] "//main.sefs.uw.edu/main/Space/Lawler/Shared/Caitlin/temp_for_fat_files/"
rasterOptions(default=TRUE) # Doesn't change it back... FIXME

# Also got this error at one point: # Error: cannot allocate vector of size 5.3 Gb
memory.limit() # 262047 <- 262 GB? seems like plenty...

# Load SLR & Surge .gdbs
slr.db <- "//main.sefs.uw.edu/main/Space/Lawler/Shared/Julia/NPVuln/GIS/NPS/SLR/SLR_vs2.gdb"
surge.db <- "//main.sefs.uw.edu/main/Space/Lawler/Shared/Julia/NPVuln/GIS/NPS/StormSurge/Surge_vs2.gdb"

# Create ref raster for rasterizing. Using mins & maxes from prior look at bbox of slr/low layers.
# Rounding up to ensure ref.raster is big enough to capture all layers' extents.
# Can't just use park DEM b/c there are multiple DEMs (5m & 10m), covering diff areas
ref.ras <- raster(xmn = 437700, xmx = 571800,
                  ymn = 2732400, ymx = 2912500,
                  resolution = 5, # setting res means don't need nrows or ncol
                  crs = "+proj=utm +zone=17 +ellps=GRS80 +units=m +no_defs", 
                  vals = 1)



###################################
### BICY: RASTERIZE & PREP PARK ###
###################################

start <- Sys.time()

# Load shapefile with all parks
parks <- st_read(dsn = surge.db, layer = "NPS_park_boundary_coast") %>%
  st_buffer(dist = 0) # fix invalid geometries (warning re: lat/long vs. dd)
object.size(parks)

# Extract park I want
(park <- parks[which(df.parks$UNIT_CODE == "BICY"),]) # in lat/long

# Create reference dataframe but otherwise remove big park shapefile
df.parks <- parks ; st_geometry(df.parks) <- NULL
remove(parks)

# Set park CRS to match slr/low layers (i.e,. UTM17N) with EPSG code; rasterize
park.r <- park %>% st_transform(6346) %>% as("Spatial") %>% rasterize(ref.ras)
remove(park)

# Set land=1, water/non-land=0; else NA flunks raster calculations
park.r[is.na(park.r[])] <- 0

print(paste0("Done with rasterizing & prep."))
print(Sys.time() - start) 



#######################################
### RASTERIZE & PREP SLR/LOW LAYERS ###
#######################################

start <- Sys.time()

# Find names of SLR/Low layers
(slrMHHW.5m.name <- grep("EVER", layers.slr.MHHW, value = TRUE)[1]) 
(lowMHHW.5m.name <- grep("EVER", layers.low.MHHW, value = TRUE)[1]) 
(slr85.10m.name <- grep("EVER", layers.slr.85.2050, value = TRUE)[1])
(slr85.5m.name <- grep("EVER", layers.slr.85.2050, value = TRUE)[2])
(low85.10m.name <- grep("EVER", layers.low.85.2050, value = TRUE)[1])
(low85.5m.name <- grep("EVER", layers.low.85.2050, value = TRUE)[2]) 


# Load associated layers from geodatabase & rasterize; combining first halves computation time
slrMHHW.5m.r <- st_read(slr.db, slrMHHW.5m.name) %>%
  st_combine() %>% as("Spatial") %>% rasterize(ref.ras)
lowMHHW.5m.r <- st_read(slr.db, lowMHHW.5m.name) %>%
  st_combine() %>% as("Spatial") %>% rasterize(ref.ras)
slr85.10m.r <- st_read(slr.db, slr85.10m.name) %>%
  st_combine() %>% as("Spatial") %>% rasterize(ref.ras)
slr85.5m.r <- st_read(slr.db, slr85.5m.name) %>%
  st_combine() %>% as("Spatial") %>% rasterize(ref.ras)
low85.10m.r <- st_read(slr.db, low85.10m.name) %>%
  st_combine() %>% as("Spatial") %>% rasterize(ref.ras)
low85.5m.r <- st_read(slr.db, low85.5m.name) %>%
  st_combine() %>% as("Spatial") %>% rasterize(ref.ras)


# Clean up!
remove(slrMHHW.5m.name, lowMHHW.5m.name, slr85.10m.name,
       slr85.5m.name, low85.10m.name, low85.5m.name)
remove(ref.ras)

# In raster calculations, NA will lead to NA regardless of math.
# Use values of 0 & 1 and multiplication, not subtraction. 
# In projections, water becomes 0 & land (or non-water) becomes 1
# Reclassify requires matrix of: (from, to, becomes) # n.b., (1, 1, 0) won't work
rclmat <- c(0.5, 2, 0) %>% matrix(ncol=3, byrow=TRUE)
slrMHHW.5m.r <- reclassify(slrMHHW.5m.r, rclmat)
lowMHHW.5m.r <- reclassify(lowMHHW.5m.r, rclmat)
slr85.10m.r <- reclassify(slr85.10m.r, rclmat)
slr85.5m.r <- reclassify(slr85.5m.r, rclmat)
low85.10m.r <- reclassify(low85.10m.r, rclmat)
low85.5m.r <- reclassify(low85.5m.r, rclmat)

# Any NA flunks raster algebra b/c NA will return NA. Set this non-water to 1.
slrMHHW.5m.r[is.na(slrMHHW.5m.r[])] <- 1
lowMHHW.5m.r[is.na(lowMHHW.5m.r[])] <- 1
slr85.10m.r[is.na(slr85.10m.r[])] <- 1
slr85.5m.r[is.na(slr85.5m.r[])] <- 1
low85.10m.r[is.na(low85.10m.r[])] <- 1
low85.5m.r[is.na(low85.5m.r[])] <- 1

print(paste0("Done with rasterizing & prep."))
print(Sys.time() - start) # took 6 hrs.







########################################
### CREATE FUTURE PARK UNDER SLR/LOW ###
########################################

# Multiply MHHW (as zero) by park to create park at MHHW: land=1, non-land=0
# Overlay is far more efficient than raster math.
park.MHHW.r <- overlay(park.r, slrMHHW.5m.r, lowMHHW.5m.r,
                       fun=function(r1, r2, r3){return(r1*r2*r3)})

# Multiply slr/low (as zero) by park to create high-water park.
# Overlay is far more efficient than raster math.
park.85.2050.r <- overlay(park.MHHW.r, slr85.10m.r, slr85.5m.r, low85.10m.r, low85.5m.r,
                          fun=function(r1, r2, r3, r4, r5){return(r1*r2*r3*r4*r5)})










##########################################################
### COMPUTE & RECORD FUTURE AREA OF PARK UNDER SLR/LOW ###
##########################################################


# How many 5m pixels of value 1 are in each raster? Mult. by 25 to get area in m2
park.area.MHHW.m2 <- freq(park.MHHW.r, value = 1) * 5^2
park.area.85.2050.m2 <- freq(park.85.2050.r, value = 1) * 5^2

# What's ID associated with this park?
i <- grep("BICY", df.parks$UNIT_CODE)

# Fill lists with park code and areas (lists created in main script); 
list.park.name[[i]] <- df.parks$UNIT_CODE[i]
list.park.area[[i]] <- park.area.MHHW.m2
list.park.area.85.2050[[i]] <- park.area.85.2050.m2






###############################
### WRITE SLR RESULTS TABLE ###
###############################

# Set any NULLS in list to NA so they don't disappear
list.park.name <- sapply(list.park.name, function(x) ifelse(x == "NULL", NA, x))
list.park.area <- sapply(list.park.area, function(x) ifelse(x == "NULL", NA, x))
list.park.area.85.2050 <- lapply(list.park.area.85.2050, function(x) ifelse(x == "NULL", NA, x))

# Turn them into matrices; dunno what this does. t tranposes.
library(plyr)
mat1 <- (plyr::rbind.fill.matrix(lapply(list.park.name, t)))
mat2 <- (plyr::rbind.fill.matrix(lapply(list.park.area, t)))
mat3 <- (plyr::rbind.fill.matrix(lapply(list.park.area.85.2050, t)))

# Create dataframe and make sure the numbers are numeric
df.results <- data.frame(cbind(mat1, mat2, mat3)) ; remove(mat1, mat2, mat3)
df.results[,2] <- as.numeric(as.character(df.results[,2]))
df.results[,3] <- as.numeric(as.character(df.results[,3]))

# Calculate percent losses
df.results <- df.results %>%
  dplyr::rename_(UNIT_CODE = names(.)[1], # Dunno why rename_; but rename won't work
                 area.m2 = names(.)[2],
                 area.85.2050.m2 = names(.)[3]) %>%
  mutate(area.slr.loss.m2 = (area.m2 - area.85.2050.m2)) %>%
  mutate(area.slr.loss.perc = (area.slr.loss.m2 / area.m2))

# Write results to csv
currentDate <- Sys.Date()
csvFileName <- paste0("BICY_SLR_results_",currentDate,".csv")
write.csv(df.results, paste0("//main.sefs.uw.edu/main/Space/Lawler/Shared/Julia/NPVuln/GIS/NPS/SLR/SLR_Compute/",csvFileName))
remove(currentDate, csvFileName)

# Clean-up!
remove(park.MHHW.r, slr85.10m.r, slr85.5m.r, low85.10m.r, low85.5m.r)










######################################################
### WRITE SLR/LOW SHAPEFIES AT YOUR OWN RISK ###
######################################################

park.MHHW.r[park.MHHW.r == 0] <- NA # so ras->poly conversion only keeps park
park.85.2050.r[park.85.2050.r == 0] <- NA # so ras->poly conversion only keeps park

# Convert rasters to polygons & save; don't.
park.MHHW.poly <- park.MHHW.r %>%
  rasterToPolygons(dissolve=TRUE) %>% # only returns non-NA values
  as("sf") %>% # coerce to sf object from Spatial for st_write() below
  st_transfor(4269) # set back to crs to match all other parks
park.85.2050.poly <- park.85.2050.r %>%
  rasterToPolygons(dissolve=TRUE) %>% # only returns non-NA values
  as("sf") %>% # coerce to sf object from Spatial for st_write() below
  st_transfor(4269) # set back to crs to match all other parks

# Write shapefiles.
# N.b., if writeRaster, try ras <- as.integer(ras) b/c Arc won't play nice with flat/doubles
setwd("//main.sefs.uw.edu/main/Space/Lawler/Shared/Julia/NPVuln/GIS/NPS/SLR/SLR_Compute")
st_write(park.MHHW.poly, paste0(df.parks$UNIT_CODE[i],"_baseline.shp"))
st_write(park.85.2050.poly, paste0(df.parks$UNIT_CODE[i],"_slr.85.2050.shp"))
setwd("//main.sefs.uw.edu/main/Space/Lawler/Shared/Julia/NPVuln")

# Clean up!
remove(park.MHHW.r, park.MHHW.poly, park.85.2050.poly) # BUT KEEP park.85.2050.r FOR SURGE BELOW!!









START HERE



##########################
### BICY - STORM SURGE ###
##########################

surge.db <- "//main.sefs.uw.edu/main/Space/Lawler/Shared/Julia/NPVuln/GIS/NPS/StormSurge/Surge_vs2.gdb"

# Create reference raster for rasterizing surge layers -- these #s based on bbox of each.
ref.ras <- raster(xmn = 437700, xmx = 571800,
                  ymn = 2732400, ymx = 2912500,
                  resolution = 5, # setting res means don't need nrows or ncol
                  crs = "+proj=utm +zone=17 +ellps=GRS80 +units=m +no_defs", 
                  vals = 1)
# For park & projections, 0=water, 1=land (b/c NA --> NA); here's reclass matrix:
rclmat <- c(0.5, 2, 0) %>% matrix(ncol=3, byrow=TRUE)

# Do one surge layer at a time else no memory

# Surge1
# Read & rasterize surge layer in surge layers
surge1.r <- st_read(surge.db, "BICY10m_low_85_2050_C5H") %>%
  st_combine() %>% as("Spatial") %>% rasterize(ref.ras)
# For park & projections, 0=water, 1=land (b/c NA --> NA)
rclmat <- c(0.5, 2, 0) %>% matrix(ncol=3, byrow=TRUE) # Sets 1s in water to 0
surge1.r <- reclassify(surge1.r, rclmat)
# Any NA flunks the raster algebra below -- NA will return NA. Set this non-water to 1.
surge1.r[is.na(surge1.r[])] <- 1 # Sets NAs in slr/low to 1 (for land)
park.85.2050.r[is.na(park.85.2050.r[])] <- 0  # Sets NAs in park to 0 (for water/non-park)
# Multiply MHHW (as zero) by park to create park at MHHW: land=1, non-land=0
# Overlay is far more efficient than raster math.
park.85.2050.surge.r <- overlay(park.85.2050.r, surge1.r, # note this is baseline park
                                fun=function(r1, r2,){return(r1*r2)})
remove(surge1.r)

# Surge2
surge2.r <- st_read(surge.db, "BICY10m_slr_85_2050_C5H") %>%
  st_combine() %>% as("Spatial") %>% rasterize(ref.ras)
rclmat <- c(0.5, 2, 0) %>% matrix(ncol=3, byrow=TRUE)
surge2.r <- reclassify(surge2.r, rclmat)
surge2.r[is.na(surge2.r[])] <- 1 
park.85.2050.r[is.na(park.85.2050.r[])] <- 0
park.85.2050.surge.r <- overlay(park.85.2050.surge.r, surge2.r, # building off last
                                fun=function(r1, r2,){return(r1*r2)})
remove(surge2.r)

# Surge3
surge3.r <- st_read(surge.db, "BICY5m_low_85_2050_C5H") %>%
  st_combine() %>% as("Spatial") %>% rasterize(ref.ras)
rclmat <- c(0.5, 2, 0) %>% matrix(ncol=3, byrow=TRUE)
surge3.r <- reclassify(surge3.r, rclmat)
surge3.r[is.na(surge3.r[])] <- 1
park.85.2050.r[is.na(park.85.2050.r[])] <- 0
park.85.2050.surge.r <- overlay(park.85.2050.surge.r, surge3.r, # building off last
                                fun=function(r1, r2,){return(r1*r2)})
remove(surge3.r)

# Surge4
surge4.r <- st_read(surge.db, "BICY5m_slr_85_2050_C5H") %>%
  st_combine() %>% as("Spatial") %>% rasterize(ref.ras)
rclmat <- c(0.5, 2, 0) %>% matrix(ncol=3, byrow=TRUE)
surge4.r <- reclassify(surge4.r, rclmat)
surge4.r[is.na(surge4.r[])] <- 1
park.85.2050.r[is.na(park.85.2050.r[])] <- 0 
park.85.2050.surge.r <- overlay(park.85.2050.surge.r, surge4.r, # building off last
                                fun=function(r1, r2,){return(r1*r2)})
remove(surge4.r)

remove(ref.ras)

# How many 5m pixels of value 1 are in each raster?
# park.area.85.2050.m2 <- freq(park.85.2050.r, value = 1) * 5^2
park.area.85.2050.surge.m2 <- freq(park.85.2050.surge.r, value = 1) * 5^2

# What's ID associated with this park?
i <- grep("BICY", df.parks$UNIT_CODE)

# Create repository list for area computation; list good if length unknown.
list.park.name <- list()
list.park.area.85.2050 <- list() # (fyi, already computed from SLR in slr results)
list.park.area.85.2050.surge <- list()

# Fill lists with park code and areas (lists created in main script); 
list.park.name[[i]] <- df.parks$UNIT_CODE[i]
list.park.area.85.2050[[i]] <- park.area.85.2050.m2
list.park.area.85.2050.surge[[i]] <- park.area.85.2050.surge.m2

print(paste0("Done with BICY Surge", df.parks$UNIT_CODE[i], "."))
print(Sys.time() - start)


#################################
### WRITE SURGE RESULTS TABLE ###
#################################

# Set any NULLS in list to NA so they don't disappear
list.park.name <- sapply(list.park.name, function(x) ifelse(x == "NULL", NA, x))
list.park.area.85.2050 <- sapply(list.park.area.85.2050, function(x) ifelse(x == "NULL", NA, x))
list.park.area.85.2050.surge <- lapply(list.park.area.85.2050.surge, function(x) ifelse(x == "NULL", NA, x))

# Turn them into matrices; dunno what this does. t tranposes.
library(plyr)
mat1 <- (plyr::rbind.fill.matrix(lapply(list.park.name, t)))
mat2 <- (plyr::rbind.fill.matrix(lapply(list.park.area.85.2050, t)))
mat3 <- (plyr::rbind.fill.matrix(lapply(list.park.area.85.2050.surge, t)))

# Create dataframe and make sure the numbers are numeric
df.results <- data.frame(cbind(mat1, mat2, mat3)) ; remove(mat1, mat2, mat3)
df.results[,2] <- as.numeric(as.character(df.results[,2]))
df.results[,3] <- as.numeric(as.character(df.results[,3]))

# Calculate percent losses
df.results <- df.results %>%
  dplyr::rename_(UNIT_CODE = names(.)[1], # Dunno why rename_; but rename won't work
                 area.85.2050.m2 = names(.)[2],
                 area.85.2050.surge.m2 = names(.)[3]) %>%
  mutate(area.surge.loss.m2 = (area.85.2050.m2 - area.85.2050.surge.m2)) %>%
  mutate(area.surge.loss.perc = (area.surge.loss.m2 / area.85.2050.m2))

# Write results to csv
currentDate <- Sys.Date()
csvFileName <- paste0("BICY_SLR5m_results_",currentDate,".csv")
write.csv(df.results, paste0("//main.sefs.uw.edu/main/Space/Lawler/Shared/Julia/NPVuln/GIS/NPS/StormSurge/Surge_Compute/",csvFileName))

remove(currentDate)
remove(csvFileName)

########################################################################################
### LEAVE ALL SLR/LOW LAYERS LOADED & RASTERIZED FOR EVER, BUT USE DIFF SURGE LAYERS ###
### EVER10m_low_85_2050_C5H
### EVER10m_slr_85_2050_C5H
### EVER5m_low_85_2050_C5H
### EVER5m_slr_85_2050_C5H
########################################################################################






