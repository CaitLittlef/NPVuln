# Below creates rasters of park feature class and slr/low layers.
# Subtract MHHW to get baseline park; compute area and create shapefile.
# Subtract future projections to get future park; compute area and create shapefile.
# N.b.,for JELA domain, apply to parks JELA & JAZZ


###########################
### LOAD SLR/LOW LAYERS ###
###########################

start <- Sys.time()

# Find names of SLR/Low layers
(slrMHHW.name <- grep("JELA", layers.slr.MHHW, value = TRUE)[1]) # Keep JELA not JELA2 (no park overlap).
(lowMHHW.name <- grep("JELA", layers.low.MHHW, value = TRUE)[1]) # Keep JELA not JELA2 (no park overlap).
(slr85.name <- grep("JELA", layers.slr.85.2050, value = TRUE)[1]) # Keep JELA not JELA2 (no park overlap).
(low85.name <- grep("JELA", layers.low.85.2050, value = TRUE)[1]) # Keep JELA not JELA2 (no park overlap).

# Load associated layers from geodatabase
slrMHHW <- st_read(slr.db, slrMHHW.name)
lowMHHW <- st_read(slr.db, lowMHHW.name)
slr85 <- st_read(slr.db, slr85.name)
low85 <- st_read(slr.db, low85.name)
# Note bbox & crs (below from slr85)
# bbox:           xmin: 702301.3 ymin: 3284677 xmax: 804896.9 ymax: 3337838
# epsg (SRID):    6344
# proj4string:    +proj=utm +zone=15 +ellps=GRS80 +units=m +no_defs

#################
### RASTERIZE ###
#################

# Create ref raster for rasterizing. Round up to ensure big enough to capture all layers' extents.
# N.b., using park DEM as ref is option, but JELA_DEM fails & JELA2_DEM doesn't cover full extent >:(
# ref.ras <- raster(xmn = 555000, xmx = 805000,
#                   ymn = 3200000, ymx = 3380000,
#                   resolution = 10, # setting res means don't need nrows or ncol
#                   crs = "+proj=utm +zone=15 +ellps=GRS80 +units=m +no_defs",
#                   vals = 1)
# I know the above works, but see if this much smaller one works, too... 
ref.ras <- raster(xmn = 702250, xmx = 804900,
                  ymn = 3283600, ymx = 3337900,
                  resolution = 10, # setting res means don't need nrows or ncol
                  crs = "+proj=utm +zone=15 +ellps=GRS80 +units=m +no_defs", 
                  vals = 1)



# Rasterize the slr/low layers. Combining first halves computation time.
slrMHHW.r <- slrMHHW %>% st_combine() %>% as("Spatial") %>% rasterize(ref.ras)
lowMHHW.r <- lowMHHW %>% st_combine() %>% as("Spatial") %>% rasterize(ref.ras)
slr85.r <- slr85 %>% st_combine() %>% as("Spatial") %>% rasterize(ref.ras)
low85.r <- low85 %>% st_combine() %>% as("Spatial") %>% rasterize(ref.ras)

slrMHHW.r@data@min ; slrMHHW.r@data@max # Confirm they're only 1 & NA
lowMHHW.r@data@min ; lowMHHW.r@data@max
slr85.r@data@min ; slr85.r@data@max
low85.r@data@min ; low85.r@data@max

# In computing baselines/projections, use water=0, land=1
# Reclassify requires matrix of: (from, to, becomes) # n.b., (1, 1, 0) won't work
rclmat <- c(0.5, 2, 0) %>% matrix(ncol=3, byrow=TRUE)
slrMHHW.r <- reclassify(slrMHHW.r, rclmat)
lowMHHW.r <- reclassify(lowMHHW.r, rclmat)
slr85.r <- reclassify(slr85.r, rclmat)
low85.r <- reclassify(low85.r, rclmat)

slrMHHW.r[is.na(slrMHHW.r[])] <- 1 
lowMHHW.r[is.na(lowMHHW.r[])] <- 1
slr85.r[is.na(slr85.r[])] <- 1
low85.r[is.na(low85.r[])] <- 1

print(paste0("Done with rasterizing."))
print(Sys.time() - start)



####################################################################
### JELA - CREATE BASELINE & FUTURE PARKS; COMPUTE & WRITE AREAS ###
####################################################################

# Load park shapefile
(park <- parks[which(parks$UNIT_CODE == "JELA"),])
# Note it's in lat-long
# bbox:           xmin: -92.41747 ymin: 29.7411 xmax: -89.91491 ymax: 30.49338
# epsg (SRID):    4269
# proj4string:    +proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs

# Get CRS of park layer to match slr/low layers (i.e., UTM 15N).
# st_transform maybe prefers EPSG code (fetched from SLR/LOW layers) to crs string.
# EPSG is stored in those layers, but if can't find for a raster, look up at spatialreference.org
park.utm <- park %>% st_transform(6344) ; park.utm

# Rasterize doesn't like sfc -- set to formal spatial class then rasterize with new raster as reference.
park.r <- park.utm %>% as("Spatial") %>% rasterize(ref.ras) ; crs(park.r)

# In raster calculations, NA will lead to NA regardless of math.
# Use values of 0 & 1 and multiplication, not subtraction. 
# In park, land remains 1 and water becomes 0 (not NA)
park.r[is.na(park.r[])] <- 0

# Multiply MHHW (as zero) by park to create park at MHHW: land=1, non-land=0
# Overlay is far more efficient than raster math.
park.MHHW.r <- overlay(park.r, slrMHHW.r, lowMHHW.r,
                       fun=function(r1, r2, r3){return(r1*r2*r3)})
park.MHHW.r[park.MHHW.r == 0] <- NA # so ras->poly conversion only keeps park

# Multiply slr/low (as zero) by park to create high-water park.
# Overlay is far more efficient than raster math.
park.85.2050.r <- overlay(park.MHHW.r, slr85.r, low85.r,
                          fun=function(r1, r2, r3){return(r1*r2*r3)})
park.85.2050.r[park.85.2050.r == 0] <- NA # so ras->poly conversion only keeps park


# Convert rasters to polygons.
start <- Sys.time()
park.MHHW.poly <- park.MHHW.r %>%
  rasterToPolygons(dissolve=TRUE) %>% # only returns non-NA values
  as("sf") %>% # coerce to sf object from Spatial for st_write() below
  st_transform(4269) # try to match prior CRS for all parks (FIXME: not exact match)
print(Sys.time() - start)

start <- Sys.time()
park.85.2050.poly <- park.85.2050.r %>%
  rasterToPolygons(dissolve=TRUE) %>% # only returns non-NA values
  as("sf") %>% # coerce to sf object from Spatial for st_write() below
  st_transform(4269) # try to match prior CRS for all parks (FIXME: not exact match)
print(Sys.time() - start)

# What's ID associated with this park?
i <- grep("JELA", parks$UNIT_CODE)

# Fill lists with park code and areas (lists created in main scripst)
list.park.name[[i]] <- parks$UNIT_CODE[i]
list.park.area[[i]] <- round(as.numeric(st_area(park.MHHW.poly)),2)
list.park.area.85.2050[[i]] <- round(as.numeric(st_area(park.85.2050.poly)),2)

# Write shapefiles. N.b., if writeRaster, try ras <- as.integer(ras)...
# ...b/c Arc won't play nicely with float/doubles. 
setwd("//main.sefs.uw.edu/main/Space/Lawler/Shared/Julia/NPVuln/GIS/NPS/SLR/SLR_Compute")
st_write(park.MHHW.poly, paste0(parks$UNIT_CODE[i],"_baseline.shp"))
st_write(park.85.2050.poly, paste0(parks$UNIT_CODE[i],"_slr.85.2050.shp"))
setwd("//main.sefs.uw.edu/main/Space/Lawler/Shared/Julia/NPVuln")

print(paste0("Done with park ", parks$UNIT_CODE[i], "."))
print(Sys.time() - start)












####################################################################
### JAZZ - CREATE BASELINE & FUTURE PARKS; COMPUTE & WRITE AREAS ###
####################################################################

# Load park shapefile
(park <- parks[which(parks$UNIT_CODE == "JAZZ"),])
# Note it's in lat-long
# bbox:           xmin: -92.41747 ymin: 29.7411 xmax: -89.91491 ymax: 30.49338
# epsg (SRID):    4269
# proj4string:    +proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs

# Get CRS of park layer to match slr/low layers (i.e., UTM 15N).
# st_transform maybe prefers EPSG code (fetched from SLR/LOW layers) to crs string.
# EPSG is stored in those layers, but if can't find for a raster, look up at spatialreference.org
park.utm <- park %>% st_transform(6344) ; park.utm

# Rasterize doesn't like sfc -- set to formal spatial class then rasterize with new raster as reference.
park.r <- park.utm %>% as("Spatial") %>% rasterize(ref.ras) ; crs(park.r)

# In raster calculations, NA will lead to NA regardless of math.
# Use values of 0 & 1 and multiplication, not subtraction. 
# In park, land remains 1 and water becomes 0 (not NA)
park.r[is.na(park.r[])] <- 0


# Multiply MHHW (as zero) by park to create park at MHHW: land=1, non-land=0
# Overlay is far more efficient than raster math.
park.MHHW.r <- overlay(park.r, slrMHHW.r, lowMHHW.r,
                       fun=function(r1, r2, r3){return(r1*r2*r3)})
park.MHHW.r[park.MHHW.r == 0] <- NA # so ras->poly conversion only keeps park

# Multiply slr/low (as zero) by park to create high-water park.
# Overlay is far more efficient than raster math.
park.85.2050.r <- overlay(park.MHHW.r, slr85.r, low85.r,
                          fun=function(r1, r2, r3){return(r1*r2*r3)})
park.85.2050.r[park.85.2050.r == 0] <- NA # so ras->poly conversion only keeps park


# Convert rasters to polygons.
start <- Sys.time()
park.MHHW.poly <- park.MHHW.r %>%
  rasterToPolygons(dissolve=TRUE) %>% # only returns non-NA values
  as("sf") %>% # coerce to sf object from Spatial for st_write() below
  st_transform(4269) # try to match prior CRS for all parks (FIXME: not exact match)
print(Sys.time() - start)

start <- Sys.time()
park.85.2050.poly <- park.85.2050.r %>%
  rasterToPolygons(dissolve=TRUE) %>% # only returns non-NA values
  as("sf") %>% # coerce to sf object from Spatial for st_write() below
  st_transform(4269) # try to match prior CRS for all parks (FIXME: not exact match)
print(Sys.time() - start)

# What's ID associated with this park?
i <- grep("JAZZ", parks$UNIT_CODE)

# Fill lists with park code and areas (lists created in main scripst)
list.park.name[[i]] <- parks$UNIT_CODE[i]
list.park.area[[i]] <- round(as.numeric(st_area(park.MHHW.poly)),2)
list.park.area.85.2050[[i]] <- round(as.numeric(st_area(park.85.2050.poly)),2)

# Write shapefiles. N.b., if writeRaster, try ras <- as.integer(ras)...
# ...b/c Arc won't play nicely with float/doubles. 
setwd("//main.sefs.uw.edu/main/Space/Lawler/Shared/Julia/NPVuln/GIS/NPS/SLR/SLR_Compute")
st_write(park.MHHW.poly, paste0(parks$UNIT_CODE[i],"_baseline.shp"))
st_write(park.85.2050.poly, paste0(parks$UNIT_CODE[i],"_slr.85.2050.shp"))
setwd("//main.sefs.uw.edu/main/Space/Lawler/Shared/Julia/NPVuln")

print(paste0("Done with park ", parks$UNIT_CODE[i], "."))
print(Sys.time() - start)







###########################
### WRITE RESULTS TABLE ###
###########################

# Set any NULLS in list to NA so they don't disappear
list.park.name <- sapply(list.park.name, function(x) ifelse(x == "NULL", NA, x))
list.park.area <- sapply(list.park.area, function(x) ifelse(x == "NULL", NA, x))
list.park.area.85.2050 <- lapply(list.park.area.85.2050, function(x) ifelse(x == "NULL", NA, x))

# Turn them into matrices; dunno what this does. t tranposes.
library(plyr)
mat1 <- (plyr::rbind.fill.matrix(lapply(list.park.name, t)))
mat2 <- (plyr::rbind.fill.matrix(lapply(list.park.area, t)))
mat3 <- (plyr::rbind.fill.matrix(lapply(list.park.area.85.2050, t)))
# Alt below. Avoiding b/c some lists may have more/less NULLS...
#... based on data availability. Don't want to collapse to diff sizes.
# mat1 <- do.call("rbind",list.park.name) # do.call asks for LIST of arguments
# mat2 <- do.call("rbind",list.park.area)
# mat3  <- do.call("rbind",list.park.area.85.2050)
df.results <- data.frame(cbind(mat1, mat2, mat3)) ; remove(mat1, mat2, mat3)
df.results[,2] <- as.numeric(as.character(df.results[,2]))
df.results[,3] <- as.numeric(as.character(df.results[,3]))

df.results <- df.results %>%
  dplyr::rename_(UNIT_CODE = names(.)[1], # Dunno why rename_; but rename won't work
                 area.m2 = names(.)[2],
                 area.85.2050.m2 = names(.)[3]) %>%
  mutate(area.slr.loss.m2 = (area.m2 - area.85.2050.m2)) %>%
  mutate(area.slr.loss.perc = (area.slr.loss.m2 / area.m2))


# Write results to csv
currentDate <- Sys.Date()
csvFileName <- paste0("weirdo_results_JAZZJELA",currentDate,".csv")
write.csv(df.results, paste0("//main.sefs.uw.edu/main/Space/Lawler/Shared/Julia/NPVuln/GIS/NPS/SLR/SLR_Compute/",csvFileName))

remove(currentDate, csvFileName)



