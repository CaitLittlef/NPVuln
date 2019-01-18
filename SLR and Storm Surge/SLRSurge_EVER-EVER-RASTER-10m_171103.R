# Below creates rasters of park feauture class and slr/low layers.
# Subtract MHHW to get baseline park; compute area and create shapefile.
# Subtract future projections to get future park; compute area and create shapefile.
# N.b.,for EVER domain, runs BICY & EVER sequentially (specified )
# ... & skip slr/low raster creation if doing one after the other.


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



###########################
### LOAD SLR/LOW LAYERS ###
###########################

start <- Sys.time()

# Find names of SLR/Low layers
(slrMHHW.5m.name <- grep("EVER", layers.slr.MHHW, value = TRUE)[1]) 
(lowMHHW.5m.name <- grep("EVER", layers.low.MHHW, value = TRUE)[1]) 
(slr85.10m.name <- grep("EVER", layers.slr.85.2050, value = TRUE)[1])
(slr85.5m.name <- grep("EVER", layers.slr.85.2050, value = TRUE)[2])
(low85.10m.name <- grep("EVER", layers.low.85.2050, value = TRUE)[1])
(low85.5m.name <- grep("EVER", layers.low.85.2050, value = TRUE)[2]) 

# Load associated layers from geodatabase
slrMHHW.5m <- st_read(slr.db, slrMHHW.5m.name)
lowMHHW.5m <- st_read(slr.db, lowMHHW.5m.name)
slr85.10m <- st_read(slr.db, slr85.10m.name)
slr85.5m <- st_read(slr.db, slr85.5m.name)
low85.10m <- st_read(slr.db, low85.10m.name)
low85.5m <- st_read(slr.db, low85.5m.name)

remove(slrMHHW.5m.name, lowMHHW.5m.name, slr85.10m.name,
       slr85.5m.name, low85.10m.name, low85.5m.name)


# Note bbox & crs from slr85.10m 
# bbox:           xmin: 475268.3 ymin: 2794725 xmax: 525227.8 ymax: 2859035
# epsg (SRID):    6346
# proj4string:    +proj=utm +zone=17 +ellps=GRS80 +units=m +no_defs

# From slr85.5m
# bbox:           xmin: 437715.2 ymin: 2732420 xmax: 571762.4 ymax: 2912409
# epsg (SRID):    6346
# proj4string:    +proj=utm +zone=17 +ellps=GRS80 +units=m +no_defs


#################
### RASTERIZE ###
#################

# Create ref raster for rasterizing. Round up to ensure big enough to capture all layers' extents.
# N.b., using park DEM as ref is option, but there are 3 of them: 5m.v1, 5m.v2, 10m so create new w/ high res
ref.ras <- raster(xmn = 437700, xmx = 571800,
                  ymn = 2732400, ymx = 2912500,
                  resolution = 10, # setting res means don't need nrows or ncol
                  crs = "+proj=utm +zone=17 +ellps=GRS80 +units=m +no_defs", 
                  vals = 1)


# Rasterize the slr/low layers. Combining first halves computation time.
slrMHHW.5m.r <- slrMHHW.5m %>% st_combine() %>% as("Spatial") %>% rasterize(ref.ras)
lowMHHW.5m.r <- lowMHHW.5m %>% st_combine() %>% as("Spatial") %>% rasterize(ref.ras)
slr85.10m.r <- slr85.10m %>% st_combine() %>% as("Spatial") %>% rasterize(ref.ras)
slr85.5m.r <- slr85.5m %>% st_combine() %>% as("Spatial") %>% rasterize(ref.ras)
low85.10m.r <- low85.10m %>% st_combine() %>% as("Spatial") %>% rasterize(ref.ras)
low85.5m.r <- low85.5m %>% st_combine() %>% as("Spatial") %>% rasterize(ref.ras)

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

# Any NA flunks the raster algebra below -- NA will return NA. Set this non-water to 1.
slrMHHW.5m.r[is.na(slrMHHW.5m.r[])] <- 1
lowMHHW.5m.r[is.na(lowMHHW.5m.r[])] <- 1
slr85.10m.r[is.na(slr85.10m.r[])] <- 1
slr85.5m.r[is.na(slr85.5m.r[])] <- 1
low85.10m.r[is.na(low85.10m.r[])] <- 1
low85.5m.r[is.na(low85.5m.r[])] <- 1

print(paste0("Done with rasterizing."))
print(Sys.time() - start) 

####################################################################
### EVER - CREATE BASELINE & FUTURE PARKS; COMPUTE & WRITE AREAS ###
####################################################################

start <- Sys.time()

# Load park shapefile
(park <- parks[which(parks$UNIT_CODE == "EVER"),]) # in lat/long

# Get CRS of park layer to match slr/low layers (i.e., UTM 17N).
# st_transform maybe prefers EPSG code (fetched from SLR/LOW layers) to crs string.
# EPSG is stored in those layers, but if can't find for a raster, look up at spatialreference.org
park.utm <- park %>% st_transform(6346) ; park.utm

# Rasterize doesn't like sfc -- set to formal spatial class then rasterize with new raster as reference.
park.r <- park.utm %>% as("Spatial") %>% rasterize(ref.ras) ; crs(park.r)

# In raster calculations, NA will lead to NA regardless of math.
# Use values of 0 & 1 and multiplication, not subtraction. 
# In park, land remains 1 and water becomes 0 (not NA)
park.r[is.na(park.r[])] <- 0

# Multiply MHHW (as zero) by park to create park at MHHW: land=1, non-land=0
# Overlay is far more efficient than raster math.
park.MHHW.r <- overlay(park.r, slrMHHW.5m.r, lowMHHW.5m.r,
                       fun=function(r1, r2, r3){return(r1*r2*r3)})
# park.MHHW.r[park.MHHW.r == 0] <- NA # so ras->poly conversion only keeps park
# setting 0 to NA is irrelevent if just asking for frequency of value=1 for area, tho.

# Multiply slr/low (as zero) by park to create high-water park.
# Overlay is far more efficient than raster math.
park.85.2050.r <- overlay(park.MHHW.r, slr85.10m.r, slr85.5m.r, low85.10m.r, low85.5m.r,
                          fun=function(r1, r2, r3, r4, r5){return(r1*r2*r3*r4*r5)})
# park.85.2050.r[park.85.2050.r == 0] <- NA # so ras->poly conversion only keeps park
# setting 0 to NA is irrelevent if just asking for frequency of value=1 for area, tho.

#################### Error: memory exhausted (limit reached?) ############################
# Following fails. So, not creating shapefiles for each park below nor calculating area from polygons
# # Convert rasters to polygons. 
# start <- Sys.time()
# park.MHHW.poly <- park.MHHW.r %>%
#   rasterToPolygons(dissolve=TRUE) %>% # only returns non-NA values
#   as("sf") %>% # coerce to sf object from Spatial for st_write() below
#   st_transfor(4269) # set back to crs to match all other parks
# print(Sys.time() - start)
# 
# start <- Sys.time()
# park.85.2050.poly <- park.85.2050.r %>%
#   rasterToPolygons(dissolve=TRUE) %>% # only returns non-NA values
#   as("sf") %>% # coerce to sf object from Spatial for st_write() below
#   st_transfor(4269) # set back to crs to match all other parks
# print(Sys.time() - start)
##########################################################################################

# How many 10m pixels of value 1 are in each raster?
park.area.MHHW.m2 <- freq(park.MHHW.r, value = 1) * 10^2
park.area.85.2050.m2 <- freq(park.85.2050.r, value = 1) * 10^2

# What's ID associated with this park?
i <- grep("EVER", parks$UNIT_CODE)

# Create repository list for area computation; list good if length unknown.
list.park.name <- list()
list.park.area <- list()
list.park.area.85.2050 <- list()

# Fill lists with park code and areas (lists created in main script); 
list.park.name[[i]] <- parks$UNIT_CODE[i]
list.park.area[[i]] <- park.area.MHHW.m2
list.park.area.85.2050[[i]] <- park.area.85.2050.m2

# # Write shapefiles. N.b., if writeRaster, try ras <- as.integer(ras)...
# # ...b/c Arc won't play nicely with float/doubles. 
# setwd("//main.sefs.uw.edu/main/Space/Lawler/Shared/Julia/NPVuln/GIS/NPS/SLR/SLR_Compute")
# st_write(park.MHHW.poly, paste0(parks$UNIT_CODE[i],"_baseline.shp"))
# st_write(park.85.2050.poly, paste0(parks$UNIT_CODE[i],"_slr.85.2050.shp"))
# setwd("//main.sefs.uw.edu/main/Space/Lawler/Shared/Julia/NPVuln")

print(paste0("Done with SLR for ", parks$UNIT_CODE[i], "."))
print(Sys.time() - start)


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
csvFileName <- paste0("EVER_SLR_resultz_",currentDate,".csv")
write.csv(df.results, paste0("//main.sefs.uw.edu/main/Space/Lawler/Shared/Julia/NPVuln/GIS/NPS/SLR/SLR_Compute/",csvFileName))
remove(currentDate, csvFileName)







##########################
### EVER - STORM SURGE ###
##########################

surge.db <- "//main.sefs.uw.edu/main/Space/Lawler/Shared/Julia/NPVuln/GIS/NPS/StormSurge/Surge_vs2.gdb"

# Already created look-up tbl for which parks get which surge projections. 
lu.surge <- read.csv("//main.sefs.uw.edu/main/Space/Lawler/Shared/Julia/NPVuln/GIS/NPS/StormSurge/lu_surge_layers.csv")

surge1 <- st_read(surge.db, "EVER10m_low_85_2050_C5H") 
surge2 <- st_read(surge.db, "EVER10m_slr_85_2050_C5H")
surge3 <- st_read(surge.db, "EVER5m_low_85_2050_C5H")
surge4 <- st_read(surge.db, "EVER5m_slr_85_2050_C5H")

ref.ras <- raster(xmn = 437700, xmx = 571800,
                  ymn = 2732400, ymx = 2912500,
                  resolution = 10, # setting res means don't need nrows or ncol
                  crs = "+proj=utm +zone=17 +ellps=GRS80 +units=m +no_defs", 
                  vals = 1)

# Rasterize the surge layers. Combining first halves computation time.
surge1.r <- surge1 %>% st_combine() %>% as("Spatial") %>% rasterize(ref.ras)
surge2.r <- surge2 %>% st_combine() %>% as("Spatial") %>% rasterize(ref.ras)
surge3.r <- surge3 %>% st_combine() %>% as("Spatial") %>% rasterize(ref.ras)
surge4.r <- surge4 %>% st_combine() %>% as("Spatial") %>% rasterize(ref.ras)

remove(ref.ras)

# In raster calculations, NA will lead to NA regardless of math.
# Use values of 0 & 1 and multiplication, not subtraction. 
# In projections, water becomes 0 & land (or non-water) becomes 1
# Reclassify requires matrix of: (from, to, becomes) # n.b., (1, 1, 0) won't work
rclmat <- c(0.5, 2, 0) %>% matrix(ncol=3, byrow=TRUE)
surge1.r <- reclassify(surge1.r, rclmat)
surge2.r <- reclassify(surge2.r, rclmat)
surge3.r <- reclassify(surge3.r, rclmat)
surge4.r <- reclassify(surge4.r, rclmat)

# Any NA flunks the raster algebra below -- NA will return NA. Set this non-water to 1.
surge1.r[is.na(surge1.r[])] <- 1
surge2.r[is.na(surge2.r[])] <- 1
surge3.r[is.na(surge3.r[])] <- 1
surge4.r[is.na(surge4.r[])] <- 1

# In raster calculations, NA will lead to NA regardless of math.
# Use values of 0 & 1 and multiplication, not subtraction. 
# In park, land remains 1 and water becomes 0 (not NA)
park.85.2050.r[is.na(park.85.2050.r[])] <- 0

# Multiply MHHW (as zero) by park to create park at MHHW: land=1, non-land=0
# Overlay is far more efficient than raster math.
park.85.2050.surge.r <- overlay(park.85.2050.r, surge1.r, surge2.r, surge3.r, surge4.r,
                       fun=function(r1, r2, r3, r4, r5){return(r1*r2*r3*r4*r5)})


# How many 10m pixels of value 1 are in each raster?
park.area.85.2050.surge.m2 <- freq(park.85.2050.surge.r, value = 1) * 10^2

# What's ID associated with this park?
i <- grep("EVER", parks$UNIT_CODE)

# Create repository list for area computation; list good if length unknown.
list.park.name <- list()
list.park.area.85.2050 <- list() # (fyi, already computed from SLR in slr results)
list.park.area.85.2050.surge <- list()

# Fill lists with park code and areas (lists created in main script); 
list.park.name[[i]] <- parks$UNIT_CODE[i]
list.park.area.85.2050[[i]] <- park.area.85.2050.m2
list.park.area.85.2050.surge[[i]] <- park.area.85.2050.surge.m2


print(paste0("Done with surge for ", parks$UNIT_CODE[i], "."))
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
csvFileName <- paste0("EVER_resultz_surge_",currentDate,".csv")
write.csv(df.results, paste0("//main.sefs.uw.edu/main/Space/Lawler/Shared/Julia/NPVuln/GIS/NPS/StormSurge/Surge_Compute/",csvFileName))

remove(currentDate)
remove(csvFileName)

########################################################################################
### LEAVE ALL SLR/LOW LAYERS LOADED & RASTERIZED FOR BICY, BUT USE DIFF SURGE LAYERS ###
########################################################################################

