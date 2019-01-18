# setwd("//main.sefs.uw.edu/main/Space/Lawler/Shared/Julia/NPVuln")
# #
# 
# #
# # FIXME:
# # In all cases, use st_transform for parks to SLW/low CRS, not vice versa >:p
# 
# 
# # install.packages("sf")
# # install.packages("rgdal")
# # install.packages("rgeos")
# # install.packages("raster")
# # install.packages("tidyverse")
# # install.packages("dplyr")
# # install.packages("mapview")
# # install.packages("geosphere")
# 
# library(sf)
# library(rgdal)
# library(rgeos)
# library(raster)
# library(tidyverse)
# library(dplyr)
# library(mapview)
# library(geosphere)
# 
# # Load SLR & Surge .gdbs
# slr.db <- "//main.sefs.uw.edu/main/Space/Lawler/Shared/Julia/NPVuln/GIS/NPS/SLR/SLR_vs2.gdb"
# surge.db <- "//main.sefs.uw.edu/main/Space/Lawler/Shared/Julia/NPVuln/GIS/NPS/StormSurge/Surge_vs2.gdb"
# 
# ###########################
# # Process park shapefiles #
# ###########################
# 
# # GOAL: extract (or reference) individual park boundaries from shapefile below.
# parks <- st_read(dsn = slr.db, layer = "NPS_park_boundary_coast") %>%
#   st_buffer(dist = 0) # fix invalid geometries (warning re: lat/long vs. dd)
# class(parks) # sf: simple feature; info as data.frame
# # 118 data observations at 115 levels (i.e., UNIT_CODE) with 118 obs; 23 variables
# parks <- parks[,-(2:19)] # Drops all variables except FileName, shape info.
# 
# # 3 UNIT_CODES are for both park & associated reserve (GLBA, KATM, WRST)
# # Dissolve those 3x2 instances into 3x1 obs --> 115 obs
# parks <- parks %>%
#   dplyr::group_by(UNIT_CODE) %>%
#   dplyr::summarise() # tried summarise(AREA = sum(Shape_Area)) but later got carried as num. not real area.
# # Dunno why, but parks is now sfc_GEOMETRY not MULTIPOLYGON.
# parks <- st_cast(parks) # Re-convert to MULTIPOLYGON, else probs with join
# 
# # Load in look-up table for SLR domains representing multiple parks (UNIT_CODE)...
# # ...& YES/NO for precise to actual projection layers. Src: NPS file 0Readme_Domains_vs2.xlsx
# lu.domains <- read.csv("//main.sefs.uw.edu/main/Space/Lawler/Shared/Julia/NPVuln/GIS/NPS/SLR/lu_unit_domains.csv")
# parks <- left_join(parks, lu.domains, by = "UNIT_CODE") # now 115 obs, 4 variables
# 
# # Check for mis-matches or empty domains.
# apply(is.na(parks),2,sum) # Shows 2 domains are NA
# is.na(parks$DOMAIN) # shows rows 29, 75
# parks[c(29,75),]
# parks$DOMAIN[29] <- "EBLA" # Weirdly got dropped; EBLA=EBLA in orig NPS lookup
# parks$READY[29] <- "YES" # There's only 1 projection for EBLA -- good to go in loop.
# parks$DOMAIN[75] <- "NCAP" # NACC in DC so assigning to NCAP; not in orig NPS lookup
# parks$READY[75] <- "NO" # Multiple projections for NCAP -- not good to go in loop.
# apply(is.na(parks),2,sum) # Clear!
# 
# # Keep df of parks around
# df.parks <- parks ; st_geometry(df.parks) <- NULL
# 
# ##########################
# # CREATE REFERENCE LISTS #
# ##########################
# 
# # Pull names from SLR .gdb
# layer.list <- ogrListLayers(slr.db) # N.b., this is a vector, not R-style list
# 
# # Won't be using any RCP 4.5 projections
# layers.rcp45 <- grep(pattern = "45", x = layer.list) # Not having value=TRUE just gives index
# layer.list <- layer.list[-layers.rcp45] %>% sort(.)
# remove(layers.rcp45)
# 
# # Make vectors of each type of projection (value=TRUE gives 'em); sort so names line-up
# layers.MHHW <- grep(pattern = "MHHW", x = layer.list, value = TRUE) %>% sort(.)
# layers.slr.MHHW <- grep(pattern = "slr", x = layers.MHHW, value = TRUE) %>% sort(.)
# layers.low.MHHW <- grep(pattern = "low", x = layers.MHHW, value = TRUE) %>% sort(.)
# layers.85 <- grep(pattern = "85", x = layer.list, value = TRUE) %>% sort(.)
# layers.85.2050 <- grep(pattern="2050", x=layers.85, value=TRUE) %>% sort(.)
# layers.slr.85.2050 <- grep(pattern = "slr", x = layers.85.2050, value = TRUE) %>% sort(.)
# layers.low.85.2050 <- grep(pattern = "low", x = layers.85.2050, value = TRUE) %>% sort(.)
# layers.85.2100 <- grep(pattern="2100", x=layers.85, value=TRUE) %>% sort(.)
# # layers.slr.85.2100 <- grep(pattern = "slr", x = layers.85.2100, value = TRUE) %>% sort(.)
# # layers.low.85.2100 <- grep(pattern = "low", x = layers.85.2100, value = TRUE) %>% sort(.)
# remove(layers.MHHW, layers.85, layers.85.2050, layers.85.2100)



#################
### LOOP PREP ###
#################

# Only uses parks that aren't too big and don't have multiple versions and
# that have normal names, complete geometries, and precise projection matches.
# see SLR_StormSurge_MISMATCH for comparisons.
# Added column READY to domain look_up (from NPS .xls).
# Says YES/NO if good to go in this loop.
# Use precise matches to start seed of INDICES for loop:
loop.ready <- grep(pattern = "YES", x = parks$READY)

# Function erases all of y from x; not sure why but don't union x (or else get all parks)
st_erase = function(x, y) st_difference(x, st_union(st_combine(y)))

# Create repository list for area computation; list good if length unknown.
list.park.name <- list()
list.park.area <- list()
list.park.area.85.2050 <- list()



############
### LOOP ###
############
remove(i)
for(i in loop.ready){ 

start <- Sys.time()
    
# Find the domain associated with the first park (substr finds characters btwn start/stop)
temp <- substr(parks$DOMAIN[i], start=1, stop=4)

# Find the associated MHHW projections. Need to say "if error don't worry abt it.
# For parks w/ 2 projections with 1 w/ proper name (e.g,. ACAD + ACAD2)...
# ...just run 1st (see [1] below) then fix later with union of ADAD & ACAD2
low.MHHW.name <- grep(pattern=temp, x=layers.low.MHHW, value=TRUE)[1] # only keep the first instance of match
slr.MHHW.name <- grep(pattern=temp, x=layers.slr.MHHW, value=TRUE)[1]

# Read in associated MHHW shapefiles; set projection to match parks
low.MHHW.poly <- st_read(slr.db, low.MHHW.name) %>%
  st_transform(st_crs(parks)) %>%
  st_buffer(dist = 0) # fix invalid geometries (warning re: lat/long vs. dd)
slr.MHHW.poly <- st_read(slr.db, slr.MHHW.name) %>%
  st_transform(st_crs(parks)) %>%
  st_buffer(dist = 0) # fix invalid geometries (warning re: lat/long vs. dd)

# Create baseline park: erase 1) low-lying (flooded land) and...
# ...2) intersection of slr.MHHW & park to get flooded land.
park.MHHW <- st_erase(parks[i,], low.MHHW.poly) %>%
  st_erase(slr.MHHW.poly)

# Find associated 2050 SLR projections (includes coastal SLR & low-lying areas)
low.85.2050.name <- grep(pattern=temp, x=layers.low.85.2050, value=TRUE)[1]
slr.85.2050.name <- grep(pattern=temp, x=layers.slr.85.2050, value=TRUE)[1]

# Read in associated SLR shapefiles; set projection to match parks
low.85.2050.poly <- st_read(slr.db, low.85.2050.name) %>%
  st_transform(st_crs(parks)) %>%
  st_buffer(dist = 0) # fix invalid geometries (warning re: lat/long vs. dd)
slr.85.2050.poly <- st_read(slr.db, slr.85.2050.name) %>%
  st_transform(st_crs(parks)) %>%
  st_buffer(dist = 0) # fix invalid geometries (warning re: lat/long vs. dd)

# Create future park: erase low-lying areas & land lost to SLR
park.85.2050 <- st_erase(park.MHHW, low.85.2050.poly) %>%
  st_erase(slr.85.2050.poly)

# Compute areas and write to lists
list.park.name[[i]] <- parks$UNIT_CODE[i]
list.park.area[[i]] <- round(as.numeric(st_area(park.MHHW)),2)
list.park.area.85.2050[[i]] <- round(as.numeric(st_area(park.85.2050)),2)

# Output folder if I generate shapefiles
setwd("//main.sefs.uw.edu/main/Space/Lawler/Shared/Julia/NPVuln/GIS/NPS/SLR/SLR_Compute")
# Write as new shapefile in directory SLR_Compute
st_write(park.MHHW, paste0(parks$UNIT_CODE[i],"_baseline.shp"))
st_write(park.85.2050, paste0(parks$UNIT_CODE[i],"_slr.85.2050.shp"))
# Reset wd
setwd("//main.sefs.uw.edu/main/Space/Lawler/Shared/Julia/NPVuln")

print(paste0("Done with park ", parks$UNIT_CODE[i], "."))
print(Sys.time() - start)
}


# Remove unnecessary thangs
remove(slr.MHHW.name, low.MHHW.name,
       slr.MHHW.poly, low.MHHW.poly,
       slr.85.2050.name, low.85.2050.name,
       slr.85.2050.poly, low.85.2050.poly,
       park.MHHW, park.85.2050, temp, i)


##########################
### MAKE RESULTS TABLE ###
##########################

# Set any NULLS in list to NA so they don't disappear
list.park.name <- sapply(list.park.name, function(x) ifelse(x == "NULL", NA, x))
list.park.area <- sapply(list.park.area, function(x) ifelse(x == "NULL", NA, x))
list.park.area.85.2050 <- lapply(list.park.area.85.2050, function(x) ifelse(x == "NULL", NA, x))

# Turn them into matrices; t tranposes.
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
csvFileName <- paste0("loop_results_KHWK_",currentDate,".csv")
write.csv(df.results, paste0("//main.sefs.uw.edu/main/Space/Lawler/Shared/Julia/NPVuln/GIS/NPS/SLR/SLR_Compute/",csvFileName))

remove(currentDate)
remove(csvFileName)

