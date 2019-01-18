# Identifies shapefiles in SLR geodatabase that have no dimensions and/or zero features.
# See output table in here \\lawlercompute.sefs.uw.edu\Space_Lawler\Shared\Julia\NPVuln\GIS\NPS\SLR\SLR_Compute
# Named na_geom_2017-09-28.csv

setwd("//main.sefs.uw.edu/main/Space/Lawler/Shared/Julia/NPVuln")

install.packages("sf")
install.packages("rgdal") 
install.packages("rgeos")
install.packages("raster")
install.packages("tidyverse")
install.packages("dplyr")
install.packages("mapview")
install.packages("geosphere")

library(sf)
library(rgdal)
library(rgeos)
library(raster)
library(tidyverse)
library(dplyr)
library(mapview)
library(geosphere)


slr.db <- "//main.sefs.uw.edu/main/Space/Lawler/Shared/Julia/NPVuln/GIS/NPS/SLR/SLR_vs2.gdb"
layer.list <- ogrListLayers(slr.db)

# Won't be using any RCP 4.5 projections or 2100 projections.
layers.rcp45 <- grep(pattern = "45", x = layer.list)
layer.list <- layer.list[-layers.rcp45] %>% sort(.)
layers.2100 <- grep(pattern = "2100", x = layer.list)
layer.list <- layer.list[-layers.2100] %>% sort(.)
remove(layers.rcp45, layers.2100)


# Create list to catch park names and whether or not geometry is NA
list.layer.name <- list()
list.layer.geom.na <- list()
list.layer.feat.zero <- list()
list.layer.feat.num <- list()
list.run.time <- list()

# Get all index values in layer.list
loop.ready <- (seq_along(layer.list))

# # Set mini loop to see if this works. Know CABR_low_MHHW is empty.
# temp <- grep(pattern = "CABR", layer.list) ; (grep(pattern = "CABR", layer.list, value = TRUE))
# loop.ready <- loop.ready[temp]
# # Alt: loop.ready <- as.integer(77,80) based on values from grep

remove(i)
for(i in loop.ready){
  start <- Sys.time()
  # Catch layer name.
  list.layer.name[[i]] <- layer.list[i]
  # Catch if dimensions are na. If na, gives TRUE. 
  list.layer.geom.na[[i]] <- any(is.na(st_dimension(st_read(slr.db, layer.list[i]))))
  # Catch if Shape_area variable has length of zero, in which case there aren't features.
  list.layer.feat.zero[[i]] <- ifelse(length(st_read(slr.db,layer.list[i])$Shape_Area) > 0, "NON-ZERO", "ZERO")
  # How many features are there?
  list.layer.feat.num[[i]] <- length(st_dimension(st_read(slr.db, layer.list[i])))
  # How long did that take?
  list.run.time[[i]] <- print(Sys.time() - start)
}


# Set any NULLS in list to NA so they don't disappear
list.layer.name <- sapply(list.layer.name, function(x) ifelse(x == "NULL", NA, x))
list.layer.geom.na <- sapply(list.layer.geom.na, function(x) ifelse(x == "NULL", NA, x))
list.layer.feat.zero <- sapply(list.layer.feat.zero, function(x) ifelse(x == "NULL", NA, x))
list.layer.feat.num <- sapply(list.layer.feat.num, function(x) ifelse(x == "NULL", NA, x))
list.run.time <- sapply(list.run.time, function(x) ifelse(x == "NULL", NA, x))

# Turn them into matrices; dunno what this does. t tranposes.
library(plyr)
mat1 <- (plyr::rbind.fill.matrix(lapply(list.layer.name, t)))
mat2 <- (plyr::rbind.fill.matrix(lapply(list.layer.geom.na, t)))
mat3 <- (plyr::rbind.fill.matrix(lapply(list.layer.feat.zero, t)))
mat4 <- (plyr::rbind.fill.matrix(lapply(list.layer.feat.num, t)))
mat5 <- (plyr::rbind.fill.matrix(lapply(list.run.time, t)))
library(dplyr)

df.geometry <- data.frame(cbind(mat1, mat2, mat3, mat4, mat5)) ; remove(mat1, mat2, mat3, mat4, mat5)

df.geometry <- df.geometry %>%
  dplyr::rename_(UNIT_CODE = names(.)[1], # Dunno why rename_; but rename won't work
                 geom_na = names(.)[2],
                 feat.zero = names(.)[3],
                 feat.num = names(.)[4],
                 run.time = names(.)[5])


currentDate <- Sys.Date()
csvFileName <- paste0("geometries_",currentDate,".csv")
write.csv(df.geometry, paste0("//main.sefs.uw.edu/main/Space/Lawler/Shared/Julia/NPVuln/GIS/NPS/SLR/SLR_Compute/",csvFileName))

remove(temp, loop.ready, i, start, currentDate, csvFileName)

print(paste0("total time elapsed: ",sum(as.numeric(df.geometry$run.time), na.rm=TRUE), " seconds."))
