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

# Load SLR & Surge .gdbs
# slr.db <- "//main.sefs.uw.edu/main/Space/Lawler/Shared/Julia/NPVuln/GIS/NPS/SLR/SLR_vs2.gdb"
surge.db <- "//main.sefs.uw.edu/main/Space/Lawler/Shared/Julia/NPVuln/GIS/NPS/StormSurge/Surge_vs2.gdb"

# GOAL: extract (or reference) individual park boundaries from shapefile below.
parks <- st_read(dsn = surge.db, layer = "NPS_park_boundary_coast") %>%
  st_buffer(dist = 0) # fix invalid geometries (warning re: lat/long vs. dd)
class(parks) # sf: simple feature; info as data.frame
# 118 data observations at 115 levels (i.e., UNIT_CODE) with 118 obs; 23 variables
parks <- parks[,-(2:23)] # Drops all variables except FileName, shape info.


# Dunno why, but parks is now sfc_GEOMETRY not MULTIPOLYGON. 
parks <- st_cast(parks) # Re-convert to MULTIPOLYGON, else probs with join

# Using original NPS look-up spreadsheet (02Readme_Surge_vs2.xls), created look-up tbl.
# Has UNIT_CODES for parks with surge projections & all associated layers.
# N.b., AK parks don't have projections, nor do several others (e.g., OLYM).
# Any projections with zero features are excluded (see geometries tab in .xls)
# N.b., suffix is based on layer, not highest cat recorded + 1 in some cases.
lu.surge <- read.csv("//main.sefs.uw.edu/main/Space/Lawler/Shared/Julia/NPVuln/GIS/NPS/StormSurge/lu_surge_layers.csv")
# Remove artifact (empty rows)
lu.surge <- lu.surge[-c(101:154),]
# Set THJE(JEFM) UNIT_CODE to JEFM, which is what slr projections call it
lu.surge$UNIT_CODE <- as.character(lu.surge$UNIT_CODE)
lu.surge$UNIT_CODE[which(lu.surge$UNIT_CODE == "THJE(JEFM)")] <- "JEFM"


# Join lu tbl to parks; inner join will keep only parks with matches in lu tbl. 
parks <- inner_join(parks, lu.surge, by = "UNIT_CODE")
# Alt, make drop list:
# no.surge <- c("ANIA", "BELA", "CAKR", "GLBA", "KATM", "LACL", etc.)
# parks <- parks[! parks$UNIT_CODE %in% no.surge,] ; remove(no.surge)

# Won't need park geometries, so can just ref df in future.
df.parks <- parks ; st_geometry(df.parks) <- NULL

# Pull names from Surge.gdb -- I'll be calling these in loop.
layer.list <- ogrListLayers(surge.db) # N.b., this is a vector, not R-style list
