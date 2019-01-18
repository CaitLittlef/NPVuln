####################
### FUTURE FIXES ###
####################



# !!!!!!!!!!!!
# MAJOR FIXME: in all caes, set st_transofrm to send single park to slr/low crs, not vice versa!!
# !!!!!!!!!!!!


# Consider faster python-based code for converting raster --> polygons.
# See post here below (says rasterToPolygons can be agonizingly slow):
# https://johnbaumgartner.wordpress.com/2012/07/26/getting-rasters-into-shape-from-r/

# Figure out area computation direction from rasters (zonal()).
# Rather than making rasters for all layers, maybe just mask out SLR/LOW on park raster?
# Consider parallelization if rasterizing tons of layers. See Scott's code below...

# # with for-loop:
# 
# library(maptools)
# library(raster)
# 
# treemaps <- list.files("~/Desktop/Work data/Species/Tree shapefiles", 
#                        pattern=".shp$", full.names=T)
# climdat <- raster("~/Documents/QERM/Lawler lab stuff/MeanTemp.tif")
# n <- length(treemaps)
# species_presence <- NULL
# 
# for(i in 1:n){
#   tree <- readShapePoly(treemaps[i])
#   tree.ras <- rasterize(tree, climdat, "CODE")
#   species_presence[i] <- length(tree.ras[tree.ras==1])
# }
# 
# 
# # parallel option:
# 
# treemaps <- list.files("~/Desktop/Work data/Species/Tree shapefiles", 
#                        pattern=".shp$", full.names=T)
# climdat <- raster("~/Documents/QERM/Lawler lab stuff/MeanTemp.tif")
# n <- length(treemaps)
# 
# cl <- makeCluster(4)
# registerDoParallel(cl)
# clusterExport(cl,c("treemaps","climdat","n","rasterize","readShapePoly"))
# species_presence_cl <- foreach(i=1:n, .combine=c) %dopar% {
#   tree <- readShapePoly(treemaps[i])
#   tree.ras <- rasterize(tree, climdat, "CODE")
#   length(tree.ras[tree.ras==1])
# }
# stopCluster(cl)