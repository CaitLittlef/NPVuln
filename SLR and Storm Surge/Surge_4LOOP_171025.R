# Create list of future park shapefiles that have undergone SLR.
# I've had trouble with st_read in non-geodatabase...
# ...so here I'm keeping entire file paths to reference.
slr.parks <- list.files("//main.sefs.uw.edu/main/Space/Lawler/Shared/Julia/NPVuln/GIS/NPS/SLR/SLR_Compute",
                           pattern="slr.85.2050.shp$", # $ should stop text search
                           full.names = T) # Pulls full file name



#################
### LOOP PREP ###
#################

# Create repository list for area computation; list good if length unknown.
list.park.name <- list()
list.park.area.85.2050 <- list() # (fyi, already computed from SLR in slr results)
list.park.area.85.2050.surge <- list()

# Get all index values for every park -- this is from surge-generated df
loop.ready <- (seq_along(df.parks$UNIT_CODE))

# CUIS has z dimension which flunks area computation, so run separately.
loop.ready <- loop.ready[-c(23)]
# Turn on removal of z function above code that detects are and run with...
# i <- 23

# Function erases all of y from x; not sure why but don't union x (or else get all parks)
st_erase = function(x, y) st_difference(x, st_union(st_combine(y)))

############
### LOOP ###
############
remove(i)
for(i in loop.ready){ 

start <- Sys.time()

# Load park shapefile
temp <- df.parks$UNIT_CODE[i]
temp2 <- grep(pattern=temp, x=slr.parks, value=T)
slr.park.poly <- st_read(paste0(temp2)) %>%
  st_buffer(dist = 0) # fix invalid geometries (warning re: lat/long vs. dd)

# Load surge layers based on look-up tbl. (There's always at least the first!)
surge1 <- st_read(surge.db, parks$SURGE_1[i]) %>%
  st_transform(st_crs(slr.park.poly)) %>% # set projections to match park crs
  st_buffer(dist = 0) %>% # should't be any invalid geometries but JIC...
  st_cast() # make sure it's a polygon or multipolygon -- geometries crash later.

# If there's a 2nd surge layer, load it or set surge2 to NULL, etc...
if (!is.na(df.parks$SURGE_2[i])) {
  surge2 <- st_read(surge.db, parks$SURGE_2[i]) %>%
    st_transform(st_crs(slr.park.poly)) %>%
    st_buffer(dist = 0) %>%
    st_cast() 
} else
  surge2 <- NULL

if (!is.na(df.parks$SURGE_3[i])) {
  surge3 <- st_read(surge.db, parks$SURGE_3[i]) %>%
    st_transform(st_crs(slr.park.poly)) %>%
    st_buffer(dist = 0) %>%
    st_cast() 
} else
  surge3 <- NULL

if (!is.na(df.parks$SURGE_4[i])) {
  surge4 <- st_read(surge.db, parks$SURGE_4[i]) %>%
    st_transform(st_crs(slr.park.poly)) %>%
    st_buffer(dist = 0) %>%
    st_cast() 
} else
  surge4 <- NULL

if (!is.na(df.parks$SURGE_5[i])) {
  surge5 <- st_read(surge.db, parks$SURGE_5[i]) %>%
    st_transform(st_crs(slr.park.poly)) %>%
    st_buffer(dist = 0) %>%
    st_cast() 
} else
  surge5 <- NULL

if (!is.na(df.parks$SURGE_6[i])) {
  surge6 <- st_read(surge.db, parks$SURGE_6[i]) %>%
    st_transform(st_crs(slr.park.poly)) %>%
    st_buffer(dist = 0) %>%
    st_cast() 
} else
  surge6 <- NULL

# Alt: could try to catch errors while loading, but that takes a few seconds each. Checking if layer exists above is faster.
# surge6 <- tryCatch({st_read(surge.db, parks$SURGE_6[i])}, error = function(e){return(0)}) # sets surge6 to zero
# surge6 <- tryCatch({st_read(surge.db, parks$SURGE_6[i])}, error = function(e){"ERROR: I don't exist!"}) # gives NULL surge6

# Subtract surges from park shapefile but only if they're not NULL!
# And check to see if park is compltely inundated (i.e., geometry of zero) at same time.
if (length(slr.park.poly$geometry) > 0 && length(surge1) > 0 ) {
  park.surge <- st_erase(slr.park.poly, surge1)
} else
  park.surge <- slr.park.poly

if (length(park.surge$geometry) > 0 && length(surge2) > 0) {
  park.surge <- st_erase(park.surge, surge2)
} else
  park.surge <- park.surge

if (length(park.surge$geometry) > 0 && length(surge3) > 0 ) {
  park.surge <- st_erase(park.surge, surge3)
} else
  park.surge <- park.surge

if (length(park.surge$geometry) > 0 && length(surge4) > 0 ) {
  park.surge <- st_erase(park.surge, surge4)
} else
  park.surge <- park.surge

if (length(park.surge$geometry) > 0 && length(surge5) > 0 ) {
  park.surge <- st_erase(park.surge, surge5)
} else
  park.surge <- park.surge

if (length(park.surge$geometry) > 0 && length(surge6) > 0 ) {
  park.surge <- st_erase(park.surge, surge6)
} else
  park.surge <- park.surge 

# Turn this on for CUIS (i=23), which has z geometry; remove for area computation
# slr.park.poly <- st_zm(slr.park.poly, drop = TRUE, what = "ZM")
# park.surge <- st_zm(park.surge, drop = TRUE, what = "ZM")


# Compute areas and write to lists
list.park.name[[i]] <- parks$UNIT_CODE[i]
list.park.area.85.2050[[i]] <- round(as.numeric(st_area(slr.park.poly)),2)
list.park.area.85.2050.surge[[i]] <- if(length(park.surge$geometry)==0){0
} else
  round(as.numeric(st_area(park.surge)),2)

# Output folder if I generate shapefiles
setwd("//main.sefs.uw.edu/main/Space/Lawler/Shared/Julia/NPVuln/GIS/NPS/StormSurge/Surge_Compute")
# Write as new shapefile in directory Surge_Compute
if(length(park.surge$geometry)==0){cat(paste0(parks$UNIT_CODE[i], " is underwater ><(((°>"))
} else
  st_write(park.surge, paste0(parks$UNIT_CODE[i],"_surge.85.2050.shp"))
# Reset wd
setwd("//main.sefs.uw.edu/main/Space/Lawler/Shared/Julia/NPVuln")

print(paste0("Done with park ", parks$UNIT_CODE[i], "."))
print(Sys.time() - start)
}


# Remove unnecessary thangs
remove(slr.park.poly, park.surge, temp, temp2, start, i,
       surge1, surge2, surge3, surge4, surge5, surge6)


##########################
### MAKE RESULTS TABLE ###
##########################

# Set any NULLS in list to NA so they don't disappear
list.park.name <- sapply(list.park.name, function(x) ifelse(x == "NULL", NA, x))
list.park.area.85.2050 <- sapply(list.park.area.85.2050, function(x) ifelse(x == "NULL", NA, x))
list.park.area.85.2050.surge <- lapply(list.park.area.85.2050.surge, function(x) ifelse(x == "NULL", NA, x))

# Turn them into matrices; dunno what this does. t tranposes.
library(plyr)
mat1 <- (plyr::rbind.fill.matrix(lapply(list.park.name, t)))
mat2 <- (plyr::rbind.fill.matrix(lapply(list.park.area.85.2050, t)))
mat3 <- (plyr::rbind.fill.matrix(lapply(list.park.area.85.2050.surge, t)))
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
                area.85.2050.m2 = names(.)[2],
                area.85.2050.surge.m2 = names(.)[3]) %>%
  mutate(area.surge.loss.m2 = (area.85.2050.m2 - area.85.2050.surge.m2)) %>%
  mutate(area.surge.loss.perc = (area.surge.loss.m2 / area.85.2050.m2))


# Write results to csv
currentDate <- Sys.Date()
csvFileName <- paste0("loop_results_surge_v3_",currentDate,".csv")
write.csv(df.results, paste0("//main.sefs.uw.edu/main/Space/Lawler/Shared/Julia/NPVuln/GIS/NPS/StormSurge/Surge_Compute/",csvFileName))

remove(currentDate)
remove(csvFileName)













# 
# ################ THIS IS ALL FAILING #########################
# # I ultimately want to be able to reference only surge layers that AREN'T NULL.
# # Create quick ref table with surge names and length (NULL will have length of zero)
# 
# (surge.name <- c("surge1", "surge2", "surge3", "surge4", "surge5", "surge6"))
# (length <- c(length(surge1), length(surge2), length(surge3), length(surge4), length(surge5), length(surge6)))
# (surges <- as.data.frame(cbind(surge.name, length))) ; surges$length <- as.numeric(as.character(surges$length))
# # Drop those surge layers that are NULL placeholders
# (surges <- surges[(surges$length>0),])
# # Grab index values for these surge layres in this little reference table.
# (surge.ready <- seq_along(surges$surge.name))
# (surge.layer <- as.vector((surges$surge.name[surge.ready])))
# 
# for(s in surge.ready)
#   park.surge.test <- st_erase(slr.park.poly, surge.layer[s]) # still just a character
# 
# paste0(surges$surge.num[s])
# s <- 2
# surge.layer[s]
# layer.list[3]
# 
# #################################################################
