These are scripts for computing park area lost to sea-level rise, low-lying area inundation, and storm surge.
------------------------------------------

* In all cases, run setup and check to make sure lists get created for loading computed areas!!
* In all cases, check output .csv names
* Future fix: Instead of setting CRS of multiple slr/low layers to match that of the park, set park to match CRS of those layers (usually a UTM zone) and then transform back to common park CRS.

*******************************************
SLR_1Setup: Sets working directory, installs packages, loads databases and park shapefile, applies look-up table, generates reference lists.

SLR_2GEOMETRIES: Checks for any null geometries and numbers of features in each slr/low layer.

SLR_3MISMATCH: Because the loop looks for several types of layers per park, need to know when/if any layers are missing.

SLR_4LOOP: If geometries check out and there aren't any missing layers, loop runs through all parks to:

1) create a baseline park by subtracting MHHW,
2) subtract out area that is covered in water in 2050 under rcp 8.5 due to SLR and low-lying area inundation,
3) compute the area for baseline park, future park area, and percentage loss.

Code at bottom lets user create csv from lists now filled with areas and percentages lost.

SLR_5WEIRDOS: Individual code chunks for parks that have huge geometries, mismatches, multiple areas (e.g,. Acadia has two zones), or z-dimensions.

SLR_6JELA-RASTER: New Orleans slr/low projections had maaaaany features so converting to raster, doing raster overlay, and then creating back to shapefile was best course of action.

SLR_9FIXMES: Some ideas for speeding up code including reversing CRS transformation, using Python code for converting raster to polygons, using zonal() for raster area computations, maybe parallelizing for rasterizing.

*******************************************
Surge_1SETUP_171021: Comparable to SLR set-up above, but for surge. N.b., instead of pulling in layers based on matching park to associated domain, this code references a look-up table that explicitly names the surge layers to pull for each park.

Surge_2GEOMETRIES: Surge layer look-up table was informed by any null geometries or layers with zero features that were identified in this script.

Surge_3MISMATCH: Surge layer look-up table was informed by any mismatches that showed up with this script.

Surge_4LOOP: Loops through each park, and pulls in associated surge layers IF those layers exist, else move on.

*******************************************
SLRSurge_BICY-EVER-RASTER-5m: AVOID. Park BICY uses EVER domain for slr/low layers, some of which are at 5m resolution. This keeps failing -- have to set new temp directory or else server temp directory gets too full. Still, some memory (RAM) failures flunk the code. But, this version tries to add one raster at a time, use it, then remove it. Repeat. Very un-sleek and slow. Instead, use...

SLRSurge_BICY-EVER-RASTER-10m: Computes SLR/low for BICY using EVER domain layers and rasterizing all data to 10m which is much faster and yet seems to have similar results to 5m. While future park is loaded, does surge computation, too, using BICY-specific surge projections. No park shapefiles generated.

SLRSurge_EVER-EVER-RASTER-10m: Computes SLR/low for EVER using EVER domain layers and rasterizing all data to 10m which is much faster and yet seems to have similar results to 5m. While future park is loaded, does surge computation, too. No park shapefiles generated because too damn big. 

