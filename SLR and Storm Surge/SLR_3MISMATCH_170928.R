# Identifies any projection mismatches or misses


############
### MHHW ###
############

# low.MHHW
# Visually ID'ed many weird ones below; forcing name format convention.
temp <- gsub(pattern = "AS_", x = layers.low.MHHW, replacement = "AS")
temp <- gsub(pattern = "CA_", x = temp, replacement = "CA")
temp <- gsub(pattern = "HI_", x = temp, replacement = "HI")
temp <- gsub(pattern = "Mauilow", x = temp, replacement = "Maui_low")
temp <- gsub(pattern = "Mauislr", x = temp, replacement = "Maui_slr")
temp <- gsub(pattern = "Mololow", x = temp, replacement = "Molo_low")
temp <- gsub(pattern = "Moloslr", x = temp, replacement = "Molo_slr")
temp <- gsub(pattern = "Oahulow", x = temp, replacement = "Oahu_low")
temp <- gsub(pattern = "Oahuslr", x = temp, replacement = "Oahu_slr")

# Split vector of formatted names by underscore; fill matrix with words.
tbl.low.MHHW <- strsplit(temp, split = "_") %>%
  unlist %>% matrix(ncol=3, byrow = TRUE) %>%
  as.data.frame()

# slr.MHHW
# Visually ID'ed many weird ones below; forcing name format convention.
temp <- gsub(pattern = "AS_", x = layers.slr.MHHW, replacement = "AS")
temp <- gsub(pattern = "CA_", x = temp, replacement = "CA")
temp <- gsub(pattern = "HI_", x = temp, replacement = "HI")
temp <- gsub(pattern = "Mauilow", x = temp, replacement = "Maui_low")
temp <- gsub(pattern = "Mauislr", x = temp, replacement = "Maui_slr")
temp <- gsub(pattern = "Mololow", x = temp, replacement = "Molo_low")
temp <- gsub(pattern = "Moloslr", x = temp, replacement = "Molo_slr")
temp <- gsub(pattern = "Oahulow", x = temp, replacement = "Oahu_low")
temp <- gsub(pattern = "Oahuslr", x = temp, replacement = "Oahu_slr")

# Split vector of formatted names by underscore; fill matrix with words.
tbl.slr.MHHW <- strsplit(temp, split = "_") %>%
  unlist %>% matrix(ncol=3, byrow = TRUE) %>%
  as.data.frame()

# See which mismatches exist in the MHHW layers
mismatch.mhhw.v1 <- anti_join(tbl.slr.MHHW, tbl.low.MHHW, by = "V1") # Gives rows in tbl1 but not tbl2.
print(mismatch.mhhw.v1)
# These are in slr.MHHW but not in low.MHHW
# V1  V2   V3
# 1   ASTau slr MHHW
# 2 CAChan2 slr MHHW
# 3 DRTO10m slr MHHW

mismatch.mhhw.v2 <- anti_join(tbl.low.MHHW, tbl.slr.MHHW, by = "V1") # Gives rows in tbl1 but not tbl2.
print(mismatch.mhhw.v2)
# No records are in low.MHHW but not in slr.MHHW


###########
### SLR ###
###########

# slr.85.2050
# Visually ID'ed many weird ones below; forcing name format convention:
temp <- gsub(pattern = "AS_", x = layers.slr.85.2050, replacement = "AS")
temp <- gsub(pattern = "CA_", x = temp, replacement = "CA")
temp <- gsub(pattern = "HI_", x = temp, replacement = "HI")
temp <- gsub(pattern = "Mauilow", x = temp, replacement = "Maui_low")
temp <- gsub(pattern = "Mauislr", x = temp, replacement = "Maui_slr")
temp <- gsub(pattern = "Mololow", x = temp, replacement = "Molo_low")
temp <- gsub(pattern = "Moloslr", x = temp, replacement = "Molo_slr")
temp <- gsub(pattern = "Oahulow", x = temp, replacement = "Oahu_low")
temp <- gsub(pattern = "Oahuslr", x = temp, replacement = "Oahu_slr")

# Split vector of formatted names by underscore; fill matrix with words.
tbl.slr.85.2050 <- strsplit(temp, split = "_") %>%
  unlist %>% matrix(ncol=4, byrow = TRUE) %>%
  as.data.frame()


# low.85.2050
# Visually ID'ed many weird ones below; forcing name format convention:
temp <- gsub(pattern = "AS_", x = layers.low.85.2050, replacement = "AS")
temp <- gsub(pattern = "CA_", x = temp, replacement = "CA")
temp <- gsub(pattern = "HI_", x = temp, replacement = "HI")
temp <- gsub(pattern = "Mauilow", x = temp, replacement = "Maui_low")
temp <- gsub(pattern = "Mauislr", x = temp, replacement = "Maui_slr")
temp <- gsub(pattern = "Mololow", x = temp, replacement = "Molo_low")
temp <- gsub(pattern = "Moloslr", x = temp, replacement = "Molo_slr")
temp <- gsub(pattern = "Oahulow", x = temp, replacement = "Oahu_low")
temp <- gsub(pattern = "Oahuslr", x = temp, replacement = "Oahu_slr")

# Split vector of formatted names by underscore; fill matrix with words.
tbl.low.85.2050 <- strsplit(temp, split = "_") %>%
  unlist %>% matrix(ncol=4, byrow = TRUE) %>%
  as.data.frame()
tbl.slr.85.2050$DOMAIN <- tbl.slr.85.2050$V1


mismatch.slr.v1 <- anti_join(tbl.slr.85.2050, tbl.low.85.2050, by = "V1") # Gives rows in tbl1 but not tbl2.
print(mismatch.slr.v1)
# These are in slr.85.2050 but not in low.85.2050:
# V1  V2 V3   V4
# 1   ASTau slr 85 2050
# 2 CAChan2 slr 85 2050
# 3 DRTO10m slr 85 2050

mismatch.slr.v2 <- anti_join(tbl.low.85.2050, tbl.slr.85.2050, by = "V1") # Gives rows in tbl1 but not tbl2.
print(mismatch.slr.v2)
# This is in low.85.2050 but not in slr.85.2050:
# V1  V2 V3   V4
# 1 JELA2 low 85 205 <-- THIS IS THE ONLY JELA2 layer

# Or see all rows and all columns for both tables; non-matching values gives NA
compare <- full_join(tbl.slr.85.2050, tbl.low.85.2050, by = "V1")
print(compare)


###################
### WEIRD NAMES ###
###################

df.parks <- parks ; st_geometry(df.parks) <- NULL

tbl.slr.85.2050$DOMAIN <- tbl.slr.85.2050$V1

compare2 <- full_join(df.parks[1:2], tbl.slr.85.2050, by = "DOMAIN")
print(compare2)

# These are all the projection domain codes that don't have a matching one in the parks database.
# ... and don't forget that the [DOMAIN]2 ones also have an associated [DOMAIN] which is incomplete.
# 116      <NA>   ACAD2   ACAD2  slr   85 2050
# 117      <NA>   ASOfu   ASOfu  slr   85 2050
# 118      <NA>   ASTau   ASTau  slr   85 2050
# 119      <NA>  ASTutu  ASTutu  slr   85 2050
# 120      <NA>   ASIS2   ASIS2  slr   85 2050
# 121      <NA>  CAChan  CAChan  slr   85 2050
# 122      <NA> CAChan2 CAChan2  slr   85 2050
# 123      <NA> DRTO10m DRTO10m  slr   85 2050
# 124      <NA>  DRTO5m  DRTO5m  slr   85 2050
# 125      <NA> EVER10m EVER10m  slr   85 2050
# 126      <NA>  EVER5m  EVER5m  slr   85 2050
# 127      <NA>   FOVA2   FOVA2  slr   85 2050
# 128      <NA>   GATE2   GATE2  slr   85 2050
# 129      <NA>    Guam    Guam  slr   85 2050
# 130      <NA>   GUIS2   GUIS2  slr   85 2050
# 131      <NA>    HIHw    HIHw  slr   85 2050
# 132      <NA>  HIMaui  HIMaui  slr   85 2050
# 133      <NA>  HIMolo  HIMolo  slr   85 2050
# 134      <NA>  HIOahu  HIOahu  slr   85 2050
# 135      <NA>   LEWI2   LEWI2  slr   85 2050
# 136      <NA>   NCAP2   NCAP2  slr   85 2050
# 137      <NA>   NCAP3   NCAP3  slr   85 2050
# 138      <NA>   PAIS2   PAIS2  slr   85 2050
# 139      <NA>   SNFR2   SNFR2  slr   85 2050


