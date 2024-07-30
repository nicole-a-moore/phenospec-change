## filter IEQM sites 
## must: be inside the area of interest, be inside the quebec agricultural zone, have been totally cut at least once, not have been cut again, have known forest cover type  
library(sf)
library(tidyverse)
theme_set(theme_bw())

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
##               read in IEQM sites             ## 
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## read in IEQM permanent sites
#################################
pep_path <- "data-raw/carte_ecoforestiere/PEP_GPKG/PEP.gpkg"
lrs <- sf::st_layers(pep_path)
pep <- sf::st_read(pep_path, layer = lrs$name[1]) # placette - contains high level metadata, including coordinates of permanent plots
peeori <- sf::st_read(pep_path, layer = lrs$name[29]) #pee_ori_sond - contains derived variables for the stand in which the plot falls  

## add geographic information 
peeori <- peeori %>%
  dplyr::left_join(pep, by = "id_pe") %>%
  sf::st_as_sf() 

## read in IEQM polygons
#################################
## read in the first tile 
poly_path <- "data-raw/carte_ecoforestiere/CARTE_ECO_MAJ_31I_10.gdb"
lrs <- sf::st_layers(poly_path)
poly_pep <- sf::st_read(poly_path, layer = lrs$name[2]) # placette - contains high level metadata, including coordinates of permanent plots
poly_pep <- st_as_sf(poly_pep) 

## add metadata 
poly_meta <- sf::st_read(poly_path, layer = lrs$name[3]) %>%
  select(GEOC_MAJ, AN_PRO_ORI)
poly_pep <- st_join(poly_pep, poly_meta, left = TRUE) %>%
  select(-GEOC_MAJ.y) %>%
  rename("GEOC_MAJ" = GEOC_MAJ.x)

## add the second tile  
poly_path <- "data-raw/carte_ecoforestiere/CARTE_ECO_MAJ_31H_10.gdb"
lrs <- sf::st_layers(poly_path)
poly_pepH <- sf::st_read(poly_path, layer = lrs$name[2]) # placette - contains high level metadata, including coordinates of permanent plots
poly_pepH <- st_as_sf(poly_pepH) 

## add metadata 
poly_metaH <- sf::st_read(poly_path, layer = lrs$name[3]) %>%
  select(GEOC_MAJ, AN_PRO_ORI)
poly_pepH <- st_join(poly_pepH, poly_metaH) %>%
  select(-GEOC_MAJ.y) %>%
  rename("GEOC_MAJ" = GEOC_MAJ.x)

## join tiles and remove second tile object
poly_pep <- rbind(poly_pep, poly_pepH)
rm(list = ls(poly_pepH))

## add the third tile  
poly_path <- "data-raw/carte_ecoforestiere/PRODUITS_IEQM_21E_10/PRODUITS_IEQM_21E_10_C.gdb"
lrs <- sf::st_layers(poly_path)
poly_pepE <- sf::st_read(poly_path, layer = lrs$name[2]) # placette - contains high level metadata, including coordinates of permanent plots
poly_pepE <- st_as_sf(poly_pepE) 

## add metadata 
poly_metaE <- sf::st_read(poly_path, layer = lrs$name[3]) 
poly_metaE <- st_as_sf(poly_metaE) %>%
  select(GEOCODE, AN_PRO_ORI) 
poly_pepE <- st_join(poly_pepE, poly_metaE) %>%
  select(-GEOCODE.y) %>%
  rename("GEOCODE" = GEOCODE.x)

## join tiles and remove third tile object
poly_pep <- bind_rows(poly_pep, poly_pepE)
rm(list = ls(poly_pepE))

## add the fourth tile  
poly_path <- "data-raw/carte_ecoforestiere/PRODUITS_IEQM_21L_10/PRODUITS_IEQM_21L_10_C.gdb"
lrs <- sf::st_layers(poly_path)
poly_pepL <- sf::st_read(poly_path, layer = lrs$name[2]) # placette - contains high level metadata, including coordinates of permanent plots
poly_pepL <- st_as_sf(poly_pepL) 

## add metadata 
poly_metaL <- sf::st_read(poly_path, layer = lrs$name[3]) 
poly_metaL <- st_as_sf(poly_metaL) %>%
  select(GEOCODE, AN_PRO_ORI) 
poly_pepL <- st_join(poly_pepL, poly_metaL) %>%
  select(-GEOCODE.y) %>%
  rename("GEOCODE" = GEOCODE.x)

## join tiles and remove third tile object
poly_pep <- bind_rows(poly_pep, poly_pepL)
rm(list = ls(poly_pepL))

## get colnames 
colnames(peeori)
colnames(poly_pep)
colnames(poly_pep) <- tolower(colnames(poly_pep))
st_geometry(poly_pep) <- "shape"
colnames(peeori) %in% colnames(poly_pep)
## id_pe = plot id 
## no_mes = measurement number within plot
## id_pe_mes = unique id for each measurement
## gr_ess = species grouping ()
## origine = original disturbance
## an_origine = year of original disturbance
## perturb = partial disturbance 
## an_perturb = year of partial disturbance 
## reb_ess1/2/3 = species used to reforest 
## type_couv = major canopy type
## an_pro_ori = year of photo used to measure variales 

## filter to dry land 
poly_pep <- filter(poly_pep, type_ter == "TRF") 
peeori <- filter(peeori, type_ter == "TRF")

## add variable for age of perturbation 
# the column an_pro_ori tells us the year associated with the satellite photo used
# so age since clearing = satellite photo - year of cutting 
peeori$age_perturb <- as.numeric(peeori$an_pro_ori) - as.numeric(peeori$an_origine)
poly_pep$age_perturb <- as.numeric(poly_pep$an_pro_ori) - as.numeric(poly_pep$an_origine)

# # map the data
# poly_pep %>%
#   ggplot() +
#   geom_sf(size = 0.5) +
#   geom_sf(data = peeori) 

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
##  1. crop to area of interest  ## 
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
aoi <- st_read("data-raw/aoi.gpkg")

ggplot() +
  geom_sf(data = aoi) 

## crop IEQM permanent samples to AOI
peeori <- st_intersection(peeori, aoi) 
## crop polygons to AOI
poly_pep <- st_intersection(poly_pep, aoi) 


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
##  2. remove sites with no canopy cover variable or a second disturbance   ## 
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## filter out stands with no forest cover type
poly_pep <- filter(poly_pep, !is.na(type_couv)) 
peeori <- filter(peeori, !is.na(type_couv)) 

## filter out sites with a second perturbation
length(which(!is.na(peeori$an_perturb))) ## 21
length(which(!is.na(poly_pep$an_perturb))) ## 5477
poly_pep <- filter(poly_pep, is.na(an_perturb)) 
peeori <- filter(peeori, is.na(an_perturb)) 


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
##  3. remove sites in non-agriculutural areas  ## 
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## read in Quebec zone agricole shapefiles:
za_p <- st_read("data-raw/zone_agricole/zonage/zonage_p.shp")

## filter polygons to only agricultural areas
za_p <- filter(za_p, DEFINITION == "Zone agricole")

## change CRS
za_p <- st_transform(za_p, st_crs(poly_pep))

## plot
ggplot(data = za_p, aes(fill = DEFINITION)) +
  geom_sf()
mapview::mapview(za_p, zcol = "DEFINITION")

## dissolve into one big polygon
dissolve_sf <- st_union(za_p)

## subset IEQM sites to ones falling in within the agricultural zone
overlap_peeori = which(st_intersects(peeori, dissolve_sf, sparse = FALSE) == TRUE)
peeori <- peeori[as.vector(overlap_peeori), ]

overlap_poly = which(st_intersects(poly_pep, dissolve_sf, sparse = FALSE) == TRUE)
poly_pep <- poly_pep[as.vector(overlap_poly), ]

nrow(peeori) # 358
nrow(poly_pep) # 48852

## plot sites on top of zone agricole
ggplot() +
  geom_sf(data = za_p, aes(fill = DEFINITION)) +
  geom_sf(data = aoi, fill = "red") +
  geom_sf(data = poly_pep, colour = "blue", alpha = 0.5) +
  geom_sf(data = peeori, size = 0.5, colour = "black") +
  scale_fill_manual(values = c("grey")) 


## ~~~~~~~~~~~~~~~~~~~ ##
##  4. classify sites  ## 
## ~~~~~~~~~~~~~~~~~~~ ##
## make variable based on whether landsat data is fully available since clearing
peeori <- peeori %>%
  mutate(landsat_avail_full = ifelse(as.numeric(an_origine) > 1972 | is.na(an_origine), 1, 0)) 
poly_pep <- poly_pep %>%
  mutate(landsat_avail_full = ifelse(as.numeric(an_origine) > 1972 | is.na(an_origine), 1, 0)) 

nrow(peeori) #358
nrow(poly_pep) #48852

## classify sites as either:
## chronosequence - totally cleared < 30 years ago 
## reforested reference - totally cleared > 30 years ago 

## note: an "origine" perturbation is an intervention that removed >75% of the merchantable surface area
poly_classified <- poly_pep %>%
  ## filter to only sites that were cleared + for which we know when they were cleared
  filter(origine == "CT" & !(origine == "CT" & is.na(an_origine))) %>%
  mutate(age_perturb = as.numeric(age_perturb), ## convert variable to numeric
         an_origine = as.numeric(an_origine)) %>%
  mutate(site_type = ifelse(origine == "CT" & an_origine <= 1994, 
                            "reforested reference",
         ifelse(origine == "CT" &  an_origine >= 1994,  
                "chronosequence site", NA)))

peeori_classified <- peeori %>%
  filter(origine == "CT" & !(origine == "CT" & is.na(an_origine))) %>%
  mutate(age_perturb = as.numeric(age_perturb), ## convert variable to numeric
         an_origine = as.numeric(an_origine)) %>%
  mutate(site_type = ifelse(origine == "CT" & an_origine <= 1994, 
                            "reforested reference",
                            ifelse(origine == "CT" &  an_origine >= 1994,  
                                   "chronosequence site", NA)))


## ~~~~~~~~~~~~~~~~~~~ ##
##  6. visualize       ## 
## ~~~~~~~~~~~~~~~~~~~ ##
peeori_classified %>%
  ggplot(aes(y = site_type)) +
  geom_bar()

poly_classified %>%
  ggplot(aes(y = site_type)) +
  geom_bar()

ggplot() +
  geom_sf(data = peeori_classified, aes(fill = site_type), alpha = 0.5) +
  geom_sf(data = poly_classified, aes(fill = site_type), size = 0.5)

## write out to visualize in QGIS
sf::st_write(poly_classified, "data-processed/potential_sites/potential_sites.shp",
             append = FALSE)
sf::st_write(peeori_classified, "data-processed/IEQM_permanentplots/IEQM_permanentplots.shp", append = FALSE)

