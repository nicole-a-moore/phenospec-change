## prepare ref and target site shapefiles using coulees agricoles polygons
library(tidyverse)
library(sf)
library(terra)
library(tidyterra)

######################################################################
##                prepare reference & target site files             ##
######################################################################
## read in IEQM polygon data (to use for reference sites)
ieqm <- st_read("data-processed/potential_sites/potential_sites.shp")
colnames(ieqm)

## read in coulees agricoles polygon data
coulees <- st_read("data-raw/coulees_agricoles/chrono_poly/chrono_poly1.shp") %>%
  select(SiteCode, Delin, geometry)
colnames(coulees)
## and attributes
kobo <- read.csv("data-raw/coulees_agricoles/SITES_terrain2024_Kobo_export.csv")
coulees <- left_join(coulees, kobo)

## clean some columns
coulees$Type.de.sites <- ifelse(coulees$Type.de.sites == "Reference ", "Reference", 
                                coulees$Type.de.sites)
coulees$Type.foret = ifelse(is.na(coulees$Type.foret), "Unknown", coulees$Type.foret)
coulees$Age = ifelse(coulees$Age == "?" | is.na(coulees$Age), "Unknown",
                     ifelse(coulees$Age %in% c("ref", "Ref"), "Reference site", coulees$Age))

## reproject 
coulees = st_transform(coulees, crs(ieqm))

## rename object
target <- coulees

## reference site = IEQM sites that were clear cut before 1974 (>50 years ago)
ref1 <- filter(ieqm, an_orgn <= 1974)
## and also polygons in the coulees shapefile that are labelled as ref
ref2 <- filter(target, Type.de.sites == "Reference")
## bind IEQM + coulees ref sites 
ref1 <- ref1 %>%
  rename("SiteCode" = geocode) %>%
  mutate(Delin = "") %>%
  select(SiteCode, Delin, geometry)
ref2 <- ref2 %>%
  mutate(Delim = "") %>%
  select(SiteCode, Delin, geometry)
ref <- rbind(ref1, ref2)

## remove coulees ref sites from target shapefile
target <- target %>%
  select(SiteCode, Delin, geometry) %>% 
  filter(!SiteCode %in% ref$SiteCode)

## add unqique polygon number
ref$poly_num <- paste0("RefPolygon ", 1:nrow(ref))
target$poly_num <- paste0("TargPolygon ", 1:nrow(target))

nrow(ref) # 8
nrow(target) # 21

## add IEQM + coulees ref sites back to target file 
target <- rbind(target, ref)
nrow(target) # 29

## make area of interest file from bbox of target shape 
bbox <- st_bbox(target) 

xmin = bbox$xmin - 100
xmax = bbox$xmax + 100
ymin = bbox$ymin - 100
ymax = bbox$ymax + 100

aoi <- st_polygon(list(cbind(c(xmin,xmax,xmax,xmin,xmin),
                             c(ymin,ymin,ymax,ymax,ymin)))) %>%
  st_sfc(crs = st_crs(target))

# aoi %>%
#   ggplot() +
#   geom_sf() +
#   geom_sf(data = target) +
#   geom_sf(data = ref, fill = "red") 

## reproject to EPSG:4326
ref <- st_transform(ref, "EPSG:4326")
target <- st_transform(target, "EPSG:4326")
aoi <- st_transform(aoi, "EPSG:4326")

## assign unique id to ref sites
ref$ref_id <- ref$SiteCode

## assign specific reference sites to target site using their unique ref id
## for now, compare each ref site to each target site
ref_id <- data.frame(ref_id = unique(ref$SiteCode))
target = cross_join(target, ref_id)

## write files:
st_write(ref, "data-processed/reference_sites_coulees/reference_sites_coulees.shp",
         append = FALSE)
st_write(target, "data-processed/target_sites_coulees/target_sites_coulees.shp",
         append = FALSE)
st_write(aoi, "data-processed/aoi_coulees/aoi_coulees.shp",
         append = FALSE)

## upload to Google Earth Engine as assets and run script 
