## prepare ref and target site shapefiles using IEQM sites 
library(tidyverse)
library(sf)
library(terra)
library(tidyterra)

######################################################################
##                prepare reference & target site files             ##
######################################################################
## read in the subset of IEQM polygon data created in script 01
ieqm <- st_read("data-processed/potential_sites/potential_sites.shp")
colnames(ieqm)

## define reference sites as sites that were clear cut before 1974 (>50 years ago)
ref <- filter(ieqm, an_orgn <= 1974)

## and target sites as sites that were clear cut after 1994 (<30 years ago)
target <- filter(ieqm, an_orgn >= 1994)

## add polygon number
ref$poly_num <- paste0("RefPolygon ", 1:nrow(ref))
target$poly_num <- paste0("TargPolygon ", 1:nrow(target))

nrow(ref) # 3 refs
nrow(target) # 705 targets

## add ref polygons to target file
target <- rbind(target, ref)

## and extent file
aoi <- st_read("data-raw/aoi.gpkg")

## reproject all the files to EPSG:4326
ref <- st_transform(ref, "EPSG:4326")
target <- st_transform(target, "EPSG:4326")
aoi <- st_transform(aoi, "EPSG:4326")

## assign unique id to ref sites
ref$ref_id <- 1:nrow(ref)

## assign specific reference sites to target site using their unique ref id
## for now, compare each ref site to each target site
ref_id <- data.frame(ref_id = 1:3)
target = cross_join(target, ref_id) ## joins all combinations of rows 

## get area of sites 
mean(target$superfc)
## mean area of 6.066667
hist(target$superfc)

## write files:
st_write(ref, "data-processed/reference_sites/reference_sites.shp",
         append = FALSE)
st_write(target, "data-processed/target_sites/target_sites.shp",
         append = FALSE)
st_write(aoi, "data-processed/aoi/aoi.shp",
         append = FALSE)

## upload files to Google Earth Engine as assets and run script 
