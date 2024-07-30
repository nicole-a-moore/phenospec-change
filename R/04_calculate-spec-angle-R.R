## script to calcualte the spectral angle in R
library(tidyverse)
library(sf)
library(terra)
library(tidyterra)

## Note: must download Sentinel 2 data using Google Earth Engine script titled "get_s2_layers"

########################################################
##           compute spectral angle in R              ##
########################################################
## read in reference and target site polygons
ref <- st_read("data-processed/reference_sites/reference_sites.shp")
target <- st_read("data-processed/target_sites/target_sites.shp")

## function to calculate the spectral angle 
calc_spec_angle <- function(joined_df) {
  
  AAA = joined_df$B2*joined_df$RB2 +
    joined_df$B3*joined_df$RB3 +
    joined_df$B4*joined_df$RB4 +
    joined_df$B5*joined_df$RB5 +
    joined_df$B6*joined_df$RB6 +
    joined_df$B7*joined_df$RB7 +
    joined_df$B8*joined_df$RB8 +
    joined_df$B8A*joined_df$RB8A + 
    joined_df$B11*joined_df$RB11 +
    joined_df$B12*joined_df$RB12
  
  BBB = sqrt(joined_df$B2^2 +
               joined_df$B3^2 +
               joined_df$B4^2 +
               joined_df$B5^2 +
               joined_df$B6^2 +
               joined_df$B7^2 +
               joined_df$B8^2 +
               joined_df$B8A^2 + 
               joined_df$B11^2 +
               joined_df$B12^2)
  
  CCC = sqrt(joined_df$RB2^2 +
               joined_df$RB3^2 +
               joined_df$RB4^2 +
               joined_df$RB5^2 +
               joined_df$RB6^2 +
               joined_df$RB7^2 +
               joined_df$RB8^2 +
               joined_df$RB8A^2 + 
               joined_df$RB11^2 +
               joined_df$RB12^2)
  
  DIV = AAA/(BBB*CCC)
  DIV[DIV > 1 & !is.nan(DIV)] = 1
  
  DDD = acos(DIV)
  
  return(DDD)
}

## convert target and ref objects to spatVect
target$type = "target"
target <- vect(target)
ref$type = "ref"
ref <- vect(ref)

year = 1
years <- c(2017:2024)
spec_ang <- c()
## for each year 
while(year <= length(years)) {
  
  month = 1
  months = c("Jun", "Jul", "Aug", "Sept")
  ## for each month
  while(month <= 4) {
    
    ## read in Sentinel 2 data for the month
    ############################################
    filename = paste0("data-processed/GEE/Sentinel2/", years[year], "_", months[month], "_Sentinel2.tif")
    
    if(file.exists(filename)) {
      s2 <- rast(filename)
      
      ## mask the raster by the ref and target polygons 
      s2 <- mask(s2, target)
      #plot(s2)
      
      # # plot a single ref polygon with raster cells below to see it:
      # polys_one <- target %>%
      #   filter(poly_num == "RefPolygon 2")
      # 
      # ext = st_bbox(polys_one)
      # 
      # ## transform raster to df
      # xy <- as.data.frame(s2, xy=TRUE)
      # 
      # xy %>%
      #   ggplot(aes(x = x, y = y)) +
      #   geom_tile(aes(fill = B2)) +
      #   coord_fixed() +
      #   geom_sf(data = polys_one, inherit.aes = FALSE, fill = "transparent") +
      #   scale_x_continuous(limits = c(ext$xmin, ext$xmax)) +
      #   scale_y_continuous(limits = c(ext$ymin, ext$ymax))
      
      ## calculate mean spectral emittance for each band per reference and target polygon
      ###################################################################################
      target_ext <- extract(s2, target, fun = mean, na.rm = TRUE, ID = F)
      ref_ext <- extract(s2, ref, fun = mean, na.rm = TRUE, ID = F)
      
      target_ext$poly_num = target$poly_num
      ref_ext$ref_poly_num = ref$poly_num
      
      ## rename bands in ref so they start with an R
      colnames(ref_ext)[1:(ncol(ref_ext)-1)] <- paste0("R", colnames(ref_ext)[1:(ncol(ref_ext)-1)])
      
      ## calculate spectral angle 
      ###########################
      ## for each ref site 
      ls <- list()
      i=1
      while(i <= nrow(ref_ext)) {
        ## join into a single object 
        ref_ext_temp <- ref_ext[i,]
        
        join <- cross_join(target_ext, ref_ext_temp)
        
        ## calculate spectral angle 
        ls[[i]] <- calc_spec_angle(join)
        
        i = i + 1
      }
      
      #################################################
      ##                save data                    ##
      #################################################
      ## join with original data 
      bound <- c()
      for(i in ls) {
        bound = cbind(bound, i)
      }
      colnames(bound) <- unique(ref_ext$ref_poly_num)
      join <- cbind(join, bound)
      
      df <- left_join(target, join) %>%
        as.data.frame() %>%
        gather(key = "ref", value = "spec_angle", colnames(bound)) %>% 
        mutate(month = months[month], 
               year = years[year]) 
      
      spec_ang = rbind(spec_ang, df)
    }
    else {
      
    }
    
    month = month + 1
  }
  
  year = year + 1
}

spec_ang %>%
  filter(poly_num %in% c("RefPolygon 1", "RefPolygon 2", "RefPolygon 3")) %>%
  select(spec_angle, poly_num, month, ref) %>% 
  View() 
## sanity check: ref sites compared to themselves have spec angle ~= 0

na_polys <- unique(spec_ang$poly_num[which(is.na(spec_ang$spec_angle))])
length(na_polys) #no. unique polys that have no spec angle data for some months 

## remove ones with no spectral band data 
spec_ang <- filter(spec_ang, !is.nan(spec_angle))

## calculate average across months per polygon
spec_ang_test <- spec_ang %>%
  select(-ref_poly_num, -ref_id) %>% 
  distinct() %>%
  group_by(poly_num, ref, year) %>%
  mutate(Avg_SA = mean(spec_angle)) %>%
  ungroup() %>% 
  spread(key = "month", value = "spec_angle") %>% 
  gather(key = "month", value = "spec_angle", 
         c("August", "July", "June", "September", "Avg_SA")) 

## write:
write.csv(spec_ang, "data-processed/spectral-angle-R.csv", row.names = FALSE)

