## analyze the IEQM site spectral angle data 
library(tidyverse)
library(sf)
library(terra)
library(broom)
theme_set(theme_bw())

########################################################
##          analyze GEE spectral angle files          ##
########################################################
## read in spectral angle files from GEE script and combine them
## there is one file per month x year
## files might be missing when there is no Sentinel 2 data within that month + year for any of the sites 
spec_df <- c()
years <- seq(from = 2017, to = 2024, by = 1)
y = 1
## loop through years 
while(y <= length(years)) {
  year = years[y]
  
  spec <- c()
  months <- c("June", "July", "August", "September")
  ## loop through months 
  for(m in months) {
    filename = paste0("data-processed/GEE/spec-ang-files_IEQM/", year, 
                      "_SpectralAngle_", m, ".shp")
    
    ## check if there is data for that month + year
    if(file.exists(filename)) {
      ## if there is, join it to a data frame  
      if(is.null(spec)) {
        spec <- st_read(filename) %>% st_drop_geometry()
      }
      else {
        spec <- inner_join(spec, st_drop_geometry(st_read(filename)))
      }
    }
  }
  
  ## gather the monthly data from the year current, add a year column
  if(!is.null(spec)) {
    num_cols <- length(which(str_detect(colnames(spec), "\\_SA")))
    spec <- select(spec, PolygonNam, ends_with("SA"), everything())
    spec <- gather(spec, key = "month", value = "spectral_angle", 2:(num_cols+1))
    spec$year <- year
    
    if(is.null(spec_df)) {
      spec_df <- spec
    }
    else {
      spec_df <- rbind(spec_df, spec)
    }
    
  }

  y = y + 1
}

names(spec_df)
length(unique(spec_df$PolygonNam)) ## 677

## calculate an average spectral angle per site per year 
spec_df <- spec_df %>%
  group_by(PolygonNam, year, Ref_id) %>%
  mutate(Avg_SA = mean(spectral_angle)) %>%
  spread(key = "month", value = "spectral_angle") %>%
  gather(key = "month", value = "spectral_angle", 
         c("Aug_SA", "Jul_SA", "Jun_SA", "Sept_SA", "Avg_SA")) %>%
  distinct()

## join with site information 
target <- st_read("data-processed/target_sites/target_sites.shp") %>%
  select(-ref_id) %>%
  distinct()
ref <- st_read("data-processed/reference_sites/reference_sites.shp")
spec_df <- left_join(st_drop_geometry(target), spec_df, c("poly_num" = "PolygonNam")) 

## some polygons don't have spectral angle values
length(which(is.na(spec_df$spectral_angle))) ## 563

## get rid of missing data
spec_df <- filter(spec_df, !is.na(spectral_angle))

## calculate years since clearing
spec_df$yrs_since_clearing <- 2024 - spec_df$an_orgn

## change values for reference poly column 
spec_df$Ref_id = paste0("Reference site ", spec_df$Ref_id)

#write.csv(spec_df, "data-processed/S2-spec-angle_2017-2024_IEQM.csv", row.names = F)
spec_df <- read.csv("data-processed/S2-spec-angle_2017-2024_IEQM.csv")

## check that there are the right number of observations
# spec_df %>%
#   group_by(Ref_id, month, poly_num, year) %>%
#   select(Ref_id, month, poly_num, year) %>%
#   tally() %>% View()
  

## plot against time 
####################################
## make faceting variable for better visualization
spec_df <- select(spec_df, poly_num) %>%
  unique() %>%
  mutate(facet_group = rep(1:20, nrow(.))[1:length(unique(spec_df$poly_num))]) %>%
  left_join(spec_df, .)

## plot SA from 2021 and use chronosequence as time (years since clearing)
spec_df %>%
  filter(year == 2021) %>%
  filter(month != "Avg_SA") %>%
  filter(!str_detect(poly_num, "Ref")) %>%
  ggplot(aes(x = yrs_since_clearing, y = spectral_angle, colour = month)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~Ref_id)

spec_df %>%
  filter(year == 2021) %>%
  filter(month != "Avg_SA") %>%
  filter(!str_detect(poly_num, "Ref")) %>%
  ggplot(aes(x = yrs_since_clearing, y = spectral_angle, colour = month)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.5) +
  theme(legend.position = "none") +
  facet_wrap(~Ref_id)

## plot SA measures data from different months and years within sites
## average per year
spec_df %>%
  filter(month != "Avg_SA") %>%
  filter(!str_detect(poly_num, "Ref")) %>%
  ggplot(aes(x = year, y = spectral_angle, colour = poly_num, group = poly_num)) +
  geom_point(size = 0.5) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.1) +
  theme(legend.position = "none") +
  facet_wrap(~Ref_id) +
  labs(y = "Monthly spectral angle", x = "Year")

## write plot
ggsave(path = "figures", filename = "spectral_angle_all_plots_IEQM.png", width = 7.5, height = 3)

## facet to make better visualization 
spec_df %>%
  filter(month != "Avg_SA") %>%
  filter(!str_detect(poly_num, "Ref")) %>%
  ggplot(aes(x = year, y = spectral_angle, colour = poly_num)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.5) +
  theme(legend.position = "none") +
  facet_wrap(~facet_group)

## include monthly data
spec_df %>%
  filter(month != "Avg_SA") %>%
  filter(!str_detect(poly_num, "Ref")) %>%
  ggplot(aes(x = year, y = spectral_angle, colour = poly_num)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.5) +
  theme(legend.position = "none") +
  facet_wrap(~facet_group)


## what might explain different trends between sites?
## type of tree cover (type_cv) - what type are ref sites? F

## filter IEQM sites to only F sites 
spec_df %>%
  mutate(is_same_cover = ifelse(type_cv == "F", "yes", "no")) %>%
  filter(month != "Avg_SA") %>%
  filter(!str_detect(poly_num, "Ref")) %>%
  mutate(group = paste(poly_num, Ref_id)) %>%
  ggplot(aes(x = year, y = spectral_angle, group = group, colour = is_same_cover)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.5) +
  facet_grid(~yrs_since_clearing)

spec_df %>%
  mutate(is_same_cover = ifelse(type_cv == "F", "Same forest type as reference",
                                "Different forest type as reference")) %>%
  filter(month != "Avg_SA") %>%
  filter(!str_detect(poly_num, "Ref")) %>%
  mutate(group = paste(poly_num, Ref_id)) %>%
  ggplot(aes(x = year, y = spectral_angle, colour = is_same_cover, group = group)) +
  geom_point(size = 0.5) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.1) +
  theme(legend.position = "none") +
  facet_wrap(~Ref_id) +
  labs(y = "Monthly spectral angle", x = "Year") +
  facet_wrap(~is_same_cover)

ggsave(path = "figures", filename = "spectral_angle_by_forest_type_IEQM.png", width = 9.5, height = 2.5)

## facet by yrs since clearing - are really old ones always spectral similar? are really new ones decreasing more dramatically?
spec_df %>%
  mutate(is_same_cover = ifelse(type_cv == "F", "yes",
                                "no")) %>%
  filter(is_same_cover == "yes") %>%
  filter(month != "Avg_SA") %>%
  filter(!str_detect(poly_num, "Ref")) %>%
  ggplot(aes(x = year, y = spectral_angle, colour = poly_num)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.5) +
  theme(legend.position = "none") +
  facet_grid(Ref_id~yrs_since_clearing)

## group by yrs since clearing bins
spec_df %>%
  mutate(bin = ifelse(yrs_since_clearing > 25, "26 - 30 years", 
                      ifelse(yrs_since_clearing <= 25 & yrs_since_clearing > 20, "21 - 25 years", 
                             ifelse(yrs_since_clearing <= 20 & yrs_since_clearing > 15, "16 - 20 years",
                                    ifelse(yrs_since_clearing <= 15 & yrs_since_clearing > 10, 
                                           "11 - 15 years", "< 10 years")))))%>%
  mutate(is_same_cover = ifelse(type_cv == "F", "yes",
                                "no")) %>%
  filter(is_same_cover == "yes") %>%
  filter(month != "Avg_SA") %>%
  filter(!str_detect(poly_num, "Ref")) %>%
  mutate(group = paste(poly_num, Ref_id)) %>%
  ggplot(aes(x = year, y = spectral_angle, colour = poly_num, group = group)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.5) +
  theme(legend.position = "none") +
  facet_grid(~bin) + 
  labs(y = "Monthly spectral angle", x = "Year") 

ggsave(path = "figures", filename = "spectral_angle_by_yrs_since_clearing_IEQM.png",
       width = 9.5, height = 3)

spec_df %>%
  filter(month != "Avg_SA") %>%
  filter(!str_detect(poly_num, "Ref")) %>%
  ggplot(aes(x = year, y = spectral_angle, colour = poly_num)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.5) +
  theme(legend.position = "none") +
  facet_grid(Ref_id~yrs_since_clearing)
## the reference site selected doesn't seem to affect the trend that much


## NEXT
## fit linear models: 
## bunch of lms separately; sa ~ year 
lms <- spec_df %>%
  filter(month != "Avg_SA") %>%
  group_by(poly_num, Ref_id) %>%
  do(tidy(lm(spectral_angle ~ year, data = .), conf.int = TRUE)) %>% 
  filter(term == "year") %>% 
  ungroup()

## how many significant increases / decreases 
length(which(lms$estimate > 0))
length(which(lms$estimate < 0))

lms <- left_join(spec_df, lms)

lms %>%
  ggplot(aes(x = yrs_since_clearing, y = estimate, fill = type_cv)) + 
  geom_violin() +
  stat_summary(fun = "mean",
               geom = "point", 
               colour = "black") +
  facet_wrap(~type_cv)

lms %>%
  ggplot(aes(x = estimate)) + 
  geom_vline(xintercept = 0) +
  geom_histogram() +
  facet_wrap(~Ref_id) +
  labs(x = "Change in spectral angle over time",  y = "Number of sites") 

ggsave(path = "figures", filename = "spectral_angle_histograms_IEQM.png", width = 7.5, height = 3)

lms %>%
  filter(!str_detect(poly_num, "Ref")) %>% 
  ggplot(aes(x = yrs_since_clearing, y = estimate, colour = type_cv)) + 
  geom_point() +
  facet_wrap(~type_cv)



lms %>%
  mutate(is_same_cover = ifelse(type_cv == "F", "Same forest type as reference",
                                "Different forest type as reference"),
         is_increasing = (estimate > 0)) %>%
  filter(month != "Avg_SA", !is.na(is_increasing)) %>%
  filter(!str_detect(poly_num, "Ref")) %>%
  mutate(group = paste(poly_num, Ref_id)) %>%
  ggplot(aes(x = year, y = spectral_angle, colour = is_same_cover, group = group)) +
  geom_point(size = 0.5) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.1) +
  theme(legend.position = "none") +
  facet_wrap(~Ref_id) +
  labs(y = "Monthly spectral angle", x = "Year") +
  facet_grid(is_increasing~is_same_cover)
