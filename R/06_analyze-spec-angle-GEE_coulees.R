## analyze the spectral angle data from the coulee sites
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
    filename = paste0("data-processed/GEE/spec-ang-files_coulees/", year, "_SpectralAngle_", m, "_coulees.shp")
    
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
length(unique(spec_df$PolygonNam)) ## 29
length(unique(spec_df$Ref_id)) ## 8

## calculate an average spectral angle per site per year 
spec_df <- spec_df %>%
  group_by(PolygonNam, year, Ref_id) %>%
  mutate(Avg_SA = mean(spectral_angle)) %>%
  spread(key = "month", value = "spectral_angle") %>%
  gather(key = "month", value = "spectral_angle", c("Aug_SA", "Jul_SA", "Jun_SA", "Sept_SA", "Avg_SA"))

## join with site information 
target <- st_read("data-processed/target_sites_coulees/target_sites_coulees.shp")
ref <- st_read("data-processed/reference_sites_coulees/reference_sites_coulees.shp")
kobo <- read.csv("data-raw/coulees_agricoles/SITES_terrain2024_Kobo_export.csv")

spec_df <- left_join(st_drop_geometry(target), spec_df, c("poly_num" = "PolygonNam"), 
                     relationship = "many-to-many") %>%
  left_join(., kobo)

## some polygons don't have spectral angle values
length(which(is.na(spec_df$spectral_angle))) ## 3712

## get rid of missing data
spec_df <- filter(spec_df, !is.na(spectral_angle))

#write.csv(spec_df, "data-processed/S2-spec-angle_2017-2024_coulees.csv", row.names = F)
spec_df <- read.csv("data-processed/S2-spec-angle_2017-2024_coulees.csv")


## plot against time 
####################################
## change values for reference poly column 
spec_df$Ref_id = ifelse(spec_df$Ref_id == "-281730,15+244597,32", "1", 
                        ifelse(spec_df$Ref_id == "-295424,99+218942,28", "2",
                               ifelse(spec_df$Ref_id == "-295494,34+218762,80", 
                                      "3", spec_df$Ref_id)))
spec_df$Ref_id = paste("Reference site", spec_df$Ref_id)

## make column that says coulee, foret, or chrono 
spec_df$SiteType = ifelse(str_detect(spec_df$SiteCode, "Foret"), "Foret",
                          ifelse(str_detect(spec_df$SiteCode, "Coulee"), "Coulee", 
                                 ifelse(str_detect(spec_df$SiteCode, "Chrono"), "Chrono", 
                                        "Reference")))

## clean forest type column
spec_df$Type.foret = ifelse(is.na(spec_df$Type.foret), "Unknown", spec_df$Type.foret)
spec_df$Type.foret = ifelse(spec_df$SiteType == "Reference", "Feuillus", spec_df$Type.foret)

## clean age column
spec_df$Age = ifelse(spec_df$Age == "?" | is.na(spec_df$Age), "Unknown",
                     ifelse(spec_df$Age %in% c("ref", "Ref"), "Reference site", spec_df$Age))
  
### COMPARE TO IEQM REF SITES 
## plot SA measures data from different months and years within sites
## average per year
## colour by site type 
spec_df %>%
  filter(Ref_id %in% c("Reference site 1", "Reference site 2", "Reference site 3")) %>% 
  filter(!SiteType %in% c("Reference")) %>%
  filter(Age != "Reference site") %>%
  filter(month != "Avg_SA") %>%
  mutate(Age = as.numeric(ifelse(SiteType == "Coulee", 0, ifelse(Age == "Unknown", NA, Age)))) %>%
  ggplot(aes(x = year, y = spectral_angle, colour = Age, group = poly_num)) +
  geom_point(size = 0.75) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.5) +
  facet_wrap(~Ref_id) +
  labs(y = "Monthly spectral angle", x = "Year")

## write plot
ggsave(path = "figures", filename = "spectral_angle_all_plots_coulees_age.png", width = 7.5, height = 3)

gg <- spec_df %>%
  filter(!SiteType %in% c("Reference")) %>%
  filter(Ref_id %in% c("Reference site 1", "Reference site 2", "Reference site 3")) %>%
  filter(month != "Avg_SA") %>%
  filter(Age != "Reference site") %>%
  mutate(Type.foret = ifelse(Type.foret == "?", "Unknown", Type.foret)) %>%
  mutate(Type.foret = ifelse(Type.foret == "Feuillus", "Leafy", 
                             ifelse(Type.foret == "Conifere", "Coniferous", Type.foret))) %>%
  mutate(Age = as.numeric(ifelse(SiteType == "Coulee", 0, ifelse(Age == "Unknown", NA, Age)))) %>%
  mutate(Age = as.numeric(ifelse(SiteType == "Coulee", 0, ifelse(Age == "Unknown", NA, Age)))) %>%
  ggplot(aes(x = year, y = spectral_angle, colour = Age, group = poly_num)) +
  geom_point(size = 0.75) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.5) +
  facet_grid(Type.foret~Ref_id) +
  labs(y = "Monthly spectral angle", x = "Year", colour = "Years\nsince\nreplanting")

legend <- cowplot::get_plot_component(gg, 'guide-box-right', return_all = TRUE)

gg <- gg + theme(legend.position = "none")

## write plot
ggsave(gg, path = "figures", filename = "spectral_angle_all_plots_coulees_age_type.png", width = 7.5,
       height = 5)
ggsave(legend, path = "figures", filename = "spectral_angle_all_plots_coulees_age_type_legend.png", 
       width = 4, height = 2)


### COMPARE TO LOCAL REF SITES 
gg <- spec_df %>%
  filter(!SiteType %in% c("Reference")) %>%
  filter(!Ref_id %in% c("Reference site 1", "Reference site 2", "Reference site 3")) %>%
  filter(month != "Avg_SA") %>%
  filter(Age != "Reference site") %>% 
  mutate(region = str_split_fixed(.$SiteCode, '\\_', 3)[,1]) %>%
  filter(str_detect(.$Ref_id, .$region)) %>%
  mutate(Type.foret = ifelse(Type.foret == "?", "Unknown", Type.foret)) %>%
  mutate(Type.foret = ifelse(Type.foret == "Feuillus", "Leafy", 
                             ifelse(Type.foret == "Conifere", "Coniferous", Type.foret))) %>%
  mutate(Age = as.numeric(ifelse(SiteType == "Coulee", 0, ifelse(Age == "Unknown", NA, Age)))) %>%
  mutate(Age = as.numeric(ifelse(SiteType == "Coulee", 0, ifelse(Age == "Unknown", NA, Age)))) %>%
  ggplot(aes(x = year, y = spectral_angle, colour = Age, group = poly_num)) +
  geom_point(size = 0.75) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.5) +
  facet_grid(Type.foret~Region) +
  labs(y = "Monthly spectral angle", x = "Year", colour = "Years\nsince\nreplanting") +
  theme(legend.position = "none")

ggsave(gg, path = "figures", filename = "spectral_angle_all_plots_coulees_age_type_localref.png", 
       width = 7.5,
       height = 5)


cdq <- spec_df %>%
  #filter(!SiteType %in% c("Reference")) %>%
  filter(!Ref_id %in% c("Reference site 1", "Reference site 2", "Reference site 3")) %>%
  filter(month != "Avg_SA") %>%
  #filter(Age != "Reference site") %>% 
  mutate(region = str_split_fixed(.$SiteCode, '\\_', 3)[,1]) %>%
  filter(str_detect(.$Ref_id, .$region)) %>%
  mutate(Type.foret = ifelse(Type.foret == "?", "Unknown", Type.foret)) %>%
  mutate(Type.foret = ifelse(Type.foret == "Feuillus", "Leafy", 
                             ifelse(Type.foret == "Conifere", "Coniferous", Type.foret))) %>%
  filter(SiteCode != "CDQ_MB_Chrono1") %>% 
  filter(Region == "Centre Quebec") %>%
  filter(SiteType %in% c("Chrono", "Foret")) %>%
  mutate(group = paste0(poly_num, Ref_id)) %>%
  ggplot(aes(x = year, y = spectral_angle, colour = SiteCode, group = group)) +
  geom_point(size = 0.75) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.5) +
  labs(y = "Monthly spectral angle", x = "Year", colour = "") +
  scale_colour_discrete(labels = c("Chronosequence 1", "Chronosequence 2", "Reference"))

ggsave(cdq, path = "figures", 
       filename = "spectral_angle_cdq.png", 
       width = 5,
       height = 3)

## facet by site type + ref id
spec_df %>%
  filter(month != "Avg_SA") %>%
  ggplot(aes(x = year, y = spectral_angle, colour = SiteType, group = poly_num)) +
  geom_point(size = 0.75) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.5) +
  labs(y = "Monthly spectral angle", x = "Year") +
  facet_grid(SiteType~Ref_id) 


## fit linear models: 
## bunch of lms separately; sa ~ year 
lms <- spec_df %>%
  filter(month != "Avg_SA") %>%
  group_by(poly_num, Ref_id) %>%
  do(tidy(lm(spectral_angle ~ year, data = .), conf.int = TRUE)) %>% 
  filter(term == "year") %>% 
  ungroup()

lms <- left_join(spec_df, lms)

lms %>%
  ggplot(aes(x = estimate, fill = SiteType)) + 
  geom_vline(xintercept = 0) +
  geom_histogram() +
  facet_grid(SiteType~Ref_id) +
  labs(x = "Change in spectral angle over time",  y = "Number of sites") 

ggsave(path = "figures", filename = "spectral_angle_histograms_coulees.png", width = 7.5, height = 5.5)

## now plot by forest type
spec_df %>%
  filter(month != "Avg_SA") %>%
  ggplot(aes(x = year, y = spectral_angle, colour = Type.foret, group = poly_num)) +
  geom_point(size = 0.75) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.5) +
  labs(y = "Monthly spectral angle", x = "Year") +
  facet_grid(SiteType~Ref_id)
## mixed + feuillus show expected trends, but not coniferous :-)
## note: there is only mixed reference 

spec_df %>%
  filter(SiteType %in% c("Chrono")) %>%
  filter(month != "Avg_SA") %>%
  ggplot(aes(x = year, y = spectral_angle, colour = Type.foret, group = poly_num)) +
  geom_point(size = 0.75) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.5) +
  labs(y = "Monthly spectral angle", x = "Year") +
  facet_grid(Type.foret~Ref_id)

spec_df %>%
  filter(Type.foret %in% c("Mixed", "Feuillus")) %>%
  filter(month != "Avg_SA") %>%
  ggplot(aes(x = year, y = spectral_angle, colour = Type.foret, group = poly_num)) +
  geom_point(size = 0.75) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.5) +
  labs(y = "Monthly spectral angle", x = "Year") +
  facet_grid(SiteType~Ref_id)

spec_df %>%
  filter(!Type.foret %in% c("Mixed", "Feuillus")) %>%
  filter(month != "Avg_SA") %>%
  ggplot(aes(x = year, y = spectral_angle, colour = Type.foret, group = poly_num)) +
  geom_point(size = 0.75) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.5) +
  labs(y = "Monthly spectral angle", x = "Year") +
  facet_grid(SiteType~Ref_id)

## colour by age in chronosequence
spec_df %>%
  filter(SiteType == "Chrono") %>%
  filter(Type.foret %in% c("Mixed", "Feuillus")) %>%
  filter(month != "Avg_SA") %>%
  ggplot(aes(x = year, y = spectral_angle, colour = Age, group = poly_num)) +
  geom_point(size = 0.75) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.5) +
  labs(y = "Monthly spectral angle", x = "Year") +
  facet_grid(SiteType~Ref_id)

ggsave(path = "figures", filename = "spectral_angle_chronosequence_by_age_coulees.png", width = 7.5, height = 2.5)

