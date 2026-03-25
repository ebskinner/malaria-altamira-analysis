##CODE WRITTEN BY DR DANIELA DE ANGELI DUTRA [d.deangelidutra@bangor.ac.uk]

#### setwd and loading packages #####
setwd("~/Library/CloudStorage/CloudMounter-DanielaDeAngeliDutra(Staff)/Previous affiliations/Stanford/Stanford - Altamira project/R code - Elle")
library(mgcv)
library(tidyverse)
library(mapview)
library(ggplot2)
library(sf)

# Plotting Clusters
shape <- st_read("data/exploded_polys.shp")
mapview(shape, legend = NULL)

#### Data Carpentry ####

# reading cases data
mal_cases <- read.csv("data/malaria_location_data.csv")
mal_cases <- mal_cases[,c(2,4:13)]
mal_cases <- na.omit(mal_cases)
mal_cases <- mal_cases %>%
  arrange(codigo, year)
mal_cases$codigo <- as.integer(mal_cases$codigo)

poly <- read.csv("data/data_points2.csv")
mal_cases <- left_join(mal_cases, poly[,c(2,5)], by = "codigo")

mal_cases <- mal_cases %>%
  group_by(polygon_id, year) %>%
  summarise(pos = sum(pos),
            pop_raw = sum(pop_raw),
            P.vivax = sum(P.vivax),
            P.falcumparum = sum(P.falciparum),
            P.mixed = sum(P.mixed),
            P.malarie = sum(P.malaria),
            P.ovali = sum(P.ovali))

#mal_cases <- filter(mal_cases, !(polygon_id %in% c(30, 32))) # list of polygons in Altamira city
#print(unique(mal_cases$polygon_id))

# Create an empty list to store data frames
deforest_data <- list()

# Loop through the years from 2005 to 2021
for (year in 2005:2021) {
  # Read the CSV file for the specific year
  file_path <- paste("clusters distance/distance_", year, "_deforestation_data.csv", sep="")
  deforest_data[[year]] <- read.csv(file_path)
  
  # Select columns 2 and 3 (assuming columns 1 is the index)
  deforest_data[[year]] <- deforest_data[[year]][, c(2, 3)]
  
  # Add a column for the year
  deforest_data[[year]]$year <- year
}

# Combine all data frames into a single data frame
def_data <- do.call(rbind, deforest_data)

# joining data
data <- left_join(mal_cases, def_data, by = c("polygon_id", "year"))
data$dist_def <- data$mean 
data$mean <- NULL

# Create an empty list to store data frames
mining_data <- list()

# Loop through the years from 2005 to 2021
for (year in 2005:2021) {
  # Read the CSV file for the specific year
  file_path <- paste("clusters distance/distance_", year, "_mining_data.csv", sep="")
  mining_data[[year]] <- read.csv(file_path)
  
  # Select columns 2 and 3 (assuming columns 1 is the index)
  mining_data[[year]] <- mining_data[[year]][, c(2, 3)]
  
  # Add a column for the year
  mining_data[[year]]$year <- year
}

# Combine all data frames into a single data frame
min_data <- do.call(rbind, mining_data)

# joining data
data <- left_join(data, min_data, by = c("polygon_id", "year"))
data$dist_min <- data$mean 
data$mean <- NULL

data$dist_min <- ifelse(is.na(data$dist_min), 50000, data$dist_min) # 50km as maximum distance

# Create an empty list to store data frames
forest_edge_data <- list()

# Loop through the years from 2005 to 2021
for (year in 2005:2021) {
  # Read the CSV file for the specific year
  file_path <- paste("Forest edge/forest_edge", year, ".csv", sep="")
  forest_edge_data[[year]] <- read.csv(file_path)
  
  # Select columns 2 and 3 (assuming columns 1 is the index)
  forest_edge_data[[year]] <- forest_edge_data[[year]][, c(2, 3, 4)]
  
  # Add a column for the year
  forest_edge_data[[year]]$year <- year
}

# Combine all data frames into a single data frame
edge_data <- do.call(rbind, forest_edge_data)

# joining data
data <- left_join(data, edge_data, by = c("polygon_id", "year"))
data$dist_edge <- data$perimeter 
data$perimeter <- NULL
data$label <- NULL

data$dist_edge <- data$dist_edge/1000 # change to km 

# write csv for full dataset
#write.csv(data, file = "land use distances to clusters and cases.csv")

# remove dataset not in use
rm(deforest_data, def_data, mining_data, min_data, forest_edge_data, edge_data, mal_cases, poly, file_path, year)

# load data

# data <- read.csv(land use distances and cases.csv")

data$dist_def <- data$dist_def/1000 # convert to km
data$dist_min <- data$dist_min/1000 # convert to km

#load data
total_new <- read.csv("data/total_hih_dataset.csv", header = T)

data <- inner_join(data, total_new[,c(2:3,13:14)], by =c("polygon_id", "year"))

rm(total_new)

# add human movement data
access <- read.csv("xingu_accessibility.csv")

data <- inner_join(data,  access[,c(2:3)],  by = "polygon_id")

# plotting data distribution
hist(data$pos)
hist(data$pop_raw)
hist(data$dist_def)
hist(data$dist_min)
hist(data$mean_annual_temp)
hist(data$mean_annual_prec_mm)
hist(data$Mean_time_to_urban_areas_minutes)

data <- filter(data, dist_def > 0.5)
hist(data$dist_def)
hist(data$dist_min)

ggpubr::ggdensity(data$dist_def, color = "darkred", size = 2, 
                  xlab = "Values for Distance to Deforestation", ylab = "Values density")

ggpubr::ggdensity(data$dist_min, color = "black", size = 2, 
                  xlab = "Values for Distance to Mining", ylab = "Values density")

ggpubr::ggdensity(data$Mean_time_to_urban_areas_minutes, color = "darkblue", size = 2, 
                 xlab = "Values for site accessibility", ylab = "Values density")

ggpubr::ggdensity(data$dist_edge, color = "darkorange", size = 2,
                  xlab = "Values for forest edge perimeter", ylab = "Values density")

# scaling data
data_scaled <- data
data_scaled[c(4, 10:15)] <- scale(data_scaled[c(4, 10:15)])

cor(data_scaled$dist_def, data$dist_edge)

#### Running models ####

###### GAM ######

# 2017-2021
data1 <- filter(data_scaled, year > 2016)
hist(data1$pos, breaks = 100)

gam2 <- mgcv::gam(pos ~ dist_def + dist_edge + pop_raw * Mean_time_to_urban_areas_minutes + mean_annual_prec_mm + mean_annual_temp +
                   s(year, polygon_id, bs = "re"),
                   data = data1, 
                   family = negbin(theta = 1), 
                   niterPQL = 1000)
summary(gam2)

gam2B <- mgcv::gam(pos ~ dist_def + dist_edge + pop_raw + Mean_time_to_urban_areas_minutes + mean_annual_prec_mm + mean_annual_temp +
                    s(year, polygon_id, bs = "re"),
                  data = data1, 
                  family = negbin(theta = 1), 
                  niterPQL = 1000)
summary(gam2B)

gam2C <- mgcv::gam(pos ~ dist_def + dist_edge + Mean_time_to_urban_areas_minutes + mean_annual_prec_mm + mean_annual_temp +
                     s(year, polygon_id, bs = "re"),
                   data = data1, 
                   family = negbin(theta = 1), 
                   niterPQL = 1000)
summary(gam2C)

gam2D <- mgcv::gam(pos ~ dist_def + dist_edge + pop_raw * Mean_time_to_urban_areas_minutes + mean_annual_temp +
                     s(year, polygon_id, bs = "re"),
                   data = data1, 
                   family = negbin(theta = 1), 
                   niterPQL = 1000)
summary(gam2D)

gam2E <- mgcv::gam(pos ~ dist_def + dist_edge + pop_raw * Mean_time_to_urban_areas_minutes + mean_annual_prec_mm +
                     s(year, polygon_id, bs = "re"),
                   data = data1, 
                   family = negbin(theta = 1), 
                   niterPQL = 1000)
summary(gam2E)

gam2F <- mgcv::gam(pos ~ dist_def + dist_edge + pop_raw * Mean_time_to_urban_areas_minutes + 
                     s(year, polygon_id, bs = "re"),
                   data = data1, 
                   family = negbin(theta = 1), 
                   niterPQL = 1000)
summary(gam2F)

# 2005-2011
data2 <- filter(data_scaled, year < 2012)
hist(data2$pos, breaks = 100)

gam3 <- mgcv::gam(pos ~ dist_def+ dist_edge +  pop_raw * Mean_time_to_urban_areas_minutes + mean_annual_prec_mm + mean_annual_temp +
                  s(year, polygon_id, bs = "re"),
                  data = data2, 
                  family = negbin(theta = 1), 
                  niterPQL = 1000)

summary(gam3)

gam3B <- mgcv::gam(pos ~ dist_def + dist_edge + pop_raw + Mean_time_to_urban_areas_minutes + mean_annual_prec_mm + mean_annual_temp +
                     s(year, polygon_id, bs = "re"),
                   data = data2, 
                   family = negbin(theta = 1), 
                   niterPQL = 1000)
summary(gam3B)

gam3C <- mgcv::gam(pos ~ dist_def + dist_edge + Mean_time_to_urban_areas_minutes + mean_annual_prec_mm + mean_annual_temp +
                     s(year, polygon_id, bs = "re"),
                   data = data2, 
                   family = negbin(theta = 1), 
                   niterPQL = 1000)
summary(gam3C)

gam3D <- mgcv::gam(pos ~ dist_def + dist_edge + pop_raw * Mean_time_to_urban_areas_minutes + mean_annual_temp +
                     s(year, polygon_id, bs = "re"),
                   data = data2, 
                   family = negbin(theta = 1), 
                   niterPQL = 1000)
summary(gam3D)

gam3E <- mgcv::gam(pos ~ dist_def + dist_edge + pop_raw * Mean_time_to_urban_areas_minutes + mean_annual_prec_mm +
                     s(year, polygon_id, bs = "re"),
                   data = data2, 
                   family = negbin(theta = 1), 
                   niterPQL = 1000)
summary(gam3E)

gam3F <- mgcv::gam(pos ~ dist_def + dist_edge + pop_raw * Mean_time_to_urban_areas_minutes + 
                     s(year, polygon_id, bs = "re"),
                   data = data2, 
                   family = negbin(theta = 1), 
                   niterPQL = 1000)
summary(gam3F)

#### Plots ####
load("HIH_GAMM.RData")
library(mdthemes) # to add italic to ggplot labels mdthemes::theme_classic 

data1$res <- residuals.gam(gam2)
data1$pred <- predict.gam(gam2, type = "response")

p3 <- ggplot(data = data1, aes(x = dist_edge, y = pred)) +
  geom_point(cex = 2, color = "black", shape = 1) +
  geom_smooth(color = "black", linewidth = 2, fill = "grey", method = "lm") + 
  theme_classic() + 
  labs(x = "Forest edge perimeter (km)", y = "Notified malaria cases", title = "C - Malaria cases 2017 to 2020") +
  theme(plot.title = element_text(size=12))
p3 

p4 <- ggplot(data = data1, aes(x = pop_raw, y = pred)) +
  geom_point(cex = 2, color = "black", shape = 1) +
  geom_smooth(color = "black", linewidth = 2, fill = "grey", method = "lm") +
  theme_classic() + 
  labs(x = "Local population", y = "Notified malaria cases", title = "D - Malaria cases 2017 to 2020") +
  theme(plot.title = element_text(size=12))
p4

data2$res <- residuals.gam(gam3)
data2$pred <- predict.gam(gam3, type = "response")

p5 <- ggplot(data = data2, aes(x = dist_edge, y = pred)) +
  geom_point(cex = 2, color = "black", shape = 1) +
  geom_smooth(color = "black", linewidth = 2, fill = "grey", method = "lm") +
  theme_classic() + ylim(0,250) +
  labs(x = "Forest edge perimeter (km)", y = "Notified malaria cases", title = "A - Malaria cases 2006 to 2011") +
  theme(plot.title = element_text(size=12))
p5

p6 <- ggplot(data = data2, aes(x = pop_raw, y = pred)) +
  geom_point(cex = 2, color = "black", shape = 1) +
  geom_smooth(color = "black", linewidth = 2, fill = "grey", method = "lm") +
  theme_classic() + 
  labs(x = "Local population", y = "Notified malaria cases", title = "B - Malaria cases 2006 to 2011") +
  theme(plot.title = element_text(size=12))
p6



data_year <- data %>%
  group_by(year) %>%
  summarise(pos = sum(pos),
            vivax = sum(P.vivax),
            falc = sum(P.falcumparum))
# Example: Reshape your data to a long format
data_long <- data_year %>%
  pivot_longer(cols = c(pos, vivax, falc), 
               names_to = "type", 
               values_to = "cases")

data_long$type <- as.factor(data_long$type)
data_long$type <- relevel(data_long$type, ref = "pos")

# Create the plot with the legend
p8 <- ggplot(data = data_long, aes(x = year, y = cases, color = type)) +
  annotate('rect', xmin=2012, xmax=2016, ymin = -Inf, ymax = Inf, alpha=.5, fill='lightgray') +
  geom_line(linewidth = 1.5) +
  mdthemes::md_theme_classic() +
  scale_color_manual(
    values = c("pos" = "darkred", "vivax" = "orange3", "falc" = "yellow3"),
    labels = c("pos" = "All cases", "vivax" = "*P. vivax*", "falc" = "*P. falciparum*")
  ) +
  labs(
    x = "Years", 
    y = "Notified malaria cases", 
    title = "A - Malaria cases 2006 to 2020", 
    color = "Malaria infection"
  ) +
  theme(legend.position = "top")

p8

data_site <- data %>%
  group_by(year, polygon_id, pop_raw,.add = TRUE) %>%
  summarise(pos = sum(pos),
             vivax = sum(P.vivax),
             falc = sum(P.falcumparum)) %>%
  mutate(parasite_index = (pos/pop_raw) * 100,
         vivax_index = (vivax/pop_raw) * 100,
         falc_index = (falc/pop_raw) * 100)

data_site <- filter(data_site, polygon_id != 38)

data_site$ID <- paste("Cluster", data_site$polygon_id)
data_site$ID <- as.factor(data_site$ID)

data_site$ID <- factor(data_site$ID, levels = paste0("Cluster ", sort(as.numeric(gsub("Cluster ", "", levels(data_site$ID))))))

p9 <- data_site %>%
  ggplot(aes(x = year, y = parasite_index)) + #color = treated, group = treated)) + 
  annotate('rect', xmin=2012, xmax=2016, ymin = -Inf, ymax = Inf, alpha=.5, fill='lightgray') +
  geom_smooth(linewidth = 1, se = FALSE, color = "darkred") +
  theme_linedraw() +
  labs(x = "Year", y = "Parasite Index (malaria cases per 100 people)", title = "B - Parasite index per year and cluster") + #,
  facet_wrap(~ ID, nrow = 4, ncol = 6, scales = "free_y") + # Facet by site
  theme(strip.background = element_blank(), strip.placement = "outside", legend.position = "none")

library(cowplot)

plot_grid(p8, p9, rel_widths = c(1.2, 2))
#ggsave(filename = "Figure 2 V3.png", width = 13, height = 5, units = "in", dpi = 600)

plot_grid(p5, p6, p3, p4, nrow = 2)
ggsave(filename = "Figure 3 revised.png", width = 6, height = 5)

save.image("HIH_GAMM.RData")


