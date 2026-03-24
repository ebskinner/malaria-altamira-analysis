library(spdep)
library(rgdal)

# Read in shapefile of clusters
clusters <- readOGR("ebskinn/hih/clusters.shp")

# Create neighbor list and spatial weights 
nb <- poly2nb(clusters)
w <- nb2listw(nb, style="W")

# Read in the processed data
processed_data <- read_csv("processed_data.csv")

# Loop through years and calculate Moran's I
moran_results <- data.frame(year=2005:2021, 
                            morans_i=numeric(length=17),
                            p_value=numeric(length=17))

for(i in 1:nrow(moran_results)){
  
  year <- moran_results$year[i]
  
  # Extract cases for this year
  year_data <- processed_data %>% filter(year == year)
  
  # Join to spatial data
  clusters@data <- left_join(clusters@data, year_data, by = "cluster_id")
  
  # Extract case column
  cases <- clusters@data$cases
  
  moran <- moran.test(cases, listw=w, alternative="greater")
  
  moran_results$morans_i[i] <- moran$estimate
  moran_results$p_value[i] <- moran$p.value
}

write_csv(moran_results, "moran_results.csv")
