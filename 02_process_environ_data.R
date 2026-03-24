library(tidyverse)

# Read in the environmental data exported from GEE
environ_data <- read_csv("cluster_environ_data.csv")

# Read in the processed case data 
case_data <- read_csv("processed_case_data.csv")  

# Pivot the environmental data longer
environ_long <- environ_data %>% 
  pivot_longer(cols = c(tempVals, precipVals, deforestDist, edgeLength),
               names_to = "variable", 
               values_to = "value") %>%
  mutate(year = rep(2005:2021, times = 4))

# Spread the variables wide  
environ_wide <- environ_long %>%
  pivot_wider(names_from = variable, values_from = value)

# Join with the case data
joined_data <- left_join(case_data, environ_wide, by = c("year", "cluster_id"))

# Write out the processed data
write_csv(joined_data, "processed_data.csv")
