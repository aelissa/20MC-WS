library(dplyr)
library(sf)

poi <- st_read("20MN-WS/data/poi_scotland.gpkg")

cat_20MN <- read_csv("cat20MN.csv")

cat_20MN %>%
  count(framework_category, framework_theme, name = "n_categories") %>%
  arrange(framework_category, framework_theme)

POI_20MN <- poi %>%
  inner_join(cat_20MN, by = c("category_name" = "category")) %>%

st_write(POI_20MN, "20MN-WS/data/destinations.gpkg")


