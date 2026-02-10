## Replicate map of FL counties with SC landings

#Load packages to work with. Install missing packages as needed.
if (!require("pacman")) {install.packages("pacman")} #- MAKE SURE PACMAN IS INSTALLED AND RUNNING!
pacman::p_load(readxl, plyr, tidyverse, #xtable, 
               sf, tigris, ggpattern, nhdplusTools,
               install = TRUE) 
#
# Use options so tigris returns sf objects
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)

# Get Florida counties
fl_counties <- counties(state = "FL", cb = TRUE) %>%
  mutate(NAME = toupper(NAME))

# Water bodies in Florida (includes Lake Okeechobee)
okeechobee_counties <- fl_counties %>%
  filter(NAME %in% c("Glades", "Hendry", "Palm Beach", "Martin", "Okeechobee"))
fl_water <- do.call(rbind, lapply(okeechobee_counties$COUNTYFP, function(cty) {
  area_water(state = "FL", county = cty)
}))
fl_water <- fl_water %>%
  filter(AWATER > 5e9)


# Read in crab data
crab_data <- read_excel("Stone crab/Stone_Crab_2024_25_County_FWRI.xlsx", skip = 1, .name_repair = "universal")

# Join data to counties
fl_counties_joined <- fl_counties %>%
  left_join(crab_data, by = c("NAME" = "County")) %>%
  mutate(Landings = case_when(
    Sum.of.value > 50000 ~ "Over 50,000",
    Sum.of.value >= 10001 & Sum.of.value <= 50000 ~ "10,001 – 50,000",
    Sum.of.value >= 5001  & Sum.of.value <= 10000 ~ "5,001 – 10,000",
    Sum.of.value >= 1001  & Sum.of.value <= 5000  ~ "1,001 – 5,000",
    Sum.of.value >= 1     & Sum.of.value <= 1000  ~ "1 – 1,000",
    TRUE ~ "0"
  ))

# Basic map
ggplot() +
  
  # Patterned county layer
  geom_sf_pattern(
    data = fl_counties_joined,
    aes(
      fill = Landings,
      pattern = Landings
    ),
    color = "black",
    size = 0.3,
    pattern_fill = "black",
    pattern_color = "black",
    pattern_density = 0.05
  ) +
  
  # Water overlay so Lake Okeechobee still shows
  geom_sf(
    data = fl_water,
    fill = "lightblue",
    color = "blue",
    size = 0.2
  ) +
  # Define exact colors
  scale_fill_manual(
    values = c(
      "Over 50,000"     = "red",
      "10,001 – 50,000" = "darkgray",
      "5,001 – 10,000"  = "darkred",
      "1,001 – 5,000"   = "darkgreen",
      "1 – 1,000"       = "darkblue",
      "0"               = "white"
    ),
    breaks = c(
      "Over 50,000",
      "10,001 – 50,000",
      "5,001 – 10,000",
      "1,001 – 5,000",
      "1 – 1,000"
    ),      # <-- excludes "0" from legend
    name = "Stone Crab Value"
  ) +
  
  # Define exact patterns
  scale_pattern_manual(
    values = c(
      "Over 50,000"     = "none",
      "10,001 – 50,000" = "none",
      "5,001 – 10,000"  = "crosshatch",
      "1,001 – 5,000"   = "stripe",
      "1 – 1,000"       = "stripe",
      "0"               = "none"
    ),
    breaks = c(
      "Over 50,000",
      "10,001 – 50,000",
      "5,001 – 10,000",
      "1,001 – 5,000",
      "1 – 1,000"
    ),
    name = "Stone Crab Value"
  ) +
  
  labs(
    title = "Florida Stone Crab Values by County (2024–25)",
    subtitle = "Patterned classification with Lake Okeechobee shown"
  ) +
  
  theme_minimal()
