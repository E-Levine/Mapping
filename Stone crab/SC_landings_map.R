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

# Lake Okeechobee shpfile
lakeO <- lake <- st_read("Stone crab/lakeokachobee01_geo_simple.shp")

# Read in crab data
crab_data <- read_excel("Stone crab/Stone_Crab_2024_25_County_FWRI.xlsx", skip = 1, .name_repair = "universal")

# Join data to counties
fl_counties_joined <- fl_counties %>%
  left_join(crab_data, by = c("NAME" = "County")) %>%
  mutate(Landings = case_when(
    Sum.of.value > 50000 ~ "50,000+",
    Sum.of.value >= 10001 & Sum.of.value <= 50000 ~ "10,001 – 50,000",
    Sum.of.value >= 5001  & Sum.of.value <= 10000 ~ "5,001 – 10,000",
    Sum.of.value >= 1001  & Sum.of.value <= 5000  ~ "1,001 – 5,000",
    Sum.of.value >= 1     & Sum.of.value <= 1000  ~ "1 – 1,000",
    TRUE ~ "0"
  ))


# Make a map:
ggplot() +
  # Set data for color/fill/patterns
  geom_sf_pattern(
    data = fl_counties_joined,
    aes(fill = Landings,
        pattern = Landings,
        pattern_color = Landings),
    size = 0.5,
    pattern_density = 0.002, #frequency of lines
    pattern_spacing = 0.015, #distance between lines
    pattern_size = 0.2       #thickness of lines
  ) +
  # Set color fills to use by class:
  scale_fill_manual(
    values = c(
      "50,000+"     = "#E31A1C",   # bright red
      "10,001 – 50,000" = "#8E7C8E",   # gray-purple from image
      "5,001 – 10,000"  = "white",   # light pink background
      "1,001 – 5,000"   = "white",
      "1 – 1,000"       = "white",
      "0"               = "white"
    ),
    breaks = c(
      "50,000+",
      "10,001 – 50,000",
      "5,001 – 10,000",
      "1,001 – 5,000",
      "1 – 1,000"
    ),
    name = "Landings (lbs)"
  ) +
  # Set patterns by class:
  scale_pattern_manual(
    values = c(
      "50,000+"     = "none",
      "10,001 – 50,000" = "none",
      "5,001 – 10,000"  = "crosshatch",
      "1,001 – 5,000"   = "stripe",
      "1 – 1,000"       = "stripe",
      "0"               = "none"
    ),
    breaks = c(
      "50,000+",
      "10,001 – 50,000",
      "5,001 – 10,000",
      "1,001 – 5,000",
      "1 – 1,000"
    ),
    name = "Landings (lbs)"
  ) +
  # Set patttern colors by class:
  scale_pattern_color_manual(
    values = c(
      "50,000+"     = NA,
      "10,001 – 50,000" = NA,
      "5,001 – 10,000"  = "darkred",   # red crosshatch
      "1,001 – 5,000"   = "darkgreen",  # blue diagonal
      "1 – 1,000"       = "darkblue",  # blue horizontal
      "0"               = NA
    )
  )+
  # Control stripe orientation
  guides(
    fill = guide_legend(title = "Stone crabs \n2025 Commercial \nLandings (lbs)"),
    pattern_color = "none",
    pattern = guide_legend(
      title = "Stone crabs \n2025 Commercial \nLandings (lbs)",
      override.aes = list(
        pattern_angle = c(0, 0, 45, 45, 0),
        pattern_size = 0.8,
        fill = c("#E31A1C", "#8E7C8E", "white", "white", "white"),  # match fill for clarity
        color = "black",  # outline color
        pattern_color = c(NA, NA, "darkred", "darkgreen", "darkblue")
      )
    )
  ) +
  
  # Add water bodies/base lines
  geom_sf(data = florida, fill = NA, color = "black", size = 0.2)+
  geom_sf(data = lakeO, fill = "white", color = "black", size = 0.2)+
  
  # Mapping themes and formatting 
  theme_bw() +
  
  theme(
    # Legend formatting
    legend.position = "inside", legend.position.inside = c(0.16, 0.20),
    legend.frame = element_blank(), 
    legend.text = element_text(hjust = 1, size = 12), legend.title = element_text(size = 14),
    # Plot formatting 
    axis.text = element_blank(), axis.ticks = element_blank(), 
    panel.grid =  element_blank())
