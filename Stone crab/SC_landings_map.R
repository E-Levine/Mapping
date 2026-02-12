## Replicate map of FL counties with SC landings

#Load packages to work with. Install missing packages as needed.
if (!require("pacman")) {install.packages("pacman")} #- MAKE SURE PACMAN IS INSTALLED AND RUNNING!
pacman::p_load(readxl, plyr, tidyverse, #xtable, 
               sf, tigris, ggpattern, nhdplusTools,
               geodata,
               install = TRUE) 
#
# Setup ----
#
# Use options so tigris returns sf objects
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)

# Florida outline with water
US <- gadm(country = "USA", level = 1, path = tempdir())
florida <- US[US$NAME_1 == "Florida", ]
florida <- st_as_sf(florida)

# Get Florida counties
fl_counties <- counties(state = "FL", cb = TRUE) %>%
  mutate(NAME = toupper(NAME))

# Lake Okeechobee shpfile
lakeO <- lake <- st_read("Stone crab/lakeokachobee01_geo_simple.shp")

# Read in crab data
crab_data <- read_excel("Stone crab/Stone_Crab_2024_25_County_FWRI.xlsx", skip = 1, .name_repair = "universal")


# original map ----
legend_levels <- c(
  "50,000+",
  "10,001 – 50,000",
  "5,001 – 10,000",
  "1,001 – 5,000",
  "1 – 1,000"
)

# Join data to counties
fl_counties_joined <- fl_counties %>%
  left_join(crab_data, by = c("NAME" = "County")) %>%
  mutate(Landings = factor(case_when(
    Sum.of.pounds > 50000 ~ "50,000+",
    Sum.of.pounds >= 10001 & Sum.of.pounds <= 50000 ~ "10,001 – 50,000",
    Sum.of.pounds >= 5001  & Sum.of.pounds <= 10000 ~ "5,001 – 10,000",
    Sum.of.pounds >= 1001  & Sum.of.pounds <= 5000  ~ "1,001 – 5,000",
    Sum.of.pounds >= 1     & Sum.of.pounds <= 1000  ~ "1 – 1,000",
    TRUE ~ "0"
  ),
  levels = c(legend_levels, "0"))) 

# Check for missing data, create dummies as needed for proper legend display:
missing_levels <- setdiff(levels(fl_counties_joined$Landings), unique(fl_counties_joined$Landings))

if(length(missing_levels) > 0){
  dummy_rows <- data.frame(
    NAME = paste0("dummy_", missing_levels),
    Landings = factor(missing_levels, levels = levels(fl_counties_joined$Landings))
  )
  
  # Give dummy geometry (sf requires geometry column)
  dummy_rows <- st_sf(dummy_rows, 
                      geometry = st_sfc(lapply(missing_levels, function(x) st_point(c(NA_real_, NA_real_))),
                                        crs = 4269))
  
  # Bind to main data
  fl_counties_joined <- bind_rows(fl_counties_joined, dummy_rows)
}

# Make a map:
ggplot() +
  # Set data for color/fill/patterns
  geom_sf_pattern(
    data = fl_counties_joined,
    aes(fill = Landings,
        pattern = Landings,
        pattern_color = Landings,
        pattern_angle = Landings),
    size = 0.5,
    pattern_density = 0.002, #frequency of lines
    pattern_spacing = 0.015, #distance between lines
    pattern_size = 0.35       #thickness of lines
  ) +
  # Set color fills to use by class:
  scale_fill_manual(
    values = c(
      "50,000+"     = "#E31A1C",   
      "10,001 – 50,000" = "#8E7C8E",   
      "5,001 – 10,000"  = "white",   
      "1,001 – 5,000"   = "white",
      "1 – 1,000"       = "white",
      "0"               = "white"
    ),
    breaks = legend_levels,
    labels = legend_levels,
    drop = FALSE
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
    breaks = legend_levels,
    drop = FALSE
  ) +
  # Set pattern (line) colors by class:
  scale_pattern_color_manual(
    values = c(
      "50,000+"     = NA,
      "10,001 – 50,000" = NA,
      "5,001 – 10,000"  = "darkred",   # red crosshatch
      "1,001 – 5,000"   = "darkgreen",  # green diagonal
      "1 – 1,000"       = "darkblue",  # blue horizontal
      "0"               = NA
    ),
    breaks = legend_levels,
    drop = FALSE
  )+
  # Set pattern angles by class:
  scale_pattern_angle_manual(
    values = c(
      "50,000+"         = 0,
      "10,001 – 50,000" = 0,
      "5,001 – 10,000"  = 45,
      "1,001 – 5,000"   = 45,
      "1 – 1,000"       = 0,   
      "0"               = 0
    ),
    breaks = legend_levels,
    drop = FALSE
  ) +
  # Control stripe orientation
  guides(
    fill = "none", #guide_legend(title = "Stone crabs \n2024 Commercial \nLandings (lbs)"),
    pattern_color = "none",
    pattern_angle = "none",
    pattern = guide_legend(
      title = "Stone crabs \n2024 Commercial \nLandings (lbs)",
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

#

# 500000+ map ----
legend_levels <- c(
  "500,000+",
  "100,001 – 500,000",
  "50,001 – 100,000",
  "5,001 – 50,000",
  "1 – 5,000"
)

# Join data to counties
fl_counties_joined <- fl_counties %>%
  left_join(crab_data, by = c("NAME" = "County")) %>%
  mutate(Landings = factor(case_when(
    Sum.of.pounds > 500000 ~ "500,000+",
    Sum.of.pounds >= 100001 & Sum.of.pounds <= 500000 ~ "100,001 – 500,000",
    Sum.of.pounds >= 50001  & Sum.of.pounds <= 100000 ~ "50,001 – 100,000",
    Sum.of.pounds >= 5001  & Sum.of.pounds <= 50000  ~ "5,001 – 50,000",
    Sum.of.pounds >= 1     & Sum.of.pounds <= 5000  ~ "1 – 5,000",
    TRUE ~ "0"
  ),
  levels = c(legend_levels, "0"))) 

# Check for missing data, create dummies as needed for proper legend display:
missing_levels <- setdiff(levels(fl_counties_joined$Landings), unique(fl_counties_joined$Landings))

if(length(missing_levels) > 0){
  dummy_rows <- data.frame(
    NAME = paste0("dummy_", missing_levels),
    Landings = factor(missing_levels, levels = levels(fl_counties_joined$Landings))
  )
  
  # Give dummy geometry (sf requires geometry column)
  dummy_rows <- st_sf(dummy_rows, 
                      geometry = st_sfc(lapply(missing_levels, function(x) st_point(c(NA_real_, NA_real_))),
                                        crs = 4269))
  
  # Bind to main data
  fl_counties_joined <- bind_rows(fl_counties_joined, dummy_rows)
}

# Make a map:
ggplot() +
  # Set data for color/fill/patterns
  geom_sf_pattern(
    data = fl_counties_joined,
    aes(fill = Landings,
        pattern = Landings,
        pattern_color = Landings,
        pattern_angle = Landings),
    size = 0.5,
    pattern_density = 0.002, #frequency of lines
    pattern_spacing = 0.015, #distance between lines
    pattern_size = 0.35       #thickness of lines
  ) +
  # Set color fills to use by class:
  scale_fill_manual(
    values = c(
      "500,000+"     = "#E31A1C",   
      "100,001 – 500,000" = "#8E7C8E",   
      "50,001 – 100,000"  = "white",   
      "5,001 – 50,000"   = "white",
      "1 – 5,000"       = "white",
      "0"               = "white"
    ),
    breaks = legend_levels,
    labels = legend_levels,
    drop = FALSE
  ) +
  # Set patterns by class:
  scale_pattern_manual(
    values = c(
      "500,000+"     = "none",
      "100,001 – 500,000" = "none",
      "50,001 – 100,000"  = "crosshatch",
      "5,001 – 50,000"   = "stripe",
      "1 – 5,000"       = "stripe",
      "0"               = "none"
    ),
    breaks = legend_levels,
    drop = FALSE
  ) +
  # Set pattern (line) colors by class:
  scale_pattern_color_manual(
    values = c(
      "500,000+"     = NA,
      "100,001 – 500,000" = NA,
      "50,001 – 100,000"  = "darkred",   # red crosshatch
      "5,001 – 50,000"   = "darkgreen",  # green diagonal
      "1 – 5,000"       = "darkblue",  # blue horizontal
      "0"               = NA
    ),
    breaks = legend_levels,
    drop = FALSE
  )+
  # Set pattern angles by class:
  scale_pattern_angle_manual(
    values = c(
      "500,000+"         = 0,
      "100,001 – 500,000" = 0,
      "50,001 – 100,000"  = 45,
      "5,001 – 50,000"   = 45,
      "1 – 5,000"       = 0,   
      "0"               = 0
    ),
    breaks = legend_levels,
    drop = FALSE
  ) +
  # Control stripe orientation
  guides(
    fill = "none", #guide_legend(title = "Stone crabs \n2024 Commercial \nLandings (lbs)"),
    pattern_color = "none",
    pattern_angle = "none",
    pattern = guide_legend(
      title = "Stone crab \n2024/2025 commercial \nlandings (lbs)",
      override.aes = list(
        pattern_angle = c(0, 0, 45, 45, 0),
        pattern_size = 0.5,
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
    legend.position = "inside", legend.position.inside = c(0.20, 0.22),
    legend.frame = element_blank(), 
    legend.text = element_text(hjust = 1, size = 12), legend.title = element_text(size = 14),
    # Plot formatting 
    axis.text = element_blank(), axis.ticks = element_blank(), 
    panel.grid =  element_blank())

#
# 750000 + map ----

legend_levels <- c(
  "750,000+",
  "500,001 – 750,000",
  "100,001 – 500,000",
  "50,001 – 100,000",
  "5,001 – 50,000",
  "1 – 5,000"
)

# Join data to counties
fl_counties_joined <- fl_counties %>%
  left_join(crab_data, by = c("NAME" = "County")) %>%
  mutate(Landings = factor(case_when(
    Sum.of.pounds > 750000 ~ "750,000+",
    Sum.of.pounds >= 500001 & Sum.of.pounds <= 750000 ~ "500,001 – 750,000",
    Sum.of.pounds >= 100001 & Sum.of.pounds <= 500000 ~ "100,001 – 500,000",
    Sum.of.pounds >= 50001  & Sum.of.pounds <= 100000 ~ "50,001 – 100,000",
    Sum.of.pounds >= 5001  & Sum.of.pounds <= 50000  ~ "5,001 – 50,000",
    Sum.of.pounds >= 1     & Sum.of.pounds <= 5000  ~ "1 – 5,000",
    TRUE ~ "0"
  ),
  levels = c(legend_levels, "0"))) 

# Check for missing data, create dummies as needed for proper legend display:
missing_levels <- setdiff(levels(fl_counties_joined$Landings), unique(fl_counties_joined$Landings))

if(length(missing_levels) > 0){
  dummy_rows <- data.frame(
    NAME = paste0("dummy_", missing_levels),
    Landings = factor(missing_levels, levels = levels(fl_counties_joined$Landings))
  )
  
  # Give dummy geometry (sf requires geometry column)
  dummy_rows <- st_sf(dummy_rows, 
                      geometry = st_sfc(lapply(missing_levels, function(x) st_point(c(NA_real_, NA_real_))),
                                        crs = 4269))
  
  # Bind to main data
  fl_counties_joined <- bind_rows(fl_counties_joined, dummy_rows)
}

# Make a map:
ggplot() +
  # Set data for color/fill/patterns
  geom_sf_pattern(
    data = fl_counties_joined,
    aes(fill = Landings,
        pattern = Landings,
        pattern_color = Landings,
        pattern_angle = Landings),
    size = 0.5,
    pattern_density = 0.002, #frequency of lines
    pattern_spacing = 0.015, #distance between lines
    pattern_size = 0.35       #thickness of lines
  ) +
  # Set color fills to use by class:
  scale_fill_manual(
    values = c(
      "750,000+"     = "#E31A1C",   
      "500,001 – 750,000" = "#1F78B4",   
      "100,001 – 500,000" = "#8E7C8E",   
      "50,001 – 100,000"  = "white",   
      "5,001 – 50,000"   = "white",
      "1 – 5,000"       = "white",
      "0"               = "white"
    ),
    breaks = legend_levels,
    labels = legend_levels,
    drop = FALSE
  ) +
  # Set patterns by class:
  scale_pattern_manual(
    values = c(
      "750,000+"     = "none",
      "500,001 – 750,000" = "none",
      "100,001 – 500,000" = "none",
      "50,001 – 100,000"  = "crosshatch",
      "5,001 – 50,000"   = "stripe",
      "1 – 5,000"       = "stripe",
      "0"               = "none"
    ),
    breaks = legend_levels,
    drop = FALSE
  ) +
  # Set pattern (line) colors by class:
  scale_pattern_color_manual(
    values = c(
      "750,000+"     = NA,
      "500,001 – 750,000" = NA,
      "100,001 – 500,000" = NA,
      "50,001 – 100,000"  = "darkred",   # red crosshatch
      "5,001 – 50,000"   = "darkgreen",  # green diagonal
      "1 – 5,000"       = "darkblue",  # blue horizontal
      "0"               = NA
    ),
    breaks = legend_levels,
    drop = FALSE
  )+
  # Set pattern angles by class:
  scale_pattern_angle_manual(
    values = c(
      "750,000+"         = 0,
      "500,001 – 750,000" = 0,
      "100,001 – 500,000" = 0,
      "50,001 – 100,000"  = 45,
      "5,001 – 50,000"   = 45,
      "1 – 5,000"       = 0,   
      "0"               = 0
    ),
    breaks = legend_levels,
    drop = FALSE
  ) +
  # Control stripe orientation
  guides(
    fill = "none", #guide_legend(title = "Stone crabs \n2024 Commercial \nLandings (lbs)"),
    pattern_color = "none",
    pattern_angle = "none",
    pattern = guide_legend(
      title = "Stone crabs \n2024 Commercial \nLandings (lbs)",
      override.aes = list(
        pattern_angle = c(0, 0, 0, 45, 45, 0),
        pattern_size = 0.8,
        fill = c("#E31A1C", "#1F78B4", "#8E7C8E", "white", "white", "white"),  # match fill for clarity
        color = "black",  # outline color
        pattern_color = c(NA, NA, NA, "darkred", "darkgreen", "darkblue")
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
    legend.position = "inside", legend.position.inside = c(0.16, 0.22),
    legend.frame = element_blank(), 
    legend.text = element_text(hjust = 1, size = 12), legend.title = element_text(size = 14),
    # Plot formatting 
    axis.text = element_blank(), axis.ticks = element_blank(), 
    panel.grid =  element_blank())

#
# Saving map output ----
#
## Save map for web applications:
ggsave(
  "Stone crab/stone_crab_map.png",
  width = 8,
  height = 6,
  dpi = 150
)
## Save for print applications:
ggsave(
  "Stone crab/stone_crab_map.tiff",
  width = 7,
  height = 6,
  dpi = 600,
  compression = "lzw"
)
