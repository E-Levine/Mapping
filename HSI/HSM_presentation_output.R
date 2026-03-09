# Code to create HSM summary maps and figures for presentations
#
#
#Load packages to work with. Install missing packages as needed.
if (!require("pacman")) {install.packages("pacman")} #- MAKE SURE PACMAN IS INSTALLED AND RUNNING!
pacman::p_load(readxl, plyr, tidyverse, #xtable, 
               sf, tigris, ggpattern, nhdplusTools,
               geodata, writexl,
               install = TRUE) 
#
#
#
# Setup ----
#
# Use options so tigris returns sf objects
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)
#
# Get starting data for US
US <- gadm(country = "USA", level = 1, path = tempdir())
#
# Load HSM summary info
HSMs <- read_xlsx("HSI/Data/Existing_HSM_summary.xlsx", sheet = 1, .name_repair = "universal")
head(HSMs)
#
#
# Formatting ----
#
basetheme <- theme_classic()+
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    axis.title = element_blank(),#element_text(size = 14, color = "black"), 
    axis.text =  element_text(size = 15, color = "black", family = "Arial"),
    axis.text.x = element_text(angle = 30, vjust = 0.5)
  )
basetheme2 <- theme(text = element_text(family = "sans"),
                   panel.grid = element_blank(),
                   panel.border = element_blank(), 
                   axis.line = element_line(color = "black"),
                   axis.ticks.length = unit(-0.15, "cm"), 
                   axis.text.x = element_text(color = "black", margin = margin(t=0.5, r=0.5, b=0, l=0.5, unit = "cm")), 
                   axis.text.y = element_text(color = "black", margin = margin(t=0, r=0.5, b=0, l=0, unit = "cm")))
#
legendtheme <- theme(
  legend.title = element_text(size = 14, color = "black", family = "Arial"),
  legend.text = element_text(size = 13, color = "black", family = "Arial"),
  legend.background = element_blank(),
  legend.key = element_blank()
)
#
#
#
# Southeast map ----
#
# Create base map
US_map <- st_as_sf(US)
#
# Gather state abbreviations and create df for mapping
US_map$id <- 1:nrow(US)
US.df <- data.frame(id = US_map$id, state = str_extract(US_map$VARNAME_1, "[A-Z]{2}"))
US.df <- US.df %>%
  # Update Rhode Island
  mutate(state = if_else(is.na(state), "RI", state))
#
US_map.df <- inner_join(US_map, US.df, by = "id") %>%
  filter(!(state %in% c("AK", "HI"))) %>%
  # Add HSM counts
  left_join(HSMs %>% group_by(Abbr) %>% summarise(Count = n()), 
            by = c("state" = "Abbr")) %>%
  mutate(Count = as.factor(Count))
#
# state of Florida
florida <- US_map.df %>% filter(state == "FL")
#
# Center abbreviations on each state
ST.centers_ll <- st_point_on_surface(US_map.df)
ST.centers <- data.frame(
  st_coordinates(ST.centers_ll),
  state = US_map.df$state
) %>%
  # Limit to states with HSMs
  filter(state %in% HSMs$Abbr) %>%
  # nudge some locations
  mutate(Y = if_else(state == "NC", Y + 0.45, 
                     if_else(state == "VA", Y - 0.3, 
                             if_else(state == "LA", Y - 0.4, Y))), 
         X = if_else(state == "NC", X + 0.5, 
                     if_else(state == "FL", X + 0.15, 
                             if_else(state == "TX", X + 1, 
                                     if_else(state == "LA", X +0.6, X)))))

#
(p1 <- ggplot()+
  # map of states
  geom_sf(data = US_map.df, fill = "#CCCCCC",color = "black")+
  #color states with HSM
  geom_sf(data = US_map.df %>% filter(state %in% HSMs$Abbr), 
          aes(fill = Count), color = "black", alpha = 0.8)+
  # with Florida colored
  geom_sf(data = florida, fill = NA, color = "darkred", size = 1)+
  # add State abbreviations
  geom_text(data = ST.centers, aes(X, Y, label = state), size = 6, color = "black", fontface = "bold")+
  # Modify colors used
  scale_fill_viridis_d()+
  # Make look "normal"
  coord_sf()+
  # Limit to southeast
  scale_x_continuous("", limits = c(-106, -72))+
  scale_y_continuous("", limits = c(25, 45)) +
  # Formatting
  basetheme +
  theme(panel.border = element_rect(color = "black"),
        panel.background = element_rect(color = "white"),
        axis.text = element_text(size = 15, color = "black", family = "Arial"))+
  legendtheme +
  labs(fill = "HSM Count"))
#
ggsave(
  filename = "HSI/Output/HSMs_by_State.png",
  plot = p1,
  width = 9,
  height = 5,
  units = "in",
  dpi = 300 # Use 300 dpi for high quality
)
#
#
#
# Parameter summary ----
#
Param_count <- HSMs %>%
  dplyr::select(Salinity:Larval) %>%
  pivot_longer(names_to = "Parameter", cols = everything()) %>%
  group_by(Parameter) %>%
  summarise(Count = sum(value)) %>%
  mutate(Parameter = str_replace_all(Parameter, "\\.", "/"))
#
(p2 <-  Param_count %>%
    mutate(fill_col = if_else(rank(-Count) <= 4, "darkgreen", "#333333")) %>%
    ggplot(aes(x = Parameter, y = Count, fill = fill_col)) +
    geom_bar(stat = "identity") +
    scale_fill_identity() +
    scale_x_discrete(expand = c(0.075,0))+
    scale_y_continuous("Number of HSM", limits = c(0, 24), expand = c(0,0), breaks = seq(0, 24, by = 6))+
    basetheme+
    theme(axis.title = element_text(size = 18, color = "black", face = "bold"),
          axis.text.x = element_text(vjust = 0.68)))
#
write_xlsx(Param_count, "HSI/Output/Param_count.xlsx")
ggsave(
  filename = "HSI/Output/Parameters_by_HSMs.png",
  plot = p2,
  width = 9,
  height = 5,
  units = "in",
  dpi = 300 # Use 300 dpi for high quality
)

# Ave param per model by state ----
#

US_map.df <- US_map.df %>%
  left_join(HSMs %>%
              group_by(Abbr) %>%
              summarise(Mean = round(mean(Total, na.rm = T),0),
                        Min  = min(Total, na.rm = T),
                        Max = max(Total, na.rm = T)),
            by = c("state" = "Abbr")) %>%
  mutate(across(c(Mean, Min, Max), as.factor))
#
(p3 <- ggplot()+
   # map of states
   geom_sf(data = US_map.df, fill = "#CCCCCC",color = "black")+
   #color states with HSM
   geom_sf(data = US_map.df %>% filter(state %in% HSMs$Abbr), 
           aes(fill = Mean), color = "black", alpha = 0.6)+
   # add State abbreviations
   geom_text(data = ST.centers, aes(X, Y, label = state), size = 6, color = "black", fontface = "bold")+
   # Modify colors used
   scale_fill_viridis_d(option = "C")+
   # Make look "normal"
   coord_sf()+
   # Limit to southeast
   scale_x_continuous("", limits = c(-106, -72))+
   scale_y_continuous("", limits = c(25, 45)) +
   # Formatting
   basetheme +
   theme(panel.border = element_rect(color = "black"),
         panel.background = element_rect(color = "white"),
         axis.text = element_text(size = 15, color = "black", family = "Arial"))+
   legendtheme +
   labs(fill = "Parameter\nCount"))
#
ggsave(
  filename = "HSI/Output/Parameters_by_State.png",
  plot = p3,
  width = 9,
  height = 5,
  units = "in",
  dpi = 300 # Use 300 dpi for high quality
)
#
#
#
#