##Oyster Survey comparisons for SSR 2024
#Mapping CRE surveys from 2003, 2010 (7/2011 file), and 2019 for comparisons
#
#
#
#Load require packages (will install packages as necessary) - MAKE SURE PACMAN IS INSTALLED AND RUNNING!
if (!require("pacman")) {install.packages("pacman")}
pacman::p_load(plyr, tidyverse, #Df manipulation, basic summary
               sf, raster, terra,
               leaflet, tmap, openxlsx,
               glue, #SQL
               install = TRUE) #Mapping and figures
#
#
#Assign Region, Estuary Code, and StateGrid(s). Only assign the Alternate state grid if required.
Region <- c("SouthEast") #SouthEast, SouthWest, NorthEast, NorthWest, NorthCentral
Site_Code <- c("SL") #
Compiled_date <- c("2023-07-20") #Microgrid data compiled date
#
#
#
####Load files####
#
##Estuary specific MicroGrid data 
MicroGrid <- st_read(paste0("Maps/Shapefiles/", Site_Code, "_compiled_", Compiled_date, ".shp"))
plot(MicroGrid[2])
#
##Estuary area 
Estuary_area <- st_read(paste0("../Base Layers/Site_Region_Areas/", Site_Code, ".kml"))
#
#Oyster layer files
OR_2010 <- st_read(paste0("../Base Layers/WMD SHapefiles/", Site_Code, "/sle_substrate_classification_july-2011.shp"))
OR_2019 <- st_read(paste0("../Base Layers/WMD SHapefiles/", Site_Code, "/Oysters_only_SLE_Mapping_2019.shp"))
#
#FL shoreline 
FL_outline <- st_read(paste0("../Base Layers/FL_Outlines/FL_Outlines.shp"))
qtm(FL_outline)
#
#
#
####Check and filter data, assign grid P/A####
#
head(OR_2010)
unique(OR_2010$CLASS_NAME) #Want "Sand / Shell / Rock w-OYS"
OR_2010_clean <- OR_2010 %>% subset(CLASS_NAME == "Oyster" | CLASS_NAME == "Relic shell / shell hash") %>%  st_transform(crs="+proj=longlat +datum=WGS84 +no_defs +type=crs")
#
head(OR_2019)
unique(OR_2019$SUBSTRATE) #INCLUDES mUCK/SAND?
unique(OR_2019$BENTHIC) #BUT ALL OYSTERS
OR_2019_clean <- st_transform(OR_2019, crs="+proj=longlat +datum=WGS84 +no_defs +type=crs")
#
#
#
###Assign 2010 grids####
st_crs(MicroGrid) == st_crs(OR_2010_clean) #Confirm matching CRS
#
##Subset of grid cells with Oysters in 2010
Oysters_10 <- MicroGrid[lengths(st_intersects(MicroGrid, st_as_sf(OR_2010_clean))) > 0,] %>% 
  mutate(OY_2010 = "1") 
#
Grid_Oysters_2010 <- Oysters_10 %>% st_set_geometry(NULL)
#Plot Estuary area against Oyster presence to confirm areas similar
tmap_arrange(
  tm_shape(MicroGrid) + tm_polygons(col = "Section"),
  tm_shape(Oysters_10) + tm_polygons(),
  nrow = 1, ncol = 2)
#
#Add survey designations to working df
Oyster_surveys_1 <- full_join(MicroGrid, Grid_Oysters_2010) %>% #Add data by ID
  mutate(OY_2010 = ifelse(is.na(OY_2010), 0, 1)) #Change NA to 0
#
head(Oyster_surveys_1)
#
##Plot to confirm join was correct
tm_shape(Oyster_surveys_1) + tm_polygons(col ="OY_2010") + tm_layout(frame = FALSE)
#
#
#
###Assign 2019 grids####
st_crs(MicroGrid) == st_crs(OR_2019_clean) #Confirm matching CRS
#
##Subset of grid cells with Oysters in 2019
sf_use_s2(FALSE)
Oysters_19 <- MicroGrid[lengths(st_intersects(MicroGrid, st_as_sf(OR_2019_clean))) > 0,] %>% 
  mutate(OY_2019 = "1")
sf_use_s2(TRUE)
#
Grid_Oysters_2019 <- Oysters_19 %>% st_set_geometry(NULL)
#Plot Estuary area against Oyster presence to confirm areas similar
tmap_arrange(
  tm_shape(MicroGrid) + tm_polygons(col = "Section"),
  tm_shape(Oysters_19) + tm_polygons(),
  nrow = 1, ncol = 2)
#
#Add survey designations to working df
Oyster_surveys_2 <- full_join(Oyster_surveys_1 , Grid_Oysters_2019) %>% #Add data by ID
  mutate(OY_2019 = ifelse(is.na(OY_2019), 0, 1)) #Change NA to 0
#
head(Oyster_surveys_2)
#
##Plot to confirm join was correct
tm_shape(Oyster_surveys_2) + tm_polygons(col ="OY_2019") + tm_layout(frame = FALSE)
#
#
#
####Relate survey data####
#
Oyster_surveys <- Oyster_surveys_2 %>% mutate(v10_19 = case_when(OY_2010 == 1 & OY_2019 == 0 ~ -1,
                                                                 OY_2010 == 0 & OY_2019 == 1 ~ 1, 
                                                                 OY_2010 == 1 & OY_2019 == 1 ~ 0,
                                                                 OY_2010 == 0 & OY_2019 == 0 ~ 0,
                                                                 TRUE ~ NA),
                                              Sum = case_when(OY_2010 == 1 & OY_2019 == 0 ~ "2010",
                                                                 OY_2010 == 0 & OY_2019 == 1 ~ "2019", 
                                                                 OY_2010 == 1 & OY_2019 == 1 ~ "Both",
                                                                 OY_2010 == 0 & OY_2019 == 0 ~ "Neither",
                                                                 TRUE ~ NA)) %>%
  rowwise() %>% 
  mutate(v10_19 = factor(v10_19, levels = c("-1", "0", "1"), labels = c("-1", "0", "1")))
#
#
tm_shape(MicroGrid, bbox = extent(Oysters_10)) + tm_borders(col = "#CCCCCC") +
  tm_shape(Oyster_surveys) + tm_polygons("v10_19", palette = c("red", "#999999", "#006633"))+
  tm_shape(name = "Shoreline", st_make_valid(FL_outline)) + tm_polygons()+
  tm_layout(title = "2010 vs. 2019 surveys", title.position = c("center", "top"))
#
tm_shape(MicroGrid, bbox = extent(Oysters_10)) + tm_borders(col = "#CCCCCC") +
  tm_shape(Oyster_surveys) + tm_polygons("Sum", palette = c("red", "orange", "#006633", "#999999"))+
  tm_shape(name = "Shoreline", st_make_valid(FL_outline)) + tm_polygons()+
  tm_layout(title = "2010 & 2019 surveys", title.position = c("center", "top"))
#
#
#
###Summarize grid cells
Oyster_surveys %>% as.data.frame() %>% group_by(v10_19) %>% summarise("2010-2019" = n()) %>% rename(Change = v10_19)
Oyster_surveys %>% as.data.frame() %>% group_by(Sum) %>% summarise(Obs. = n()) %>% rename(Change = SumChange) 
