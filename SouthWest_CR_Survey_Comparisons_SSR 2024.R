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
Region <- c("SouthWest") #SouthEast, SouthWest, NorthEast, NorthWest, NorthCentral
Site_Code <- c("CR") #CR, F5, G5; TB F4, F3
Compiled_date <- c("2023-08-09") #Microgrid data compiled date
#State_Grid <- c("F5")
#Alt_State_Grid <- c("G5") 
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
#
OR_2003 <- st_read(paste0("../Base Layers/WMD SHapefiles/", Site_Code, "/CR_EB_OysterReefs_2003.shp"))
OR_2010 <- st_read(paste0("../Base Layers/WMD SHapefiles/", Site_Code, "/cal_substrate_classification_july-2011_utm17n.shp"))
OR_2019 <- st_read(paste0("../Base Layers/WMD SHapefiles/", Site_Code, "/cal_oysters_nov-2019_FINAL_fl-sp83w.shp"))
#
#
#
####Check and filter data, assign grid P/A####
#
head(OR_2003) #Just oyster polygons. Good.
OR_2003_clean <- st_transform(OR_2003, crs="+proj=longlat +datum=WGS84 +no_defs +type=crs")
#
head(OR_2010)
unique(OR_2010$CLASS_NAME) #Want "Sand / Shell / Rock w-OYS"
OR_2010_clean <- OR_2010 %>% subset(CLASS_NAME == "Sand / Shell / Rock w-OYS") %>%  st_transform(crs="+proj=longlat +datum=WGS84 +no_defs +type=crs")
head(OR_2019) #Just oyster polygons. Good.
OR_2019_clean <- st_transform(OR_2019, crs="+proj=longlat +datum=WGS84 +no_defs +type=crs")
#
#
#
###Assign 2003 grids####
st_crs(MicroGrid) == st_crs(OR_2003_clean) #Confirm matching CRS
#
##Subset of grid cells with Oysters in 2003
Oysters_03 <- MicroGrid[lengths(st_intersects(MicroGrid, st_as_sf(OR_2003_clean))) > 0,] %>% 
  mutate(OY_2003 = "1") 
#
Grid_Oysters_2003 <- Oysters_03 %>% st_set_geometry(NULL)
#Plot Estuary area against Oyster presence to confirm areas similar
tmap_arrange(
  tm_shape(MicroGrid) + tm_polygons(col = "Section"),
  tm_shape(Oysters_03) + tm_polygons(),
  nrow = 1, ncol = 2)
#
#Add survey designations to working df
Oyster_surveys_1 <- full_join(MicroGrid , Grid_Oysters_2003) %>% #Add data by ID
  mutate(OY_2003 = ifelse(is.na(OY_2003), 0, 1)) #Change NA to 0
#
head(Oyster_surveys_1)
#
##Plot to confirm join was correct
tm_shape(Oyster_surveys_1) + tm_polygons(col ="OY_2003") + tm_layout(frame = FALSE)
#
#
#
###Assign 2010 grids####
st_crs(MicroGrid) == st_crs(OR_2010_clean) #Confirm matching CRS
#
##Subset of grid cells with Oysters in 2010
sf_use_s2(FALSE)
Oysters_10 <- MicroGrid[lengths(st_intersects(MicroGrid, st_as_sf(OR_2010_clean))) > 0,] %>% 
  mutate(OY_2010 = "1") 
sf_use_s2(TRUE)
#
Grid_Oysters_2010 <- Oysters_10 %>% st_set_geometry(NULL)
#Plot Estuary area against Oyster presence to confirm areas similar
tmap_arrange(
  tm_shape(MicroGrid) + tm_polygons(col = "Section"),
  tm_shape(Oysters_10) + tm_polygons(),
  nrow = 1, ncol = 2)
#
#Add survey designations to working df
Oyster_surveys_2 <- full_join(Oyster_surveys_1 , Grid_Oysters_2010) %>% #Add data by ID
  mutate(OY_2010 = ifelse(is.na(OY_2010), 0, 1)) #Change NA to 0
#
head(Oyster_surveys_2)
#
##Plot to confirm join was correct
tm_shape(Oyster_surveys_2) + tm_polygons(col ="OY_2010") + tm_layout(frame = FALSE)
#
#
#
###Assign 2019 grids####
st_crs(MicroGrid) == st_crs(OR_2019_clean) #Confirm matching CRS
#
##Subset of grid cells with Oysters in 2019
Oysters_19 <- MicroGrid[lengths(st_intersects(MicroGrid, st_as_sf(OR_2019_clean))) > 0,] %>% 
  mutate(OY_2019 = "1") 
#
Grid_Oysters_2019 <- Oysters_19 %>% st_set_geometry(NULL)
#Plot Estuary area against Oyster presence to confirm areas similar
tmap_arrange(
  tm_shape(MicroGrid) + tm_polygons(col = "Section"),
  tm_shape(Oysters_19) + tm_polygons(),
  nrow = 1, ncol = 2)
#
#Add survey designations to working df
Oyster_surveys_3 <- full_join(Oyster_surveys_2 , Grid_Oysters_2019) %>% #Add data by ID
  mutate(OY_2019 = ifelse(is.na(OY_2019), 0, 1)) #Change NA to 0
#
head(Oyster_surveys_3)
#
##Plot to confirm join was correct
tm_shape(Oyster_surveys_3) + tm_polygons(col ="OY_2019") + tm_layout(frame = FALSE)
#
#
#
