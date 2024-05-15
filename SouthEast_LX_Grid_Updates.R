####Regional Florida MicroGrid Metadata Updates
#
#
#Use Alt+O to collapse all sections, Alt+Shift+O to expand all sections
#Each section builds upon the previous to add data to the microgrids:
#Data order: Estuary and Sections, Oysters in FL, SHA classifications, Seagrass area
#
#
#.rs.restartR() #Restarts session (good if rerunning after working with other files)
#graphics.off()  # turns off any plots from previous work session
#rm(list=ls(all=TRUE)) # clears out environment 
#
#Load require packages (should install missing packages as necessary)
if (!require("pacman")) {install.packages("pacman")} #- MAKE SURE PACMAN IS INSTALLED AND RUNNING!
pacman::p_load(plyr, tidyverse, #Df manipulation, basic summary
               sf, raster, terra,
               leaflet, tmap, 
               install = TRUE) #Mapping and figures
#
#Assign Region, Estuary Code, and StateGrid(s). Only assign the Alternate state grid if required.
Region <- c("SouthEast") #SouthEast, SouthWest, NorthEast, NorthWest, NorthCentral
Site_Code <- c("LX")
State_Grid <- c("H5")
Alt_State_Grid <- c("H4") 
#
###Throughout this file, a primary State_Grid is used with the option of an alternate or additional StateGrid.
###If an estuary falls completely within 1 StateGrid, skip lines for the "Alt" as instructed in each section.
#
#
####Load files - first run or updating multiple layers####
#
##MicroGrid - skip lines 34, 40, and 41 if no Alt.
MicroGrid <- st_read(paste0("../Base Layers/MicroGrids/Florida_MicroGrid_WGS84_",State_Grid,"_clip.shp"))
Alt_MicroGrid <- st_read(paste0("../Base Layers/MicroGrids/Florida_MicroGrid_WGS84_",Alt_State_Grid,"_clip.shp"))
#
#Check data, view map  to confirm area
plot(MicroGrid$geometry)
head(MicroGrid)
#
plot(Alt_MicroGrid$geometry) #Check alternate
head(Alt_MicroGrid)
#
#
#
#
##Reference Tables - Estuary names and SHA codes
(Estuary_long <- read.csv("../Reference Files/Estuary_SiteCodes.csv", na.string = c("Z", "", "NA", " ")))
(SHA_Codes <- read.csv("../Reference Files/SHA_Class_Codes.csv", na.string = c("Z", "", "NA", " ")) %>% 
    mutate(Subsection = factor(Subsection, unique(Subsection)))) #Set SHA priority
(Section_Order <- read.csv("../Reference Files/Section_Orders.csv", na.string = c("Z", "", "NA", " ")) %>%
    filter(Site == Site_Code) %>% arrange(Order) %>% #Limit to desired Site and order Sections
    mutate(Section = factor(Section, unique(Section)))) #Set Section priority
#
#
#
#
#
##Estuary area and Sections - change to whole estuary KML layer name and section names
#Copy and add Sections as needed for additional sections, changing the number sequentially
Estuary_area <- st_read(paste0("../Base Layers/Site_Region_Areas/",Site_Code,".kml"))
#
plot(Estuary_area[2])
head(Estuary_area)
#
Section1 <- st_read("../Base Layers/Site_Region_Areas/LX-North.kml") 
plot(Section1[2])
#
Section2 <- st_read("../Base Layers/Site_Region_Areas/LX-South.kml") 
plot(Section2[2])
#
#
#
#
#
##Oysters in Florida - run line 78 for 1 StateGrid or line 81 for 2 StateGrids. Make sure to only run one or the other.
FL_Oysters_all <- crop(as(st_read("../Base Layers/Oyster Beds in Florida/Oyster_Beds_in_Florida.shp"), "Spatial"),
                       extent(MicroGrid))
#
FL_Oysters_all <- crop(as(st_read("../Base Layers/Oyster Beds in Florida/Oyster_Beds_in_Florida.shp"), "Spatial"),
                       extent(merge(extent(MicroGrid), extent(Alt_MicroGrid))))
#
#Check data
plot(FL_Oysters_all)
head(FL_Oysters_all)
st_crs(FL_Oysters_all)
#
##Limit area data to State_grid area - skip lines 93-94 if no Alt
FL_Oysters <- st_as_sf(crop(FL_Oysters_all, extent(MicroGrid)))
plot(FL_Oysters[4])
#
FL_Oysters_alt <- st_as_sf(crop(FL_Oysters_all, extent(Alt_MicroGrid)))
plot(FL_Oysters_alt[4])
#
#
#
#
#
#
##Shellfish Harvest Area
SHA_all <- as(st_read("../Base Layers/SHA/All_SHAs.shp"), "sf")
plot(SHA_all[8])
st_crs(SHA_all)
SHA_all_t <- st_transform(SHA_all, crs="+proj=longlat +datum=WGS84 +no_defs +type=crs") %>% #Transform coordinates
  rename("SHA_Class" = "CLASSTYP", "SHA_Name" = "SH_NAME") %>% #rename columns for merging
  left_join(SHA_Codes) #Add subsection information
#
plot(SHA_all_t[8])
#
##Limit to State_grid area - skip lines 117-121 if no Alt
SHA_grid <- st_crop(st_make_valid(SHA_all_t), #Make valid polygons and crop 
                    xmin = min(MicroGrid$Long_DD_X), ymin = min(MicroGrid$Lat_DD_Y), 
                    xmax = max(MicroGrid$Long_DD_X), ymax = max(MicroGrid$Lat_DD_Y)) 
plot(SHA_grid[8])
#
SHA_grid_alt <- st_crop(st_make_valid(SHA_all_t), #Make valid polygons and crop 
                        xmin = min(Alt_MicroGrid$Long_DD_X), ymin = min(Alt_MicroGrid$Lat_DD_Y), 
                        xmax = max(Alt_MicroGrid$Long_DD_X), ymax = max(Alt_MicroGrid$Lat_DD_Y))
#
plot(SHA_grid_alt[8])
#
#
#
#
#Bathymetry
Depth <- as(st_read(paste0("../Base Layers/Bathymetry/", State_Grid, "_depth.shp")), "Spatial")
crs(Depth)
#Remove land (>0 values) 
Depth <- Depth[Depth@data$Depth >= 0,]
plot(Depth)
#
#Skip lines 134-138 if no Alt
Depth_alt <- as(st_read(paste0("../Base Layers/Bathymetry/", Alt_State_Grid, "_depth.shp")), "Spatial")
crs(Depth_alt)
#Remove land (>0 values) 
Depth_alt <- Depth_alt[Depth_alt@data$Depth >= 0,]
plot(Depth_alt)
#
#
##Make sure data is limited to State_grid area - skip lines 145-6 if no Alt
Depth <- st_as_sf(crop(Depth, extent(Estuary_area)))
plot(Depth)
#
Depth_alt <- st_as_sf(crop(Depth_alt, extent(Estuary_area)))
plot(Depth_alt)
#
#
#
#
##Seagrass areas - limited to grid areas
All_seagrass <- as(st_read("../Base Layers/Seagrass/Seagrass_Habitat_in_Florida.shp"), "Spatial")
#Limit to primary state grid
Seagrass <- st_as_sf(crop(All_seagrass, extent(Estuary_area)))
#Check data
plot(Seagrass[Seagrass$SEAGRASS == "Continuous",])
head(Seagrass)
st_crs(Seagrass)
#
##Limit to Alt State_grid area - skip lines 154-9 if no Alt
Seagrass_alt <- st_as_sf(crop(All_seagrass, extent(Estuary_area)))
#Check data
plot(Seagrass_alt[Seagrass_alt$SEAGRASS == "Continuous",])
head(Seagrass_alt)
st_crs(Seagrass_alt)
#
#
#
#
#
#
####Load files - updating specific layers####
#
##Estuary MicroGrid - ONLY FOR USE WITH EXISTING DATA - RETURN TO LINE 30 IF FIRST RUN
#
#Date of previous data compilation (located in file name)
Compiled_date <- ("2023-07-27") #format "YYYY-MM-DD"
#
MicroGrid <- st_read(paste0("Maps/Shapefiles/", Site_Code,"_compiled_", Compiled_date, ".shp"))
#
#Check data, view map  to confirm area
plot(MicroGrid$geometry)
head(MicroGrid)
#
#
#
#
##Reference Tables - Estuary names and SHA codes - RUN
(Estuary_long <- read.csv("../Reference Files/Estuary_SiteCodes.csv", na.string = c("Z", "", "NA", " ")))
(SHA_Codes <- read.csv("../Reference Files/SHA_Class_Codes.csv", na.string = c("Z", "", "NA", " ")) %>% 
    mutate(Subsection = factor(Subsection, unique(Subsection)))) #Set SHA priority
(Section_Order <- read.csv("../Reference Files/Section_Orders.csv", na.string = c("Z", "", "NA", " ")) %>%
    filter(Site == Site_Code) %>% arrange(Order) %>% #Limit to desired Site and order Sections
    mutate(Section = factor(Section, unique(Section)))) #Set Section priority
#
#
#
#
#
##Estuary area and Sections - change to whole estuary KML layer name and section names - RUN
#Copy and add Sections as needed for additional sections, changing the number sequentially
Estuary_area <- st_read("../Base Layers/Site_Region_Areas/TB.kml")
#
plot(Estuary_area[2])
head(Estuary_area)
#
Section1 <- st_read("../Base Layers/Site_Region_Areas/TB-Lower.kml") 
plot(Section1[2])
#
Section2 <- st_read("../Base Layers/Site_Region_Areas/TB-Middle.kml") 
plot(Section2[2])
#
Section3 <- st_read("../Base Layers/Site_Region_Areas/TB-OldTB.kml") 
plot(Section3[2])
#
Section4 <- st_read("../Base Layers/Site_Region_Areas/TB-Hillsborough.kml") 
plot(Section4[2])
#
Section5 <- st_read("../Base Layers/Site_Region_Areas/TB-BocaCiega.kml") 
plot(Section5[2])
#
Section6 <- st_read("../Base Layers/Site_Region_Areas/TB-RiverManatee.kml") 
plot(Section6[2])
#
#
#
#
##Oysters in Florida - only run if updating data layer
FL_Oysters_all <- crop(as(st_read("../Base Layers/Oyster Beds in Florida/Oyster_Beds_in_Florida.shp"), "Spatial"),
                       extent(MicroGrid))
#
#Check data
plot(FL_Oysters_all)
head(FL_Oysters_all)
st_crs(FL_Oysters_all)
#
FL_Oysters <- FL_Oysters_all
#
#
#
#
##Shellfish Harvest Area - only run if updating data layer
SHA_all <- as(st_read("../Base Layers/SHA/All_SHAs.shp"), "sf")
plot(SHA_all[8])
st_crs(SHA_all)
SHA_all_t <- st_transform(SHA_all, crs="+proj=longlat +datum=WGS84 +no_defs +type=crs") %>% #Transform coordinates
  rename("SHA_Class" = "CLASSTYP", "SHA_Name" = "SH_NAME") %>% #rename columns for merging
  left_join(SHA_Codes) #Add subsection information
#
plot(SHA_all_t[8])
#
##Limit to State_grid area 
SHA_grid <- st_crop(st_make_valid(SHA_all_t), #Make valid polygons and crop 
                    xmin = min(MicroGrid$Long_DD_X), ymin = min(MicroGrid$Lat_DD_Y), 
                    xmax = max(MicroGrid$Long_DD_X), ymax = max(MicroGrid$Lat_DD_Y)) 
plot(SHA_grid[8])
#
#
#
#
#
#Bathymetry - only run if updating data layer
Depth <- as(st_read(paste0("../Base Layers/Bathymetry/", State_Grid, "_depth.shp")), "Spatial")
crs(Depth)
#Remove land (>0 values) 
Depth <- Depth[Depth@data$Depth >= 0,]
plot(Depth)
#
#
##Make sure data is limited to State_grid area - skip lines 145-6 if no Alt
Depth <- st_as_sf(crop(Depth, extent(MicroGrid)))
plot(Depth)
#
#
#
#
##Seagrass areas - limited to grid areas - only run if updating data layer
All_seagrass <- as(st_read("../Base Layers/Seagrass/Seagrass_Habitat_in_Florida.shp"), "Spatial")
#Limit to primary state grid
Seagrass <- st_as_sf(crop(All_seagrass, extent(MicroGrid)))
#Check data
plot(Seagrass[Seagrass$SEAGRASS == "Continuous",])
head(Seagrass)
st_crs(Seagrass)
#
#
#
#Assign microgrid data to object to work with
Site_Grid <- MicroGrid
plot(Site_Grid$geometry)
#
#
#
#
####Base Site microgrid cells to work with - Skip if updating data layer####
#
Site_Grid <- MicroGrid[lengths(st_intersects(MicroGrid, Estuary_area))> 0,]  %>% dplyr::select(-Estuary)
plot(Site_Grid$geometry)
#
#Skip lines 178-9, 183 if no Alt
Site_Grid_alt <- Alt_MicroGrid[lengths(st_intersects(Alt_MicroGrid, Estuary_area))> 0,]  %>% dplyr::select(-Estuary)
plot(Site_Grid_alt$geometry)
#
##Save working df to keep original area grids intact
Site_Grid_working <- Site_Grid
Site_Grid_alt_working <- Site_Grid_alt
#
#
#
####1. Add Estuary and Section Information - CANNOT SKIP UNLESS UPDATING DATA LAYERS - SKIP TO #1 MERGE SECTION FOR DATA LAYER UPDATES####
#
#Combine and remove duplicates based on priority ranking - add tmp# in "Section_cells_geo" for all sections as needed
tmp1 <- Site_Grid[lengths(st_intersects(Site_Grid, Section1)) > 0,] %>% #Limit to section area
  mutate(Site = Site_Code, Section = "N") %>%  left_join(Estuary_long, by = "Site") #Add Site, Section, Estuary info
tmp2 <- Site_Grid[lengths(st_intersects(Site_Grid, Section2)) > 0,] %>% 
  mutate(Site = Site_Code, Section = "S") %>% left_join(Estuary_long, by = "Site")
#tmp3 <- Site_Grid[lengths(st_intersects(Site_Grid, Section3)) > 0,] %>% 
#  mutate(Site = Site_Code, Section = "S") %>% left_join(Estuary_long, by = "Site")
#
#
#Combine and remove duplicates based on priority ranking - add tmp# in "Section_cells_geo" for all sections as needed
(Section_cells_geo <-  rbind(tmp1, tmp2) %>% #Join all sections then reorder Section values
    mutate(Section = factor(Section, levels = unique(Section_Order$Section[order(Section_Order$Order)]), ordered = TRUE)) %>%
    arrange(Section) %>% group_by(MGID) %>%  #Arrange df in order by ID and keep highest ranked Section
    slice(1)) 
#
Section_cells <- Section_cells_geo %>% st_set_geometry(NULL) #Remove geometry
#
head(Section_cells) #Check data
#
#Plot Estuary area against Sections to confirm all are represented
summary(Site_Grid_working$MGID %in% Section_cells$MGID) #Check IDs match, FALSE = cells with no designation
tmap_arrange(
  tm_shape(Site_Grid) + tm_polygons(col = "Section"),
  tm_shape(Section_cells_geo) + tm_polygons(col = "Section"),
  nrow = 1, ncol = 2)
#
#Add Section designations to working df
Site_Grid_working_1 <- inner_join(Site_Grid_working %>% dplyr::select(-Site, -Section), #Remove columns being edited from working df
                                  Section_cells %>% dplyr::select(MGID, Site, Section), by = "MGID") #Add data by ID
#
head(Site_Grid_working_1)
#
##Plot to confirm join was correct
tm_shape(Site_Grid_working_1) + tm_polygons(col ="Section") + tm_layout(frame = FALSE)
#
#
#
#
#
#
####1Alt. SKIP CODE SECTION IF NO ALT: Add Additional Area Estuary and Section Information####
#
#Match to which sections overlap into alternate area - use those Sections from "Load Files" section
Section1
Section2
#Section5
#Compile cells in each Section (1 tmp df per section)
tmp1_alt <- Site_Grid_alt[lengths(st_intersects(Site_Grid_alt, Section1)) > 0,] %>% 
  mutate(Site = Site_Code, Section = "N") %>% left_join(Estuary_long, by = "Site")
tmp2_alt <- Site_Grid_alt[lengths(st_intersects(Site_Grid_alt, Section2)) > 0,] %>% 
  mutate(Site = Site_Code, Section = "S") %>% left_join(Estuary_long, by = "Site")
#tmp5_alt <- Site_Grid_alt[lengths(st_intersects(Site_Grid_alt, Section5)) > 0,] %>% 
#  mutate(Site = Site_Code, Section = "B") %>% left_join(Estuary_long, by = "Site")
#
#Combine and remove duplicates based on priority ranking
(Section_cells_geo_alt <-  rbind(tmp1_alt, tmp2_alt) %>% #Join all sections then reorder Section values
    mutate(Section = factor(Section, levels = unique(Section_Order$Section[order(Section_Order$Order)]), ordered = TRUE)) %>%
    arrange(Section) %>% group_by(MGID) %>%  #Arrange df in order by ID and keep highest ranked Section
    slice(1)) 
#
Section_cells_alt <- Section_cells_geo_alt %>% st_set_geometry(NULL) #Remove geometry
#
head(Section_cells_alt) #Check data
#
#Plot Estuary area against Sections to confirm all are represented
summary(Site_Grid_alt_working$MGID %in% Section_cells_alt$MGID) #Check IDs match, FALSE = cells with no designation
tmap_arrange(
  tm_shape(Site_Grid_alt) + tm_polygons(col = "Section"),
  tm_shape(Section_cells_geo_alt) + tm_polygons(col = "Section"),
  nrow = 1, ncol = 2)
#
#Add Section designations to working df
Site_Grid_working_1_alt <- inner_join(Site_Grid_alt_working %>% dplyr::select(-Site, -Section), #Remove columns being edited from working df
                                      Section_cells_alt %>% dplyr::select(MGID, Site, Section), by = "MGID") #Add data by ID
#
head(Site_Grid_working_1_alt)
#
##Plot to confirm join was correct
tm_shape(Site_Grid_working_1_alt) + tm_polygons(col ="Section")+ tm_layout(frame = FALSE)
#
##Name if saving: StateGrid_Site_grid sections
#
#
#
#
####1 Merge - run (-A-) if no Alt, run (-B-) if working with Alt, run (-C-) if updating data layers####
#
#
Site_Grid_All <- Site_Grid_working_1 #(-A-)
Site_Grid_All <- rbind(Site_Grid_working_1, Site_Grid_working_1_alt) #(-B-)
#
##ONLY RUN IF UPDATING DATA LAYERS (-C-)
Site_Grid_All <- Site_Grid
Site_Grid_working_1 <- Site_Grid
#
head(Site_Grid_All)
#
tm_shape(Site_Grid_All) + tm_polygons(col ="Section")+ tm_layout(frame = FALSE)
#Name if saving map: Site_MicroGrid_Sections
#
####2. Edit Oysters in FL data - CANNOT SKIP UNLESS UPDATING DATA LAYERS - SKIP TO #2 MERGE SECTION IF NOT UPDATING OYSTERS DATA LAYER####
#
#
plot(FL_Oysters[4])
st_crs(Site_Grid_working_1) == st_crs(FL_Oysters) #Confirm matching CRS
#
##Subset of grid cells with Oysters
plot(st_join(Site_Grid_working_1, st_as_sf(FL_Oysters))[1])
Grid_Oysters_geo <- Site_Grid[lengths(st_intersects(Site_Grid, st_as_sf(FL_Oysters))) > 0,] %>% 
  mutate(FL_Oysters = "Y") %>% #Add column for presence
  dplyr::select(MGID, FL_Oysters)
#
Grid_Oysters <- Grid_Oysters_geo %>% st_set_geometry(NULL)
#
#
#Plot Estuary area against Oyster presence to confirm areas similar
summary(Site_Grid_working_1$MGID %in% Grid_Oysters$MGID) #Check IDs match TRUE = number with oysters
tmap_arrange(
  tm_shape(Site_Grid_working_1) + tm_polygons(col = "Section"),
  tm_shape(Grid_Oysters_geo) + tm_polygons(),
  nrow = 1, ncol = 2)
#
#Add Section designations to working df
Site_Grid_working_2 <- full_join(Site_Grid_working_1 , Grid_Oysters, by = "MGID") %>% #Add data by ID
  mutate(FL_Oysters = ifelse(is.na(FL_Oysters), "N", "Y")) #Change NA to N
#
head(Site_Grid_working_2)
#
##Plot to confirm join was correct
tm_shape(Site_Grid_working_2) + tm_polygons(col ="FL_Oysters") + tm_layout(frame = FALSE)
#
#
#
#
#
####2Alt. SKIP CODE SECTION IF NO ALT OR UPDATING DATA LAYER:  Edit additional Oysters in FL data####
#
#
plot(FL_Oysters_alt[4])
st_crs(Site_Grid_working_1_alt) == st_crs(FL_Oysters_alt) #Confirm matching CRS
#
##Subset of grid cells with Oysters
plot(st_join(Site_Grid_working_1_alt, st_as_sf(FL_Oysters_alt))[1])
Grid_Oysters_geo_alt <- Site_Grid_alt[lengths(st_intersects(Site_Grid_alt, st_as_sf(FL_Oysters_alt))) > 0,] %>% 
  mutate(FL_Oysters = "Y") %>% #Add column for presence
  dplyr::select(MGID, FL_Oysters)
#
Grid_Oysters_alt <- Grid_Oysters_geo_alt %>% st_set_geometry(NULL)
#
#
#Plot Estuary area against Oyster presence to confirm areas similar
summary(Site_Grid_working_1_alt$MGID %in% Grid_Oysters_alt$MGID) #Check IDs match TRUE = number with oysters
tmap_arrange(
  tm_shape(Site_Grid_alt) + tm_polygons(col = "Section"),
  tm_shape(Grid_Oysters_geo_alt) + tm_polygons(),
  nrow = 1, ncol = 2)
#
#Add Section designations to working df
Site_Grid_working_2_alt <- full_join(Site_Grid_working_1_alt , Grid_Oysters_alt, by = "MGID") %>% #Add data by ID
  mutate(FL_Oysters = ifelse(is.na(FL_Oysters), "N", "Y")) #Change NA to N
#
head(Site_Grid_working_2_alt)
#
##Plot to confirm join was correct
tm_shape(Site_Grid_working_2_alt) + tm_polygons(col ="FL_Oysters") + tm_layout(frame = FALSE)
#
##Name if saving: StateGrid_Site_Oysters
#
#
#
#
####2 Merge - run (-A-) if no Alt or updating oyster layer, run (-B-) if working with Alt, run (-C-) if updating data layers but NOT oyster layer####
#
#
Site_Grid_All <- Site_Grid_working_2 #(-A-)
Site_Grid_All <- rbind(Site_Grid_working_2, Site_Grid_working_2_alt) #(-B-) #If no oysters found in alt grid, change Site_Grid_working_2_alt to #1 and run
#
##ONLY RUN IF UPDATING DATA LAYER BUT NOT UPDATING OYSTER LAYER #(-C-)
Site_Grid_working_2 <- Site_Grid_working_1
#
head(Site_Grid_All)
#
tm_shape(Site_Grid_All) + tm_polygons(col ="FL_Oysters")+ tm_layout(frame = FALSE)
#Name for saving: Site_MicroGrid_Oysters
#
#
####3. Add SHA classifications - CAN SKIP - SKIP TO #3 MERGE SECTION IF NOT UPDATING SHA DATA LAYER####
#
#If not including SHA class in final data, can skip to line 454.
#
plot(SHA_grid[8])
st_crs(Site_Grid_working) == st_crs(SHA_grid) #Confirm matching CRS
#
##Subset of grid cells with SHA classes
Grid_SHA_geo <- st_intersection(Site_Grid_working, st_as_sf(SHA_grid)) %>%
  dplyr::select(MGID, SHA_Class.1, SHA_Name.1) %>%
  rename(SHA_Class = SHA_Class.1, SHA_Name = SHA_Name.1) %>%
  inner_join(SHA_Codes) #Add subsection designations
#
Grid_SHA <- Grid_SHA_geo %>% st_set_geometry(NULL)
#
#
#Plot Estuary area against SHA class to confirm areas similar
summary(Site_Grid_working$MGID %in% Grid_SHA$MGID) #Check IDs match TRUE = number with SHA class
tmap_arrange(
  tm_shape(Site_Grid) + tm_polygons(col = "Section"),
  tm_shape(Grid_SHA_geo) + tm_polygons(col = "SHA_Class"),
  nrow = 1, ncol = 2)
#
#Add Section designations to working df
Site_Grid_working_3 <- full_join(Site_Grid_working_2 %>% dplyr::select(-SHA_Name, -SHA_Class, -Subsection), 
                                 Grid_SHA, by = "MGID") #Add data by ID
#
#Check duplication - remove duplicates favoring open over closed (AP > CA > CR > RE > PD) Run lines389-391 to remove
Site_Grid_working_3 %>% group_by(MGID) %>% filter(n()>1) %>% dplyr::select(MGID, SHA_Class, SHA_Name, Subsection)
#Site_Grid_working_3 <- Site_Grid_working_3 %>% 
#  arrange(Subsection) %>% group_by(MGID) %>%  #Arrange df in order by ID and keep highest ranked Section
#  slice(1)
#
head(Site_Grid_working_3)
#
##Plot to confirm join was correct
tm_shape(Site_Grid_working_3) + tm_polygons(col ="Subsection") + tm_layout(frame = FALSE)
#
#
#
#
#
####3Alt. SKIP CODE SECTION IF NO ALT OR UPDATING DATA LAYER: Add SHA classifications####
#
plot(SHA_grid_alt[8])
st_crs(Site_Grid_alt_working) == st_crs(SHA_grid_alt) #Confirm matching CRS
#

##Subset of grid cells with SHA classes
Grid_SHA_geo_alt <- st_intersection(Site_Grid_alt_working, st_as_sf(SHA_grid_alt)) %>%
  dplyr::select(MGID, SHA_Class.1, SHA_Name.1) %>%
  rename(SHA_Class = SHA_Class.1, SHA_Name = SHA_Name.1) %>%
  inner_join(SHA_Codes) #Add subsection designations
#
Grid_SHA_alt <- Grid_SHA_geo_alt %>% st_set_geometry(NULL)
#
#
#Plot Estuary area against SHA class to confirm areas similar
summary(Site_Grid_alt_working$MGID %in% Grid_SHA_alt$MGID) #Check IDs match TRUE = number with SHA class
tmap_arrange(
  tm_shape(Site_Grid_alt) + tm_polygons(col = "Section"),
  tm_shape(Grid_SHA_geo_alt) + tm_polygons(col = "SHA_Class"),
  nrow = 1, ncol = 2)
#
#Add Section designations to working df
Site_Grid_working_3_alt <- full_join(Site_Grid_working_2_alt %>% dplyr::select(-SHA_Name, -SHA_Class, -Subsection), 
                                     Grid_SHA_alt, by = "MGID") #Add data by ID
#
#Check duplication - remove duplicates favoring open over closed (AP > CA > CR > RE > PD)
Site_Grid_working_3_alt %>% group_by(MGID) %>% filter(n()>1) %>% dplyr::select(MGID, SHA_Class, SHA_Name, Subsection)
Site_Grid_working_3_alt <- Site_Grid_working_3_alt %>% 
  arrange(Subsection) %>% group_by(MGID) %>%  #Arrange df in order by ID and keep highest ranked Section
  slice(1)
#
head(Site_Grid_working_3_alt)
#
##Plot to confirm join was correct
tm_shape(Site_Grid_working_3_alt) + tm_polygons(col ="Subsection") + tm_layout(frame = FALSE)
#
#Name if saving:StateGrid_Site_SHA
#
#
#
####3 Merge - Only run (--) if skipping SHA class. Run (-A-) if no Alt or updating SHA layer, run (-B-) if working with Alt, run (-C-) if updating data layer but NOT SHA layer####
#
#
#Site_Grid_All <- Site_Grid_working_2 #(--)
Site_Grid_All <- Site_Grid_working_3 #(-A-)
Site_Grid_All <- rbind(Site_Grid_working_3, Site_Grid_working_2_alt) #(-B-) #If no SHA found in Alt grid change Site_Grid_working_3_alt to #2
#
##ONLY RUN IF UPDATING DATA LAYER BUT NOT UPDATING SHA LAYER #(-C-)
Site_Grid_working_3 <- Site_Grid_working_2
#
head(Site_Grid_All)
#
tm_shape(Site_Grid_All) + tm_polygons(col ="Subsection")+ tm_layout(frame = FALSE)
#Name for saving: Site_MicroGrid_SHA
#
#
####4. Add depth data - CAN SKIP - SKIP TO #4 MERGE SECTION IF NOT UPDATING DEPTH DATA LAYER####
#
#If not including depth in final data, can skip to #4 Merge section.
#
plot(Depth)
st_crs(Site_Grid_working) == st_crs(Depth) #Confirm matching CRS
#
##Subset of grid cells with depth data - if not working due to Edge degeneration contact EL (same for other sections)
Grid_Depth_geo <- st_intersection(Site_Grid_working, st_as_sf(Depth)) %>%
  dplyr::select(MGID, Depth) %>% group_by(MGID) %>%
  summarize(Depth = mean(Depth, na.rm = T)) %>% 
  st_make_valid()
#
Grid_Depth <- Grid_Depth_geo %>% st_set_geometry(NULL)
#
#Plot Estuary area against Oyster presence to confirm areas similar
summary(Site_Grid_working_1$MGID %in% Grid_Depth$MGID) #Check IDs match TRUE = number of cells with depth measurement
tmap_options(check.and.fix = TRUE)
tmap_arrange(
  tm_shape(Site_Grid_working_1) + tm_polygons(col = "Section"),
  tm_shape(Grid_Depth_geo) + tm_polygons(col = "Depth", palette = c("BrBG")),
  nrow = 1, ncol = 2)
#
#Add designations to working df - skipped SHA so using wporking_2 instead of working_3
Site_Grid_working_4 <- full_join(Site_Grid_working_2 , Grid_Depth, by = "MGID") %>% #Add data by ID
  dplyr::select(-Bathy_m) %>% #Drop original Bathy column
  drop_na(Lat_DD_Y)
#
head(Site_Grid_working_4)
#
##Plot to confirm join was correct
tm_shape(Site_Grid_working_4) + tm_polygons(showNA = TRUE, col ="Depth") + tm_layout(frame = FALSE)
#
#
#
#
#
####4Alt. SKIP CODE SECTION IF NO ALT OR UPDATING DATA LAYER: Add depth data####
#
#
plot(Depth_alt)
st_crs(Site_Grid_alt_working) == st_crs(Depth_alt) #Confirm matching CRS
#
##Subset of grid cells with depth data
Grid_Depth_geo_alt <- st_intersection(Site_Grid_alt_working, st_as_sf(Depth_alt)) %>%
  dplyr::select(MGID, Depth) %>% group_by(MGID) %>%
  summarize(Depth = mean(Depth, na.rm = T)) %>% 
  st_make_valid()
#
Grid_Depth_alt <- Grid_Depth_geo_alt %>% st_set_geometry(NULL)
#
#Plot Estuary area against Oyster presence to confirm areas similar
summary(Site_Grid_working_1_alt$MGID %in% Grid_Depth_alt$MGID) #Check IDs match TRUE = number of cells with depth measurement
tmap_arrange(
  tm_shape(Site_Grid_working_1_alt) + tm_polygons(col = "Section"),
  tm_shape(Grid_Depth_geo_alt) + tm_polygons(col = "Depth", palette = c("BrBG")),
  nrow = 1, ncol = 2)
#
#Add designations to working df  - skipped SHA so using wporking_2 instead of working_3
Site_Grid_working_4_alt <- full_join(Site_Grid_working_2_alt , Grid_Depth_alt, by = "MGID") %>% #Add data by ID
  dplyr::select(-Bathy_m) #Drop original Bathy column
#
head(Site_Grid_working_4_alt)
#
##Plot to confirm join was correct
tm_shape(Site_Grid_working_4_alt) + tm_polygons(showNA = TRUE, col ="Depth") + tm_layout(frame = FALSE)
#
#
#
#
#
####4 Merge - Only run (--) if skipping depth. Run (-A-) if no Alt or updating depth layer, run (-B-) if working with Alt, run (-C-) if updating data layer but NOT depth layer####
#
#Site_Grid_All <- Site_Grid_working_3 (--)
Site_Grid_All <- Site_Grid_working_4 #(-A-)
Site_Grid_All <- rbind(Site_Grid_working_4, Site_Grid_working_4_alt) #(-B-)
#
##ONLY RUN IF UPDATING DATA LAYER BUT NOT UPDATING DEPTH LAYER #(-C-)
Site_Grid_working_4 <- Site_Grid_working_3
#
head(Site_Grid_All)
#
tm_shape(Site_Grid_All) + tm_polygons(col ="Depth")+ tm_layout(frame = FALSE)
#Name for saving: Site_MicroGrid_Depth
#
#
####5. Add Seagrass areas - CAN SKIP - SKIP TO #5 MERGE SECTION IF NOT UPDATING DEPTH DATA LAYER####
#
#If not including seagrass in final data, can skip to #5 Merge section.
#
plot(Seagrass[5])
st_crs(Site_Grid_working) == st_crs(Seagrass) #Confirm matching CRS
#
##Subset of grid cells with Seagrass classifications - may take a while to run
Grid_Seagrass_geo <- rbind(
  #Continuous seagrass
  Site_Grid[lengths(st_intersects(Site_Grid_working, st_as_sf(Seagrass %>% filter(SEAGRASS == "Continuous")))) > 0,] %>% 
    mutate(Seagrass = "Continuous") %>% dplyr::select(MGID, Seagrass),
  #Discontinuous seagrass
  Site_Grid[lengths(st_intersects(Site_Grid_working, st_as_sf(Seagrass %>% filter(SEAGRASS == "Discontinuous")))) > 0,] %>% 
    mutate(Seagrass = "Discontinuous") %>% dplyr::select(MGID, Seagrass)
) 
#
Grid_Seagrass <- Grid_Seagrass_geo %>% st_set_geometry(NULL)
#
#
#Plot Estuary area against SHA class to confirm areas similar
summary(Site_Grid_working$MGID %in% Grid_Seagrass$MGID) #Check IDs match TRUE = number with SHA class
tmap_arrange(
  tm_shape(Site_Grid) + tm_polygons(col = "Section"),
  tm_shape(Grid_Seagrass_geo) + tm_polygons(col = "Seagrass"),
  nrow = 1, ncol = 2)
#
#Add Section designations to working df
Site_Grid_working_5 <- full_join(Site_Grid_working_4, Grid_Seagrass, by = "MGID") %>% #Add data by ID
  mutate(Seagrass = ifelse(is.na(Seagrass), NA, Seagrass))
#
#Check duplication
Site_Grid_working_5 %>% group_by(MGID) %>% filter(n()>1) %>% dplyr::select(MGID, Seagrass)
Site_Grid_working_5 <- Site_Grid_working_5 %>% 
  arrange(Seagrass) %>% group_by(MGID) %>%  #Arrange df in order by ID and keep highest ranked Section
  slice(1)
#
head(Site_Grid_working_5)
#
##Plot to confirm join was correct
tm_shape(Site_Grid_working_5) + tm_polygons(col ="Seagrass") + tm_layout(frame = FALSE)
#
#
#
#
#
####5Alt. SKIP CODE SECTION IF NO ALT OR UPDATING DATA LAYER: Add Seagrass areas####
#
plot(Seagrass_alt[5])
st_crs(Site_Grid_alt_working) == st_crs(Seagrass_alt) #Confirm matching CRS
#
##Subset of grid cells with Seagrass classifications - may take a while to run
Grid_Seagrass_geo_alt <- rbind(
  #Continuous seagrass
  Site_Grid_alt[lengths(st_intersects(Site_Grid_alt_working, st_as_sf(Seagrass_alt %>% filter(SEAGRASS == "Continuous")))) > 0,] %>% 
    mutate(Seagrass = "Continuous") %>% dplyr::select(MGID, Seagrass),
  #Discontinuous seagrass
  Site_Grid_alt[lengths(st_intersects(Site_Grid_alt_working, st_as_sf(Seagrass_alt %>% filter(SEAGRASS == "Discontinuous")))) > 0,] %>% 
    mutate(Seagrass = "Discontinuous") %>% dplyr::select(MGID, Seagrass)
)
#
Grid_Seagrass_alt <- Grid_Seagrass_geo_alt %>% st_set_geometry(NULL)
#
#
#Plot Estuary area against SHA class to confirm areas similar
summary(Site_Grid_alt_working$MGID %in% Grid_Seagrass_alt$MGID) #Check IDs match TRUE = number with SHA class
tmap_arrange(
  tm_shape(Site_Grid_alt) + tm_polygons(col = "Section"),
  tm_shape(Grid_Seagrass_geo_alt) + tm_polygons(col = "Seagrass"),
  nrow = 1, ncol = 2)
#
#Add Section designations to working df
Site_Grid_working_5_alt <- full_join(Site_Grid_working_4_alt, 
                                     Grid_Seagrass_alt, by = "MGID") #Add data by ID
#
#Check duplication - remove duplicates 
Site_Grid_working_5_alt %>% group_by(MGID) %>% filter(n()>1) %>% dplyr::select(MGID, Seagrass)
#
Site_Grid_working_5_alt <- Site_Grid_working_5_alt %>% 
  arrange(Seagrass) %>% group_by(MGID) %>%  #Arrange df in order by ID and keep highest ranked Section
  slice(1)
#
head(Site_Grid_working_5_alt)
#
##Plot to confirm join was correct
tm_shape(Site_Grid_working_5_alt) + tm_polygons(col ="Seagrass") + tm_layout(frame = FALSE)
#
#StateGrid_Site_Seagrass
#
#
#
####5 Merge - Only run (--) if skipping seagrass. Run (-A-) if no Alt or updating seagrass layer, run (-B-) if working with Alt, run (-C-) if updating data layer but NOT seagrass layer ####
#
#Site_Grid_All <- Site_Grid_working_4 #(--)
Site_Grid_All <- Site_Grid_working_5 #(-A-)
Site_Grid_All <- rbind(Site_Grid_working_5, Site_Grid_working_5_alt) #(-B-)
#
##ONLY RUN IF UPDATING DATA LAYER BUT NOT UPDATING SEAGRASS LAYER #(-C-)
Site_Grid_working_5 <- Site_Grid_working_4
#
head(Site_Grid_All)
#
tm_shape(Site_Grid_All) + tm_polygons(col ="Seagrass")+ tm_layout(frame = FALSE)
#Name for saving: LX_MicroGrid_Seagrass
#
#
####Output data for use in selecting stations####
#
head(Site_Grid_All)
#
##Write shapefile of Estuary and data
st_write(Site_Grid_All, 
         paste0("Maps/Shapefiles/", Site_Code,"_compiled_", Sys.Date(), ".shp"),
         overwrite = TRUE)
#
#
##Save data frame as CSV - need to drop geometry 
Site_grid_output_data <- as.data.frame(Site_Grid_All) %>% dplyr::select(-geometry)
head(Site_grid_output_data)
#
write.csv(Site_grid_output_data, paste0("Output_Data/", Site_Code, "_MicrogridData_", Sys.Date(), ".csv"), 
          row.names = FALSE)
#
#
#
#