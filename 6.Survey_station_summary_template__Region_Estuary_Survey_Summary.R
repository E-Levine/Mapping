####Template for summarizing completed oyster surveys by grid cells and polygons
####Save file name using "Region ... Summary" - change 'Region' and 'Estuary' to correct location
####Update the Region, Estuary/Site Code, and StateGrids in lines 26-29 (lines 21-24 after removing template header). 
####Delete lines 1-4 when saving as new working file.
#
#Survey station summary
#For summarizing completed oyster surveys by grid cells and polygons 
#
#Use Alt+O to collapse all sections, Alt+Shift+O to expand all sections
#Each section builds upon the previous to add data to the microgrids:
#
#
#.rs.restartR() #Restarts session (good if rerunning after working with other files)
#graphics.off()  # turns off any plots from previous work session
rm(list=ls(all=TRUE)) # clears out environment 
#
#Load require packages (will install packages as necessary)
if (!require("pacman")) install.packages("pacman")
pacman::p_load(plyr, tidyverse, #Df manipulation, basic summary
               sf, raster, spData, rgeos, rgdal,
               ggmap, ggsn, leaflet, tmap, ggpubr, 
               htmlwidgets, openxlsx,
               install = TRUE) #Mapping and figures
#
#Assign Region, Estuary Code, and StateGrid(s). Only assign the Alternate state grid in line 24 if required.
Region <- c("SouthEast")
Site_Code <- c("SL")
State_Grid <- c("H4")
#Alt_State_Grid <- c("H5") 
#
#Date of grid updates (output data date from file name)
Date <- c("2023-07-20") #Format: YYY-MM-DD
#
##WQ data range for HSM - start and end year for WQ data compilation (refer to file name for dates)
Start_year <- c("2012")
End_year <- c("2022")
#
#Start and end dates for time period of summary - replace both with large boundary dates if all data is desired
Start_date <- as.Date("2020-01-01", "%Y-%m-%d")
End_date <- as.Date("2030-12-31", "%Y-%m-%d")#
#
#
####Load files####
#
#Microgrid - skip line 43 if no Alt. 
MicroGrid <- st_read(paste0("../Base Layers/MicroGrids/Florida_MicroGrid_WGS84_",State_Grid,"_clip.shp"))
Alt_MicroGrid <- st_read(paste0("../Base Layers/MicroGrids/Florida_MicroGrid_WGS84_",Alt_State_Grid,"_clip.shp"))
#
Estuary_area <- st_read(paste0("../Base Layers/Site_Region_Areas/", Site_Code, ".kml"))
#Run line 47 if no Alt or lines 48-49 if working with Alt 
Site_Grid <- MicroGrid[lengths(st_intersects(MicroGrid, Estuary_area))> 0,]  %>% dplyr::select(-Estuary)
Site_Grid <- rbind(MicroGrid[lengths(st_intersects(MicroGrid, Estuary_area))> 0,]  %>% dplyr::select(-Estuary),
                   Alt_MicroGrid[lengths(st_intersects(Alt_MicroGrid, Estuary_area))> 0,]  %>% dplyr::select(-Estuary))
#
##Microgrid Estuary Data - load and merge
Site_df <- read.csv(paste0("Output_data/", Site_Code, "_MicrogridData_", Date, ".csv"), 
                    na.string = c("Z", "", "NA"))
head(Site_df)
#HSM data
HSM_data <-  read.csv(paste0("Output_data/", Site_Code, "_MicrogridData_HSM_", Start_year,"_", End_year, ".csv"), 
                      na.string = c("Z", "", "NA"))
head(HSM_data)
#Merge
Site_data <- left_join(Site_df, HSM_data %>% dplyr::select(MGID, Salinity_HSI:HSM_Score))
head(Site_data)
#
#
#Microgrid 
FL_outline <- st_read(paste0("../Base Layers/FL_Outlines/FL_Outlines.shp"))
qtm(FL_outline)
#
#
#
#Monitoring stations - for plotting fixed stations if desired - can skip if not plotting fixed stations in final maps
Monitoring <- read.csv("../Reference Files/Current_Monitoring_Stations_2023.csv", na.string = c("Z", "", "NA"))
head(Monitoring)
Monitor_spdf <- SpatialPointsDataFrame(Monitoring[,3:4], Monitoring)
crs(Monitor_spdf) <- "+proj=longlat +datum=WGS84 +no_defs +type=crs"
#
Monitoring <- "NA" #Run if not including Monitoring stations
#
#Completed survey stations with data 
Comp_Stations <- read.csv(paste0("../Reference Files/Station data-Mapping/Survey_station_data.csv"), na.string = c("Z", "", "NA")) %>%
  mutate(Date = as.Date(Date, "%m/%d/%Y")) %>%
  subset(Site == Site_Code & Date > Start_date & Date < End_date) %>% rename(MGID = Confirmed_MGID)
head(Comp_Stations)
#
#
##Oysters in Florida - polygons for comparison by polygon area rather than grid cell
FL_Oysters <- crop(as(st_read("../Base Layers/Oyster Beds in Florida/Oyster_Beds_in_Florida.shp"), "Spatial"),
                   extent(Site_Grid))
#
#Check data
plot(FL_Oysters)
head(FL_Oysters)
st_crs(FL_Oysters)
#
#
#
#
####Mapping options####
#
#Show oyster layer presence? Y/N
Oyster_layer <- c("Y")
#Show depth? Y/N
Depth_layer <- c("N")
#Interactive or static map. Static map does not include station IDs, interactive map does. Inter/Static
Map_type <- c("Inter")
#Map at the Site/Estuary or Section level? Site/Section
Map_area <- c("Site")
#Include text for station ID on Section-level static maps? Can make the map busy/cluttered looking. Y/N
Section_Static_IDs <- c("N")
#
#
####Oyster presence by grid cell - mapping####
#
#Final combined data frame
All_data <- left_join((Site_Grid %>% dplyr::select(-Site, -Section, -SHA_Name, -SHA_Class, -Subsection, -Bathy_m)),
                      Site_data %>% dplyr::select(MGID, Site:HSM_Score)) %>%
  mutate(Seagrass = ifelse(is.na(Seagrass), "Unk", Seagrass)) %>%
  left_join(Comp_Stations %>% dplyr::select(Site, Section, Date, MGID:Longitude, FixedLocationID:Dead_Count) %>% mutate(Oysters = as.factor(Oysters)))
#
head(All_data)
#
##Map of either Site or Sections, either static or interactive - based on selections in "Mapping Options" (starting line 97)
if(Map_area == "Site"){
  if(Map_type == "Static") {
    leaflet_map <- tm_shape(name = "Microgrid cells", All_data) + 
      tm_borders(col = "gray") + #Cell borders
      #Add oyster layer 
      {if(Oyster_layer == "Y") tm_shape(name = "Oyster layer presence", All_data %>% subset(FL_Oysters == "Y") %>% 
                                          mutate(FL_Oysters = ifelse(FL_Oysters == "Y", "Oyster layer", FL_Oysters)))+  #Change text for legend
          tm_polygons("FL_Oysters", title = "", palette = c("viridis"), alpha = 0.4)}+  #Change text for legend
      #Add depth
      {if(Depth_layer == "Y") tm_shape(name = "Depth", All_data %>% filter(!is.na(Depth))) + tm_polygons("Depth", title = "", palette = c("YlGnBu"), alpha = 0.5)} +
      #Add stations surveyed
      tm_shape(name = "Surveyed stations", All_data %>% subset(!is.na(Oysters)))+  
      tm_polygons("Oysters", title = "Oyster Reef", palette = c("YlOrRd"), alpha = 0.8)+
      #Add FL shoreline
      tm_shape(name = "Shoreline", st_make_valid(FL_outline)) + tm_polygons() +
      #Add monitoring stations
      {if(Monitoring != "NA") tm_shape(name = "Monitoring stations", Monitor_spdf) +  tm_symbols(shape = 16, size = 0.2, col = "black", border.col = "black", alpha = 0.4)}+
      {if(Monitoring != "NA") tm_add_legend('fill', col = "black", border.col = "black", labels = c("Monitoring Stations"))}+
      tm_layout(main.title = paste0(Site_Code, " Survey"), main.title.position = "center")+
      tm_view(symbol.size.fixed = FALSE)
    #
    (Site_map <- leaflet_map)
    #
    tmap_save(Site_map, file = paste0("Maps/Survey/", Site_Code, "/Completed/", Site_Code, "_survey_summary.jpg"), dpi = 1000)
    #
  } else if(Map_type == "Inter"){
    leaflet_map <- tm_shape(name = "Microgrid cells", All_data) + 
      tm_borders(col = "gray") + #Cell borders
      #Add oyster layer 
      {if(Oyster_layer == "Y") tm_shape(name = "Oyster layer presence", All_data %>% subset(FL_Oysters == "Y") %>% 
                                          mutate(FL_Oysters = ifelse(FL_Oysters == "Y", "Oyster layer", FL_Oysters)))+  #Change text for legend
          tm_polygons("FL_Oysters", title = "", palette = c("viridis"), alpha = 0.4)}+  #Change text for legend
      #Add depth
      {if(Depth_layer == "Y") tm_shape(name = "Depth", All_data %>% filter(!is.na(Depth))) + tm_polygons("Depth", title = "", palette = c("YlGnBu"), alpha = 0.5)} +
      #Add stations surveyed
      tm_shape(name = "Surveyed stations", All_data %>% subset(!is.na(Oysters)))+  
      tm_polygons("Oysters", title = "Oyster Reef", palette = c("YlOrRd"), alpha = 0.8)+
      #Add FL shoreline
      tm_shape(name = "Shoreline", st_make_valid(FL_outline)) + tm_polygons() +
      #Add cell Station numbers
      tm_shape(name = "Location ID", All_data) + tm_text("FixedLocationID", size = "AREA")+ 
      #Add monitoring stations
      {if(Monitoring != "NA") tm_shape(name = "Monitoring stations", Monitor_spdf) +  tm_symbols(shape = 16, size = 0.2, col = "black", border.col = "black", alpha = 0.4)}+
      {if(Monitoring != "NA") tm_add_legend('fill', col = "black", border.col = "black", labels = c("Monitoring Stations"))}+
      tm_layout(main.title = paste0(Site_Code, " Survey"), main.title.position = "center")+
      tm_view(symbol.size.fixed = FALSE)
    #
    (Site_map <- tmap_leaflet(leaflet_map))
    #
    saveWidget(Site_map, paste0("Maps/Survey/", Site_Code, "/Completed/Interactive maps/", Site_Code,"_survey_summary_widget.html"))
    #
  }
} else if(Map_area == "Section"){
  if(Map_type == "Static") { 
    for(i in unique(Comp_Stations$Section)){
      leaflet_map <- tm_shape(name = "Microgrid cells", All_data %>% filter(Section == i)) + 
        tm_borders(col = "gray") + #Cell borders
        #Add oyster layer if used for selection
        {if(Oyster_layer == "Y") tm_shape(name = "Oyster layer presence", All_data %>% subset(FL_Oysters == "Y" & Section == i) %>% 
                                            mutate(FL_Oysters = ifelse(FL_Oysters == "Y", "Oyster layer", FL_Oysters)))+  #Change text for legend
            tm_polygons("FL_Oysters", title = "", palette = c("viridis"), alpha = 0.4)} +
        #Add depth
        {if(Depth_layer == "Y") tm_shape(name = "Depth", All_data %>% filter(!is.na(Depth))) + tm_polygons("Depth", title = "", palette = c("YlGnBu"), alpha = 0.5)} +
        #Add stations
        tm_shape(name = "Surveyed stations", All_data %>% subset(!is.na(Oysters) & Section == i))+  
        tm_polygons("Oysters", title = "Oyster Reef", palette = c("YlOrRd"), alpha = 0.8) + 
        #Add FL shoreline
        tm_shape(name = "Shoreline", st_make_valid(FL_outline)) + tm_polygons() +
        #Add cell Station numbers
        {if(Section_Static_IDs == "Y") tm_shape(name = "Location ID", All_data %>% subset(Section == i)) + tm_text("FixedLocationID", size = "AREA", auto.placement = TRUE)}+ 
        #Add monitoring stations
        {if(Monitoring != "NA") tm_shape(name = "Monitoring stations", Monitor_spdf) +  tm_symbols(shape = 16, size = 0.75, col = "black", border.col = "black", alpha = 0.4)}+
        {if(Monitoring != "NA") tm_add_legend('fill', col = "black", border.col = "black", labels = c("Monitoring Stations"))}+
        tm_layout(main.title = paste0(Site_Code, " ", i, " Survey"), main.title.position = "center")+
        tm_view(symbol.size.fixed = FALSE)
      #
      tmap_save(leaflet_map, file = paste0("Maps/Survey/", Site_Code, "/Completed/", Site_Code, "_", i, "_survey_summary.jpg"),
                dpi = 1000)
      #
    }
  } else if(Map_type == "Inter"){
    for(i in unique(Comp_Stations$Section)){
      leaflet_map <- tm_shape(name = "Microgrid cells", All_data %>% filter(Section == i)) + 
        tm_borders(col = "gray") + #Cell borders
        #Add oyster layer 
        {if(Oyster_layer == "Y") tm_shape(name = "Oyster layer presence", All_data %>% subset(FL_Oysters == "Y" & Section == i) %>% 
                                            mutate(FL_Oysters = ifelse(FL_Oysters == "Y", "Oyster layer", FL_Oysters)))+  #Change text for legend
            tm_polygons("FL_Oysters", title = "", palette = c("viridis"), alpha = 0.4)}+  #Change text for legend
        #Add depth
        {if(Depth_layer == "Y") tm_shape(name = "Depth", All_data %>% filter(!is.na(Depth))) + tm_polygons("Depth", title = "", palette = c("YlGnBu"), alpha = 0.5)} +
        #Add stations surveyed
        tm_shape(name = "Surveyed stations", All_data %>% subset(!is.na(Oysters) & Section == i))+  
        tm_polygons("Oysters", title = "Oyster Reef", palette = c("YlOrRd"), alpha = 0.8)+
        #Add FL shoreline
        tm_shape(name = "Shoreline", st_make_valid(FL_outline)) + tm_polygons() +
        #Add cell Station numbers
        tm_shape(name = "Location ID", All_data %>% filter(Section == i)) + tm_text("FixedLocationID", size = "AREA")+ 
        #Add monitoring stations
        {if(Monitoring != "NA") tm_shape(name = "Monitoring stations", Monitor_spdf) +  tm_symbols(shape = 16, size = 0.2, col = "black", border.col = "black", alpha = 0.4)}+
        {if(Monitoring != "NA") tm_add_legend('fill', col = "black", border.col = "black", labels = c("Monitoring Stations"))}+
        tm_layout(main.title = paste0(Site_Code, " Survey"), main.title.position = "center")+
        tm_view(symbol.size.fixed = FALSE)
      #
      (Site_map <- tmap_leaflet(leaflet_map))
      #
      saveWidget(Site_map, paste0("Maps/Survey/", Site_Code, "/Completed/Interactive maps/", Site_Code,"_", i,"_survey_summary_widget.html"))
    }
    #
  }
}
#
#
#
####Oyster presence by grid cell - summary####
#
#Summary of how many Yes or No oysters out of all oyster presence cells
(Grid_summ <- cbind(
  cbind(
    All_data %>% subset(FL_Oysters == "Y" & Oysters == "Yes") %>% as.data.frame() %>% summarise(Yes = n()), #Yes/Total in shapefile layer
    All_data %>% subset(FL_Oysters == "Y" & Oysters == "No") %>% as.data.frame() %>% summarise(No = n()), #No/Total in shapefile layer
    All_data %>% subset(FL_Oysters == "Y") %>% as.data.frame() %>% summarise(Total = n())) %>%
    mutate(Remains = Total - Yes - No,
           Pct_Yes = round(Yes/Total*100, 2),
           Pct_No = round(No/Total*100, 2)),
  All_data %>% subset(FL_Oysters != "Y" & Oysters == "Yes") %>% as.data.frame() %>% summarise(Extra_Yes = n()), #All possible "Yes"
  All_data %>% subset(FL_Oysters != "Y" & Oysters == "No") %>% as.data.frame() %>% summarise(Extra_No = n()))) #All possible "No"
#
#
#
#
####Oyster presence by polygon - mapping####
#
#Convert Comp_Stations to spdf
Comp_Stations_sf <- SpatialPointsDataFrame(Comp_Stations[,c(7, 6)], Comp_Stations %>% rename(Oyster_S = Oysters))
crs(Comp_Stations_sf) <- "+proj=longlat +datum=WGS84 +no_defs +type=crs"
plot(Comp_Stations_sf)
#
#Determine number of polygons intersected by at least 1 point
Checked_polys <- over(Comp_Stations_sf, FL_Oysters)
#Identify which polygons are intersected 
intersected <- unique(Checked_polys) %>% mutate(Checked = "Yes") %>% drop_na(OBJECTID)
#Merge data and mark polygons not intersected as "No" for checked
t <- merge(FL_Oysters, intersected) 
t@data <- t@data %>% mutate(Checked = ifelse(is.na(Checked), "No", "Yes"))
#
#Assign polygon data to points for mapping coloration
t2 <- st_intersection(st_as_sf(Comp_Stations_sf), st_as_sf(t)) 
t3 <-  merge(Comp_Stations_sf, t2)  #Add polygon info to stations.
t3@data <- t3@data %>% 
  mutate(Type = ifelse(is.na(OBJECTID) & Oyster_S == "Yes", "Present/Out", 
                       ifelse(!is.na(OBJECTID) & Oyster_S == "Yes", "Present/In", 
                              ifelse(!is.na(OBJECTID) & Oyster_S == "No", "Absent/In", "Absent/Out")))) %>%
  mutate(Type = factor(Type, levels = c("Present/In", "Present/Out", "Absent/In", "Absent/Out")))
#
# 
#
#Map of polygons checked and station locations
(Polygon_checks <- tm_shape(t)+
    tm_polygons(col = "Checked")+
    tm_shape(t3)+
    tm_symbols(shape = 16, size = 1, col = "Type", palette = c("RdYlGn"), border.col = "black")+
    tm_shape(st_make_valid(FL_outline)) + tm_polygons() +  
    tm_layout(main.title = paste0(Site_Code, " Survey v. Polygon Comparison"), main.title.position = "center")+
    tm_view(symbol.size.fixed = FALSE))
#
tmap_save(Polygon_checks, file = paste0("Maps/Survey/", Site_Code, "/Completed/", Site_Code, "_polygon_summary.jpg"), dpi = 1000)
#
#
#
####Oyster presence by polygon - summary####
#
#Summary of how many Yes or No oysters out of all oyster presence cells
(Poly_summ <- cbind(
  cbind(
    t3@data %>% subset(Type == "Present/In") %>% summarise(Pres_In = n()), #Yes/Total in shapefile layer
    t3@data %>% subset(Type == "Absent/In") %>% summarise(Abs_In = n()), #No/Total in shapefile layer
    t@data %>% summarise(Total = n())) %>%  #Total number of polygons) %>%
    mutate(Remains = Total - Pres_In - Abs_In,
           Pct_Pres_In = round(Pres_In/Total*100, 2),
           Pct_Abs_In = round(Abs_In/Total*100, 2)),
  t3@data %>% subset(Type == "Present/Out") %>% summarise(Pres_Out = n()), #All possible "Yes"
  t3@data %>% subset(Type == "Absent/Out") %>% summarise(Abs_Out = n())) %>% #All possible "No"
   mutate(Pct_Pres_Out = round(Pres_Out/Total*100, 2),
          Pct_Abs_Out = round(Abs_Out/Total*100, 2)))
#
#
#