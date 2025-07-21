####Template for Estuary MicroGrid Station Mapping (Mapping of completed survey stations)
####Update the Region, Estuary/Site Code, and StateGrids in lines 25-28 (lines 21-24 after removing template header). 
####Delete lines 1-3 when saving as new working file.
#
###Randomized Survey Station Mapping
#Requires Completed survey station information, ouput from code #3.
#
#Use Alt+O to collapse all sections, Alt+Shift+O to expand all sections
#Each section builds upon the previous to add data to the microgrids:
#Data order: Estuary and Sections, Oysters in FL, SHA classifications, Depth, Seagrass area
#
#
#.rs.restartR() #Restarts session (good if rerunning after working with other files)
#graphics.off()  # turns off any plots from previous work session
rm(list=ls(all=TRUE)) # clears out environment 
#
#Load require packages (will install packages as necessary) - MAKE SURE PACMAN IS INSTALLED AND RUNNING!
if (!require("pacman")) {install.packages("pacman")}
pacman::p_load(plyr, tidyverse, #Df manipulation, basic summary
               sf, raster, terra,
               leaflet, tmap,  
               htmlwidgets, openxlsx,
               install = TRUE) #Mapping and figures
#
#Assign Region, Estuary Code, and StateGrid(s). Only assign the Alternate state grid in line 24 if required.
Region <- c("SouthWest")
Site_Code <- c("CR")
State_Grid <- c("F5")
#Alt_State_Grid <- c("G5") 
#
#Date of grid updates (output data date from file name)
Date <- c("2025-05-08") #Format: YYY-MM-DD
#
##WQ data range for HSM - start and end year for WQ data compilation (refer to file name for dates)
Start_year <- c("2012")
End_year <- c("2022")
#
##Station data range - start and end month and year for station data mapping
Start_date <- c("2022-01-01") #Format: YYY-MM-DD
End_date <- c("2023-12-31") #Format: YYY-MM-DD
#
#
#
####Load files####
#
#Microgrid - skip line 44 if no Alt. 
MicroGrid <- st_read(paste0("../Base Layers/MicroGrids/Florida_MicroGrid_WGS84_",State_Grid,"_clip.shp"))
Alt_MicroGrid <- st_read(paste0("../Base Layers/MicroGrids/Florida_MicroGrid_WGS84_",Alt_State_Grid,"_clip.shp"))
#
Estuary_area <- st_read(paste0("../Base Layers/Site_Region_Areas/", Site_Code, ".kml"))
#Run line 48 if no Alt or lines 49-50 if working with Alt 
Site_Grid <- MicroGrid[lengths(st_intersects(MicroGrid, Estuary_area))> 0,]  %>% dplyr::select(-Estuary)
Site_Grid <- rbind(MicroGrid[lengths(st_intersects(MicroGrid, Estuary_area))> 0,]  %>% dplyr::select(-Estuary),
                   Alt_MicroGrid[lengths(st_intersects(Alt_MicroGrid, Estuary_area))> 0,]  %>% dplyr::select(-Estuary))
#
##Microgrid Estuary Data - load and merge
Site_df <- read.csv(paste0("Output_data/", Site_Code, "_MicrogridData_", Date, ".csv"), 
                    na.string = c("Z", "", "NA"))
head(Site_df)
#HSM data - run first line to include data, run second line to map without HSM data
HSM_data <-  read.csv(paste0("Output_data/", Site_Code, "_MicrogridData_HSM_", Start_year,"_", End_year, ".csv"), 
                      na.string = c("Z", "", "NA"))
HSM_data <- NA
head(HSM_data)
#Merge
if(!is.na(HSM_data)){
  Site_data <- left_join(Site_df, HSM_data %>% dplyr::select(MGID, Salinity_HSI:HSM_Score))
} else {
  Site_data <- Site_df
}
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
Monitoring <- read.csv("../Reference Files/Current_Monitoring_Stations_2023.csv", na.string = c("Z", "", "NA"))  %>% drop_na(DecLong, DecLat)
head(Monitoring)
Monitor_spdf <- SpatialPointsDataFrame(Monitoring[,3:4], Monitoring)
crs(Monitor_spdf) <- "+proj=longlat +datum=WGS84 +no_defs +type=crs"
#
Monitoring <- "NA" #Run if not including Monitoring stations
#
#Completed survey stations 
Comp_Stations <- read.csv(paste0("Output_Data/", Site_Code, "_Station_MGIDs.csv"), na.string = c("Z", "", "NA")) %>%
  rename("MGID" = Confirmed_MGID, "Lat_DD_Y" = Latitude, "Long_DD_X" = Longitude)
head(Comp_Stations)
#
#
#
####Station selection and parameter specification setup####
#
#Final combined data frame
All_data <- left_join((Site_Grid %>% dplyr::select(-Site, -Section, -SHA_Name, -SHA_Class, -Subsection, -Bathy_m)),
                      Site_data %>% dplyr::select(MGID, County:last_col())) %>%
  mutate(Seagrass = ifelse(is.na(Seagrass), "Unk", Seagrass)) %>%
  left_join(Comp_Stations %>% dplyr::select(MGID, Oysters, FixedLocationID) %>% mutate(Oysters = as.factor(Oysters)))
#
head(All_data)
#
#Following line is used to determine which stations have already been surveyed. Can skip if not including any data within "Comp_Stations":
Stations_surveyed <- All_data %>% subset(!is.na(Oysters))
#
#
###Mapping filters: Oyster GIS Layer presence, survey results
Oyster_layer <- c("Y") #Should cells with oysters in the GIS layer be shown ("Y"), or not shown
Zone_include <- c("Zone") #Should "Zone", "ShoreZone", "both", or NA (neither) be included in static maps
#
#
#
####Interactive site and static section maps####
#
###Maps are created and automatically saved. Create network folder with following mapping if needed: Region/Maps/Survey/SiteCode/Completed
#Specify mapping output as either "Site" or "Section". Site will output an overall map, Section will output individuals maps for each section. 
#Depth will be added as a layer in interactive Site map but not in static Section maps
#Zone and ShoreZone (if present) will be included as a layer in interactive Site map, will only be included in static maps if specified above.
#
Map_output <- c("Section") #"Site" or "Section"
#
if(Map_output == "Site") {
  leaflet_map <- tm_shape(name = "Microgrid cells", All_data) + 
    tm_borders(col = "gray") + #Cell borders
    #Add oyster layer if used for selection
    {if(Oyster_layer == "Y") tm_shape(name = "Oyster layer presence", All_data %>% subset(FL_Oysters == "Y") %>% 
                                        mutate(FL_Oysters = ifelse(FL_Oysters == "Y", "Oyster layer", FL_Oysters)))+  #Change text for legend
        tm_polygons("FL_Oysters", title = "", palette = c("viridis"), alpha = 0.4)} +
    #Add depth
    tm_shape(name = "Depth", All_data %>% filter(!is.na(Depth))) + tm_polygons("Depth", title = "", palette = c("YlGnBu"), alpha = 0.5) +
    #Add Zone and ShoreZone
    tm_shape(name = "Zone", All_data %>% filter(!is.na(Zone))) + tm_polygons("Zone", title = "", palette = c("brewer.accent"), alpha = 0.4) +
    tm_shape(name = "ShoreZone", All_data %>% filter(!is.na(ShoreZone))) + tm_polygons("ShoreZone", title = "", palette = c("brewer.dark2"), alpha = 0.4) +
    #Add stations
    tm_shape(name = "Survey stations", All_data %>% subset(!is.na(Oysters)))+  
    tm_symbols("Oysters", palette = c("viridis"), size = 0.02) + 
    #Add FL shoreline
    tm_shape(name = "Shoreline", st_make_valid(FL_outline)) + tm_polygons() +
    #Add cell Station numbers
    tm_shape(name = "Station numbers", Stations_surveyed) + tm_text("FixedLocationID", size = "AREA")+ 
    #Add monitoring stations
    {if(Monitoring != "NA") tm_shape(name = "Monitoring stations", Monitor_spdf) +  tm_symbols(shape = 16, size = 0.2, col = "black", border.col = "black", alpha = 0.4)}+
    {if(Monitoring != "NA") tm_add_legend('fill', col = "black", border.col = "black", labels = c("Monitoring Stations"))}+
    tm_layout(main.title = paste0("Survey Station Observations: ", min(as.Date(Comp_Stations$Date, "%m/%d/%Y")), " - ", max(as.Date(Comp_Stations$Date, "%m/%d/%Y"))), 
              main.title.position = "center")+
    tm_view(symbol.size.fixed = FALSE)
  #
  (Site_map <- tmap_leaflet(leaflet_map))
  #
  saveWidget(Site_map, paste0("Maps/Survey/", Site_Code, "/Completed/", Site_Code,"_survey_station_observations_widget.html"))
} else if (Map_output == "Section"){
  #Make plots
  #map_list = list()
  for(i in unique(Stations_surveyed$Section)){
    leaflet_map <- tm_shape(name = "Microgrid cells", All_data %>% filter(Section == i)) + 
      tm_borders(col = "gray") + #Cell borders
      #Add oyster layer if used for selection
      {if(Oyster_layer == "Y") tm_shape(name = "Oyster layer presence", All_data %>% subset(FL_Oysters == "Y" & Section == i) %>% 
                                          mutate(FL_Oysters = ifelse(FL_Oysters == "Y", "Oyster layer", FL_Oysters)))+  #Change text for legend
          tm_polygons("FL_Oysters", title = "", palette = c("viridis"), alpha = 0.4)} +
      #Add Zone and ShoreZone
      {if(Zone_include == "Zone" | Zone_include == "both") tm_shape(name = "Zone", All_data %>% filter(!is.na(Zone))) + tm_polygons("Zone", title = "", palette = c("brewer.accent"), alpha = 0.4)} +
      {if(Zone_include == "ShoreZone" | Zone_include == "both") tm_shape(name = "ShoreZone", All_data %>% filter(!is.na(ShoreZone))) + tm_polygons("ShoreZone", title = "", palette = c("brewer.dark2"), alpha = 0.4)} +
      #Add stations
      tm_shape(name = "Survey stations", All_data %>% subset(!is.na(Oysters)))+  
      tm_symbols("Oysters", palette = c("viridis"), size = 0.5) + 
      #Add FL shoreline
      tm_shape(name = "Shoreline", st_make_valid(FL_outline)) + tm_polygons() +
      #Add cell Station numbers
      tm_shape(name = "Station numbers", Stations_surveyed %>% filter(Section == i)) + tm_text("FixedLocationID", size = 0.45)+ 
      #Add monitoring stations
      {if(Monitoring != "NA") tm_shape(name = "Monitoring stations", Monitor_spdf) +  tm_symbols(shape = 16, size = 0.75, col = "black", border.col = "black", alpha = 0.4)}+
      {if(Monitoring != "NA") tm_add_legend('fill', col = "black", border.col = "black", labels = c("Monitoring Stations"))}+
      tm_layout(main.title = paste0("Survey Station Observations: ", i, ": ", min(as.Date(Comp_Stations$Date, "%m/%d/%Y")), " - ", max(as.Date(Comp_Stations$Date, "%m/%d/%Y"))),
                main.title.position = "center")
    #
    tmap_save(leaflet_map, file = paste0("Maps/Survey/", Site_Code, "/Completed/", Site_Code, "_", i, "_survey_station_observations.jpg"),
              dpi = 1000)
  }
}
#
#
#
#
