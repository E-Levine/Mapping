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
####Oyster presence by grid cell####
#