##Selection of cleaned Water Quality data
##
##Croping to estuary area
#Requires existing WQ data file for StateGrid of interest: Use WQ_data_compilation code to compile and clean before running this code. 
#Requires existing estuary KML file
#
#Run line 7 if continuing work from WQ data compilation
rm(list=ls(all=TRUE))
#
#Use Alt+O to collapse all sections, Alt+Shift+O to expand all sections
#
#
#Load require packages (install as necessary)
if (!require("pacman")) install.packages("pacman")
pacman::p_load(plyr, tidyverse, #Df manipulation, basic summary
               sf, raster, spData, rgeos, rgdal,
               tmap, tmaptools, scales, broom, RColorBrewer, magicfor, ecorest,
               install = TRUE) #Mapping and figures
#
#
#Enter StateGrid, Site Code, and starting and ending year of WQ data
State_Grid <- c("F5")
Site_Code <- c("CR")
Start_year <- c("2012")
End_year <- c("2022")
#
#
#
####Load files####
#
##MicroGrid 
MicroGrid <- st_read(paste0("../Base Layers/MicroGrids/Florida_MicroGrid_WGS84_",State_Grid,"_clip.shp"))
#
#Check data, view map
plot(MicroGrid$geometry)
head(MicroGrid)
#
#
#
##Estuary area and Sections 
Estuary_area <- st_read(paste0("../Base Layers/Site_Region_Areas/", Site_Code, ".kml"))
plot(Estuary_area[2])
#
#
#
##WQ data
##Water quality CSV
WQ <- read.csv(paste0("Cleaned_data/", State_Grid, "_combined_filtered_", Start_year, "_", End_year,".csv"), 
               na.string = c("Z", "", "NA"))
#
#
##State Outline
FL_outline <- st_read("../Base Layers/FL_Outlines/FL_Outlines.shp")
plot(FL_outline)
#
#
#
###Select data points in estuary area####
#
#Transform CRS and data as spatial data
WQ_sp <- spTransform(SpatialPointsDataFrame(coords = WQ[,c(9,8)], data = WQ,
                                            proj4string = CRS("+init=epsg:4269 +proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0")),
                     CRSobj = CRS(SRS_string = "EPSG:4326"))
#
#Check CRS s
crs(Estuary_area)
crs(WQ_sp)
#
##Crop to estuary area
Estuary_data <- WQ_sp[as.vector(st_intersects(Estuary_area, st_as_sf(WQ_sp), sparse = FALSE)), ]
#Only run 73-75 if issue with Loop in line 70
#
#sf_use_s2(FALSE)
#Estuary_data <- WQ_sp[lengths(st_intersects(Estuary_area, st_as_sf(WQ_sp))>0,]
#sf_use_s2(TRUE)
#
#
#
####Map of stations and output dataframe####
#
#Visualize data locations
(m <- tm_shape(Estuary_data) + 
    tm_dots("StateCode", palette = "Set2", size = 1, legend.show = FALSE) +
    tm_shape(FL_outline) +
    tm_polygons()+
    tm_layout(main.title = paste(Site_Code, "WQ Stations", sep = " "), 
              main.title.position = c('left')))
#
tmap_save(m, paste0("Maps/", State_Grid, "_", Site_Code,"_WQ_stations_", Start_year, "_", End_year, ".jpg"))
#
#Convert to data frame for output and simplicity
Estuary_dataframe <- as.data.frame(Estuary_data) %>% dplyr::select(-LongitudeMeasure.1, -LatitudeMeasure.1) %>%
  mutate(HorizontalCoordinateReferenceSystemDatumName = "WGS84")
#
head(Estuary_dataframe)
#
#Save output
write.csv(Estuary_dataframe, 
          paste0("Estuary_data/",State_Grid, "_", Site_Code,"_WQ_", Start_year, "_", End_year,".csv"))
#
