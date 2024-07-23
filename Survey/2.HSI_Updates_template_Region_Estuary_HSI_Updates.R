####Template for creating HSM and assigning evaluation to MicroGrid.
####Update the Region, Estuary/Site Code, and StateGrids in lines 25-28 (lines 21-24 after removing template header). 
####Delete lines 1-4 then save as new working file.
#
####Regional Florida MicroGrid HSI updates
#
#Compute salinity, temperature, and depth point HSI values
#Interpolate values over estuary area
#
#Requires existing WQ data file for StateGrid of interest: Use WQ_data_compilation code to compile and clean before running this code. 
#Can also use the Selected stations from WQ_data_selection if desired. Using StateGrid in this code file to limit possible errors/missing file updates.
#
#Run line 10 if continuing work from WQ data compilation or selection
rm(list=ls(all=TRUE))
#
#Use Alt+O to collapse all sections, Alt+Shift+O to expand all sections
#
#
#Load require packages (install as necessary)  - MAKE SURE PACMAN IS INSTALLED AND RUNNING!
if (!require("pacman")) {install.packages("pacman")}
pacman::p_load(plyr, tidyverse, #Df manipulation, basic summary
               sf, raster, sp, terra,
               tmap, tmaptools, #Mapping and figures
               RColorBrewer, magicfor, ecorest, #HSV scoring
               marmap, gstat, xts, dismo, #Depth, interpolation
               install = TRUE) 
#
#
#Assign Region, Estuary Code, and StateGrid(s). Only assign the Alternate state grid if required.
Region <- c("SouthEast") #SouthEast, SouthWest, NorthEast, NorthWest, NorthCentral
Site_Code <- c("SLE")
State_Grid <- c("H4")
Alt_State_Grid <- c("H5") 
Date <- c("2023-07-20") #Date of grid updates (output data date from file name) Format: YYYY-MM-DD
#
##WQ data range - start and end year for WQ data compilation (refer to file name for dates)
Start_year <- c("2012")
End_year <- c("2022")
#
###When loading files, a primary State_Grid is used with the option of an alternate or additional StateGrid.
###If an estuary falls completely within 1 StateGrid, skip lines for the "Alt" as instructed in each section.
#
#
####Load files####
#
##MicroGrid - skip lines 44, 50-1 if no Alt.
MicroGrid <- st_read(paste0("../Base Layers/MicroGrids/Florida_MicroGrid_WGS84_",State_Grid,"_clip.shp"))
Alt_MicroGrid <- st_read(paste0("../Base Layers/MicroGrids/Florida_MicroGrid_WGS84_",Alt_State_Grid,"_clip.shp"))
#
#Check data, view map
plot(MicroGrid$geometry)
head(MicroGrid)
#
plot(Alt_MicroGrid$geometry)
head(Alt_MicroGrid)
#
#
Estuary_area <- st_read(paste0("../Base Layers/Site_Region_Areas/", Site_Code, ".kml"))
#
#Run line 57 if no Alt or line 59 if Alt.
Site_Grid <- MicroGrid[lengths(st_intersects(MicroGrid, Estuary_area))> 0,] %>% dplyr::select(-Estuary)
#
Site_Grid <- rbind(MicroGrid[lengths(st_intersects(MicroGrid, Estuary_area))> 0,], 
                   Alt_MicroGrid[lengths(st_intersects(Alt_MicroGrid, Estuary_area))> 0,]) %>% dplyr::select(-Estuary)
#
#
##Microgrid Estuary Data - change date to match data of grid update file creation (in file name)
Site_data <- read.csv(paste0("Output_data/", Site_Code, "_MicrogridData_", Date,".csv"), 
                      na.string = c("Z", "", "NA"))
head(Site_data)
#
#
##WQ data - run line 71-2 if no Alt, or lines 75-79 if Alt
##Water quality CSV (one grid)
WQ <- read.csv(paste0("../WQ/Cleaned_data/", State_Grid, "_combined_filtered_", Start_year, "_" ,End_year ,".csv"), 
               na.string = c("Z", "", "NA"))
#
##Water quality CSV (two grids)
WQ1 <- read.csv(paste0("../WQ/Cleaned_data/", State_Grid, "_combined_filtered_", Start_year, "_" ,End_year ,".csv"), 
                na.string = c("Z", "", "NA"))
WQ2 <- read.csv(paste0("../WQ/Cleaned_data/", Alt_State_Grid, "_combined_filtered_", Start_year, "_" ,End_year ,".csv"), 
                na.string = c("Z", "", "NA"))
WQ <- rbind(WQ1, WQ2)
#
#
#
##Salinity data
#Filter to only salinity, add Year and Month columns, rename columns, and clean data/checks
Salinity <- WQ %>% filter(CharacteristicName == "Salinity") %>%
  mutate(Year = as.integer(substr(ActivityStartDate, 1, 4)),
         Month = as.integer(substr(ActivityStartDate, 6, 7))) %>% 
  dplyr::select(Year, Month, LongitudeMeasure, LatitudeMeasure, ResultMeasureValue) %>% 
  rename(Longitude = LongitudeMeasure,
         Latitude = LatitudeMeasure,
         Salinity = ResultMeasureValue) %>%
  filter(Salinity < 52) %>% filter(Longitude < 0 & Latitude > 0)
#
#
##Temp data
#Filter to only salinity, add Year and Month columns, rename columns, and clean data/checks
Temperature <- WQ %>% filter(CharacteristicName == "Temperature, water") %>%
  mutate(Year = as.integer(substr(ActivityStartDate, 1, 4)),
         Month = as.integer(substr(ActivityStartDate, 6, 7))) %>% 
  dplyr::select(Year, Month, LongitudeMeasure, LatitudeMeasure, ResultMeasureValue) %>% 
  rename(Longitude = LongitudeMeasure,
         Latitude = LatitudeMeasure,
         Temp = ResultMeasureValue) %>%
  filter(Longitude < 0 & Latitude > 0)
#
#
##Depth (from WQ station data)
Depth <- WQ %>% filter(CharacteristicName == "Depth") %>%
  mutate(Year = as.integer(substr(ActivityStartDate, 1, 4)),
         Month = as.integer(substr(ActivityStartDate, 6, 7))) %>% 
  dplyr::select(Year, Month, LongitudeMeasure, LatitudeMeasure, ResultMeasureValue) %>% 
  rename(Longitude = LongitudeMeasure,
         Latitude = LatitudeMeasure,
         WQ_Depth = ResultMeasureValue) %>%
  filter(Longitude < 0 & Latitude > 0)
#
#
#
#
####Set up HSI curves####
#
##Establish HSI curves to assign suitability scores to parameters measurements. Value break points are assigned scores 0 (worst) to 1 (best)
#
###Salinity
#S1 = mean salinity May - September 
#S2 = min salinity Jan - Dec
#S3 = mean annual salinity
#
Sal_S1 <- data.frame(S1_sal = c(0, 5, 12, 15, 18, 23, 30, 35, 40, 45),
                     S1_SIV = c(0, 0, 0.5, 0.65, 1, 1, 0.3, 0.1, 0, 0))
Sal_S1_m <- as.matrix(Sal_S1)
#
Sal_S2 <- data.frame(S2_sal = c(0, 4, 8, 30),
                     S2_SIV = c(0, 0.1, 1, 1))
Sal_S2_m <- as.matrix(Sal_S2)
#
Sal_S3 <- data.frame(S3_sal = c(0, 10, 18, 25, 30, 40, 45),
                     S3_SIV = c(0, 1, 1, 0.25, 0.1, 0, 0))
Sal_S3_m <- as.matrix(Sal_S3)
#
ggplot()+
  geom_line(data = Sal_S1, aes(S1_sal, S1_SIV, color = "Mean ppt May-Sept"), linetype = 1)+
  geom_line(data = Sal_S2, aes(S2_sal, S2_SIV, color = "Min ppt Jan-Dec"), linetype = 1)+
  geom_line(data = Sal_S3, aes(S3_sal, S3_SIV, color = "Annual mean ppt"), linetype = 1)+
  scale_color_manual(name = "SI curves", 
                     values = c("Mean ppt May-Sept" = "red", "Min ppt Jan-Dec" = "blue", "Annual mean ppt" = "dark green"))+
  scale_y_continuous(limits = c(0,1.01), expand = c(0,0))+
  scale_x_continuous(limits = c(0, 45), expand = c(0,0))+
  xlab("Salinity") + ylab("SI Score")+
  theme_classic()+
  theme(legend.position = "right")
#
#
#
#
##Temperature
Temp_S1 <- data.frame(t1_temp = c(0, 8, 10, 22, 30, 50),
                      t1_SIV = c(0, 0.75, 1, 1, 0.75, 0))
Temp_S1_m <- as.matrix(Temp_S1)
#
ggplot()+
  geom_line(data = Temp_S1, aes(t1_temp, t1_SIV), linetype = 1)+
  scale_y_continuous(limits = c(0,1.01), expand = c(0,0))+
  scale_x_continuous(limits = c(0, 50), expand = c(0,0))+
  xlab("Temperature (C)") + ylab("SI Score")+
  theme_classic()
#
#
#
###Depth
Depth_curve <- data.frame(Depth_val = c(0, 0.5, 3, 3.5, 10),
                          Depth_SIV = c(0.5, 1, 1, 0.5, 0)) 
#
Depth_ma <- as.matrix(Depth_curve)
#
ggplot() + 
  geom_line(data = Depth_curve, aes(Depth_val, Depth_SIV, color = "Depth"), size = 1)+
  theme_classic()+
  scale_x_continuous(name = "Water depth (m)", expand = c(0, 0)) + 
  scale_y_continuous(name = "Suitability Score", expand = c(0.001, 0)) +
  theme(legend.position = "none")
#
#
#
#
#
####Salinity scaling####
#
##S1 - mean salinities May-Sept - Limit to proper months, drop NAs and typos, group by Month and Location, get mean value
S1_data <- Salinity %>% filter(Month > 4 & Month < 10) %>% 
  filter(Longitude < 0 & !is.na(Longitude)) %>% 
  filter(!is.na(Salinity)) %>%
  group_by(Month, Longitude, Latitude) %>%
  summarise(meanSal = mean(Salinity, na.rm = T)) 
#
head(S1_data)
#
#Scale values using HSI S1 curve
magic_for(print)
for (i in S1_data$meanSal){
  S1_SIV <- SIcalc(Sal_S1_m[,1:2], c(i)) 
  print(S1_SIV)
}
temp_s1 <- magic_result_as_dataframe()
#Bind to original data
S1_scoring <- cbind(S1_data, temp_s1)
#Check scores for non-conforming data (points not on the curve)
S1_scoring %>% filter(meanSal < 60) %>%
  ggplot(aes(meanSal, S1_SIV))+
  geom_point()+
  geom_line(data = Sal_S1, aes(S1_sal, S1_SIV), color = "red")+
  theme_classic()
#
#Score output for s1 curve
S1_HSI <- S1_scoring %>% group_by(Longitude, Latitude) %>%
  summarise(meanS1 = mean(S1_SIV, na.rm = T))
#
#
#
##S2 - min salinities Jan - Dec - Remove bad data, group by month and location, get minumum per month/location
S2_data <- Salinity %>% 
  filter(Longitude < 0 & !is.na(Longitude)) %>% 
  filter(!is.na(Salinity)) %>%
  group_by(Month, Longitude, Latitude) %>%
  summarise(minSal = min(Salinity, na.rm = T)) 
#
head(S2_data)
#
#Scale values using HSI S2 curve
magic_for(print)
for (i in S2_data$minSal){
  S2_SIV <- SIcalc(Sal_S2_m[,1:2], c(i)) 
  print(S2_SIV)
}
temp_s2 <- magic_result_as_dataframe()
#Bind to original data
S2_scoring <- cbind(S2_data, temp_s2)
##
#Check scores
S2_scoring %>% filter(minSal < 60) %>%
  ggplot(aes(minSal, S2_SIV))+
  geom_point()+
  geom_line(data = Sal_S2, aes(S2_sal, S2_SIV), color = "red")+
  theme_classic()
#
S2_HSI <- S2_scoring %>% group_by(Longitude, Latitude) %>%
  summarise(meanS2 = mean(S2_SIV, na.rm = T))
#
#
#
##S3 - mean annual salinity - Remove bad data, group by Year and location, get mean values per year/station
S3_data <- Salinity %>% 
  filter(Longitude < 0 & !is.na(Longitude)) %>% 
  filter(!is.na(Salinity)) %>%
  group_by(Year, Longitude, Latitude) %>%
  summarise(meanSal = mean(Salinity, na.rm = T)) 
#
head(S3_data)
#
#Scale values using HSI S3 curve
magic_for(print)
for (i in S3_data$meanSal){
  S3_SIV <- SIcalc(Sal_S3_m[,1:2], c(i)) 
  print(S3_SIV)
}
temp_s3 <- magic_result_as_dataframe()
#Bind to original data
S3_scoring <- cbind(S3_data, temp_s3)
##
#Check scores
S3_scoring %>% filter(meanSal < 60) %>%
  ggplot(aes(meanSal, S3_SIV))+
  geom_point()+
  geom_line(data = Sal_S3, aes(S3_sal, S3_SIV), color = "red")+
  theme_classic()
#
S3_HSI <- S3_scoring %>% group_by(Longitude, Latitude) %>%
  summarise(meanS3 = mean(S3_SIV, na.rm = T))
#
#
#
#
##Combine all values for each grid square to determine final Salinity score - match values, and get final product (s1*s2*s3)
Salinity_HSI <- full_join(S1_HSI, S2_HSI) %>% full_join(S3_HSI) %>%
  rowwise() %>% 
  mutate(FinalSalinityScore = prod(c(meanS1, meanS2, meanS3), na.rm = T))
#
head(Salinity_HSI)
#
rm(S1_data, temp_s1, S1_scoring, S2_data, temp_s2, S2_scoring, S3_data, temp_s3, S3_scoring)
rm(Sal_S1_m, Sal_S2_m, Sal_S3_m)
#
#
#
#
####Temperature scaling####
#
##T1 - mean temperature May-Oct - Limit to proper months, drop NAs and typos, group by Month and Location, get mean value
t1_data <- Temperature %>% filter(Month > 4 & Month < 11) %>% 
  filter(Longitude < 0 & !is.na(Longitude)) %>% 
  filter(!is.na(Temp)) %>%
  group_by(Month, Longitude, Latitude) %>%
  summarise(meanTemp = mean(Temp, na.rm = T)) 
#
head(t1_data)
#
#Scale values using HSI S1 curve
magic_for(print)
for (i in t1_data$meanTemp){
  t1_SIV <- SIcalc(Temp_S1_m[,1:2], c(i)) 
  print(t1_SIV)
}
temp_t1 <- magic_result_as_dataframe()
#Bind to original data
t1_scoring <- cbind(t1_data, temp_t1)
#Check scores for non-conforming data (points not on the curve)
t1_scoring %>% filter(meanTemp < 60) %>%
  ggplot(aes(meanTemp, t1_SIV))+
  geom_point()+
  geom_line(data = Temp_S1, aes(t1_temp, t1_SIV), color = "red")+
  theme_classic()
#
#Score output for s1 curve
t1_HSI <- t1_scoring %>% group_by(Longitude, Latitude) %>%
  summarise(meant1 = mean(t1_SIV, na.rm = T))
#
#
#
#
##T2 - min temperature Jan - Dec - Remove bad data, group by month and location, get minimum per month/location
t2_data <- Temperature %>% 
  filter(Longitude < 0 & !is.na(Longitude)) %>% 
  filter(!is.na(Temp)) %>%
  group_by(Month, Longitude, Latitude) %>%
  summarise(minTemp = min(Temp, na.rm = T)) 
#
head(t2_data)
#
#Scale values using HSI t2 curve
magic_for(print)
for (i in t2_data$minTemp){
  t2_SIV <- SIcalc(Temp_S1_m[,1:2], c(i)) 
  print(t2_SIV)
}
temp_t2 <- magic_result_as_dataframe()
#Bind to original data
t2_scoring <- cbind(t2_data, temp_t2)
##
#Check scores
t2_scoring %>% 
  ggplot(aes(minTemp, t2_SIV))+
  geom_point()+
  geom_line(data = temp_t2, aes(i, t2_SIV), color = "red")+
  theme_classic()
#
t2_HSI <- t2_scoring %>% group_by(Longitude, Latitude) %>%
  summarise(meant2 = mean(t2_SIV, na.rm = T))
#
#
#
#
##T3 - mean annual temperature - Remove bad data, group by Year and location, get mean values per year/station
t3_data <- Temperature %>% 
  filter(Longitude < 0 & !is.na(Longitude)) %>% 
  filter(!is.na(Temp)) %>%
  group_by(Year, Longitude, Latitude) %>%
  summarise(meanTemp = mean(Temp, na.rm = T)) 
#
head(t3_data)
#
#Scale values using HSI t3 curve
magic_for(print)
for (i in t3_data$meanTemp){
  t3_SIV <- SIcalc(Temp_S1_m[,1:2], c(i)) 
  print(t3_SIV)
}
temp_t3 <- magic_result_as_dataframe()
#Bind to original data
t3_scoring <- cbind(t3_data, temp_t3)
##
#Check scores
t3_scoring %>% 
  ggplot(aes(meanTemp, t3_SIV))+
  geom_point()+
  geom_line(data = temp_t3, aes(i, t3_SIV), color = "red")+
  theme_classic()
#
t3_HSI <- t3_scoring %>% group_by(Longitude, Latitude) %>%
  summarise(meant3 = mean(t3_SIV, na.rm = T))
#
#
#
#
##Combine all values for each grid square to determine final Salinity score - match values, and get final product
Temperature_HSI <- full_join(t1_HSI, t2_HSI) %>% full_join(t3_HSI) %>%
  rowwise() %>% 
  mutate(FinalTemperatureScore = prod(c(meant1, meant2, meant3), na.rm = T))
#
head(Temperature_HSI)
#
rm(t1_data, temp_t1, t1_scoring, t2_data, temp_t2, t2_scoring, t3_data, temp_t3, t3_scoring)
rm(Temp_S1_m)
#
#
#
####Depth scaling####
#
##Average depth on station - Drop NAs and typos, group by Month and Location, get mean value
d1_data <- Depth %>% 
  filter(Longitude < 0 & !is.na(Longitude)) %>% 
  filter(!is.na(WQ_Depth)) %>%
  group_by(Longitude, Latitude) %>%
  summarise(WQmeanDepth = mean(WQ_Depth, na.rm = T)) 
#
head(d1_data)
#
#Scale values using HSI S1 curve
magic_for(print)
for (i in d1_data$WQmeanDepth){
  d1_SIV <- SIcalc(Depth_curve[,1:2], c(i)) 
  print(d1_SIV)
}
temp_d1 <- magic_result_as_dataframe()
#Bind to original data
d1_scoring <- cbind(d1_data, temp_d1)
#Check scores for non-conforming data (points not on the curve)
d1_scoring %>% 
  ggplot(aes(WQmeanDepth, d1_SIV))+
  geom_point()+
  geom_line(data = Depth_curve, aes(Depth_val, Depth_SIV), color = "red")+
  theme_classic()
#
#Score output for s1 curve
d1_HSI <- d1_scoring %>% group_by(Longitude, Latitude) %>%
  summarise(meanWQD = mean(d1_SIV, na.rm = T))
#
rm(d1_data, temp_d1, d1_scoring, Depth_curve)
#
#
#
####Save all scores to data frame####
#
#
Site_HSI_Scores <- full_join(Salinity_HSI, Temperature_HSI) %>% full_join(d1_HSI) %>%
  rename("Sal1" = meanS1, "Sal2" = meanS2, "Sal3" = meanS3, "Salinity" = FinalSalinityScore,
         "Temp1" = meant1, "Temp2" = meant2, "Temp3" = meant3, "Temperature" = FinalTemperatureScore,
         "WQDepth" = meanWQD) %>%
  rowwise() %>% 
  mutate(HSM_Score = prod(c(Sal1, Sal2, Sal3, Temp1, Temp2, Temp3, WQDepth), na.rm = T),
         Salinity_C = round(Salinity, 1), #Round scores into rough groups
         Temperature_C = round(Temperature, 1), 
         Depth_C = round(WQDepth, 1),
         HSM_C = round(HSM_Score, 1))
#
head(Site_HSI_Scores)
#
#
#Output scores with year range of data used
write.csv(Site_HSI_Scores, file = paste0("Output_Data/", Site_Code, "_HSI_Scores_", 
                                         Start_year, "_", End_year,".csv"), row.names = FALSE)
#
#
####Interpolation of HSM values####
#
#inverse distance weighted interpolation
Site_Grid_spdf <- as(Site_Grid, "Spatial")
#
#Create grid of area based on station locations - used for all scores
grid <- spsample(Site_Grid_spdf, type = 'regular', n = 10000)
plot(grid) 
#
###Salinity
#Data as spatial dataframes
S_Scores_spdf <- SpatialPointsDataFrame(coords = Site_HSI_Scores[,1:2],
                                        Site_HSI_Scores[,13], #Data interested in Salinity=13, Temperatue=14, Depth=15, HSM=16
                                        proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +type=crs"))
plot(S_Scores_spdf)
#IDW method - removing NAs for interpolation
Sal_idw <- idw(na.omit(S_Scores_spdf$Salinity_C)~1, S_Scores_spdf[!is.na(S_Scores_spdf$Salinity_C),], newdata = grid)
#Convert to dataframe to rename and add parameters levels as values rounded to 0.1
Sal_idw.output <- as.data.frame(Sal_idw) %>% rename("Longitude" = x1, "Latitude" = x2, "Prediction" = var1.pred) %>%
  mutate(Sal_Pred_C = round(Prediction, 1)) 
head(Sal_idw.output)
#
#Convert interpolated values to spatial data
Sal_idw_spdf <- SpatialPointsDataFrame(coords = Sal_idw.output[,1:2],
                                       as.data.frame(Sal_idw.output$Sal_Pred_C) %>% rename(Sal_Pred_C = "Sal_idw.output$Sal_Pred_C"), 
                                       proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +type=crs"))
#
#Use nearest neighbor to merge values into polygons, limit to bounding box of estuary area
Sal_nn <- voronoi(Sal_idw_spdf, ext = extent(Site_Grid)) 
qtm(Sal_nn, fill = "Sal_Pred_C")
#
#Determine overlay of data on SiteGrid
Sal_output <- intersect(Sal_nn, Site_Grid_spdf)
#
tmap_arrange(qtm(Sal_nn, fill = "Sal_Pred_C", bbox = extent(Site_Grid)),
             tm_shape(Sal_output)+ tm_fill("Sal_Pred_C"))
#
#
#
#
#
#
###Temperature
#Data as spatial dataframes
T_Scores_spdf <- SpatialPointsDataFrame(coords = Site_HSI_Scores[,1:2],
                                        Site_HSI_Scores[,14], #Data interested in Salinity=13, Temperatue=14, Depth=15, HSM=16
                                        proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +type=crs"))
plot(T_Scores_spdf)
#IDW method - removing NAs for interpolation
Temp_idw <- idw(na.omit(T_Scores_spdf$Temperature_C)~1, T_Scores_spdf[!is.na(T_Scores_spdf$Temperature_C),], newdata = grid)
#Convert to dataframe to rename and add parameters levels as values rounded to 0.1
Temp_idw.output <- as.data.frame(Temp_idw) %>% rename("Longitude" = x1, "Latitude" = x2, "Prediction" = var1.pred) %>%
  mutate(Temp_Pred_C = round(Prediction, 1)) 
head(Temp_idw.output)
#
#Convert interpolated values to spatial data
Temp_idw_spdf <- SpatialPointsDataFrame(coords = Temp_idw.output[,1:2],
                                        as.data.frame(Temp_idw.output$Temp_Pred_C) %>% rename(Temp_Pred_C = "Temp_idw.output$Temp_Pred_C"), 
                                        proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +type=crs"))
#
#Use nearest neighbor to merge values into polygons, limit to bounding box of estuary area
Temp_nn <- voronoi(Temp_idw_spdf, ext = extent(Site_Grid)) 
qtm(Temp_nn, fill = "Temp_Pred_C")
#
#Determine overlay of data on SiteGrid
Temp_output <- intersect(Temp_nn, Site_Grid_spdf)
#
tmap_arrange(qtm(Temp_nn, fill = "Temp_Pred_C", bbox = extent(Site_Grid)),
             tm_shape(Temp_output)+ tm_fill("Temp_Pred_C"))
#
# 
#
#
#
#
###Depth
#Data as spatial dataframes
D_Scores_spdf <- SpatialPointsDataFrame(coords = Site_HSI_Scores[,1:2],
                                        Site_HSI_Scores[,15], #Data interested in Salinity=13, Temperatue=14, Depth=15, HSM=16
                                        proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +type=crs"))
plot(D_Scores_spdf)
#IDW method - removing NAs for interpolation
Depth_idw <- idw(na.omit(D_Scores_spdf$Depth_C)~1, D_Scores_spdf[!is.na(D_Scores_spdf$Depth_C),], newdata = grid)
#Convert to dataframe to rename and add parameters levels as values rounded to 0.1
Depth_idw.output <- as.data.frame(Depth_idw) %>% rename("Longitude" = x1, "Latitude" = x2, "Prediction" = var1.pred) %>%
  mutate(Depth_Pred_C = round(Prediction, 1)) 
head(Depth_idw.output)
#
#Convert interpolated values to spatial data
Depth_idw_spdf <- SpatialPointsDataFrame(coords = Depth_idw.output[,1:2],
                                         as.data.frame(Depth_idw.output$Depth_Pred_C) %>% rename(Depth_Pred_C = "Depth_idw.output$Depth_Pred_C"), 
                                         proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +type=crs"))
#
#Use nearest neighbor to merge values into polygons, limit to bounding box of estuary area
Depth_nn <- voronoi(Depth_idw_spdf, ext = extent(Site_Grid)) 
qtm(Depth_nn, fill = "Depth_Pred_C")
#
#Determine overlay of data on SiteGrid
Depth_output <- intersect(Depth_nn, Site_Grid_spdf)
#
tmap_arrange(qtm(Depth_nn, fill = "Depth_Pred_C", bbox = extent(Site_Grid)),
             tm_shape(Depth_output)+ tm_fill("Depth_Pred_C"))
#
#
#
#
###Combined
#Data as spatial data frames
C_Scores_spdf <- SpatialPointsDataFrame(coords = Site_HSI_Scores[,1:2],
                                        Site_HSI_Scores[,16], #Data interested in Salinity=13, Temperature=14, Depth=15, HSM=16
                                        proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +type=crs"))
plot(C_Scores_spdf)
#IDW method - removing NAs for interpolation
Comb_idw <- idw(na.omit(C_Scores_spdf$HSM_C)~1, C_Scores_spdf[!is.na(C_Scores_spdf$HSM_C),], newdata = grid)
#Convert to dataframe to rename and add parameters levels as values rounded to 0.1
Comb_idw.output <- as.data.frame(Comb_idw) %>% rename("Longitude" = x1, "Latitude" = x2, "Prediction" = var1.pred) %>%
  mutate(Comb_Pred_C = round(Prediction, 1)) 
head(Comb_idw.output)
#
#Convert interpolated values to spatial data
Comb_idw_spdf <- SpatialPointsDataFrame(coords = Comb_idw.output[,1:2],
                                        as.data.frame(Comb_idw.output$Comb_Pred_C) %>% rename(Comb_Pred_C = "Comb_idw.output$Comb_Pred_C"), 
                                        proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +type=crs"))
#
#Use nearest neighbor to merge values into polygons, limit to bounding box of estuary area
Comb_nn <- voronoi(Comb_idw_spdf, ext = extent(Site_Grid)) 
qtm(Comb_nn, fill = "Comb_Pred_C")
#
#Determine overlay of data on SiteGrid
Comb_output <- intersect(Comb_nn, Site_Grid_spdf)
#
tmap_arrange(qtm(Comb_nn, fill = "Comb_Pred_C", bbox = extent(Site_Grid)),
             tm_shape(Comb_output)+ tm_fill("Comb_Pred_C"))
#
#
#
rm(grid, S_Scores_spdf, Sal_idw, Sal_idw.output, Sal_idw_spdf, Sal_nn, 
   T_Scores_spdf, Temp_idw, Temp_idw.output, Temp_idw_spdf, Temp_nn,
   D_Scores_spdf, Depth_idw, Depth_idw.output, Depth_idw_spdf, Depth_nn,
   C_Scores_spdf, Comb_idw, Comb_idw.output, Comb_idw_spdf, Comb_nn)
#
#
#
####Data compilation####
#
#
Final_data <- Site_data %>% 
  #Add Salinity scores and keep highest score if multiple exist
  left_join(as.data.frame(Sal_output) %>% dplyr::select(MGID, Sal_Pred_C)) %>%
  group_by(MGID) %>% arrange(desc(Sal_Pred_C)) %>% slice(1) %>%
  #Add Temperature scores
  left_join(as.data.frame(Temp_output) %>% dplyr::select(MGID, Temp_Pred_C)) %>%
  group_by(MGID) %>% arrange(desc(Temp_Pred_C)) %>% slice(1) %>%
  #Add Depth scores
  left_join(as.data.frame(Depth_output) %>% dplyr::select(MGID, Depth_Pred_C)) %>%
  group_by(MGID) %>% arrange(desc(Depth_Pred_C)) %>% slice(1) %>%
  #Add combined scores
  left_join(as.data.frame(Comb_output) %>% dplyr::select(MGID, Comb_Pred_C)) %>%
  group_by(MGID) %>% arrange(desc(Comb_Pred_C)) %>% slice(1) %>%
  rename("Salinity_HSI" = Sal_Pred_C, "Temperature_HSI" = Temp_Pred_C, 
         "Depth_HSI" = Depth_Pred_C, "HSM_Score" = Comb_Pred_C)
#
head(Final_data)
#
#Output scores with year range of data used
write.csv(Final_data, file = paste0("Output_Data/", Site_Code, "_MicrogridData_HSM_", 
                                    Start_year, "_", End_year,".csv"), row.names = FALSE)
#
#
#
