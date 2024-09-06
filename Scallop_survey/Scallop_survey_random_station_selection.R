###Survey station selection
#If running from project file, do not need to set working directory
#
#Code to load possible stations and select by total number or number per block
#Output: Selected station info, final list, map
#Lines to update for each survey indicated by ##***
#
#
rm(list=ls(all=TRUE)) # clears out environment  
#
#Load packages to work with. Install missing packages as needed.
if (!require("pacman")) {install.packages("pacman")} #- MAKE SURE PACMAN IS INSTALLED AND RUNNING!
pacman::p_load(readxl, plyr, tidyverse, #xtable, 
               sf, raster, marmap, #terra, sp, rgdal,
               leaflet, tmap, DescTools, tmaptools, tigris,
               sampling, 
               install = TRUE) 

#library(ggmap)
#options("sp_evolution_status"=2)
#
#
####Load File and set working info####
#
##***Set Site - 2 letter code
Site <- c("PA")
##***Set year of survey - 4 digits
Year <- c("2024")
##***Set "PreSeason" or "PostSeason"
Season <- c("PostSeason")
#
#Change seed number between seasons/regions - can leave seed the same within same season to replicate output
#Add seed number used to line below to avoid duplication.
#Seeds used: 4321-HO2022Pre, 5421-HO2022Post, 5321-AN2024Post
seeding <- c("5321")
#
#Excel sheet of all possible stations assigned into blocks/regions
Possible <- read_xlsx("Data/Survey_Grid_coordinates.xlsx", sheet = 1, .name_repair = "universal") %>%
  filter(Location == Site) %>% relocate(Longitude, .before = Latitude)
str(Possible)
#
#
##County information
Counties <- read.csv("Data/Counties_Sites_FIPS.csv", na.strings = c("", " ", "Z", "NA"))
head(Counties)
FIPS <- as.list((Counties %>% filter(Site_Code == Site))[2])
#
###Station selection by total number of stations####
#
##***Input total number of stations needed
Num_stations <- 36
##***Sets minimum number of extra stations per block (Num_extras)
Num_extras <- 1
#
##Code to determine number of stations needed per block/region for even distribution - rounded up. 
Num_select <- ceiling(Num_stations/length(unique(Possible$Block)))
#
#
##Randomly select required number of stations from each block - use set.seed to replicate stations already chosen.
set.seed(seeding)
Selected <- Possible %>% group_by(Block) %>% slice_sample(n = (Num_select + Num_extras))
#
#
##Assign stations to Target/trawl type or Extra 
Target_stations <- Selected %>% group_by(Block) %>% slice_sample(n = Num_select) %>% #Select target station
  ungroup() %>% mutate(Type = "Target") #Assign as Target
#
Extra_stations <- anti_join(Selected, Target_stations) %>% group_by(Block) %>% #Determine which stations are left
  ungroup() %>% mutate(Type = "Extra") #Assign as Extra
#
#
##Check number of extra stations select. If too many run "Extra_stations %>% arrange", if number is okay skip set.seed().
nrow(Extra_stations) 
nrow(Extra_stations)/5 #Chnage number to decide how many to keep. Chnage seq# and by=# in next line to desired number from this line.
#
Extra_stations <- Extra_stations %>% arrange(Block) %>% slice(seq(5, n(), by = 5))
#
#Randomly select excess target stations to turn into Extra stations
set.seed(seeding)
Excess <- sample(nrow(Target_stations), nrow(Target_stations) - Num_stations)
head(Add_Extra <- Target_stations[Excess,] %>% mutate(Type = "Extra"),4)
#
Final_Target <- if(length(Excess) == 0){Target_stations} else {Target_stations[-Excess,]}
#
#
#Combine final Target stations and all Extra stations into data frame
Survey_Stations <- rbind( #Join together
  rbind(Final_Target, Add_Extra), #Target stations and additional Extras
  Extra_stations) #And original Extras
#
#Double check for duplicates - want all FALSE
duplicated(Survey_Stations$StationID)
#
#Save station list
write.csv(Survey_Stations, file = paste("Output/",Year,"_",Site,"_",Season,"_Stations.csv", sep = ""), row.names = F)
#
#
#
#
###Station selection by number per block####
#
##***Input number of stations needed per block
Num_block <- 2
#
#Randomly select the desired number of stations from each block
set.seed(seeding)
Selected <- Possible %>% group_by(Block) %>% slice_sample(n = Num_block)
#
Survey_Stations <- cbind(Selected, rep(c("Target","Extra"), times = (nrow(Selected)/2)))
names(Survey_Stations)[7] = "Type"
#
#Double check for duplicates - want all FALSE
duplicated(Survey_Stations$StationID)
#
write.csv(Survey_Stations, file = paste("Output/",Year,"_",Site,"_",Season,"_Stations.csv", sep = ""), row.names = F)
#
#
#
#

####Map stations####
#
##Outputs map of all stations, map of target stations
#
##Map area with buffer - W, S, E, N
Area_box <- bb(c(min(Possible$Longitude) - 0.015,  min(Possible$Latitude) - 0.015,
                 max(Possible$Longitude) + 0.015, max(Possible$Latitude) + 0.015))
#
#Get state boundaries and water for FL
FL_state <- states(year = as.numeric(Year)-1) %>% filter(STUSPS == "FL")
plot(FL_state[1])
#
for(i in unique(FIPS)){
  Code <- paste(i)
  t <- area_water("FL", Code, year = as.numeric(Year)-1)
  FL_water <- rbind(t)
}
plot(FL_water[2])
#
#Create map of whole possible survey area to view/check data loaded
Possible_spdf <- st_as_sf(Possible, coords= c("Longitude", "Latitude"), crs = 4326)
tm_shape(FL_state, bbox = Area_box)+ tm_polygons(fill = "gray")+
  tm_shape(FL_water)+ tm_polygons(col = "#CCFFFF")+
  tm_shape(Possible_spdf)+ tm_dots(col = "Block", size = 0.3)
#
#
#Station file
Survey_mapping <- st_as_sf(Survey_Stations, coords = c("Longitude", "Latitude"), crs = 4326)
#DF of ramps and rough channel entrances
Ramps_df <- read_xlsx("Data/Boat_ramp_locations.xlsx", sheet = 1, .name_repair = "universal") 
Ramps <- Ramps_df %>% st_as_sf(coords = c("Long", "Lat"), crs = 4326)
#
#
##Mapping choices
##***Include ramps on map? Y/N
Include_ramps <- c("Y")
##***Map of all stations, just target, or one of each ("All", "Target", "Both")
Mapping_output <- c("Both")
#
#Run next code chunk once mapping choices are updated. Code will create and save map outputs. 
if(Mapping_output == "All"){
  #Map of all stations
  (All_map <- tm_shape(FL_state, bbox = Area_box)+ tm_polygons(fill = "gray")+
     tm_shape(FL_water)+ tm_polygons(col = "#CCFFFF")+
     tm_shape(Survey_mapping)+
      tm_dots(col = "Type", size = 1, palette = c(Target = "#FF9933", Extra = "#009966"),
              alpha = 0.5)+
     tm_text("StationNum", size = 0.6)+
     {if(Include_ramps == "Y") tm_shape(Ramps)+ tm_dots(col = "black", size = 1, alpha = 0.7)+
         tm_text("Ramp", size = 0.75, just = "left", ymod  = -0.2, xmod = 0.3)}+
     tm_layout(legend.position = c("right", "bottom"), legend.bg.color = "white", legend.bg.alpha = 0.8,
               legend.frame = TRUE,
               main.title = paste0(Site, " ", Year, " ", Season, " Survey"),
               main.title.size = 1.25, main.title.position = "center")+
     tm_graticules(lines = FALSE))
  tmap_save(All_map, file = paste0("Output/", Year, "_", Site, "_", Season, "_All.jpg", sep =""),
            dpi = 1000)
} else if(Mapping_output == "Target") {
  #Map of target stations
  (Target_map <- tm_shape(FL_state, bbox = Area_box)+ tm_polygons(fill = "gray")+
      tm_shape(FL_water)+ tm_polygons(col = "#CCFFFF")+
      tm_shape(Survey_mapping %>% filter(Type == "Target"))+
      tm_dots(col = "Type", size = 1, palette = c(Target = "#FF9933", Extra = "#009966"),
              alpha = 0.5)+
      tm_text("StationNum", size = 0.6)+
      {if(Include_ramps == "Y") tm_shape(Ramps)+ tm_dots(col = "black", size = 1, alpha = 0.7)+
          tm_text("Ramp", size = 0.75, just = "left", ymod  = -0.2, xmod = 0.3)}+
      tm_layout(legend.position = c("right", "bottom"), legend.bg.color = "white", legend.bg.alpha = 0.8,
                legend.frame = TRUE,
                main.title = paste0(Site, " ", Year, " ", Season, " Survey"),
                main.title.size = 1.25, main.title.position = "center")+
      tm_graticules(lines = FALSE))
  tmap_save(Target_map, file = paste0("Output/", Year, "_", Site, "_", Season, "_Targets.jpg", sep =""),
            dpi = 1000)
} else if(Mapping_output == "Both"){
  #Map of all stations
  (All_map <- tm_shape(FL_state, bbox = Area_box)+ tm_polygons(fill = "gray")+
     tm_shape(FL_water)+ tm_polygons(col = "#CCFFFF")+
     tm_shape(Survey_mapping)+
     tm_dots(col = "Type", size = 1, palette = c(Target = "#FF9933", Extra = "#009966"),
             alpha = 0.5)+
     tm_text("StationNum", size = 0.6)+
     {if(Include_ramps == "Y") tm_shape(Ramps)+ tm_dots(col = "black", size = 1, alpha = 0.7)+
         tm_text("Ramp", size = 0.75, just = "left", ymod  = -0.2, xmod = 0.3)}+
     tm_layout(legend.position = c("right", "bottom"), legend.bg.color = "white", legend.bg.alpha = 0.8,
               legend.frame = TRUE,
               main.title = paste0(Site, " ", Year, " ", Season, " Survey"),
               main.title.size = 1.25, main.title.position = "center")+
     tm_graticules(lines = FALSE))
  tmap_save(All_map, file = paste0("Output/", Year, "_", Site, "_", Season, "_All.jpg", sep =""),
            dpi = 1000)
  #
  #Map of target stations
  (Target_map <- tm_shape(FL_state, bbox = Area_box)+ tm_polygons(fill = "gray")+
      tm_shape(FL_water)+ tm_polygons(col = "#CCFFFF")+
      tm_shape(Survey_mapping %>% filter(Type == "Target"))+
      tm_dots(col = "Type", size = 1, palette = c(Target = "#FF9933", Extra = "#009966"),
              alpha = 0.5)+
      tm_text("StationNum", size = 0.6)+
      {if(Include_ramps == "Y") tm_shape(Ramps)+ tm_dots(col = "black", size = 1, alpha = 0.7)+
          tm_text("Ramp", size = 0.75, just = "left", ymod  = -0.2, xmod = 0.3)}+
      tm_layout(legend.position = c("right", "bottom"), legend.bg.color = "white", legend.bg.alpha = 0.8,
                legend.frame = TRUE,
                main.title = paste0(Site, " ", Year, " ", Season, " Survey"),
                main.title.size = 1.25, main.title.position = "center")+
      tm_graticules(lines = FALSE))
  tmap_save(Target_map, file = paste0("Output/", Year, "_", Site, "_", Season, "_Targets.jpg", sep =""),
            dpi = 1000)
} else {print("Incorrect mapping choice specified. Please choose among All, Target, or Both")}
#
#
#