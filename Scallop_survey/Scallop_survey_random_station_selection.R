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
pacman::p_load(plyr, tidyverse, xtable, 
               sf, raster, terra, sp, rgdal,
               leaflet, tmap, DescTools, tmaptools,
               sampling, readxl,
               install = TRUE) 

#library(ggmap)
options("sp_evolution_status"=2)
#
#
####Load File and set working info####
#
##***Set Site - 2 letter code
Site <- c("AN")
##***Set year of survey - 4 digits
Year <- c("2024")
##***Set "PreSeason" or "PostSeason"
Season <- c("PostSeason")
#
#
#Excel sheet of all possible stations assigned into blocks/regions
Possible <- read_xlsx("Data/Survey_Grid_coordinates_by_block.xlsx", sheet = 1, .name_repair = "universal") %>%
  filter(Location == Site)
str(Possible)
#
#
#Change seed number between seasons/regions - can leave seed the same within same season to replicate output
#Add seed number used to line below to avoid duplication.
#Seeds used: 4321-HO2022Pre, 5421-HO2022Post, 5321-AN2024Post
seeding <- c("5321")
#
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
#Randomly select excess target stations to turn into Extra stations
set.seed(seeding)
Excess <- sample(nrow(Target_stations), nrow(Target_stations) - Num_stations)
head(Add_Extra <- Target_stations[Excess,] %>% mutate(Type = "Extra"),4)
#
Final_Target <- if(nrow(Target_stations) - Num_stations == 0){Target_stations} else {Target_stations[-Excess,]}
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
