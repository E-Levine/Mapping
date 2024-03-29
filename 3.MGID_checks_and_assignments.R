#Code to confirm and assign MGIDs to survey stations. Updated 10/13/2023.
#Outputs confirmed MGID file to Region, station tracking updates to shared folder
#Make sure all stations completed have been added to the "Completed_survey_stations" Excel file.
#Can be run within any Region project-will save to Region's output data of working project
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
Site_Code <- c("CR")
State_Grid <- c("F5")
Alt_State_Grid <- c("G5") 
#
First_Survey <- c("Y") #If this is the first ever random survey completed in the estuary = "Y"; "N" for all other options
Month_Quarter <- c("Month") #Should survey dates be limited by "Month" or by "Quarter" - Quarters divided: Jan-Mar, Apr-Jun, Jul-Sep, Oct-Dec
Survey_Year <- ("2023") #Year of surveys
Survey_Time <- c("05") #Month or quarter of surveys (For naming files)
#
#
#
#
####Load and clean files####
#
#Stations surveyed
Completed <- read.csv("../Reference Files/Completed_survey_stations.csv", na.strings = c("Z", "", "NA"))
head(Completed)
#Limit to site
Comp_stations <- Completed %>% subset(Site == Site_Code)
head(Comp_stations)
#
#
##MicroGrid - skip lines 42, 48-9 if no Alt.
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
##Estuary area and Sections 
Estuary_area <- st_read(paste0("../Base Layers/Site_Region_Areas/", Site_Code, ".kml"))
#
#
#Limit StateGrids to estuary area - Run line 57 if no Alt or lines 59-60 if Alt.
Site_Grid <- MicroGrid[lengths(st_intersects(MicroGrid, Estuary_area))> 0,] %>% mutate(Estuary = Site_Code)
#
Site_Grid <- rbind(MicroGrid[lengths(st_intersects(MicroGrid, Estuary_area))> 0,], 
                   Alt_MicroGrid[lengths(st_intersects(Alt_MicroGrid, Estuary_area))> 0,]) %>% mutate(Estuary = Site_Code)
head(Site_Grid)
#
#
#Estuary codes for fixed station IDs
Codes <- read.csv("../Reference Files/Estuary_SiteCodes.csv", na.strings = c("Z", "", "NA"))
head(Codes)
#
#File of previously uploaded stations with IDs - don't run if no previous survey
Location_IDs <- read.csv(paste0("Output_Data/", Site_Code, "_Station_MGIDs.csv"), na.strings = c("Z", "", "NA"))
head(Location_IDs)
#
Quarters <- data.frame(Month = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"),
                       minQ = c(1, 1, 1, 4, 4, 4, 7, 7, 7, 10, 10, 10),
                       maxQ = c(3, 3, 3, 6, 6, 6, 9, 9, 9, 12, 12, 12))
#
#
#
#
#
###Confirm MGID####
#
#Run line 83 if first survey or line 85 if subsequent survey
Current <- Comp_stations
#Limit to stations not yet checked/added
Current <- Comp_stations %>% anti_join(Location_IDs)
head(Current)
#
#DF of lat and long
Comp_LL <- Current %>% dplyr::select(Longitude, Latitude)
head(Comp_LL)
#
#Identify MGID for locations
Comp_LL$Confirmed_MGID <- apply(Comp_LL, 1, function(row) { 
  #Transform to palnar
  Site_Grid_pl <- st_transform(Site_Grid, 2163)   
  coords <- as.data.frame(matrix(row, nrow = 1, 
                                 dimnames = list("", c("Longitude", "Latitude"))))   
  Comp_sf <- st_transform(st_sfc(st_point(row),crs = 4326), 2163)
  # st_intersects with sparse = FALSE returns a logical matrix

  Site_Grid_pl[which(st_intersects(Comp_sf, Site_Grid_pl, sparse = FALSE)), ]$MGID 
})
head(Comp_LL)
#
#Join Confirmed MGIDs to original df and check if matching records.
Corrected <- left_join(Current, Comp_LL) %>% 
  mutate(Correct = ifelse(is.na(MGID), "N", ifelse(MGID == Confirmed_MGID, "Y", "N")))
head(Corrected)
#
#
#
###Output data for reference####
#
#Limit df and Save output for station upload to ODIN, datachecks
Final_stations <- Corrected %>% 
  dplyr::select(Site, RunningStation, Date, Section, Confirmed_MGID, Latitude, Longitude, MGID) %>%
  rename(Initial_MGID = MGID) %>%
  arrange(RunningStation) %>% 
  mutate(Number = if(First_Survey == "Y") 1:n() else (max(Location_IDs$Number)+row_number()),
         FixedLocationID = paste0(filter(Codes, Site == Site_Code)$FixedID, str_pad(Number, 3, pad = "0")))
#
head(Final_stations)
#
#Use line 125 for first survey, line 126-7 for subsequent surveys within an estuary
#write.csv(Final_stations, file = paste0("Output_Data/", Site_Code, "_Station_MGIDs.csv"), row.names = FALSE)
write.table(Final_stations, file = paste0("Output_Data/", Site_Code, "_Station_MGIDs.csv"), row.names = FALSE,
            sep = ",", append = TRUE, quote = FALSE, col.names = FALSE)
#
#
#
###Create data frame for upload and SQL code####
#
#
Initials <- c("EL") #Your initials
Additional_data <- c("NA") #Were any other data types collected? Enter each with quotes around the type separated by a comma or enter "NA"
#Possible data types: Recruitment, Sediment, Collections, ShellBudget, DataLoggers, Cage, Wave
#
#
#Create empty dataframe to fill
FixedLocations <- data.frame(matrix(ncol = 31, nrow = 0))
#
#Data frame of station summary info
(ODIN_output <- Final_stations %>% 
    dplyr::select(FixedLocationID, Site, Section, RunningStation, Latitude, Longitude, Date, Confirmed_MGID) %>%
    rename("Estuary" = Site, "SectionName" = Section, "StationNumber" = RunningStation,
           "LatitudeDec" = Latitude, "LongitudeDec" = Longitude, 
           "Comments" = Confirmed_MGID) %>%
    group_by(Estuary) %>%
    mutate(FixedLocationID = paste0("'", FixedLocationID, "'"),
           StationName = paste0("'",paste(Estuary, SectionName, Comments, StationNumber, sep = " "),"'"),
           ParcelName = "NULL",
           ParcelArea = "NULL",
           CultchDensity = "NULL",
           Recruitment = paste0("'",ifelse("Recruitment" %in% list(Additional_data),"Y","N"),"'"),
           Survey = paste0("'", "Y", "'"),
           Sediment = paste0("'", ifelse("Sediment" %in% list(Additional_data),"Y","N"), "'"),
           Collections = paste0("'", ifelse("Collections" %in% list(Additional_data),"Y","N"), "'"),
           ShellBudget = paste0("'", ifelse("ShellBudget" %in% list(Additional_data),"Y","N"), "'"),
           Dataloggers = paste0("'", ifelse("DataLoggers" %in% list(Additional_data),"Y","N"), "'"),
           Cage = paste0("'", ifelse("Cage" %in% list(Additional_data),"Y","N"), "'"),
           Wave = paste0("'", ifelse("Wave" %in% list(Additional_data),"Y","N"), "'"),
           StartDate = paste0("'", 
                              ifelse(Month_Quarter == "Month",
                                             paste0(ceiling_date(min(as.Date(Final_stations$Date, "%m/%d/%Y")) %m-% months(1), 'month') - days(1)),
                                             paste0(ceiling_date(as.Date(paste(format(min(as.Date(Final_stations$Date, "%m/%d/%Y")), "%Y"), #Extract year
                                                           filter(Quarters,  #Filter Quarters to proper month for cut off
                                                                  Month == format(min(as.Date(Final_stations$Date, "%m/%d/%Y")), "%m"))$minQ, #Extract month from date
                                                           "1", sep = "-"), format = "%Y-%m-%d") %m-% months(1), 'month')- days(1))),
                              "'"),
           EndDate = paste0("'", 
                            ifelse(Month_Quarter == "Month",
                                   paste0(floor_date(max(as.Date(Final_stations$Date, "%m/%d/%Y")) %m+% months(1), 'month')),
                                   paste0(as.Date(paste(format(max(as.Date(Final_stations$Date, "%m/%d/%Y")), "%Y"), #Extract year
                                                                   filter(Quarters,  #Filter Quarters to proper month for cut off
                                                                          Month == format(max(as.Date(Final_stations$Date, "%m/%d/%Y")), "%m"))$maxQ, #Extract month from date
                                                                   "1", sep = "-"), format = "%Y-%m-%d") %m+% months(1))),
                            "'"),
           DataStatus = paste0("'", "Completed", "'"),
           DateEntered = paste0("'", ymd(Sys.Date()), "'"),
           EnteredBy = paste0("'", Initials, "'"),
           DateProofed = "NULL",
           ProofedBy = "NULL",
           DateCompleted = "NULL",
           CompletedBy = "NULL",
           AdminNotes = "NULL",
           StationNameNumber = paste0("'", paste(Estuary, SectionName, Comments, StationNumber, sep = "-"), "'"),
           EstuaryLongName = paste0("'", filter(Codes, Site == Site_Code)$Estuary, "'")) %>%
   #Add missing quotation marks
   mutate(Estuary = paste0("'", Estuary, "'"),
          SectionName = paste0("'", SectionName, "'"),
          StationNumber = paste0("'", StationNumber, "'"),
          Comments = paste0("'", Comments, "'")) %>%
    dplyr::select(FixedLocationID:SectionName, StationName, StationNumber, ParcelName:CultchDensity, LatitudeDec, LongitudeDec,
                  Recruitment:CompletedBy, Comments, everything()) %>% dplyr::select(-Date))
#
#Fill data frame with station information
FixedLocations <- rbind(FixedLocations, ODIN_output, stringsAsFactors = FALSE)
#
#
#SQL base template code
FixedLocationSQLtemplate <- "
INSERT INTO [dbo].[FixedLocations]
    ([FixedLocationID]
      ,[Estuary]
      ,[SectionName]
      ,[StationName]
      ,[StationNumber]
      ,[ParcelName]
      ,[ParcelArea]
      ,[CultchDensity]
      ,[LatitudeDec]
      ,[LongitudeDec]
      ,[Recruitment]
      ,[Survey]
      ,[Sediment]
      ,[Collections]
      ,[ShellBudget]
      ,[Dataloggers]
      ,[Cage]
      ,[Wave]
      ,[StartDate]
      ,[EndDate]
      ,[DataStatus]
      ,[DateEntered]
      ,[EnteredBy]
      ,[DateProofed]
      ,[ProofedBy]
      ,[DateCompleted]
      ,[CompletedBy]
      ,[Comments]	
      ,[AdminNotes]
      ,[StationNameNumber]
      ,[EstuaryLongName])
  VALUES
      ({FixedLocationID}
      ,{Estuary}
      ,{SectionName}
      ,{StationName}
      ,{StationNumber}
      ,{ParcelName}
      ,{ParcelArea}
      ,{CultchDensity}
      ,{LatitudeDec}
      ,{LongitudeDec}
      ,{Recruitment}
      ,{Survey}
      ,{Sediment}
      ,{Collections}
      ,{ShellBudget}
      ,{Dataloggers}
      ,{Cage}
      ,{Wave}
      ,{StartDate}
      ,{EndDate}
      ,{DataStatus}
      ,{DateEntered}
      ,{EnteredBy}
      ,{DateProofed}
      ,{ProofedBy}
      ,{DateCompleted}
      ,{CompletedBy}
      ,{Comments}
      ,{AdminNotes}
      ,{StationNameNumber}
      ,{EstuaryLongName})
GO"
#
# Use the glue function to fill in the template with the data frame values
FixedLocationSQL <- glue(FixedLocationSQLtemplate, .envir = FixedLocations)
#
#Save SQL code
write_lines(FixedLocationSQL, paste0("../SOPs and Templates/ODIN/SQL/", Site_Code, "_FixedLocations_", Survey_Year, "_", Survey_Time,".sql"))
#
#
#Save summary information for tracking
write.csv(ODIN_output, file = paste0("../SOPs and Templates/ODIN/", Site_Code, "_Station_Data_", Sys.Date(), ".csv"), row.names = FALSE)
#
#
#
###Update tracking file with new stations and data####
#
#Create dataframe of required data
ODIN_output2 <- as.data.frame(sapply(ODIN_output, function(x) gsub("'","",x))) #Remove all quotations
#
(ODIN_site_summ <- ODIN_output2 %>% 
   mutate(StationNumber = as.numeric(ODIN_output2$StationNumber),
          DateEntered = as.Date(ODIN_output2$DateEntered, "%Y-%m-%d")) %>%
   group_by(Estuary, EnteredBy, DateEntered) %>%
   summarise(MinRunning = min(StationNumber),
             MaxRunning = max(StationNumber),
             StartDate = min(as.Date(ODIN_output2$StartDate, format = "%Y-%m-%d")),
             EndDate = max(as.Date(ODIN_output2$EndDate, format = "%Y-%m-%d"))) %>%
   left_join((ungroup(ODIN_output2) %>% group_by(Estuary) %>% slice(1L) %>% rename("MinFixed" = FixedLocationID))[1:2]) %>%
   left_join((ungroup(ODIN_output2) %>% group_by(Estuary) %>% slice(n()) %>% rename("MaxFixed" = FixedLocationID))[1:2]) %>%
   mutate(OdinFile = "Y",
          AddDatabase = NA,
          AddDate = NA) %>%
   rename("CreatedBy" = EnteredBy, "CreationDate" = DateEntered) %>%
   dplyr::select(Estuary, MinRunning, MaxRunning, MinFixed, MaxFixed, StartDate, EndDate, OdinFile, everything()))
#
(ODIN_section_summ <- ODIN_output2 %>% 
    mutate(StationNumber = as.numeric(ODIN_output2$StationNumber),
           DateEntered = as.Date(ODIN_output2$DateEntered, "%Y-%m-%d")) %>%
    group_by(Estuary, SectionName, EnteredBy, DateEntered) %>%
    summarise(MinRunning = min(StationNumber),
              MaxRunning = max(StationNumber),
              StartDate = min(as.Date(ODIN_output2$StartDate, format = "%Y-%m-%d")),
              EndDate = max(as.Date(ODIN_output2$EndDate, format = "%Y-%m-%d"))) %>%
  left_join((ODIN_output2 %>% group_by(Estuary, SectionName) %>% slice(1L) %>% rename("MinFixed" = FixedLocationID))[1:3]) %>%
  left_join((ODIN_output2 %>% group_by(Estuary, SectionName) %>% slice(n()) %>% rename("MaxFixed" = FixedLocationID))[1:3]) %>%
  mutate(OdinFile = "Y",
         AddDatabase = NA,
         AddDate = NA) %>%
  rename("Section" = SectionName, "CreatedBy" = EnteredBy, "CreationDate" = DateEntered) %>%
  dplyr::select(Estuary, Section, MinRunning, MaxRunning, MinFixed, MaxFixed, StartDate, EndDate, OdinFile, everything()) %>%
  arrange(Estuary, MaxRunning))
#
#Load data from file
Track_Site <- readWorkbook("../SOPs and Templates/ODIN/ODIN_random_station_tracking.xlsx", sheet = "BySite") %>%
  mutate(StartDate = as.Date(StartDate, origin = "1899-12-30"),
         EndDate = as.Date(StartDate, origin = "1899-12-30"),
         CreationDate = as.Date(StartDate, origin = "1899-12-30"),
         AddDate = as.Date(StartDate, origin = "1899-12-30"))
head(Track_Site)
#
Track_Section <- readWorkbook("../SOPs and Templates/ODIN/ODIN_random_station_tracking.xlsx", sheet = "BySection") %>%
  mutate(StartDate = as.Date(StartDate, origin = "1899-12-30"),
         EndDate = as.Date(StartDate, origin = "1899-12-30"),
         CreationDate = as.Date(StartDate, origin = "1899-12-30"),
         AddDate = as.Date(StartDate, origin = "1899-12-30"))
head(Track_Section)
#
#Append data row
Track_Site <- bind_rows(Track_Site, ODIN_site_summ)
Track_Section <- bind_rows(Track_Section, ODIN_section_summ)
#
#Write updated data frame to existing worksheets
wb <- loadWorkbook("../SOPs and Templates/ODIN/ODIN_random_station_tracking.xlsx")
writeData(wb, "BySite", Track_Site)
writeData(wb, "BySection", Track_Section)
#
Track_Site
Track_Section
#
#To overwrite the pre-existing data file with the updated data, remove the # and run following line
#ONLY RUN AFTER CHECKING DATA!
#saveWorkbook(wb, "../SOPs and Templates/ODIN/ODIN_random_station_tracking.xlsx", overwrite=TRUE)
