####Template for Estuary MicroGrid Station Selection
####Update the Region, Estuary/Site Code, and StateGrids in lines 25-28 (lines 21-24 after removing template header). 
####Delete lines 1-3 when saving as new working file.
#
###Randomized Survey Station Selection
#
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
Region <- c("SouthEast")
Site_Code <- c("SLE")
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
#Year or timeperiod of survey to be completed
Survey_year <- c("2023")
Survey_timeperiod <- 05 #Change to Month of Survey (##) or Quarter (c("Spring/Summer/Fall/Winter")) of survey
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
#HSM data
HSM_data <-  read.csv(paste0("Output_data/", Site_Code, "_MicrogridData_HSM_", Start_year,"_", End_year, ".csv"), 
                      na.string = c("Z", "", "NA"))
head(HSM_data)
#Merge (If skipping HSM data, run line 62)
Site_data <- left_join(Site_df, HSM_data %>% dplyr::select(MGID, Salinity_HSI:HSM_Score))
#Site_data <- Site_df
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
Monitoring <- NA #Run if not including Monitoring stations
Monitoring_locations <- "NA" #Either "Y" to include in maps, or "NA" to not include
#
#Completed survey stations 
Comp_Stations <- read.csv(paste0("Output_Data/", Site_Code, "_Station_MGIDs.csv"), na.string = c("Z", "", "NA")) %>%
  rename("MGID" = Confirmed_MGID, "Lat_DD_Y" = Latitude, "Long_DD_X" = Longitude)
head(Comp_Stations)
#
#
#
####Station selection parameter and specification setup####
#
#Final combined data frame
All_data <- left_join((Site_Grid %>% dplyr::select(-Site, -Section, -SHA_Name, -SHA_Class, -Subsection, -Bathy_m)),
                      Site_data %>% dplyr::select(MGID, Site:last_col())) %>%
  mutate(Seagrass = ifelse(is.na(Seagrass), "Unk", Seagrass))
#
head(All_data)
#
#
###Data filters: Oyster GIS Layer presence, SHA Class, Depth, Seagrass presence, HSM score, previous data 
#If data should be used to filter, enter desired data/range in following lines. If data is not used or specific entries are required (i.e. SHA class, Section), enter NA.\
Selection_process <- c("Ordered") #Will stations be chosen by rank/value of qualifiers ("Ordered"), by random selection within area ("Random"), or by specified boundary ("B_box")
Station_selection <- c("Section") #Should cells be selected within the entire estuary ("Site"), within each estuary section ("Section"), or within specific Sections or boundary ("NA")
Oyster_Layer <- c("Y") #Should cells with oysters in the GIS layer be prioritize ("Y"), without oysters be prioritized ("N"), or NA
SHA_classification <- c("N") #Should SHA classification be used for selection "Y" or "N"
SHA_grouping <- c("N") #Should SHA class be used for grouping to select stations (i.e. group_by(Section) = "N", group_by(Section, SHA) = "Y")
Depth_range <- c(0, 3) #Minimum and maximum depth for selections (inclusive), or "NA" for no depth limitations
Seagrass_presence <- c("NA") #Should presence of seagrass exclude cells ("Y"), include cells ("N"), or NA
HSM_scoring <- c(NA) #Cut off value for HSM scores below which cells are excluded, or NA
Existing_survey_data <- c("E") #Should cells with existing data (survey stations) be excluded ("E"), selected for ("I"), or ignored ("N")
#
#If stations should only come from one Section and should be assigned proportionally within SHA class, Station_selection = NA and SHA_grouping = Y
#
##Data specifications: Selection of stations from specific/limited SHA class or Section (If using, SHA_classification and/or Station_Selection should be NA above)
Target_SHA  <- c("AP", "CA", "CR", "PD", "UN") #list of SHA classes to include
Target_Sections <- c("N", "S") #Enter section code for all sections desired or "NA" if want all sections.
#
##Survey specifications
#Num_[]:Number of stations per site/section if require specific number per group - Can replace with NA if #/section varies
Num_Target <- 20
Num_Extra <- 15
#Num_[]_Total: Number of stations total if number per group can vary, proportion of selected cells to select from total per group
Num_Target_Total <- 100
Num_Extra_Total <- 100*0.5
Prop_required <- c(0.5) #Proportion required for random selection - if specifying number T/E (lines 117/118), then NA
#
##Boundary of selection - limiting coordinate in each direction - West, East, South, North
Boundary <- c(-82.44655, -82.53977, 27.66694, 27.72659)
Include_name <- c("Y") #Should a specific name be included in file naming (to identify bounded area)? Y/N
B_name <- c("PRassa_TarponB") #What name should be used?
#
#
#
#
####Data filtering####
#
###View data area
qtm(All_data, fill = "Depth")
#
##Ordered selection will select the number of stations specified in lines 114-115
##Random selection will select proportion number of target stations and half as many extras stations. Cannot select for highest HSM scores in random selection.
if(Selection_process == "Ordered"){
  #
  #Strict selection based on all specified parameters
  (temp <- All_data %>% 
     #Stations by Site or within every Section
     {if(Station_selection == "Section") group_by(., Section) else if (Station_selection == "Site") group_by(., Site) else if (Station_selection == "NA") filter(., Section %in% Target_Sections) %>% group_by(., Section)} %>%
     #Filter by Oyster GIS layer
     filter(if(Oyster_Layer == "Y")  FL_Oysters == "Y" else if (Oyster_Layer == "N") FL_Oysters == "N" else FL_Oysters == FL_Oysters) %>%
     #Filter by SHA classification
     filter(if(SHA_classification == "Y") (Subsection %in% Target_SHA) else MGID == MGID) %>%
     #Filter by depth
     filter(if(is.numeric(Depth_range)) (Depth >= Depth_range[1] & Depth <= Depth_range[2]) else MGID == MGID) %>%
     #Filter by seagrass
     filter(if(Seagrass_presence == "N") !(Seagrass == "Continuous" & Seagrass == "Discontinuous") else if (Seagrass_presence == "Y") (Seagrass == "Continuous" | Seagrass == "Discontinuous") else Seagrass == Seagrass) %>%
     #Filter by HSM score
     filter(if(is.na(HSM_scoring)) MGID == MGID else HSM_Score >= HSM_scoring)%>%
     #Filter by existing data 
     filter(if(Existing_survey_data == "E") !(MGID %in% Comp_Stations$MGID) else if(Existing_survey_data == "I") (MGID %in% Comp_Stations$MGID) else (MGID == MGID))
  )
  print(tm_shape(temp) + tm_polygons(col = "Depth") + tm_shape(FL_outline) + tm_polygons(col = "gray"))
  
  #Assign station numbers
  t <- temp %>% 
    {if(Station_selection == "Site") group_by(., Site) else group_by(., Section)} %>% #Group by estuary site or section
    {if(SHA_grouping == "Y") group_by(., Subsection) else group_by(., Section)} %>% #Group by estuary site or section
    {if(!is.na(HSM_scoring)) arrange(., desc(HSM_Score)) else (.)} %>% #Arrange in order of decreasing HSM score if included in selection process
    mutate(Station = 1:n()) #Assign numbers by order within group
  #
  #
  #Collect target and extra stations
  Target <- t[t$Station < Num_Target+1,] %>% mutate(Type = "Target")
  Extra <- t[t$Station > Num_Target & t$Station < (Num_Target + Num_Extra + 1),]  %>% mutate(Type = "Extra")
  #
  #Compile into final data set 
  Stations_selected <- rbind(Target, Extra) %>% dplyr::select(Type, Section, Station, MGID:HSM_Score, geometry) %>% 
    {if(Station_selection == "Site") group_by(., Section) %>% arrange(., Station, .by_group = FALSE) else arrange(., Station, .by_group = TRUE)}
  head(Stations_selected)
} else if(Selection_process == "B_box"){
  (tempb <- All_data %>% 
     #Restrict grid to within specified boundary
     subset(Long_DD_X > Boundary[1] & Long_DD_X < Boundary[2] & Lat_DD_Y > Boundary[3] & Lat_DD_Y < Boundary[4]) %>%
     #Stations by Site or within every Section
     {if(Station_selection == "Section") group_by(., Section) else if (Station_selection == "Site") group_by(., Site) else if (Station_selection == "NA" & Selection_process != "B_box") filter(., Section %in% Target_Sections) %>% group_by(., Section) else if (Station_selection == "NA" & Selection_process == "B_box") group_by(., Section)} %>%
     #Filter by Oyster GIS layer
     filter(if(Oyster_Layer == "Y")  FL_Oysters == "Y" else if (Oyster_Layer == "N") FL_Oysters == "N" else FL_Oysters == FL_Oysters) %>%
     #Filter by SHA classification
     filter(if(SHA_classification == "Y") (Subsection %in% Target_SHA) else MGID == MGID) %>%
     #Filter by depth
     filter(if(is.numeric(Depth_range)) (Depth >= Depth_range[1] & Depth <= Depth_range[2]) else MGID == MGID) %>%
     #Filter by seagrass
     filter(if(Seagrass_presence == "N") !(Seagrass == "Continuous" & Seagrass == "Discontinuous") else if (Seagrass_presence == "Y") (Seagrass == "Continuous" | Seagrass == "Discontinuous") else Seagrass == Seagrass) %>%
     #Filter by HSM score
     filter(if(is.na(HSM_scoring)) MGID == MGID else HSM_Score >= HSM_scoring)%>%
     #Filter by existing data 
     filter(if(Existing_survey_data == "E") !(MGID %in% Comp_Stations$MGID) else if(Existing_survey_data == "I") (MGID %in% Comp_Stations$MGID))
  )
  print(tm_shape(tempb) + tm_polygons(col = "Depth") + tm_shape(FL_outline) + tm_polygons(col = "gray"))
  
  #Assign station numbers
  tb <- tempb %>% 
    {if(Station_selection == "Site") group_by(., Site) else group_by(., Section)} %>% #Group by estuary site or section
    {if(SHA_grouping == "Y") group_by(., Subsection) else group_by(., Section)} %>% #Group by estuary site or section
    {if(!is.na(HSM_scoring)) arrange(., desc(HSM_Score)) else (.)} %>% #Arrange in order of decreasing HSM score if included in selection process
    slice(sample(1:n())) %>% #Randomize within specified boundary
    mutate(Station = 1:n()) #Assign numbers by order within group
  #
  #
  #Collect target and extra stations
  Target_b <- tb[tb$Station < Num_Target+1,] %>% mutate(Type = "Target")
  Extra_b <- tb[tb$Station > Num_Target & tb$Station < (Num_Target + Num_Extra + 1),]  %>% mutate(Type = "Extra")
  #
  #Compile into final data set 
  Stations_selected <- rbind(Target, Extra) %>% dplyr::select(Type, Section, Station, MGID:HSM_Score, geometry) %>% 
    {if(Station_selection == "Site") group_by(., Section) %>% arrange(., Station, .by_group = FALSE) else arrange(., Station, .by_group = TRUE)}
  head(Stations_selected)
} else {
  (temp_r <- All_data %>% 
     #Stations by Site or within every Section
     {if(Station_selection == "Section") group_by(., Section) else if (Station_selection == "Site") group_by(., Site) else if (Station_selection == "NA") filter(., Section %in% Target_Sections) %>% group_by(., Section)} %>%
     #Filter by Oyster GIS layer
     filter(if(Oyster_Layer == "Y")  FL_Oysters == "Y" else if (Oyster_Layer == "N") FL_Oysters == "N" else FL_Oysters == FL_Oysters) %>%
     #Filter by SHA classification
     filter(if(SHA_classification == "Y") (Subsection %in% Target_SHA) else MGID == MGID) %>%
     #Filter by depth
     filter(if(is.numeric(Depth_range)) (Depth >= Depth_range[1] & Depth <= Depth_range[2]) else MGID == MGID) %>%
     #Filter by seagrass
     filter(if(Seagrass_presence == "N") !(Seagrass == "Continuous" & Seagrass == "Discontinuous") else if (Seagrass_presence == "Y") (Seagrass == "Continuous" | Seagrass == "Discontinuous") else Seagrass == Seagrass) %>%
     #Filter by HSM score
     filter(if(is.na(HSM_scoring)) MGID == MGID else HSM_Score >= HSM_scoring) %>%
     #Filter by existing data 
     filter(if(Existing_survey_data == "E") !(MGID %in% Comp_Stations$MGID) else if(Existing_survey_data == "I") (MGID %in% Comp_Stations$MGID) else (MGID == MGID)) %>%
     rowwise() %>%  mutate(Group = ifelse(SHA_grouping == "Y", paste(Section, Subsection, sep = "-"), Section))
  )
  #
  print(tm_shape(temp_r) + tm_polygons(col = "Depth") + tm_shape(FL_outline) + tm_polygons(col = "gray"))
  #
  #Determine number of stations per group
  (Stations_needed <- if(Station_selection == "Section" & SHA_grouping == "Y") {
    count(as.data.frame(temp_r), Section, Subsection) %>% 
      mutate(Possible_cells = nrow(temp_r), 
             x = (if(is.na(Prop_required)) Num_Target else round(n*Prop_required,0)), 
             y = (if(is.na(Prop_required)) Num_Extra else round(n*Prop_required*0.5,0)),
             n_Stations = x+y,
             Total = sum(n_Stations),
             Code = paste(Section, Subsection, sep = "-")) %>%
      rename(!!paste0((if(is.na(Prop_required)) Num_Target else Prop_required),"_Target") := x, !!paste0((if(is.na(Prop_required)) Num_Extra else Prop_required*0.5), "_Extra") := y)
  } 
    else if (Station_selection == "NA" & SHA_grouping == "Y") {
      count(as.data.frame(temp_r), Group) %>% 
        mutate(Possible_cells = nrow(temp_r), 
               x = (if(is.na(Prop_required)) Num_Target else round(n*Prop_required,0)), 
               y = (if(is.na(Prop_required)) Num_Extra else round(n*Prop_required*0.5,0)),
               n_Stations = x+y,
               Total = sum(n_Stations),
               Code = paste(Group)) %>%
        rename(!!paste0((if(is.na(Prop_required)) Num_Target else Prop_required),"_Target") := x, !!paste0((if(is.na(Prop_required)) Num_Extra else Prop_required*0.5), "_Extra") := y)
  } else {
      count(as.data.frame(temp_r), Section) %>% 
        mutate(Possible_cells = nrow(temp_r), 
               x = (if(is.na(Prop_required)) Num_Target else round(n*Prop_required,0)), 
               y = (if(is.na(Prop_required)) Num_Extra else round(n*Prop_required*0.5,0)),
               n_Stations = x+y,
               Total = sum(n_Stations),
               Code = Section) %>%
        rename(!!paste0(Prop_required*100,"%_Target") := x, !!paste0(Prop_required*0.5*100, "%_Extra") := y)
    }) 
  #
  #Assign random numbers per group
  t_r <- temp_r %>% 
    {if(Station_selection == "Site") group_by(., Site) else group_by(., Section)} %>% #Group by estuary site or section
    {if(SHA_grouping == "Y" & Station_selection == "Section") group_by(., Section, Subsection) else if (Station_selection == "NA") group_by(., Subsection) else (.)} %>% #Group by section or section and SHA class
    {if(!is.na(HSM_scoring)) arrange(., desc(HSM_Score)) else (.)} %>% #Arrange in order of decreasing HSM score if included in selection process
    mutate(rand_num = sample.int(n())) #Assign numbers by order within group
  #
  #Selection stations 
  Stations_selected = data.frame() #Empty dataframe to fill
  #Select target and extra stations per group and append to dataframe
  for(i in unique(t_r$Group)){
    Target_r <- t_r %>% filter(Group == i) %>% arrange(rand_num) %>%
      head(as.integer((filter(Stations_needed, Code == i))[,ncol(Stations_needed)-4])) %>%
      mutate(Type = "Target") %>% rename("Station" = rand_num)
    Extra_r <- t_r %>% filter(Group == i) %>% arrange(rand_num) %>%
      subset(rand_num > as.integer((filter(Stations_needed, Code == i))[,ncol(Stations_needed)-4]) & rand_num <= as.integer((filter(Stations_needed, Code == i))[,ncol(Stations_needed)-2])) %>%
      mutate(Type = "Extra") %>% rename("Station" = rand_num)
    #
    Stations_selected <- rbind(Stations_selected, rbind(Target_r, Extra_r)) %>%
      {if(SHA_classification == "Y") group_by(., Section, SHA_Class) else group_by(., Section)} %>% 
      arrange(Station, .by_group =  TRUE)
  }
  head(Stations_selected)
}
#
#
#
#
####Save station Excel files####
#
#Excel file will be saved with metadata, all raw data, and cleaned data for field sheets. Code will overwrite previous files with same survey location, time period, and year.
#
#Selection summary
(Survey_summary <- data.frame("Metric" = c("Region", "Site", "State grid", "Alt grid", "Grid data compiled",
                                           "WQ year range", "Survey",
                                           "Selected by",
                                           "Station IDs",
                                           "Oyster layer",
                                           "SHA classification",
                                           "SHA use in groups",
                                           "Depth range",
                                           "Seagrass",
                                           "HSM scores",
                                           "Existing survey data",
                                           "Station groups"),
                              "Evaluation" = c(Region, Site_Code, State_Grid, ifelse(exists("Alt_State_Grid"), Alt_State_Grid, "NA"), Date,
                                               paste(Start_year, End_year, sep = "-"), paste(if(is.numeric(Survey_timeperiod)) {month.abb[Survey_timeperiod]} else {Survey_timeperiod}, Survey_year, sep = " "),
                                               ifelse(Selection_process == "Ordered", "Ranked values", "Random"),
                                               ifelse(Station_selection == "NA" & Selection_process == "B_box", paste0("Station IDs assigned within specified boundary: ", paste(sapply(Boundary, paste, collapse = ""), collapse = ", ")), ifelse(Station_selection == "NA" & Selection_process != "B_box", paste0("Station IDs assigned within: ", paste(sapply(Target_Sections, paste, collapse = ""), collapse = ", ")), paste0("Station IDs assigned within: ", Station_selection))),
                                               ifelse(Oyster_Layer == "Y", "Oyster presence selected for", ifelse(Oyster_Layer == "N", "Oyster presence selected against", "Oyster layer ignored")),
                                               ifelse(SHA_classification == "Y", paste("SHA classes included:", paste(sapply(Target_SHA, paste, collapse = ""), collapse = ", "), collapse =  " "), "SHA classes ignored"),
                                               ifelse(SHA_grouping == "Y", "SHA class used to group stations for selection", "SHA not used to group stations for selection"),
                                               paste0("Depth limited to: ", Depth_range[1], "-", Depth_range[2], "m"),
                                               ifelse(Seagrass_presence == "Y", "Exclusion of cells with seagrass", ifelse(Seagrass_presence == "N", "Inclusion of cells with seagrass", "Segrass ignored")),
                                               ifelse(is.numeric(HSM_scoring), paste0("Minimum HSM value included: ", HSM_scoring), "HSM scores ignored"),
                                               ifelse(Existing_survey_data == "Y", "Stations previously surveyed excluded", "All stations allowed"),
                                               ifelse(Selection_process == "B_box", paste("Specified boundary"), ifelse(Selection_process == "Random", paste(sapply(unique(Stations_needed$Code), paste, collapse = ""), collapse = ", "), paste(sapply(unique(Stations_selected$Section), paste, collapse = ""), collapse = ", "))))))
#
#
#Save all raw data and cleaned data for output
All_raw_data <- as.data.frame(Stations_selected) %>% dplyr::select(-geometry)
Cleaned_data <- as.data.frame(Stations_selected) %>% 
  dplyr::select(Site, Section, Type, Station, MGID:Long_DD_X, FL_Oysters:Seagrass, HSM_Score, -geometry)
#
head(Cleaned_data)
#
Output_list <- list("Summary" = Survey_summary, 
                    "Raw_data" = All_raw_data,
                    "Cleaned_data" = Cleaned_data)
#
#Export to Excel
if(Include_name == "N"){
  write.xlsx(Output_list, file =  paste0("Output_Data/", Site_Code, "_Survey_Stations_", 
                                         Survey_year, "_", Survey_timeperiod,".xlsx"), 
             colnames = TRUE, rowNames = FALSE, keepNA = TRUE, na.string = c("Z"), colWidths = "auto")
} else {
  write.xlsx(Output_list, file =  paste0("Output_Data/", Site_Code, "_Survey_Stations_", 
                                         Survey_year, "_", Survey_timeperiod, "_", B_name,".xlsx"), 
             colnames = TRUE, rowNames = FALSE, keepNA = TRUE, na.string = c("Z"), colWidths = "auto")
}
#
#
#
#
####Interactive site and static section maps####
#
###Maps are created and automatically saved. Create network folder with following mapping if needed: Region/Maps/Survey/SiteCode
#Specify mapping output as either "Site", "Section", or "Box". Site and Box will output an overall map for the Site or the boundary specified, Section will output individuals maps for each section. 
#Oyster layer and SHA classifications will be added as a layer if used in station selection process.
#Depth will be added as a layer in interactive Site map but not in static Section maps
#
Map_output <- c("Section") #"Site", "Section", "Box" (boundary specified)
#
if(Map_output == "Site") {
  leaflet_map <- tm_shape(name = "Microgrid cells", All_data) + 
    tm_borders(col = "gray") + #Cell borders
    #Add oyster layer if used for selection
    {if(Oyster_Layer == "Y") tm_shape(name = "Oyster layer presence", All_data %>% subset(FL_Oysters == "Y") %>% 
                                        mutate(FL_Oysters = ifelse(FL_Oysters == "Y", "Oyster layer", FL_Oysters)))+  #Change text for legend
        tm_polygons("FL_Oysters", title = "", palette = c("viridis"), alpha = 0.4)} +
    #Add SHA classes if used for selection
    {if(SHA_classification == "Y") tm_shape(name = "SHA classification", All_data %>% filter(!is.na(SHA_Class)))+  
        tm_polygons("SHA_Class", title = "", palette = c("magma"), alpha = 0.4)} +
    #Add depth
    tm_shape(name = "Depth", All_data %>% filter(!is.na(Depth))) + tm_polygons("Depth", title = "", palette = c("YlGnBu"), alpha = 0.5) +
    #Add stations
    tm_shape(name = "Survey stations", Stations_selected %>% mutate(Type = paste0(Type, " Station")))+  
    tm_polygons("Type", title = "", palette = c("YlOrRd")) + #Add colors for Target and Extra stations - use "palette = c("red")" if only Target stations
    #Add FL shoreline
    tm_shape(name = "Shoreline", st_make_valid(FL_outline)) + tm_polygons() +
    #Add cell Station numbers
    tm_shape(name = "Station numbers", Stations_selected) + tm_text("Station", size = "AREA")+ 
    #Add monitoring stations
    {if(Monitoring_locations == "Y") tm_shape(name = "Monitoring stations", Monitor_spdf) +  tm_symbols(shape = 16, size = 0.2, col = "black", border.col = "black", alpha = 0.4)}+
    {if(Monitoring_locations == "Y") tm_add_legend('fill', col = "black", border.col = "black", labels = c("Monitoring Stations")) + tm_view(text.size.variable = TRUE)}+
    tm_layout(main.title = paste0(if(is.numeric(Survey_timeperiod)) month.abb[Survey_timeperiod] else Survey_timeperiod, " ", Survey_year, 
                                  " Survey Station Selection"), main.title.position = "center")
  #
  (Site_map <- tmap_leaflet(leaflet_map))
  #
  if(Include_name == "N"){
      saveWidget(Site_map, paste0("Maps/Survey/", Site_Code, "/", Site_Code,"_survey_stations_", Survey_year, "_", Survey_timeperiod, "_widget.html"))
    } else {
      saveWidget(Site_map, paste0("Maps/Survey/", Site_Code, "/", Site_Code,"_survey_stations_", Survey_year, "_", Survey_timeperiod, "_", B_name, "_widget.html"))
    }
} else if (Map_output == "Section"){
  #Make plots
  #map_list = list()
  for(i in unique(Stations_selected$Section)){
    leaflet_map <- tm_shape(name = "Microgrid cells", All_data %>% filter(Section == i)) + 
      tm_borders(col = "gray") + #Cell borders
      #Add oyster layer if used for selection
      {if(Oyster_Layer == "Y") tm_shape(name = "Oyster layer presence", All_data %>% subset(FL_Oysters == "Y" & Section == i) %>% 
                                          mutate(FL_Oysters = ifelse(FL_Oysters == "Y", "Oyster layer", FL_Oysters)))+  #Change text for legend
          tm_polygons("FL_Oysters", title = "", palette = c("viridis"), alpha = 0.4)} +
      #Add SHA classes if used for selection
      {if(SHA_classification == "Y") tm_shape(name = "SHA classification", All_data %>% filter(!is.na(SHA_Class) & Section == i))+  
          tm_polygons("SHA_Class", title = "", palette = c("magma"), alpha = 0.4)} +
      #Add stations
      tm_shape(name = "Survey stations", Stations_selected %>% filter(Section == i) %>% mutate(Type = paste0(Type, " Station")))+  
      tm_polygons("Type", title = "", palette = c("YlOrRd")) + #Add colors for Target and Extra stations - use "palette = c("red")" if only Target stations
      #Add FL shoreline
      tm_shape(name = "Shoreline", st_make_valid(FL_outline)) + tm_polygons() +
      #Add cell Station numbers
      tm_shape(name = "Station numbers", Stations_selected %>% filter(Section == i)) + tm_text("Station", size = 0.45)+ 
      #Add monitoring stations
      {if(Monitoring_locations == "Y") tm_shape(name = "Monitoring stations", Monitor_spdf) +  tm_symbols(shape = 16, size = 0.75, col = "black", border.col = "black", alpha = 0.4)}+
      {if(Monitoring_locations == "Y") tm_add_legend('fill', col = "black", border.col = "black", labels = c("Monitoring Stations")) + tm_view(text.size.variable = TRUE)}+
      tm_layout(main.title = paste0(if(is.numeric(Survey_timeperiod)) month.abb[Survey_timeperiod] else Survey_timeperiod, " ", Survey_year, 
                                    " ", i, " Survey Station Selection"),
                main.title.position = "center")
    #
    if(Include_name == "N"){
      tmap_save(leaflet_map, file = paste0("Maps/Survey/", Site_Code, "/", Site_Code, "_", i, "_survey_stations_", Survey_year, "_", Survey_timeperiod, ".jpg"),
                dpi = 1000)
    } else {
      tmap_save(leaflet_map, file = paste0("Maps/Survey/", Site_Code, "/", Site_Code, "_", i, "_survey_stations_", Survey_year, "_", Survey_timeperiod, "_", B_name, ".jpg"),
                dpi = 1000)
    }
  }
} else if (Map_output == "Box"){
  leaflet_map <- tm_shape(name = "Microgrid cells", All_data %>% subset(Long_DD_X > Boundary[1] & Long_DD_X < Boundary[2] & Lat_DD_Y > Boundary[3] & Lat_DD_Y < Boundary[4])) + 
    tm_borders(col = "gray") + #Cell borders
    #Add oyster layer if used for selection
    {if(Oyster_Layer == "Y") tm_shape(name = "Oyster layer presence", All_data %>% subset(FL_Oysters == "Y") %>% 
                                        mutate(FL_Oysters = ifelse(FL_Oysters == "Y", "Oyster layer", FL_Oysters)))+  #Change text for legend
        tm_polygons("FL_Oysters", title = "", palette = c("viridis"), alpha = 0.4)} +
    #Add SHA classes if used for selection
    {if(SHA_classification == "Y") tm_shape(name = "SHA classification", All_data %>% filter(!is.na(SHA_Class)))+  
        tm_polygons("SHA_Class", title = "", palette = c("magma"), alpha = 0.4)} +
    #Add depth
    tm_shape(name = "Depth", All_data %>% filter(!is.na(Depth))) + tm_polygons("Depth", title = "", palette = c("YlGnBu"), alpha = 0.5) +
    #Add stations
    tm_shape(name = "Survey stations", Stations_selected %>% mutate(Type = paste0(Type, " Station")))+  
    tm_polygons("Type", title = "", palette = c("YlOrRd")) + #Add colors for Target and Extra stations - use "palette = c("red")" if only Target stations
    #Add FL shoreline
    tm_shape(name = "Shoreline", st_make_valid(FL_outline)) + tm_polygons() +
    #Add cell Station numbers
    tm_shape(name = "Station numbers", Stations_selected) + tm_text("Station", size = "AREA")+ 
    #Add monitoring stations
    {if(Monitoring_locations == "Y") tm_shape(name = "Monitoring stations", Monitor_spdf) +  tm_symbols(shape = 16, size = 0.2, col = "black", border.col = "black", alpha = 0.4)}+
    {if(Monitoring_locations == "Y") tm_add_legend('fill', col = "black", border.col = "black", labels = c("Monitoring Stations")) + tm_view(text.size.variable = TRUE)}+
    tm_layout(main.title = paste0(if(is.numeric(Survey_timeperiod)) month.abb[Survey_timeperiod] else Survey_timeperiod, " ", Survey_year, 
                                  " Survey Station Selection"), main.title.position = "center")
  #
  (Site_map <- tmap_leaflet(leaflet_map))
  #
  if(Include_name == "N"){
    saveWidget(Site_map, paste0("Maps/Survey/", Site_Code, "/", Site_Code,"_survey_stations_Bbox_", Survey_year, "_", Survey_timeperiod, "_widget.html"))
  } else {
    saveWidget(Site_map, paste0("Maps/Survey/", Site_Code, "/", Site_Code,"_survey_stations_Bbox_", Survey_year, "_", Survey_timeperiod, "_", B_name, "_widget.html"))
  }
}
#
#
#
#
