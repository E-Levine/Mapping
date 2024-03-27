####Water Quality Data Compilation###
#
##Compile WQ data from WQ Portal - Select parameters, combine station and WQ data
##Output of cleaned data
#
#
#Use Alt+O to collapse all sections, Alt+Shift+O to expand all sections
#
#Load require packages (install as necessary)
if (!require("pacman")) install.packages("pacman")
pacman::p_unlock()
pacman::p_load(plyr, tidyverse, readxl, #Df manipulation, basic summary
               ggmap, tibble, zoo, measurements,
               install = TRUE) 
#
##Set StateGrid working within, and start and end years of data
State_Grid <- c("F4")
Start_year <- c("2012")
Mid_year <- c("2017")
End_year <- c("2022")
#
####Load files####
#
##Read in Excel site file
Location_data <- as.data.frame(read_excel(paste0("Raw_data/", State_Grid,"_Site data_", Start_year, "_", End_year,".xlsx"), 
                          na = c("NA", " ", "", "Z")))
#
#Read in Excel results file (for 1 file) - skip to next section if only 1 resutls file
Results_data <- as.data.frame(read_excel(paste0("Raw_data/", State_Grid,"_Results_", Start_year, "_", End_year,".xlsx"), 
                                          na = c("NA", " ", "", "Z")))
#Read in Excel results file (for 2 files)
Results1 <- as.data.frame(read_excel(paste0("Raw_data/", State_Grid,"_Results_", Start_year, "_", Mid_year,".xlsx"), 
                                         na = c("NA", " ", "", "Z")))
Results2 <- as.data.frame(read_excel(paste0("Raw_data/", State_Grid,"_Results_", (as.numeric(Mid_year)+1) , "_", End_year,".xlsx"), 
                                     na = c("NA", " ", "", "Z")))
Results_data <- rbind(Results1, Results2)
#
#
#
####Select data columns####
#
##Select location data
#
#List of columns to keep from original file 
keep_site <- c("MonitoringLocationIdentifier", "OrganizationIdentifier", "OrganizationFormalName",
               "MonitoringLocationName", "MonitoringLocationTypeName", "MonitoringLocationDescriptionText",
               "LatitudeMeasure", "LongitudeMeasure", "HorizontalCoordinateReferenceSystemDatumName", "StateCode",
               "CountyCode", "ProviderName")
#
#Subset columns and add "StateGrid" column
Location_data <- Location_data[keep_site] %>% 
  add_column(StateGrid = State_Grid, .before = "MonitoringLocationIdentifier")
#Check columns
head(Location_data, 4)
#
#
#
##Select Results data
#
#Subset df by columns to keep - change list as needed
keep_results <- c("MonitoringLocationIdentifier", "ResultIdentifier", "ActivityStartDate", 
                  "ActivityStartTime/Time", "ActivityStartTime/TimeZoneCode", "CharacteristicName", 
                  "ResultMeasureValue", "ResultMeasure/MeasureUnitCode")
Results <- Results_data[keep_results]
#
#Confirm desired columns and naming
head(Results, 4)
#
#
#
####Combine data by station - limit to desired parameters####
#
Combined <- merge(Location_data, Results, by = "MonitoringLocationIdentifier")
#
###Filter combined file to only include specified characteristics 
#List of possible characteristics to select from
unique(Combined$CharacteristicName)
#
#Assemble list of characteristics to KEEP
Characters <- c("Salinity", "Temperature, water", "Depth, bottom", "Depth, Secchi disk depth",  
                "Temperature, air, deg C", "Turbidity", "Nitrate + Nitrite", "Inorganic nitrogen (nitrate and nitrite)", 
                "Depth", "Organic Nitrogen", "Conductivity", "Specific conductance", "pH", "Dissolved oxygen (DO)", 
                "Dissolved oxygen saturation", "Chlorophyll a, corrected for pheophytin", "Chlorophyll a, uncorrected for pheophytin", 
                "Chlorophyll a", "Total dissolved solids", "Total suspended solids", "Zooplankton", "Diatoms", 
                "Stream flow, instantaneous", "Flow, severity (choice list)", "Stream stage", "Flow", "Stream flow, mean. Daily")
#
#Filter to only the desired characteristics and check remaining list
Combined_filtered <- Combined %>% filter(CharacteristicName %in% Characters)
#
##Correct basic typos in units/provide clarification - mg/L, mg/m3 = ug/L
Combined_filtered$`ResultMeasure/MeasureUnitCode` <- str_replace(Combined_filtered$`ResultMeasure/MeasureUnitCode`, "mg/l", "mg/L")
Combined_filtered$`ResultMeasure/MeasureUnitCode` <- str_replace(Combined_filtered$`ResultMeasure/MeasureUnitCode`, "mg/m3", "ug/L")
Combined_filtered$`ResultMeasure/MeasureUnitCode` <- str_replace(Combined_filtered$`ResultMeasure/MeasureUnitCode`, "ug/l", "ug/L")
Combined_filtered$`ResultMeasure/MeasureUnitCode` <- str_replace(Combined_filtered$`ResultMeasure/MeasureUnitCode`, "ft3/sec", "cfs")
#
#Confirm list of characters selected.
unique(Combined_filtered$CharacteristicName)
#
#
#
#
####Clean parameter data####
#
#
#
Combined_filtered <- Combined_filtered %>% 
  mutate(ResultMeasureValue = as.numeric(ifelse(CharacteristicName == "Specific conductance" & 'ResultMeasure/MeasureUnitCode' == "mS/cm", #Convert Spec Cond mS to uS
                                                ResultMeasureValue*1000, 
                                                ifelse(CharacteristicName == "Stream flow, instantaneous" & 'ResultMeasure/MeasureUnitCode' == "ft3/s", #Convert ft3 to m3
                                                       ResultMeasureValue*0.0283168, ResultMeasureValue))),
         'ResultMeasure/MeasureUnitCode' = ifelse(CharacteristicName == "Salinity", "ppt",  #Change all Salinity values to 'ppt' units
                                                  ifelse(CharacteristicName == "Conductivity", "uS/cm", #Correct all Conductivity results
                                                         ifelse(CharacteristicName == "Specific conductance", "uS/cm",#Correct Specific conductance units
                                                                ifelse(CharacteristicName == "pH", NA,  #Correct pH units
                                                                       ifelse(CharacteristicName == "Stream flow, instantaneous", "m3/s", #Correct Stream flow units
                                                                              Combined_filtered$'ResultMeasure/MeasureUnitCode'))))))
#
head(Combined_filtered)
#
#
#
#
####Save filtered data####
#
#
write.csv(Combined_filtered, 
          file = paste0("Cleaned_data/",State_Grid,"_combined_filtered_", Start_year, "_", End_year,".csv"), 
          row.names = FALSE)

#
#