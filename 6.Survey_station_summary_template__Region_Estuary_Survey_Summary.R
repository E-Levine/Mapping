####Template for summarizing completed oyster surveys by grid cells and polygons
####Save file name using "Region ... Summary" - change 'Region' and 'Estuary' to correct location
####Update the Region, Estuary/Site Code, and StateGrids in lines 25-28 (lines 21-24 after removing template header). 
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