###Converting bathymetry lines to  polygon depth shapefile
#Need to repeat for each StateGrid once. Only need to run if updating data or using a new StateGrid
#
##Working within any of the regional folders/projects. Do NOT save workspace.
#
#
#load packages, install as needed
if (!require("pacman")) install.packages("pacman")
pacman::p_load(plyr, tidyverse, #Df manipulation, basic summary
               sf, raster, spData, rgeos, rgdal, tmaptools, 
               terra, mgcv, fpc, sp, fields, interp, #mgcv - interpolation, fpc::bscan - clustering
               ggmap, ggsn, leaflet, tmap, ggpubr,
               install = TRUE) #Mapping and figures
#
#
#Update to StateGrid. Only one at a time can be completed. Change and rerun for additional StateGrids
State_Grid <- c("F3")
#
#
#
#####Load files to work with####
#
#Load StateGrid 
MicroGrid <- as(st_read(paste0("../Base Layers/MicroGrids/Florida_MicroGrid_WGS84_",State_Grid,"_clip.shp")), "Spatial")
head(MicroGrid)
plot(MicroGrid)
#
###Load bathymetry line file
Depth <- as(st_read("../Base Layers/Bathymetry/Bathymetry_Contours_Southeast_United_States.shp"), "Spatial")
plot(Depth[6])
#
#
##Limit to desired StateGrid area
Depth_SG <- crop(Depth, extent(MicroGrid))
plot(Depth_SG)
#
#
#
#####Add StateGrid Boundary to depth data####
#
#Convert boundary of StateGrid to polygon
Depth_border <- st_sfc(st_polygon(list(rbind(c(bbox(MicroGrid)[1,1], bbox(MicroGrid)[2,2]), #NorthWest
                                             c(bbox(MicroGrid)[1,2], bbox(MicroGrid)[2,2]), #NorthEast
                                             c(bbox(MicroGrid)[1,2], bbox(MicroGrid)[2,1]), #SouthWest
                                             c(bbox(MicroGrid)[1,1], bbox(MicroGrid)[2,1]), #SouthEast
                                             c(bbox(MicroGrid)[1,1], bbox(MicroGrid)[2,2]))))) 
#
#Convert boundary polygon to SpatialLinesDataFrame to add to depth data
Depth_border_line <- Depth_border %>% st_cast("MULTILINESTRING") %>% st_cast("LINESTRING") %>%
  st_as_sf() %>% mutate(OBJECTID = as.integer(0000),
                        DATASET = "border",
                        SOURCE = "na",
                        RES = "na",
                        DEPTH_FT = 0,
                        DEPTH_M = 0,
                        DEPTH_FA = 0,
                        last_edite = "na") %>% as("Spatial")
proj4string(Depth_border_line) <- CRS("+proj=longlat +datum=WGS84") #Update CRS 
#
#Add depth and boundary lines together
Depth_all <- rbind(Depth_SG, Depth_border_line)
plot(Depth_all)
#
#Cast lines to points
Depth_all_pts <- st_as_sf(Depth_all) %>% st_cast("POINT") %>% #Needs lines to be points for interpolation
  drop_na(DEPTH_M) %>% st_transform(32617)
plot(Depth_all_pts[6])
#
#
#
####Interpolate depth lines to whole area####
#
#Convert boundary to meters
MicroGrid_box_m <- as.matrix(bb(extent(MicroGrid), current.projection = 4326, projection = 32617)) #W, S, E, N
#
#create blank raster to fill with placeholder value (meters) - change ncols and nrows as needed
(tGrid <- rast(resolution = c(165, 185),
               xmin = MicroGrid_box_m[1], xmax = MicroGrid_box_m[3], ymin = MicroGrid_box_m[2], ymax = MicroGrid_box_m[4], 
               crs = "+init=EPSG:32617"))
#
#Convert depth points to vector to rasterize over grid
Depth_vec <- vect(Depth_all_pts)
Depth_ras <- terra::rasterize(Depth_vec, tGrid, field = "DEPTH_M")
#
##Interpolate depth values using thin plate spline - may take a little while to run
fit_T <- interpolate(tGrid, Tps(xyFromCell(Depth_ras, 1:ncell(Depth_ras)),
                                values(Depth_ras)))
#
plot(fit_T)
#
#Convert to polygons, rename column, reproject to WGS84
temp_poly_m <- as.polygons(fit_T)#, crs = "EPSG:32617", round = TRUE, aggregate = TRUE, values = TRUE, digits = 1) 
names(temp_poly_m) <- c("Depth")
temp_poly <- project(temp_poly_m, "EPSG:4326")
plot(temp_poly)
#
#Save data output as shapefile
writeVector(temp_poly, 
            paste0("../Base Layers/Bathymetry/", State_Grid,"_depth.shp"),
            filetype = "ESRI Shapefile", layer = "Depth",
            overwrite = TRUE)
#
#
#
# 