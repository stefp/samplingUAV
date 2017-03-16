# Sampling design NZ function
#
# Given a boundary shapefile of a forest area of interest the code generates a point shapefile
# with the plot positions (at the moment only a systematic design is implemented )and 
# a grid shapefile (with cells of same area of the plots) to clip the RS data.
# 
# Author: Stefano Puliti
# Date  : 16/03/2017
#####################################################################################################
# arguments function

# Working directories
# inWD        =  path to directory with boundary shapefile of the area of interest (only the population of interest)
# outWD       = directory where point shapefile of sample plots will be exported

# Parameters for the sampling design
# n.plots     = number of desired field plots
# size.plots  = size in m2 of each field plots (this code assumes circular fixed-area plots)
# design      = sampling design. For now only "systematic" is implemented.
# gridCells   = logical, if TRUE then the area is tessellated into grid cells that can be composed  by squares
#######################################################################################################

samplePlotLocations <- function(inWD, outWD, n.plots, size.plots, design, gridCells){
                                # load required libraries
                                library(rgeos)
                                library(rgdal)
                                library(sp)
                                library(Grid2Polygons)
                                # load area of interest (AOI)
                                name.shape  <- tools::file_path_sans_ext(list.files(inWD, pattern = "*.dbf"))
                                AOI         <- readOGR(inWD, name.shape) # reads the boundary shapefile of the AOI 
                                radius.plot <- sqrt(size.plots/pi)
                                AOI.inner   <-gBuffer(AOI, width = -radius.plot) # creates an inner buffer of a distance = to the plot radius. This is created in order to avoid having plots on stand borders.
                                
                                # define plot locations (in the function used the origin of the sampling grid is random, hence we can consider it as a probability sample)
                                plot.locations <-  {
                                  if(design =="systematic"){spsample(AOI.inner, n.plots, type = "regular", bb = bbox(AOI.inner))}
                                  else if (design =="SRSWOR") {spsample(AOI.inner, n.plots, type = "random", bb = bbox(AOI.inner))}}
                                # generate data frame with plot coordinates
                                coords                         <- as.data.frame(coordinates(plot.locations))
                                coords$plotID                  <- seq(1:length(plot.locations))
                                colnames(coords)               <- c("x","y", "plotID")
                                plot.locations.DF              <- SpatialPointsDataFrame(plot.locations,coords)
                                proj4string(plot.locations.DF) <- proj4string(AOI)
                                # visualize the field plot design
                                plot(AOI)
                                plot(AOI.inner, col="red", add=T)
                                plot(plot.locations, add=T)
                                # export field plot locations
                                writeOGR(plot.locations.DF,outWD,paste("plot_locations",n.plots, size.plots,design, sep="_"), driver="ESRI Shapefile")

                                # generate grids
                               if(gridCells =="TRUE"|gridCells =="T"){
                                  bb <- bbox(AOI)
                                  cs <- rep(sqrt(size.plots),2)  # cell size 
                                  cc <- bb[, 1] + (cs/2)         # cell offset
                                  cd <- ceiling(diff(t(bb))/cs)  # number of cells per direction
                                  grd <- GridTopology(cellcentre.offset=cc, cellsize=cs, cells.dim=cd)
                                  sp_grd <- SpatialGridDataFrame(grd, data= data.frame(seq(1:length(SpatialGrid(grd)))),proj4string=CRS(proj4string(AOI)))
                                  grd2pol <- Grid2Polygons(sp_grd)
                                  intersect <-  gIntersection(AOI,grd2pol,byid = TRUE, drop_lower_td = TRUE)
                                  sp_grd <- SpatialPolygonsDataFrame(intersect,data=data.frame(coordinates(intersect)))
                                  sp_grd@data$Area <- sapply(slot(intersect, "polygons"), slot, "area")
                                  # visualize the grid 
                                  plot(sp_grd , add=T)
                                  # write grid shapefile 
                                  writeOGR(sp_grd,outWD,paste("grid_cells",size.plots, sep="_"), driver="ESRI Shapefile")
                               }
                        }

# example usage:
inWD <- "C:/Documents/.../in"
outWD <- "C:/Documents/.../out"

samplePlotLocations(inWD,outWD,8,400,"systematic",gridCells =TRUE)
