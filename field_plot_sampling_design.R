# Sampling design NZ 
# Given a boundary shapefile of a forest area of interest the code generates a point shapefile
# with the plot positions. 
# at the moment only a systematic desiimplemented 
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
#######################################################################################################

samplePlotLocations <- function(inWD, outWD, n.plots, size.plots, design){
                                # load required libraries
                                library(rgeos)
                                library(rgdal)
                                library(sp)
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
}

# example usage:
samplePlotLocations("C:/Documents/.../field_data","C:/Documents/.../field_data/out",30,400,"systematic")
