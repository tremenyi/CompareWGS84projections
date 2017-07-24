
# ~~~~~~~~~~~ Set libraries ~~~~~~~~~~~ #
library(rgdal)      # for spTransform() & project()
library(ggplot2)    # for ggplot()
library(ggrepel)    # for geom_text_repel() - repel overlapping text labels
library(data.table)

# ~~~~~~~~~~~ Load a function that allows to plot multiple seapartate ggplots in a single figure ~~~~~~
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# ~~~~~~~~~~~ Load ready to use data from GitHub ~~~~~~~~~~~ #
load(url("https://github.com/valentinitnelav/RandomScripts/blob/master/NaturalEarth.RData?raw=true"))
# This will load 6 objects:
#   xbl.X & lbl.Y are two data.frames that contain labels for graticule lines
#       They can be created with the code at this link: 
#       https://gist.github.com/valentinitnelav/8992f09b4c7e206d39d00e813d2bddb1
#   NE_box is a SpatialPolygonsDataFrame object and represents a bounding box for Earth 
#   NE_countries is a SpatialPolygonsDataFrame object representing countries 
#   NE_graticules is a SpatialLinesDataFrame object that represents 10 dg latitude lines and 20 dg longitude lines
#           (for creating graticules check also the graticule package or gridlines fun. from sp package)
#           (or check this gist: https://gist.github.com/valentinitnelav/a7871128d58097e9d227f7a04e00134f)
#   NE_places - SpatialPointsDataFrame with city and town points
#   NOTE: data downloaded from http://www.naturalearthdata.com/
#         here is a sample script how to download, unzip and read such shapefiles:
#         https://gist.github.com/valentinitnelav/a415f3fbfd90f72ea06b5411fb16df16

# ~~~~~~~~~~~ Project from long-lat to Eckert IV projection ~~~~~~~~~~~ #
# spTransform() is used for shapefiles and project() in the case of data frame
# for more PROJ.4 strings check the followings
#   http://proj4.org/projections/index.html
#   https://epsg.io/

# __ give the PORJ.4 string for Eckert IV projection
PROJ_longlat <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" 
PROJ_eck4 <- "+proj=eck4 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
PROJ_cea <- "+proj=cea +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

# __ project the shapefiles
map_data_longlat <- spTransform(NE_countries, CRSobj = PROJ_longlat)
map_data_eck4 <- spTransform(NE_countries, CRSobj = PROJ_eck4)
map_data_cea <- spTransform(NE_countries, CRSobj = PROJ_cea)

map_plot_longlat <- ggplot(data = map_data_longlat, aes(long,lat, group = group)) +
  geom_polygon(colour = "gray70", fill = "gray90", size = .25) + labs(title="longlat")
map_plot_eck4 <- ggplot(data = map_data_eck4, aes(long,lat, group = group)) +
  geom_polygon(colour = "gray70", fill = "gray90", size = .25) + labs(title="eck4")
map_plot_cea <- ggplot(data = map_data_cea, aes(long,lat, group = group)) +
  geom_polygon(colour = "gray70", fill = "gray90", size = .25) + labs(title="cea")
png("~/WGS84datum_threeProjections.png", width=297*2, height=210, units="mm", res=200)
  multiplot(map_plot_longlat, map_plot_eck4, map_plot_cea, layout=matrix(1:3, 1, 3) )
dev.off()
