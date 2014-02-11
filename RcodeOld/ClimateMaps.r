library(OpenStreetMap)
library(shapefiles)
map <- openmap(c(42,-112), c(45.5,-108),type="bing")
 plot(map)
a<-readShapePoly("H:\\Desktop\\Climate\\ParkBoundaries\\NPSBoundaries.shp")

