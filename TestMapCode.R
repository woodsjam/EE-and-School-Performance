library(maptools)
gpclibPermit()
IOU <- readShapePoly("MapData/Utility_Customer_Boundaries_for_Investor_Owned_Utilities/Utility_Customer_Boundaries_for_Investor_Owned_Utilities.shp")

Schools <- readShapePoly("MapData/cb_2016_41_unsd_500k/cb_2016_41_unsd_500k.shp")

library(ggmap)
mapImage <- get_map(location = "oregon", zoom =7, maptype = "toner")

IOUBound <- fortify(IOU)
SchoolBound <- fortify(Schools)

library(RColorBrewer)
colors <- brewer.pal(9, "BuGn")

ggmap(mapImage)+
  geom_polygon(aes(x = long,
                   y = lat,
                   group = group),
               data = SchoolBound,
               color = colors[9],
               fill = colors[6],
               alpha = 0.5) +
  labs(x = "Longitude",
       y = "Latitude")
