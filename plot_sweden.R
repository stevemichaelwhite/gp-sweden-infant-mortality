library(sf)
swe_bdry <- st_read("SWE_adm1.shp")
st_geometry_type(swe_bdry)
st_crs(swe_bdry) # Degrees, "EPSG",4326
st_bbox(swe_bdry)
# xmin     ymin     xmax     ymax
# 10.96139 55.33625 24.17240 69.05904
#
swe_bdry # See all gory detail
#
ggplot() +
  geom_sf(data = swe_bdry, size = 1, color = "black", fill = swe_bdry$ID_1) +
  ggtitle("Swedish Counties") +
  coord_sf()
#
# Plot the county names as well, but convert utf-8/unicode to plain ascii:
swe_bdry$NAME_1 <- recode(swe_bdry$NAME_1 ,
                          Östergötland  =    "Ostergotland",
                          Blekinge     =     "Blekinge",
                          Dalarna      =     "Dalarna",
                          Gävleborg   =      "Gavleborg",
                          Gotland     =      "Gotland",
                          Halland    =      "Halland",
                          Jämtland     =     "Jamtland",
                          Jönköping    =     "Jonkoping",
                          Kalmar      =      "Kalmar",
                          Kronoberg    =     "Kronoberg",
                          Norrbotten   =     "Norrbotten",
                          Örebro       =     "Orebro",
                          Södermanland =    "Sodermanland",
                          Skåne         =    "Skane",
                          Stockholm    =     "Stockholm",
                          Uppsala      =     "Uppsala",
                          Värmland     =     "Varmland",
                          Västerbotten   =   "Vasterbotten",
                          Västernorrland  =  "Vasternorrland",
                          Västmanland    =   "Vastmanland",
                          Västra         =   "Gotaland",
                          'Västra Götaland'  = "Gotaland")
#
# devtools::install_github("yutannihilation/ggsflabel") # for geom_sf_label_repel()
library(ggsflabel)
swe_bdry %>%
  ggplot() +
  geom_sf(fill = swe_bdry$ID_1) +
  geom_sf_label_repel(aes(label = NAME_1), force = 50) +
  theme_void()
#
