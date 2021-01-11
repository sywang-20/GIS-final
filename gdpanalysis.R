## GDP mapping
library(sf)
library(tmap)
library(tmaptools)
library(tidyverse)
library(here)
##Load all our data
china<-st_read(here::here("gdp_data","gadm36_CHN_shp","gadm36_CHN_1.shp"))
china<-subset(china,NAME_1!="Hainan")   #excluding Hainan
gdp<-read_csv(here::here("gdp_data","gdp.csv"))
qtm(china)
# merge
gdpmap <- china %>%
  merge(.,
        gdp, 
        by.x="NAME_1", 
        by.y="city",
        no.dups = TRUE)%>%
  distinct(.,NAME_1, 
           .keep_all = TRUE)
# set the mode
tmap_mode("plot")
# GDP 2019
tm_shape(gdpmap) +
  tm_polygons("GDP_2019",
              title="GDP per capita(yuan)")+
  tm_layout("GDP per capita in China in 2019",
            legend.title.size = 1,
            legend.text.size = 0.6,
            legend.position = c("left","bottom"),)+
  tm_compass(position=c("left", "top"))+
  tm_scale_bar(position=c("right", "bottom"),text.size=0.6)
# GDP 2015
tm_shape(gdpmap) +
  tm_polygons("GDP_2015",
              title="GDP per capita(yuan)")+
  tm_layout("GDP per capita in China in 2015",
            legend.title.size = 1,
            legend.text.size = 0.6,
            legend.position = c("left","bottom"),)+
  tm_compass(position=c("left", "top"))+
  tm_scale_bar(position=c("right", "bottom"),text.size=0.6)
# GDP 2010
tm_shape(gdpmap) +
  tm_polygons("GDP_2010",
              title="GDP per capita(yuan)")+
  tm_layout("GDP per capita in China in 2010",
            legend.title.size = 1,
            legend.text.size = 0.6,
            legend.position = c("left","bottom"),)+
  tm_compass(position=c("left", "top"))+
  tm_scale_bar(position=c("right", "bottom"),text.size=0.6)




# Spatial Autocorrelation
#library a bunch of packages we may (or may not) use - install them first if not installed already. 
library(tidyverse)
library(tmap)
library(geojsonio)
library(plotly)
library(rgdal)
library(broom)
library(mapview)
library(crosstalk)
library(sf)
library(sp)
library(spdep)
library(car)
library(fs)
library(janitor)
library(spData)
library(spdep)

#load data
china <- st_read(here::here("gdp_data","gadm36_CHN_shp","gadm36_CHN_1.shp"))
china<-subset(china,NAME_1!="Hainan")   #excluding Hainan
qtm(china)
chinadata <- read_csv(here::here("gdp_data","gdp.csv"), 
                      na = c("NA", "n/a")) %>% 
  clean_names()
# join the data
china<- china %>% 
  left_join(chinadata, 
            by = c("NAME_1" = "city"))%>%
  distinct(NAME_1,gdp_2019)
# check crs
st_crs(china)
# select the data we need
points_sf_joined_china <- china%>%
  dplyr::select(NAME_1,gdp_2019)


#First calculate the centroids of all provinces in China
coordsW_china <- points_sf_joined_china%>%
  st_centroid()%>%
  st_geometry()
plot(coordsW_china,axes=TRUE)
#create a neighbours list
LWard_nb_china <- points_sf_joined_china %>%
  poly2nb(., queen=T)
#knn
#knn_wards_china<-coordsW_china%>%
#  knearneigh(.,k=10)
#knn_wards_china
#coordsW_china
#LWard_knn_china <- knn_wards_china %>%
#  knn2nb()
#plot them
plot(LWard_nb_china, st_geometry(coordsW_china), col="red")
#add a map underneath
plot(points_sf_joined_china$geometry, add=T)
#create a spatial weights object from these weights
Lward.lw_china <- LWard_nb_china %>%
  nb2listw(., style="C")
head(Lward.lw_china$neighbours)


#Moran's I
I_LWard_Global_Density_china <- points_sf_joined_china %>%
  pull(gdp_2019) %>%
  as.vector()%>%
  moran.test(., Lward.lw_china)
I_LWard_Global_Density_china


# join the data
china<- china %>% 
  left_join(chinadata, 
            by = c("NAME_1" = "city"))%>%
  distinct(NAME_1,gdp_2018)
# check crs
st_crs(china)
# select the data we need
points_sf_joined_china <- china%>%
  dplyr::select(NAME_1,gdp_2018)
#Moran's I
I_LWard_Global_Density_china <- points_sf_joined_china %>%
  pull(gdp_2018) %>%
  as.vector()%>%
  moran.test(., Lward.lw_china)
I_LWard_Global_Density_china



# join the data
china<- china %>% 
  left_join(chinadata, 
            by = c("NAME_1" = "city"))%>%
  distinct(NAME_1,gdp_2017)
# check crs
#st_crs(china)
# select the data we need
points_sf_joined_china <- china%>%
  dplyr::select(NAME_1,gdp_2017)
#Moran's I
I_LWard_Global_Density_china <- points_sf_joined_china %>%
  pull(gdp_2017) %>%
  as.vector()%>%
  moran.test(., Lward.lw_china)
I_LWard_Global_Density_china



# join the data
china<- china %>% 
  left_join(chinadata, 
            by = c("NAME_1" = "city"))%>%
  distinct(NAME_1,gdp_2016)
# check crs
#st_crs(china)
# select the data we need
points_sf_joined_china <- china%>%
  dplyr::select(NAME_1,gdp_2016)
#Moran's I
I_LWard_Global_Density_china <- points_sf_joined_china %>%
  pull(gdp_2016) %>%
  as.vector()%>%
  moran.test(., Lward.lw_china)
I_LWard_Global_Density_china


# join the data
china<- china %>% 
  left_join(chinadata, 
            by = c("NAME_1" = "city"))%>%
  distinct(NAME_1,gdp_2015)
# check crs
#st_crs(china)
# select the data we need
points_sf_joined_china <- china%>%
  dplyr::select(NAME_1,gdp_2015)
#Moran's I
I_LWard_Global_Density_china <- points_sf_joined_china %>%
  pull(gdp_2015) %>%
  as.vector()%>%
  moran.test(., Lward.lw_china)
I_LWard_Global_Density_china




# join the data
china<- china %>% 
  left_join(chinadata, 
            by = c("NAME_1" = "city"))%>%
  distinct(NAME_1,gdp_2014)
# check crs
#st_crs(china)
# select the data we need
points_sf_joined_china <- china%>%
  dplyr::select(NAME_1,gdp_2014)
#Moran's I
I_LWard_Global_Density_china <- points_sf_joined_china %>%
  pull(gdp_2014) %>%
  as.vector()%>%
  moran.test(., Lward.lw_china)
I_LWard_Global_Density_china


# join the data
china<- china %>% 
  left_join(chinadata, 
            by = c("NAME_1" = "city"))%>%
  distinct(NAME_1,gdp_2013)
# check crs
#st_crs(china)
# select the data we need
points_sf_joined_china <- china%>%
  dplyr::select(NAME_1,gdp_2013)
#Moran's I
I_LWard_Global_Density_china <- points_sf_joined_china %>%
  pull(gdp_2013) %>%
  as.vector()%>%
  moran.test(., Lward.lw_china)
I_LWard_Global_Density_china


# join the data
china<- china %>% 
  left_join(chinadata, 
            by = c("NAME_1" = "city"))%>%
  distinct(NAME_1,gdp_2012)
# check crs
#st_crs(china)
# select the data we need
points_sf_joined_china <- china%>%
  dplyr::select(NAME_1,gdp_2012)
#Moran's I
I_LWard_Global_Density_china <- points_sf_joined_china %>%
  pull(gdp_2012) %>%
  as.vector()%>%
  moran.test(., Lward.lw_china)
I_LWard_Global_Density_china


# join the data
china<- china %>% 
  left_join(chinadata, 
            by = c("NAME_1" = "city"))%>%
  distinct(NAME_1,gdp_2011)
# check crs
#st_crs(china)
# select the data we need
points_sf_joined_china <- china%>%
  dplyr::select(NAME_1,gdp_2011)
#Moran's I
I_LWard_Global_Density_china <- points_sf_joined_china %>%
  pull(gdp_2011) %>%
  as.vector()%>%
  moran.test(., Lward.lw_china)
I_LWard_Global_Density_china




# join the data
china<- china %>% 
  left_join(chinadata, 
            by = c("NAME_1" = "city"))%>%
  distinct(NAME_1,gdp_2010)
# check crs
#st_crs(china)
# select the data we need
points_sf_joined_china <- china%>%
  dplyr::select(NAME_1,gdp_2010)
#Moran's I
I_LWard_Global_Density_china <- points_sf_joined_china %>%
  pull(gdp_2010) %>%
  as.vector()%>%
  moran.test(., Lward.lw_china)
I_LWard_Global_Density_china


# local Moran's I
# join the data
china<- china %>% 
  left_join(chinadata, 
            by = c("NAME_1" = "city"))%>%
  distinct(NAME_1,gdp_2019)
# check crs
st_crs(china)
# select the data we need
points_sf_joined_china <- china%>%
  dplyr::select(NAME_1,gdp_2019)

I_LWard_Local_gdp <- china %>%
  arrange(NAME_1)%>%
  pull(gdp_2019) %>%
  as.vector()%>%
  localmoran(., Lward.lw_china)%>%
  as_tibble()

points_sf_joined_china <- points_sf_joined_china %>%
  arrange(NAME_1)%>%
  mutate(gdp_LocIz = as.numeric(I_LWard_Local_gdp$Z.Ii))

library(RColorBrewer)
breaks1<-c(-1000,-2.58,-1.96,-1.65,1.65,1.96,2.58,1000)
MoranColours<- rev(brewer.pal(8, "RdGy"))

tm_shape(points_sf_joined_china) +
  tm_polygons("gdp_LocIz",
              style="fixed",
              breaks=breaks1,
              palette=MoranColours,
              midpoint=NA,
              title="Local Moran's I")+
  tm_layout("Local Moran's I for each province in 2019",
            legend.title.size = 1,
            legend.text.size = 0.6,
            legend.position = c("left","bottom"),)+
  tm_compass(position=c("left", "top"))+
  tm_scale_bar(position=c("right", "bottom"),text.size=0.6)





# join the data
china<- china %>% 
  left_join(chinadata, 
            by = c("NAME_1" = "city"))%>%
  distinct(NAME_1,gdp_2010)
# check crs
st_crs(china)
# select the data we need
points_sf_joined_china <- china%>%
  dplyr::select(NAME_1,gdp_2010)

I_LWard_Local_gdp <- china %>%
  arrange(NAME_1)%>%
  pull(gdp_2010) %>%
  as.vector()%>%
  localmoran(., Lward.lw_china)%>%
  as_tibble()

points_sf_joined_china <- points_sf_joined_china %>%
  arrange(NAME_1)%>%
  mutate(gdp_LocIz = as.numeric(I_LWard_Local_gdp$Z.Ii))

library(RColorBrewer)
breaks1<-c(-1000,-2.58,-1.96,-1.65,1.65,1.96,2.58,1000)
MoranColours<- rev(brewer.pal(8, "RdGy"))

tm_shape(points_sf_joined_china) +
  tm_polygons("gdp_LocIz",
              style="fixed",
              breaks=breaks1,
              palette=MoranColours,
              midpoint=NA,
              title="Local Moran's I")+
  tm_layout("Local Moran's I for each province in 2010",
            legend.title.size = 1,
            legend.text.size = 0.6,
            legend.position = c("left","bottom"),)+
  tm_compass(position=c("left", "top"))+
  tm_scale_bar(position=c("right", "bottom"),text.size=0.6)

