library(sf)
library(sp)
library(spdep)
library(ggplot2)
library(RColorBrewer)

setwd('/Users/przemekturski/Library/CloudStorage/OneDrive-Osobisty/Pliki studia/5. Ekonometria/Ekonometria_Przestrzenna/data')

data<-read.csv("regional_dataset.csv", header=TRUE, sep=";", dec=",", encoding="UTF-8")
data23<-data[data$year==2023,]

# reading point data
points<-read.csv("firms_data_utf8.csv", header=TRUE, sep=",", dec=".", encoding="UTF-8")
points.sf<-st_as_sf(points, coords=c("crds.x", "crds.y"), crs=4326)

# Read NUTS4 and NUTS2 contour map (shapefile) into sf class
pov<-st_read("powiaty.shp", options = "ENCODING=UTF-8") # 380 units
pov<-st_transform(pov, 4326) 		# reproject to WGS84
pov<-st_make_valid(pov)			# correct the shapefile object

voi<-st_read("wojewodztwa.shp", options="ENCODING=UTF-8") # 16 units 
voi<-st_transform(voi, 4326) 		# reproject to WGS84

# Read NUTS4 and NUTS2 contour map (shapefile) into sf class
pov<-st_read("powiaty.shp", options = "ENCODING=UTF-8") # 380 units
pov<-st_transform(pov, 4326) 		# reproject to WGS84
pov<-st_make_valid(pov)			# correct the shapefile object

voi<-st_read("wojewodztwa.shp", options="ENCODING=UTF-8") # 16 units 
voi<-st_transform(voi, 4326) 		# reproject to WGS84

# fragments of maps
region.voi<-voi[voi$JPT_NAZWA_=="lubelskie",] # creating a map subset
units<-which(data23$region_name=="lubelskie")
region.pov<-pov[units, ] 

# macierz wag przestrzennych W wg kryterium wspólnej granicy
# spatial weights matrix W – contiguity
cont.nb<-poly2nb(pov)						# class nb
cont.listw<-nb2listw(cont.nb, style="W")		# class listw

# macierz wag przestrzennych W – odwrotna odległość
# spatial weights matrix W – inverse distance
crds<-st_centroid(st_geometry(pov)) 	# centroidy w sf / sf centroids

pow.knn<-knearneigh(crds, k=379)	# knn for all regions (total 380) 
pow.nb<-knn2nb(pow.knn)			# w klasie nb / here in nb class

# macierz odwrotnej odległości / inverse distance matrix
inv.dist<-nb2listwdist(pow.nb, crds, type="idw", style="raw", alpha=1, dmax=NULL, longlat=TRUE, zero.policy=NULL) # class "listw" "nb"   


