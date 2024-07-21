#5/22/24, initiated by BS
#Goal: To load and tidy initial zoning downloads

#Analysis includes
  #Data tidying, parcel visualization

#Libraries
require(tidyverse)
require(tidyverse)
require(sf)
library(tigris)

options(tigris_use_cache = TRUE)

#Set parameters (state and year)
ST = "VA"
YR1 = 1990
YR2 = 2000
YR3 = 2010
YR4 = 2020

CBSA = c("Richmond")
CENTRAL_CITY = c("Richmond city")
GEOG = "tract"

#Create a filepath to OneDrive
onedrivepath="~/OneDrive - The Pennsylvania State University/"

#--------------------------------------------------------------------------------
#Download county boundaries as a reference point

#Download geographies of interest (in this case, the Richmond CBSA boundary
CBSA_2020 <- core_based_statistical_areas(resolution = "500k", year = YR4) %>%
  filter(str_detect(NAME, ST)) %>%
  filter(str_detect(NAME, CBSA))

#Download tracts of for VA, prior to a clip
Tracts_2020 <- map_dfr(c(ST), ~{
  tracts(.x, year = YR4)})

#Acquiring the cities within VA
CentralCities_2020 <- places(state = ST, year = YR4) %>%
  filter(str_detect(NAMELSAD, "city")) 

#Spatial filter the cities within the Richmond MSA
CentralCities_2020 <- CentralCities_2020[lengths(st_within(CentralCities_2020,CBSA_2020)) > 0,] 

#Download Counties and filter to within CBSA
Counties_2020 <- counties(ST,
                          year = YR4,
                          cb = F) %>%
  filter(lengths(st_within(., CBSA_2020)) > 0)


#--------------------------------------------------------------------------------
  #Load zoning data

#City of Richmond
CoR_Zoning <- read_sf(paste0(onedrivepath, "Zoning data/Richmond MSA/Richmond City/ZoningDistricts.shp")) %>%
  mutate(County = "Richmond",
         Year_Adopted = substr(AdoptionDa, 1, 4),
         Year = Year_Adopted) %>%
  rename(Code = Name) #%>%
  #select(Code, County, Year, geometry)
  

#Henrico
Henrico_Zoning <- read_sf(paste0(onedrivepath, "Zoning data/Richmond MSA/Henrico/Zoning.shp")) %>%
  mutate(County = "Henrico",
         Year = 2022) %>%
  rename(Code = ZONE_NAME) #%>%
  #select(Code, County, Year, geometry)

#Chesterfield
Chesterfield_Zoning <- read_sf(paste0(onedrivepath, "Zoning data/Richmond MSA/Chesterfield/Zoning.shp")) %>%
  mutate(County = "Chesterfield",
         Year = 2020) %>%
  rename(Code = Zoning) #%>%
#select(Code, County, Year, geometry) %>%

#----
#Amelia
  #Amelia county downloaded as a kml file
Amelia_Zoning <- read_sf(paste0(onedrivepath, "Zoning data/Richmond MSA/Amelia/WebGIS_Export.kml")) %>%
  filter(st_geometry_type(geometry) != "POINT") %>%
  mutate(County = "Amelia",
         Year = 2020) %>%
  select(-Description) %>%
  rename(Code = Name) #%>%
#select(Code, County, Year, geometry) %>%

#----
#Dinwiddie County
#Extract data from ArcGIS directory
Dinwiddie_Zoning <- st_read(paste0("https://services.arcgis.com/pXJ1hE8HkwpsRwne/ArcGIS/rest/services/Dinwiddie_Operational_Layers_view/FeatureServer/19", 
                                   "/query?where=1=1&outFields=*&f=json")) 

# # Save the data to a CSV file
# write.csv(Dinwiddie_Zoning, paste0(onedrivepath, "Zoning data/Richmond MSA/Dinwiddie/Dinwiddie_zoning.csv"), row.names = FALSE)
# 
# # Save the data to a shapefile
# st_write(Dinwiddie_Zoning, paste0(onedrivepath, "Zoning data/Richmond MSA/Dinwiddie/Dinwiddie_zoning.shp"))
# 
#Reload (to test file) and tody
Dinwiddie_Zoning <- read_sf(paste0(onedrivepath, "Zoning data/Richmond MSA/Dinwiddie/Dinwiddie_zoning.shp")) %>%
  mutate(County = "Dinwiddie") %>%
  rename(Code = ZONING) #%>%
#select(Code, County, Year, geometry) %>%

#----
#Colonial Heights
#Extract data from ArcGIS directory
Colonial_Heights_Zoning <- st_read(paste0("https://services.arcgis.com/xg1QZ4hgUiwl9v6R/ArcGIS/rest/services/City%20of%20Colonial%20Heights%20Zoning%20Districts/FeatureServer/0", 
                                   "/query?where=1=1&outFields=*&f=json")) %>%
  #Select variables to reduce export size
  select(ZONINGCODE, YEAR_BUILT, ParcelID, geometry)

# Save the data to a CSV file
write.csv(Colonial_Heights_Zoning, paste0(onedrivepath, "Zoning data/Richmond MSA/Colonial Heights/Colonial_Heights_zoning.csv"), row.names = FALSE)

# Save the data to a shapefile
st_write(Colonial_Heights_Zoning, paste0(onedrivepath, "Zoning data/Richmond MSA/Colonial Heights/Colonial_Heights_zoning.shp"))

#Reload (to test file) and tody
Colonial_Heights_Zoning <- read_sf(paste0(onedrivepath, "Zoning data/Richmond MSA/Colonial Heights/Colonial_Heights_zoning.shp")) %>%
  mutate(County = "Colonial_Heights") %>%
  rename(Code = ZONINGCODE,
         Year = 2019) #%>%
#select(Code, County, Year, geometry) %>%

CHECK NAs

#----
#Goochland County
#Extract data from ArcGIS directory
Goochland_Zoning <- st_read(paste0("https://gis.co.goochland.va.us/arcgis/rest/services/Goochland/MapServer/21", 
                       "/query?where=1=1&outFields=*&f=json")) %>%
  select(Zoning, Description, Case_No, OBJECTID_1, Calc_Acres, geometry) %>%
  rename(OBJECTID = OBJECTID_1)

#Extract data from ArcGIS directory
Goochland_Permits <- st_read(paste0("https://gis.co.goochland.va.us/arcgis/rest/services/Goochland/MapServer/22", 
                                   "/query?where=1=1&outFields=*&f=json")) %>%
  filter(Case_No == "CU-2013-00005A" |
         Case_No == "CU-2008-00003" |
         Case_No == "CU-2006-00001") %>%
  select(Zoning, Description, Case_No, OBJECTID, Calc_Acres, geometry)


#Extract data from ArcGIS directory
Goochland_Zoning <- rbind(Goochland_Zoning, Goochland_Permits)

# Save the data to a CSV file
# write.csv(Goochland_Zoning, paste0(onedrivepath, "Zoning data/Richmond MSA/Goochland/goochland_zoning.csv"), row.names = FALSE)
 
# Save the data to a shapefile
# st_write(Goochland_Zoning, paste0(onedrivepath, "Zoning data/Richmond MSA/Goochland/goochland_zoning.shp"))

#Reload (to test file) and tody
Goochland_Zoning <- read_sf(paste0(onedrivepath, "Zoning data/Richmond MSA/Goochland/goochland_zoning.shp")) %>%
  mutate(County = "Goochland") %>%
  rename(Code = Zoning) #%>%
#select(Code, County, Year, geometry) %>%







#For initial visualizing of Richmond
ggplot() + 
  geom_sf(data = CentralCities_2020, fill = NA, color = "black", linewidth = 0.65) +
  geom_sf(data = CBSA_2020, color = "black", fill = "black", linewidth = 0.6) +
  geom_sf(data = Colonial_Heights_Zoning, aes(fill = Code), col = "white") + 
  geom_sf(data = Dinwiddie_Zoning, aes(fill = Code), col = "white") + 
  geom_sf(data = Goochland_Zoning, aes(fill = Code), col = "white") + 
  geom_sf(data = CoR_Zoning, aes(fill = Code), col = "white") + 
  geom_sf(data = Henrico_Zoning, aes(fill = Code), col = "white") + 
  geom_sf(data = Chesterfield_Zoning, aes(fill = Code), col = "white") + 
  # geom_sf(data = Counties_2020[Counties_2020$NAME == "Chesterfield", ], 
  #         fill = NA, color = "black", linewidth = 0.20) + 
  theme_void() +
  theme(legend.spacing.y = unit(.1, "lines")) 

