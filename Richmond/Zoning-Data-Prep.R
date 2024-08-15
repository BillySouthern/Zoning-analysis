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
  
#----
#Henrico
Henrico_Zoning <- read_sf(paste0(onedrivepath, "Zoning data/Richmond MSA/Henrico/Zoning.shp")) %>%
  mutate(County = "Henrico",
         Year = 2022) %>%
  rename(Code = ZONE_NAME) #%>%
  #select(Code, County, Year, geometry)

#----
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
# write.csv(Colonial_Heights_Zoning, paste0(onedrivepath, "Zoning data/Richmond MSA/Colonial Heights/Colonial_Heights_zoning.csv"), row.names = FALSE)
# 
# # Save the data to a shapefile
# st_write(Colonial_Heights_Zoning, paste0(onedrivepath, "Zoning data/Richmond MSA/Colonial Heights/Colonial_Heights_zoning.shp"))

#Reload (to test file) and tody
Colonial_Heights_Zoning <- read_sf(paste0(onedrivepath, "Zoning data/Richmond MSA/Colonial Heights/Colonial_Heights_zoning.shp")) %>%
  mutate(County = "Colonial Heights",
         Year = 2019) %>%
  rename(Code = "ZONINGCODE") %>%
  filter(!Code == "NA") #%>%
#select(Code, County, Year, geometry) %>%

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

#----
#Hanover
Hanover_Zoning <- read_sf(paste0(onedrivepath, "Zoning data/Richmond MSA/Hanover/Zoning.shp")) %>%
  mutate(County = "Hanover",
         Year_Created = 2019,
         Last_Updated = 2024) %>%
  rename(Code = CLASS) #%>%
#select(Code, County, Year, geometry) %>%


#----
#Powhattan
Powhattan_Zoning <- read_sf(paste0(onedrivepath, "Zoning data/Richmond MSA/Powhattan/Zoning.shp")) %>%
  mutate(County = "Powhattan",
         Year_Created = NA,
         Last_Updated = 2024) %>%
  rename(Code = ZONING) #%>%
#select(Code, County, Year, geometry)


#----
#Sussex
#Extract data from ArcGIS directory
Sussex_Zoning <- st_read(paste0("https://services3.arcgis.com/nJbIFHiSnaX0z0hS/ArcGIS/rest/services/SussexParcels_Zoning/FeatureServer/1", 
                                   "/query?where=1=1&outFields=*&f=json")) %>%   #Select variables to reduce export size
  select(zoning, geometry)

# Save the data to a CSV file
# write.csv(Sussex_Zoning, paste0(onedrivepath, "Zoning data/Richmond MSA/Sussex/Sussex_zoning.csv"), row.names = FALSE)
# # 
# # # Save the data to a shapefile
# st_write(Sussex_Zoning, paste0(onedrivepath, "Zoning data/Richmond MSA/Sussex/Sussex_zoning.shp"))
# 
#Reload (to test file) and tody
Sussex_Zoning <- read_sf(paste0(onedrivepath, "Zoning data/Richmond MSA/Sussex/Sussex_zoning.shp")) %>%
  mutate(County = "Sussex",
         Year_Created = 2021,
         Last_Updated = 2024) %>%
  rename(Code = "zoning") #%>%
#select(Code, County, Year, geometry) %>%

#----
#Hopewell city
Hopewell_Zoning <- read_sf(paste0(onedrivepath, "Zoning data/Richmond MSA/Hopewell/Hopewell Zoning.gdb")) %>%
  mutate(County = "Hopewell",
         Year_Created = NA,
         Last_Updated = 2024) %>%
  rename(Code = ZONE_ID) #%>%
#select(Code, County, Year, geometry)

#----
#King and Queen County
KingQueen_Zoning <- read_sf(paste0(onedrivepath, "Zoning data/Richmond MSA/King and Queen/Zoning_Boundaries.shp")) %>%
  mutate(County = "King and Queen",
         Year_Created = NA,
         Last_Updated = 2024) %>%
  rename(Code = ZONING) 

#----
#Prince George
PrinceGeorge_Zoning <- read_sf(paste0(onedrivepath, "Zoning data/Richmond MSA/Prince George/Zoning.shp")) %>%
  mutate(County = "Prince George",
         Year_Created = NA,
         Last_Updated = 2024) %>%
  rename(Code = ZoningClas) 


#----
#Charles City
Charles_City <- read_sf(paste0(onedrivepath, "Zoning data/Richmond MSA/Charles City/CC_Zoning.shp")) %>%
  mutate(Code = case_when(
    is.na(Code) ~ "A-1",
    TRUE ~ Code
  )) %>%
  mutate(County = "Charles City",
         Year_Created = 2020,
         Last_Updated = 2020) 

#----
#New Kent
NewKent_Zoning <- read_sf(paste0(onedrivepath, "Zoning data/Richmond MSA/New Kent/Zoning.shp")) %>%
  mutate(County = "New Kent",
         Year_Created = NA,
         Last_Updated = 2024) %>%
  rename(Code = ZONING) 





#----
#King William
#NOTE: King William data comes from two sources (KW County and West Point town separately).  
  #Need to first digitize the KW County and they join the West Point point data

#Load King William parcels
# King_William_Parcels <- read_sf(paste0(onedrivepath, "Zoning data/Richmond MSA/King William/Parcels/King_William_Parcels.shp")) 
# 
# #Load King William parcels
# King_William_Zoning <- read_sf(paste0(onedrivepath, "Zoning data/Richmond MSA/King William/King_William_zoning.shp")) 

#Load zoning data for county
  #This after coding parcels in QGIS
King_William_Zoning <- read_sf(paste0(onedrivepath, "Zoning data/Richmond MSA/King William/King William without WP/King_William_Zoning.shp"))

#Load West point town point data
WestPoint_point_parcels <- read_sf(paste0(onedrivepath, "Zoning data/Richmond MSA/King William/Owner Boundaries (Proximity).geojson")) %>%
  select(LOCALITY, Zoning, geometry)
  
#Create centroids for point in polygon
WestPoint_Centroids <- st_centroid(WestPoint_point_parcels)

# Convert data frames to sf objects
WestPoint_Centroids <- st_transform(WestPoint_Centroids, st_crs(King_William_Zoning))

# King_William_Parcels <- st_as_sf(King_William_Parcels, wkt = "geometry", crs = 4326) # Adjust the WKT column and CRS accordingly
# WestPoint_Centroids <- st_as_sf(WestPoint_Centroids, coords = c(geometry), crs = 4326)

#Create the point in polygon count
joined_sf <- st_join(King_William_Zoning, WestPoint_Centroids, join = st_intersects) 

#Tidy the merge of point and polygon
joined_sf <- joined_sf %>%
  rename(County = LOCALITY.x,
         Town = LOCALITY.y) %>%
   mutate(Zoning_Code = coalesce(Code, Zoning)) %>%
   select(-Code, -Zoning) %>%
  filter(!st_is_empty(geometry), !is.na(geometry)) %>%
  mutate(Zoning_Code = ifelse(Zoning_Code == "NA", NA, Zoning_Code)) %>%
  mutate(Town = ifelse(is.na(Town), "Not West Point", Town)) %>%
  mutate(Zoning_Code = ifelse(is.na(Zoning_Code) & Town == "Not West Point", "A-C", Zoning_Code))

#Viz
ggplot() + 
  geom_sf(data = joined_sf, aes(fill = Zoning_Code), color = "black", linewidth = 0.05) 

#Export the completed dataset
# st_write(joined_sf, paste0(onedrivepath, "Zoning data/Richmond MSA/King William/King William and West Point/King_William_zoning_draft.shp"))

#Reload zoning data after tidying in QGIS
King_William_Zoning <- read_sf(paste0(onedrivepath, "Zoning data/Richmond MSA/King William/King_William_zoning.shp")) %>%
  rename(Code = Znng_Cd) %>%
  select(-Town)

#Viz
ggplot() + 
  geom_sf(data = King_William_Zoning, aes(fill = Code), color = "black", linewidth = 0.05) 


#----
#Petersburg city
Petersburg_Zoning <- read_sf(paste0(onedrivepath, "Zoning data/Richmond MSA/Petersburg/Petersburg_Parcels.shp")) %>%
  mutate(County = "Petersburg",
         Year_Created = NA,
         Last_Updated = 2024) %>%
  rename(Code = Zoning) %>%
  select(Code, County, Year_Created, Last_Updated, geometry) 

# Petersburg_Zoning <- st_simplify(Petersburg_Zoning, preserveTopology = TRUE)

#Validate geometries without an end
Petersburg_Zoning <- st_make_valid(Petersburg_Zoning)

#Viz
ggplot() + 
  # geom_sf(data = Amelia_Zoning, aes(fill = Code), col = "white", linewidth = 0.1) +
  geom_sf(data = Petersburg_Zoning, aes(fill = Code), col = "white", linewidth = 0.1) 

#Export above and fill missing/invalid geometries


#----
#For initial visualizing of Richmond
ggplot() + 
  geom_sf(data = CentralCities_2020, fill = NA, color = "black", linewidth = 0.65) +
  geom_sf(data = CBSA_2020, color = "black", fill = "white", linewidth = 0.6) +
  geom_sf(data = Amelia_Zoning, aes(fill = Code), col = "white", linewidth = 0.1) +
  geom_sf(data = Charles_City, aes(fill = Code), col = NA, linewidth = 0.1) +
  geom_sf(data = Colonial_Heights_Zoning, aes(fill = Code), col = NA, linewidth = 0.1) +
  geom_sf(data = Chesterfield_Zoning, aes(fill = Code), col = "white", linewidth = 0.1) +
  geom_sf(data = CoR_Zoning, aes(fill = Code), col = "white", linewidth = 0.1) +
  geom_sf(data = Dinwiddie_Zoning, aes(fill = Code), col = "white", linewidth = 0.1) +
  geom_sf(data = Goochland_Zoning, aes(fill = Code), col = "white", linewidth = 0.1) +
  geom_sf(data = Hanover_Zoning, aes(fill = Code), col = "white", linewidth = 0.1) +
  geom_sf(data = Henrico_Zoning, aes(fill = Code), col = "white", linewidth = 0.1) +
  geom_sf(data = Hopewell_Zoning, aes(fill = Code), col = "white", linewidth = 0.1) +
  geom_sf(data = KingQueen_Zoning, aes(fill = Code), col = "white", linewidth = 0.1) +
  geom_sf(data = King_William_Zoning, aes(fill = Code), col = "white", linewidth = 0.1) +
  geom_sf(data = NewKent_Zoning, aes(fill = Code), col = "white", linewidth = 0.1) +
  geom_sf(data = Petersburg_Zoning, aes(fill = Code), col = "white", linewidth = 0.1) +
  geom_sf(data = Powhattan_Zoning, aes(fill = Code), col = "white", linewidth = 0.1) +
  geom_sf(data = PrinceGeorge_Zoning, aes(fill = Code), col = "white", linewidth = 0.1) +
  geom_sf(data = Sussex_Zoning, aes(fill = Code), col = "white", linewidth = 0.1) +
  geom_sf(data = Counties_2020[Counties_2020$NAME == "Chesterfield", ],
          fill = NA, color = "black", linewidth = 0.20) +
  theme_void() +
  theme(legend.spacing.y = unit(.1, "lines")) +
  geom_sf(data = Counties_2020, fill = NA, color = "black", linewidth = 0.65) 

# ggsave("RichMSA-Zone-All.png",
#        path = "~/desktop",
#        width = 25,
#        height = 15,
#        units = "in",
#        dpi = 500)


