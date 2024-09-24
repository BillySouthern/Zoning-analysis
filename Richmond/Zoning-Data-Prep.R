#5/22/24, initiated by BS
#Goal: To load and tidy initial zoning downloads

#Analysis includes
  #Data tidying, parcel visualization

#Libraries
require(tidyverse)
require(tidyverse)
require(sf)
library(tigris)
library(readxl)

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

#Load transitional geographies
#2020
Transitional_2020 <- read_sf(paste0(onedrivepath, "Mapping Richmond/Index/Index_2020/Richmond_Index_2020.shp")) %>%
  rename(Suburban_Index = Sbrbn_I,
         Urban_Index = Urbn_In,
         Landscape_Four = Lndscp_Fr,
         Landscape_Five = Lndscp_Fv,
         Landscape_Six = Lndsc_S,
         Landscape_All = Lndsc_A) %>%
  select(GEOID, Landscape_Five)

#Load places too
Place_Tracts_2020 <- read.csv(file.path(onedrivepath, "Mapping Richmond/Places/2020/Place_Tracts_2020.csv")) %>%
  select(-X) %>%
  mutate(GEOID = as.character(GEOID))

#Join two above
Richmond_Geographies <- Transitional_2020 %>%
  left_join(Place_Tracts_2020, by = "GEOID")


#--------------------------------------------------------------------------------
  #Load zoning data

#City of Richmond
CoR_Zoning <- read_sf(paste0(onedrivepath, "Zoning data/Richmond MSA/Richmond City/ZoningDistricts.shp")) %>%
  mutate(County = "City of Richmond",
         Year_Adopted = substr(AdoptionDa, 1, 4),
         Year = Year_Adopted) %>%
  rename(Code = Name) %>%
  select(Code, County, Year, geometry) 

#Load zoning description
RVA_Zoning_Descriptions <- read_excel("~/Library/CloudStorage/OneDrive-ThePennsylvaniaStateUniversity/RQ3/RVA-Zoning-Descriptions.xlsx")

#Join descriptions with code
CoR_Zoning <- CoR_Zoning %>%
  left_join(RVA_Zoning_Descriptions, by= c("County", "Code")) %>%
  select(County, Year, Code, Name, Nature, Housing_Description, Maximum_Density_Allowed, Zoning_Atlas_Definition, Source, geometry) 

#----
#Henrico
Henrico_Zoning <- read_sf(paste0(onedrivepath, "Zoning data/Richmond MSA/Henrico/Zoning.shp")) %>%
  mutate(County = "Henrico",
         Year = 2022) %>%
  rename(Code = ZONE_NAME) %>%
  mutate(Code = str_remove(Code, "C$")) %>% #Remove conditional category
  #select(Code, County, Year, geometry) %>%
  mutate(Code = case_when( #One parcel in the process of rezoning from A to R-3
   ZONE_LABEL == "REZ2019-00027" ~ "R-3",  # Update this line as needed
   Code == "x" ~ "A-1",  #One zoning code set to x
   TRUE ~ Code
  )) %>%
  st_transform(st_crs(CoR_Zoning)) 

#Load zoning description
RVA_Zoning_Descriptions <- read_excel("~/Library/CloudStorage/OneDrive-ThePennsylvaniaStateUniversity/RQ3/RVA-Zoning-Descriptions.xlsx")

#Join descriptions with code
Henrico_Zoning <- Henrico_Zoning %>%
  left_join(RVA_Zoning_Descriptions, by= c("County", "Code")) %>%
  select(County, Year, Code, Name, Nature, Housing_Description, Maximum_Density_Allowed, Zoning_Atlas_Definition, Source, geometry)

#----
#Chesterfield
Chesterfield_Zoning <- read_sf(paste0(onedrivepath, "Zoning data/Richmond MSA/Chesterfield/Zoning.shp")) %>%
  mutate(County = "Chesterfield",
         Year = 2020) %>%
  rename(Code = Zoning) %>%
  st_transform(st_crs(CoR_Zoning)) 

#Load zoning description
RVA_Zoning_Descriptions <- read_excel("~/Library/CloudStorage/OneDrive-ThePennsylvaniaStateUniversity/RQ3/RVA-Zoning-Descriptions.xlsx")

#Join descriptions with code
Chesterfield_Zoning <- Chesterfield_Zoning %>%
  left_join(RVA_Zoning_Descriptions, by= c("County", "Code")) %>%
  select(County, Year, Code, Name, Nature, Housing_Description, Maximum_Density_Allowed, Zoning_Atlas_Definition, Source, geometry)

#----
#Amelia
  #Amelia county downloaded as a kml file
Amelia_Zoning <- read_sf(paste0(onedrivepath, "Zoning data/Richmond MSA/Amelia/WebGIS_Export.kml")) %>%
  filter(st_geometry_type(geometry) != "POINT") %>%
  mutate(County = "Amelia",
         Year = 2020) %>%
  select(-Description) %>%
  rename(Code = Name) %>%
  st_transform(st_crs(CoR_Zoning)) 

#Load zoning description
RVA_Zoning_Descriptions <- read_excel("~/Library/CloudStorage/OneDrive-ThePennsylvaniaStateUniversity/RQ3/RVA-Zoning-Descriptions.xlsx")

#Join descriptions with code
Amelia_Zoning <- Amelia_Zoning %>%
  left_join(RVA_Zoning_Descriptions, by= c("County", "Code")) %>%
  select(County, Year, Code, Name, Nature, Housing_Description, Maximum_Density_Allowed, Zoning_Atlas_Definition, Source, geometry)

#----
#Dinwiddie County
#Extract data from ArcGIS directory
# Dinwiddie_Zoning <- st_read(paste0("https://services.arcgis.com/pXJ1hE8HkwpsRwne/ArcGIS/rest/services/Dinwiddie_Operational_Layers_view/FeatureServer/19", 
#                                    "/query?where=1=1&outFields=*&f=json")) 

# # Save the data to a CSV file
# write.csv(Dinwiddie_Zoning, paste0(onedrivepath, "Zoning data/Richmond MSA/Dinwiddie/Dinwiddie_zoning.csv"), row.names = FALSE)
# 
# # Save the data to a shapefile
# st_write(Dinwiddie_Zoning, paste0(onedrivepath, "Zoning data/Richmond MSA/Dinwiddie/Dinwiddie_zoning.shp"))
# 
#Reload (to test file) and tody
Dinwiddie_Zoning <- read_sf(paste0(onedrivepath, "Zoning data/Richmond MSA/Dinwiddie/Dinwiddie_zoning.shp")) %>%
  mutate(County = "Dinwiddie",
         Year = 2023) %>%
  rename(Code = ZONING) %>%
  st_transform(st_crs(CoR_Zoning)) 

#Load zoning description
RVA_Zoning_Descriptions <- read_excel("~/Library/CloudStorage/OneDrive-ThePennsylvaniaStateUniversity/RQ3/RVA-Zoning-Descriptions.xlsx")

#Join descriptions with code
Dinwiddie_Zoning <- Dinwiddie_Zoning %>%
  left_join(RVA_Zoning_Descriptions, by= c("County", "Code")) %>%
  select(County, Year, Code, Name, Nature, Housing_Description, Maximum_Density_Allowed, Zoning_Atlas_Definition, Source, geometry)


#----
#Colonial Heights
#Extract data from ArcGIS directory
# Colonial_Heights_Zoning <- st_read(paste0("https://services.arcgis.com/xg1QZ4hgUiwl9v6R/ArcGIS/rest/services/City%20of%20Colonial%20Heights%20Zoning%20Districts/FeatureServer/0", 
#                                    "/query?where=1=1&outFields=*&f=json")) %>%
#   #Select variables to reduce export size
#   select(ZONINGCODE, YEAR_BUILT, ParcelID, geometry)

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
  filter(!Code == "NA") %>%
  mutate(Code = case_when( #One parcel in the process of rezoning from A to R-3
    Code == "PUD\r\nPUD" ~ "PUD",  # Update this line as needed
    Code == "RL\r\nRL\r\nRL" ~ "RL",  #One zoning code set to x
    Code == "RL\r\nRL\r\nRL\r\nRL" ~ "RL",  # Update this line as needed
    Code == "RL\r\nRL" ~ "RL",  #One zoning code set to x
    Code == "R\r\nRL" ~ "RL",  # Update this line as needed
    Code == "Rl" ~ "RL",  #One zoning code set to x
    TRUE ~ Code
  ))%>%
  st_transform(st_crs(CoR_Zoning)) 

#Load zoning description
RVA_Zoning_Descriptions <- read_excel("~/Library/CloudStorage/OneDrive-ThePennsylvaniaStateUniversity/RQ3/RVA-Zoning-Descriptions.xlsx")

#Join descriptions with code
Colonial_Heights_Zoning <- Colonial_Heights_Zoning %>%
  left_join(RVA_Zoning_Descriptions, by= c("County", "Code")) %>%
  select(County, Year, Code, Name, Nature, Housing_Description, Maximum_Density_Allowed, Zoning_Atlas_Definition, Source, geometry) 

#----
#Goochland County
#Extract data from ArcGIS directory
# Goochland_Zoning <- st_read(paste0("https://gis.co.goochland.va.us/arcgis/rest/services/Goochland/MapServer/21", 
#                        "/query?where=1=1&outFields=*&f=json")) %>%
#   select(Zoning, Description, Case_No, OBJECTID_1, Calc_Acres, geometry) %>%
#   rename(OBJECTID = OBJECTID_1)
# 
# #Extract data from ArcGIS directory
# Goochland_Permits <- st_read(paste0("https://gis.co.goochland.va.us/arcgis/rest/services/Goochland/MapServer/22", 
#                                    "/query?where=1=1&outFields=*&f=json")) %>%
#   filter(Case_No == "CU-2013-00005A" |
#          Case_No == "CU-2008-00003" |
#          Case_No == "CU-2006-00001") %>%
#   select(Zoning, Description, Case_No, OBJECTID, Calc_Acres, geometry)
# 
# 
# #Extract data from ArcGIS directory
# Goochland_Zoning <- rbind(Goochland_Zoning, Goochland_Permits)

# Save the data to a CSV file
# write.csv(Goochland_Zoning, paste0(onedrivepath, "Zoning data/Richmond MSA/Goochland/goochland_zoning.csv"), row.names = FALSE)
 
# Save the data to a shapefile
# st_write(Goochland_Zoning, paste0(onedrivepath, "Zoning data/Richmond MSA/Goochland/goochland_zoning.shp"))

#Reload (to test file) and tody
Goochland_Zoning <- read_sf(paste0(onedrivepath, "Zoning data/Richmond MSA/Goochland/goochland_zoning.shp")) %>%
  mutate(County = "Goochland",
         Year = 2024) %>%
  rename(Code = Zoning) %>%
  st_transform(st_crs(CoR_Zoning)) 

#Load zoning description
RVA_Zoning_Descriptions <- read_excel("~/Library/CloudStorage/OneDrive-ThePennsylvaniaStateUniversity/RQ3/RVA-Zoning-Descriptions.xlsx")

#Join descriptions with code
Goochland_Zoning <- Goochland_Zoning %>%
  left_join(RVA_Zoning_Descriptions, by= c("County", "Code")) %>%
  select(County, Year, Code, Name, Nature, Housing_Description, Maximum_Density_Allowed, Zoning_Atlas_Definition, Source, geometry) 

#----
#Hanover
Hanover_Zoning <- read_sf(paste0(onedrivepath, "Zoning data/Richmond MSA/Hanover/Zoning.shp")) %>%
  mutate(County = "Hanover",
         Year = 2019) %>%
  rename(Code = CLASS) %>%
  st_transform(st_crs(CoR_Zoning)) 

#Load zoning description
RVA_Zoning_Descriptions <- read_excel("~/Library/CloudStorage/OneDrive-ThePennsylvaniaStateUniversity/RQ3/RVA-Zoning-Descriptions.xlsx")

#Join descriptions with code
Hanover_Zoning <- Hanover_Zoning %>%
  left_join(RVA_Zoning_Descriptions, by= c("County", "Code")) %>%
  select(County, Year, Code, Name, Nature, Housing_Description, Maximum_Density_Allowed, Zoning_Atlas_Definition, Source, geometry) %>%
  filter(!Code == "NA") #One parcel with no data

#----
#Powhattan
Powhattan_Zoning <- read_sf(paste0(onedrivepath, "Zoning data/Richmond MSA/Powhattan/Zoning.shp")) %>%
  mutate(County = "Powhattan",
         Year = 2024) %>%
  rename(Code = ZONING) %>%
  mutate(Code = if_else(is.na(Code), as.character(row_number()), Code)) %>%
  mutate(Code = case_when( #One parcel in the process of rezoning from A to R-3
    Code == "2" ~ "R-R",  #One zoning code set to x
    TRUE ~ Code
  )) %>%
  st_transform(st_crs(CoR_Zoning)) 

#Load zoning description
RVA_Zoning_Descriptions <- read_excel("~/Library/CloudStorage/OneDrive-ThePennsylvaniaStateUniversity/RQ3/RVA-Zoning-Descriptions.xlsx")

#Join descriptions with code
Powhattan_Zoning <- Powhattan_Zoning %>%
  left_join(RVA_Zoning_Descriptions, by= c("County", "Code")) %>%
  select(County, Year, Code, Name, Nature, Housing_Description, Maximum_Density_Allowed, Zoning_Atlas_Definition, Source, geometry) %>%
  filter(!Name == "NA") #One parcel with no data

#----
#Sussex
#Extract data from ArcGIS directory
# Sussex_Zoning <- st_read(paste0("https://services3.arcgis.com/nJbIFHiSnaX0z0hS/ArcGIS/rest/services/SussexParcels_Zoning/FeatureServer/1", 
#                                    "/query?where=1=1&outFields=*&f=json")) %>%   #Select variables to reduce export size
#   select(zoning, geometry)

# Save the data to a CSV file
# write.csv(Sussex_Zoning, paste0(onedrivepath, "Zoning data/Richmond MSA/Sussex/Sussex_zoning.csv"), row.names = FALSE)
# # 
# # # Save the data to a shapefile
# st_write(Sussex_Zoning, paste0(onedrivepath, "Zoning data/Richmond MSA/Sussex/Sussex_zoning.shp"))
# 
#Reload (to test file) and tody
Sussex_Zoning <- read_sf(paste0(onedrivepath, "Zoning data/Richmond MSA/Sussex/Sussex_zoning.shp")) %>%
  mutate(County = "Sussex",
         Year = 2024) %>%
  rename(Code = "zoning") %>%
  mutate(Code = case_when( #One parcel in the process of rezoning from A to R-3
    Code == "AREA OF DISPUTE" ~ "A-1",  #One parcel in disupute and still A-1
    TRUE ~ Code
  )) 

#Match crs
Sussex_Zoning <- st_set_crs(Sussex_Zoning, st_crs(CoR_Zoning))

#Load zoning description
RVA_Zoning_Descriptions <- read_excel("~/Library/CloudStorage/OneDrive-ThePennsylvaniaStateUniversity/RQ3/RVA-Zoning-Descriptions.xlsx")

#Join descriptions with code
Sussex_Zoning <- Sussex_Zoning %>%
  left_join(RVA_Zoning_Descriptions, by= c("County", "Code")) %>%
  select(County, Year, Code, Name, Nature, Housing_Description, Maximum_Density_Allowed, Zoning_Atlas_Definition, Source, geometry) %>%
  filter(!Name == "NA") #One parcel with no data


#----
#Hopewell city
Hopewell_Zoning <- read_sf(paste0(onedrivepath, "Zoning data/Richmond MSA/Hopewell/Hopewell Zoning.gdb")) %>%
  mutate(County = "Hopewell",
         Year = 2024) %>%
  rename(Code = ZONE_LBL,
         geometry = Shape) %>%
  st_transform(st_crs(CoR_Zoning)) 

#Load zoning description
RVA_Zoning_Descriptions <- read_excel("~/Library/CloudStorage/OneDrive-ThePennsylvaniaStateUniversity/RQ3/RVA-Zoning-Descriptions.xlsx")

#Join descriptions with code
Hopewell_Zoning <- Hopewell_Zoning %>%
  left_join(RVA_Zoning_Descriptions, by= c("County", "Code")) %>%
  select(County, Year, Code, Name, Nature, Housing_Description, Maximum_Density_Allowed, Zoning_Atlas_Definition, Source, geometry) 

#----
#King and Queen County
KingQueen_Zoning <- read_sf(paste0(onedrivepath, "Zoning data/Richmond MSA/King and Queen/Zoning_Boundaries.shp")) %>%
  mutate(County = "King and Queen",
         Year = 2024) %>%
  rename(Code = ZONING) %>%
  st_transform(st_crs(CoR_Zoning)) 

#Load zoning description
RVA_Zoning_Descriptions <- read_excel("~/Library/CloudStorage/OneDrive-ThePennsylvaniaStateUniversity/RQ3/RVA-Zoning-Descriptions.xlsx")

#Join descriptions with code
KingQueen_Zoning <- KingQueen_Zoning %>%
  left_join(RVA_Zoning_Descriptions, by= c("County", "Code")) %>%
  select(County, Year, Code, Name, Nature, Housing_Description, Maximum_Density_Allowed, Zoning_Atlas_Definition, Source, geometry) 

#----
#Prince George
PrinceGeorge_Zoning <- read_sf(paste0(onedrivepath, "Zoning data/Richmond MSA/Prince George/Zoning.shp")) %>%
  mutate(County = "Prince George",
         Year = 2024) %>%
  rename(Code = ZoningClas) %>%
  mutate(Code = case_when( #One parcel in the process of rezoning from A to R-3
    is.na(Code) ~ "I-2",  #One parcel in disupute and still A-1
    TRUE ~ Code
  ))  %>%
  st_transform(st_crs(CoR_Zoning)) 

#Load zoning description
RVA_Zoning_Descriptions <- read_excel("~/Library/CloudStorage/OneDrive-ThePennsylvaniaStateUniversity/RQ3/RVA-Zoning-Descriptions.xlsx")

#Join descriptions with code
PrinceGeorge_Zoning <- PrinceGeorge_Zoning %>%
  left_join(RVA_Zoning_Descriptions, by= c("County", "Code")) %>%
  select(County, Year, Code, Name, Nature, Housing_Description, Maximum_Density_Allowed, Zoning_Atlas_Definition, Source, geometry) 

#----
#Charles City
Charles_City_Zoning <- read_sf(paste0(onedrivepath, "Zoning data/Richmond MSA/Charles City/CC_Zoning.shp")) %>%
  mutate(Code = case_when(
    is.na(Code) ~ "A-1",
    TRUE ~ Code
  )) %>%
  mutate(County = "Charles City",
         Year = 2020)  %>%
  st_transform(st_crs(CoR_Zoning)) 

#Load zoning description
RVA_Zoning_Descriptions <- read_excel("~/Library/CloudStorage/OneDrive-ThePennsylvaniaStateUniversity/RQ3/RVA-Zoning-Descriptions.xlsx")

#Join descriptions with code
Charles_City_Zoning <- Charles_City_Zoning %>%
  left_join(RVA_Zoning_Descriptions, by= c("County", "Code")) %>%
  select(County, Year, Code, Name, Nature, Housing_Description, Maximum_Density_Allowed, Zoning_Atlas_Definition, Source, geometry) 

#----
#New Kent
NewKent_Zoning <- read_sf(paste0(onedrivepath, "Zoning data/Richmond MSA/New Kent/Zoning.shp")) %>%
  mutate(County = "New Kent",
         Year = 2024) %>%
  rename(Code = ZONING) %>%
  mutate(Code = case_when( #One parcel in the process of rezoning from A to R-3
    is.na(Code) ~ "A1",  #One parcel in disupute and still A-1
    TRUE ~ Code
  ))  %>%
  st_transform(st_crs(CoR_Zoning)) 

#Load zoning description
RVA_Zoning_Descriptions <- read_excel("~/Library/CloudStorage/OneDrive-ThePennsylvaniaStateUniversity/RQ3/RVA-Zoning-Descriptions.xlsx")

#Join descriptions with code
NewKent_Zoning <- NewKent_Zoning %>%
  left_join(RVA_Zoning_Descriptions, by= c("County", "Code")) %>%
  select(County, Year, Code, Name, Nature, Housing_Description, Maximum_Density_Allowed, Zoning_Atlas_Definition, Source, geometry) 

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
# King_William_Zoning <- read_sf(paste0(onedrivepath, "Zoning data/Richmond MSA/King William/King William without WP/King_William_Zoning.shp"))
# 
# #Load West point town point data
# WestPoint_point_parcels <- read_sf(paste0(onedrivepath, "Zoning data/Richmond MSA/King William/Owner Boundaries (Proximity).geojson")) %>%
#   select(LOCALITY, Zoning, geometry)
#   
# #Create centroids for point in polygon
# WestPoint_Centroids <- st_centroid(WestPoint_point_parcels)
# 
# # Convert data frames to sf objects
# WestPoint_Centroids <- st_transform(WestPoint_Centroids, st_crs(King_William_Zoning))
# 
# # King_William_Parcels <- st_as_sf(King_William_Parcels, wkt = "geometry", crs = 4326) # Adjust the WKT column and CRS accordingly
# # WestPoint_Centroids <- st_as_sf(WestPoint_Centroids, coords = c(geometry), crs = 4326)
# 
# #Create the point in polygon count
# joined_sf <- st_join(King_William_Zoning, WestPoint_Centroids, join = st_intersects) 
# 
# #Tidy the merge of point and polygon
# joined_sf <- joined_sf %>%
#   rename(County = LOCALITY.x,
#          Town = LOCALITY.y) %>%
#    mutate(Zoning_Code = coalesce(Code, Zoning)) %>%
#    select(-Code, -Zoning) %>%
#   filter(!st_is_empty(geometry), !is.na(geometry)) %>%
#   mutate(Zoning_Code = ifelse(Zoning_Code == "NA", NA, Zoning_Code)) %>%
#   mutate(Town = ifelse(is.na(Town), "Not West Point", Town)) %>%
#   mutate(Zoning_Code = ifelse(is.na(Zoning_Code) & Town == "Not West Point", "A-C", Zoning_Code))
# 
# #Viz
# ggplot() + 
#   geom_sf(data = joined_sf, aes(fill = Zoning_Code), color = "black", linewidth = 0.05) 

#Export the completed dataset
# st_write(joined_sf, paste0(onedrivepath, "Zoning data/Richmond MSA/King William/King William and West Point/King_William_zoning_draft.shp"))

#Reload zoning data after tidying in QGIS
King_William_Zoning <- read_sf(paste0(onedrivepath, "Zoning data/Richmond MSA/King William/King_William_zoning.shp")) %>%
  rename(Code = Znng_Cd) %>%
  mutate(Year = 2014) %>%
  mutate(County = str_remove(County, " County")) %>%
  mutate(Code = case_when( #One parcel in the process of rezoning from A to R-3
    is.na(Code) ~ "R-1",  #One parcel in disupute and still A-1
    Code == "T-1" ~ "R-1",  #One parcel in disupute and still A-1
    TRUE ~ Code
  ))  %>%
  st_transform(st_crs(CoR_Zoning)) 

#Load zoning description
RVA_Zoning_Descriptions <- read_excel("~/Library/CloudStorage/OneDrive-ThePennsylvaniaStateUniversity/RQ3/RVA-Zoning-Descriptions.xlsx")

#Join descriptions with code
King_William_Zoning <- King_William_Zoning %>%
  left_join(RVA_Zoning_Descriptions, by= c("County", "Code")) %>%
  select(County, Year, Code, Name, Nature, Housing_Description, Maximum_Density_Allowed, Zoning_Atlas_Definition, Source, geometry) 




#Viz above
ggplot() + 
  # geom_sf(data = King_William_Zoning, aes(fill = Zoning_Atlas_Definition), col = "white", linewidth = 0) +
  # geom_sf(data = Petersburg_Zoning, aes(fill = Zoning_Atlas_Definition), col = "white", linewidth = 0) +
  # geom_sf(data = NewKent_Zoning, aes(fill = Zoning_Atlas_Definition), col = "white", linewidth = 0) +
  # geom_sf(data = PrinceGeorge_Zoning, aes(fill = Zoning_Atlas_Definition), col = "white", linewidth = 0) +
  # geom_sf(data = KingQueen_Zoning, aes(fill = Zoning_Atlas_Definition), col = "white", linewidth = 0) +
  # geom_sf(data = Charles_City_Zoning, aes(fill = Zoning_Atlas_Definition), col = "white", linewidth = 0) +
  # geom_sf(data = Hopewell_Zoning, aes(fill = Zoning_Atlas_Definition), col = "white", linewidth = 0) +
  # geom_sf(data = Sussex_Zoning, aes(fill = Zoning_Atlas_Definition), col = "white", linewidth = 0) +
  # geom_sf(data = Powhattan_Zoning, aes(fill = Zoning_Atlas_Definition), col = "white", linewidth = 0) +
  geom_sf(data = Hanover_Zoning, aes(fill = Zoning_Atlas_Definition), col = "white", linewidth = 0) +
  # geom_sf(data = Goochland_Zoning, aes(fill = Zoning_Atlas_Definition), col = "white", linewidth = 0) +
  # geom_sf(data = Colonial_Heights_Zoning, aes(fill = Zoning_Atlas_Definition), col = "white", linewidth = 0) +
  # geom_sf(data = Dinwiddie_Zoning, aes(fill = Zoning_Atlas_Definition), col = "white", linewidth = 0) +
  # geom_sf(data = Amelia_Zoning, aes(fill = Zoning_Atlas_Definition), col = "white", linewidth = 0) +
  # geom_sf(data = Chesterfield_Zoning, aes(fill = Zoning_Atlas_Definition), col = "white", linewidth = 0) + 
  # geom_sf(data = CoR_Zoning, aes(fill = Zoning_Atlas_Definition), col = "white", linewidth = 0) +
  geom_sf(data = Henrico_Zoning, aes(fill = Zoning_Atlas_Definition), col = "white", linewidth = 0) +
  geom_sf(data = Transitional_2020[Transitional_2020$Landscape_Five == "Unstable", ], fill = NA, col = "black", linewidth = 0.5) 
#geom_sf(data = Richmond_Geographies[Richmond_Geographies$Place == "Place", ], fill = NA, col = "black", linewidth = 0.5) 

#To check which codes have missing labels
Missing_Codes <- King_William_Zoning %>%
  filter(is.na(Name))

table(Missing_Codes$Code)

plot(Missing_Codes)



#----
#Petersburg city
Petersburg_Zoning <- read_sf(paste0(onedrivepath, "Zoning data/Richmond MSA/Petersburg/Petersburg_Zoning.shp")) %>%
  mutate(County = "Petersburg",
         Year = 2024) %>%
  rename(Code = T_1l_Ex_23) %>%
  mutate(Code = str_remove(Code, "C"),
         Code = str_remove(Code, "c")) %>%
  filter(!Code == "NA") %>% #three parcels with no data
  mutate(Code = case_when( #One parcel in the process of rezoning from A to R-3
    Code == "R1A" ~ "R-1A",  #One parcel in disupute and still A-1
    TRUE ~ Code
  ))  %>%
  st_transform(st_crs(CoR_Zoning)) 

  
# #Override unclosed rings
# Sys.setenv(OGR_GEOMETRY_ACCEPT_UNCLOSED_RING = "NO")
# 
# #Simplify geometries
# Petersburg_Zoning_simplified <- st_simplify(Petersburg_Zoning, preserveTopology = TRUE)
# 
# #Validate geometries without an end
# Petersburg_Zoning <- st_make_valid(Petersburg_Zoning)


#Load zoning description
RVA_Zoning_Descriptions <- read_excel("~/Library/CloudStorage/OneDrive-ThePennsylvaniaStateUniversity/RQ3/RVA-Zoning-Descriptions.xlsx")

#Join descriptions with code
Petersburg_Zoning <- Petersburg_Zoning %>%
  left_join(RVA_Zoning_Descriptions, by= c("County", "Code")) %>%
  select(County, Year, Code, Name, Nature, Housing_Description, Maximum_Density_Allowed, Zoning_Atlas_Definition, Source, geometry) 


#Viz
ggplot() + 
  # geom_sf(data = Amelia_Zoning, aes(fill = Code), col = "white", linewidth = 0.1) +
  geom_sf(data = Petersburg_Zoning, aes(fill = Zoning_Atlas_Definition), col = "white", linewidth = 0.1) 

#Export above and fill missing/invalid geometries
# st_write(Petersburg_Zoning, paste0(onedrivepath, "Zoning data/Richmond MSA/Petersburg/Petersburg_Untidy.shp"))


#----
#For initial visualizing of Richmond
ggplot() + 
  # geom_sf(data = CentralCities_2020, fill = NA, color = "black", linewidth = 0.65) +
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


#Join all counties into one
Richmond_Zoning <- rbind(Amelia_Zoning, CoR_Zoning, Charles_City_Zoning, Chesterfield_Zoning, 
                         Colonial_Heights_Zoning, Dinwiddie_Zoning, Goochland_Zoning, Hanover_Zoning, 
                         Henrico_Zoning, Hopewell_Zoning, KingQueen_Zoning, King_William_Zoning, 
                         NewKent_Zoning, Petersburg_Zoning, Powhattan_Zoning, PrinceGeorge_Zoning, 
                         Sussex_Zoning)

#Export file
# st_write(Richmond_Zoning, "~/Desktop/Export/Richmond_Zoning.shp")

#At this point, I validate geographies in QGIS using a 0.00001 buffer, then reintroduce into R and sum

#Reload
Richmond_Zoning <- read_sf("~/Desktop/Export_2/Richmond_Zoning.shp") 
  
#Sum Richmond by various columns
Richmond_Zoning <- Richmond_Zoning %>%
  group_by(County, Name, Nature, Hsng_Ds, Mxm_D_A, Znn_A_D) %>%
  summarise(geometry = st_union(geometry)) %>%  # Dissolve by union
  ungroup() %>%
  rename(Hou_Den = Hsng_Ds,
         Max_Den = Mxm_D_A,
         ZA_Def = Znn_A_D) %>%
  st_set_crs(NAD83) %>%
  st_transform(crs = st_crs(CoR_Zoning))

#Export to shapefile
st_write(Richmond_Zoning, "~/Desktop/Export_3/Richmond_Zoning.shp") 

#Reload
Richmond_Zoning <- read_sf(paste0(onedrivepath, "Zoning data/Richmond MSA/Richmond_Complete/Richmond_Zoning.shp"))  


#For initial visualizing of Richmond
ggplot() + 
  # geom_sf(data = CentralCities_2020, fill = NA, color = "black", linewidth = 0.65) +
  # geom_sf(data = CBSA_2020, color = "black", fill = "white", linewidth = 0.6) +
  geom_sf(data = Richmond_Zoning, aes(fill = ZA_Def), col = "white", linewidth = 0) +
  theme_void() +
  theme(legend.spacing.y = unit(.1, "lines")) +
  geom_sf(data = Counties_2020, fill = NA, color = "black", linewidth = 0.6) +
  geom_sf(data = Transitional_2020[Transitional_2020$Landscape_Five == "Unstable", ], fill = NA, col = "black", linewidth = 0.2) 

  

library(readxl)
CP <- read_excel("/Users/billy/Library/CloudStorage/OneDrive-ThePennsylvaniaStateUniversity/RQ3/Timeline-work.xlsx", 
                sheet = 4) 


#land check
intersect(CP$"2013 land", CP$"2023 land")
setdiff(CP$"2013 land", CP$"2023 land")

#use check
intersect(CP$"2013 unique use", CP$"2023 unique use")
setdiff(CP$"2013 unique use", CP$"2023 unique use")

#Zoning check
intersect(CP$"2013 unique zoning", CP$"2023 unique zoning")
setdiff(CP$"2013 unique zoning", CP$"2023 unique zoning")

