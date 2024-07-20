#5/22/24, initiated by BS
#Goal: To run the segregation analysis

#Analysis includes:
# Rank Order econ seg

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
#Download counties boundaries

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



#Load some zoning data
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

#For initial visualizing of Richmond
ggplot() + 
  geom_sf(data = CoR_Zoning, aes(fill = Code), col = "white") + 
  geom_sf(data = Henrico_Zoning, aes(fill = Code), col = "white") + 
  geom_sf(data = Chesterfield_Zoning, aes(fill = Code), col = "white") + 
  # geom_sf(data = Counties_2020[Counties_2020$NAME == "Chesterfield", ], 
  #         fill = NA, color = "black", linewidth = 0.20) + 
  geom_sf(data = CentralCities_2020, fill = NA, color = "black", linewidth = 0.65) + 
  geom_sf(data = CBSA_2020, fill = NA, color = "black", linewidth = 0.6) + 
  theme_void() +
  theme(legend.spacing.y = unit(.1, "lines")) 

