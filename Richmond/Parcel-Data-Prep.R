#5/22/24, initiated by BS
#Goal: data tidying of parcel data

#Analysis includes
  #tidying parcel leve data of hanover and henrico co

#Libraries
require(tidyverse)
# require(tidyverse)
# require(sf)
# library(tigris)
library(readxl)

# library(leaflet) #For interactive mapping
# library(htmlwidgets) #To export map
# library(RColorBrewer)
# 
library(data.table)
library(stringr)

# options(tigris_use_cache = TRUE)

#Set parameters (state and year)
ST = "VA"
# YR1 = 1990
# YR2 = 2000
# YR3 = 2010
# YR4 = 2020

CBSA = c("Richmond")
CENTRAL_CITY = c("Richmond city")
GEOG = "tract"

#Create a filepath to OneDrive
onedrivepath="~/OneDrive - The Pennsylvania State University/"

#--------------------------------------------------------------------------------
#Core logic work

#Load data with base R (slow)
# data <- read.delim("/Users/billy/Library/CloudStorage/OneDrive-ThePennsylvaniaStateUniversity/RQ3/External data/pennsylvania_state_university_mortgage_basic2_300000230479868_20210409_103424_VA.txt", 
#                    sep = "|", header = TRUE, stringsAsFactors = FALSE)

# Load with readr (slow)
# data <- read_delim("/Users/billy/Library/CloudStorage/OneDrive-ThePennsylvaniaStateUniversity/RQ3/External data/pennsylvania_state_university_mortgage_basic2_300000230479868_20210409_103424_VA.txt",
#                    delim = "|", col_names = TRUE)


#Load mortgage data
# VA_mortgages <- fread(paste0(normalizePath(onedrivepath), "Mapping Richmond/Parcel-Buildings/pennsylvania_state_university_mortgage_basic2_300000230479868_20210409_103424_VA.txt"), 
#                       sep = "|", header = TRUE) #%>%
#slice(1:25)

# Load Corelogic Property data
VA_property <- fread(paste0(onedrivepath, "Mapping Richmond/Parcel-Buildings/pennsylvania_state_university_property_basic2_300000230479867_20210409_102905_VA.txt"), 
                     sep = "|", header = TRUE) 

#Filter hanover and henrico
Hanover_Henrico_Builds <- VA_property[str_detect(`SITUS COUNTY`, regex("HANOVER|HENRICO", ignore_case = TRUE))]

#Export
#saveRDS(Hanover_Henrico_Builds, "Hanover_Henrico_Buildings.rds")

#Filter relevant data
# Remove columns where ALL values are NA
Hanover_Henrico_Builds <- Hanover_Henrico_Builds[, lapply(.SD, function(col) if (all(is.na(col))) NULL else col)]

#identify columns
Geography <- c("FIPS CODE", "SITUS COUNTY", "SITUS CITY", "COUNTY USE DESCRIPTION", 
               "STATE USE DESCRIPTION", "PARCEL LEVEL LONGITUDE", "PARCEL LEVEL LATITUDE")

Building <- c("ZONING CODE", "ZONING CODE DESCRIPTION", "BUILDING STYLE CODE", "MULTI OR SPLIT PARCEL CODE", 
              "OWNER OCCUPANCY CODE", "ACRES", "UNIVERSAL BUILDING SQUARE FEET", "BUILDING SQUARE FEET", 
              "TOTAL SQUARE FOOTAGE ALL BUILDINGS", "LAND SQUARE FOOTAGE", "YEAR BUILT", "EFFECTIVE YEAR BUILT")

Value <- c("SALE DATE", "SALE RECORDING DATE","TRANSACTION BATCH DATE", "SALE AMOUNT",  
           "TAX AMOUNT", "TAX YEAR", "ASSESSED YEAR", 
           "TOTAL VALUE CALCULATED","LAND VALUE CALCULATED", "IMPROVEMENT VALUE CALCULATED", 
           "ASSESSED TOTAL VALUE", "ASSESSED LAND VALUE", "ASSESSED IMPROVEMENT VALUE", 
           "MARKET TOTAL VALUE", "MARKET LAND VALUE", "MARKET IMPROVEMENT VALUE")

#HANOVER County
#To create a tidy Hanover dataframe
Hanover_CL_Tidy <- Hanover_Henrico_Builds %>%
  filter(`SITUS COUNTY` == "HANOVER") %>%
  select("ORIGINAL APN", Geography, Building, Value) %>%
  rename(PIN = `ORIGINAL APN`) 

#Load in County's own data
Hanover_Buildings_County <- read_excel(paste0(onedrivepath, "/Mapping Richmond/Parcel-Buildings/Hanover_Buildings.xls")) %>%
  rename(PIN = `GPIN #`) %>%
  select("Account Number", "PIN", "Sale Date", "Sale Amount", "Acreage", "Property Use Description", 
         "Year Built", "Sqft", "Zoning")

#Merge CoreLogic and County data
Hanover_Buildings <- Hanover_Buildings_County %>%
  left_join(Hanover_CL_Tidy, by = "PIN") %>%
  mutate(    
    `BUILDING SQUARE FEET` = coalesce(as.numeric(Sqft), `BUILDING SQUARE FEET`),
    `YEAR BUILT` = ifelse(`Year Built` == 0, 
                                       `YEAR BUILT`, 
                                       coalesce(`Year Built`, `YEAR BUILT`)),
    `COUNTY USE DESCRIPTION` = coalesce(`Property Use Description`, `COUNTY USE DESCRIPTION`),
    `ZONING CODE` = coalesce(`Zoning`, `ZONING CODE`),
    ACRES = coalesce(as.numeric(Acreage), ACRES),
    `MARKET TOTAL VALUE` = coalesce(as.numeric(`MARKET TOTAL VALUE`), `Sale Amount`)) %>%
  select(Geography, Building, Value) 
  
#Export data
# saveRDS(Hanover_Buildings, "Hanover_Buildings.rds")

#----
#Henrico County
Henrico_CL_Tidy <- Hanover_Henrico_Builds %>%
  filter(`SITUS COUNTY` == "HENRICO") %>%
  select("ORIGINAL APN", Geography, Building, Value) %>%
  rename(PIN = `ORIGINAL APN`) 

#Load in County's own data
Henrico_Tax_Parcels <- read_csv(paste0(onedrivepath, "/Mapping Richmond/Parcel-Buildings/Henrico_Tax_Parcels.csv")) %>%
  rename(PIN = `CAMA_GPIN`) %>%
  filter(!is.na(USE_CODE))

  select("PIN", LAST_SALE_DATE, LAST_SALE_PRICE, ACREAGE, Use_Description, 
         Year_Built, SQFT_FINISHED)
  
No zoning data found in above county data - check this and the date of info 

