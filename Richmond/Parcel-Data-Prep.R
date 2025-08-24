#5/22/24, initiated by BS
#Goal: data tidying of parcel data

#Analysis includes
  #tidying parcel level data of hanover and henrico co

#Libraries
require(tidyverse)
library(sf)

#For data handling
library(data.table)
library(stringr)
library(bit64) #change integer to numeric
library(readxl)
library(priceR)

#For data viz
library(scales)
library(ggtext)
library(ggpubr)
library(grid)
library(ggridges)
library(forcats)  
library(tmap)
library(tigris)
library(tidycensus)
library(patchwork)


options(tigris_use_cache = TRUE)


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
# VA_mortgages <- fread(paste0(onedrivepath, "Mapping Richmond/Parcel-Buildings/VA/pennsylvania_state_university_mortgage_basic2_300000230479868_20210409_103424_VA.txt"),
#                       sep = "|", header = TRUE) %>%
# slice(1:25)

# Load Corelogic Property data
VA_property <- fread(paste0(onedrivepath, "Mapping Richmond/Parcel-Buildings/VA/pennsylvania_state_university_property_basic2_300000230479867_20210409_102905_VA.txt"), 
                     sep = "|", header = TRUE) 

#Filter hanover and henrico
Hanover_Henrico_Builds <- VA_property[str_detect(`SITUS COUNTY`, regex("HANOVER|HENRICO", ignore_case = TRUE))]

# #Filter hanover and henrico
# Charlottesville_Builds <- VA_property[str_detect(`SITUS COUNTY`, regex("CHARLOTTESVILLE|ALBEMARLE", ignore_case = TRUE))]
# Charlottesville_Mobile_Home <- VA_property[str_detect(`COUNTY USE DESCRIPTION`, regex("MOBILE", ignore_case = TRUE))] %>%
#   select("TAX ACCOUNT NUMBER", "PARCEL LEVEL LONGITUDE", "PARCEL LEVEL LATITUDE") %>%
#   filter(!is.na(`PARCEL LEVEL LONGITUDE`) & !is.na(`PARCEL LEVEL LATITUDE`))
# 
# # Convert the data frame into an sf object, specifying CRS (e.g., WGS84)
# df_sf <- st_as_sf(Charlottesville_Mobile_Home, coords = c("PARCEL LEVEL LONGITUDE", "PARCEL LEVEL LATITUDE"), crs = 4326) 
# 
# VirginiaCounties <- counties(state = "VA", cb = T, resolution = "500k", year = 2021) %>%
#   filter(NAME == 'Charlottesville'| NAME == 'Albemarle')
# 
# # Plot using ggplot and geom_sf
# ggplot() +
#   geom_sf(data = VirginiaCounties, fill = "darkgrey", color = "black") +  # County background
#   geom_sf(data = df_sf, fill = "white", color = "white", size = 2) +  # Points from latitude/longitude
#   geom_sf(data = df_sf, fill = "white", color = "navy", size = 1.5) +  # Points from latitude/longitude
#   coord_sf(xlim = c(st_bbox(VirginiaCounties)[1], st_bbox(VirginiaCounties)[3]),
#            ylim = c(st_bbox(VirginiaCounties)[2], st_bbox(VirginiaCounties)[4])) +  # Set limits to Virginia counties
#   theme_void() +
#   labs(title = "All home described as Mobile or as Mobile Homes in C'ville and Albemarle")

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
  select("ORIGINAL APN", Geography, Building, Value, `SITUS STREET NAME`, `SITUS MODE`) %>%
  unite("FULL_ADDRESS", `SITUS STREET NAME`, `SITUS MODE`, sep = " ") %>%
  rename(PIN = `ORIGINAL APN`) 

#Load in County's own data
Hanover_Buildings_County <- read_excel(paste0(onedrivepath, "/Mapping Richmond/Parcel-Buildings/Hanover/Hanover_Buildings.xls")) %>%
  rename(PIN = `GPIN #`) %>%
  select("Account Number", "PIN", "Sale Date", "Sale Amount", "Acreage", "Property Use Description", 
         "Year Built", "Sqft", "Zoning")

#Merge CoreLogic and County data
Hanover_Buildings <- Hanover_Buildings_County %>%
  left_join(Hanover_CL_Tidy, by = "PIN") %>%
  # select(PIN, `Sale Amount`, `MARKET TOTAL VALUE`)
  mutate(    
    `BUILDING SQUARE FEET` = coalesce(as.numeric(Sqft), `BUILDING SQUARE FEET`),
    `YEAR BUILT` = ifelse(`Year Built` == 0, 
                                       `YEAR BUILT`, 
                                       coalesce(`Year Built`, `YEAR BUILT`)),
    `COUNTY USE DESCRIPTION` = coalesce(`Property Use Description`, `COUNTY USE DESCRIPTION`),
    `ZONING CODE` = coalesce(`Zoning`, `ZONING CODE`),
    ACRES = coalesce(as.numeric(Acreage), ACRES),
    `MARKET TOTAL VALUE` = coalesce(as.numeric(`MARKET TOTAL VALUE`), `Sale Amount`)) %>%
  select(Geography, Building, Value, FULL_ADDRESS) %>%
  relocate(FULL_ADDRESS, .before = `SITUS COUNTY`) 
    
  

#Export data
# saveRDS(Hanover_Buildings, "Hanover_Buildings.rds")

#----
#Henrico County
Henrico_CL_Tidy <- Hanover_Henrico_Builds %>%
  filter(`SITUS COUNTY` == "HENRICO") %>%
  select("ORIGINAL APN", Geography, Building, Value) %>%
  rename(PIN = `ORIGINAL APN`) 

#Load in County's own data
# Henrico_Tax_Parcels <- read_csv(paste0(onedrivepath, "/Mapping Richmond/Parcel-Buildings/Henrico_Tax_Parcels/Henrico_Tax_Parcels.csv")) %>%
#   rename(PIN = `CAMA_GPIN`) %>%
#   filter(!is.na(USE_CODE)) %>%
#   select("PIN", "FULL_ADDRESS", LAST_SALE_DATE, LAST_SALE_PRICE, ACREAGE, USE_DESCRIPTION, 
#          YEAR_BUILT, SQFT_FINISHED)

#Load shapefile
Henrico_Tax_Parcels_shp <- read_sf(paste0(onedrivepath, "Mapping Richmond/Parcel-Buildings/Henrico/Tax_Parcel_shp/Tax_Parcels_and_CAMA_Data_External.shp")) %>%
  rename(PIN = `CAMA_GPIN`) %>%
  filter(!is.na(USE_CODE)) %>%
  rename(FULL_ADDRESS = FULL_ADDRE,
         LAST_SALE_DATE = LAST_SALE_,
         LAST_SALE_PRICE = LAST_SAL_1,
         USE_DESCRIPTION = USE_DESCRI,
         SQFT_FINISHED = SQFT_FIN_1,) %>%
  select("PIN", FULL_ADDRESS, LAST_SALE_DATE, LAST_SALE_PRICE, PARCEL_ACR, USE_DESCRIPTION, 
         YEAR_BUILT, SQFT_FINISHED)
  
# ggplot() +
#   geom_sf(data = Henrico_Tax_Parcels_shp, fill = "black", linewidth = 0) +
#   theme_minimal()
  
#Load larger area zoning data
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
  st_transform(st_crs(Henrico_Tax_Parcels_shp)) 

#Aggregate parcel level data to larger zoning area
Henrico_Parcel_Zoning <- Henrico_Tax_Parcels_shp %>%
  st_join(Henrico_Zoning, join = st_within) %>%
  mutate(New_Column = Code) %>%
  left_join(Henrico_CL_Tidy, by = "PIN") %>%
  select(PIN, geometry, New_Column, `ZONING CODE`) %>%
  mutate(`ZONING CODE` = na_if(`ZONING CODE`, "")) %>%
  mutate(Zoning_Combined = coalesce(`ZONING CODE`, `New_Column`)) %>%
  select(PIN, Zoning_Combined) %>%
  rename(Zoning = Zoning_Combined) %>%
  st_drop_geometry()

#Combine data above
#Merge CoreLogic and County data
Henrico <- Henrico_Tax_Parcels_shp %>%
  left_join(Henrico_Parcel_Zoning, by = "PIN") %>%
  left_join(Henrico_CL_Tidy, by = "PIN") %>%
    mutate(    
      `BUILDING SQUARE FEET` = coalesce(as.numeric(SQFT_FINISHED), `BUILDING SQUARE FEET`),
      `YEAR BUILT` = ifelse(`YEAR_BUILT` == 0, 
                            `YEAR BUILT`, 
                            coalesce(`YEAR_BUILT`, `YEAR BUILT`)),
      `COUNTY USE DESCRIPTION` = coalesce(`USE_DESCRIPTION`, `COUNTY USE DESCRIPTION`),
      ACRES = coalesce(as.numeric(PARCEL_ACR), ACRES),
      `MARKET TOTAL VALUE` = coalesce(as.numeric(`MARKET TOTAL VALUE`), as.numeric(`LAST_SALE_PRICE`))) %>%
    select(Geography, Building, Value, FULL_ADDRESS, Zoning) %>%
  select(-`ZONING CODE`) %>%
  relocate(FULL_ADDRESS, .before = `SITUS COUNTY`) %>%
  relocate(Zoning, .before = `ZONING CODE DESCRIPTION`) %>%
  rename(`ZONING CODE` = Zoning)

#Save
# saveRDS(Henrico, "Henrico_Buildings.rds")
#------------------------------------------
#RECONSTRUCTING ABOVE DATA
  #To include fewer variables and more consistent data

# Load Corelogic Property data
VA_property <- fread(paste0(onedrivepath, "Mapping Richmond/Parcel-Buildings/VA/pennsylvania_state_university_property_basic2_300000230479867_20210409_102905_VA.txt"), 
                     sep = "|", header = TRUE) 

#Filter hanover and henrico
Hanover_Henrico_Builds <- VA_property[str_detect(`SITUS COUNTY`, regex("HANOVER|HENRICO", ignore_case = TRUE))]

# Remove columns where ALL values are NA
Hanover_Henrico_Builds <- Hanover_Henrico_Builds[, lapply(.SD, function(col) if (all(is.na(col))) NULL else col)]

#identify columns
Geography <- c("FIPS CODE", "SITUS STREET NAME", "SITUS MODE", "SITUS COUNTY", "COUNTY USE DESCRIPTION", 
               "PARCEL LEVEL LONGITUDE", "PARCEL LEVEL LATITUDE")

Building <- c("ZONING CODE", "ZONING CODE DESCRIPTION", "MULTI OR SPLIT PARCEL CODE", "ACRES", 
              "TOTAL SQUARE FOOTAGE ALL BUILDINGS", "LAND SQUARE FOOTAGE", "YEAR BUILT")

Value <- c("SALE DATE", "SALE AMOUNT", "MARKET TOTAL VALUE")

#HANOVER County
#Load Counties zoning data
Hanover_Buildings_County <- read_excel(paste0(onedrivepath, "/Mapping Richmond/Parcel-Buildings/Hanover/Hanover_Buildings.xls")) %>%
  rename(PIN = `GPIN #`) %>%
  select("PIN", "Zoning", "Property Use Description") %>%
  rename("County_Description" = "Property Use Description")

#Load Corelogic data and join zoning data above
Hanover_CL_Tidy <- Hanover_Henrico_Builds %>%
  filter(`SITUS COUNTY` == "HANOVER") %>%
  select("ORIGINAL APN", Geography, Building, Value) %>%
  unite("FULL_ADDRESS", `SITUS STREET NAME`, `SITUS MODE`, sep = " ") %>%
  rename(PIN = `ORIGINAL APN`,
         "CoreLogic_Description" = `COUNTY USE DESCRIPTION`) %>%
  left_join(Hanover_Buildings_County, by = "PIN") %>%
  mutate(Zoning = gsub("-", "", Zoning),
         `ZONING CODE` = na_if(`ZONING CODE`, ""),
         `ZONING CODE` = if_else(is.na(`ZONING CODE`), Zoning, `ZONING CODE`)) %>%
  select(-Zoning) %>%
  mutate(across(where(is.integer64), as.numeric)) %>%
  relocate(County_Description, .before = CoreLogic_Description) 



#Save Henrico
# saveRDS(Hanover_CL_Tidy, "Hanover_Buildings_small.rds")

#HENRICO
#Load shapefile
Henrico_Tax_Parcels_shp <- read_sf(paste0(onedrivepath, "Mapping Richmond/Parcel-Buildings/Henrico/Tax_Parcel_shp/Tax_Parcels_and_CAMA_Data_External.shp")) %>%
  rename(PIN = `CAMA_GPIN`) %>%
  filter(!is.na(USE_CODE)) %>%
  select("PIN", "USE_DESCRI", NUMBER_UNI, geometry) %>%
  rename("County_Description" = "USE_DESCRI",
         "Number_of_Units" = NUMBER_UNI)


#Henrico Zoning
#Henrico
Henrico_Zoning <- read_sf(paste0(onedrivepath, "Zoning data/Richmond MSA/Henrico/Zoning.shp")) %>%
  rename(Code = ZONE_NAME) %>%
  mutate(Code = str_remove(Code, "C$")) %>% #Remove conditional category
  #select(Code, County, Year, geometry) %>%
  mutate(Code = case_when( #One parcel in the process of rezoning from A to R-3
    ZONE_LABEL == "REZ2019-00027" ~ "R-3",  # Update this line as needed
    Code == "x" ~ "A-1",  #One zoning code set to x
    TRUE ~ Code
  )) %>%
  st_transform(st_crs(Henrico_Tax_Parcels_shp)) 

#Aggregate parcel level data to larger zoning area
Henrico_Parcel_Zoning <- Henrico_Tax_Parcels_shp %>%
  st_join(Henrico_Zoning, join = st_within) %>%
  mutate(Zoning_Code = Code) %>%
  select(PIN, Zoning_Code, Number_of_Units, County_Description) %>%
  st_drop_geometry()

#Load Corelogic data and join zoning data above
Henrico_CL_Tidy <- Hanover_Henrico_Builds %>%
  filter(`SITUS COUNTY` == "HENRICO") %>%
  select("ORIGINAL APN", Geography, Building, Value) %>%
  unite("FULL_ADDRESS", `SITUS STREET NAME`, `SITUS MODE`, sep = " ") %>%
  rename(PIN = `ORIGINAL APN`,
         "CoreLogic_Description" = `COUNTY USE DESCRIPTION`) %>%
  left_join(Henrico_Parcel_Zoning, by = "PIN") %>%
  mutate(Zoning_Code = gsub("-", "", Zoning_Code),
         `ZONING CODE` = na_if(`ZONING CODE`, ""),
         `ZONING CODE` = if_else(is.na(`ZONING CODE`), Zoning_Code, `ZONING CODE`)) %>%
  select(-Zoning_Code) %>%
  relocate(County_Description, .before = CoreLogic_Description) 


#Save
saveRDS(Henrico_CL_Tidy, "Henrico_Buildings_small.rds")


#-------------------------
#Hanover
#Select residential codes
Hanover_Res_Zoning_Codes <- c("A1", "AR1", "AR2", "AR6", "R1", "R2", 
                              "R3", "R4", "R5", "R6", "RC", "RM", "RO1", "RR1", "RS")

#Select residential descriptions
Hanover_Res_Units <- c("College-Res", "College-Vac", "Comm/Impr. Residential-Com", 
                       "Condo-Res", "Conversion-Res", "Conversion-Vac", 
                       "Day Care Center-Res", "Dormitory-Vac", "Duplex-Res", "Frat House-Res", 
                       "Fraternal Building-Res", "Library-Res", 
                       "Multiple Residence-Com", "Multiple Residence-Res", "Office Building-Res",
                       "Rec Facil., Community-Res", "S/F Residential-Com", "S/F Residential-Res", 
                       "S/F Residential-Vac", "S/F Residential-Cdu", "Stable-Res", "Townhouse-Res",
                       "Townhouse-Vac", "Vacant Land-Res")
#Load and tidy
Hanover <- read_rds(paste0(onedrivepath, "Mapping Richmond/Parcel-Buildings/Hanover/Hanover_Buildings_small.rds")) %>%
  filter(`ZONING CODE` %in% Hanover_Res_Zoning_Codes) %>%
  filter(CoreLogic_Description == "MULTI-FAMILY" |
           CoreLogic_Description == "SINGLE FAMILY RESID (SUBURBAN)" | 
           CoreLogic_Description == "SINGLE FAMILY RESID (URBAN)") %>%
  filter(County_Description %in% Hanover_Res_Units) %>%
  filter(!(PIN %in% c("7787-95-1158", "7880-02-8611"))) %>%
#Divide parcel value by number of units
  group_by(PIN) %>%
  mutate(PIN_count = n()) %>%
  mutate(Unit_Value = `MARKET TOTAL VALUE` / PIN_count) %>%
  ungroup() %>%
  select(-PIN_count) %>%
  mutate(Code_Age = case_when(
    `ZONING CODE` %in% c("AR1", "AR2", "R1", "R2", 
                         "R3", "R4", "R5", "R6", "RO1", "RR1") ~ "Repealed",
    TRUE ~ "Active"  
  ),
  `ZONING CODE` = case_when(
    `ZONING CODE` %in% c("A1", "AR1", "AR2", "AR6", "R1", "R2", "R3", "R4", "R5", "R6", "RO1", "RR1") ~ 
      str_replace(`ZONING CODE`, "(A|AR|R)([0-9])", "\\1-\\2"),
    TRUE ~ `ZONING CODE`
  ))%>%
  filter(!is.na(`PARCEL LEVEL LATITUDE`)) %>%
  st_as_sf(coords = c("PARCEL LEVEL LONGITUDE", "PARCEL LEVEL LATITUDE"), crs = 4326) 

#Load in concentrated census tracts
Income_LISA_all <- readRDS(file.path(onedrivepath, "Mapping Richmond/Binned Income Data/LISA/LISA_Income_RVA.rds")) %>%
  mutate(lisa_cluster = case_when(
    Pval_i >= 0.05 ~ "Not significant",
    scaled_estimate > 0 & Local_M_i > 0 ~ "High-high", #High income, high-income neighbors
    scaled_estimate > 0 & Local_M_i < 0 ~ "High-low", #High income, low-income neighbors
    scaled_estimate < 0 & Local_M_i > 0 ~ "Low-low", #Low income, low-income neighbors
    scaled_estimate < 0 & Local_M_i < 0 ~ "Low-high" #Low income, high-income neighbors
  )) %>%
  mutate(Tract_type = case_when(
    Med_Income_Adj >= 125000 ~ "Affluent tract",
    Med_Income_Adj <= 26200 ~ "Poor tract",
  )) %>%
  mutate(Concentrations = case_when(
    Pval_i >= 0.05 ~ "Not significant",
    Med_Income_Adj > 26200 & Med_Income_Adj <= 125000 & Local_M_i < 0 ~ "Not significant",  # Middle income
    Med_Income_Adj > 26200 & Med_Income_Adj <= 125000 & Local_M_i > 0 ~ "Middle income clustered",  # Middle income
    Med_Income_Adj >= 125000 & Local_M_i < 0 ~ "High-none", #High income, not concentrated
    Med_Income_Adj >= 125000 & Local_M_i > 0 ~ "High-high", #High income, highly concentrated
    Med_Income_Adj <= 26200 & Local_M_i > 0 ~ "Low-high", #Low income, highly concentrated
    Med_Income_Adj <= 26200 & Local_M_i < 0 ~ "Low-none" #Low income, not concentrated
  )) %>%
  filter(Year == 2020) %>%
  mutate(Facet = if_else(Concentrations == "High-high",
                         "Concentrated affluence",
                         "Non-concentrated affluence")) %>%
  select(GEOID, Landscape_Five, Med_Income_Adj, Facet, geometry)


#Align coord
# Transform tract data to same CRS as parcels if needed
Income_LISA_all <- st_transform(Income_LISA_all, crs = st_crs(Hanover))

#Merge parcel and conc aff
Hanover <- st_join(Hanover, Income_LISA_all[, c("Facet")])   

#Padding missing facets
zoning_levels <- c("A-1", "R-0", "R-1", "R-2", "R-3", "R-4", "R-5", "R-6", "RTH")
facet_levels <- c("Concentrated affluence", "Non-concentrated affluence")

Hanover <- Hanover %>%
  filter(`ZONING CODE` %in% c("RC", "AR-2", "AR-6", "AR-1", "A-1",
                              "RS", "R-2", "R-3", "R-1",
                              "RM", "R-4", "R-5")) %>%
  complete(`ZONING CODE`, Facet)

#Plot Hanover
Hanover_Boxplot <- ggplot(Hanover, aes(x = factor(`ZONING CODE`, 
                               levels = c("A-1", "AR-6", "RC", "RS", "RM",
                                          "AR-1", "AR-2", "R-1", "R-2", 
                                          "R-3", "R-4", "R-5", "R-6", "R-O1", "RR-1")), y = Unit_Value)) +
  facet_grid( . ~ fct_relevel(Code_Age, "Repealed", "Active"), 
              scales = "free_x", space = "free") +
  geom_boxplot(fill = "#fdb462", color = "black") +
  theme_minimal() +
  labs(
    title = "Hanover County",
    y = NULL,
    x = NULL
  ) +
  theme(plot.subtitle = element_text(hjust = 0.5, size = 13, face = "bold"),
        strip.placement = "outside",
        strip.text.x = element_markdown(size = 12, face = "bold"), 
        axis.text.x = element_text(size = 12, angle = 45, hjust = 0.75, vjust = 0.825),
        axis.text.y = element_markdown(size = 14),
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        legend.position = "right",
        axis.title.x = element_blank(),
        axis.title.y = element_markdown(size = 14),
        panel.grid.major.x = element_line(size = 0.2, color = "grey"),
        panel.grid.minor.x = element_line(size = 0),
        panel.grid.major.y = element_line(size = 0.2, color = "grey"),
        panel.grid.minor.y = element_line(size = 0.1),
        panel.border = element_rect(color = "black", fill = NA, size = 0.75)
  )+
  scale_y_continuous(labels = label_dollar(),
                     breaks = c(250000, 500000, 750000, 
                                1000000, 1250000, 1500000)) +
  coord_cartesian(ylim=c(25000,1600000)) 

#Repeat with Henrico
#Hanover
#Select residential codes
Henrico_Res_Zoning_Codes <- c("A1", "R0", "R1", "R1A", "R2", "R2A", "R2AC", "R2C", "R3", "R3A",
                              "R3AC", "R3C", "R4", "R4A", "R4AC", "R5", "R5A", "R5AC", "R5C", "R6", "R6C",
                              "RMP", "RO", "RTH", "RTHC")

#Select residential descriptions
Henrico_Res_Units <- c("APARTMENT", "COMMERCIAL DWELLING", "COMMON AREA (NON-HOA)", 
                       "COMMON AREA/MASTER CARD", "CONDOMINIUM", "COOP", 
                       "HOME OWNERS ASSOCIATION", "IMPROVED COMMON AREA", "MANUFACTURED HOME", "MOBILE HOME PARK", 
                       "RES-IMPROVED < 5 ACRES", "RES-IMPROVED > 100 ACRES", 
                       "RES-IMPROVED 10-20 ACRES", "RES-IMPROVED 20-100 ACRES", "RES-IMPROVED 5-10 ACRES",
                       "RES-SUBD(1 FAM)", "RES-SUBD(2 FAM)", "RES-SUBD(3 FAM)", 
                       "TOWNHOUSE", "VACANT < 5 ACRES", "VACANT > 100 ACRES", "VACANT 10-20 ACRES",
                       "VACANT 20-100 ACRES", "VACANT 5-10 ACRES", "VACANT MULTI-FAMILY", 
                       "VACANT RES (SUB WATERFRONT)", "VACANT RESIDENTIAL")

Henrico_Res_Units <- c("Apartment", "Condominium", 
                       "HOA(Improved)", "Manufactured Home", "Res - Imprv < 5 Acres", 
                       "Res - Imprv > 100 Acres", "Res - Imprv 10 - 20 Acres", "Res - Imprv 20 - 100 Acres", 
                       "Res - Imprv 10 - 20 Acres", 
                       "Res - Imprv 20 - 100 Acres", "Res - Imprv 5 - 10 Acres", 
                       "Res - Subd (1 Fam)", "Res - Subd (2 Fam)", "Res - Subd (3 Fam)",
                       "Townhouse", "Vacant < 5 Acres", "Vacant > 100 Acres", 
                       "Vacant 10 - 20 Acres", "Vacant 20 - 100 Acres", "Vacant 5 - 10 Acres", 
                       "Vacant Common Area (HOA)", "Vacant Multi Fam R5-R6", "Vacant Res (Sub. Wtrfrnt)", 
                       "Vacant Residential")


#Load and tidy
Henrico <- read_rds(paste0(onedrivepath, "Mapping Richmond/Parcel-Buildings/Henrico/Henrico_Buildings_small.rds")) %>%
  # mutate(PIN = str_remove(PIN, "\\.\\d+$")) %>%  # Remove the decimal part
  # mutate(PIN = str_sub(PIN, 1, 12)) %>%
  mutate(`ZONING CODE` = str_replace(`ZONING CODE`, "RO", "R0")) %>%
  filter(`ZONING CODE` %in% Henrico_Res_Zoning_Codes) %>%
  # filter(CoreLogic_Description == "MULTI-FAMILY" |
  #          CoreLogic_Description == "SINGLE FAMILY RESID (SUBURBAN)" | 
  #          CoreLogic_Description == "SINGLE FAMILY RESID (URBAN)") %>%
  filter(County_Description %in% Henrico_Res_Units) %>%
  filter(!(PIN %in% c("734-769-4535", "748-731-9963", "836-667-5251"))) %>%
  filter(!(Number_of_Units %in% c(0))) %>%
  mutate(Number_of_Units = ifelse(is.na(Number_of_Units), 1, Number_of_Units)) %>%
  #Divide parcel value by number of units
  mutate(Unit_Value = `MARKET TOTAL VALUE` / Number_of_Units) %>%
  mutate(Code_Age = case_when(
    `ZONING CODE` %in% c("A1", "R0", "R1", "R1A", "R2", "R2A", "R2AC", 
                         "R2C", "R3", "R3A", "R3AC", "R3C", "R4", "R4A", "R4AC", "RMP") ~ "Single-family exclusive",
    `ZONING CODE` %in% c("R5", "R5A", "R5AC", "R5C", "R6", 
                         "R6C", "RTH", "RTHC") ~ "Single family and multifamily",
    TRUE ~ "Active"  
  ),
  `ZONING CODE` = case_when(
    `ZONING CODE` %in% c("A1", "R0", "R1", "R1A", "R2", "R2A", "R2AC", 
                         "R2C", "R3", "R3A", "R3AC", "R3C", "R4", "R4A","R5", "R5A", "R5AC", "R5C", "R6", 
                         "R6C", "RTH", "RTHC", "R4AC", "RMP") ~ 
      str_replace(`ZONING CODE`, "(A|AR|R)([0-9])", "\\1-\\2"),
    TRUE ~ `ZONING CODE`
  )) %>%
  filter(!is.na(`PARCEL LEVEL LATITUDE`)) %>%
  st_as_sf(coords = c("PARCEL LEVEL LONGITUDE", "PARCEL LEVEL LATITUDE"), crs = 4326) 

#Load in concentrated census tracts
Income_LISA_all <- readRDS(file.path(onedrivepath, "Mapping Richmond/Binned Income Data/LISA/LISA_Income_RVA.rds")) %>%
  mutate(lisa_cluster = case_when(
    Pval_i >= 0.05 ~ "Not significant",
    scaled_estimate > 0 & Local_M_i > 0 ~ "High-high", #High income, high-income neighbors
    scaled_estimate > 0 & Local_M_i < 0 ~ "High-low", #High income, low-income neighbors
    scaled_estimate < 0 & Local_M_i > 0 ~ "Low-low", #Low income, low-income neighbors
    scaled_estimate < 0 & Local_M_i < 0 ~ "Low-high" #Low income, high-income neighbors
  )) %>%
  mutate(Tract_type = case_when(
    Med_Income_Adj >= 125000 ~ "Affluent tract",
    Med_Income_Adj <= 26200 ~ "Poor tract",
  )) %>%
  mutate(Concentrations = case_when(
    Pval_i >= 0.05 ~ "Not significant",
    Med_Income_Adj > 26200 & Med_Income_Adj <= 125000 & Local_M_i < 0 ~ "Not significant",  # Middle income
    Med_Income_Adj > 26200 & Med_Income_Adj <= 125000 & Local_M_i > 0 ~ "Middle income clustered",  # Middle income
    Med_Income_Adj >= 125000 & Local_M_i < 0 ~ "High-none", #High income, not concentrated
    Med_Income_Adj >= 125000 & Local_M_i > 0 ~ "High-high", #High income, highly concentrated
    Med_Income_Adj <= 26200 & Local_M_i > 0 ~ "Low-high", #Low income, highly concentrated
    Med_Income_Adj <= 26200 & Local_M_i < 0 ~ "Low-none" #Low income, not concentrated
  )) %>%
  filter(Year == 2020) %>%
  mutate(Facet = if_else(Concentrations == "High-high",
                                           "Concentrated affluence",
                                           "Non-concentrated affluence")) %>%
  select(GEOID, Landscape_Five, Med_Income_Adj, Facet, geometry)


#Align coord
# Transform tract data to same CRS as parcels if needed
Income_LISA_all <- st_transform(Income_LISA_all, crs = st_crs(Henrico))

#Merge parcel and conc aff
Henrico <- st_join(Henrico, Income_LISA_all[, c("Facet")])  



#Plot Hanover
Henrico_Boxplot <- ggplot(Henrico, aes(x = factor(`ZONING CODE`, 
                               levels = c("A-1", "R-0", "R-1", "R-1A", "R-2", "R-2A", "R-2AC", 
                                          "R-2C", "R-3", "R-3A", "R-3AC", "R-3C", "R-4", "R-4A", "R-4AC", 
                                          "R-5", "R-5A", "R-5AC", "R-5C", "R-6", "R-6C",
                                          "RMP", "R-O", "RPN", "RTH", "RTHC")), y = Unit_Value)) +
  geom_boxplot(fill = "#80b1d3", color = "black") +
  facet_grid( . ~ fct_relevel(Code_Age, "Single-family exclusive", "Single family and multifamily"), 
              scales = "free_x", space = "free") +
  theme_minimal() +
  labs(
    title = "Henrico County",
    y = NULL,
    x = NULL
  ) +
  theme(plot.subtitle = element_text(hjust = 0.5, size = 13, face = "bold"),
        strip.placement = "outside",
        strip.text.x = element_markdown(size = 12, face = "bold"), 
        axis.text.x = element_text(size = 12, angle = 45, hjust = 0.75, vjust = 0.825),
        axis.text.y = element_markdown(size = 14),
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        legend.position = "right",
        axis.title.x = element_blank(),
        axis.title.y = element_markdown(size = 14),
        panel.grid.major.x = element_line(size = 0.2, color = "grey"),
        panel.grid.minor.x = element_line(size = 0),
        panel.grid.major.y = element_line(size = 0.2, color = "grey"),
        panel.grid.minor.y = element_line(size = 0.1),
        panel.border = element_rect(color = "black", fill = NA, size = 0.75)
  ) +
  scale_y_continuous(labels = label_dollar(),
                     breaks = c(500000, 1000000, 1500000, 2000000, 2500000, 
                                3000000, 3500000, 4000000, 4500000, 5000000)) +
  coord_cartesian(ylim=c(100000,4500000)) 
  

#Plot together
combined_plot <- ggarrange(Hanover_Boxplot, Henrico_Boxplot, 
                           heights = c(0.5, 1),
                           widths = c(0.7, 1),
                           ncol = 1, 
                           align = "v",
                           common.legend = TRUE, legend = "right")

# Add a common Y-axis label using annotate_figure
annotate_figure(combined_plot,
                left = grid::textGrob(expression("Parcel unit value"), 
                                      rot = 90, vjust = 1, 
                                      gp = gpar(fontsize = 14)))

# ggsave("Parcel_Boxplot.png",
#        path = "~/desktop",
#        width = 12,
#        height = 12,
#        units = "in",
#        dpi = 500)


#Geom density mapping

Hanover_Density <- ggplot(Hanover 
                          %>%
                            filter((`ZONING CODE` %in% c("RC", "A-1", "AR-1",  # First group
                                                         "RS", "R-2", "R-3", "R-1",     # Second group
                                                         "RM", "R-4", "R-5"))), 
                          aes(x = Unit_Value, y = Facet)) +
                                      # factor(`ZONING CODE`, 
                                      #     levels = rev(c("A-1", "AR-6", "RC", "RS", "RM",
                                      #                    "AR-1", "AR-2", "R-1", "R-2", 
                                      #                    "R-3", "R-4", "R-5", "R-6", "R-O1", "RR-1"))))) +
  geom_density_ridges(quantile_lines = TRUE, quantiles = 2,
                      scale = 1.1,
                      rel_min_height = 0.005, 
                      aes(fill = Facet), col = "black") + 
  facet_grid(fct_relevel(`ZONING CODE`) ~ .,
             scales = "free_y", space = "free",
             switch = "y"
  ) +
  # facet_grid(fct_relevel(Code_Age, "Active", "Repealed") ~ .,
  #            scales = "free_y", space = "free",
  #            switch = "y") +
  theme_minimal(base_size = 14) + 
  scale_x_continuous(labels = label_dollar(),
                     breaks = c(0, 250000, 500000, 750000, 1000000, 1250000, 
                                1500000, 1750000, 2000000, 2500000, 
                                3000000, 3500000, 4000000, 4500000, 5000000)) +
  coord_cartesian(xlim=c(00000,2000000)) +
  theme_minimal() +
  scale_fill_manual(values = c("Concentrated affluence" = "#7f3b08",
                               "Non-concentrated affluence" = "grey"),
                    name = NULL, guide = "none") +
  labs(
    subtitle = "Hanover County",
    y = NULL,
    x = NULL
  ) +
  theme(plot.subtitle = element_text(hjust = 0.5, size = 14, face = "bold"),
        strip.placement = "outside",
        strip.text.y = element_markdown(size = 12, face = "bold"), 
        axis.text.x = element_text(size = 11, angle = 45, hjust = 0.75, vjust = 0.825),
        # axis.text.y = element_markdown(size = 12),
        axis.text.y = element_blank(),
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        legend.position = "right",
        axis.title.x = element_blank(),
        axis.title.y = element_markdown(size = 14),
        panel.grid.major.x = element_line(size = 0.2, color = "darkgrey"),
        panel.grid.minor.x = element_line(size = 0.2, color = "lightgrey"),
        panel.grid.major.y = element_line(size = 0.2, color = "grey"),
        panel.grid.minor.y = element_line(size = 0.1),
        panel.border = element_rect(color = "black", fill = NA, size = 0.75)
  ) 


Henrico_Density <- ggplot(Henrico %>%
                            filter((`ZONING CODE` %in% c("A-1", "R-0", "R-1", "R-2", "R-3", "R-4", "R-5", "R-6", "RTH"))), 
       aes(x = Unit_Value, y = Facet)) +
           # y = factor(`ZONING CODE`, 
           #                            levels = rev(c("A-1", "R-0", "R-1", "R-1A", "R-2", "R-2A", "R-2AC", 
           #                                           "R-2C", "R-3", "R-3A", "R-3AC", "R-3C", "R-4", "R-4A", "R-4AC", 
           #                                           "R-5", "R-5A", "R-5AC", "R-5C", "R-6", "R-6C",
           #                                           "RMP", "R-O", "RPN", "RTH", "RTHC"))))) +
  geom_density_ridges(quantile_lines = TRUE, quantiles = 2,
                      scale = 1.1,
                      rel_min_height = 0.005, 
                      aes(fill = Facet), col = "black") + 
  facet_grid(fct_rev(`ZONING CODE`) ~ .,
             scales = "free_y", space = "free",
             # switch = "y"
             ) +
  # facet_grid(fct_relevel(Code_Age, "Single-family exclusive", "Single family and multifamily") ~ .,
  #            scales = "free_y", space = "free",
  #            # switch = "y"
  #            ) +
  theme_minimal(base_size = 14) + 
  scale_fill_manual(values = c("Concentrated affluence" = "#7f3b08",
                               "Non-concentrated affluence" = "grey"),
                    name = NULL, guide = "none") +
  scale_x_continuous(labels = label_dollar(),
                     breaks = c(0, 250000, 500000, 750000, 1000000, 1250000, 
                                1500000, 1750000, 2000000, 2500000, 
                                3000000, 3500000, 4000000, 4500000, 5000000)) +
  coord_cartesian(xlim=c(00000,2000000)) +
  theme_minimal() +
  labs(
    subtitle = "Henrico County",
    y = NULL,
    x = NULL
  ) +
  theme(plot.subtitle = element_text(hjust = 0.5, size = 14, face = "bold"),
        strip.placement = "outside",
        strip.text.y = element_markdown(size = 12, face = "bold"), 
        axis.text.x = element_text(size = 11, angle = 45, hjust = 0.75, vjust = 0.825),
        axis.text.y = element_blank(),
        # axis.text.y = element_markdown(size = 12),
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        legend.position = "right",
        axis.title.x = element_blank(),
        axis.title.y = element_markdown(size = 14),
        panel.grid.major.x = element_line(size = 0.2, color = "darkgrey"),
        panel.grid.minor.x = element_line(size = 0.2, color = "lightgrey"),
        panel.grid.major.y = element_line(size = 0.2, color = "grey"),
        panel.grid.minor.y = element_line(size = 0.1),
        panel.border = element_rect(color = "black", fill = NA, size = 0.75)
  ) 

#Plot together
combined_plot <- ggarrange(
  Hanover_Density, Henrico_Density, 
  widths = c(0.5, 0.5),
  ncol = 2, 
  align = "v",
  common.legend = TRUE, legend = "right"
)

# Add title with color formatting
final_plot <- combined_plot + 
  plot_annotation(
    title = "Parcel values across areas of<br><span style='color:#7f3b08;'>concentrated affluence</span> and those <span style='color:darkgrey;'>not concentrated affluence</span>",
    theme = theme(
      plot.title = element_markdown(size = 20, face = "bold", hjust = 0.5)
    )
  )


ggsave("Conc_Aff_Parcel_Plot.jpg",
       path = "~/desktop",
       width = 10,
       height = 12,
       units = "in",
       dpi = 500)

#Line chart tracking prices
Henrico_Med_Test <- Henrico %>%
  mutate(`SALE YEAR` = as.numeric(substr(`SALE DATE`, 1, 4))) %>%
  filter(`SALE AMOUNT` <= 2000000) %>%   
  filter(!`ZONING CODE` %in% c("RMH", "RMP")) %>%
  mutate(Zoning_Protection = case_when(
    `ZONING CODE` %in% c("R-5", "R-5A", "R-5AC", "R-5C", "R-6", "R-6C", "RTH", "RTHC", "A-1") ~ "General residence district",
    `ZONING CODE` %in% c("R-0", "R-1", "R-2", "R-3", "R-4",
                                "R-1A", "R-2A", "R-3A", "R-4A", 
                                 "R-2C", "R-2AC", "R-2AC", "R-3AC", "R-3C", "R-4AC") ~ "Districts that 'provide and protect'",
    TRUE ~ NA_character_  # Fallback for any other values not captured
  )) %>%
  mutate(
    Lot_Size = case_when(
      ACRES > 0   & ACRES <= 0.25  ~ "0 - 0.25 acres",
      ACRES > 0.25 & ACRES <= 0.75     ~ "0.25 - 0.5 acres",
      ACRES > 0.75                    ~ "Larger than 0.75 acres",
      TRUE                         ~ NA_character_  # Handles missing or 0 values
    )
  ) %>%
  arrange(ACRES) %>%  # optional: explicitly sort first
  mutate(Acres_Tertile = ntile(ACRES, 3)) %>%
  filter(!is.na(Lot_Size)) %>%
  filter(!str_detect(PIN, "803-680-7933|763-730-8976|748-732-4135|754-731-8217")) %>%
  # filter(`SALE YEAR` >= 1925) %>%   
  group_by(`SALE YEAR`, 
           `Facet`, 
           # `ZONING CODE`, 
           Lot_Size
           # Code_Age
           ) %>%  
  summarise(Median_Unit_Value = median(`SALE AMOUNT`, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    Facet = str_replace_all(
      as.character(Facet),
      c(
        "Non-concentrated affluence" = "Areas not concentrated affluence",
        "Concentrated affluence" = "Concentrated affluence")))

#Below code edits data for a facet of all values by code but is filtered out here
  # mutate(Lot_Size_Description = case_when(
  #   `ZONING CODE` %in% c("A-1", "R-0", "R1", "R-1A") ~ "A-1, R-0, and R-1",
  #   `ZONING CODE` %in% c("R-2", "R-2A", "R-2AC", "R-2C") ~ "R-2",
  #   `ZONING CODE` %in% c("R-3", "R-3A", "R-3AC", "R-3C") ~ "R-3",
  #   `ZONING CODE` %in% c("R-4", "R-4A", "R-4AC") ~ "R-4",
  #   `ZONING CODE` %in% c("R-5", "R-5A", "R-5AC", "R-5C") ~ "R-5",
  #   `ZONING CODE` %in% c("R-6", "R-6C", "RTH", "RTHC") ~ "R-6 and Townhouses",
  #   TRUE ~ NA_character_  # Fallback for any other values not captured
  # )) %>%
  # mutate(Color = case_when(
  #   `ZONING CODE` %in% c("R-1A", "R-2A", "R-3A", "R-4A", "R-5A") ~ "A subcodes",
  #   `ZONING CODE` %in% c("R-2AC", "R-3AC", "R-4AC", "R-5AC", "R-5A") ~ "AC subcodes",
  #   `ZONING CODE` %in% c("R-2C", "R-3C", "R-5C", "R-6C") ~ "C subcodes",
  #   `ZONING CODE` %in% c("RTH", "RTHC") ~ "Townhouses",
  #   `ZONING CODE` %in% c("R-1", "R-2", "R-3", "R-4", "R-5", "R-6") ~ "Standard R code",
  #   `ZONING CODE` %in% c("A-1") ~ "A-1",
  #   `ZONING CODE` %in% c("R-0") ~ "R-0",
  #   TRUE ~ NA_character_  # Fallback for any other values not captured
  # ))  %>%
  # mutate(Linetype = case_when(
  #   `ZONING CODE` %in% c("R-1A", "R-2A", "R-3A", "R-4A", "R-5A", 
  #                        "R-2AC", "R-3AC", "R-4AC", "R-5AC", "R-5A", 
  #                        "R-2C", "R-3C", "R-5C", "R6C") ~ "A, C, and AC",
  #   `ZONING CODE` %in% c("RTH", "RTHC") ~ "Townhouses",
  #   `ZONING CODE` %in% c("R-1", "R-2", "R-3", "R-4", "R-5", "R-6") ~ "Standard R code",
  #   `ZONING CODE` %in% c("A-1") ~ "A-1",
  #   `ZONING CODE` %in% c("R-0") ~ "R-0",
  #   TRUE ~ NA_character_  # Fallback for any other values not captured
  # )) %>%
  # mutate(Lot_Size = case_when(
  #   `ZONING CODE` %in% c("R-5", "R-6", "RTH", "RTHC") ~ "Mulitfamily and single-family zoning",
  #   `ZONING CODE` %in% c("R-2", "R-3", "R-4") ~ "SF housing, minimum lot 8,000 feet²",
  #   `ZONING CODE` %in% c("R-0", "A-1", "R-1") ~ "SF housing, minimum lot 25,000 feet²",
  #   TRUE ~ NA_character_  # Fallback for any other values not captured
  # )) 
  # 
  
 #Inflation adjust above figures 
Henrico_Med_Test <- Henrico_Med_Test %>% 
  mutate(Median_Unit_Value_Inf = adjust_for_inflation(Median_Unit_Value, 
                                                  `SALE YEAR`, "US", to_date = 2020)) %>%
  filter(`Median_Unit_Value_Inf` <= 1500000)   # Exclude sale amounts greater than 5,000,000
  
  

#Median sale value by year (and code) adjusted to 2020 dollars
ggplot(Henrico_Med_Test 
       # %>%
       #   filter((`ZONING CODE` %in% c("A-1", "R-0", "R-1", "R-2", "R-3", "R-4", "R-5", "R-6", "RTH")))
       , 
       aes(x = `SALE YEAR`, y = Median_Unit_Value_Inf, color = `Facet`, 
                     group = `Facet`)) +
  geom_line(size = 1) +
  facet_grid(fct_rev(`Lot_Size`) ~ .,
             # scales = "free_y", 
             space = "free",
             switch = "y") +
  # facet_grid(fct_relevel(Lot_Size_Description, "R-6 and Townhouses", "R-5", "R-4", "R-3", "R-2", "A-1, R-0, and R-1") ~ .,
  #            # scales = "free_y", space = "free",
  #            switch = "y"
  # ) +
  # geom_rect(aes(xmin = start, xmax = above200_end, ymin = 0, ymax = Inf,
  #               fill = Transitional), col = NA, alpha = 1) +  
  # geom_vline(xintercept = seq(0, 36.5, by = 5), color = "black", alpha = 0.5, linetype = "solid", size = 0.2) +  # geom_vline(xintercept = 8.94, color = 'darkgrey', linetype = 'solid', linewidth = 0.25) +
  # geom_vline(xintercept = distances$x, color = "black", linetype = "longdash", size = 1) +
  geom_vline(xintercept = 1960, color = "black", linetype = "solid", size = 0.75) +
  geom_text(data = Henrico_Med_Test %>% filter(`Lot_Size` == "Larger than 0.75 acres"),  # Filtering inside the layer
            aes(x = 1960.25, y = 750000, angle = 0, label = "1960 zoning ordinance"),
            hjust = 0, color = "black", size = 3.5) +
  geom_vline(xintercept = 2021, color = "black", linetype = "solid", size = 0.75) +
  geom_text(data = Henrico_Med_Test %>% filter(`Lot_Size` == "Larger than 0.75 acres"),  # Filtering inside the layer
            aes(x = 2008.5, y = 750000, angle = 0, label = "2021 zoning ordinance"),
            hjust = 0, color = "black", size = 3.5) +
  # scale_fill_manual(values = c("Urban" = "#c8edc7", "Unstable" = "#e8c2ed", "Suburban" = "#fae3c5"), guide = "none") +
  # geom_smooth(span = 0.1, method = "loess", fill = "lightgrey", alpha = 0, size = 0.85) +
  # geom_hline(yintercept = 1, color = 'black', linetype = 'dashed') +
  theme_minimal() +
  scale_y_continuous(labels = label_dollar(),
                     breaks = c(0, 250000, 500000, 750000),
                     position = "right") +
  scale_color_manual(values = c("Areas not concentrated affluence" = "grey",
                                "Concentrated affluence" = "#7f3b08"),
                     name = NULL, guide = "none") +
  # scale_color_manual(values = c("General residence district" = "#377eb8",
  #                               "Districts that 'provide and protect'" = "#e41a1c"),
  #                    name = NULL, guide = "none") +
  # scale_color_manual(values = c("Areas of concentrated affluence" = "#7f3b08",
  #                              "Areas not concentrated affluence" = "darkgrey"),
  #                   name = NULL, guide = "none") +
  # scale_color_manual(values = c("Standard R code" = "black",
  #                               "A subcodes" = "#1f78b4",
  #                               "AC subcodes" = "#a6cee3",
  #                               "C subcodes" = "#bdbdbd",
  #                               "Townhouses" = "#998ec3",
  #                               "A1" = "#1b9e77",
  #                               "R0" = "#d95f02"),
  #                    name = NULL) +
  # scale_linetype_manual(values = c("A, C, and AC" = "dashed",
  #                               "Standard R code" = "solid",
  #                               "Townhouses" = "dashed",
  #                               "A1" = "solid",
  #                               "R0" = "solid"),
  #                       guide = "none") +
  # labs(x = "Year of sale",
  #      y = "Median parcel unit sale value",
  #      subtitle = NULL,
  #      # caption = "Color shading represents <span style='color:#4daf4a;'>urban</span>, 
  #      # <span style='color:#984ea3;'>transitional</span>, and 
  #      # <span style='color:#fdbf6f;'>suburban</span> census tracts"
  #      ) +
  labs(title = "Henrico County",
       # subtitle = "Areas of <span style='color:#7f3b08;'>concentrated affluence</span> and areas <span style='color:darkgrey;'>not of concentrated affluence</span>",
       subtitle = "Parcel values across areas of<br><span style='color:#7f3b08;'>concentrated affluence</span> and those <span style='color:darkgrey;'>not concentrated affluence</span>",
       x = NULL,
       y = "Median parcel sale value",
       caption = "All sales adjusted to 2020 dollars"
  ) +
  # scale_y_continuous(labels = label_dollar(),
  #                    breaks = c(0, 250000, 500000, 750000, 1000000, 1250000, 1500000),
  #                    position = "right") +
  scale_x_continuous(breaks = c(1950, 1960, 1970, 1980, 1990, 2000, 2010, 2020)) +
  coord_cartesian(xlim=c(1952, 2020)) +
  theme(plot.subtitle = element_markdown(hjust = 0.5, size = 13, face = "bold"),
        strip.placement = "outside",
        strip.text.y = element_markdown(size = 12, face = "bold"), 
        axis.text.x = element_text(size = 11, hjust = 0.5, vjust = 0.825),
        axis.text.y = element_markdown(size = 12),
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        legend.position = "right",
        axis.title.x = element_blank(),
        axis.title.y = element_markdown(size = 14),
        panel.grid.major.x = element_line(size = 0.2, color = "darkgrey"),
        panel.grid.minor.x = element_line(size = 0.2, color = "lightgrey"),
        panel.grid.major.y = element_line(size = 0.2, color = "grey"),
        panel.grid.minor.y = element_line(size = 0.1),
        panel.border = element_rect(color = "black", fill = NA, size = 0.75)
  ) 
  # +
  # guides(color = guide_legend(override.aes = list(linetype = c("dashed", "solid", "dashed", "dashed", "solid", "solid", "solid"),
  #                                                 size = 1.5)),  # Increase line width in the legend
  #        title = element_blank()) +

  
ggsave("Henrico_Sales_By_Acre.png",
       path = "~/desktop",
       width = 10,
       height = 10,
       units = "in",
       dpi = 500)

#median sale value of parcel by zoning code in that year
#Henrico
ggplot(Henrico_Med_Test %>%
         mutate(`ZONING CODE` = fct_relevel(`ZONING CODE`, 
                                            "R-0", "R-1", "A-1",    # Last group
                                            "R-2", "R-3", "R-4",   # Second group
                                            "RTH", "R-5", "R-6"  # First group
         )) %>%
         filter(`ZONING CODE` %in% c("A-1", "R-0", "R-1", "R-2", "R-3", "R-4", "R-5", "R-6", "RTH")), 
       aes(x = `SALE YEAR`, y = Median_Unit_Value_Inf, color = `ZONING CODE`, 
           group = `ZONING CODE`, linetype = `ZONING CODE`)) +
  facet_grid(fct_relevel(Lot_Size, "SF housing, minimum lot 25,000 feet²", 
                                   "SF housing, minimum lot 8,000 feet²",
                          "Mulitfamily and single-family zoning",
                         ) ~ .,
             # scales = "free_y", space = "free",
             switch = "y"
  ) +
  geom_vline(xintercept = 1933, color = "darkgrey", linetype = "solid", size = 0.75) +
  geom_text(data = Henrico_Med_Test %>% filter(Lot_Size == "Mulitfamily and single-family zoning"),  # Filtering inside the layer
    aes(x = 1932, y = 300000, angle = 90, label = "First zoning ordinance of 1933"), 
    hjust = 0, color = "black", size = 3.5
  ) +  
  geom_vline(xintercept = 1942, color = "darkgrey", linetype = "solid", size = 0.75) +
  geom_text(data = Henrico_Med_Test %>% filter(Lot_Size == "Mulitfamily and single-family zoning"),  # Filtering inside the layer
            aes(x = 1941, y = 500000, angle = 90, label = "1942 ordinance"), 
            hjust = 0, color = "black", size = 3.5) +
  
  geom_vline(xintercept = 1945, color = "darkgrey", linetype = "solid", size = 0.75) +
  geom_text(data = Henrico_Med_Test %>% filter(Lot_Size == "Mulitfamily and single-family zoning"),  # Filtering inside the layer
            aes(x = 1944, y = 500000, angle = 90, label = "1945 ordinance"), 
            hjust = 0, color = "black", size = 3.5) +
  geom_vline(xintercept = 1960, color = "darkgrey", linetype = "solid", size = 0.75) +
  geom_text(data = Henrico_Med_Test %>% filter(Lot_Size == "Mulitfamily and single-family zoning"),  # Filtering inside the layer
            aes(x = 1959, y = 500000, angle = 90, label = "1960 ordinance"), 
            hjust = 0, color = "black", size = 3.5) +
  geom_vline(xintercept = 2022, color = "darkgrey", linetype = "solid", size = 0.75) +
  geom_text(data = Henrico_Med_Test %>% filter(Lot_Size == "Mulitfamily and single-family zoning"),  # Filtering inside the layer
            aes(x = 2021, y = 500000, angle = 90, label = "2022 zoning ordinance"), 
            hjust = 0, color = "black", size = 3.5) +
  geom_line(col = "white", size = 1.75) +
  geom_line(size = 0.8, , lineend = "round", linejoin = "round") +
  geom_hline(yintercept = 0, color = 'darkgrey', linetype = 'solid') +
  theme_minimal() +
  scale_y_continuous(labels = label_dollar(),
                     breaks = c(0, 250000, 500000, 750000, 1000000, 1250000, 1500000),
                     position = "right") +
  scale_x_continuous(breaks = c(1950, 1960, 1970, 1980, 1990, 2000, 2010, 2020)) +
  coord_cartesian(xlim=c(1952, 2020)) +
  scale_color_manual(values = c("A-1" = "black",
                                "R-0" = "#1b9e77",
                                "R-2" = "#7570b3",
                                "R-3" = "#e7298a",
                                "R-4" = "#66a61e",
                                "R-5" = "#e6ab02",
                                "R-6" = "red",
                                "RTH" = "#a6761d",
                                "R-1" = "#d95f02"),
                     name = "Parcel's current\nzoning code") +
  scale_linetype_manual(values = c("A-1" = "dashed",
                                   "R-0" = "solid",
                                   "R-1" = "solid",
                                   "R-2" = "solid",
                                   "R-3" = "solid",
                                   "R-4" = "solid",
                                   "R-5" = "solid",
                                   "R-6" = "solid",
                                   "RTH" = "dashed"),
                        name = "Parcel's current\nzoning code") +
  labs(title = "Henrico County",
       x = NULL,
       y = "Median parcel unit sale value",
       subtitle = NULL,
       caption = "All sales adjusted to 2020 dollars"
  ) +
  theme(plot.subtitle = element_text(hjust = 0.5, size = 13, face = "bold"),
        strip.placement = "outside",
        strip.text.y = element_markdown(size = 12, face = "bold"), 
        axis.text.x = element_text(size = 11),
        axis.text.y = element_markdown(size = 12),
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        legend.position = "right",
        axis.title.x = element_markdown(size = 14),
        axis.title.y = element_markdown(size = 14),
        panel.grid.major.x = element_line(size = 0.2, color = "darkgrey"),
        panel.grid.minor.x = element_line(size = 0.2, color = "grey"),
        panel.grid.major.y = element_line(size = 0.2, color = "grey"),
        panel.grid.minor.y = element_line(size = 0.1),
        panel.border = element_rect(color = "black", fill = NA, size = 0.75),
        legend.text = element_text(size = 14),
        legend.key.size = unit(2, 'lines'),           # Increase the size of the legend key
        legend.spacing = unit(4.0, 'cm')        
  ) 

# ggsave("Henrico_R_Parcel_Sales_Facet.jpg",
#        path = "~/desktop",
#        width = 12,
#        height = 10,
#        units = "in",
#        dpi = 500)



#Hanover county
Hanover_Med_Test <- Hanover %>%
  mutate(`SALE YEAR` = as.numeric(substr(`SALE DATE`, 1, 4))) %>%
  # filter(`SALE AMOUNT` <= 2000000) %>%   
  # filter(!`ZONING CODE` == "RR-1") %>%
  # mutate(Lot_Size = case_when(
  #   `ZONING CODE` %in% c("A-1", "AR-1", "AR-2", "AR-6", "RC") ~ "Agricultural/rural zoning",
  #   `ZONING CODE` %in% c("R-1", "R-2", "R-3", "RS") ~ "Single-family zoning",
  #   `ZONING CODE` %in% c("R-4", "R-5", "RM") ~ "Mulitfamily and single-family zoning",
  #   TRUE ~ NA_character_  # Fallback for any other values not captured
  # )) %>%
  mutate(
    Lot_Size = case_when(
      ACRES > 0   & ACRES <= 0.25  ~ "0 - 0.25 acres",
      ACRES > 0.25 & ACRES <= 0.75     ~ "0.25 - 0.75 acres",
      ACRES > 0.75                    ~ "Larger than 0.75 acres",
      TRUE                         ~ NA_character_  # Handles missing or 0 values
    )
  ) %>%
  filter(!PIN %in% c("7815-96-6285")) %>%
  filter(!(`SALE YEAR` %in% c(2003, 2004, 2006) & `SALE AMOUNT` < 145000)) %>%
  arrange(ACRES) %>%  # optional: explicitly sort first
  mutate(Acres_Tertile = ntile(ACRES, 3)) %>%
  filter(!is.na(Lot_Size)) %>%
  # filter(`SALE YEAR` >= 1925) %>%   
  group_by(`SALE YEAR`, `Lot_Size`,`Code_Age`) %>%  
  summarise(Median_Unit_Value = median(`SALE AMOUNT`, na.rm = TRUE)) %>%
  ungroup() 
# %>% 
#   mutate(
#     Facet = str_replace_all(
#       as.character(Facet),
#       c(
#         "Non-concentrated affluence" = "Areas not concentrated affluence",
#         "Concentrated affluence" = "Concentrated affluence")))

#Below code tidies above object to recreate the larger 'all parcel' chart, not necessary for the following ggplot though
# %>%
#   mutate(Lot_Size_Description = case_when(
#     `ZONING CODE` %in% c("A-1", "AR-1", "AR-2", "AR-6", "RC") ~ "A1 to AR6",
#     `ZONING CODE` %in% c("R-1", "R-2", "R-3", "R-4", "R-5") ~ "R1 to R5",
#     `ZONING CODE` %in% c("RC", "RS", "RR-1", "R-3") ~ "RC, RS, and RR1",
#     `ZONING CODE` %in% c("RM") ~ "RM",
#     TRUE ~ NA_character_  # Fallback for any other values not captured
#   )) %>%
#   # mutate(Color = case_when(
#   #   `ZONING CODE` %in% c("R1A", "R2A", "R3A", "R4A", "R5A") ~ "A subcodes",
#   #   `ZONING CODE` %in% c("R2AC", "R3AC", "R4AC", "R5AC", "R5A") ~ "AC subcodes",
#   #   `ZONING CODE` %in% c("R2C", "R3C", "R5C", "R6C") ~ "C subcodes",
#   #   `ZONING CODE` %in% c("RTH", "RTHC") ~ "Townhouses",
#   #   `ZONING CODE` %in% c("R1", "R2", "R3", "R4", "R5", "R6") ~ "Standard R code",
#   #   `ZONING CODE` %in% c("A1") ~ "A1",
#   #   `ZONING CODE` %in% c("R0") ~ "R0",
#   #   TRUE ~ NA_character_  # Fallback for any other values not captured
#   # ))  %>%
#   mutate(Linetype = case_when(
#     `ZONING CODE` %in% c("AR-1", "AR-2", "R-1", "R-2", "R-3", "R-4", "R-5") ~ "Repealed",
#     `ZONING CODE` %in% c("A-1", "A-6", "RC", "RS", "RM") ~ "Active",
#     TRUE ~ NA_character_  # Fallback for any other values not captured
#   )) 
  
Hanover_Med_Test <- Hanover_Med_Test %>% 
  mutate(Median_Unit_Value_Inf = adjust_for_inflation(Median_Unit_Value, 
                                                      `SALE YEAR`, "US", to_date = 2020)) %>%
  filter(`Median_Unit_Value_Inf` <= 1500000)   



# ggplot(Hanover_Med_Test %>%
#        # %>%
#        # mutate(`ZONING CODE` = fct_relevel(`ZONING CODE`,
#        #                                    "RC", "AR-2", "AR-6", "AR-1",  # First group
#        #                                    "RS", "R-2", "R-3" ,"R-1",   # Second group
#        #                                    "RM", "R-4", "R-5"    # Last group
#        # )) %>%
#        filter(`ZONING CODE` %in% c("RC", "AR-2", "AR-6", "AR-1",  # First group
#                                    "RS", "R-2", "R-3" ,"R-1",   # Second group
#                                    "RM", "R-4", "R-5")),
#        aes(x = `SALE YEAR`, y = Median_Unit_Value_Inf, color = `ZONING CODE`, 
#            group = `ZONING CODE`, linetype = `ZONING CODE`, linewidth = `ZONING CODE`)) +

ggplot(Hanover_Med_Test 
       # %>%
       #   mutate(`ZONING CODE` = fct_relevel(`ZONING CODE`,
       #                                      "RC", "AR-2", "AR-6", "AR-1", "A-1",  # First group
       #                                      "RS", "R-2", "R-3", "R-1",     # Second group
       #                                      "RM", "R-4", "R-5"))
       ,
       aes(x = `SALE YEAR`, y = Median_Unit_Value_Inf, color = `Code_Age`, 
           group = `Code_Age`)) +
  facet_grid(fct_rev(`Lot_Size`) ~ .,
             # scales = "free_y", 
             space = "free",
             switch = "y") +
  geom_line(size = 1, , lineend = "round", linejoin = "round") +
  geom_vline(xintercept = 1980, color = "black", linetype = "solid", size = 0.75) +
  geom_text(data = Hanover_Med_Test %>% filter(Lot_Size == "Larger than 0.75 acres"),  # Filtering inside the layer
            aes(x = 1979, y = 420000, angle = 90, label = "SSA introduced"),
            hjust = 0, color = "black", size = 3.5) +
  geom_vline(xintercept = 1999, color = "black", linetype = "solid", size = 0.75) +
  # geom_text(data = Hanover_Med_Test %>% filter(Lot_Size == "Larger than 0.75 acres"),  # Filtering inside the layer
  #           aes(x = 1998, y = 375000, angle = 90, label = "AR-1/2 and R-1 to R-3 replaced"),
  #           hjust = 0, color = "black", size = 3.5) +
  geom_text(data = Hanover_Med_Test %>% filter(Lot_Size == "Larger than 0.75 acres"),  # Filtering inside the layer
            aes(x = 2000, y = 420000, angle = 90, label = "RC and RS added"),
            hjust = 0, color = "black", size = 3.5) +
  geom_vline(xintercept = 2010, color = "black", linetype = "solid", size = 0.75) +
  geom_text(data = Hanover_Med_Test %>% filter(Lot_Size == "Larger than 0.75 acres"),  # Filtering inside the layer
            aes(x = 2011, y = 420000, angle = 90, label = "RM added"),
            hjust = 0, color = "black", size = 3.5) +
  geom_hline(yintercept = 0, color = 'darkgrey', linetype = 'solid') +
  theme_minimal() +
  scale_y_continuous(labels = label_dollar(),
                     breaks = c(0, 250000, 500000, 750000),
                     position = "right") +
  scale_x_continuous(breaks = c(1950, 1960, 1970, 1980, 1990, 2000, 2010, 2020)) +
  coord_cartesian(xlim=c(1952, 2020),
                  ylim = c(0, 750000)) +
  scale_color_manual(values = c("Active" = "#66c2a5",
                                "Repealed" = "#e78ac3"),
                     name = NULL, guide = "none") +
  # scale_color_manual(values = c("A-1" = "black",
  #                               "AR-6" = "blue",
  #                               "AR-1" = "black",
  #                               "AR-2" = "grey",
  #                               "RC" = "#1b9e77",
  #                               "RS" = "#e7298a",
  #                               "R-2" = "#7570b3",
  #                               "R-3" = "#e6ab02",
  #                               "R-4" = "#66a61e",
  #                               "R-5" = "#a6761d",
  #                               "RM" = "#984ea3",
  #                               "R-1" = "#d95f02"),
  #                    name = "Parcel's current\nzoning code") +
  # scale_linetype_manual(values = c("A-1" = "solid",
  #                                  "AR-6" = "solid",
  #                                  "AR-1" = "dashed",
  #                                  "AR-2" = "dashed",
  #                                  "RC" = "solid",
  #                                  "RS" = "solid",
  #                                  "R-2" = "dashed",
  #                                  "R-3" = "dashed",
  #                                  "R-4" = "dashed",
  #                                  "R-5" = "dashed",
  #                                  "RM" = "solid",
  #                                  "R-1" = "dashed"),
  #                       name = "Parcel's current\nzoning code") +
  # scale_linewidth_manual(values = c("A-1" = 2,
  #                              "AR-6" = 2,
  #                              "AR-1" = 1,
  #                              "AR-2" = 1,
  #                              "RC" = 2,
  #                              "RS" = 2,
  #                              "R-2" = 1,
  #                              "R-3" = 1,
  #                              "R-4" = 1,
  #                              "R-5" = 1,
  #                              "RM" = 2,
  #                              "R-1" = 1),
  #                   name = "Parcel's current\nzoning code") +
  labs(title = "Hanover County",
       subtitle = "Parcel values across <span style='color:#66c2a5;'>active</span> and <span style='color:#e78ac3;'>repealed</span> zoning codes",
       x = NULL,
       y = "Median parcel unit sale value",
       # subtitle = NULL,
       caption = "All sales adjusted to 2020 dollars"
  ) +
  theme(plot.subtitle = element_markdown(hjust = 0.5, size = 13, face = "bold"),
        strip.placement = "outside",
        strip.text.y = element_markdown(size = 12, face = "bold"), 
        axis.text.x = element_text(size = 14),
        axis.text.y = element_markdown(size = 14),
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        legend.position = "right",
        axis.title.x = element_markdown(size = 14),
        axis.title.y = element_markdown(size = 14),
        panel.grid.major.x = element_line(size = 0.2, color = "darkgrey"),
        panel.grid.minor.x = element_line(size = 0.2, color = "grey"),
        panel.grid.major.y = element_line(size = 0.2, color = "grey"),
        panel.grid.minor.y = element_line(size = 0.1),
        panel.border = element_rect(color = "black", fill = NA, size = 0.75),
        legend.text = element_text(size = 14),
        legend.key.size = unit(2, 'lines'),           # Increase the size of the legend key
        legend.spacing = unit(4.0, 'cm')        
  ) 


ggsave("Hanover_ActiveRepealed_LotSize.png",
       path = "~/desktop",
       width = 10,
       height = 10,
       units = "in",
       dpi = 500)

#---------------------------------------------------------------------------------------
#Mapping mobile homes in Henrico

#Load and tidy
Henrico <- read_rds(paste0(onedrivepath, "Mapping Richmond/Parcel-Buildings/Henrico/Henrico_Buildings_small.rds")) %>%
  filter(if_any(c(CoreLogic_Description, County_Description), ~ 
                  str_detect(.x, regex("Manufacture|Mobile", ignore_case = TRUE)))) %>%
  st_as_sf(coords = c("PARCEL LEVEL LONGITUDE", "PARCEL LEVEL LATITUDE"), crs = 4326) %>%
  mutate(CoreLogic_Description = str_replace(str_to_lower(CoreLogic_Description), "^.", ~str_to_upper(.)))





#Download Counties and filter to within CBSA
Counties_2020 <- counties(ST,
                        year = YR4,
                        cb = F) %>%
  filter(NAME == "Henrico")

#Create lines
Counties_2020 <- st_boundary(Counties_2020)
  
#2020 Median HH income by urban/suburban
Med_inc_2020 <- get_acs(
  geography = "block group", 
  variables = "B19013_001E", 
  state = "VA", 
  year = YR4, 
  output = "wide", 
  geometry = TRUE) %>%
  rename("Income_2020" = B19013_001E,
          "IncomeMOE_2020" = B19013_001M) %>%
  filter(str_detect(NAME, regex("Henrico County", ignore_case = TRUE))) 
  
data <- load_variables("2020", "acs5")
  
# Plot using tmap
tmap_mode("plot") # Interactive mode or "view"
# tm_basemap("OpenStreetMap") +
Income_Zoning <- tm_shape(Med_inc_2020) +
  tm_fill("Income_2020", 
          title = "Median household income", 
          style = "fixed", 
          breaks = c(0, 50000, 100000, 150000, 200000, Inf),
          labels = c("$0 - $50,000", 
                     "$50,001 - $100,000", 
                     "$100,001 - $150,000", 
                     "$150,001 - $200,000", 
                     "$200,001 or more"),
          palette = "Purples", 
          alpha = 1,
          legend.show = TRUE,
          colorNA = "grey",        # Color for NA values
          textNA = "No population"  # Label for NA values
  ) +  
  tm_shape(Counties_2020) +
  tm_lines(col="black", lwd = 0.5, scale=  2, legend.lwd.show = FALSE) +
  tm_shape(Henrico) +
  tm_dots(col = "CoreLogic_Description", size = 0.25, palette = c("#d95f02", "#1b9e77"), title = "") +
  tm_layout(main.title = "",
            main.title.size = 1.4,
            main.title.position = "left",
            main.title.fontface = "bold",
            frame = F,
            legend.position = c(0.1, 0.05), 
            # legend.position = "left",
            legend.title.size = 1,
            legend.title.fontface = "bold",
            legend.outside = F,
            legend.show = T,
            panel.show = F,
            panel.label.bg.color = "transparent",
            panel.label.color = "#737373",
            #panel.labels = "Year",
            panel.label.fontface = "bold",
            #inner.margins = c(0.02, 0.15, 0.2, 0.1)
  ) 


#To save
tmap_save(
  tm = Income_Zoning,
  filename = "~/desktop/Emilys_Map.png",
  height = 8,
  width = 8,
  dpi = 500
)

#---------------
#Descriptive stats of parcels across Conc aff and non-Conc aff

#Join two counties
Hanover_Join <- Hanover %>%
  select(`ZONING CODE`, Facet, PIN, `SITUS COUNTY`, County_Description, CoreLogic_Description, `ZONING CODE`, 
         `ZONING CODE DESCRIPTION`, `MULTI OR SPLIT PARCEL CODE`, ACRES, `TOTAL SQUARE FOOTAGE ALL BUILDINGS`,
         `LAND SQUARE FOOTAGE`, `YEAR BUILT`, `SALE DATE`, `SALE AMOUNT`, `MARKET TOTAL VALUE`, Unit_Value, Code_Age)

Henrico_Join <- Henrico %>%
  select(`ZONING CODE`, Facet, PIN, `SITUS COUNTY`, County_Description, CoreLogic_Description, `ZONING CODE`, 
         `ZONING CODE DESCRIPTION`, `MULTI OR SPLIT PARCEL CODE`, ACRES, `TOTAL SQUARE FOOTAGE ALL BUILDINGS`,
         `LAND SQUARE FOOTAGE`, `YEAR BUILT`, `SALE DATE`, `SALE AMOUNT`, `MARKET TOTAL VALUE`, Unit_Value, Code_Age) %>%
  st_drop_geometry()

Combined_Counties <- rbind(Hanover_Join, Henrico_Join)

#Percent each zoning code
Concentrated_Affluent_Values <- Combined_Counties %>%
  filter(!is.na(Facet)) %>%
  group_by(Facet, `ZONING CODE`) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(percentage = 100 * n / sum(n))

#Median/mean
Concentrated_Affluent_Values <- Combined_Counties %>%
  filter(!is.na(Facet)) %>%
  filter(!str_starts(`ZONING CODE`, "A")) %>%
  group_by(Facet) %>%
  summarise(Mean = mean(ACRES, na.rm = TRUE))

#Percent built after...
Concentrated_Affluent_Values <- Combined_Counties %>%
  filter(!is.na(Facet)) %>%
  group_by(Facet) %>%
  summarise(
    total_units = n(),
    units_built_after_2000 = sum(`YEAR BUILT` > 2000, na.rm = TRUE),
    percent_built_after_2000 = 100 * units_built_after_2000 / total_units
  )

#Percent sold after 2000
Concentrated_Affluent_Values <- Combined_Counties %>%
  filter(!is.na(Facet)) %>%
  group_by(Facet) %>%
  summarise(
    total_units = n(),
    units_sold_after_threshold = sum(`SALE DATE` > 20150000, na.rm = TRUE),
    percent_sold_after_threshold = 100 * units_sold_after_threshold / total_units
  )

