#5/22/24, initiated by BS
#Goal: data tidying of parcel data

#Analysis includes
  #tidying parcel leve data of hanover and henrico co

#Libraries
require(tidyverse)
# library(tigris)
library(readxl)
library(sf)

library(data.table)
library(stringr)
library(bit64) #change integer to numeric
library(scales)
library(ggtext)



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
VA_property <- fread(paste0(onedrivepath, "Mapping Richmond/Parcel-Buildings/pennsylvania_state_university_property_basic2_300000230479867_20210409_102905_VA.txt"), 
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
  select(PIN, `Sale Amount`, `MARKET TOTAL VALUE`)
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
# saveRDS(Henrico_CL_Tidy, "Henrico_Buildings_small.rds")


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
  ))

#Plot Hanover
Hanover_Boxplot <- ggplot(Hanover, aes(x = factor(`ZONING CODE`, 
                               levels = c("A1", "AR6", "RC", "RS", "RM",
                                          "AR1", "AR2", "R1", "R2", 
                                          "R3", "R4", "R5", "R6", "RO1", "RR1")), y = Unit_Value)) +
  facet_grid( . ~ Code_Age, scales = "free_x", space = "free") +
  geom_boxplot(fill = "#a6761d", color = "black") +
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
  )) 

#Plot Hanover
Henrico_Boxplot <- ggplot(Henrico, aes(x = factor(`ZONING CODE`, 
                               levels = c("A1", "R0", "R1", "R1A", "R2", "R2A", "R2AC", 
                                          "R2C", "R3", "R3A", "R3AC", "R3C", "R4", "R4A", "R4AC", 
                                          "R5", "R5A", "R5AC", "R5C", "R6", "R6C",
                                          "RMP", "RO", "RPN", "RTH", "RTHC")), y = Unit_Value)) +
  geom_boxplot(fill = "#e6ab02", color = "black") +
  facet_grid( . ~ Code_Age, scales = "free_x", space = "free") +
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
                           heights = c(0.75, 1),
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












