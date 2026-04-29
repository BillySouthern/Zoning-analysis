#5/22/24, initiated by BS
#Goal: To assess Henrico's R zoning codes

#Analysis includes
#Tidying data, matching inc seg with zoning codes

#Libraries
require(tidyverse)
require(tigris)
library(sf)
library(tidycensus)
library(tmap)
library(tmaptools)
library(readxl)
library(scales)
library(ggtext)
library(ggridges)
library(data.table)
library(ggforce)

  #For income seg work
library(haven)
library(RStata)
library(spdep) #For spatial analyses
library(priceR)

options(tigris_use_cache = TRUE)

#Set parameters (state and year)
ST = "VA"
# YR1 = 1990
# YR2 = 2000
# YR3 = 2010
YR4 = 2020

CBSA = c("Richmond")
CENTRAL_CITY = c("Richmond city")
GEOG = "tract"

#Create a filepath to OneDrive
# onedrivepath="~/OneDrive - The Pennsylvania State University/"
#Create a filepath to OneDrive
onedrivepath="~/Library/CloudStorage/OneDrive-UniversityOfOregon/"

#--------------------------------------------------------------------------------
#Income figure and seg measure by census tract in Henrico

#Download median hh income data by tract
#2020
#2020 Median HH income 
Henrico_Income_2020 <- get_acs(
  geography = "county", 
  variables = "B19019_001", 
  state = ST,
  year = YR4,
  output = "wide",
  geometry = T) %>%
  filter(str_detect(GEOID, "51087")) %>%
  rename("Income_2020" = B19019_001E,
         "IncomeMOE_2020" = B19019_001M) %>%
  select(-NAME)

#2020 income counts (for exploratory purposes)
Henrico_Income_Binned_2020 <- get_acs(
  geography = GEOG, 
  variables = c(Below10000 = "B19001_002E",
                Btw10000_14999 = "B19001_003E",
                Btw15000_19999 = "B19001_004E",
                Btw20000_24999 = "B19001_005E",
                Btw25000_29999 = "B19001_006E",
                Btw30000_34999 = "B19001_007E",
                Btw35000_39999 = "B19001_008E",
                Btw40000_44999 = "B19001_009E",
                Btw45000_49999 = "B19001_010E",
                Btw50000_59999 = "B19001_011E",
                Btw60000_74999 = "B19001_012E",
                Btw75000_99999 = "B19001_013E",
                Btw100000_124999 = "B19001_014E",
                Btw125000_149999 = "B19001_015E",
                Btw150000_199999 = "B19001_016E",
                Above200000 = "B19001_017E"),
  state = ST,
  year = YR4,
  output = "wide",
  geometry = T) %>%
  filter(str_detect(GEOID, "51087")) %>%
  select(-NAME)

#Load in Henrico inc seg by tract figures
Henrico_Inc_Seg <- read_dta("/Users/billy/Library/CloudStorage/OneDrive-ThePennsylvaniaStateUniversity/Mapping Richmond/Binned Income Data/Stata Outputs/Within/GEOID_2020_Within.dta") %>%
  mutate(Year = YR4,
         Population = "Total Population",
         GEOID = as.character(GEOID)) %>%
  filter(str_detect(GEOID, "51087")) %>%
  distinct(GEOID, h4, .keep_all = TRUE) %>%
  left_join(Henrico_Income_2020, by="GEOID") %>%
  mutate(Affluence = case_when(
      Income_2020 >= 125000 ~ "Affluent tract",
      Income_2020 <= 26200  ~ "Poor tract",
      TRUE ~ "Middle income tract"))

#Exploratory line graphs
ggplot(Henrico_Inc_Seg,
       aes(x = Income_2020, y = h4, fill = Affluence, alpha = Income_2020)) +
  geom_abline(intercept = 0, slope = 1, color = "black", alpha = 0.65, 
              linetype = "longdash", linewidth = 0.25) +
  geom_point(aes(size = Income_2020), 
             shape = 21,  # Use shape 21 for points with a border
             color = "black",  # Border color
             stroke = 0.5  # Thickness of the border
  ) 

#Exploring the census tract maps
Henrico_Inc_Seg <- st_as_sf(Henrico_Inc_Seg)

tmap_mode("view")  # interactive mode

tm_shape(Henrico_Inc_Seg) +
  tm_polygons(
    col = "Affluence",     # replace with column you want to color by
    palette = c("red", "grey70", "green"),  # example colors for poor/middle/affluent
    alpha = 0.7,
    border.col = "black"
  ) +
  tm_basemap("OpenStreetMap")  # OSM basemap
  
#Calculate share of Residential zoning for each census tract
#Load and tidy
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
  st_transform(st_crs(Henrico_Inc_Seg)) 


#Load zoning description
RVA_Zoning_Descriptions <- read_excel("~/Library/CloudStorage/OneDrive-ThePennsylvaniaStateUniversity/RQ3/RVA-Zoning-Descriptions.xlsx")

#Join descriptions with code
Henrico_Zoning <- Henrico_Zoning %>%
  left_join(RVA_Zoning_Descriptions, by= c("County", "Code")) %>%
  select(County, Year, Code, Name, Nature, Housing_Description, Maximum_Density_Allowed, Zoning_Atlas_Definition, Source, geometry) %>%
  mutate(Mapping_Variable = case_when(
    Zoning_Atlas_Definition == "Mixed with Residential" ~ "Mixed with Residential",
    Zoning_Atlas_Definition == "Nonresidential" ~ "Nonresidential",
    Zoning_Atlas_Definition == "Primarily Residential" ~ Code,
    TRUE ~ NA_character_  
  )) %>%
  mutate(Mapping_Variable = case_when(
    Mapping_Variable == "A-1" ~ "Agricultural",
    TRUE ~ Mapping_Variable
  )) %>%
  group_by(Mapping_Variable) %>%
  mutate(geometry = st_make_valid(geometry))%>%
  summarise(geometry = st_union(geometry), .groups = "drop") 

# Calculate total area of all combined polygons
Henrico_Total_Area <- sum(st_area(Henrico_Zoning$geometry))

# Add percent area column
Henrico_Zoning <- Henrico_Zoning %>%
  mutate(
    Area = st_area(geometry),
    pct_of_total = as.numeric(Area / Henrico_Total_Area) * 100
  )

# Exploratory mapping of the zoning codes
tmap_mode("view")  # interactive mode

tm_shape(Henrico_Zoning) +
  tm_polygons(
    col = "Mapping_Variable",     
    # palette = c("red", "grey70", "green"),  
    alpha = 0.7,
    border.col = "black"
  ) +
  tm_basemap("OpenStreetMap")  


#------------------------------
#Calculating zoning percent of each census tract above

#Create intersection between polygons
Henrico_tract_zoning_overlap <- st_intersection(Henrico_Inc_Seg, Henrico_Zoning)

#Calculate area
Henrico_tract_zoning_overlap <- Henrico_tract_zoning_overlap %>%
  mutate(Area = st_area(geometry))

#Summarize by census tract
Henrico_Seg_Zoning <- Henrico_tract_zoning_overlap %>%
  group_by(GEOID, Mapping_Variable) %>%
  summarise(overlap_area = sum(Area), .groups = "drop")

#To calc percentages
Henrico_Inc_Seg <- Henrico_Inc_Seg %>%
  mutate(Census_Tract_Area = st_area(geometry))

Henrico_Seg_Zoning <- Henrico_Seg_Zoning %>%
  left_join(
    st_drop_geometry(Henrico_Inc_Seg[, c("GEOID", "Census_Tract_Area")]),
    by = "GEOID"
  ) %>%
  mutate(pct_of_tract = as.numeric(overlap_area / Census_Tract_Area))

#Census tract and zoning share of land
Census_Tract_Zoning_share <- Henrico_Seg_Zoning %>%
  select(GEOID, Zoning_Code, Zoning_pct_of_tract)

#----------------------------
#Rejoin above area figures with income and income seg
Henrico_Seg_Zoning <- Henrico_Seg_Zoning %>%
  select(GEOID, Mapping_Variable, pct_of_tract, geometry) %>%
  rename(Zoning_Code = Mapping_Variable,
         Zoning_pct_of_tract = pct_of_tract) %>%
  st_drop_geometry() %>%
  left_join(Henrico_Inc_Seg, by="GEOID")
  
#exploratory mapping by zoning code
ggplot(Henrico_Seg_Zoning[Henrico_Seg_Zoning$Zoning_Code == "Agricultural", ],
       aes(x = Zoning_pct_of_tract, y = h4, fill = Affluence)) +
  geom_abline(intercept = 0, slope = 1, color = "black", alpha = 0.65, 
              linetype = "longdash", linewidth = 0.25) +
  geom_point(aes(size = Income_2020), 
             shape = 21,  # Use shape 21 for points with a border
             color = "black",  # Border color
             stroke = 0.5  # Thickness of the border
  ) +
  # facet_wrap(~ Zoning_Code, nrow = 3, strip.position = "top") +
  theme_minimal() +
  geom_smooth(method='lm', formula= y~x)



#------------------------------------------------------------------------------------------------------------------
#Re-calculating income segregation for just Henrico

#2020 income counts 
  #Census tract
Henrico_Income_Binned_2020 <- get_acs(
  geography = GEOG, 
  variables = c(Below10000 = "B19001_002E",
                Btw10000_14999 = "B19001_003E",
                Btw15000_19999 = "B19001_004E",
                Btw20000_24999 = "B19001_005E",
                Btw25000_29999 = "B19001_006E",
                Btw30000_34999 = "B19001_007E",
                Btw35000_39999 = "B19001_008E",
                Btw40000_44999 = "B19001_009E",
                Btw45000_49999 = "B19001_010E",
                Btw50000_59999 = "B19001_011E",
                Btw60000_74999 = "B19001_012E",
                Btw75000_99999 = "B19001_013E",
                Btw100000_124999 = "B19001_014E",
                Btw125000_149999 = "B19001_015E",
                Btw150000_199999 = "B19001_016E",
                Above200000 = "B19001_017E"),
  state = ST,
  year = YR4,
  output = "wide",
  geometry = F) %>%
  filter(str_detect(GEOID, "51087")) %>%
  select(-NAME,
         -ends_with("M"))

#Write data as Stata file
write_dta(Henrico_Income_Binned_2020, file.path(onedrivepath, "Mapping Richmond/Henrico-Segregation-Work/Stata input/Household_Income_Tracts_2020.dta"))

#Affluent and low income of the above
Henrico_Aff_LowInc_2020 <- Henrico_Income_Binned_2020 %>%
  mutate(
    Low_Income = Below10000 + Btw10000_14999 + Btw15000_19999 + Btw20000_24999,
    Middle_Income = Btw25000_29999 + Btw30000_34999 + Btw35000_39999 + 
      Btw40000_44999 + Btw45000_49999 + Btw50000_59999 +
      Btw60000_74999 + Btw75000_99999 + Btw100000_124999,
    Affluent = Btw125000_149999 + Btw150000_199999 + Above200000,
  ) %>%
  select(GEOID, Low_Income, Middle_Income, Affluent) 
  

#Save dta
write_dta(Henrico_Aff_LowInc_2020, file.path(onedrivepath, "Mapping Richmond/Henrico-Segregation-Work/Stata input/Henrico_Aff_LowInc_2020.dta"))


#STATA WORK
#Set the Stata path and version
options("RStata.StataPath" = "/Applications/Stata/StataMP.app/Contents/MacOS/stata-mp")
options("RStata.StataVersion" = 18)

#Check these
# # Set path to Stata
# stata_path <- "/Applications/Stata/StataMP.app/Contents/MacOS/stata-mp"
# Run Stata commands from R
# stata("ssc install rankseg")
# 
# # Install rankseg remotely in Stata using system command
# system(paste(shQuote(stata_path), "do install rankseg"))


#--------------------------------------
#WITHIN UNIT STATA SEG MEASUREMENT

#Create a stata command to run the rankseg function in the background
#All groups
# stata_commands <- '
# use "/Users/billy/Library/CloudStorage/OneDrive-ThePennsylvaniaStateUniversity/Mapping Richmond/Henrico-Segregation-Work/Stata input/Henrico_Aff_LowInc_2020", clear
# rankseg Below10000 - Above200000, order(4) h r by(GEOID) adjust popcounts 
# save "/Users/billy/Library/CloudStorage/OneDrive-ThePennsylvaniaStateUniversity/Mapping Richmond/Henrico-Segregation-Work/Stata output/Henrico_Aff_LowInc_2020"'

#Low inc, middle, affluent
stata_commands <- '
use "/Users/billy/Library/CloudStorage/OneDrive-ThePennsylvaniaStateUniversity/Mapping Richmond/Henrico-Segregation-Work/Stata input/Henrico_Aff_LowInc_2020", clear
rankseg Low_Income - Affluent, order(1) h r by(GEOID) adjust popcounts 
save "/Users/billy/Library/CloudStorage/OneDrive-ThePennsylvaniaStateUniversity/Mapping Richmond/Henrico-Segregation-Work/Stata output/Henrico_Aff_LowInc_2020"'

#Run Stata command for the individual object above
stata(stata_commands, data.in = NULL, data.out = FALSE)

#Load in recently created dta
Henrico_Aff_LowInc_2020_Seg <- haven::read_dta("/Users/billy/Library/CloudStorage/OneDrive-ThePennsylvaniaStateUniversity/Mapping Richmond/Henrico-Segregation-Work/Stata output/Henrico_Aff_LowInc_2020.dta")



#----------
#Tidy census tracts and check their incomes
Henrico_Aff_LowInc_2020_Seg <- Henrico_Aff_LowInc_2020_Seg %>%
  left_join(Henrico_Income_2020, by="GEOID") %>%
  mutate(Affluence = case_when(
    Income_2020 >= 125000 ~ "Affluent tract",
    Income_2020 <= 26200  ~ "Poor tract",
    TRUE ~ "Middle income tract")) 

#Explore
ggplot(Henrico_Aff_LowInc_2020_Seg,
       aes(x = Income_2020, y = h1, fill = Affluence, alpha = Income_2020)) +
  geom_abline(intercept = 0, slope = 1, color = "black", alpha = 0.65, 
              linetype = "longdash", linewidth = 0.25) +
  geom_point(aes(size = Income_2020), 
             shape = 21,  # Use shape 21 for points with a border
             color = "black",  # Border color
             stroke = 0.5  # Thickness of the border
  ) +
  geom_smooth(method='lm', formula= y~x, aes(group = 1))

#---------------------
#Merge tracts with zoning data
Henrico_Aff_LowInc_2020_Seg <- Henrico_Aff_LowInc_2020_Seg %>%
  left_join(Census_Tract_Zoning_share, by = "GEOID")%>%
  mutate(
    Min_Acre = case_when(
      Zoning_Code %in% c("R-0", "Agricultural") ~ "1",      
      Zoning_Code == "R-1" ~ "0.5",                        
      Zoning_Code %in% c("R-1A", "R-2", "R-3") ~ "0.25",         
      Zoning_Code %in% c("Mixed with Residential") ~ "Mixed with Residential",  
      Zoning_Code %in% c("Nonresidential") ~ "Nonresidential",         
      TRUE ~ "<0.25"                                       
    ))

#Explore
ggplot(Henrico_Aff_LowInc_2020_Seg
       [Henrico_Aff_LowInc_2020_Seg$Zoning_Code %in% c("R-0", "R-1"),],
  aes(x = Zoning_pct_of_tract, y = h1, fill = Affluence, alpha = Income_2020)) +
  geom_abline(intercept = 0, slope = 1, color = "black", alpha = 0.65, 
              linetype = "longdash", linewidth = 0.25) +
  geom_point(aes(size = Income_2020), 
             shape = 21,  # Use shape 21 for points with a border
             color = "black",  # Border color
             stroke = 0.5  # Thickness of the border
  ) +
  geom_smooth(method='lm', formula= y~x, aes(group = 1)) +
  facet_wrap(~ Min_Acre, nrow = 3, strip.position = "top") 
  

#-----------------------------------------------------------------------------------
#LISA and zoning
  #Load in LISA statistics and test with zoning codes
Income_LISA_all <- readRDS(file.path(onedrivepath, "Mapping Richmond/Binned Income Data/LISA/LISA_Income_RVA.rds"))

#Tidy LISA ahead of merge with zoning data above
Income_LISA_all <- Income_LISA_all %>%
  filter(Year == "2020") %>%
  filter(str_detect(GEOID, "51087")) %>%
  select(GEOID, Landscape_Five, scaled_estimate, lagged_estimate, Local_M_i) %>%
  st_drop_geometry()

#Merge with zoning data
Henrico_Seg_LISA_Zoning <- Henrico_Aff_LowInc_2020_Seg %>%
  left_join(Income_LISA_all, by = "GEOID")
  
#Explore
ggplot(Henrico_Seg_LISA_Zoning,
       # [Henrico_Aff_LowInc_2020_Seg$Zoning_Code %in% c("R-0", "R-1"),],
       aes(x = Zoning_pct_of_tract, y = Local_M_i, fill = Affluence, alpha = Income_2020)) +
  geom_abline(intercept = 0, slope = 1, color = "black", alpha = 0.65, 
              linetype = "longdash", linewidth = 0.25) +
  geom_point(aes(size = Income_2020), 
             shape = 21,  # Use shape 21 for points with a border
             color = "black",  # Border color
             stroke = 0.5  # Thickness of the border
  ) +
  geom_smooth(method='lm', formula= y~x, aes(group = 1)) +
  facet_wrap(~ Zoning_Code, nrow = 3, strip.position = "top") 

#----
#Rerun LISA on just Henrico County tracts (as a test measure)
#Create dataframe
Income_LISA <- Henrico_Income_2020 %>%
  filter(!Income_2020 == "NA") %>%
  mutate(scaled_estimate = as.numeric(scale(Income_2020))) 

# Income_LISA$scaled_estimate <- as.numeric(scale(Income_LISA$Med_Income_Adj))

#Create spatial neighbors object
neighbors <- poly2nb(Income_LISA$geometry, queen = TRUE)
# summary(neighbors)

# Ensure your data is an sf object
Income_LISA <- st_as_sf(Income_LISA) 

# Calculate centroids and extract coordinates
Income_LISA_coords <- Income_LISA %>%
  st_centroid() %>%
  st_coordinates()

#Create weights 
weights <- nb2listw(neighbors, style = "W")

#Isolate weights
weights$weights[[1]]

#Run LISA
Income_LISA_Results <- localmoran_perm(
  Income_LISA$scaled_estimate, 
  weights, 
  nsim = 999L, 
  alternative = "two.sided"
) %>%
  as_tibble() %>%
  set_names(c("Local_M_i", "Expected_i", "Variance_i", "Z_i", "Pval_i",
              "Pval_i_sim", "Pvali_sim_folded", "Skewness", "Kurtosis"))

#Join LISA with income data
Income_LISA <- Income_LISA %>%
  select(GEOID, Income_2020, scaled_estimate) %>%
  mutate(lagged_estimate = lag.listw(weights, scaled_estimate)) %>%
  bind_cols(Income_LISA_Results) 

#Recreate string
Income_LISA$Local_M_i <- as.numeric(Income_LISA$Local_M_i)


#Set the clusters
Income_LISA_Henrico <- Income_LISA %>%
  mutate(lisa_cluster = case_when(
    Pval_i >= 0.05 ~ "Not significant",
    scaled_estimate > 0 & Local_M_i > 0 ~ "High-high", #High income, high-income neighbors
    scaled_estimate > 0 & Local_M_i < 0 ~ "High-low", #High income, low-income neighbors
    scaled_estimate < 0 & Local_M_i > 0 ~ "Low-low", #Low income, low-income neighbors
    scaled_estimate < 0 & Local_M_i < 0 ~ "Low-high" #Low income, high-income neighbors
  )) %>%
  mutate(Tract_type = case_when(
    Income_2020 >= 125000 ~ "Affluent tract",
    Income_2020 <= 26200 ~ "Poor tract",
  )) %>%
  mutate(Concentrations = case_when(
    Pval_i >= 0.05 ~ "Not significant",
    Income_2020 > 26200 & Income_2020 <= 125000 & Local_M_i < 0 ~ "Not significant",  # Middle income
    Income_2020 > 26200 & Income_2020 <= 125000 & Local_M_i > 0 ~ "Middle income clustered",  # Middle income
    Income_2020 >= 125000 & Local_M_i < 0 ~ "High-none", #High income, not concentrated
    Income_2020 >= 125000 & Local_M_i > 0 ~ "High-high", #High income, highly concentrated
    Income_2020 <= 26200 & Local_M_i > 0 ~ "Low-high", #Low income, highly concentrated
    Income_2020 <= 26200 & Local_M_i < 0 ~ "Low-none" #Low income, not concentrated
  ))

#Exploratory map of Henrico LISA
tmap_mode("view")  

tm_shape(Income_LISA_Henrico) +
  tm_polygons(
    col = "Concentrations",     
    # palette = c("red", "grey70", "green"),  
    alpha = 0.7,
    border.col = "black"
  ) +
  tm_basemap("OpenStreetMap")  

#Join Henrico Lisa with zoning data then explore
  #Tidy LISA ahead of merge with zoning data above
Income_LISA_Henrico <- Income_LISA_Henrico %>%
  left_join(Henrico_Aff_LowInc_2020_Seg, by = "GEOID")

#Explore
ggplot(Income_LISA_Henrico,
       # [Henrico_Aff_LowInc_2020_Seg$Zoning_Code %in% c("R-0", "R-1"),],
       aes(x = Zoning_pct_of_tract, y = Local_M_i, fill = Affluence)) +
  geom_abline(intercept = 0, slope = 1, color = "black", alpha = 0.65, 
              linetype = "longdash", linewidth = 0.25) +
  geom_point(aes(size = Income_2020.x), 
             shape = 21,  # Use shape 21 for points with a border
             color = "black",  # Border color
             stroke = 0.5  # Thickness of the border
  ) +
  geom_smooth(method='lm', formula= y~x, aes(group = 1)) +
  facet_wrap(~ Zoning_Code, nrow = 3, strip.position = "top") 

#----------------------------------------------------------------------------------------------------------
#Redoing the above work at the block group unit
  #All analysis up to this point is at the tract level

#See ACS variables
  #Note that there are several Block Groups with no incomes, 
    #A few of these are from the census tract with no income too
income_variables <- load_variables(2022, "acs5")

#Download block level data
#2020 Median HH income 
Henrico_Income_2022 <- get_acs(
  geography = "block group", 
  variables = "B19013_001", 
  state = ST,
  year = YR4,
  output = "wide",
  geometry = T) %>%
  filter(str_detect(GEOID, "51087")) %>%
  rename("Income_2022" = B19013_001E,
         "IncomeMOE_2022" = B19013_001M) %>%
  select(-NAME)

#Exploratory map of Henrico LISA
tmap_mode("view")  

tm_shape(Henrico_Income_2022) +
  tm_polygons(
    col = "Income_2022",     
    # palette = c("red", "grey70", "green"),  
    alpha = 0.7,
    border.col = "black"
  ) +
  tm_basemap("OpenStreetMap")  

#-------
#Lisa by block group
#Rerun LISA on just Henrico County tracts (as a test measure)
#Create dataframe
Income_LISA <- Henrico_Income_2022 %>%
  filter(!Income_2022 == "NA") %>%
  mutate(scaled_estimate = as.numeric(scale(Income_2022))) 

# Income_LISA$scaled_estimate <- as.numeric(scale(Income_LISA$Med_Income_Adj))

#Create spatial neighbors object
neighbors <- poly2nb(Income_LISA$geometry, queen = TRUE)
summary(neighbors)

# Ensure your data is an sf object
Income_LISA <- st_as_sf(Income_LISA) 

# Calculate centroids and extract coordinates
Income_LISA_coords <- Income_LISA %>%
  st_centroid() %>%
  st_coordinates()

#Create weights 
weights <- nb2listw(neighbors, style = "W")

#Isolate weights
weights$weights[[1]]

#Run LISA
Income_LISA_Results <- localmoran_perm(
  Income_LISA$scaled_estimate, 
  weights, 
  nsim = 999L, 
  alternative = "two.sided"
) %>%
  as_tibble() %>%
  set_names(c("Local_M_i", "Expected_i", "Variance_i", "Z_i", "Pval_i",
              "Pval_i_sim", "Pvali_sim_folded", "Skewness", "Kurtosis"))

#Join LISA with income data
Income_LISA <- Income_LISA %>%
  select(GEOID, Income_2022, scaled_estimate) %>%
  mutate(lagged_estimate = lag.listw(weights, scaled_estimate)) %>%
  bind_cols(Income_LISA_Results) 

#Recreate string
Income_LISA$Local_M_i <- as.numeric(Income_LISA$Local_M_i)

#Set the clusters
Income_LISA_Henrico <- Income_LISA %>%
  mutate(lisa_cluster = case_when(
    Pval_i >= 0.05 ~ "Not significant",
    scaled_estimate > 0 & Local_M_i > 0 ~ "High-high", #High income, high-income neighbors
    scaled_estimate > 0 & Local_M_i < 0 ~ "High-low", #High income, low-income neighbors
    scaled_estimate < 0 & Local_M_i > 0 ~ "Low-low", #Low income, low-income neighbors
    scaled_estimate < 0 & Local_M_i < 0 ~ "Low-high" #Low income, high-income neighbors
  )) %>%
  mutate(Tract_type = case_when(
    Income_2022 >= 138750 ~ "Affluent tract",
    Income_2022 <= 27750 ~ "Poor tract",
  )) %>%
  mutate(Concentrations = case_when(
    Pval_i >= 0.05 ~ "Not significant",
    Income_2022 > 27750 & Income_2022 <= 138750 & Local_M_i < 0 ~ "Not significant",  # Middle income
    Income_2022 > 27750 & Income_2022 <= 138750 & Local_M_i > 0 ~ "Middle income clustered",  # Middle income
    Income_2022 >= 138750 & Local_M_i < 0 ~ "High-none", #High income, not concentrated
    Income_2022 >= 138750 & Local_M_i > 0 ~ "High-high", #High income, highly concentrated
    Income_2022 <= 27750 & Local_M_i > 0 ~ "Low-high", #Low income, highly concentrated
    Income_2022 <= 27750 & Local_M_i < 0 ~ "Low-none" #Low income, not concentrated
  )) %>%
  mutate(Facet = if_else(Concentrations == "High-high",
                         "Concentrated affluence",
                         "Non-concentrated affluence")) %>%
  select(GEOID, Income_2022, scaled_estimate, Local_M_i, Facet, geometry)

#Exploratory map of Henrico LISA
tmap_mode("view")  

tm_shape(Income_LISA_Henrico) +
  tm_polygons(
    col = "Facet",     
    # palette = c("red", "grey70", "green"),  
    alpha = 0.7,
    border.col = "black") 
  #+
  # tm_shape(Henrico_LISA_Zoning_BG %>% filter(is.na(Facet))) +
  # tm_dots(
  #   col = "Facet",
  #   size = 0.05,
  #   alpha = 0.7,
  #   border.col = "black"
  # ) +
  # tm_basemap("OpenStreetMap")  

#Reload zoning parcel data to the block group LISA
#set parameters
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
Henrico_Zoning_parcels <- read_rds(paste0(onedrivepath, "Mapping Richmond/Parcel-Buildings/Henrico/Henrico_Buildings_small.rds")) %>%
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


#Join the LISA and zoning
#Match crs
Henrico_Zoning_parcels <- st_transform(Henrico_Zoning_parcels, st_crs(Income_LISA_Henrico))

#Merge parcel and conc aff
Henrico_LISA_Zoning_BG <- st_join(Henrico_Zoning_parcels, Income_LISA_Henrico[, c("Facet", "Income_2022",
                                                                                  "scaled_estimate", "Local_M_i")]) %>%
  #then we remove the NAs, the parcels that are located in census tracts with no income and no LISA value
  filter(!is.na(Facet)) %>%
  filter(!is.na(ACRES)) %>%
  # filter(!str_detect(`ZONING CODE`, "A-1")) %>% #Test filter Agricultural
  mutate(
    Lot_Size = case_when(
      ACRES > 0   & ACRES <= 0.25  ~ "0 - 0.25 acres",
      ACRES > 0.25 & ACRES <= 0.75     ~ "0.25 - 0.5 acres",
      ACRES > 0.75                    ~ "Larger than 0.75 acres",
      TRUE                         ~ NA_character_)) %>%  
  mutate(
    Zoning_Group_Acre = case_when(
          ACRES > 0   & ACRES <= 0.25  ~ "Small lot zoned parcels",
          ACRES > 0.25 & ACRES <= 0.75     ~ "Medium lot zoned parcels",
          ACRES > 0.75                    ~ "Large lot zoned parcels",
          TRUE                         ~ NA_character_)) %>%
  mutate(
    Zoning_Group = case_when(
      `ZONING CODE` %in% c("A-1") ~ "Agricultural",
      `ZONING CODE` %in% c("R-0") ~ "Very large lot zones",
      `ZONING CODE` %in% c("R-1", "R-1A") ~ "Large lot zones",
      `ZONING CODE` %in% c("R-2", "R-2C", "R-2A", "R-2AC", "R-3AC", "R-3C", "R-3", "R-3A") ~ "Medium lot zones",
      `ZONING CODE` %in% c("R-5", "R-5A", "R-5AC", "R-5C", "R-6", "R-6C", "RTH", "RTHC", "RMP", "R-4", "R-4A", "R-4AC") ~ "Small lot zones",
      TRUE ~ NA_character_  ))


#Boxplot of codes and value
Henrico_LISA_Zoning_BG %>%
  filter(`Zoning_Group_Acre` %in% c("Large lot zoned parcels", "Medium lot zoned parcels")) %>%
ggplot(aes(x = factor(`Facet`), 
                                              # levels = c("R-0", "R-1")), 
                                               # levels = c("A-1", "R-0", "R-1", "R-1A", "R-2", "R-2A", "R-2AC", 
                                               #               "R-2C", "R-3", "R-3A", "R-3AC", "R-3C", "R-4", "R-4A", "R-4AC", 
                                               #               "R-5", "R-5A", "R-5AC", "R-5C", "R-6", "R-6C",
                                               #               "RMP", "R-O", "RPN", "RTH", "RTHC")), 
                                   y = Unit_Value)) +
  geom_boxplot(fill = "#80b1d3", color = "black") +
  # facet_grid( . ~ fct_relevel(Code_Age, "Single-family exclusive", "Single family and multifamily"), 
  #             scales = "free_x", space = "free") +
  facet_grid(fct_rev(`Zoning_Group_Acre`) ~ .,
             # scales = "free_y", 
             space = "free",
             switch = "y") +
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
  # coord_cartesian(ylim=c(0, 1000000)) +
  scale_y_continuous(labels = label_dollar())

#Geom density of parcel values
Henrico_LISA_Zoning_BG %>%
  filter(`Zoning_Group_Acre` %in% c("Large lot zoned parcels", "Medium lot zoned parcels")) %>%
  ggplot(aes(x = Unit_Value, y = Facet)) +
  # y = factor(`ZONING CODE`, 
  #                            levels = rev(c("A-1", "R-0", "R-1", "R-1A", "R-2", "R-2A", "R-2AC", 
  #                                           "R-2C", "R-3", "R-3A", "R-3AC", "R-3C", "R-4", "R-4A", "R-4AC", 
  #                                           "R-5", "R-5A", "R-5AC", "R-5C", "R-6", "R-6C",
  #                                           "RMP", "R-O", "RPN", "RTH", "RTHC"))))) +
  geom_density_ridges(quantile_lines = TRUE, quantiles = 2,
                      scale = 1.1,
                      rel_min_height = 0.005, 
                      aes(fill = Facet), col = "black") + 
  facet_grid(fct_rev(`Zoning_Group_Acre`) ~ .,
             # scales = "free_y", 
             space = "free",
             switch = "y") +
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

#Plot line chart by conc and non conc
Henrico_Med_Test <- Henrico_LISA_Zoning_BG %>%
  mutate(`SALE YEAR` = as.numeric(substr(`SALE DATE`, 1, 4))) %>%
  filter(`SALE AMOUNT` <= 2000000) %>%   
  filter(!`ZONING CODE` %in% c("RMH", "RMP")) %>%
  arrange(ACRES) %>%  # optional: explicitly sort first
  mutate(Acres_Tertile = ntile(ACRES, 3)) %>%
  filter(!is.na(Lot_Size)) %>%
  filter(!str_detect(PIN, "803-680-7933|763-730-8976|748-732-4135|754-731-8217")) %>%
  mutate(
    Affluent_Place = case_when(
      is.na(Income_2022) ~ NA_character_,
      Income_2022 > 125000 ~ "Affluent",
      TRUE ~ "Not affluent"
    )) %>%
  mutate(
    Affluence_Group = case_when(
      Affluent_Place == "Concentrated Affluence" ~ "Concentrated Affluence",
      Affluent_Place %in% c("Affluent", "Concentrated Affluence") ~ "Affluent",
      TRUE ~ "Not Affluent"
    )
  ) %>%
  # filter(`SALE YEAR` >= 1925) %>%   
  group_by(`SALE YEAR`, 
           Affluence_Group,
           `Facet`, 
           # `ZONING CODE`, 
           # Lot_Size,
           Zoning_Group_Acre
  ) %>%  
  summarise(Median_Unit_Value = median(`SALE AMOUNT`, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    Facet = str_replace_all(
      as.character(Facet),
      c(
        "Non-concentrated affluence" = "Areas not concentrated affluence",
        "Concentrated affluence" = "Concentrated affluence"))) %>%
  mutate(
    Facet = case_when(
      Facet == "Concentrated affluence" |
        Affluence_Group == "Concentrated affluence" ~ "Concentrated affluence",
      Affluence_Group == "Affluent" ~ "Affluent",
      TRUE ~ Facet))

#Adjust hosue value figures 
Henrico_Med_Test <- Henrico_Med_Test %>% 
  mutate(Median_Unit_Value_Inf = adjust_for_inflation(`Median_Unit_Value`, 
                                                      `SALE YEAR`, "US", to_date = 2022)) %>%
  filter(`Median_Unit_Value_Inf` <= 1500000)   %>%
  mutate(
    Affluence_Status = case_when(
      Facet == "Concentrated affluence" ~ "Concentrated Affluence",
      Affluence_Group == "Affluent" ~ "Affluent",
      TRUE ~ "Not affluent or concentrated affluent"
    )
  )

#Median sale value by year (and code) adjusted to 2020 dollars
Henrico_Med_Test %>%
  filter(`Zoning_Group_Acre` %in% c("Large lot zoned parcels", "Medium lot zoned parcels")) %>%
  ggplot(
       # %>%
       #   filter((`ZONING CODE` %in% c("A-1", "R-0", "R-1", "R-2", "R-3", "R-4", "R-5", "R-6", "RTH")))
       aes(x = `SALE YEAR`, y = Median_Unit_Value_Inf, color = `Facet`, 
           group = `Facet`)) +
  geom_line(size = 1) +
  facet_grid(fct_rev(`Zoning_Group_Acre`) ~ .,
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
  # geom_text(data = Henrico_Med_Test %>% filter(`Lot_Size` == "Larger than 0.75 acres"),  # Filtering inside the layer
  #           aes(x = 1960.25, y = 750000, angle = 0, label = "1960 zoning ordinance"),
  #           hjust = 0, color = "black", size = 3.5) +
  geom_vline(xintercept = 2021, color = "black", linetype = "solid", size = 0.75) +
  # geom_text(data = Henrico_Med_Test %>% filter(`Lot_Size` == "Larger than 0.75 acres"),  # Filtering inside the layer
  #           aes(x = 2008.5, y = 750000, angle = 0, label = "2021 zoning ordinance"),
  #           hjust = 0, color = "black", size = 3.5) +
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
       caption = "All sales adjusted to 2022 dollars"
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




#------------------------------------------------------------------------
#Calculating zoning percent of each block group above

  
#Create intersection between polygons
Henrico_BG_zoning_overlap <- st_join(Henrico_LISA_Zoning_BG, Income_LISA_Henrico)

#Count points by polygon
summary_poly <- Henrico_BG_zoning_overlap %>%
  group_by(GEOID) %>%
  count(Facet.x, Zoning_Group_Acre, name = "n")

# Total number of points
total_points <- nrow(Henrico_LISA_Zoning_BG)

# Total count of values across all zoning groups
total_value <- sum(summary_poly$n, na.rm = TRUE)

# Calculate shares
summary_poly <- summary_poly %>%
  mutate(
    share_points = n / total_points,
    share_value  = n / total_value
  ) %>%
  left_join(
    st_drop_geometry(Income_LISA_Henrico), 
    by = "GEOID"
  )


#point chart of LISA values by lot size, faceted by conc affluence
summary_poly %>%
  filter(`Zoning_Group_Acre` %in% c("Large lot zoned parcels")) %>%
  ggplot(aes(x = share_value, y = Local_M_i)) +
  geom_abline(intercept = 0, slope = 1, color = "black", alpha = 0.65, 
              linetype = "longdash", linewidth = 0.25) +
  geom_point(shape = 21,  # Use shape 21 for points with a border
             color = "black",  # Border color
             stroke = 0.5  # Thickness of the border
  ) +
  # facet_wrap(~ Facet.x, nrow = 2, strip.position = "top") +
  theme_minimal() +
  geom_smooth(method='lm', formula= y~x)


#---------------------------------------------------------------------------------
#Descriptive stats of parcels across Conc aff and non-Conc aff

#Percent each zoning code
Concentrated_Affluent_Values <- Henrico_LISA_Zoning_BG %>%
  filter(!is.na(Facet)) %>%
  group_by(Facet, `Zoning_Group_Acre`) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(percentage = 100 * n / sum(n))

#Percent each acreage
Concentrated_Affluent_Values <- Henrico_LISA_Zoning_BG %>%
  filter(!is.na(Facet)) %>%
  group_by(Facet, `Lot_Size`) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(percentage = 100 * n / sum(n))

#Unit share of land percent 
#Percent each zoning code
Concentrated_Affluent_Values <- Henrico_LISA_Zoning_BG %>%
  filter(!is.na(Facet)) %>%
  mutate(Unit_Percent = `LAND SQUARE FOOTAGE` / `TOTAL SQUARE FOOTAGE ALL BUILDINGS`) %>%
  group_by(Facet, `Zoning_Group_Acre`) %>%
    summarise(Median_Unit_Percent = median(Unit_Percent, na.rm = TRUE),
              Mode_Unit_Percent = as.numeric(names(sort(table(Unit_Percent), decreasing = TRUE)[1])),
              Min_Unit_Percent = min(Unit_Percent, na.rm = TRUE),
              Max_Unit_Percent = max(Unit_Percent, na.rm = TRUE),
              Range_Unit_Percent = Max_Unit_Percent - Min_Unit_Percent)

#Median/mean
  #Range needs filtering as there are two 20 acre farms
Concentrated_Affluent_Values <- Henrico_LISA_Zoning_BG %>%
  filter(!is.na(Facet)) %>%
  filter(!str_starts(`ZONING CODE`, "A")) %>%
  group_by(Facet, `Zoning_Group_Acre`) %>%
  summarise(Mean = mean(ACRES, na.rm = TRUE),
            Median = median(ACRES, na.rm = TRUE),
            Mode_ACRES = as.numeric(names(sort(table(ACRES), decreasing = TRUE)[1])),
            Min_ACRES = min(ACRES, na.rm = TRUE),
            Max_ACRES = max(ACRES, na.rm = TRUE),
            Range_ACRES = Max_ACRES - Min_ACRES)

#Percent built after...
Concentrated_Affluent_Values <- Henrico_LISA_Zoning_BG %>%
  filter(!is.na(Facet)) %>%
  group_by(Facet, `Zoning_Group_Acre`) %>%
  summarise(
    total_units = n(),
    units_built_after_2000 = sum(`YEAR BUILT` > 2000, na.rm = TRUE),
    percent_built_after_2000 = 100 * units_built_after_2000 / total_units
  )

#Percent sold after 2000
Concentrated_Affluent_Values <- Henrico_LISA_Zoning_BG %>%
  filter(!is.na(Facet)) %>%
  group_by(Facet, `Zoning_Group_Acre`) %>%
  summarise(
    total_units = n(),
    units_sold_after_threshold = sum(`SALE DATE` > 20150000, na.rm = TRUE),
    percent_sold_after_threshold = 100 * units_sold_after_threshold / total_units
  )


#-------------------------------------------------------------------------------
#Henrico Lisa and Parcel work

#Download block level data
#2020 Median HH income 
Henrico_Income_2022 <- get_acs(
  geography = "block group", 
  variables = "B19013_001", 
  state = ST,
  year = 2022,
  output = "wide",
  geometry = T) %>%
  filter(str_detect(GEOID, "51087")) %>%
  rename("Income_2022" = B19013_001E,
         "IncomeMOE_2022" = B19013_001M) %>%
  select(-NAME)

#Exploratory map of Henrico LISA
tmap_mode("view")  

tm_shape(Henrico_Income_2022) +
  tm_polygons(
    col = "Income_2022",     
    # palette = c("red", "grey70", "green"),  
    alpha = 0.7,
    border.col = "black"
  ) +
  tm_basemap("OpenStreetMap")  

#-------
#Lisa by block group
#Rerun LISA on just Henrico County tracts (as a test measure)
#Create dataframe
Income_LISA <- Henrico_Income_2022 %>%
  filter(!Income_2022 == "NA") %>%
  mutate(scaled_estimate = as.numeric(scale(Income_2022))) 

# Income_LISA$scaled_estimate <- as.numeric(scale(Income_LISA$Med_Income_Adj))

#Create spatial neighbors object
neighbors <- poly2nb(Income_LISA$geometry, queen = TRUE)
# summary(neighbors)

# Ensure your data is an sf object
Income_LISA <- st_as_sf(Income_LISA) 

# Calculate centroids and extract coordinates
Income_LISA_coords <- Income_LISA %>%
  st_centroid() %>%
  st_coordinates()

#Create weights 
weights <- nb2listw(neighbors, style = "W")

#Isolate weights
weights$weights[[1]]

#Run LISA
Income_LISA_Results <- localmoran_perm(
  Income_LISA$scaled_estimate, 
  weights, 
  nsim = 999L, 
  alternative = "two.sided"
) %>%
  as_tibble() %>%
  set_names(c("Local_M_i", "Expected_i", "Variance_i", "Z_i", "Pval_i",
              "Pval_i_sim", "Pvali_sim_folded", "Skewness", "Kurtosis"))

#Join LISA with income data
Income_LISA <- Income_LISA %>%
  select(GEOID, Income_2022, scaled_estimate) %>%
  mutate(lagged_estimate = lag.listw(weights, scaled_estimate)) %>%
  bind_cols(Income_LISA_Results) 

#Recreate string
Income_LISA$Local_M_i <- as.numeric(Income_LISA$Local_M_i)

#Set the clusters
Income_LISA_Henrico <- Income_LISA %>%
  mutate(lisa_cluster = case_when(
    Pval_i >= 0.05 ~ "Not significant",
    scaled_estimate > 0 & Local_M_i > 0 ~ "High-high", #High income, high-income neighbors
    scaled_estimate > 0 & Local_M_i < 0 ~ "High-low", #High income, low-income neighbors
    scaled_estimate < 0 & Local_M_i > 0 ~ "Low-low", #Low income, low-income neighbors
    scaled_estimate < 0 & Local_M_i < 0 ~ "Low-high" #Low income, high-income neighbors
  )) %>%
  mutate(Tract_type = case_when(
    Income_2022 >= 138750 ~ "Affluent tract",
    Income_2022 <= 27750 ~ "Poor tract",
  )) %>%
  mutate(Concentrations = case_when(
    Pval_i >= 0.05 ~ "Not significant",
    Income_2022 > 27750 & Income_2022 <= 138750 & Local_M_i < 0 ~ "Not significant",  # Middle income
    Income_2022 > 27750 & Income_2022 <= 138750 & Local_M_i > 0 ~ "Middle income clustered",  # Middle income
    Income_2022 >= 138750 & Local_M_i < 0 ~ "High-none", #High income, not concentrated
    Income_2022 >= 138750 & Local_M_i > 0 ~ "High-high", #High income, highly concentrated
    Income_2022 <= 27750 & Local_M_i > 0 ~ "Low-high", #Low income, highly concentrated
    Income_2022 <= 27750 & Local_M_i < 0 ~ "Low-none" #Low income, not concentrated
  )) %>%
  mutate(Facet = if_else(Concentrations == "High-high",
                         "Concentrated affluence",
                         "Non-concentrated affluence")) %>%
  select(GEOID, Income_2022, scaled_estimate, Local_M_i, Facet, geometry)

#Exploratory map of Henrico LISA
tmap_mode("view")  

tm_shape(Income_LISA_Henrico) +
  tm_polygons(
    col = "Facet",     
    # palette = c("red", "grey70", "green"),  
    alpha = 0.7,
    border.col = "black") 

#-----
#Comparing CA and Non-CA tracts
ACS_Variables <- load_variables(2022, "acs5", cache = TRUE)

#High school and college
Opportunity_Comparison <- get_acs(
  geography = "block group",
  state = "VA",
  table = "B15003",
  year = 2022,
  survey = "acs5",
  output = "wide"
) %>%
  mutate(
    total_25plus = B15003_001E,
    hs_complete = B15003_017E + B15003_018E + B15003_019E + B15003_020E + B15003_021E + B15003_022E + B15003_023E + B15003_024E + B15003_025E,
    college_complete = B15003_021E + B15003_022E + B15003_023E + B15003_024E + B15003_025E,    
    share_hs_complete = hs_complete / total_25plus,
    share_college_complete = college_complete / total_25plus
  ) %>%
  left_join(Income_LISA_Henrico, by = "GEOID") %>%
  select(GEOID, Facet, total_25plus, hs_complete, college_complete, share_hs_complete, share_college_complete) %>%
  filter(!is.na(Facet)) %>%
  # group_by(Facet) %>%
  summarise(
    total_25plus = sum(total_25plus, na.rm = TRUE),
    hs_complete = sum(hs_complete, na.rm = TRUE),
    college_complete = sum(college_complete, na.rm = TRUE),
    share_hs_complete = (hs_complete / total_25plus)*100,
    share_college_complete = (college_complete / total_25plus)*100
  )

#Public assistance and poverty
Opportunity_Comparison <- get_acs(
  geography = "block group",
  state = "VA",
  table = "B19057",
  year = 2022,
  survey = "acs5",
  output = "wide"
) %>%
  mutate(
    total_households = B19057_001E,
    households_pa = B19057_002E,
  ) %>%
  left_join(Income_LISA_Henrico, by = "GEOID") %>%
  filter(!is.na(Facet)) %>%
  group_by(Facet) %>%
  summarise(
    total_households = sum(total_households, na.rm = TRUE),
    households_pa = sum(households_pa, na.rm = TRUE),
    share_public_assistance = (households_pa / total_households)*100
  )

#Poverty
Opportunity_Comparison <- get_acs(
  geography = "block group",
  state = "VA",
  county = "Henrico",
  table = "B19001", # income bins
  year = 2022,
  survey = "acs5",
  output = "wide"
) %>%
  mutate(
    total_households = B19001_001E,
    households_below_fpl = rowSums(select(., B19001_002E:B19001_006E)),  # <$30k approx FPL
    share_below_fpl = households_below_fpl / total_households
  ) %>%
  left_join(
    get_acs(
      geography = "block group",
      state = "VA",
      county = "Henrico",
      table = "B19013", # median household income
      year = 2022,
      survey = "acs5",
      output = "wide"
    ) %>% select(GEOID, median_income = B19013_001E),
    by = "GEOID"
  ) %>%
  left_join(Income_LISA_Henrico, by = "GEOID") %>%
  filter(!is.na(Facet)) %>%
  group_by(Facet) %>%
  summarise(
    total_households = sum(total_households, na.rm = TRUE),
    households_below_fpl = sum(households_below_fpl, na.rm = TRUE),
    share_below_fpl = households_below_fpl / total_households,
    median_income = median(median_income, na.rm = TRUE)  # median of block groups
  )

#Unemployment
Opportunity_Comparison <- get_acs(
  geography = "block group",
  state = "VA",
  county = "Henrico",
  table = "B23025",
  year = 2022,
  survey = "acs5",
  output = "wide"
) %>%
  mutate(
    civ_labor_force = B23025_003E,
    civ_unemployed = B23025_005E) %>%
    left_join(Income_LISA_Henrico, by = "GEOID") %>%
  filter(!is.na(Facet)) %>%
  group_by(Facet) %>%
  summarise(
    labor_force = sum(civ_labor_force, na.rm = TRUE),
    unemployed = sum(civ_unemployed, na.rm = TRUE),
    unemployment_rate = (unemployed / labor_force)*100
  ) %>%
    summarise(
    labor_force = sum(labor_force),
    unemployed = sum(unemployed),
    unemployment_rate = (unemployed / labor_force) * 100
  ) %>%
  mutate(Facet = "Henrico County Overall") %>%
  select(Facet, everything())

#Share White/Black
Opportunity_Comparison <- get_acs(
  geography = "block group",
  state = "VA",
  county = "Henrico",
  table = "B02001",
  year = 2022,
  survey = "acs5",
  output = "wide"
) %>%
  mutate(
    total_pop = B02001_001E,
    white_pop = B02001_002E,
    black_pop = B02001_003E,
    asian_pop = B02001_005E,
    # Sum of all other categories: AIAN + NHOPI + Some other race + Two or more races
    other_pop = rowSums(select(., B02001_004E, B02001_006E, B02001_007E, B02001_008E), na.rm = TRUE)
  ) %>%
  left_join(Income_LISA_Henrico, by = "GEOID") %>%
  filter(!is.na(Facet)) %>%
  group_by(Facet) %>%
  summarise(
    total_pop = sum(total_pop, na.rm = TRUE),
    white_pop = sum(white_pop, na.rm = TRUE),
    black_pop = sum(black_pop, na.rm = TRUE),
    asian_pop = sum(asian_pop, na.rm = TRUE),
    other_pop = sum(other_pop, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    share_white = (white_pop / total_pop) * 100,
    share_black = (black_pop / total_pop) * 100,
    share_asian = (asian_pop / total_pop) * 100,
    share_other = (other_pop / total_pop) * 100,
    share_facet = (total_pop / sum(total_pop)) * 100  # % of Henrico population in each facet
  )
# %>%
#   ungroup() %>%
#   summarise(
#     total_pop = sum(total_pop, na.rm = TRUE),
#     white_pop = sum(white_pop, na.rm = TRUE),
#     black_pop = sum(black_pop, na.rm = TRUE)
#   ) %>%
#   mutate(
#     share_white = (white_pop / total_pop) * 100,
#     share_black = (black_pop / total_pop) * 100
#   )

#--------------------------
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
Henrico_Zoning_parcels <- read_rds(paste0(onedrivepath, "Mapping Richmond/Parcel-Buildings/Henrico/Henrico_Buildings_small.rds")) %>%
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
  st_as_sf(coords = c("PARCEL LEVEL LONGITUDE", "PARCEL LEVEL LATITUDE"), crs = 4326) %>%
  select(-Number_of_Units)

#Join the LISA and zoning
#Match crs
Henrico_Zoning_parcels <- st_transform(Henrico_Zoning_parcels, st_crs(Income_LISA_Henrico))
# Hanover_Zoning_parcels <- st_transform(Hanover, st_crs(Income_LISA_Henrico))

# Zoning_Parcels <- rbind(Hanover_Zoning_parcels, Henrico_Zoning_parcels)

#Merge parcel and conc aff
Zoning_Parcels_Income <- st_join(Henrico_Zoning_parcels, Income_LISA_Henrico[, c("Facet", "Income_2022",
                                                                                  "scaled_estimate", "Local_M_i")]) %>%
  #then we remove the NAs, the parcels that are located in census tracts with no income and no LISA value
  filter(!is.na(Facet)) %>%
  filter(!is.na(ACRES)) %>%
  # filter(!str_detect(`ZONING CODE`, "A-1")) %>% #Test filter Agricultural
  mutate(
    Lot_Size = case_when(
      ACRES > 0   & ACRES <= 0.25  ~ "0 - 0.25 acres",
      ACRES > 0.25 & ACRES <= 0.75     ~ "0.25 - 0.5 acres",
      ACRES > 0.75                    ~ "Larger than 0.75 acres",
      TRUE                         ~ NA_character_)) %>%  
  # mutate(
  #   Zoning_Group_Acre = case_when(
  #     ACRES > 0   & ACRES <= 0.25  ~ "Small lot zoned parcels",
  #     ACRES > 0.25 & ACRES <= 0.5     ~ "Medium lot zoned parcels",
  #     ACRES > 0.5                    ~ "Large lot zoned parcels",
  #     TRUE                         ~ NA_character_)) 
  # mutate(
  #   Zoning_Group_Acre = case_when(
  #     `ZONING CODE` %in% c("A-1", "AR-1", "AR-2", "AR-6") ~ "Agricultural",
  #     `ZONING CODE` %in% c("R-0", "RC", "RR-1", "R-1", "R-1A") ~ "Large lot zoned parcels",
  #     `ZONING CODE` %in% c("R-2", "R-2C", "R-2A", "R-2AC", "R-3AC", "R-3C", "R-3", "R-3A") ~ "Medium lot zoned parcels",
  #     `ZONING CODE` %in% c("R-5", "R-5A", "R-5AC", "R-5C", "R-6", "R-6C", "RTH", "RTHC", "RMP", "R-4", "R-4A", "R-4AC", "RM", "RS") ~ "Small lot zoned parcels",
  #     TRUE ~ NA_character_  ))
  mutate(
    Zoning_Group_Acre = case_when(
      `ZONING CODE` %in% c("A-1", "AR-1", "AR-2", "AR-6") ~ "Agricultural",
      `ZONING CODE` %in% c("R-0", "RC") ~ "Large lot zoned parcels",
      `ZONING CODE` %in% c("RR-1", "R-1", "R-1A") ~ "Large lot zoned parcels",
      `ZONING CODE` %in% c("R-2", "R-2C", "R-2A", "R-2AC", "R-3AC", "R-3C", "R-3", "R-3A") &
        ACRES > 1 ~ "Large lot zoned parcels",
      `ZONING CODE` %in% c("R-2", "R-2C", "R-2A", "R-2AC", "R-3AC", "R-3C", "R-3", "R-3A") &
        ACRES <= 1 ~ "Medium lot zoned parcels",
      `ZONING CODE` %in% c("R-5", "R-5A", "R-5AC", "R-5C", "R-6", "R-6C", "RTH", "RTHC", 
                           "RMP", "R-4", "R-4A", "R-4AC", "RM", "RS") ~ "Small lot zoned parcels",
      TRUE ~ NA_character_ ))
  # mutate(
  #   Zoning_Group_Acre = case_when(
  #     `ZONING CODE` %in% c("A-1", "AR-1", "AR-2", "AR-6") ~ "Agricultural",
  #     ACRES > 1 ~ "Large lot zoned parcels",
  #     `ZONING CODE` %in% c("R-1", "R-1A",
  #                          "R-2", "R-2C", "R-2A", "R-2AC",
  #                          "R-3AC", "R-3C", "R-3", "R-3A")
  #     ~ "Medium lot zoned parcels",
  #     `ZONING CODE` %in% c("R-5", "R-5A", "R-5AC", "R-5C",
  #                          "R-6", "R-6C", "RTH", "RTHC",
  #                          "RMP", "R-4", "R-4A", "R-4AC",
  #                          "RM", "RS")
  #     ~ "Small lot zoned parcels",
  #     TRUE ~ NA_character_))

#Outliers
Outliers <-  Zoning_Parcels_Income %>%
  mutate(`SALE YEAR` = as.numeric(substr(`SALE DATE`, 1, 4))) %>%
  filter(str_detect(`SALE YEAR`, "2007")) %>%
  filter(Zoning_Group_Acre %in% c("Large lot zoned parcels")) %>%
  # mutate(
  #   Affluent_Group = Facet != "Concentrated affluence" & Income_2022 >= 138750
  # ) %>%
  # filter(Facet %in% c("Non-concentrated affluence")) %>%
  filter(!`Zoning_Group_Acre` %in% c("Agricultural")) %>%
  mutate(Median_Unit_Value_Inf = adjust_for_inflation(`SALE AMOUNT`,
                                                      `SALE YEAR`, "US", to_date = 2022)) %>%
  # mutate(
  #   Facet = if_else(Income_2022 > 138750, 
  #                   "Concentrated affluence", "Non-concentrated affluence")) %>%  
  filter(Facet %in% c("Non-concentrated affluence")) 

#Lot size
OUTLIERS <- c("735-776-0588|741-736-6965|743-735-0975|750-733-4242|755-736-1847|738-747-9304|745-731-9112|738-739-7705|
               803-680-7933|763-730-8976|750-737-6343|756-741-3706|753-738-7149|754-767-0176|740-781-2953.033|740-781-2953.052|
               732-771-9082 756-770-3110|756-770-3110|761-773-1380|738-732-1228|742-755-6442|752-734-3747|763-768-8824|764-768-0700|
               748-732-4135|754-731-8217|769-750-7033|735-775-3488|738-767-5664|737-767-7352|768-768-4680|756-735-8377.905|
               749-734-4747|811-688-3052|735-778-9229|769-760-7554|748-733-7225|743-733-2625|793-745-2509|745-735-1477|728-759-5517|
               856-708-3890|740-744-3452|763-731-9711|745-769-7520|752-754-6407|768-778-1006|768-777-1390|741-757-8038|740-738-6948")

OUTLIERS <- c("793-757-4383|746-736-1603|745-736-3594|739-739-6593|741-740-3276|742-740-5584|741-740-8986|742-740-5542|805-737-7793|
              745-735-1477|740-740-5955|741-740-3276|742-740-5584|741-740-8986|742-740-5542|766-754-7342|832-714-1493|814-723-9520|
              819-720-4239|772-765-6720|802-693-3130|800-690-7298|774-755-9001740-740-2098|770-775-4450|731-759-5663|744-738-4036|
              804-691-1459|808-705-3773741-759-9411|745-769-7520|741-777-7897|741-759-9411|739-741-3559|746-735-6198|806-676-7240|
              803-692-2373|815-722-8456|768-779-2223|742-740-3319|770-754-0948|757-746-9814|772-765-6720|802-693-3130|742-740-5542|
              750-737-2226|774-755-9001|827-727-2829|778-751-5408")

#Code
OUTLIERS <-  c("742-739-9837|813-716-6294|759-763-9056|
               741-739-8043|742-739-2299|742-739-3397|742-739-7443|739-740-6347|
                759-736-5541|768-778-1006|768-777-1390|744-737-5297|745-735-9855|745-735-9855|768-778-0091|759-736-5541|
                743-739-1128|745-735-7181|768-776-1349|768-776-3720|741-740-8423|749-737-8502|746-735-5383|742-740-0046|
                749-736-9080|741-739-5096|741-740-4923|740-740-0840|742-740-5668|741-739-6295|742-739-0356|747-734-9733
               |768-778-1006|768-777-1390|744-737-5297|745-735-9855|
               744-738-4036|768-779-2223|742-740-3319|768-778-4940|740-741-4308|740-740-6139|770-774-3118|768-777-8484|
               741-741-2615|768-779-8627|746-736-2117|746-736-5119|769-778-0780|768-778-9095|768-778-8312|
               741-740-6659|739-739-8399|745-736-1108|745-737-0325|744-738-3207|748-734-6131|748-736-5779|742-740-3544|742-739-4948|
               745-736-8018|757-734-2776|752-735-0768")

# #Filter and Median for just affluent tracts
Zoning_Parcels_Affluent_Median <- Zoning_Parcels_Income %>%
  mutate(`SALE YEAR` = as.numeric(substr(`SALE DATE`, 1, 4))) %>%
  mutate(
    Affluent_Group = Facet != "Concentrated affluence" & Income_2022 >= 138750
  ) %>%
  filter(!Zoning_Group_Acre %in% c("Agricultural", "Very Large lot zoned parcels")) %>%
  # filter(Facet %in% c("Concentrated affluence")) %>%
  # filter(!Zoning_Group_Acre %in% c("Small lot zoned parcels")) %>%
  filter(!Zoning_Group_Acre %in% c("Agricultural")) %>%
  filter(!Affluent_Group %in% c("False")) %>%
  # filter(!is.na(Income_2022) & Income_2022 >= 138750) %>%
  # arrange(ACRES) %>%  # optional: explicitly sort first
  # mutate(Acres_Tertile = ntile(ACRES, 3)) %>%
  filter(!is.na(Lot_Size)) %>%
  filter(!str_detect(PIN, OUTLIERS)) %>%
  filter(
    !is.na(`SALE AMOUNT`),
    !is.na(`SALE YEAR`))  %>%
  mutate(Median_Unit_Value_Inf = adjust_for_inflation(`SALE AMOUNT`,
                                                      `SALE YEAR`, "US", to_date = 2022)) %>%
  filter(`Median_Unit_Value_Inf` <= 1500000) %>%
  filter(`SALE AMOUNT` <= 1000000) %>%
  # filter(`SALE AMOUNT` >= 20000) %>%
  group_by(`SALE YEAR`,
           # `ZONING CODE`,
           # Lot_Size,
           Zoning_Group_Acre,
           Affluent_Group
  ) %>%
  summarise(Median_Unit_Value = median(Median_Unit_Value_Inf, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(`Median_Unit_Value` <= 1500000)  %>%
  filter(!Affluent_Group %in% c(FALSE))


#Adjust house value figures 
Zoning_Parcels_Income_Median <- Zoning_Parcels_Income %>%
  mutate(`SALE YEAR` = as.numeric(substr(`SALE DATE`, 1, 4))) %>%
  # mutate(
  #   Facet = if_else(Income_2022 > 138750, 
  #                            "Concentrated affluence", "Non-concentrated affluence")) %>%  
  filter(!`ZONING CODE` %in% c("RMH", "RMP")) %>%
  filter(!Zoning_Group_Acre %in% c("Agricultural", "Very Large lot zoned parcels")) %>%
  # arrange(ACRES) %>%  # optional: explicitly sort first
  # mutate(Acres_Tertile = ntile(ACRES, 3)) %>%
  filter(!is.na(Lot_Size)) %>%
  # filter(!str_detect(PIN, OUTLIERS)) %>%
  # filter(
  #   CoreLogic_Description %in% c(
  #     # "APARTMENT",
  #     "CONDOMINIUM",
  #     "MANUFACTURED HOME",
  #     "RES-IMPROVED < 5 ACRES",
  #     "RES-IMPROVED 5-10 ACRES",
  #     "RES-IMPROVED 10-20 ACRES",
  #     "RES-IMPROVED 20-100 ACRES",
  #     "RES-IMPROVED > 100 ACRES",
  #     "RES-SUBD(1 FAM)",
  #     "RES-SUBD(2 FAM)",
  #     "RES-SUBD(3 FAM)",
  #     "TOWNHOUSE"
  #   )
  # ) %>%
  filter(
    !is.na(`SALE AMOUNT`),
    !is.na(`SALE YEAR`))  
# %>% 
#   filter(!(
#     `SALE YEAR` %in% c(1984, 2003, 2005) &
#       Zoning_Group_Acre == "Large lot" &
#       min_rank(desc(`SALE AMOUNT`)) <= 5
#   )) %>%
#   ungroup()

#Find outlier buildings
Outliers <- Zoning_Parcels_Income_Median %>%
  # mutate(`SALE YEAR` = as.numeric(substr(`SALE DATE`, 1, 4))) %>%
  mutate(Median_Unit_Value_Inf = adjust_for_inflation(`SALE AMOUNT`,
                                                      `SALE YEAR`, "US", to_date = 2022)) %>%
  group_by(Facet, County_Description, Zoning_Group_Acre) %>%
  summarise(
    Median_Median_Unit_Value_Inf = median(Median_Unit_Value_Inf, na.rm = TRUE),
    .groups = "drop"  # Ungroup automatically
  ) %>%
  filter(Zoning_Group_Acre == "Large lot zoned parcels")

#MEdian and adjust for graph
Zoning_Parcels_Income_Median <- Zoning_Parcels_Income_Median %>%
  # filter(!(CoreLogic_Description == "VACANT 5-10 ACRES")) %>%
  # filter(!(CoreLogic_Description == "VACANT < 5 ACRES")) %>%
  # filter(!(CoreLogic_Description == "COMMON AREA/MASTER CARD")) %>%
  # filter(!(CoreLogic_Description == "RES-IMPROVED 10-20 ACRES")) %>%
  # filter(!(County_Description == "Vacant 5 - 10 Acres")) %>%
  mutate(Median_Unit_Value_Inf = adjust_for_inflation(`SALE AMOUNT`, 
                                                      `SALE YEAR`, "US", to_date = 2022)) %>%
  filter(`Median_Unit_Value_Inf` <= 1500000) %>%
  filter(`SALE AMOUNT` <= 1000000) %>%
  # filter(`SALE AMOUNT` >= 20000) %>%   
  group_by(`SALE YEAR`, 
           `Facet`, 
           # `ZONING CODE`, 
           # Lot_Size,
           Zoning_Group_Acre
  ) %>%
  summarise(
    p10 = quantile(Median_Unit_Value_Inf, 0.10, na.rm = TRUE),
    p25 = quantile(Median_Unit_Value_Inf, 0.25, na.rm = TRUE),
    Median_Unit_Value = quantile(Median_Unit_Value_Inf, 0.50, na.rm = TRUE), # median
    p75 = quantile(Median_Unit_Value_Inf, 0.75, na.rm = TRUE),
    p90 = quantile(Median_Unit_Value_Inf, 0.90, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  # summarise(Median_Unit_Value = median(Median_Unit_Value_Inf, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(`Median_Unit_Value` <= 1500000)  

#Rebase to 1980
Zoning_Parcels_Rebased <- Zoning_Parcels_Income_Median %>%
  group_by(`Facet`, Zoning_Group_Acre) %>%
  mutate(
    base_1980 = Median_Unit_Value[`SALE YEAR` == 1980][1]
  ) %>%
  ungroup() %>%
  mutate(
    p10_idx = (p10 / base_1980) * 100,
    p25_idx = (p25 / base_1980) * 100,
    p50_idx = (Median_Unit_Value / base_1980) * 100,
    p75_idx = (p75 / base_1980) * 100,
    p90_idx = (p90 / base_1980) * 100
  )

#Geom_Ribbon plotting
Zoning_Parcels_Rebased %>%
  ggplot(aes(x = `SALE YEAR`,
             color = `Facet`,
             fill = `Facet`,
             group = `Facet`)) +
  geom_ribbon(aes(ymin = p25_idx, ymax = p75_idx),
              alpha = 0.4,
              color = NA) +
  # geom_ribbon(aes(ymin = p10_idx, ymax = p90_idx),
  #             alpha = 0.2,
  #             color = NA) +
  geom_line(aes(y = p50_idx), size = 1) +
  facet_grid(fct_rev(`Zoning_Group_Acre`) ~ .,
             space = "free",
             switch = "y") +
  geom_vline(xintercept = 1960, color = "black", size = 0.75) +
  geom_vline(xintercept = 2021, color = "black", size = 0.75) +
  theme_minimal() +
  scale_y_continuous(name = "Indexed parcel value (1980 = 100)",
    breaks = c(50, 100, 150, 200, 250, 300, 350),
    position = "right") +
  scale_color_manual(values = c("Areas not concentrated affluence" = "grey",
                                "Concentrated affluence" = "#7f3b08"), guide = "none") +
  scale_fill_manual(values = c("Areas not concentrated affluence" = "grey",
                               "Concentrated affluence" = "#7f3b08"), guide = "none") +
  labs(
    # title = "Henrico County",
    subtitle = "Indexed parcel values (1980 = 100) across areas of<br>
    <span style='color:#7f3b08;'>concentrated affluence</span> and 
    <span style='color:darkgrey;'>not concentrated affluence</span>",
    #subtitle = "Parcel values across areas of<br><span style='color:#7f3b08;'>concentrated affluence</span>, <span style='color:#8da0cb;'>affluent but not concentrated</span>, and <span style='color:darkgrey;'>not concentrated or affluent</span>",
       x = NULL,
       y = "Median parcel sale value",
       caption = "Ribbon shows interquartile range; values adjusted to 2022 dollars"
  ) +
  # scale_y_continuous(labels = label_dollar(),
  #                    breaks = c(0, 250000, 500000, 750000, 1000000, 1250000, 1500000),
  #                    position = "right") +
  scale_x_continuous(breaks = c(1950, 1960, 1970, 1980, 1990, 2000, 2010, 2020)) +
  coord_cartesian(xlim=c(1980, 2020)) +
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
        panel.grid.minor.y = element_line(size = 0, color = "lightgrey"),
        panel.border = element_rect(color = "black", fill = NA, size = 0.75)) 


#Plot
Zoning_Parcels_Income_Median %>%
  # filter(`Zoning_Group_Acre` %in% c("Large lot zoned parcels", "Medium lot zoned parcels")) %>%
  ggplot(
    # %>%
    #   filter((`ZONING CODE` %in% c("A-1", "R-0", "R-1", "R-2", "R-3", "R-4", "R-5", "R-6", "RTH")))
    aes(x = `SALE YEAR`, y = Median_Unit_Value, color = `Facet`, 
        group = `Facet`)) +
  # geom_line(
  #   data = Zoning_Parcels_Affluent_Median,
  #   aes(x = `SALE YEAR`,
  #       y = Median_Unit_Value,
  #       group = Affluent_Group),
  #   color = "#8da0cb",
  #   linewidth = 0.75,
  #   # linetype = "dashed"
  # ) +
  # facet_grid(fct_rev(`Zoning_Group_Acre`) ~ .,
  #            space = "free",
  #            switch = "y") +
  geom_line(size = 1) +
  facet_grid(fct_rev(`Zoning_Group_Acre`) ~ .,
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
  # geom_text(data = Henrico_Med_Test %>% filter(`Lot_Size` == "Larger than 0.75 acres"),  # Filtering inside the layer
  #           aes(x = 1960.25, y = 750000, angle = 0, label = "1960 zoning ordinance"),
  #           hjust = 0, color = "black", size = 3.5) +
  geom_vline(xintercept = 2021, color = "black", linetype = "solid", size = 0.75) +
  # geom_text(data = Henrico_Med_Test %>% filter(`Lot_Size` == "Larger than 0.75 acres"),  # Filtering inside the layer
  #           aes(x = 2008.5, y = 750000, angle = 0, label = "2021 zoning ordinance"),
  #           hjust = 0, color = "black", size = 3.5) +
  # scale_fill_manual(values = c("Urban" = "#c8edc7", "Unstable" = "#e8c2ed", "Suburban" = "#fae3c5"), guide = "none") +
  # geom_smooth(span = 0.1, method = "loess", fill = "lightgrey", alpha = 0, size = 0.85) +
  # geom_hline(yintercept = 1, color = 'black', linetype = 'dashed') +
  theme_minimal() +
  scale_y_continuous(labels = label_dollar(),
                     breaks = c(0, 250000, 500000, 750000, 1000000),
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
  labs(# subtitle = "Parcel values across areas of<br><span style='color:#7f3b08;'>concentrated affluence</span> and those <span style='color:darkgrey;'>not concentrated affluence</span>",
       subtitle = "Parcel values across areas of<br><span style='color:#7f3b08;'>concentrated affluence</span>, <span style='color:#8da0cb;'>affluent but not concentrated</span>, and <span style='color:darkgrey;'>not concentrated or affluent</span>",
       x = NULL,
       y = "Median parcel sale value",
       caption = "All sales adjusted to 2022 dollars"
  ) +
  # scale_y_continuous(labels = label_dollar(),
  #                    breaks = c(0, 250000, 500000, 750000, 1000000, 1250000, 1500000),
  #                    position = "right") +
  scale_x_continuous(breaks = c(1950, 1960, 1970, 1980, 1990, 2000, 2010, 2020)) +
  coord_cartesian(xlim=c(1980, 2020)) +
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

ggsave("Henrico_Sales_By_LotSize.png",
       path = "~/desktop",
       width = 9,
       height = 12,
       units = "in",
       dpi = 500)


#Geom density of parcel values
Zoning_Parcels_Income_Ridges <- Zoning_Parcels_Income %>% 
  mutate(`SALE YEAR` = as.numeric(substr(`SALE DATE`, 1, 4))) %>%
  mutate(Median_Unit_Value_Inf = adjust_for_inflation(`SALE AMOUNT`, 
                                                      `SALE YEAR`, "US", to_date = 2022)) %>%
  # filter(!str_detect(PIN, OUTLIERS)) %>%
  filter(!`ZONING CODE` %in% c("RMH", "RMP")) %>%
  filter(!Zoning_Group_Acre %in% c("Agricultural")) 
  
  

Zoning_Parcels_Income_Ridges %>%
  filter(!`Zoning_Group_Acre` %in% c("Very Large lot zoned parcels")) %>%
  ggplot(aes(x = Median_Unit_Value_Inf, y = Facet)) +
  # y = factor(`ZONING CODE`, 
  #                            levels = rev(c("A-1", "R-0", "R-1", "R-1A", "R-2", "R-2A", "R-2AC", 
  #                                           "R-2C", "R-3", "R-3A", "R-3AC", "R-3C", "R-4", "R-4A", "R-4AC", 
  #                                           "R-5", "R-5A", "R-5AC", "R-5C", "R-6", "R-6C",
  #                                           "RMP", "R-O", "RPN", "RTH", "RTHC"))))) +
  geom_density_ridges(quantile_lines = TRUE, quantiles = 2,
                      scale = 1,
                      rel_min_height = 0.0025, 
                      aes(fill = Facet), col = "black") + 
  facet_grid(fct_rev(`Zoning_Group_Acre`) ~ .,
             # scales = "free_y", 
             space = "free",
             switch = "y") +
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
    subtitle = "Parcel values across areas of<br><span style='color:#7f3b08;'>concentrated affluence</span> and those <span style='color:darkgrey;'>not concentrated affluence</span>",
    y = NULL,
    x = NULL
  ) +
  theme(plot.subtitle = element_markdown(hjust = 0.5, size = 13, face = "bold"),
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
  ) +
  coord_cartesian(xlim=c(50000, 1750000)) 
  
ggsave("Ridges_Sales_Values_Zoning_Size.png",
       path = "~/desktop",
       width = 9,
       height = 8,
       units = "in",
       dpi = 500)

#--------------------------------------------------------------------------------
#Descriptive for the table

#Median year built by NCA and CA
Table_Descriptives <- Zoning_Parcels_Income_Ridges %>%
  filter(!`ZONING CODE` %in% c("A-1", "R-0", "RC")) %>%
  group_by(Facet, Zoning_Group_Acre) %>%
  summarise(Median_Value = median(`YEAR BUILT`, na.rm = TRUE))

#Percent each unit
Table_Descriptives <- Zoning_Parcels_Income_Ridges %>%
  # filter(!Facet %in% c("Concentrated affluence")) %>%
  group_by(Zoning_Group_Acre) %>%
  summarise(
    Count = n(),
    .groups = "drop"
  ) %>%
  mutate(
    Share = Count / sum(Count) * 100
  )

#Number of year builts per decade
Table_Descriptives <- Zoning_Parcels_Income_Ridges %>%
  filter(!`ZONING CODE` %in% c("A-1")) %>%
  mutate(
    `YEAR BUILT` = as.numeric(substr(`YEAR BUILT`, 1, 4)),
    Decade = cut(
      `YEAR BUILT`,
      breaks = seq(1950, 2030, by = 10),
      right = FALSE,
      labels = c("1950s","1960s","1970s","1980s","1990s","2000s","2010s","2020s"),
      ordered_result = TRUE
    )
  ) %>%
  filter(!is.na(Decade)) %>%
  
  group_by(Zoning_Group_Acre, Facet, Decade) %>%
  summarise(
    Count = n(),
    .groups = "drop"
  ) %>%
  group_by(Zoning_Group_Acre) %>%
  mutate(
    Percent = Count / sum(Count) * 100
  ) %>%
  arrange(Zoning_Group_Acre, Decade) %>%
  group_by(Zoning_Group_Acre) %>%
  mutate(
    Cumulative_Percent = cumsum(Percent)
  ) %>%
  ungroup()

#Share of housing stock prior to 2000
Total_Parcels <- Zoning_Parcels_Income_Ridges %>%
  filter(!`ZONING CODE` %in% c("A-1")) %>%
  group_by(Facet, Zoning_Group_Acre) %>%
  # nrow() %>%
  summarise(Total_Parcels = n(), .groups = "drop")


Table_Descriptives <- Zoning_Parcels_Income_Ridges %>%
  filter(!`ZONING CODE` %in% c("A-1")) %>%
  filter(!Zoning_Group_Acre %in% c("Very Large lot zoned parcels")) %>%
  mutate(`SALE DATE` = as.numeric(substr(`SALE DATE`, 1, 4))) %>%
  filter(`SALE DATE` >= 2012) %>%
  group_by(Facet, Zoning_Group_Acre) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Facet) %>%
  mutate(Share = round(Count / sum(Count) * 100, 2))

#Share built since 2017
Table_Descriptives <- Zoning_Parcels_Income_Ridges %>%
  filter(!`ZONING CODE` %in% c("A-1"),
         !Zoning_Group_Acre %in% c("Very Large lot zoned parcels")) %>%
  group_by(Zoning_Group_Acre, Facet) %>%
  summarise(
    Total_Parcels = n(),
    Pre2000_Parcels = sum(`YEAR BUILT` <= 2000, na.rm = TRUE),
    Share_Pre2000 = round(Pre2000_Parcels / Total_Parcels * 100, 2),
    .groups = "drop"
  )

#Count number of parcels (and share), Mean/Median acres
Table_Descriptives <- Zoning_Parcels_Income_Ridges %>%
  filter(!Zoning_Group_Acre %in% c("Very Large lot zoned parcels")) %>%
  group_by(Facet, Zoning_Group_Acre) %>%
  summarise(
    n = n(),
    Percent = round(n() / nrow(Zoning_Parcels_Income_Ridges) * 100, 2),
    Mean_Acres = round(mean(ACRES, na.rm = TRUE), 2),
    Median_Acres = round(median(ACRES, na.rm = TRUE), 2),
    Median_Coverage = round(median(`TOTAL SQUARE FOOTAGE ALL BUILDINGS` / `LAND SQUARE FOOTAGE` * 100, na.rm = TRUE), 2)
  )

#Reworking descriptives
# Step 1: Summarise from raw data (don't overwrite Test)
summary_df <- Zoning_Parcels_Income_Ridges %>%
  clean_names() %>%
  group_by(zoning_group_acre, facet) %>%
  summarise(
    n = n(),
    mean_acres = mean(acres, na.rm = TRUE),
    median_acres = median(acres, na.rm = TRUE),
    share_built_after_2000 = mean(year_built >= 2000, na.rm = TRUE) * 100,
    median_year_built = median(year_built, na.rm = TRUE),
    share_sold_last_10 = mean(sale_year >= 2014, na.rm = TRUE) * 100,
    median_sale_year = median(sale_year, na.rm = TRUE),
    median_lot_coverage = median(far, na.rm = TRUE) * 100,
    .groups = "drop"
  )

# Step 2: Pivot long
summary_long <- summary_df %>%
  pivot_longer(
    cols = c(mean_acres, median_acres, share_built_after_2000,
             median_year_built, share_sold_last_10, median_sale_year,
             median_lot_coverage),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(metric = recode(metric,
                         "mean_acres"             = "Mean Acres",
                         "median_acres"           = "Median Acres",
                         "share_built_after_2000" = "% Built After 2000",
                         "median_year_built"      = "Median Year Built",
                         "share_sold_last_10"     = "% Sold Last 10 Yrs",
                         "median_sale_year"       = "Median Sale Year",
                         "median_lot_coverage"    = "Median Lot Coverage"
  ))

# Step 3: Dumbbell Plot
summary_long %>%
  pivot_wider(names_from = facet, values_from = value) %>%
  clean_names() %>%
  # rename(non_concentrated_affluence = `non_concentrated_affluence`) %>%  
  # if still broken, force it:
  setNames(make.names(names(.), unique = TRUE)) %>%
  rename(
    con = concentrated_affluence,
    non_con = `Non.concentrated.affluence`
  ) %>%
  ggplot(aes(y = metric)) +
  geom_segment(aes(
    x = non_con,
    xend = con,
    yend = metric
  ), color = "grey60", linewidth = 1) +
  geom_point(aes(x = con), color = "#7f3b08", size = 3) +
  geom_point(aes(x = non_con), color = "grey40", size = 3) +
  facet_grid(fct_rev(zoning_group_acre) ~ ., scales = "free_x", switch = "y") +
  theme_minimal(base_size = 13) +
  labs(
    title = "Parcel characteristics by affluence concentration",
    subtitle = "<span style='color:#7f3b08;'>Concentrated affluence</span> vs <span style='color:grey40;'>Non-concentrated affluence</span>",
    x = NULL, y = NULL
  ) +
  theme(
    plot.subtitle = element_markdown(size = 12),
    strip.placement = "outside",
    strip.text.y = element_markdown(size = 11, face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, size = 0.75),
    panel.grid.major.y = element_line(color = "grey90")
  )

# Step 4: Heatmap
summary_long %>%
  group_by(metric) %>%
  mutate(scaled = scale(value)[, 1]) %>%
  ungroup() %>%
  mutate(group_label = paste0(zoning_group_acre, "\n", facet)) %>%
  ggplot(aes(x = group_label, y = metric, fill = scaled)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = round(value, 1)), size = 3, color = "black") +
  scale_fill_gradient2(
    low = "grey90", mid = "white", high = "#7f3b08",
    midpoint = 0, name = "Scaled value"
  ) +
  theme_minimal(base_size = 12) +
  labs(
    title = "Parcel characteristics heatmap",
    subtitle = "Values normalized within each metric row",
    x = NULL, y = NULL
  ) +
  theme(
    axis.text.x = element_text(angle = 35, hjust = 1, size = 10),
    axis.text.y = element_text(size = 11),
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold", size = 16))

    #Share of each unit 
#Count number of parcels (and share), Mean/Median acres
Table_Descriptives <- Zoning_Parcels_Income_Ridges %>%
  filter(!`ZONING CODE` %in% c("A-1")) %>%
  group_by(Facet, `ZONING CODE`) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Facet) %>%
  mutate(Percent = round(n / sum(n) * 100, 2))

#Each unit's share of overall
Table_Descriptives <- Zoning_Parcels_Income_Ridges %>%
  filter(!`ZONING CODE` %in% c("A-1")) %>%
  group_by(Zoning_Group_Acre) %>%
  summarise(
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(
    Percent = round(n / sum(n) * 100, 2)
  )

#Share of res units
Table_Descriptives <- Zoning_Parcels_Income_Ridges %>%
  filter(!`ZONING CODE` %in% c("A-1")) %>%
  group_by(Facet, `ZONING CODE`) %>%
  summarise(
    Count = n(),
    .groups = "drop"
  ) %>%
  mutate(
    Share = Count / sum(Count) * 100
  ) %>%
  arrange(`ZONING CODE`) %>%
  group_by(Zoning_Group_3 = str_sub(`ZONING CODE`, 1, 3)) %>%
  mutate(
    Cumulative_Share = cumsum(Share)
  ) %>%
  ungroup()%>%
  group_by(Zoning_Group_3) %>%
  summarise(
    Total_Group_Share = sum(Share)
  )


Table_Descriptives <- Zoning_Parcels_Income_Ridges %>%
  filter(!`ZONING CODE` %in% c("A-1")) %>%
  group_by(Facet, Zoning_Group_Acre, `ZONING CODE`) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Zoning_Group_Acre) %>%
  mutate(Share = Count / sum(Count) * 100) %>%
  arrange(Zoning_Group_Acre, desc(Count))

#Share of land covered by Res Zoning
  Henrico_Zoning <- read_sf(paste0(onedrivepath, "Zoning data/Richmond MSA/Henrico/Zoning.shp")) %>%
    filter(!st_is_empty(geometry)) %>%
    st_make_valid() %>%
    st_cast("MULTIPOLYGON") %>%
    mutate(County = "Henrico",
           Year = 2022) %>%
    rename(Code = ZONE_NAME) %>%
    mutate(Code = str_remove(Code, "C$")) %>% #Remove conditional category
    #select(Code, County, Year, geometry) %>%
    mutate(Code = case_when( #One parcel in the process of rezoning from A to R-3
      ZONE_LABEL == "REZ2019-00027" ~ "R-3",  # Update this line as needed
      Code == "x" ~ "A-1",  #One zoning code set to x
      TRUE ~ Code
    ))
    # st_transform(st_crs(Henrico_Tax_Parcels_shp))


 #Load zoning description
RVA_Zoning_Descriptions <- read_excel("~/Library/CloudStorage/OneDrive-UniversityOfOregon/Dissertation Research/RQ3/RVA-Zoning-Descriptions.xlsx")

#Join descriptions with code
Henrico_Zoning <- Henrico_Zoning %>%
    left_join(RVA_Zoning_Descriptions, by= c("County", "Code")) %>%
    select(OBJECTID, County, Year, Code, Name, Nature, Housing_Description, Maximum_Density_Allowed, Zoning_Atlas_Definition, Source, geometry) %>%
  mutate(
    Zoning_Group_Acre = case_when(
      Code %in% c("A-1", "AR-1", "AR-2", "AR-6") ~ "Agricultural",
      Code %in% c("R-0", "RC", "RR-1", "R-1", "R-1A") ~ "Large lot zoned parcels",
      Code %in% c("R-2", "R-2C", "R-2A", "R-2AC", "R-3AC", "R-3C", "R-3", "R-3A") ~ "Medium lot zoned parcels",
      Code %in% c("R-5", "R-5A", "R-5AC", "R-5C", "R-6", "R-6C",
                  "RTH", "RTHC", "RMP", "R-4", "R-4A", "R-4AC", "RM", "RS") ~ "Small lot zoned parcels",
      TRUE ~ "Nonresidential"
    )
  )

Henrico_Zoning <- Henrico_Zoning %>%
  st_make_valid() %>%
  mutate(Area = as.numeric(st_area(geometry))) %>%
  filter(!Zoning_Group_Acre %in% c("Nonresidential")) 
  

Total_Area <- sum(Henrico_Zoning$Area)

Table_Descriptives <- Henrico_Zoning %>%
  filter(!Zoning_Group_Acre %in% c("Nonresidential")) %>%
  group_by(Zoning_Group_Acre) %>%
  summarise(
    Residential_Area = sum(Area)
  ) %>%
  mutate(
    Share = Residential_Area / Total_Area * 100  )

#-------------------------------------------------------------------------------
#Area of Study Map

# #Prep Zoning data with geometry
#   Henrico_Zoning <- read_sf(paste0(onedrivepath, "Zoning data/Richmond MSA/Henrico/Zoning.shp")) %>%
#     filter(!st_is_empty(geometry)) %>%
#     st_make_valid() %>%
#     st_cast("MULTIPOLYGON") %>%
#     mutate(County = "Henrico",
#            Year = 2022) %>%
#     rename(Code = ZONE_NAME) %>%
#     mutate(Code = str_remove(Code, "C$")) %>% #Remove conditional category
#     #select(Code, County, Year, geometry) %>%
#     mutate(Code = case_when( #One parcel in the process of rezoning from A to R-3
#       ZONE_LABEL == "REZ2019-00027" ~ "R-3",  # Update this line as needed
#       Code == "x" ~ "A-1",  #One zoning code set to x
#       TRUE ~ Code
#     )) 
#     # st_transform(st_crs(Henrico_Tax_Parcels_shp)) 
#  
#   
#  #Load zoning description
# RVA_Zoning_Descriptions <- read_excel("~/Library/CloudStorage/OneDrive-UniversityOfOregon/Dissertation Research/RQ3/RVA-Zoning-Descriptions.xlsx")
#   
# #Join descriptions with code
# Henrico_Zoning <- Henrico_Zoning %>%
#     left_join(RVA_Zoning_Descriptions, by= c("County", "Code")) %>%
#     select(OBJECTID, County, Year, Code, Name, Nature, Housing_Description, Maximum_Density_Allowed, Zoning_Atlas_Definition, Source, geometry) %>%
#   mutate(
#     Zoning_Group_Acre = case_when(
#       Code %in% c("A-1", "AR-1", "AR-2", "AR-6") ~ "Agricultural",
#       Code %in% c("R-0", "RC", "RR-1", "R-1", "R-1A") ~ "Large lot zoned parcels",
#       Code %in% c("R-2", "R-2C", "R-2A", "R-2AC", "R-3AC", "R-3C", "R-3", "R-3A") ~ "Medium lot zoned parcels",
#       Code %in% c("R-5", "R-5A", "R-5AC", "R-5C", "R-6", "R-6C",
#                   "RTH", "RTHC", "RMP", "R-4", "R-4A", "R-4AC", "RM", "RS") ~ "Small lot zoned parcels",
#       TRUE ~ "Nonresidential"
#     )
#   ) %>%
#   mutate(
#     Mapping_Variable = case_when(
#       Zoning_Atlas_Definition == "Mixed with Residential" ~ Nature,
#       Zoning_Atlas_Definition == "Nonresidential" ~ Nature,
#       Zoning_Atlas_Definition == "Primarily Residential" ~ Zoning_Group_Acre,
#       TRUE ~ NA_character_)) %>%
#   group_by(OBJECTID, County, Year, Code, Name, Mapping_Variable, Nature, Housing_Description, Maximum_Density_Allowed, Zoning_Atlas_Definition, Source, Zoning_Group_Acre) %>%
#   summarise(geometry = st_union(geometry), .groups = "drop")  %>%
#     # mutate(geometry = st_make_valid(geometry)) %>%  # <- fix geometries
#     # mutate(Mapping_Variable = case_when(
#     #   Residential_Code %in% c("Mixed with Residential", "PMH", "PUD") ~ "Mixed with Residential",
#     #   is.na(Residential_Code) | Residential_Code == "Nonresidential" ~ "Nonresidential",
#       # TRUE ~ Residential_Code
#     # ),
#     # Mapping_Variable = case_when(
#     #   Mapping_Variable == "RRC" ~ "RC",
#     #   Mapping_Variable == "RR-1" ~ "R-1",
#     #   TRUE ~ Mapping_Variable
#     # )) %>%
#     mutate(Mapping_Variable = case_when(
#       Mapping_Variable == "A-1" ~ "Agricultural",
#       TRUE ~ Mapping_Variable
#     )) %>%
#     group_by(Mapping_Variable) %>%
#     summarise(geometry = st_union(geometry), .groups = "drop") 
#   

#Check tmap
tmap_options(check.and.fix = TRUE)

#Load shapefile of parcels
Henrico_Tax_Parcels_shp <- read_sf(paste0(onedrivepath, "Mapping Richmond/Parcel-Buildings/Henrico/Tax_Parcel_shp/Tax_Parcels_and_CAMA_Data_External.shp")) %>%
  rename(PIN = `CAMA_GPIN`) %>%
  select()

#Load point of zoning codes
Henrico_Parcel_Zoning_Codes <- read_rds(paste0(onedrivepath, "Mapping Richmond/Parcel-Buildings/Henrico/Henrico_Buildings_small.rds")) %>%
  filter(!is.na(`PARCEL LEVEL LATITUDE`)) %>%
  st_as_sf(coords = c("PARCEL LEVEL LONGITUDE", "PARCEL LEVEL LATITUDE"), crs = 4326) 
  
#Matches CRSs
Henrico_Parcel_Zoning_Codes <- st_transform(Henrico_Parcel_Zoning_Codes, 
                                            st_crs(Henrico_Tax_Parcels_shp))

#Join and create mapping value
Henrico_Parcels_Mapping <- st_join(Henrico_Tax_Parcels_shp, Henrico_Parcel_Zoning_Codes, 
                join = st_contains) %>%
  group_by(PIN) %>%
  slice(1) %>%
  ungroup()%>%
  mutate(
    Mapping_Variable = case_when(
      `ZONING CODE` %in% c("A1", "AR1", "AR2", "AR6") ~ "Agricultural",
      `ZONING CODE` %in% c("R0", "RC", "RR1") ~ "Very Large lot zoned parcels",
      `ZONING CODE` %in% c("RR1", "R1", "R1A") ~ "Large lot zoned parcels",
      `ZONING CODE` %in% c("R2", "R2C", "R2A", "R2AC", "R3AC", "R3C", "R3", "R3A") ~ "Medium lot zoned parcels",
      `ZONING CODE` %in% c("R5", "R5A", "R5AC", "R5C", "R6", "R6C",
                  "RTH", "RTHC", "RMP", "R4", "R4A", "R4AC", "RM", "RS") ~ "Small lot zoned parcels",
      TRUE ~ "Nonresidential"))

#Calculate area
Table_Descriptives <- Henrico_Parcels_Mapping %>%
  filter(!Mapping_Variable %in% c("Nonresidential", "Agricultural")) %>%
  st_make_valid() %>%
  mutate(Area = as.numeric(st_area(geometry))) %>%
  group_by(Mapping_Variable) %>%
  summarise(
    Residential_Area = sum(Area, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Share = Residential_Area / sum(Residential_Area) * 100
  )

# Download Henrico County boundary
Henrico_Boundary <- counties(state = "VA", cb = TRUE, resolution = "500k") %>%
  filter(NAME == "Henrico")

# Convert to lines
Henrico_Boundary <- st_cast(Henrico_Boundary, "MULTILINESTRING")

#Assign colors
Henrico_color_values <- c(`Agricultural` = "#d9d9d9", 
                          `Nonresidential` = "#969696", 
                          # `Commercial` = "darkgrey", 
                          # `Industrial` = "darkgrey", 
                          # `Residential and commercial` = "darkgrey", 
                          # `Commercial (with restricted residential)` = "darkgrey", 
                          `Small lot zoned parcels` = "#d9f0a3", 
                          `Medium lot zoned parcels` = "#41ab5d",
                          `Large lot zoned parcels` = "#005a32",
                          `Very large lot zoned parcels` = "black")

tmap_mode(mode = "plot")

#Map
Henrico_Lot_Sizes <- tm_shape(Henrico_Parcels_Mapping) +
  tm_fill("Mapping_Variable",
          style = "fixed", 
          palette = Henrico_color_values,
          colorNA = "white",  
          midpoint = NA,
          # breaks = c(-Inf, -2, -1, 0, 1, 2, Inf),
          alpha = 1,
          legend.show = F) +
  tm_add_legend(type = "fill",
                labels = c("Small lot",
                           "Medium lot",
                           "Large lot",
                           "Agricultural",
                           "Nonresidential"),
                col = c("#d9f0a3",
                        "#41ab5d",
                        "#005a32",
                        "#d9d9d9",
                        "#737373")) +
  #tm_facets(by=c("Year"), ncol  = 2) +
  tm_layout(main.title.size = 1.4,
            main.title.position = "center",
            main.title.fontface = "bold",
            frame = F,
            # legend.position = c(0.05, 0.85), 
            legend.title.size = 0.65,
            legend.title.fontface = "bold",
            legend.text.size = 1,
            legend.outside = F,
            legend.show = T,
            panel.show = F,
            panel.label.bg.color = "transparent",
            panel.label.color = "black",
            panel.labels = c(""),
            panel.label.fontface = "bold",
            # inner.margins = c(0.0, -0.0, 0.0, -0.2)
  ) +
  tm_shape(Henrico_Boundary) +
  tm_lines(col="black", lwd = 0.7, scale=2, legend.lwd.show = FALSE)  

#To save
tmap_save(
  tm = Henrico_Lot_Sizes,
  filename = "~/desktop/HanHen_Zoning.png",
  height = 7,
  width = 7,
  dpi = 500
)

#Isolate the CA and Non-CA too
tm_shape(Income_LISA_Henrico) +
  tm_polygons(
    col = "Facet",     
    # palette = c("red", "grey70", "green"),  
    alpha = 0.7,
    border.col = "black") 

#Filter CA
Concentrate_Affluence <- Income_LISA_Henrico %>% 
  filter(Facet == "Concentrated affluence")

#Match CRS
Concentrate_Affluence <- st_transform(Concentrate_Affluence, st_crs(Henrico_Parcels_Mapping))

# Clip parcels to that zone
Concentrate_Affluence <- st_intersection(Henrico_Parcels_Mapping, Concentrate_Affluence)%>%
  filter(!Mapping_Variable == "Agricultural") %>%
  filter(!Mapping_Variable == "Nonresidential")

#Create boundary
Concentrate_Affluence_Boundary <- Concentrate_Affluence %>%
  summarise(geometry = st_union(geometry))

#Calculate area
Table_Descriptives <- Concentrate_Affluence %>%
  filter(!Mapping_Variable %in% c("Nonresidential", "Agricultural")) %>%
  st_make_valid() %>%
  mutate(Area = as.numeric(st_area(geometry))) %>%
  group_by(Mapping_Variable) %>%
  summarise(
    Residential_Area = sum(Area, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Share = Residential_Area / sum(Residential_Area) * 100
  )

# Convert to lines
Concentrate_Affluence_Boundary <- st_cast(Concentrate_Affluence_Boundary, "MULTILINESTRING")

#Remap, highlighting only CA
Henrico_Lot_Sizes_CA <- tm_shape(Henrico_Parcels_Mapping) +
  tm_fill("Mapping_Variable",
          style = "fixed", 
          palette = Henrico_color_values,
          colorNA = "white",  
          midpoint = NA,
          # breaks = c(-Inf, -2, -1, 0, 1, 2, Inf),
          alpha = 0.15,
          legend.show = F) +
  tm_shape(Concentrate_Affluence) +
  tm_fill("Mapping_Variable",
          style = "fixed", 
          palette = Henrico_color_values,
          colorNA = "white",  
          midpoint = NA,
          # breaks = c(-Inf, -2, -1, 0, 1, 2, Inf),
          alpha = 1,
          legend.show = F) +
  # 
  # tm_add_legend(type = "fill",
  #               labels = c("Small lot",
  #                          "Medium lot",
  #                          "Large lot",
  #                          "Agricultural",
  #                          "Nonresidential"),
  #               col = c("#d9f0a3",
  #                       "#41ab5d",
  #                       "#005a32",
  #                       "#d9d9d9",
  #                       "#737373")) +
  #tm_facets(by=c("Year"), ncol  = 2) +
  tm_layout(main.title.size = 1.4,
            main.title.position = "center",
            main.title.fontface = "bold",
            frame = F,
            # legend.position = c(0.05, 0.85), 
            legend.title.size = 0.65,
            legend.title.fontface = "bold",
            legend.text.size = 1,
            legend.outside = F,
            legend.show = T,
            panel.show = F,
            panel.label.bg.color = "transparent",
            panel.label.color = "black",
            panel.labels = c(""),
            panel.label.fontface = "bold",
            # inner.margins = c(0.0, -0.0, 0.0, -0.2)
  ) 

#To save
tmap_save(
  tm = Henrico_Lot_Sizes_CA,
  filename = "~/desktop/Henrico_Lot_Sizes_CA.png",
  height = 7,
  width = 7,
  dpi = 500
)

#Non Non_CA
#Filter Non_CA
Non_Concentrate_Affluence <- Income_LISA_Henrico %>% 
  filter(Facet == "Non-concentrated affluence")

#Match CRS
Non_Concentrate_Affluence <- st_transform(Non_Concentrate_Affluence, st_crs(Henrico_Parcels_Mapping))

# Clip parcels to that zone
Non_Concentrate_Affluence <- st_intersection(Henrico_Parcels_Mapping, Non_Concentrate_Affluence) %>%
  filter(!Mapping_Variable == "Agricultural") %>%
  filter(!Mapping_Variable == "Nonresidential")

#Calculate area
Table_Descriptives <- Non_Concentrate_Affluence %>%
  filter(!Mapping_Variable %in% c("Nonresidential", "Agricultural")) %>%
  st_make_valid() %>%
  mutate(Area = as.numeric(st_area(geometry))) %>%
  group_by(Mapping_Variable) %>%
  summarise(
    Residential_Area = sum(Area, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Share = Residential_Area / sum(Residential_Area) * 100
  )

#Remap, highlighting only CA
Henrico_Lot_Sizes_Non_CA <- tm_shape(Henrico_Parcels_Mapping) +
  tm_fill("Mapping_Variable",
          style = "fixed", 
          palette = Henrico_color_values,
          colorNA = "white",  
          midpoint = NA,
          # breaks = c(-Inf, -2, -1, 0, 1, 2, Inf),
          alpha = 0.15,
          legend.show = F) +
  tm_shape(Non_Concentrate_Affluence) +
  tm_fill("Mapping_Variable",
          style = "fixed", 
          palette = Henrico_color_values,
          colorNA = "white",  
          midpoint = NA,
          # breaks = c(-Inf, -2, -1, 0, 1, 2, Inf),
          alpha = 1,
          legend.show = F) +
  # 
  # tm_add_legend(type = "fill",
  #               labels = c("Small lot",
  #                          "Medium lot",
  #                          "Large lot",
  #                          "Agricultural",
  #                          "Nonresidential"),
  #               col = c("#d9f0a3",
  #                       "#41ab5d",
  #                       "#005a32",
  #                       "#d9d9d9",
  #                       "#737373")) +
  #tm_facets(by=c("Year"), ncol  = 2) +
  tm_layout(main.title.size = 1.4,
            main.title.position = "center",
            main.title.fontface = "bold",
            frame = F,
            # legend.position = c(0.05, 0.85), 
            legend.title.size = 0.65,
            legend.title.fontface = "bold",
            legend.text.size = 1,
            legend.outside = F,
            legend.show = T,
            panel.show = F,
            panel.label.bg.color = "transparent",
            panel.label.color = "black",
            panel.labels = c(""),
            panel.label.fontface = "bold",
            # inner.margins = c(0.0, -0.0, 0.0, -0.2)
            ) 

#To save
tmap_save(
  tm = Henrico_Lot_Sizes_Non_CA,
  filename = "~/desktop/HanHen_Zoning_Non_CA.png",
  height = 7,
  width = 7,
  dpi = 500
)

#------------------------------------------------------------------------------
#Floor Area work
#Calculate FAR
Zoning_Parcels_Income <- Zoning_Parcels_Income %>%
  mutate(
    ACRES = as.numeric(ACRES),
    `TOTAL SQUARE FOOTAGE ALL BUILDINGS` = as.numeric(`TOTAL SQUARE FOOTAGE ALL BUILDINGS`),
    FAR = `TOTAL SQUARE FOOTAGE ALL BUILDINGS` / (ACRES * 43560)
  ) %>%
  filter(
    ACRES > 0,
    ACRES <= 14000,
    is.finite(FAR),
    FAR < 5
  ) %>%
  filter(!Zoning_Group_Acre == "Agricultural")

#Box plot with points
Zoning_Parcels_Income %>%
  filter(`Zoning_Group_Acre` %in% c("Large lot zoned parcels")) %>%
  ggplot(aes(x = Facet, y = FAR, fill = Facet)) +
  geom_sina(aes(fill = Facet, col = Facet), alpha = 0.4, size = 1, shape = 21) +
  geom_boxplot(fill = NA, color = "black") +
  facet_grid(fct_rev(Zoning_Group_Acre) ~ .,
             space = "free",
             switch = "y", scales = "free_y") +
  theme_minimal(base_size = 14) +
  scale_fill_manual(values = c("Concentrated affluence" = "#7f3b08",
                               "Non-concentrated affluence" = "grey"),
                    name = NULL, guide = "none") +
  scale_color_manual(values = c("Concentrated affluence" = "#7f3b08",
                               "Non-concentrated affluence" = "grey"),
                    name = NULL, guide = "none") +
  # coord_cartesian(ylim = c(0, 1.96)) + #Small lot
  # coord_cartesian(ylim = c(0, 1)) + #Medium
  coord_cartesian(ylim = c(0, 0.45)) + #Large
  labs(
    subtitle = "Floor area ratio across areas of<br><span style='color:#7f3b08;'>concentrated affluence</span> and those <span style='color:darkgrey;'>not concentrated affluence</span>",
    y = NULL,
    x = NULL,
    caption = "FAR measures building square footage relative to lot size (density of development)."
  ) +
  theme(
    plot.subtitle = element_markdown(hjust = 0.5, size = 13, face = "bold"),
    plot.caption = element_markdown(size = 9),
    strip.placement = "outside",
    strip.text.y = element_markdown(size = 12, face = "bold"),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 11, hjust = 0.75, vjust = 0.825),
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    legend.position = "right",
    axis.title.x = element_blank(),
    axis.title.y = element_markdown(size = 14),
    panel.grid.major.x = element_line(size = 0.2, color = "darkgrey"),
    panel.grid.minor.x = element_line(size = 0.2, color = "lightgrey"),
    panel.grid.major.y = element_line(size = 0.2, color = "grey"),
    panel.grid.minor.y = element_line(size = 0.1),
    panel.border = element_rect(color = "black", fill = NA, size = 0.75))

#To save
ggsave("FAR_L.png",
       path = "~/desktop",
       width = 7,
       height = 4.5,
       units = "in",
       dpi = 500)

