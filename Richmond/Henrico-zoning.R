#5/22/24, initiated by BS
#Goal: To assess Henrico's R zoning codes

#Analysis includes
#Tidying data, matching inc seg with zoning codes

#Libraries
require(tidyverse)
library(sf)
library(tidycensus)
library(tmap)
library(tmaptools)
library(readxl)
library(scales)
library(ggtext)

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
onedrivepath="~/OneDrive - The Pennsylvania State University/"

#--------------------------------------------------------------------------------
#Income figure and seg measure by census tract in Henrico

#Download median hh income data by tract
#2020
#2020 Median HH income 
Henrico_Income_2020 <- get_acs(
  geography = GEOG, 
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

#------------------------------------------------------------------------------
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
  mutate(
    Lot_Size = case_when(
      ACRES > 0   & ACRES <= 0.25  ~ "0 - 0.25 acres",
      ACRES > 0.25 & ACRES <= 0.75     ~ "0.25 - 0.5 acres",
      ACRES > 0.75                    ~ "Larger than 0.75 acres",
      TRUE                         ~ NA_character_)) %>%  
  mutate(
    Zoning_Group_Acre = case_when(
          ACRES > 0   & ACRES <= 0.2  ~ "Small lot zoned parcels",
          ACRES > 0.2 & ACRES <= 0.5     ~ "Medium lot zoned parcels",
          ACRES > 0.5                    ~ "Large lot zoned parcels",
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

#Plot line chart by conc and non conc
Henrico_Med_Test <- Henrico_LISA_Zoning_BG %>%
  mutate(`SALE YEAR` = as.numeric(substr(`SALE DATE`, 1, 4))) %>%
  filter(`SALE AMOUNT` <= 2000000) %>%   
  filter(!`ZONING CODE` %in% c("RMH", "RMP")) %>%
  arrange(ACRES) %>%  # optional: explicitly sort first
  mutate(Acres_Tertile = ntile(ACRES, 3)) %>%
  filter(!is.na(Lot_Size)) %>%
  filter(!str_detect(PIN, "803-680-7933|763-730-8976|748-732-4135|754-731-8217")) %>%
  # filter(`SALE YEAR` >= 1925) %>%   
  group_by(`SALE YEAR`, 
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
        "Concentrated affluence" = "Concentrated affluence")))

#Adjust hosue value figures 
Henrico_Med_Test <- Henrico_Med_Test %>% 
  mutate(Median_Unit_Value_Inf = adjust_for_inflation(`Median_Unit_Value`, 
                                                      `SALE YEAR`, "US", to_date = 2022)) %>%
  filter(`Median_Unit_Value_Inf` <= 1500000)   

#Median sale value by year (and code) adjusted to 2020 dollars
ggplot(Henrico_Med_Test 
       # %>%
       #   filter((`ZONING CODE` %in% c("A-1", "R-0", "R-1", "R-2", "R-3", "R-4", "R-5", "R-6", "RTH")))
       , 
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