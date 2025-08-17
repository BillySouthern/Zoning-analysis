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



  #For income seg work
library(haven)
library(RStata)
library(spdep) #For spatial analyses

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
income_variables <- load_variables(2020, "acs5")

#Download block level data
#2020 Median HH income 
Henrico_Income_2020 <- get_acs(
  geography = "block group", 
  variables = "B19013_001", 
  state = ST,
  year = YR4,
  output = "wide",
  geometry = T) %>%
  filter(str_detect(GEOID, "51087")) %>%
  rename("Income_2020" = B19013_001E,
         "IncomeMOE_2020" = B19013_001M) %>%
  select(-NAME)

#Exploratory map of Henrico LISA
tmap_mode("view")  

tm_shape(Henrico_Income_2020) +
  tm_polygons(
    col = "Income_2020",     
    # palette = c("red", "grey70", "green"),  
    alpha = 0.7,
    border.col = "black"
  ) +
  tm_basemap("OpenStreetMap")  

