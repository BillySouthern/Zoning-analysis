#5/22/24, initiated by BS
#Goal: To explore the zoning data

#Analysis includes
#timeline of zoning data

#Libraries
require(tidyverse)
require(tidyverse)
require(sf)
library(tigris)
library(readxl)

library(leaflet)
library(htmlwidgets)
library(RColorBrewer)


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
#Load timeline data

# Hanover_Timeline <- read_excel("/Users/billy/Library/CloudStorage/OneDrive-ThePennsylvaniaStateUniversity/RQ3/Timeline-work.xlsx", 
#                  sheet = 2) 
# 
# 
# ggplot(Hanover_Timeline, aes(x = Date, y = 0, col = Type, label = Event)) + 
#   labs(col="Type") +
#   scale_color_manual(values = Date, labels = Date, drop = FALSE) +
#   theme_classic() + 
#   geom_hline(yintercept=0, 
#              color = "black", size=0.3) +
#   geom_segment(data=df[df$month_count == 1,], aes(y=position,yend=0,xend=date), color='black', size=0.2)

#-----------------------------------------------
#Load full dataset
Richmond_Zoning <- read_sf(paste0(onedrivepath, "Zoning data/Richmond MSA/Richmond_Complete/Richmond_Zoning.shp")) 



#For initial visualizing of Richmond
ggplot() + 
  # geom_sf(data = CentralCities_2020, fill = NA, color = "black", linewidth = 0.65) +
  # geom_sf(data = CBSA_2020, color = "black", fill = "white", linewidth = 0.6) +
  geom_sf(data = Richmond_Zoning[Richmond_Zoning$County == "Henrico", ], aes(fill = ZA_Def), col = "white", linewidth = 0) +
  theme_void() +
  theme(legend.spacing.y = unit(.1, "lines")) +
  geom_sf(data = Counties_2020, fill = NA, color = "black", linewidth = 0.6) +
  geom_sf(data = Transitional_2020[Transitional_2020$Landscape_Five == "Unstable", ], fill = NA, col = "black", linewidth = 0.2) 

#Create interactive map

#Group by each column
Hou_Den <- Richmond_Zoning %>%
  group_by(County, ZA_Def) %>%
  summarise(geometry = st_union(geometry)) %>%  # Dissolve by union
  ungroup() 

Max_Hou_Den <- Richmond_Zoning %>%
  group_by(County, Max_Den) %>%
  summarise(geometry = st_union(geometry)) %>%  # Dissolve by union
  ungroup() %>%
  mutate(Max_Den = case_when(
    Max_Den == "Single-family detached, duplex" ~ "Duplex",
    Max_Den == "Three two-family attached dwellings" ~ "Two-family attached dwellings",
    Max_Den == "Townhouses" ~ "Townhouse",
    Max_Den == "None" ~ "No housing allowed",
    T ~ Max_Den)) %>%
  mutate(Max_Den = str_remove(Max_Den, "dwellings")) %>%
  mutate(Max_Den = str_trim(Max_Den))

Nature_Zoning <- Richmond_Zoning %>%
  # mutate(Nature = case_when(
  #Commercial
  # Nature == "Commercial (with restricted industrial)" ~ "Commercial (with restricted industrial/residential)",
  # Nature == "Commercial (with restricted residential and industrial)" ~ "Mixed use",
  # Nature == "Commercial (with restricted residential)" ~ "Commercial (with restricted industrial/residential)",
  # Nature == "Commercial and industrial" ~ "Mixed use",
  # Nature == "Commercial and industrial (with restricted residential)" ~ "Mixed use",
  # #Industrial
  # Nature == "Industrial (and restricted commercial)" ~ "Industrial (with restricted commercial/residential)",
  # Nature == "Industrial (and restricted residential and commercial)" ~ "Industrial (with restricted commercial/residential)",
  # Nature == "Industrial (with limited commercial)" ~ "Industrial (with restricted commercial/residential)",
  # #Residential
  # Nature == "Residential (with limited commercial)" ~ "Residential (with restricted commercial/industrial)",
  # Nature == "Residential and commercial" ~ "Mixed use",
  # Nature == "Residential and industrial" ~ "Mixed use",
  # Nature == "Residential, commercial, and industrial" ~ "Mixed use",
  # #No zoning
  # Nature == "No zoning" ~ "No zoning found",
  # T ~ Nature)) %>%
  mutate(Nature = case_when(
  #Commercial
  Nature == "Commercial (with restricted industrial)" ~ "Commercial",
  Nature == "Commercial (with restricted residential and industrial)" ~ "Mixed use",
  Nature == "Commercial (with restricted residential)" ~ "Commercial",
  Nature == "Commercial (with restricted industrial/residential)" ~ "Commercial",
  Nature == "Commercial (with restricted residential)" ~ "Commercial",
  Nature == "Commercial and industrial" ~ "Mixed use",
  Nature == "Commercial and industrial (with restricted residential)" ~ "Mixed use",
  #Industrial
  Nature == "Industrial (and restricted commercial)" ~ "Industrial",
  Nature == "Industrial (and restricted residential and commercial)" ~ "Industrial",
  Nature == "Industrial (with limited commercial)" ~ "Industrial",
  Nature == "Industrial (with restricted commercial/residential) " ~ "Industrial",
  #Residential
  Nature == "Residential (with limited commercial)" ~ "Residential",
  Nature == "Residential (with restricted commercial/industrial)" ~ "Residential",
  Nature == "Residential and commercial" ~ "Mixed use",
  Nature == "Residential and industrial" ~ "Mixed use",
  Nature == "Residential, commercial, and industrial" ~ "Mixed use",
  #No zoning
  Nature == "No zoning" ~ "No zoning found",
  T ~ Nature)) %>%
  mutate(Nature = str_trim(Nature)) %>%
  group_by(County, Nature) %>%
  summarise(geometry = st_union(geometry)) %>%  # Dissolve by union
  ungroup() 
  

#Convert objects above to appropriate crs
Richmond_Zoning <- st_transform(Richmond_Zoning, 4326)
Zoning_Atlas <- st_transform(Zoning_Atlas, 4326)
Nature_Zoning <- st_transform(Nature_Zoning, 4326)
Max_Hou_Den <- st_transform(Max_Hou_Den, 4326)
Counties <- st_transform(Counties_2020, 4326)

#Assign colors


# Create the leaflet map 
leaflet() %>%
  addMapPane(name = "polygons", zIndex = 410) %>% 
  addMapPane(name = "maplabels", zIndex = 420) %>% # higher zIndex rendered on top
  addProviderTiles("CartoDB.PositronNoLabels") %>%
  addProviderTiles("CartoDB.PositronOnlyLabels", 
                   options = leafletOptions(pane = "maplabels"),
                   group = "map labels") %>%
  addMiniMap(tiles = "CartoDB.Positron") %>%
  #ZONING ATLAS DEFINITION
  # addPolygons(
  #   data = Zoning_Atlas,
  #   fillColor = ~colorFactor(
  #     palette = c("#7fc97f", "#beaed4", "#fdc086"),
  #     levels = c("Primarily Residential", "Mixed with Residential", "Nonresidential"),
  #     domain = unique(Zoning_Atlas$ZA_Def)
  #   )(ZA_Def),
  #   color = ~colorFactor(
  #     palette = c("#7fc97f", "#beaed4", "#fdc086"),
  #     levels = c("Primarily Residential", "Mixed with Residential", "Nonresidential"),
  #     domain = unique(Zoning_Atlas$ZA_Def)
  #   )(ZA_Def),
  #   fillOpacity = 0.5,
  #   weight = 0,
  #   highlightOptions = highlightOptions(
  #     color = "white",
  #     fillOpacity = 1,
  #     weight = 1
  #   ),
  #   group = "Zoning Atlas Def",
  #   label = ~Zoning_Atlas$ZA_Def
  # ) %>%
  # addLegend(
  #   position = "bottomleft",
  #   pal = colorFactor(
  #     palette = c("#7fc97f", "#beaed4", "#fdc086"),
  #     levels = c("Primarily Residential", "Mixed with Residential", "Nonresidential"),
  #     domain = unique(Zoning_Atlas$ZA_Def)
  #   ),
  #   values = Zoning_Atlas$ZA_Def,
  #   title = "Zoning Atlas",
  #   group = "Zoning Atlas"
  # ) %>%
  # 
  # #MAXIMUM DENSITY
  # addPolygons(
  #     data = Max_Hou_Den,
  #     fillColor = ~colorFactor(
  #       palette = c("#a6cee3", "darkblue", "#33a02c", "#b2df8a", 
  #                   "grey", "#ff7f00", "#fdbf6f", "#e31a1c", "#fb9a99"),
  #       levels = c("Apartments", "Multifamily", "Townhouse", "Duplex",
  #                  "None", "Single-family attached", "Single-family detached", "Two-family attached",
  #                  "Two-family detached"),
  #       domain = unique(Max_Hou_Den$Max_Den)
  #     )(Max_Den),
  #     color = ~colorFactor(
  #       palette = c("#a6cee3", "darkblue", "#33a02c", "#b2df8a", 
  #                   "grey", "#ff7f00", "#fdbf6f", "#e31a1c", "#fb9a99"),
  #       levels = c("Apartments", "Multifamily", "Townhouse", "Duplex",
  #                  "None", "Single-family attached", "Single-family detached", "Two-family attached",
  #                  "Two-family detached"),
  #       domain = unique(Max_Hou_Den$Max_Den)
  #     )(Max_Den),
  #   fillOpacity = 0.5,
  #   weight = 1,       # Border width
  #   highlightOptions = highlightOptions(
  #     color = "white",
  #     fillOpacity = 1,
  #     weight = 2
  #   ),
  #   group = "Maximum housing density allowed",
  #   label = ~Max_Hou_Den$Max_Den
  # ) %>%
  # addLegend(
  #   position = "bottomleft",
  #   pal = colorFactor(
  #     palette = c("#a6cee3", "darkblue", "#33a02c", "#b2df8a", 
  #                 "white", "#ff7f00", "#fdbf6f", "#e31a1c", "#fb9a99"),
  #     levels = c("Apartments", "Multifamily", "Townhouse", "Duplex",
  #                "None", "Single-family attached", "Single-family detached", "Two-family attached",
  #                "Two-family detached"),
  #     domain = unique(Max_Hou_Den$Max_Den)
  #   ),
  #   values = Max_Hou_Den$Max_Den,
  #   title = "Housing Density",
  #   group = "Housing Density"
  # ) %>%

  #Nature
  addPolygons(
    data = Nature_Zoning,
        fillColor = ~colorFactor(
          palette = c("blue", "orange",
                      "purple", "lightgrey", "darkgreen"),
          levels = c("Commercial", "Industrial",  "Mixed use", "No zoning found",
                     "Residential"),
          domain = unique(Nature_Zoning$Nature)
        )(Nature),
    color = ~colorFactor(
      palette = c("blue", "orange",
                  "purple", "lightgrey", "darkgreen"),
      levels = c("Commercial", "Industrial",  "Mixed use", "No zoning found",
                 "Residential"),
      # palette = c("navy", "lightblue", "orange", "yellow",
      #             "purple", "lightgrey", "lightgreen", "darkgreen"),
      # levels = c("Commercial", "Commercial (with restricted industrial/residential)", 
      #            "Industrial", "Industrial (with restricted commercial/residential)",
      #            "Mixed use", "No zoning found",
      #            "Residential", "Residential (with restricted commercial/industrial)"),
      domain = unique(Nature_Zoning$Nature)
    )(Nature),
    fillOpacity = 0.5,
    weight = 0,
    highlightOptions = highlightOptions(
      color = "white",
      fillOpacity = 1,
      weight = 2
    ),
    group = "Land use",
    label = ~Nature_Zoning$Nature
  ) %>%
  addLegend(
    position = "bottomleft",
    pal = colorFactor(
      palette = c("blue", "orange",
                  "purple", "lightgrey", "darkgreen"),
      levels = c("Commercial", "Industrial",  "Mixed use", "No zoning found",
                 "Residential"),
      domain = unique(Nature_Zoning$Nature)
    ),
    values = Nature_Zoning$Nature,
    title = "Land use",
    group = "Land use"
  ) %>%
  # Add static non-interactive boundary lines
  addPolygons(
    data = Counties_2020,
    color = "black",    # Line color
    fillOpacity = 0,    # No fill
    weight = 1.5,       # Line width
    group = "Geographic Boundaries",
    
    # Disable interactivity by removing label, highlight, and popup
    label = NULL,
    highlightOptions = NULL,  # No highlight on hover
    popup = NULL,             # No popup
    options = pathOptions(clickable = FALSE)  # Disable clicking
  ) 
  

Add secondary text in highlight
Add legend with toggle on/off
Check housing density allowed

%>%
  #Building Density 
  addPolygons(
    data = BuildingDensity,
    fillColor = ~colorFactor(
      palette = c("#e78ac3", "#8da0cb", "black"),
      levels = c("Inner Suburb", "Outer Suburb", "Urban"),
      domain = unique(BuildingAge$Landscape)
    )(Landscape),
    color = "white",
    fillOpacity = 0.65,
    weight = 1,
    highlightOptions = highlightOptions(
      color = "black",
      fillOpacity = 1,
      weight = 2
    ),
    group = "Building Density",
    label = ~BuildingDensity$Landscape
  ) %>%
  addLegend(
    position = "bottomleft",
    pal = colorFactor(
      palette = c("#e78ac3", "#8da0cb", "black"),
      levels = c("Inner Suburb", "Outer Suburb", "Urban"),
      domain = unique(BuildingAge$Landscape)),
    values = BuildingDensity$Landscape,
    title = "Building Density",
    group = "Building Density",
  ) %>%
  #Distance urban/suburban
  addPolygons(
    data = Distance,
    fillColor = ~colorFactor(
      palette = c("#bd0026", "#f03b20", "#fd8d3c", "#fecc5c"),
      levels = c("Urban Core", "Inner-Urban", "Inner-Suburban", "Outer-Suburban"),
      domain = unique(Distance$Landscape)
    )(Landscape),
    color = "white",
    fillOpacity = 0.65,
    weight = 1,
    highlightOptions = highlightOptions(
      color = "black",
      fillOpacity = 1,
      weight = 2
    ),
    group = "Distance",
    label = ~Distance$Landscape
  ) %>%
  addLegend(
    position = "bottomleft",
    pal = colorFactor(
      palette = c("#bd0026", "#f03b20", "#fd8d3c", "#fecc5c"),
      levels = c("Urban Core", "Inner-Urban", "Inner-Suburban", "Outer-Suburban"),
      domain = unique(Distance$Landscape)),
    values = Distance$Landscape,
    title = "Distance",
    group = "Distance",
  ) %>%
  #Homeownership 
  addPolygons(
    data = Homeownership,
    fillColor = ~colorFactor("Set2", domain = unique(Homeownership$Landscp))(Landscp),
    color = "white",
    fillOpacity = 0.65,
    weight = 1,
    highlightOptions = highlightOptions(
      color = "black",
      fillOpacity = 1,
      weight = 2
    ),
    group = "Homeownership",
    label = ~Homeownership$Landscp
  ) %>%
  addLegend(
    position = "bottomleft",
    pal = colorFactor("Set2", domain = unique(Homeownership$Landscp)),
    values = Homeownership$Landscp,
    title = "Homeownership",
    group = "Homeownership",
  ) %>%
  #Political urban/suburban
  addPolygons(
    data = Political,
    fillColor = ~colorFactor(
      palette = c("#d9d9d9", "#636363"),
      levels = c("Suburban", "Urban"),
      domain = unique(Political$Landscape)
    )(Landscape),
    color = "white",
    fillOpacity = 0.65,
    weight = 1,
    highlightOptions = highlightOptions(
      color = "black",
      fillOpacity = 1,
      weight = 2
    ),
    group = "Political",
    label = ~Political$Landscape
  ) %>%
  addLegend(
    position = "bottomright",
    pal = colorFactor(
      palette = c("#d9d9d9", "#636363"),
      levels = c("Suburban", "Urban"),
      domain = unique(Political$Landscape)),
    values = Political$Landscape,
    title = "Political",
    group = "Political",
  ) %>%
  #Population Density urban/suburban
  addPolygons(
    data = PopulationDensity,
    fillColor = ~colorFactor(
      palette = c("#b2182b", "#d6604d", "#f4a582", "#fddbc7", "#F2F2F2", "black", "#999999"),
      levels = c("High-Density Suburban", "Mid-Density Suburban", "Low-Density Suburban", 
                 "Exurban", "No Population", "High-Density Urban", "Low-Density Urban"),
      domain = unique(PopulationDensity$Landscp)
    )(Landscp),
    color = "white",
    fillOpacity = 0.65,
    weight = 1,
    highlightOptions = highlightOptions(
      color = "black",
      fillOpacity = 1,
      weight = 2
    ),
    group = "Population Density",
    label = ~PopulationDensity$Landscp
  ) %>%
  addLegend(
    position = "bottomright",
    pal = colorFactor(
      palette = c("#b2182b", "#d6604d", "#f4a582", "#fddbc7", "#F2F2F2", "black", "#999999"),
      levels = c("High-Density Suburban", "Mid-Density Suburban", "Low-Density Suburban", 
                 "Exurban", "No Population", "High-Density Urban", "Low-Density Urban"),
      domain = unique(PopulationDensity$Landscp)
    ),
    values = PopulationDensity$Landscp,
    title = "Population Density",
    group = "Population Density"
  ) %>%
  #Travel urban/suburban
  addPolygons(
    data = Travel,
    fillColor = ~colorFactor("Set1", domain = unique(Travel$Landscp))(Landscp),
    color = "white",
    fillOpacity = 0.65,
    weight = 1,
    highlightOptions = highlightOptions(
      color = "black",
      fillOpacity = 1,
      weight = 2
    ),
    group = "Travel",
    label=~Travel$Landscp
  ) %>%  
  addLegend(
    position = "bottomright",
    pal = colorFactor("Set1", domain = unique(Travel$Landscp)),
    values = Travel$Landscp,
    title = "Travel",
    group = "Travel"
  ) %>%
  # Add other layers as needed
  addLayersControl(
    baseGroups = c("CartoDB.Voyager"),
    overlayGroups = c("Geographic Boundaries", "Building Age", "Building Density", 
                      "Distance", "Homeownership", 
                      "Political", "Population Density", "Travel"),
    options = layersControlOptions(collapsed = FALSE, hideSingleBase = TRUE))


#To export as html
saveWidget(Richmond, file="Richmond_2020.html")




