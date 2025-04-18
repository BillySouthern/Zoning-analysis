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

library(leaflet) #For interactive mapping
library(htmlwidgets) #To export map
library(RColorBrewer)

library(data.table)
library(stringr)


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

#----------------------------------------------------------------------------------------------
#Load full dataset
Richmond_Zoning <- read_sf(paste0(onedrivepath, "Zoning data/Richmond MSA/Richmond_Complete/Richmond_Zoning.shp")) 

#GEOJSON
# st_write(Richmond_Zoning, paste0(onedrivepath, "Zoning data/Richmond MSA/Richmond_Complete/GEOJSON/Richmond_Zoning.geojson"))
# Richmond_Zoning <- read_sf(paste0(onedrivepath, "Zoning data/Richmond MSA/Richmond_Complete/GEOJSON/Richmond_Zoning.geojson")) 

#Download geographies of interest (in this case, the Richmond CBSA boundary
CBSA_2020 <- core_based_statistical_areas(resolution = "500k", year = YR4) %>%
  filter(str_detect(NAME, ST)) %>%
  filter(str_detect(NAME, CBSA))

#Download Counties and filter to within CBSA
Counties_2020 <- counties(ST,
                          year = YR4,
                          cb = F) %>%
  filter(lengths(st_within(., CBSA_2020)) > 0)


# #For initial visualizing of Richmond
# ggplot() + 
#   # geom_sf(data = CentralCities_2020, fill = NA, color = "black", linewidth = 0.65) +
#   # geom_sf(data = CBSA_2020, color = "black", fill = "white", linewidth = 0.6) +
#   geom_sf(data = Richmond_Zoning[Richmond_Zoning$County == "Henrico", ], aes(fill = ZA_Def), col = "white", linewidth = 0) +
#   theme_void() +
#   theme(legend.spacing.y = unit(.1, "lines")) +
#   geom_sf(data = Counties_2020, fill = NA, color = "black", linewidth = 0.6) +
#   geom_sf(data = Transitional_2020[Transitional_2020$Landscape_Five == "Unstable", ], fill = NA, col = "black", linewidth = 0.2) 

#Create interactive map
#Group by each column
Zoning_Atlas <- Richmond_Zoning %>%
  group_by(County, ZA_Def) %>%
  # rename("Zoning Atlas Definition" = ZA_Def) %>%
  summarise(geometry = st_union(geometry)) %>%  # Dissolve by union
  ungroup() 

#Export
# st_write(Zoning_Atlas, "~/Desktop/Exp_Builder_Data/ZA/Zoning_Atlas_Definition.shp") 

Max_Hou_Den <- Richmond_Zoning %>%
  group_by(County, Max_Den, Hou_Den) %>%
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
  # rename("Maximum Housing Density" = Max_Den,
  #        "Housing Permitted" = Hou_Den)

#Export
# st_write(Max_Hou_Den, "~/Desktop/Exp_Builder_Data/Max_Hou/Maximum_Housing_Density.shp")

Nature_Zoning <- Richmond_Zoning %>%
  mutate(Nature = case_when(
  # Commercial
  Nature == "Commercial (with restricted industrial)" ~ "Commercial (with restricted industrial/residential)",
  Nature == "Commercial (with restricted residential and industrial)" ~ "Mixed use",
  Nature == "Commercial (with restricted residential)" ~ "Commercial (with restricted industrial/residential)",
  Nature == "Commercial and industrial" ~ "Mixed use",
  Nature == "Commercial and industrial (with restricted residential)" ~ "Mixed use",
  #Industrial
  Nature == "Industrial (and restricted commercial)" ~ "Industrial (with restricted commercial/residential)",
  Nature == "Industrial (and restricted residential and commercial)" ~ "Industrial (with restricted commercial/residential)",
  Nature == "Industrial (with limited commercial)" ~ "Industrial (with restricted commercial/residential)",
  #Residential
  Nature == "Residential (with limited commercial)" ~ "Residential (with restricted commercial/industrial)",
  Nature == "Residential and commercial" ~ "Mixed use",
  Nature == "Residential and industrial" ~ "Mixed use",
  Nature == "Residential, commercial, and industrial" ~ "Mixed use",
  #No zoning
  Nature == "No zoning" ~ "No zoning found",
  T ~ Nature)) %>%
  mutate("Nature Specific" = Nature,
         Nature = case_when(
  #Commercial
  Nature == "Commercial (with restricted industrial)" ~ "Commercial",
  Nature == "Commercial (with restricted residential and industrial)" ~ "Commercial",
  Nature == "Commercial (with restricted residential)" ~ "Commercial",
  Nature == "Commercial (with restricted industrial/residential)" ~ "Commercial",
  Nature == "Commercial (with restricted residential)" ~ "Commercial",
  Nature == "Commercial and industrial" ~ "Mixed use",
  Nature == "Commercial and industrial (with restricted residential)" ~ "Mixed use",
  #Industrial
  Nature == "Industrial (and restricted commercial)" ~ "Industrial",
  Nature == "Industrial (and restricted residential and commercial)" ~ "Industrial",
  Nature == "Industrial (with limited commercial)" ~ "Industrial",
  Nature == "Industrial (with restricted commercial/residential)" ~ "Industrial",
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
  group_by(County, Nature, `Nature Specific`) %>%
  summarise(geometry = st_union(geometry)) %>%  # Dissolve by union
  ungroup() 
 
#Export
# st_write(Nature_Zoning, "~/Desktop/Exp_Builder_Data/Nature/Zoning_Nature.shp")



#Convert objects above to appropriate crs
# Richmond_Zoning <- st_transform(Richmond_Zoning, 4326)
Zoning_Atlas <- st_transform(Zoning_Atlas, 4326)
Nature_Zoning <- st_transform(Nature_Zoning, 4326)
Max_Hou_Den <- st_transform(Max_Hou_Den, 4326)
Counties <- st_transform(Counties_2020, 4326)

#Assign colors

#add in building footprint

# Define the path to the shapefile
shapefile_path <- "/Users/billy/Desktop/Exp_Builder_Data/Richmond_Buildings/Richmond_Buildings.shp"
shapefile_path <- "/Users/billy/Desktop/Exp_Builder_Data/Richmond_Buildings/Buffered_Buildings.shp"

# Load the shapefile
VA_Buildings <- st_read(shapefile_path)  
setDT(VA_Buildings)



#Filter by counties
localities <- c("Amelia County","Charles City", "Colonial Heights City", "Chesterfield County", "Richmond City", 
                "Dinwiddie County", "Goochland County", "Hanover County", 
                "Henrico County", "Hopewell City", "King William County", "King and Queen", 
                "New Kent County", "Petersburg City", "Powhatan County", "Prince George County", "Sussex County")

# Remove " County" from LOCALITIES column using := in data.table
# VA_Buildings[, LOCALITY := sub(" County$", "", LOCALITY)]

# Filter rows based on localities
Richmond_Buildings <- VA_Buildings %>%
  filter(str_detect(LOCALITY, paste(localities, collapse = "|")))



Richmond_Buildings_transformed <- st_transform(Richmond_Buildings, st_crs(Zoning_Atlas))

Test <- Richmond_Buildings %>%
  st_join(Zoning_Atlas, left = TRUE)



Richmond_Buildings_transformed <- st_transform(Richmond_Buildings, st_crs(Zoning_Atlas))

Test <- Richmond_Buildings_transformed %>%
  st_join(Zoning_Atlas, left = TRUE)


# Filter rows based on the locality names with " County" appended
Richmond_Buildings <- VA_Buildings %>%
  filter(str_detect(LOCALITY, paste(str_c(localities, " City"), collapse = "|")))

#export
# st_write(Richmond_Buildings, "~/Desktop/Exp_Builder_Data/Richmond_Buildings/Richmond_Buildings.shp") 




VA_buildings <- st_read("/Users/billy/Desktop/Exp_Builder_Data/Richmond_Buildings/Virginia.geojson")

Test <- st_transform(VA_Buildings, st_crs(Nature_Zoning))


VA_buildings_clean <- st_make_valid(VA_buildings_transformed)
# Now perform the intersection
VA_buildings_centroids <- st_centroid(VA_buildings_transformed)

# Step 2: Clip the centroids to the Counties 2020 extent
Test <- st_intersection(Test, Counties_2020)

VA_buildings_buffered_0 <- st_buffer(VA_buildings_clean, dist = 0)


ggplot() + 
  geom_sf(data = Counties_2020)



Test <- Richmond_Buildings %>%
  st_set_crs(st_crs(Zoning_Atlas)) %>%
    st_transform(st_crs(Zoning_Atlas))
  
Test <- st_transform(Richmond_Buildings, st_crs(Nature_Zoning))

st_crs(Richmond_Buildings)
st_crs(Zoning_Atlas)

Zoning_Atlas_2 <- Zoning_Atlas  %>%
  st_transform(st_crs(Counties))

VA_Buildings <- VA_Buildings %>%
  st_set_crs(st_crs(Nature_Zoning))  # Assign CRS if missing

#Create points from buildings
# Create centroids for each polygon
VA_Buildings_Centroids <- VA_Buildings_Centroids %>%
  # st_centroid() %>%
  st_transform(st_crs(Zoning_Atlas))  # Transform to match CRS of Zoning_Atlas


intersections <- st_intersects(VA_Buildings_Centroids, Zoning_Atlas)
# Check if any centroid intersects any polygon
summary(intersections)

Test <- Richmond_Buildings_transformed %>%
  st_join(Zoning_Atlas, join = st_intersects)


Richmond_Buildings_transformed <- st_transform(Richmond_Buildings, st_crs(Zoning_Atlas))

Test <- Richmond_Buildings_transformed %>%
  st_join(Zoning_Atlas, left = TRUE)



Test <- VA_Buildings %>%
  st_set_crs(st_crs(Zoning_Atlas)) %>%   # Assign CRS if missing
  st_make_valid() %>%                    # Fix geometries
  st_transform(st_crs(Zoning_Atlas)) 

Test <- st_set_crs(VA_Building_Centroids, st_crs(Zoning_Atlas))




# Create an interactive map
leaflet(VA_Buildings) %>%
  addTiles() %>%  # Add default OpenStreetMap tiles
  addPolygons(color = "black",        # Polygon border color
              fillColor = "darkgrey", # Fill color
              fillOpacity = 0.5,       # Fill opacity
              weight = 0.5,            # Border thickness
              popup = ~paste("Building ID:", row.names(VA_Buildings))) %>%  # Popup with building ID
  addScaleBar() %>%
  addMiniMap(toggleDisplay = TRUE)  # Add a minimap for navigation

# Create the leaflet map 
RVA_Zoning <- leaflet() %>%
  addMapPane(name = "polygons", zIndex = 410) %>% 
  addMapPane(name = "maplabels", zIndex = 420) %>% # higher zIndex rendered on top
  addProviderTiles("CartoDB.PositronNoLabels") %>%
  addProviderTiles("CartoDB.PositronOnlyLabels", 
                   options = leafletOptions(pane = "maplabels"),
                   group = "map labels") %>%
  addMiniMap(tiles = "CartoDB.Positron") %>%
  
  #ZONING ATLAS DEFINITION
  addPolygons(
    data = Zoning_Atlas,
    fillColor = ~colorFactor(
      palette = c("#80b1d3", "#fb8072", "#ffffb3"),
      levels = c("Primarily Residential", "Mixed with Residential", "Nonresidential"),
      domain = unique(Zoning_Atlas$ZA_Def)
    )(ZA_Def),
    color = ~colorFactor(
      palette = c("#80b1d3", "#fb8072", "#ffffb3"),
      levels = c("Primarily Residential", "Mixed with Residential", "Nonresidential"),
      domain = unique(Zoning_Atlas$ZA_Def)
    )(ZA_Def),
    fillOpacity = 0.5,
    weight = 0,
    highlightOptions = highlightOptions(
      color = "white",
      fillOpacity = 1,
      weight = 1
    ),
    group = "Zoning Atlas Definition",
    label = ~Zoning_Atlas$ZA_Def,
    options = pathOptions(pane = "polygons")  # Added pane option
  ) %>%
  addLegend(
    position = "bottomleft",
    pal = colorFactor(
      palette = c("#80b1d3", "#fb8072", "#ffffb3"),
      levels = c("Primarily Residential", "Mixed with Residential", "Nonresidential"),
      domain = Zoning_Atlas$ZA_Def   # Adjusting domain directly
    ),
    values = Zoning_Atlas$ZA_Def,   # Removed the formula notation (~)
    title = "Zoning Atlas Definition"
  ) %>%
  # 
  # #MAXIMUM DENSITY
  # addPolygons(
  #     data = Max_Hou_Den,
  #     fillColor = ~colorFactor(
  #       palette = c("#a6cee3", "darkblue", "#33a02c", "#b2df8a",
  #                   "grey", "#ff7f00", "#fdbf6f", "#e31a1c", "#fb9a99"),
  #       levels = c("Apartments", "Multifamily", "Townhouse", "Duplex",
  #                  "No housing allowed", "Single-family attached", "Single-family detached", "Two-family attached",
  #                  "Two-family detached"),
  #       domain = unique(Max_Hou_Den$Max_Den)
  #     )(Max_Den),
  #     color = ~colorFactor(
  #       palette = c("#a6cee3", "darkblue", "#33a02c", "#b2df8a",
  #                   "grey", "#ff7f00", "#fdbf6f", "#e31a1c", "#fb9a99"),
  #       levels = c("Apartments", "Multifamily", "Townhouse", "Duplex",
  #                  "No housing allowed", "Single-family attached", "Single-family detached", "Two-family attached",
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
  #   group = "Maximum Housing Density",
  #   label = ~Max_Hou_Den$Max_Den,
  #   options = pathOptions(pane = "polygons")  # Added pane option
  # ) %>%
  #   addLegend(
  #   position = "bottomleft",
  #   pal = colorFactor(
  #     palette = c("#a6cee3", "darkblue", "#33a02c", "#b2df8a",
  #                 "white", "#ff7f00", "#fdbf6f", "#e31a1c", "#fb9a99"),
  #     levels = c("Apartments", "Multifamily", "Townhouse", "Duplex",
  #                "No housing allowed", "Single-family attached", "Single-family detached", "Two-family attached",
  #                "Two-family detached"),
  #     domain = Max_Hou_Den$Max_Den   # Adjusting domain directly
  #   ),
  #   values = Max_Hou_Den$Max_Den,   # Removed the formula notation (~)
  #   title = "Maximum Housing Density"
  # ) %>%
  
  # #Nature
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
    group = "Nature of Land Use",
    label = ~Nature_Zoning$Nature_Specific,
    options = pathOptions(pane = "polygons")  # Added pane option
  )  %>%

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
      title = "Maximum Housing Density"
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
  ) %>%
  # Show only one group by default
  showGroup("Nature of Land Use") %>%
  
  # Layers Control
  addLayersControl(
    baseGroups = c("CartoDB.Positron"),
    overlayGroups = c("Zoning Atlas Definition", 
                      "Maximum Housing Density", 
                      "Nature of Land Use"),
    options = layersControlOptions(collapsed = FALSE, hideSingleBase = TRUE)
  )
  
#To export as html
saveWidget(RVA_Zoning, file="RVA_Zoning.html", selfcontained = FALSE)


#----------------------------------------------------------------------------------------------

#Area of study map
#Load full dataset
Richmond_Zoning <- read_sf(paste0(onedrivepath, "Zoning data/Richmond MSA/Richmond_Complete/Richmond_Zoning.shp")) 

#Load transitional units
Richmond_2020 <- read_sf(paste0(onedrivepath, "Mapping Richmond/Index/Index_2020/Richmond_Index_2020.shp")) %>%
  rename(Suburban_Index = Sbrbn_I,
         Urban_Index = Urbn_In,
         Landscape_Four = Lndscp_Fr,
         Landscape_Five = Lndscp_Fv,
         Landscape_Six = Lndsc_S,
         Landscape_All = Lndsc_A) %>%
  select(GEOID, Year, Landscape_Five) 

#Match projections
Richmond_2020 <- st_transform(Richmond_2020, crs = st_crs(Richmond_Zoning))


#Long from to facet (but struggling with legend)
Max_Density_Facet <- Richmond_Zoning %>%
  select(Max_Den, geometry) %>%
  mutate(Max_Den = case_when(
    Max_Den == "Single-family detached, duplex" ~ "Duplex",
    Max_Den == "Three two-family attached dwellings" ~ "Two-family attached dwellings",
    Max_Den == "Townhouses" ~ "Townhouse",
    Max_Den == "None" ~ "No housing allowed",
    T ~ Max_Den)) %>%
  mutate(Zoning_Def = "Maximum Housing Density") %>%
  rename(Description = Max_Den) %>%
  select(Zoning_Def, Description, geometry)
  
ZA_Facet <- Richmond_Zoning %>%
  select(ZA_Def, geometry) %>%
  mutate(Zoning_Def = "Zoning Atlas Definition") %>%
  rename(Description = ZA_Def) %>%
  select(Zoning_Def, Description, geometry)

Zoning_Facet  <- rbind(Max_Density_Facet, ZA_Facet)

library(tmap)

#Assign colors manually
color_values <- c(
  "Single-family detached dwellings" = "#b2df8a",
  "Single-family attached dwellings" = "#33a02c",
  "No housing allowed" = "white",
  "Multifamily dwellings" = "#ff7f00",
  "Two-family attached dwellings" = "#e31a1c",
  "Townhouse" = "#fb9a99",
  "Two-family detached dwellings" = "#fdbf6f",
  "Duplex" = "#80b1d3",
  "Apartments" = "#a6cee3",
  "Primarily Residential" = "#80b1d3",
  "Mixed with Residential" = "#fb8072",
  "Nonresidential" = "#ffffb3"
)


#Create county lines
County_Boundaries <- st_boundary(Counties_2020)
Transitional_Lines <- st_boundary(Richmond_2020)

#ZA Def
ZA_Def_Map <- tm_shape(Zoning_Facet[Zoning_Facet$Zoning_Def == "Zoning Atlas Definition", ]) +
  tm_fill("Description",
          style = "fixed", 
          palette = color_values,
          alpha = 0.75,
          legend.show = T) +
  tm_borders(alpha = 1,
             col = "white",
             lty = "solid",
             lwd = .0) +
  # tm_facets(by=c("Zoning_Def"), ncol  = 2) +
  tm_layout(main.title = "Zoning Atlas Definition",
            main.title.size = 1.4,
            main.title.position = "center",
            main.title.fontface = "bold",
            frame = F,
            legend.title.color = "white",
            legend.width = 1,
            legend.position = c(0.05, 0.05), 
            legend.title.size = 0.2,
            legend.title.fontface = "bold",
            legend.text.size = 1,
            legend.outside = F,
            legend.show = T,
            panel.show = F,
            panel.label.bg.color = "transparent",
            panel.label.color = "black",
            panel.labels = c("Maximum Housing Density", "Zoning Atlas Definition"),
            panel.label.fontface = "bold",
            # inner.margins = c(0.0, -0.0, 0.0, -0.2)
  ) +
  tm_shape(County_Boundaries) +
  tm_lines(col="black", lwd = 0.5, scale=2, legend.lwd.show = FALSE) 

#To save
tmap_save(
  tm = ZA_Def_Map,
  filename = "~/desktop/Zoning_Atlas_Map.png",
  height = 7,
  width = 8,
  dpi = 500
)


#Max Housing Den
Max_Den_Map <- tm_shape(Zoning_Facet[Zoning_Facet$Zoning_Def == "Maximum Housing Density", ]) +
  tm_fill("Description",
          style = "fixed", 
          palette = color_values,
          alpha = 0.75,
          legend.show = T) +
  tm_borders(alpha = 1,
             col = "white",
             lty = "solid",
             lwd = .0) +
  # tm_facets(by=c("Zoning_Def"), ncol  = 2) +
  tm_layout(main.title = "Maximum Housing Density",
            main.title.size = 1.4,
            main.title.position = "center",
            main.title.fontface = "bold",
            frame = F,
            legend.title.color = "white",
            legend.width = 1,
            legend.position = c(0.825, 0.05), 
            legend.title.size = 0.2,
            legend.title.fontface = "bold",
            legend.text.size = 1,
            legend.outside = F,
            legend.show = T,
            panel.show = F,
            panel.label.bg.color = "transparent",
            panel.label.color = "black",
            panel.labels = c("Maximum Housing Density", "Zoning Atlas Definition"),
            panel.label.fontface = "bold",
            # inner.margins = c(0.0, -0.2, 0.0, -0.2)  # Adds size to the right margin
  ) +
  tm_shape(County_Boundaries) +
  tm_lines(col="black", lwd = 0.5, scale=2, legend.lwd.show = FALSE) 


#To save
tmap_save(
  tm = Max_Den_Map,
  filename = "~/desktop/Max_Den_Map.png",
  height = 7,
  width = 12,
  dpi = 500
)

# 
# Zoning_Descriptions <- tmap_arrange(ZA_Def_Map, Max_Den_Map, nrow=1) 
# 
# 
# #To save
# tmap_save(
#   tm = Zoning_Descriptions,
#   filename = "~/desktop/Zoning_Maps_wDescriptions.png",
#   height = 7,
#   width = 12,
#   dpi = 500
# )


#Hanover and Henrico Mapping
HanoverHenrico <- Richmond_Zoning %>%
  filter(County == "Henrico" | County == "Hanover") %>%
  select(County, Max_Den, ZA_Def, geometry) %>%
  mutate(Max_Den = case_when(
    Max_Den == "Single-family detached, duplex" ~ "Duplex",
    Max_Den == "Three two-family attached dwellings" ~ "Two-family attached dwellings",
    Max_Den == "Townhouses" ~ "Townhouse",
    Max_Den == "None" ~ "No housing allowed",
    T ~ Max_Den)) 
  
#Filter Counties for outline
HanoverHenrico_Counties <- County_Boundaries %>%
  filter(NAME == "Henrico" | NAME == "Hanover" | NAME == "Richmond") 
  
#Filter transitional to Hanover and Henrico
# Transitional_Lines <- st_transform(Transitional_Lines, st_crs(HanoverHenrico))
# 
# # Now filter based on spatial relationship
# Transitional_Lines_HH <- Transitional_Lines %>%
#   filter(lengths(st_within(., HanoverHenrico)) > 0)

# Now filter based on spatial relationship
Richmond_2020 <- st_transform(Richmond_2020, st_crs(HanoverHenrico))

Transitional_HH <- Richmond_2020 %>%
  filter(lengths(st_intersects(., HanoverHenrico)) > 1) %>%
  filter(!str_detect(GEOID, "517600"))

Transitional_HH <- st_boundary(Transitional_HH)

#Map Hen and Han
Max_Den_HH <- tm_shape(HanoverHenrico) +
  tm_fill("Max_Den",
          style = "fixed", 
          palette = color_values,
          alpha = 0.75,
          legend.show = T) +
  tm_borders(alpha = 1,
             col = "white",
             lty = "solid",
             lwd = .0) +
  # tm_facets(by=c("Zoning_Def"), ncol  = 2) +
  tm_layout(main.title = "Maximum Housing Density",
            main.title.size = 1.4,
            main.title.position = "center",
            main.title.fontface = "bold",
            frame = F,
            legend.title.color = "white",
            legend.width = 1,
            legend.position = c(0.05, 0.05), 
            legend.title.size = 0.2,
            legend.title.fontface = "bold",
            legend.text.size = 1,
            legend.outside = F,
            legend.show = T,
            panel.show = F,
            panel.label.bg.color = "transparent",
            panel.label.color = "black",
            panel.labels = c("Maximum Housing Density", "Zoning Atlas Definition"),
            panel.label.fontface = "bold",
            inner.margins = c(0.0, -0.0, 0.0, -0.2)
  ) +
  tm_shape(HanoverHenrico_Counties) +
  tm_lines(col="darkgrey", lwd = 0.35, scale=2, legend.lwd.show = FALSE) +
  tm_shape(Transitional_HH[Transitional_HH$Landscape_Five == "Unstable", ]) +
  tm_lines(col="black", lwd = 0.5, scale=2, legend.lwd.show = FALSE) +
  tm_add_legend("line", col = c("black"),
                lty = c("solid"),
                labels = c("Transitional Spaces"),
                lwd = 3)
  
ZADef_HH <- tm_shape(HanoverHenrico) +
  tm_fill("ZA_Def",
          style = "fixed", 
          palette = color_values,
          alpha = 0.75,
          legend.show = T) +
  tm_borders(alpha = 1,
             col = "white",
             lty = "solid",
             lwd = .0) +
  # tm_facets(by=c("Zoning_Def"), ncol  = 2) +
  tm_layout(main.title = "Zoning Atlas Definition",
            main.title.size = 1.4,
            main.title.position = "center",
            main.title.fontface = "bold",
            frame = F,
            legend.title.color = "white",
            legend.width = 1,
            legend.position = c(0.05, 0.05), 
            legend.title.size = 0.2,
            legend.title.fontface = "bold",
            legend.text.size = 1,
            legend.outside = F,
            legend.show = T,
            panel.show = F,
            panel.label.bg.color = "transparent",
            panel.label.color = "black",
            panel.labels = c("Maximum Housing Density", "Zoning Atlas Definition"),
            panel.label.fontface = "bold",
            inner.margins = c(0.0, -0.0, 0.0, -0.2)
  ) +
  tm_shape(HanoverHenrico_Counties) +
  tm_lines(col="darkgrey", lwd = 0.35, scale=2, legend.lwd.show = FALSE) +
  tm_shape(Transitional_HH[Transitional_HH$Landscape_Five == "Unstable", ]) +
  tm_lines(col="black", lwd = 0.5, scale=2, legend.lwd.show = FALSE) +
  tm_add_legend("line", col = c("black"),
                lty = c("solid"),
                labels = c("Transitional Spaces"),
  lwd = 3)

Zoning_Descriptions <- tmap_arrange(ZADef_HH, Max_Den_HH, nrow=1) 

tmap_save(
  tm = Zoning_Descriptions,
  filename = "~/desktop/HenricoHanover_wDescriptions.png",
  height = 7,
  width = 13,
  dpi = 500
)


#----------------------------------------------------------------------------------------------
#Calculate polygon areas percentages
#Load full dataset
Richmond_Zoning <- read_sf(paste0(onedrivepath, "Zoning data/Richmond MSA/Richmond_Complete/Richmond_Zoning.shp")) %>%
  filter(County == "City of Richmond") %>%
  # select(County, Max_Den, ZA_Def, geometry) %>%
  mutate(Max_Den = case_when(
    Max_Den == "Single-family detached, duplex" ~ "Duplex",
    Max_Den == "Three two-family attached dwellings" ~ "Two-family attached dwellings",
    Max_Den == "Townhouses" ~ "Townhouse",
    Max_Den == "None" ~ "No housing allowed",
    T ~ Max_Den)) %>%
  filter(!Max_Den == "No housing allowed")


#Calc each polygon area
Richmond_Zoning <- Richmond_Zoning %>%
  mutate(GEOID_Area = st_area(geometry)) 
  
#Area by group
Max_Den_area <- Richmond_Zoning %>%
  group_by(Max_Den) %>%
  summarize(Max_Den_Area = sum(GEOID_Area, na.rm = TRUE), .groups = "drop") 

# Calculate the overall total area
Richmond_Area <- sum(Richmond_Zoning$GEOID_Area, na.rm = TRUE)

# Calculate the percentage of each Max_Den value
Max_Den_area <- Max_Den_area %>%
  mutate(Percentage = (Max_Den_Area / Richmond_Area) * 100)


#Calculating transitional spaces
#Load full dataset
Richmond_Zoning <- read_sf(paste0(onedrivepath, "Zoning data/Richmond MSA/Richmond_Complete/Richmond_Zoning.shp")) %>%
  # select(County, Max_Den, ZA_Def, geometry) %>%
  mutate(Max_Den = case_when(
    Max_Den == "Single-family detached, duplex" ~ "Duplex",
    Max_Den == "Three two-family attached dwellings" ~ "Two-family attached dwellings",
    Max_Den == "Townhouses" ~ "Townhouse",
    Max_Den == "None" ~ "No housing allowed",
    T ~ Max_Den))  %>%
  filter(!Max_Den == "No housing allowed")

#Calc each polygon area
# Richmond_Zoning <- Richmond_Zoning %>%
#   mutate(GEOID_Area = st_area(geometry)) 

#Load transitional units
Richmond_2020 <- read_sf(paste0(onedrivepath, "Mapping Richmond/Index/Index_2020/Richmond_Index_2020.shp")) %>%
  rename(Suburban_Index = Sbrbn_I,
         Urban_Index = Urbn_In,
         Landscape_Four = Lndscp_Fr,
         Landscape_Five = Lndscp_Fv,
         Landscape_Six = Lndsc_S,
         Landscape_All = Lndsc_A) %>%
  select(GEOID, Year, Landscape_Five) 

#Match projections
Richmond_2020 <- st_transform(Richmond_2020, crs = st_crs(Richmond_Zoning))
 
# Buffer with zero to fix geometries before intersecting
Richmond_Zoning_fixed <- st_buffer(Richmond_Zoning, 0)
Richmond_2020_fixed <- st_buffer(Richmond_2020, 0)

#Sum area and group sum
Richmond_Zoning_fixed <- Richmond_Zoning_fixed %>%
  mutate(geometry = st_make_valid(geometry)) %>% #Might not be needed if buffering - check this
  mutate(Zone_Area = st_area(geometry)) %>%
  group_by(Max_Den) %>%
  summarize(Max_Den_Area = sum(Zone_Area, na.rm = TRUE), .groups = "drop")

# Filter transitional units then create intersection
intersection <- Richmond_2020 %>%
  filter(Landscape_Five == "Unstable") %>%
  st_intersection(Richmond_Zoning_fixed, Richmond_2020_fixed) %>%
  mutate(Area_Intersect = st_area(geometry)) %>%
  group_by(Max_Den) %>%
  summarize(Max_Den_Area = sum(Area_Intersect, na.rm = TRUE), .groups = "drop")


# Calculate the area of the original Richmond_Zoning polygons
Richmond_2020_fixed <- Richmond_2020_fixed %>%
  filter(Landscape_Five == "Unstable") %>%
  mutate(Unstable_Area = st_area(geometry))

# Calculate the overall total area
Transitional_Area <- sum(Richmond_2020_fixed$Unstable_Area, na.rm = TRUE)

# Calculate the percentage of each Max_Den value
intersection <- intersection %>%
  mutate(Percentage = (Max_Den_Area / Transitional_Area) * 100) %>%
  # Calculate the sum of remaining percentages
  mutate(Adjusted_Percentage = (Percentage / sum(Percentage)) * 100)

#CONCENTRATED AREA ZONING
#Calculate percent of concentrated areas by zoning

#Load full dataset
Richmond_Zoning <- read_sf(paste0(onedrivepath, "Zoning data/Richmond MSA/Richmond_Complete/Richmond_Zoning.shp")) %>%
  # filter(County == "City of Richmond") %>%
  # select(County, Max_Den, ZA_Def, geometry) %>%
  mutate(Max_Den = case_when(
    Max_Den == "Single-family detached, duplex" ~ "Duplex",
    Max_Den == "Three two-family attached dwellings" ~ "Two-family attached dwellings",
    Max_Den == "Townhouses" ~ "Townhouse",
    Max_Den == "None" ~ "No housing allowed",
    T ~ Max_Den)) 

#Calc each polygon area
Richmond_Zoning <- Richmond_Zoning %>%
  mutate(GEOID_Area = st_area(geometry)) 

#Area by group
Max_Den_area <- Richmond_Zoning %>%
  group_by(Max_Den) %>%
  summarize(Max_Den_Area = sum(GEOID_Area, na.rm = TRUE), .groups = "drop") 

# Calculate the overall total area
Richmond_Area <- sum(Richmond_Zoning$GEOID_Area, na.rm = TRUE)

# Calculate the percentage of each Max_Den value
Max_Den_area <- Max_Den_area %>%
  mutate(Percentage = (Max_Den_Area / Richmond_Area) * 100)

#Load concentrated tracts
Income_LISA_all <- readRDS(file.path(onedrivepath, "Mapping Richmond/Binned Income Data/LISA/LISA_Income_RVA.rds")) %>%
  filter(Year == 2020) %>%
  mutate(Concentrations = case_when(
    Pval_i >= 0.05 ~ "Not significant",
    Med_Income_Adj > 26200 & Med_Income_Adj <= 125000 & Local_M_i < 0 ~ "Not significant",  # Middle income
    Med_Income_Adj > 26200 & Med_Income_Adj <= 125000 & Local_M_i > 0 ~ "Middle income clustered",  # Middle income
    Med_Income_Adj >= 125000 & Local_M_i < 0 ~ "High-none", #High income, not concentrated
    Med_Income_Adj >= 125000 & Local_M_i > 0 ~ "High-high", #High income, highly concentrated
    Med_Income_Adj <= 26200 & Local_M_i > 0 ~ "Low-high", #Low income, highly concentrated
    Med_Income_Adj <= 26200 & Local_M_i < 0 ~ "Low-none" #Low income, not concentrated
  ))

#Match projections
Richmond_2020 <- st_transform(Income_LISA_all, crs = st_crs(Richmond_Zoning))

#Sum area and group sum
Richmond_Zoning_fixed <- Richmond_Zoning %>%
  mutate(geometry = st_make_valid(geometry)) %>% #Might not be needed if buffering - check this
  mutate(Zone_Area = st_area(geometry)) %>%
  group_by(Max_Den) %>%
  summarize(Max_Den_Area = sum(Zone_Area, na.rm = TRUE), .groups = "drop")

# Filter transitional units then create intersection
intersection <- Richmond_2020 %>%
  filter(Concentrations == "Middle income clustered") %>%
  st_intersection(Richmond_Zoning_fixed, Richmond_2020_fixed) %>%
  mutate(Area_Intersect = st_area(geometry)) %>%
  group_by(Max_Den) %>%
  summarize(Max_Den_Area = sum(Area_Intersect, na.rm = TRUE), .groups = "drop")


# Calculate the area of the original Richmond_Zoning polygons
Richmond_2020_fixed <- Richmond_2020 %>%
  filter(Concentrations == "Middle income clustered") %>%
  mutate(Unstable_Area = st_area(geometry))

# Calculate the overall total area
Transitional_Area <- sum(Richmond_2020_fixed$Unstable_Area, na.rm = TRUE)

# Calculate the percentage of each Max_Den value
intersection <- intersection %>%
  mutate(Percentage = (Max_Den_Area / Transitional_Area) * 100) %>%
  # Calculate the sum of remaining percentages
  mutate(Adjusted_Percentage = (Percentage / sum(Percentage)) * 100) %>%
  select(Max_Den, Percentage)

#----------------------------------------------------------------------------------------------
#Map zoning codes across counties
  #then facet with border




