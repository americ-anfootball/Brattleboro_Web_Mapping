# Load required libraries
library(terra)
library(leaflet)
library(geojsonio)
library(RColorBrewer)
library(sf)
library(pandoc)
library(htmlwidgets)

# Read GeoJSON data
gj_data <- st_read("Brattleboro_Parcels_2023_By_Residential_Use.geojson")
hro_gj <- st_read("Brattleboro_Historic_Districts.geojson")
nda_gj <- st_read("Brattleboro_NDA_2023.geojson")
fho_gj <- st_read("FEMA_FHO_Brattleboro_Clip.geojson")
mmhod_gj <- st_read("MMHOD.geojson")
wetl_gj <- st_read("VT_Wetlands_Brattleboro_Clip.geojson")
protl_gj <- st_read("VT_Protected_Lands_Brattleboro_Clip.geojson")

#additional data
slope_raster <- rast("S:/INTERNAL SUPPORT/GIS/LiDAR/Brattleboro_Slope_2024_downsampled.tif")

rp_data <- st_read("Rural_Parcels_500ft_WM.geojson")
ws_buffer_data <- st_read("500ft_Buffer_Both_Sewer_and_Water.geojson")
water_pipes_data <- st_read("Water_Pipes.geojson")
sewer_pipes_data <- st_read("Sewer_Pipes.geojson")
roofprint_data <- st_read("Brattleboro_E911_Roofprints.geojson")
hro_structures_data <- st_read("HRO_Structures.geojson")
hro_parcels_data <- st_read("HRO_Parcels.geojson")
non_hro_nrhp_sites_data <- st_read("Non_HRO_NRHP_Sites.geojson")
non_hro_vrhp_sites_data <- st_read("Non_HRO_VT_Register_Sites.geojson")
row_roads_data <- st_read("Brattleboro_ROW_Roads_Updated.geojson")
subdivision_potential_data <-st_read("Brattleboro_Parcels_By_Subdivision_Potential.geojson")
water_supply_elevation_areas_data <-st_read("Brattleboro_Water_Supply_Elevation_Areas.geojson")
#contour_data <-st_read("Brattleboro_10ft_Contours.geojson")
zoning_district_data <-st_read("brattleboro_zoning_districts.geojson")

# Convert to an sf object
geojson_data <- st_as_sf(gj_data)
geojson_data_hro <- st_as_sf(hro_gj)
geojson_data_nda <- st_as_sf(nda_gj)
geojson_data_fho <- st_as_sf(fho_gj)
geojson_data_mmhod <- st_as_sf(mmhod_gj)
geojson_data_wetl <- st_as_sf(wetl_gj)
geojson_data_protl <- st_as_sf(protl_gj)
rp_data_sf <- st_as_sf(rp_data)
ws_buffer_data_sf <- st_as_sf(ws_buffer_data)
water_pipes_data_sf <- st_as_sf(water_pipes_data)
sewer_pipes_data_sf <- st_as_sf(sewer_pipes_data)
roofprint_data_sf <- st_as_sf(roofprint_data)
hro_structures_data_sf <-st_as_sf(hro_structures_data)
hro_parcels_data_sf <-st_as_sf(hro_parcels_data)
non_hro_nrhp_sites_data_sf <-st_as_sf(non_hro_nrhp_sites_data)
non_hro_vrhp_sites_data_sf <-st_as_sf(non_hro_vrhp_sites_data)
row_roads_data_sf <-st_as_sf(row_roads_data)
subdivision_potential_data_sf <-st_as_sf(subdivision_potential_data)
water_supply_elevation_areas_data_sf <-st_as_sf(water_supply_elevation_areas_data)
#contour_data_sf <-st_as_sf(contour_data)
zoning_district_data_sf <-st_as_sf(zoning_district_data)

# Define color palette
color_palette <- c("Permitted Use" = "#A6C185", "Conditional Use" = "#E2D565", "Prohibited" = "#DB7A6B", "Permitted Use in Existing Structures, Conditional Use in New Construction" = "#7BA7CC")
color_palette_2 <- c("UC" = "#D29DA1", "VC" = "#FFDCC2", "SC" = "#FBA69D", "NC" = "#E5B28B", "MU" = "#FFCE99", "RN36" = "#E4D00A", "RN54" = "#F9EE8A", "RR" = "#D2EED4", "RL" = "#a2c596", "WF" = "#C2FCFF", "IT" = "#E8EAED", "IN" = "#CABADE")


# Create new columns for the use type colors
geojson_data$color_detached <- color_palette[geojson_data$`SINGLE.UNIT.DETACHED.DWELLING`]
geojson_data$color_attached <- color_palette[geojson_data$`SINGLE.UNIT.ATTACHED.DWELLING`]
geojson_data$color_accessory <- color_palette[geojson_data$`ACCESSORY.DWELLING`]
geojson_data$color_duplex <- color_palette[geojson_data$`DUPLEX..2.UNITS..OR.TWO.UNIT.DWELLING`]
geojson_data$color_triplex <- color_palette[geojson_data$`TRIPLEX..3.UNITS.`]
geojson_data$color_quadraplex <- color_palette[geojson_data$`QUADRAPLEX..4.UNITS.`]
geojson_data$color_multi_unit <- color_palette[geojson_data$`MULTI.UNIT.DWELLINGS..5..UNITS.`]

# Replace NA or NULL values with a default value
geojson_data$`OTHER.SPECIALIZED.RESIDENTIAL.STRUCTURES`[is.na(geojson_data$`OTHER.SPECIALIZED.RESIDENTIAL.STRUCTURES`)] <- "Default"

# Create a new column for the use type colors
geojson_data$color_other_specialized <- color_palette[geojson_data$`OTHER.SPECIALIZED.RESIDENTIAL.STRUCTURES`]

# Replace NA or NULL values with a default value
geojson_data$`RETIREMENT.HOUSING`[is.na(geojson_data$`RETIREMENT.HOUSING`)] <- "Default"

# Create a new column for the use type colors
geojson_data$color_retirement_housing <- color_palette[as.character(geojson_data$`RETIREMENT.HOUSING`)]
geojson_data$color_congregate_living <- color_palette[as.character(geojson_data$`CONGREGATE.LIVING`)]
geojson_data$color_assisted_living <- color_palette[as.character(geojson_data$`ASSISTED.LIVING`)]
geojson_data$color_skilled_nursing <- color_palette[as.character(geojson_data$`SKILLED.NURSING.SERVICES`)]

# Replace NA or NULL values with a default value
geojson_data$DISTRICT[is.na(geojson_data$DISTRICT)] <- "Default"

# Create a new column for the district colors
zoning_district_data_sf$color_district <- color_palette_2[as.character(zoning_district_data_sf$NOTE)]


# Create a leaflet map
m <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  # Add GeoJSON layer for "SINGLE-UNIT DETACHED DWELLING"
  addPolygons(data = geojson_data,
              fillColor = ~color_detached,
              color = "white",
              weight = 0.5,
              opacity = 1,
              dashArray = "3",
              fillOpacity = 1.0,
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                fillOpacity = 1.0,
                bringToFront = TRUE),
              group = "Single-Unit Detached Dwelling",
			  popup = ~paste("Zoning District:", DISTRICT, "<br>", "Single-Unit Detached Dwelling:", `SINGLE.UNIT.DETACHED.DWELLING`, "<br>", "Tax Map Parcel:", MAPID, "<br>", "NDA Status:", NDA_STATUS, "<br>", "Historic District Status:", "<br>", HISTORIC_DISTRICT, "<br>", "Protected or Conserved:", CONSERVED, "<br>", "Wetland:", WETLAND, "<br>", "Flood Hazard Overlay Status:", FLOOD_HAZARD, "<br>", "Missing Middle Overlay Status:", MMHOD))%>%
  # Add GeoJSON layer for SINGLE-UNIT ATTACHED DWELLING
  addPolygons(data = geojson_data,
              fillColor = ~color_attached,
              color = "white",
              weight = 0.5,
              opacity = 1,
              dashArray = "3",
              fillOpacity = 1.0,
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                fillOpacity = 1.0,
                bringToFront = TRUE),
              group = "Single-Unit Attached Dwelling",
			  popup = ~paste("Zoning District:", DISTRICT, "<br>", "Single-Unit Attached Dwelling:", `SINGLE.UNIT.ATTACHED.DWELLING`, "<br>", "Tax Map Parcel:", MAPID, "<br>", "NDA Status:", NDA_STATUS, "<br>", "Historic District Status:", "<br>", HISTORIC_DISTRICT, "<br>", "Protected or Conserved:", CONSERVED, "<br>", "Wetland:", WETLAND, "<br>", "Flood Hazard Overlay Status:", FLOOD_HAZARD, "<br>", "Missing Middle Overlay Status:", MMHOD))%>%
  # Add GeoJSON layer for ACCESSORY DWELLING
  addPolygons(data = geojson_data,
              fillColor = ~color_accessory,
              color = "white",
              weight = 0.5,
              opacity = 1,
              dashArray = "3",
              fillOpacity = 1.0,
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                fillOpacity = 1.0,
                bringToFront = TRUE),
              group = "Accessory Dwelling",
			  popup = ~paste("Zoning District:", DISTRICT, "<br>", "Accessory Dwelling Unit:", `ACCESSORY.DWELLING`, "<br>", "<br>", "Tax Map Parcel:", MAPID, "<br>", "NDA Status:", NDA_STATUS, "<br>", "Historic District Status:", "<br>", HISTORIC_DISTRICT, "<br>", "Protected or Conserved:", CONSERVED, "<br>", "Wetland:", WETLAND, "<br>", "Flood Hazard Overlay Status:", FLOOD_HAZARD, "<br>", "Missing Middle Overlay Status:", MMHOD))%>%
  # Add GeoJSON layer for DUPLEX
  addPolygons(data = geojson_data,
              fillColor = ~color_duplex,
              color = "white",
              weight = 0.5,
              opacity = 1,
              dashArray = "3",
              fillOpacity = 1.0,
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                fillOpacity = 1.0,
                bringToFront = TRUE),
              group = "Duplex",
			  popup = ~paste("Zoning District:", DISTRICT, "<br>", "Duplex (2 Units):", `DUPLEX..2.UNITS..OR.TWO.UNIT.DWELLING`, "<br>", "Tax Map Parcel:", MAPID, "<br>", "NDA Status:", NDA_STATUS, "<br>", "Historic District Status:", "<br>", HISTORIC_DISTRICT, "<br>", "Protected or Conserved:", CONSERVED, "<br>", "Wetland:", WETLAND, "<br>", "Flood Hazard Overlay Status:", FLOOD_HAZARD, "<br>", "Missing Middle Overlay Status:", MMHOD))%>%
  # Add GeoJSON layer for TRIPLEX
  addPolygons(data = geojson_data,
              fillColor = ~color_triplex,
              color = "white",
              weight = 0.5,
              opacity = 1,
              dashArray = "3",
              fillOpacity = 1.0,
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                fillOpacity = 1.0,
                bringToFront = TRUE),
              group = "Triplex",
			  popup = ~paste("Zoning District:", DISTRICT, "<br>", "Triplex (3 Units):", `TRIPLEX..3.UNITS.`, "<br>", "Tax Map Parcel:", MAPID, "<br>", "NDA Status:", NDA_STATUS, "<br>", "Historic District Status:", "<br>", HISTORIC_DISTRICT, "<br>", "Protected or Conserved:", CONSERVED, "<br>", "Wetland:", WETLAND, "<br>", "Flood Hazard Overlay Status:", FLOOD_HAZARD, "<br>", "Missing Middle Overlay Status:", MMHOD))%>%
  # Add GeoJSON layer for QUADRAPLEX
  addPolygons(data = geojson_data,
              fillColor = ~color_quadraplex,
              color = "white",
              weight = 0.5,
              opacity = 1,
              dashArray = "3",
              fillOpacity = 1.0,
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                fillOpacity = 1.0,
                bringToFront = TRUE),
              group = "Quadraplex",
			  popup = ~paste("Zoning District:", DISTRICT, "<br>", "Quadraplex (4 Units):", `QUADRAPLEX..4.UNITS.`, "<br>", "Tax Map Parcel:", MAPID, "<br>", "NDA Status:", NDA_STATUS, "<br>", "Historic District Status:", "<br>", HISTORIC_DISTRICT, "<br>", "Protected or Conserved:", CONSERVED, "<br>", "Wetland:", WETLAND, "<br>", "Flood Hazard Overlay Status:", FLOOD_HAZARD, "<br>", "Missing Middle Overlay Status:", MMHOD))%>%
  # Add GeoJSON layer for MULTI UNIT
  addPolygons(data = geojson_data,
              fillColor = ~color_multi_unit,
              color = "white",
              weight = 0.5,
              opacity = 1,
              dashArray = "3",
              fillOpacity = 1.0,
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                fillOpacity = 1.0,
                bringToFront = TRUE),
              group = "Multi-Unit Housing",
			  popup = ~paste("Zoning District:", DISTRICT, "<br>", "Multi-Unit Dwellings (5+ Units):", `MULTI.UNIT.DWELLINGS..5..UNITS.`, "<br>", "Tax Map Parcel:", MAPID, "<br>", "NDA Status:", NDA_STATUS, "<br>", "Historic District Status:", "<br>", HISTORIC_DISTRICT, "<br>", "Protected or Conserved:", CONSERVED, "<br>", "Wetland:", WETLAND, "<br>", "Flood Hazard Overlay Status:", FLOOD_HAZARD, "<br>", "Missing Middle Overlay Status:", MMHOD))%>%
  # Add GeoJSON layer for OTHER SPECIALIZED RESIDENTIAL STRUCTURES
  addPolygons(data = geojson_data,
              fillColor = ~color_other_specialized,
              color = "white",
              weight = 0.5,
              opacity = 1,
              dashArray = "3",
              fillOpacity = 1.0,
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                fillOpacity = 1.0,
                bringToFront = TRUE),
              group = "Other Specialized Residential Structures",
			  popup = ~paste("Zoning District:", DISTRICT, "<br>", "Other Specialized Residential Structures:", `OTHER.SPECIALIZED.RESIDENTIAL.STRUCTURES`, "<br>", "Tax Map Parcel:", MAPID, "<br>", "NDA Status:", NDA_STATUS, "<br>", "Historic District Status:", "<br>", HISTORIC_DISTRICT, "<br>", "Protected or Conserved:", CONSERVED, "<br>", "Wetland:", WETLAND, "<br>", "Flood Hazard Overlay Status:", FLOOD_HAZARD, "<br>", "Missing Middle Overlay Status:", MMHOD))%>%
  # Add GeoJSON layer for RETIREMENT HOUSING
  addPolygons(data = geojson_data,
              fillColor = ~color_retirement_housing,
              color = "white",
              weight = 0.5,
              opacity = 1,
              dashArray = "3",
              fillOpacity = 1.0,
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                fillOpacity = 1.0,
                bringToFront = TRUE),
              group = "Retirement Housing",
			  popup = ~paste("Zoning District:", DISTRICT, "<br>", "Retirement Housing:", `RETIREMENT.HOUSING`, "<br>", "Tax Map Parcel:", MAPID, "<br>", "NDA Status:", NDA_STATUS, "<br>", "Historic District Status:", "<br>", HISTORIC_DISTRICT, "<br>", "Protected or Conserved:", CONSERVED, "<br>", "Wetland:", WETLAND, "<br>", "Flood Hazard Overlay Status:", FLOOD_HAZARD, "<br>", "Missing Middle Overlay Status:", MMHOD))%>%
  # Add GeoJSON layer for CONGREGATE LIVING
  addPolygons(data = geojson_data,
              fillColor = ~color_congregate_living,
              color = "white",
              weight = 0.5,
              opacity = 1,
              dashArray = "3",
              fillOpacity = 1.0,
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                fillOpacity = 1.0,
                bringToFront = TRUE),
              group = "Congregate Living",
			  popup = ~paste("Zoning District:", DISTRICT, "<br>", "Congregate Living:", `CONGREGATE.LIVING`, "<br>", "Tax Map Parcel:", MAPID, "<br>", "NDA Status:", NDA_STATUS, "<br>", "Historic District Status:", "<br>", HISTORIC_DISTRICT, "<br>", "Protected or Conserved:", CONSERVED, "<br>", "Wetland:", WETLAND, "<br>", "Flood Hazard Overlay Status:", FLOOD_HAZARD, "<br>", "Missing Middle Overlay Status:", MMHOD))%>%
  # Add GeoJSON layer for ASSISTED LIVING
  addPolygons(data = geojson_data,
              fillColor = ~color_assisted_living,
              color = "white",
              weight = 0.5,
              opacity = 1,
              dashArray = "3",
              fillOpacity = 1.0,
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                fillOpacity = 1.0,
                bringToFront = TRUE),
              group = "Assisted Living",
			  popup = ~paste("Zoning District:", DISTRICT, "<br>", "Assisted Living:", `ASSISTED.LIVING`, "<br>", "Tax Map Parcel:", MAPID, "<br>", "NDA Status:", NDA_STATUS, "<br>", "Historic District Status:", "<br>", HISTORIC_DISTRICT, "<br>", "Protected or Conserved:", CONSERVED, "<br>", "Wetland:", WETLAND, "<br>", "Flood Hazard Overlay Status:", FLOOD_HAZARD, "<br>", "Missing Middle Overlay Status:", MMHOD))%>%
  # Add GeoJSON layer for SKILLED NURSING SERVICES
  addPolygons(data = geojson_data,
              fillColor = ~color_skilled_nursing,
              color = "white",
              weight = 0.5,
              opacity = 1,
              dashArray = "3",
              fillOpacity = 1.0,
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                fillOpacity = 1.0,
                bringToFront = TRUE),
              group = "Skilled Nursing Services",
			  popup = ~paste("Zoning District:", DISTRICT, "<br>", "Skilled Nusring Services:", `SKILLED.NURSING.SERVICES`, "<br>", "Tax Map Parcel:", MAPID, "<br>", "NDA Status:", NDA_STATUS, "<br>", "Historic District Status:", "<br>", HISTORIC_DISTRICT, "<br>", "Protected or Conserved:", CONSERVED, "<br>", "Wetland:", WETLAND, "<br>", "Flood Hazard Overlay Status:", FLOOD_HAZARD, "<br>", "Missing Middle Overlay Status:", MMHOD))%>%
  # Add GeoJSON layer for Historic Resource Overlay District
  addPolygons(data = geojson_data_hro,
              fillColor = "#9F4576",
              color = "black",
              weight = 0.25,
              opacity = 1.0,
              fillOpacity = 0.6,
              group = "Historic Resource Overlay District (HRO)")%>%
  # Add GeoJSON layer for Neighborhood Development Area
  addPolygons(data = geojson_data_nda,
              fillColor = "#FA8072",
              color = "black",
              weight = 0.25,
              opacity = 1.0,
              fillOpacity = 0.6,
              group = "Neighborhood Development Area (NDA)")%>%
  # Add GeoJSON layer for Flood Hazard Overlay District
  addPolygons(data = geojson_data_fho,
              fillColor = "#72A0C1",
              color = "black",
              weight = 0.25,
              opacity = 1.0,
              fillOpacity = 0.6,
              group = "Flood Hazard Overlay District (FHO)")%>%
  # Add GeoJSON layer for Missing Middle Housing Overlay District
  addPolygons(data = geojson_data_mmhod,
              fillColor = "#B0DAF1",
              color = "black",
              weight = 0.25,
              opacity = 1.0,
              fillOpacity = 0.6,
              group = "Missing Middle Housing Overlay District (MMHOD)")%>%
  # Add GeoJSON layer for Wetlands
  addPolygons(data = geojson_data_wetl,
              fillColor = "#85B09A",
              color = "black",
              weight = 0.25,
              opacity = 1.0,
              fillOpacity = 0.6,
              group = "Wetland")%>%
  # Add GeoJSON layer for Conserved Land
  addPolygons(data = geojson_data_protl,
              fillColor = "#B56917",
              color = "black",
              weight = 0.25,
              opacity = 1.0,
              fillOpacity = 0.6,
              group = "Conserved Land")%>%
  # Add layer for Served Rural Parcels
  addPolygons(data = rp_data_sf,
              fillColor = "#DE6FA1",
              color = "black",
              weight = 0.25,
              opacity = 1.0,
              fillOpacity = 0.6,
              group = "Serviced Rural Parcels")%>%
  #Add layer for 500ft buffer of both sewer and water lines
  addPolygons(data = ws_buffer_data_sf,
              fillColor = "#BFD7EA",
              color = "black",
              weight = 0.25,
              opacity = 1.0,
              fillOpacity = 0.6,
              group = "Sewer and Water Service 500ft Buffer")%>%
  #Add layer for water lines
    addPolygons(data = water_pipes_data_sf,
              fillColor = "#EEE0CB",
              color = "black",
              weight = 2.0,
              opacity = 1.0,
              fillOpacity = 0.6,
              group = "Existing Water Lines")%>%
  #Add layer for sewer lines
  addPolygons(data = sewer_pipes_data_sf,
              fillColor = "00563B",
			  color = "black",
              weight = 2.0,
              opacity = 1.0,
			  fillOpacity = 0.6,
              group = "Existing Sewer Lines")%>%
  #Add layer for E911 Roofprints
  addPolygons(data = roofprint_data_sf,
              fillColor = "#EEE0CB",
              color = "black",
              weight = 0.25,
              opacity = 1.0,
              fillOpacity = 0.6,
              group = "Building Footprints")%>%
  #Add layer for HRO Roofprints
  addPolygons(data = hro_structures_data_sf,
              fillColor = "#E6E6FA",
              color = "black",
              weight = 0.25,
              opacity = 1.0,
              fillOpacity = 0.6,
              group = "HRO Structures")%>%
  #Add layer for HRO Parcels
  addPolygons(data = hro_parcels_data_sf,
              fillColor = "#4B0082",
              color = "black",
              weight = 0.25,
              opacity = 1.0,
              fillOpacity = 0.6,
              group = "HRO Parcels")%>%
  #Add layer for Non-HRO NRHP Sites
  addPolygons(data = non_hro_nrhp_sites_data_sf,
              fillColor = "#E9FFDB",
              color = "black",
              weight = 0.25,
              opacity = 1.0,
              fillOpacity = 0.6,
              group = "Non-HRO NRHP Sites")%>%
  #Add layer for Non-HRO VRHP Sites
  addPolygons(data = non_hro_vrhp_sites_data_sf,
              fillColor = "#DDA0DD",
              color = "black",
              weight = 0.25,
              opacity = 1.0,
              fillOpacity = 0.6,
              group = "Non-HRO VRHP Sites")%>%
  #Add layer for road rights of way
  addPolygons(data = row_roads_data_sf,
              fillColor = "#F7F7F7",
              color = "black",
              weight = 0.25,
              opacity = 1.0,
              fillOpacity = 0.6,
              group = "Roads")%>%
  addPolygons(data = zoning_district_data_sf,
              fillColor = ~color_district,
              color = "black",
              weight = 0.25,
              opacity = 1.0,
              fillOpacity = 0.6,
              group = "Zoning Districts")%>%
  # Add layer control
    addLayersControl(
    overlayGroups = c("Single-Unit Detached Dwelling", "Single-Unit Attached Dwelling", "Accessory Dwelling", "Duplex", "Triplex", "Quadraplex", "Multi-Unit Housing", "Other Specialized Residential Structures", "Retirement Housing", "Congregate Living", "Assisted Living", "Skilled Nursing Services", "Historic Resource Overlay District (HRO)", "Neighborhood Development Area (NDA)", "Flood Hazard Overlay District (FHO)", "Missing Middle Housing Overlay District (MMHOD)", "Wetland", "Conserved Land", "Serviced Rural Parcels", "Sewer and Water Service 500ft Buffer", "Zoning Districts", "Building Footprints", "HRO Structures", "HRO Parcels", "Non-HRO NRHP Sites", "Non-HRO VRHP Sites", "Roads", "Existing Sewer Lines", "Existing Water Lines"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%  
  hideGroup(c("Single-Unit Attached Dwelling", "Accessory Dwelling", "Duplex", "Triplex", "Quadraplex", "Multi-Unit Housing", "Other Specialized Residential Structures", "Retirement Housing", "Congregate Living", "Assisted Living", "Skilled Nursing Services", "Historic Resource Overlay District (HRO)", "Neighborhood Development Area (NDA)", "Flood Hazard Overlay District (FHO)", "Missing Middle Housing Overlay District (MMHOD)", "Wetland", "Conserved Land", "Serviced Rural Parcels", "Sewer and Water Service 500ft Buffer", "Zoning Districts", "Building Footprints", "HRO Structures", "HRO Parcels", "Non-HRO NRHP Sites", "Non-HRO VRHP Sites", "Roads", "Existing Sewer Lines", "Existing Water Lines"))%>%
  addLegend(position = "bottomright",
            colors = c("#A6C185", "#E2D565", "#DB7A6B", "#7BA7CC"),
            labels = c("Permitted Use", "Conditional Use", "Prohibited", "Permitted Use in Existing Structures, Conditional Use in New Construction"),
            title = "Legend") %>%
print(m)

saveWidget(m, "Residential_Use_Finder1.html", selfcontained = TRUE)			
