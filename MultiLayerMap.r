# Load required libraries
library(leaflet)
library(geojsonio)
library(RColorBrewer)
library(sf)

# Read GeoJSON data
gj_data <- st_read("Brattleboro_Parcels_2023_By_Residential_Use.geojson")

# Convert to an sf object
geojson_data <- st_as_sf(gj_data)

# Define color palette
color_palette <- c("Permitted Use" = "#A6C185", "Conditional Use" = "#E2D565", "Prohibited" = "#DB7A6B", "Permitted Use in Existing Structures, Conditional Use in New Construction" = "#7BA7CC")

# Create new columns for the colors
geojson_data$color_detached <- color_palette[geojson_data$`SINGLE.UNIT.DETACHED.DWELLING`]
geojson_data$color_attached <- color_palette[geojson_data$`SINGLE.UNIT.ATTACHED.DWELLING`]
geojson_data$color_accessory <- color_palette[geojson_data$`ACCESSORY.DWELLING`]
geojson_data$color_duplex <- color_palette[geojson_data$`DUPLEX..2.UNITS..OR.TWO.UNIT.DWELLING`]
geojson_data$color_triplex <- color_palette[geojson_data$`TRIPLEX..3.UNITS.`]
geojson_data$color_quadraplex <- color_palette[geojson_data$`QUADRAPLEX..4.UNITS.`]
geojson_data$color_multi_unit <- color_palette[geojson_data$`MULTI.UNIT.DWELLINGS..5..UNITS.`]

# Replace NA or NULL values with a default value
geojson_data$`OTHER.SPECIALIZED.RESIDENTIAL.STRUCTURES`[is.na(geojson_data$`OTHER.SPECIALIZED.RESIDENTIAL.STRUCTURES`)] <- "Default"

# Create a new column for the colors
geojson_data$color_other_specialized <- color_palette[geojson_data$`OTHER.SPECIALIZED.RESIDENTIAL.STRUCTURES`]

# Replace NA or NULL values with a default value
geojson_data$`RETIREMENT.HOUSING`[is.na(geojson_data$`RETIREMENT.HOUSING`)] <- "Default"

# Create a new column for the colors
geojson_data$color_retirement_housing <- color_palette[as.character(geojson_data$`RETIREMENT.HOUSING`)]
geojson_data$color_congregate_living <- color_palette[as.character(geojson_data$`CONGREGATE.LIVING`)]
geojson_data$color_assisted_living <- color_palette[as.character(geojson_data$`ASSISTED.LIVING`)]
geojson_data$color_skilled_nursing <- color_palette[as.character(geojson_data$`SKILLED.NURSING.SERVICES`)]


# Create a leaflet map
leaflet() %>%
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
			  popup = ~paste("Zoning District:", DISTRICT, "<br>", "Single Unit Detached Dwelling:", `SINGLE.UNIT.DETACHED.DWELLING`, "<br>", "Tax Map Parcel:", MAPID))%>%
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
			  popup = ~paste("Zoning District:", DISTRICT, "<br>", "Single Unit Attached Dwelling:", `SINGLE.UNIT.ATTACHED.DWELLING`, "<br>", "Tax Map Parcel:", MAPID))%>%
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
			  popup = ~paste("Zoning District:", DISTRICT, "<br>", "Accessory Dwelling Unit:", `ACCESSORY.DWELLING`, "<br>", "Tax Map Parcel:", MAPID))%>%
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
			  popup = ~paste("Zoning District:", DISTRICT, "<br>", "Duplex (2 Units):", `DUPLEX..2.UNITS..OR.TWO.UNIT.DWELLING`, "<br>", "Tax Map Parcel:", MAPID))%>%
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
			  popup = ~paste("Zoning District:", DISTRICT, "<br>", "Triplex (3 Units):", `TRIPLEX..3.UNITS.`, "<br>", "Tax Map Parcel:", MAPID))%>%
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
			  popup = ~paste("Zoning District:", DISTRICT, "<br>", "Quadraplex (4 Units):", `QUADRAPLEX..4.UNITS.`, "<br>", "Tax Map Parcel:", MAPID))%>%
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
			  popup = ~paste("Zoning District:", DISTRICT, "<br>", "Multi-Unit Dwellings (5+ Units):", `MULTI.UNIT.DWELLINGS..5..UNITS.`, "<br>", "Tax Map Parcel:", MAPID))%>%
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
			  popup = ~paste("Zoning District:", DISTRICT, "<br>", "Other Specialized Residential Structures:", `OTHER.SPECIALIZED.RESIDENTIAL.STRUCTURES`, "<br>", "Tax Map Parcel:", MAPID))%>%
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
			  popup = ~paste("Zoning District:", DISTRICT, "<br>", "Retirement Housing:", `RETIREMENT.HOUSING`, "<br>", "Tax Map Parcel:", MAPID))%>%
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
			  popup = ~paste("Zoning District:", DISTRICT, "<br>", "Congregate Living:", `CONGREGATE.LIVING`, "<br>", "Tax Map Parcel:", MAPID))%>%
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
			  popup = ~paste("Zoning District:", DISTRICT, "<br>", "Assisted Living:", `ASSISTED.LIVING`, "<br>", "Tax Map Parcel:", MAPID))%>%
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
			  popup = ~paste("Zoning District:", DISTRICT, "<br>", "Skilled Nusring Services:", `SKILLED.NURSING.SERVICES`, "<br>", "Tax Map Parcel:", MAPID))%>%
  # Add layer control
  addLayersControl(
    overlayGroups = c("Single-Unit Detached Dwelling", "Single-Unit Attached Dwelling", "Accessory Dwelling", "Duplex", "Triplex", "Quadraplex", "Multi-Unit Housing", "Other Specialized Residential Structures", "Retirement Housing", "Congregate Living", "Assisted Living", "Skilled Nursing Services"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  hideGroup(c("Single-Unit Attached Dwelling", "Accessory Dwelling", "Duplex", "Triplex", "Quadraplex", "Multi-Unit Housing", "Other Specialized Residential Structures", "Retirement Housing", "Congregate Living", "Assisted Living", "Skilled Nursing Services"))%>%
  addLegend(position = "bottomright",
            colors = c("#A6C185", "#E2D565", "#DB7A6B", "#7BA7CC"),
            labels = c("Permitted Use", "Conditional Use", "Prohibited", "Permitted Use in Existing Structures, Conditional Use in New Construction"),
            title = "Legend")
			