library(shiny)
library(leaflet)
library(dplyr)
library(sf)
library(geojsonio)
library(RColorBrewer)
library(metatargetr)

# ✅ Gracefully check if the France ZIP GeoJSON file exists
geojson_path <- "www/france_zips.geojson"
if (file.exists(geojson_path)) {
  fr_sf <- tryCatch({
    st_read(geojson_path)
  }, error = function(e) {
    showNotification("Error loading France ZIP GeoJSON. Check file path.", type = "error")
    NULL
  })
} else {
  fr_sf <- NULL
  warning("France ZIP GeoJSON file not found. The map will not render.")
}

ui <- fluidPage(
  titlePanel("Targeting Exclusions Map - France"),
  
  sidebarLayout(
    sidebarPanel(
      dateInput("selected_date", "Select Date", value = as.Date("2024-07-09")),
      selectInput("timeframe", "Select Timeframe (last X days)", choices = c(7, 30, 90), selected = 7),
      actionButton("load_data", "Load Data"),
      hr(),
      uiOutput("page_selector"),
      uiOutput("exclusion_selector")
    ),
    
    mainPanel(
      leafletOutput("targeting_map")
    )
  )
)

server <- function(input, output, session) {
  dataset <- reactiveVal(NULL)
  
  observeEvent(input$load_data, {
    req(input$selected_date, input$timeframe)
    
    showNotification("Fetching data for France...", type = "message")
    
    # ✅ Gracefully handle missing data from metatargetr
    data <- tryCatch({
      metatargetr::get_targeting_db(
        the_cntry = "FR",  
        tf = as.numeric(input$timeframe),  
        ds = as.character(input$selected_date)
      ) %>%
        filter(type == "location", location_type == "zips") %>%
        mutate(code_postal = sub("\\s*,.*", "", value))
    }, error = function(e) {
      showNotification("No data available for this date/timeframe.", type = "error")
      return(NULL)
    })
    
    if (is.null(data) || nrow(data) == 0) {
      showNotification("No data found for this date/timeframe. Please try another selection.", type = "warning")
      return()
    }
    
    dataset(data)
    updateSelectizeInput(session, "page_name", choices = unique(data$page_name), server = TRUE)
  })
  
  output$page_selector <- renderUI({
    req(dataset())
    selectizeInput("page_name", "Select Page Name", choices = NULL, multiple = FALSE, options = list(placeholder = "Start typing..."))
  })
  
  output$exclusion_selector <- renderUI({
    req(input$page_name)
    radioButtons("is_exclusion", "Filter by Exclusion:", choices = c("Targeting" = FALSE, "Exclusion" = TRUE))
  })
  
  output$targeting_map <- renderLeaflet({
    req(dataset(), input$page_name, input$is_exclusion, fr_sf)
    
    filtered_data <- dataset() %>%
      filter(page_name == input$page_name & is_exclusion == as.logical(input$is_exclusion))
    
    fr_sf_filtered <- fr_sf %>%
      left_join(filtered_data, by = c("code" = "code_postal")) %>%
      mutate(spend = as.numeric(gsub("[^0-9.]", "", total_spend_formatted)))  %>% 
      mutate(spend = ifelse(is.na(spend), 0, spend))
    
    print(fr_sf_filtered)
    
    pal <- colorNumeric(palette = "Blues", domain = fr_sf_filtered$spend, na.color = "gray")
    
    
    # Create HTML popup text for tooltips
    fr_sf_filtered$popup_text <- paste0(
      "<b>Name:</b> ", fr_sf_filtered$nom, "<br>",
      "<b>ZIP Code:</b> ", fr_sf_filtered$code, "<br>",
      "<b>Spend:</b> €", format(fr_sf_filtered$spend, big.mark = ",")
    )
    
    leaflet(fr_sf_filtered) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(spend),
        color = "white", weight = 0.5, opacity = 1,
        fillOpacity = 0.7, smoothFactor = 0.5,
        popup = ~popup_text,
        highlight = highlightOptions(weight = 2, color = "black", bringToFront = TRUE)
      ) %>%
      addLegend("bottomright", pal = pal, values = fr_sf_filtered$spend, title = "Total Spend (€)")
  })
}

shinyApp(ui, server)