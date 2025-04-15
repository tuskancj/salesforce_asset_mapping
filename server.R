#source



#setup server
server <- function(input, output, session) {
  
  #filter the asset data
  filtered_data <- reactive({
    #Product Type
    if (is.null(input$type_filter)) {
      df <- data.frame(matrix(ncol = length(df_geo_codes), nrow = 0))
      colnames(df) <- names(df_geo_codes)
    } else {
      df <- df_geo_codes
      
      #region
      df <- df[df$product_clean %in% input$type_filter, ]
      # df <- df_geo_codes[df_geo_codes$product_clean %in% input$type_filter, ]
      if (input$region != "All") {
        df <- subset(df, region == input$region)
      }
      
      #primary FSE
      if (input$primary_fse != "All") {
        df <- subset(df, primary_fse == input$primary_fse)
      }
      
      #billable
      if (input$billable != 'All'){
        df <- subset(df, billable == input$billable)
      }
      
      #service contract
      if (input$service_contract != "All") {
        df <- subset(df, type == input$service_contract)
      }
    }
    df
  })
  
  #filter the FSE locations based on selection
  filtered_fse <- reactive({
    if (input$central_fse == T){
      df <- df_fse_locations
      if (input$primary_fse != "All") {
        df <- subset(df, primary_fse == input$primary_fse)
      }
      df
    } else{
      df <- data.frame(matrix(ncol = length(df_fse_locations), nrow = 0))
      colnames(df) <- names(df_fse_locations)
      df
    }
    
  })
  
  # Reactive check if any asset data exists
  has_data <- reactive({
    nrow(filtered_data()) > 0
  })
  
  # Reactive check if any field service data exists
  has_fse_data <- reactive({
    nrow(filtered_fse()) > 0
  })
  
  # update the machine count label
  output$machine_count <- renderText({
    df <- filtered_data()
    length(df$serial_number)
  })
  
  #reactive extraction of selected products for legend
  selected_products <- reactive({
    df <- filtered_data()
    unique(df$product_clean)
    # unique(filtered_data()[[input$product_clean]])
  })
  
  # if data exists, display map, otherwise, display note
  output$map_or_message <- renderUI({
    if (has_data()) {
      leafletOutput("map", height = 600)
    } else {
      div(style = "text-align: center; padding: 20px; height: 600px;", 
          h3("No data available. Select a Product to start."))
    }
  })
  
  # Create the map
  output$map <- renderLeaflet({
    req(has_data()) #only render if data
    l <- leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>% 
      
      #asset markers
      addCircleMarkers(data = filtered_data(), lng = ~lon_jitter, lat = ~lat_jitter, color = ~color, popup = ~paste("<b>Serial Number: </b>", serial_number, '<br>',
                                                                                                                    '<b>Account: </b>', account_name, '<br>',
                                                                                                                    '<b>Coverage: </b>', type, '<br>',
                                                                                                                    '<b>FSE: </b>', primary_fse)) %>% 
      
      addLegend(
        position = "topright",
        colors = colors_mapping[1:length(selected_products())],
        labels = selected_products(),
        title = "Product(s)")
    
    #FSE markers
    if(nrow(filtered_fse()) > 0){
      l %>% addMarkers(data = filtered_fse(), lng = ~lon, lat = ~lat, popup = ~paste('<b>FSE: </b>', primary_fse))
    } else{
      l
    }
  })
  
  # Update map based on filtered data
  observe({
    req(has_data())  # Ensure map exists before updating
    proxy <- leafletProxy("map", data = filtered_data()) %>%
      clearMarkers() %>%
      
      #asset markers
      addCircleMarkers(data = filtered_data(), lng = ~lon_jitter, lat = ~lat_jitter, color = ~color, popup = ~paste("<b>Serial Number: </b>", serial_number, '<br>',
                                                                                                                    '<b>Account: </b>', account_name, '<br>',
                                                                                                                    '<b>Coverage: </b>', type, '<br>',
                                                                                                                    '<b>FSE: </b>', primary_fse)) %>% 
      addLegend(
        position = "topright",
        colors = colors_mapping[1:length(selected_products())],
        labels = selected_products(),
        title = "Product(s)")
    
    #FSE markers
    if(nrow(filtered_fse()) >0){
      proxy %>% addMarkers(data = filtered_fse(), lng = ~lon, lat = ~lat, popup = ~paste('<b>FSE: </b>', primary_fse), group = 'fse_markers')
    } else{
      proxy
    }
    
  })
  
}