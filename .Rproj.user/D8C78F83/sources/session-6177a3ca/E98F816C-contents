suppressWarnings(suppressPackageStartupMessages({
  library(tidyverse)
  library(janitor)
  library(lubridate)
  library(ggplot2)
  library(ggmap)
  library(tidygeocoder)
  library(salesforcer)
  library(leaflet)
  library(leaflet.extras)
  library(shiny)
}))

colors_mapping <- c('#092c5c',
                    '#8fbce6',
                    '#f5d130',
                    'darkgrey',
                    '#3B5B91',
                    '#89A3C0',
                    'pink',
                    '#FFE680',
                    '#ECC43B',
                    '#0D3C61')

#source files
source('../product_mapping_from_serial_number.R') #private asset algorithm
source('../Geocode File/fse_locations.R') #file with specific FSE locations

#file paths
path_geocodes <- '../GeoCode File/assets_w_geo_codes.csv'

#if set to T -> will pull new data via SOQL (5-10 minutes), F -> will attempt to pull from previously saved csv (instant)
overwrite_geocodes <- F

################################
#Query Asset & Contract Data
################################

if(overwrite_geocodes){
  #log into salesforce
  sf_auth()
  
  # pull down information of person logged in
  # it's a simple easy call to get started 
  # and confirm a connection to the APIs
  user_info <- sf_user_info()
  sprintf("Organization Id: %s", user_info$organizationId)
  sprintf("User Id: %s", user_info$userId)
  
  # Record the start time
  start_time <- Sys.time()
  
  #grab contract data
  soql_contract <- "SELECT ContractNumber, Asset__r.SerialNumber, ContractType__c, ContractPrice__c, StartDate, EndDate, isActive__c, Status
            FROM Contract
            WHERE Status != 'Cancelled'"
  df_contracts_raw <- sf_query(soql_contract)
  
  #grab asset data
  soql_asset <- "SELECT SerialNumber, Account_Name__c, Address, InstallDate, Service_Territory__r.Name, Account.Region__c, NewPrimaryServiceTechnician__r.Name, NewSecondaryServiceTechnician__r.Name, Is_Luminex_Asset__c, Product2.Name
            FROM Asset"
  df_assets_raw <- sf_query(soql_asset)
  
  # Record the end time
  end_time <- Sys.time()
  
  # Calculate the duration
  duration <- end_time - start_time
  
  # Print the duration
  print(paste("Minutes taken:", round(duration, 2)))
}


################################
#Query Cleanup
################################
if(overwrite_geocodes){
  #cleanup
  df_contracts <- df_contracts_raw %>% 
    clean_names() %>% 
    rename(price = contract_price_c, 
           type = contract_type_c,
           is_active = is_active_c,
           serial_number = asset_r_serial_number)
  
  df_assets <- df_assets_raw %>% 
    clean_names() %>% 
    rename(account_name = account_name_c,
           is_luminex_asset = is_luminex_asset_c,
           product = product2_name,
           region = account_region_c,
           primary_fse = new_primary_service_technician_r_name,
           secondary_fse = new_secondary_service_technician_r_name,
           territory = service_territory_r_name)
  
  #add clean product name
  df_assets <- df_assets %>% mutate(product_clean = getProductName(serial_number, product))
}


################################
#Geocoding
################################

#to gather geo codes (takes time)
if(overwrite_geocodes){
  df_geo_codes <- df_assets %>% 
    filter(!is.na(serial_number)) %>% 
    filter(!product_clean %in% c('Other')) %>% 
    filter(!is.na(address_country) & !is.na(address_postal_code)) %>% 
    left_join(df_contracts %>% 
                filter(status == 'Activated') %>% 
                select(serial_number, type), by = 'serial_number', relationship = 'one-to-many', multiple = 'any') %>%
    mutate(single_line_address = paste(address_postal_code, address_country, sep = ' ')) %>% 
    mutate(type = case_when(is.na(type) ~ 'Billable', .default = type)) %>% 
    select(serial_number, product_clean, primary_fse, territory, region, type, single_line_address, account_name) %>% 
    geocode(address = single_line_address, method = 'osm', verbose = T, full_results = F) %>% 
    rename(lon = long)
  
  #if there's any missing geocodes, try a different method and merge back in
  if(length((df_geo_codes %>% filter(is.na(lat) == T))$lat>0)){
    #TODO:  Implement 'google' or 'arcgis' .Requires api key
    
    # df_geo_codes %>% filter(is.na(lat)) %>% head(5) %>% geocode(address = single_line_address, method = 'osm', verbose = T, full_results = F)
  }
  
  write_csv(df_geo_codes, path_geocodes, append = F)
} else{
  df_geo_codes <- read_csv(path_geocodes)
}


################################
#Geocoding Cleanup
################################
#remove NAs
df_geo_codes <- df_geo_codes %>% filter(is.na(lat)==F)

#add jitter for better mapping
set.seed(1842)  # For reproducibility
df_geo_codes$lat_jitter <- df_geo_codes$lat + runif(nrow(df_geo_codes), -0.001, 0.001)
df_geo_codes$lon_jitter <- df_geo_codes$lon + runif(nrow(df_geo_codes), -0.001, 0.001)

#add in accounts - this was added to geocoding section - double check
# df_geo_codes <- left_join(df_geo_codes, df_assets %>% select(serial_number, account_name), by = 'serial_number', relationship = 'one-to-many', multiple = 'any')

#one-hot billing vs contract
df_geo_codes <- df_geo_codes %>% mutate(billable = ifelse(type == 'Billable', T, F))

#set up colors for product type
list_products_unique <- unique(df_geo_codes$product_clean)
df_geo_codes <- df_geo_codes %>% mutate(color_base = case_when(product_clean == list_products_unique[1] ~ colors_mapping[1],
                                                               product_clean == list_products_unique[2] ~ colors_mapping[2],
                                                               product_clean == list_products_unique[3] ~ colors_mapping[3],
                                                               product_clean == list_products_unique[4] ~ colors_mapping[4],
                                                               product_clean == list_products_unique[5] ~ colors_mapping[5],
                                                               product_clean == list_products_unique[6] ~ colors_mapping[6],
                                                               product_clean == list_products_unique[7] ~ colors_mapping[7],
                                                               product_clean == list_products_unique[8] ~ colors_mapping[8],
                                                               .default = 'gray'))
df_geo_codes$color <- df_geo_codes$color_base


################################
#Local Shiny Mapping Application
################################
#setup page layout
ui <- fluidPage(
  # leafletOutput("map"),
  uiOutput('map_or_message'),
  splitLayout(
    div(
      #tag to count FSE primary machines
      tags$label("Machine Count: "),
      textOutput("machine_count"),
      
      #FSE locations
      checkboxInput('central_fse', 'Show FSE Locations', value = F),
      
      #Product Checkboxes
      checkboxGroupInput("type_filter", "Filter by Product",
                         choices = unique(df_geo_codes$product_clean))
    ),
    div(
      # Region drop down
      selectInput("region", "Region: ",
                  choices = c("All", sort(unique(df_geo_codes$region))),
                  selected = "North America"),
      
      # FSE drop down
      selectInput("primary_fse", "Primary FSE: ", 
                  choices = c("All", 
                              (df_fse_locations %>% arrange(last_name))$primary_fse, 
                              sort(unique((df_geo_codes %>% filter(!primary_fse %in% df_fse_locations$primary_fse))$primary_fse))), 
                  selected = "All"),
      
      # Billable drop down
      selectInput("billable", "Billable: ", 
                  choices = c("All", sort(unique(df_geo_codes$billable))),
                  selected = "All"),
      
      # Service Contract drop down
      selectInput("service_contract", "Service Contract: ", 
                  choices = c("All", sort(unique(df_geo_codes$type))),
                  selected = "All")
    )
  )
)

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
                                                                                                                    '<b>FSE: </b>', primary_fse))
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
                                                                                                                    '<b>FSE: </b>', primary_fse))
    
    #FSE markers
    if(nrow(filtered_fse()) >0){
      proxy %>% addMarkers(data = filtered_fse(), lng = ~lon, lat = ~lat, popup = ~paste('<b>FSE: </b>', primary_fse), group = 'fse_markers')
    } else{
      proxy
    }
    
  })
  
}

#deploy locally
shinyApp(ui, server)
# shiny::runApp(port = 8100) #specific port

