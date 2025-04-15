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
  library(rsconnect)
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






#deploy locally
source('ui.R')
source('server.R')
shinyApp(ui, server)
# shiny::runApp(port = 8100) #specific port

#deploy to shinyapps.io
# rsconnect::deployApp()
#Error in func(fname, ...) : app.R did not return a shiny.appobj object.

