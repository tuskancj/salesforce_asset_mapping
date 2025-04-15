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