sidebar_ui <- function(id) {
  ns <- NS(id)
  
  attr_list <- list(
    "Total Population" = "Total Population", 
    "Household Population" = "Household Population", 
    "Group Quarters Population" = "GQ Population",
    "Total Housing Units" = "Total Housing",
    "Households" = "Occupied Housing"
  )
  
  years <- unique(df$year)
  
  sidebar(width = "25%",
          selectInput(ns("attr"), 
                      "Dataset", 
                      choices = attr_list,
                      selected = "Total Population"),
          radioButtons(ns("report_type"),
                       "Report Type",
                       choices = list("Total" = "Total", 
                                      "Annual Change" = "Delta",
                                      "Annual Change (%)" = "Delta Percent"), 
                       selected = "Total"),
          radioButtons(ns("juris"),
                       "Jurisdiction",
                       choices = list("All" = 5,
                                      "County Only" = 1,
                                      "Unincorporated Only" = 2,
                                      "Incorporated Only" = 3,
                                      "Cities Only" = 4),
                       selected = 1),
          conditionalPanel("input.juris == 4 | input.juris == 5",
                           checkboxInput(ns("city_combine"),
                                         "Aggregate multi-county cities")),
          conditionalPanel("input.juris == 5 | input.juris == 4",
                           checkboxGroupInput(ns("county"),
                                              "County",
                                              choices = list("King" = "King",
                                                             "Kitsap" = "Kitsap",
                                                             "Pierce" = "Pierce",
                                                             "Snohomish" = "Snohomish")
                           )
          ),
          sliderInput(ns("year"), 
                      "Years",
                      min = as.numeric(min(years)), 
                      max = as.numeric(max(years)),
                      step = 1,
                      sep = "",
                      value = c(as.numeric(max(years))-3, as.numeric(max(years)))
          )
  ) 
  
}

sidebar_server <- function(id) {
  
  moduleServer(id, function(input, output, session) { 
    ns <- session$ns
    
  }) # end moduleServer
  
}