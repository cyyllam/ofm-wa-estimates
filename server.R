server <- function(input, output, session) {
  
  observeEvent(!(input$`sidebar-juris` %in% c(4,5)), {
    # clear all checkboxes if user clicks on the county related summaries
    updateCheckboxGroupInput(session, 
                             "county",  
                             choices = list("King" = "King",
                                            "Kitsap" = "Kitsap",
                                            "Pierce" = "Pierce",
                                            "Snohomish" = "Snohomish"),
                             selected = NULL)
    
    updateCheckboxInput(session,
                        "city_combine",
                        value = F)
  })
  
  table_server(id = "tbl", 
               year = input$`sidebar-year`, 
               attribute = input$`sidebar-attr`, 
               jurisdiction = input$`sidebar-juris`, 
               county = input$`sidebar-county`, 
               report_type = input$`sidebar-report_type`, 
               city_combine = input$`sidebar-city_combine`)
 
}