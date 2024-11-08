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
               year = reactive(input$`sidebar-year`), 
               attribute = reactive(input$`sidebar-attr`), 
               jurisdiction = reactive(input$`sidebar-juris`), 
               county = reactive(input$`sidebar-county`), 
               report_type = reactive(input$`sidebar-report_type`), 
               city_combine = reactive(input$`sidebar-city_combine`))
 
}