# Display main overview page

about_tab_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    column(width = 12,
           div(class = 'intro-container', 
               div(class = 'intro', includeMarkdown(here("md", "home.md")))
           )
    )
  )

}

about_tab_server <- function(id) {
  
  moduleServer(id, function(input, output, session) { 
    ns <- session$ns
    
    
    
  }) # end moduleServer
  
}