fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "stylesheet.css"),
    tags$link(rel="stylesheet", href="https://fonts.googleapis.com/css2?family=Noto+Sans+JP:wght@100;300;400;500&family=Roboto&display=swap")
  ),
  
  navbarPage(
    title = "OFM Estimates for the Central Puget Sound Region",
    id = "navbar",
    selected = "home",
    fluid = T,

# tabs --------------------------------------------------------------------


    home,
    explore
  )
)