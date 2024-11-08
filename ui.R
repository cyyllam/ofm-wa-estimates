page_navbar(
  title = "OFM Estimates for the Central Puget Sound Region",
  nav_spacer(),
  nav_panel("About", about_tab_ui("about")),
  nav_panel("Explore", 
            page_sidebar(
              sidebar = sidebar_ui("sidebar"),
              card(
                table_ui('tbl')
              ),
              fillable_mobile = TRUE)
            )
  )


#   tags$head(
#     tags$link(rel = "stylesheet", type = "text/css", href = "stylesheet.css"),
#     tags$link(rel="stylesheet", href="https://fonts.googleapis.com/css2?family=Noto+Sans+JP:wght@100;300;400;500&family=Roboto&display=swap")
