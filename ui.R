page_navbar(
  theme = bs_theme(base_font = "Poppins",
                   heading_font = "Sintony"),
  bg = "#005753",
  title = "OFM Estimates for the Central Puget Sound Region",
  # theme = "stylesheet.css",
  
  nav_spacer(),
  nav_panel("About", about_tab_ui("about")),
  nav_panel("Explore",
            
            page_sidebar(
              sidebar = sidebar_ui("sidebar"),
              card(
                table_ui('tbl')
              ),
              fillable_mobile = TRUE
            )
            
            
  )
)
