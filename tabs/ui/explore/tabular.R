attr_list <- list(
  "Total Population" = "Total Population", 
  "Household Population" = "Household Population", 
  "Group Quarters Population" = "GQ Population",
  "Total Housing Units" = "Total Housing",
  "Households" = "Occupied Housing"
)
years <- unique(df$year)

tab_tabular <- tabPanel(
  title = "Table",
  value = "tablr",
  div(class = "main",
    fluidRow(
      column(width = 3,
             selectInput("tablr_attr", 
                         "Dataset", 
                         choices = attr_list,
                         selected = "Total Population"),
             radioButtons("tablr_report_type",
                          "Report Type",
                          choices = list("Total" = "Total", 
                                         "Annual Change" = "Delta",
                                         "Annual Change (%)" = "Delta Percent"), 
                          selected = "Total"),
             radioButtons("tablr_juris",
                          "Jurisdiction",
                          choices = list("All" = 5,
                                         "County Only" = 1,
                                         "Unincorporated Only" = 2,
                                         "Incorporated Only" = 3,
                                         "Cities Only" = 4),
                          selected = 1),
             conditionalPanel("input.tablr_juris == 4 | input.tablr_juris == 5",
                              checkboxInput("tablr_city_combine",
                                            "Aggregate multi-county cities")),
             conditionalPanel("input.tablr_juris == 5 | input.tablr_juris == 4",
                              checkboxGroupInput("tablr_county",
                                                 "County",
                                                 choices = list("King" = "King",
                                                                "Kitsap" = "Kitsap",
                                                                "Pierce" = "Pierce",
                                                                "Snohomish" = "Snohomish")
                              )
             ),
             sliderInput("tablr_year", 
                         "Years",
                         min = as.numeric(min(years)), 
                         max = as.numeric(max(years)),
                         step = 1,
                         sep = "",
                         value = c(as.numeric(max(years))-3, as.numeric(max(years)))
             )
      ),
      column(width = 9,
             uiOutput("ui_tablr_main_table")
      )
    ) # end fluidRow
  ) # end div
)