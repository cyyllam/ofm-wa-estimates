server <- function(input, output, session) {
  observeEvent(!(input$tablr_juris %in% c(4,5)), {
    # clear all checkboxes if user clicks on the county related summaries
    updateCheckboxGroupInput(session, 
                             "tablr_county",  
                             choices = list("King" = "King",
                                            "Kitsap" = "Kitsap",
                                            "Pierce" = "Pierce",
                                            "Snohomish" = "Snohomish"),
                             selected = NULL)
    
    updateCheckboxInput(session,
                        "tablr_city_combine",
                        value = F)
  })
  
  filter_data <- reactive({
    years <- input$tablr_year
    id_cols <- c("Filter", "County", "Jurisdiction")

    d <- df %>% 
      arrange(year) %>% 
      filter(attr == input$tablr_attr,
             year %in% seq(min(years), max(years))) 

    if (input$tablr_juris %in% c(1:4)) {
      d <- d %>% filter(Filter == input$tablr_juris)
    }
    
    if (input$tablr_juris %in% c(4, 5) & input$tablr_city_combine) {
      d_part_a <- d %>% filter(Jurisdiction %in% str_subset(Jurisdiction, "(part)"))

      suppressWarnings(d_part_b <- d %>% 
        filter(Jurisdiction %in% str_subset(Jurisdiction, "(part)")) %>% 
        group_by(Filter, Jurisdiction, year, attr) %>% 
        summarise(value = sum(value), County = list(County)) %>% 
        mutate(Jurisdiction = str_extract(Jurisdiction, "^\\w+"),
               County = str_extract(County, '"(\\w+".*"\\w+)"'),
               County = str_replace_all(County, c('"' = "",
                                                  ',' = "-",
                                                  ' ' = ''))
        ))
      
      d <- d %>% 
        anti_join(d_part_a) %>% 
        bind_rows(d_part_b) %>% 
        arrange(Jurisdiction, County)
    }
    
    if (!is.null(input$tablr_county)) {
      if (input$tablr_county %in% "King") {
        cnty_filter <- c("King", "King-Pierce", "King-Snohomish")
      } else if (input$tablr_county == "Pierce") {
        cnty_filter <- c("Pierce", "King-Pierce")
      } else if (input$tablr_county == "Snohomish") {
        cnty_filter <- c("Snohomish", "King-Snohomish")
      }
        d <- d %>% filter(County %in% input$tablr_county)
    }

    if (input$tablr_report_type == "Total") {
      t <- d %>% 
        pivot_wider(id_cols = all_of(id_cols),
                    names_from = year)
    } else if (input$tablr_report_type == "Delta"){
      t <- d %>% 
        calc_delta() %>% 
        pivot_wider(id_cols = all_of(id_cols),
                    names_from = year,
                    values_from = delta) %>%
        ungroup()
    } else {
      t <- d %>% 
        calc_delta_share() %>%
        pivot_wider(id_cols = all_of(id_cols),
                    names_from = year,
                    values_from = delta_share) %>% 
        ungroup()%>% 
        mutate(across(where(is.numeric), function(x) ifelse(is.infinite(x), 0, x)))
    }
    
    # create list column Trendline for sparkline htmlwidget
    # t <- t %>% 
    #   mutate(Trendline = pmap(unname(.[,str_subset(colnames(.), "\\d{4}")]), list)) %>% 
    #   select(all_of(id_cols), Trendline, everything())

    if (!(input$tablr_juris %in% c(4, 5)) & (nrow(t) > 0)) {
      footer_name <- switch(input$tablr_juris, 
                            "1"= "Region", 
                            "2" = "Unincorporated Region",
                            "3" = "Incorporated Region"
      )
      
      # add total summary line if county related summary
      sum_cols <- str_subset(colnames(t), "\\d{4}")
      b <- t %>% 
        summarise(across(all_of(sum_cols), sum)) %>% 
        # if summary line includes Trendline
        # mutate(Trendline = pmap(unname(.[,str_subset(colnames(.), "\\d{4}")]), list), Jurisdiction = footer_name)
        mutate(Jurisdiction = footer_name)
      t <- bind_rows(t, b)
    }
    
    return(t)
  })

# Render UI ---------------------------------------------------------------

  
  output$ui_tablr_main_table <- renderUI({
    display_note <- "Estimates other than Total Population are not yet available for years after 2010."
    cities_note <- "Circles denote cities that rank in the top ten of filtered results with 
    hue progressing from dark to light when sorted in decending order for a given year."
    
    crit_a <- any(as.integer(input$tablr_year) > 2010) & (input$tablr_attr != "Total Population")
    crit_b <- all(as.integer(input$tablr_year) > 2010) & (input$tablr_attr != "Total Population")
    
    disp_note_div <- div(p(display_note, class = "note"), class = "note-container")
    city_note_div <- p(cities_note, class = "long-note")
    
    if (input$tablr_juris == 4) {
      
      if (crit_b) {
        disp_note_div
      } else if (crit_a) {
        div(
          div(p(display_note, class = "long-note"), city_note_div, class = "note-container"),
          reactableOutput("tablr_main_table")
        )
      } else {
        div(
          div(p(cities_note, class = "note"), class = "note-container"),
          reactableOutput("tablr_main_table")
        )
      }
      
    } else if (crit_b) {
      disp_note_div
    } else if (crit_a) {
      
      div(
        disp_note_div,
        reactableOutput("tablr_main_table")
      )
      
    } else {
      reactableOutput("tablr_main_table")
    }
  })


# Render ------------------------------------------------------------------

  
  output$tablr_main_table <- renderReactable({
    if (is.null(filter_data())) return(NULL)
    
    t <- filter_data() %>% 
      select(!Filter)

    cols <- str_subset(colnames(t), "\\d{4}")

    if (input$tablr_report_type %in% c("Delta", "Delta Percent")) {
      # # re-name column headers
      new_cols_name <- create_annual_delta_headers(t) 
      t <- t %>% rename(!!!new_cols_name)
      cols <- c(cols[1], str_subset(colnames(t), "\\-"))
    }
    
    if (input$tablr_juris == 4) {
      if (input$tablr_report_type == "Delta Percent") {
        def_col_form <- rt_default_col_def(t, "percent", add_style_top_ten = T)
      } else {
        def_col_form <- rt_default_col_def(t, "number", add_style_top_ten = T)
      }
    } else {
      if (input$tablr_report_type == "Delta Percent") {
        def_col_form <- rt_default_col_def(t, "percent")
      } else {
        def_col_form <- rt_default_col_def(t, "number")
      }
    }
    
    # If Trendline col is included
    # if (input$tablr_report_type != "Total" & (ncol(t) > 4)) {
    #   t <- t %>% select(everything(), -c(4))
    #   cols <- cols[2:length(cols)]
    # } 
    
    if (input$tablr_report_type != "Total" & (ncol(t) > 3)) {
      t <- t %>% select(everything(), -c(3))
      cols <- cols[2:length(cols)]
    }

    if (nrow(t) > 0) {
      reactable(t,
                searchable = T,
                showSortable = T,
                defaultSortOrder = "desc",
                defaultPageSize = 20,
                columnGroups = list(
                  colGroup(name = "Year", columns = cols)),
                defaultColDef = def_col_form,
                columns = rt_specific_col_def(),
                rowStyle = JS("
                function(rowInfo, state) {
                  let d = state.data
                  let e = d[d.length-1];
                  let str = e.Jurisdiction;

                  if (str.includes('Region') & (state.data.length-1 == rowInfo.index)) {
                    return {fontWeight: 'bold'}
                  }
                }")
      )
    }
  })
  
}