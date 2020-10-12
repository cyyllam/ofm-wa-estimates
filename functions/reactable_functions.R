library(reactable)
library(sparkline)

rt_specific_col_def <- function() {
  custom_colDef <- list(
    County = colDef(
      defaultSortOrder = "asc",
      align = "left",
      style = list(position = "sticky", left = 0, background = "#fff", zIndex = 1),
      headerStyle = list(position = "sticky", left = 0, background = "#fff", zIndex = 1)
                    ),
    Jurisdiction = colDef(
      defaultSortOrder = "asc",
      align = "left", 
      minWidth = 150,
      style = list(position = "sticky", left = 100, background = "#fff", zIndex = 1),
      headerStyle = list(position = "sticky", left = 100, background = "#fff", zIndex = 1)
                         ),
    Trendline = colDef(
      sortable = F,
      align = 'left',
      style = list(position = "sticky", left = 250, background = "#fff", zIndex = 1),
      headerStyle = list(position = "sticky", left = 250, background = "#fff", zIndex = 1),
      cell = function(values) {
        sparkline(values, type = "bar")
      }
    )
  )
}

make_color_pal <- function(colors, bias = 1) {
  get_color <- colorRamp(colors, bias = bias)
  function(x) rgb(get_color(x), maxColorValue = 255)
}

top_ten_color <- make_color_pal(c("#ffe4b2","#ffc966", "#e59400"), bias = 5)

rt_default_col_def <- function(table, format_type, add_style_top_ten = F) {
  if (format_type == "number") {
    frmt <- colFormat(separators = T)
  } else if (format_type == "percent") {
    frmt <- colFormat(percent = TRUE, digits = 1)
  }
  
  if (add_style_top_ten == T) {
    return(colDef(align = 'center',
                  cell = function(value, index, name) {
                    x <- head(sort(unlist(table[[name]]), decreasing = T), 10)
                    
                    if (format_type == "number") {
                      frmt <- format(value, big.mark=",")
                    } else if (format_type == "percent") {
                      frmt <-  suppressWarnings(formatC(paste0(round(as.numeric(value) * 100, 1), "%")))
                    }
                    
                    if (is.numeric(value) && value != 0 && value %in% x) {
                      normalized <- (value - min(x)) / (max(x) - min(x))
                      color <- top_ten_color(normalized)
                      div(class = "sub-cell", style = list(background = color), frmt)
                    } else if (is.numeric(value)){
                      frmt
                    } else {
                      value
                    }
                  }
                  )
           )
  } else {
    return(colDef(format = frmt))
  }
}
