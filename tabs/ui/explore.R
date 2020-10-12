tab_files <- list.files(path = "tabs/ui/explore", full.names = T)
suppressMessages(lapply(tab_files, source))

explore <- tabPanel(
  title = "Explore",
  value = "explore",
  column(
    width = 12,
    tabsetPanel(
      tab_tabular
    ) # end tabsetPanel
  ) # end column
) # end tabPanel