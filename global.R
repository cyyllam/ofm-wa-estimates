library(shiny)
library(bslib)
library(RSQLite)

source_files <- list.files(path = "functions", full.names = T, recursive = T)
suppressMessages(lapply(source_files, source))

# generate data
conn <- dbConnect(SQLite(), "data/data.sqlite")
df <- dbReadTable(conn, 'ofm_estimates')

# run all files in the modules sub-directory
module_files <- list.files('modules', full.names = TRUE)
sapply(module_files, source)

