# This script will process data and import/export the db onto the data sub-directory

library(tidyverse)
library(DBI)

source("functions/functions.R")

# compile data ----
post_censal_file <-"ofm_april1_population_final.xlsx"
time_periods <- c("2000-2010", "2010-2020")
inter_censal_files <- map(time_periods, ~paste0("ofm_april1_intercensal_estimates_", .x,".xlsx"))

ic <- map(inter_censal_files, ~compile_intercensal_data(.x))
pc <- compile_postcensal_data(post_censal_file)

ic[[1]] <- ic[[1]] |> 
  filter(year != "2010")

df <- bind_rows(ic, pc) |> 
  distinct(Filter, County, Jurisdiction, year, value, attr, .keep_all = TRUE)

# dups <- df |>
#   group_by(Filter, County, Jurisdiction, year, value, attr) |>
#   filter(n() > 1)

# create sqlite db ----

mydb <- dbConnect(RSQLite::SQLite(), "data/data.sqlite")
dbWriteTable(conn = mydb, name = "ofm_estimates", value = df, overwrite = TRUE)
dbListTables(conn = mydb)

dbDisconnect(mydb)
