library(shiny)

source_files <- list.files(path = "functions", full.names = T, recursive = T)
suppressMessages(lapply(source_files, source))

# generate data
post_censal_file <-"ofm_april1_population_final.xlsx"
inter_censal_files <- "ofm_april1_intercensal_estimates_2000-2010.xlsx"

df <- bind_inter_post_censal_data(post_censal_file, inter_censal_files)

# source tab files
tab_files <- list.files(path = "tabs", full.names = T, recursive = T)
tab_files <- tab_files[-grep(x = tab_files, pattern = "server")] # removes those in server dir

suppressMessages(lapply(tab_files, source))
