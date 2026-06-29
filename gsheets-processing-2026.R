# import, tidy, and export BIOL2015 veg and spider data
# FOR 2026 updates with separate datasheets per day
##### 0. setup workspace #####

library(readxl)
library(writexl)
library(tidyverse) # always last for conflicts

# used to correct remarkable misspellings for this species
loco_correct_spelling <- "lophostemon confertus"

#
##### 1. define importing ranges and sheets #####

# per-plot metadata ranges
rng_plot_id <- "B2"
# rng_gps <- "B4"
rng_date <- "B5"
rng_weather <- "B6"

# per-plot data ranges
rng_burrows <- "B13:B112"
rng_small_quads <- "E13:I17"
rng_tall_trees <- "L13:P15"
rng_veg_structure <- "S13:T112"



# make a filepath to the datasheets
file_list <- list.files("data/2026_data/raw", pattern = "\\.xlsx$", full.names = TRUE)

# for each file, get its sheet/tab names, build a file × tab table
read_grid <- map_dfr(file_list, function(.f) {
  tibble(file_path = .f,
    sheet_id  = excel_sheets(.f))
})


# updated function if each group samples all 3 sites.

# list the 3 datasheet files (downloaded from g-drive)

dat_plot_id <- pmap_dfr(read_grid, function(file_path, sheet_id) {
  read_excel(file_path,
    range = rng_plot_id,
    sheet = sheet_id,
    col_names = c("plot_id"),
    na = c("", "N/A")) %>% 
    mutate(day_group = basename(file_path) %>% 
        tools::file_path_sans_ext() %>% 
        str_remove("^M2_") %>% 
        str_remove("_BIOL2015_2026$"),
      sheet_id  = sheet_id,
      team      = str_extract(sheet_id, "team \\d+"),
      site      = str_extract(sheet_id, "site \\d+"),
      veg_type  = case_when(site == "site 1" ~ "1_pioneer_woodland",
        site == "site 2" ~ "2_woodland",
        site == "site 3" ~ "3_mixed_forest"),
      plot_id_unique = str_c(veg_type, "_",  day_group, "_", str_replace(team, " ", "")))
})



dat_date <- pmap_dfr(read_grid, function(file_path, sheet_id) {
  read_excel(file_path,
    range = rng_date,
    sheet = sheet_id,
    col_names = c("date"),
    na = c("", "N/A")) %>% 
    mutate(day_group = basename(file_path) %>% 
        tools::file_path_sans_ext() %>% 
        str_remove("^M2_") %>% 
        str_remove("_BIOL2015_2026$"),
      sheet_id = sheet_id)
})