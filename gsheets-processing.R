# import, tidy, and export BIOL2015 veg and spider data

##### 0. setup workspace #####

library(readxl)
library(writexl)
library(tidyverse) # always last for conflicts

#
##### 1. define importing ranges and sheets #####

# per-plot metadata ranges
rng_plot_id <- "B2"
rng_gps <- "B4"
rng_date <- "B5"
rng_weather <- "B6"

# rngs_metadata <- c(rng_plot_id,
#                    rng_gps,
#                    rng_date,
#                    rng_weather)

# per-plot data ranges
rng_burrows <- "B13:B112"
rng_small_quads <- "E13:I17"
rng_tall_trees <- "L13:P15"
rng_veg_structure <- "S13:T112"

sheet_list <- excel_sheets("data/raw/M2-A1 datasheets (BIOL2015).xlsx")
sheet_list <- sheet_list[sheet_list != "backend"]

#
##### 2. import metadata for each sheet  #####

# use duplicated code in the interests of releasing data on time

dat_plot_id <- map_dfr(.x = sheet_list,
                       .f = function(.x) {
                         read_excel("data/raw/M2-A1 datasheets (BIOL2015).xlsx",
                                    range = rng_plot_id,
                                    sheet = .x,
                                    col_names = c("plot_id"),
                                    na = c("", "N/A")) %>% 
                           mutate(sheet_id = .x)
                       })

dat_gps <- map_dfr(.x = sheet_list,
                   .f = function(.x) {
                     read_excel("data/raw/M2-A1 datasheets (BIOL2015).xlsx",
                                range = rng_gps,
                                sheet = .x,
                                col_names = c("gps"),
                                na = c("", "N/A")) %>% 
                       mutate(sheet_id = .x)
                   })

dat_date <- map_dfr(.x = sheet_list,
                    .f = function(.x) {
                      read_excel("data/raw/M2-A1 datasheets (BIOL2015).xlsx",
                                 range = rng_date,
                                 sheet = .x,
                                 col_names = c("date"),
                                 na = c("", "N/A")) %>% 
                        mutate(sheet_id = .x)
                    })

dat_weather <- map_dfr(.x = sheet_list,
                       .f = function(.x) {
                         read_excel("data/raw/M2-A1 datasheets (BIOL2015).xlsx",
                                    range = rng_weather,
                                    sheet = .x,
                                    col_names = c("weather"),
                                    na = c("", "N/A")) %>% 
                           mutate(sheet_id = .x)
                       })

dat_meta <- tibble(sheet_id = sheet_list) %>% 
  left_join(., dat_plot_id) %>% 
  left_join(., dat_date) %>% 
  left_join(., dat_gps) %>% 
  filter(!is.na(date)) %>% 
  mutate(location = case_when(day(date) == 18 ~ "woodland",
                              day(date) == 19 ~ "pioneer_woodland",
                              day(date) == 20 ~ "mixed_forest"))

#
##### 3. import data for each sheet #####

dat_burrows <- map_dfr(.x = sheet_list,
                       .f = function(.x) {
                         read_excel("data/raw/M2-A1 datasheets (BIOL2015).xlsx",
                                    range = rng_burrows,
                                    sheet = .x,
                                    col_names = c("burrow_width"),
                                    na = c("", "N/A")) %>% 
                           mutate(sheet_id = .x) %>% 
                           filter(!is.na({if("burrow_width" %in% names(.)) burrow_width else NULL}))
                       })

dat_small_quads <- map_dfr(.x = sheet_list,
                           .f = function(.x) {
                             read_excel("data/raw/M2-A1 datasheets (BIOL2015).xlsx",
                                        range = rng_small_quads,
                                        sheet = .x,
                                        col_names = c("quadrat_id",
                                                      "foilage_cover",
                                                      "live_ground_cover",
                                                      "leaf_litter_cover",
                                                      "leaf_litter_decomp"),
                                        na = c("", "N/A")) %>% 
                               mutate(sheet_id = .x,
                                      leaf_litter_decomp = as.character(leaf_litter_decomp),
                                      leaf_litter_decomp = str_split_i(leaf_litter_decomp,
                                                                       " - ",
                                                                       2)) %>% 
                               filter(!is.na({if("foilage_cover" %in% names(.)) foilage_cover else NULL}))
                           }) %>% 
  select(-quadrat_id)

dat_tall_trees <- map_dfr(.x = sheet_list,
                          .f = function(.x) {
                            read_excel("data/raw/M2-A1 datasheets (BIOL2015).xlsx",
                                       range = rng_tall_trees,
                                       sheet = .x,
                                       col_names = c("species",
                                                     "circum",
                                                     "dist_to_tree",
                                                     "eye_hgt",
                                                     "tree_hgt"),
                                       na = c("", "N/A")) %>% 
                              mutate(sheet_id = .x,
                                     species = str_to_lower(as.character(species))) %>% 
                              filter(!is.na({if("species" %in% names(.)) species else NULL}))
                          }) %>% 
  mutate(dbh = round(circum / pi, 2),
         ba = round(pi * (dbh/2)**2 / 10000, 4)) %>% 
  select(-c(circum, dist_to_tree, eye_hgt))

dat_veg_structure <- map_dfr(.x = sheet_list,
                             .f = function(.x) {
                               read_excel("data/raw/M2-A1 datasheets (BIOL2015).xlsx",
                                          range = rng_veg_structure,
                                          sheet = .x,
                                          col_names = c("species",
                                                        "dbh"),
                                          na = c("", "N/A")) %>% 
                                 mutate(sheet_id = .x) %>% 
                                 filter(!is.na({if("species" %in% names(.)) species else NULL}))
                             }) %>% 
  mutate(species = str_to_lower(species),
         ba = round(pi * (dbh/2)**2 / 10000, 4))

#
##### 4. create plot level dataset #####

dat_sum_burrows <- dat_burrows %>% 
  group_by(sheet_id) %>% 
  add_count() %>% 
  summarise(burrow_width = mean(burrow_width,
                                na.rm = TRUE),
            burrow_count = max(n)) %>%
  ungroup()

dat_sum_small_quads <- dat_small_quads %>% 
  group_by(sheet_id) %>%
  summarise(across(c(foilage_cover, 
                     live_ground_cover,
                     leaf_litter_cover),
                   ~mean(., na.rm = TRUE))) %>% 
  ungroup()

dat_sum_tall_trees <- dat_tall_trees %>% 
  group_by(sheet_id) %>%
  summarise(max_tall_tree_dbh = max(dbh,
                                    na.rm = TRUE),
            max_tall_tree_ba = max(ba,
                                   na.rm = TRUE),
            max_tall_tree_hgt = max(tree_hgt)) %>% 
  ungroup() %>% 
  filter(!is.infinite(max_tall_tree_dbh))

dat_sum_veg_structure <- dat_veg_structure %>% 
  group_by(sheet_id) %>% 
  add_count() %>% 
  summarise(dbh = round(mean(dbh,
                             na.rm = TRUE), 2),
            ba = round(mean(ba,
                            na.rm = TRUE), 4),
            stem_count = max(n)) %>%
  ungroup()

dat_plot_level<- dat_meta %>% 
  left_join(., dat_sum_burrows) %>% 
  left_join(., dat_sum_small_quads) %>% 
  left_join(., dat_sum_tall_trees) %>% 
  left_join(., dat_sum_veg_structure) %>% 
  replace_na(list(burrow_count = 0)) %>% 
  mutate(date = date(date))

#
##### 5. export plot level dataset #####

dat_meta_labels_point <- tibble(
  variable_name = c("sheet_id",
                    "plot_id",
                    "date",
                    "gps",
                    "location",
                    "burrow_width",
                    "burrow_count",
                    "foliage_cover",
                    "live_ground_cover",
                    "leaf_litter_cover",
                    "max_tall_tree_dbh",
                    "max_tall_tree_ba",
                    "max_tall_tree_hgt",
                    "dbh",
                    "ba",
                    "stem_count"),
  comment = c("sequential sheet labels from the Google Sheet",
              "your plot labels (unreliable)",
              "",
              "your plot coordinates (unreliable)",
              "type of sample site, determined using the date",
              "",
              "",
              "foilage projected cover, per densiometer dots",
              "",
              "",
              "",
              "",
              "",
              "diameter at breast height",
              "basal area",
              ""),
  units = c("",
            "",
            "",
            "lat, lon",
            "",
            "mean, mm",
            "",
            "mean, %",
            "mean, %",
            "mean, %",
            "cm",
            "m^2",
            "m",
            "mean, cm",
            "mean, m^2",
            ""))

write_xlsx(list(metadata = dat_meta_labels_point,
                "plot-level" = dat_plot_level),
           "data/processed/BIOL2015-M2-2024_plot-level.xlsx",
           format_headers = FALSE)

#
##### 6. collate and export observation level dataset #####

dat_meta_labels_obs <- tibble(
  variable_name = c("sheet_id",
                    "gps",
                    "location",
                    "burrow_width",
                    "foliage_cover",
                    "live_ground_cover",
                    "leaf_litter_cover",
                    "leaf_litter_decomp",
                    "species",
                    "tree_hgt",
                    "dbh",
                    "ba"),
  comment = c("sequential sheet labels from the Google Sheet",
              "your plot coordinates (unreliable)",
              "type of sample site, determined using the date",
              "",
              "foilage projected cover, per densiometer dots",
              "",
              "",
              "",
              "",
              "",
              "diameter at breast height",
              "basal area"),
  units = c("",
            "lat, lon",
            "",
            "mm",
            "%",
            "%",
            "%",
            "",
            "",
            "m",
            "cm",
            "m^2"))

dat_meta_trim <- dat_meta %>% 
  select(sheet_id, gps, location)

write_xlsx(list(
  metadata = dat_meta_labels_obs,
  "burrows" = dat_burrows %>% 
    left_join(., dat_meta_trim) %>% 
    relocate(sheet_id, gps, location),
  "small-quadrats" = dat_small_quads  %>% 
    left_join(., dat_meta_trim) %>% 
    relocate(sheet_id, gps, location),
  "tall-trees" = dat_tall_trees %>% 
    left_join(., dat_meta_trim) %>% 
    relocate(sheet_id, gps, location),
  "veg-structure" = dat_veg_structure %>% 
    left_join(., dat_meta_trim) %>% 
    relocate(sheet_id, gps, location)),
  "data/processed/BIOL2015-M2-2024_obs-level.xlsx",
  format_headers = FALSE)

#