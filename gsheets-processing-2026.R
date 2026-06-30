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

read_grid <- map_dfr(file_list, function(.f) {
  tibble(file_path = .f,
    sheet_id  = excel_sheets(.f))
}) %>% 
  mutate(day_group = basename(file_path) %>% 
      tools::file_path_sans_ext() %>% 
      str_remove("^M2_") %>% 
      str_remove("_BIOL2015_2026$"),
    team     = str_extract(sheet_id, "team \\d+"),
    site     = str_extract(sheet_id, "site \\d+"),
    veg_type = case_when(site == "site 1" ~ "1_pioneer_woodland",
      site == "site 2" ~ "2_woodland",
      site == "site 3" ~ "3_mixed_forest"),
    plot_id_unique = str_c(veg_type, "_", day_group, "_", str_replace(team, " ", "")))



# updated function if each group samples all 3 sites
dat_plot_id <- pmap_dfr(read_grid, function(file_path, sheet_id, plot_id_unique, ...) {
  read_excel(file_path, range = rng_plot_id, sheet = sheet_id,
    col_names = c("plot_id"), na = c("", "N/A")) %>% 
    mutate(plot_id_unique = plot_id_unique)
})

dat_date <- pmap_dfr(read_grid, function(file_path, sheet_id, plot_id_unique, ...) {
  read_excel(file_path, range = rng_date, sheet = sheet_id,
    col_names = c("date"), na = c("", "N/A")) %>% 
    mutate(plot_id_unique = plot_id_unique)
})

dat_weather <- pmap_dfr(read_grid, function(file_path, sheet_id, plot_id_unique, ...) {
  read_excel(file_path, range = rng_weather, sheet = sheet_id,
    col_names = c("weather"), na = c("", "N/A")) %>% 
    mutate(plot_id_unique = plot_id_unique)
})





##### 3. import data for each sheet #####
dat_burrows <- pmap_dfr(read_grid, function(file_path, sheet_id, plot_id_unique, ...) {
  read_excel(file_path, range = rng_burrows, sheet = sheet_id,
    col_names = c("burrow_width"), na = c("", "N/A")) %>% 
    mutate(plot_id_unique = plot_id_unique) %>% 
    filter(!is.na({if("burrow_width" %in% names(.)) burrow_width else NULL}))
})




dat_small_quads <- pmap_dfr(read_grid, function(file_path, sheet_id, plot_id_unique, ...) {
  read_excel(file_path, range = rng_small_quads, sheet = sheet_id,
    col_names = c("quadrat_id", "foilage_cover", "live_ground_cover",
      "leaf_litter_cover", "leaf_litter_decomp"),
    na = c("", "N/A")) %>% 
    mutate(plot_id_unique = plot_id_unique,
      leaf_litter_decomp = as.character(leaf_litter_decomp),
      leaf_litter_decomp = str_split_i(leaf_litter_decomp, " - ", 2)) %>% 
    filter(!is.na({if("foilage_cover" %in% names(.)) foilage_cover else NULL}))
}) %>% 
  select(-quadrat_id)

dat_tall_trees <- pmap_dfr(read_grid, function(file_path, sheet_id, plot_id_unique, ...) {
  read_excel(file_path, range = rng_tall_trees, sheet = sheet_id,
    col_names = c("species", "circum", "dist_to_tree", "eye_hgt", "tree_hgt"),
    na = c("", "N/A")) %>% 
    mutate(plot_id_unique = plot_id_unique,
      species = str_to_lower(as.character(species))) %>% 
    filter(!is.na({if("species" %in% names(.)) species else NULL}))
}) %>% 
  mutate(species = case_when(str_detect(species, "lopho") ~ loco_correct_spelling,
    str_detect(species, "lophst") ~ loco_correct_spelling,
    .default = species)) %>% 
  select(-c(circum, dist_to_tree, eye_hgt))

dat_veg_structure <- pmap_dfr(read_grid, function(file_path, sheet_id, plot_id_unique, ...) {
  read_excel(file_path, range = rng_veg_structure, sheet = sheet_id,
    col_names = c("species", "dbh"), na = c("", "N/A")) %>% 
    mutate(plot_id_unique = plot_id_unique) %>% 
    filter(!is.na({if("species" %in% names(.)) species else NULL}))
}) %>% 
  mutate(species = str_to_lower(species),
    species = case_when(str_detect(species, "lopho") ~ loco_correct_spelling,
      str_detect(species, "lophst") ~ loco_correct_spelling,
      .default = species),
    ba = round(pi * (dbh/2)**2 / 10000, 4))

##### meta + summaries, all joining on plot_id_unique #####
# get all metadata together
dat_meta <- read_grid %>% 
  select(plot_id_unique, veg_type) %>%    # <- veg_type comes in here
  left_join(dat_plot_id, by = "plot_id_unique") %>% 
  left_join(dat_date,    by = "plot_id_unique") %>% 
  filter(!is.na(date)) %>% 
  mutate(date = as.Date(date),    # strip UTC time -> plain yyyy-mm-dd
    gps = case_when(
      veg_type == "1_pioneer_woodland" ~ "-25.5980, 153.0896",
      veg_type == "2_woodland"          ~ "-25.5890, 153.0839",
      veg_type == "3_mixed_forest"      ~ "-25.5668, 153.0701"))




# burrow summary per plot
dat_sum_burrows <- dat_burrows %>% 
  group_by(plot_id_unique) %>% 
  add_count() %>% 
  summarise(mean_burrow_width = mean(burrow_width, na.rm = TRUE),
    min_burrow_width  = min(burrow_width, na.rm = TRUE),
    max_burrow_width  = max(burrow_width, na.rm = TRUE),
    burrow_count = max(n)) %>%
  ungroup()


dat_sum_small_quads <- dat_small_quads %>% 
  group_by(plot_id_unique) %>%
  summarise(across(c(foilage_cover, live_ground_cover, leaf_litter_cover),
    ~ mean(., na.rm = TRUE),
    .names = "mean_{.col}")) %>% 
  ungroup()

dat_sum_tall_trees <- dat_tall_trees %>% 
  group_by(plot_id_unique) %>%
  summarise(max_tall_tree_hgt = max(tree_hgt),
    mean_tree_height = mean(tree_hgt)) %>% 
  ungroup() %>% 
  filter(!is.infinite(max_tall_tree_hgt))

dat_sum_veg_structure <- dat_veg_structure %>% 
  group_by(plot_id_unique) %>% 
  add_count() %>% 
  summarise(mean_dbh = round(mean(dbh, na.rm = TRUE), 2),
    mean_ba  = round(mean(ba, na.rm = TRUE), 4),
    stem_count = max(n)) %>% 
  ungroup() %>%
  mutate(ba_per_ha = round(mean_ba * (stem_count * 100), 2))

dat_plot_level <- dat_meta %>% 
  left_join(dat_sum_burrows,       by = "plot_id_unique") %>% 
  left_join(dat_sum_small_quads,   by = "plot_id_unique") %>% 
  left_join(dat_sum_tall_trees,    by = "plot_id_unique") %>% 
  left_join(dat_sum_veg_structure, by = "plot_id_unique") %>% 
  replace_na(list(burrow_count = 0)) %>% 
  mutate(date = date(date))






##### 5. export plot level dataset #####
dat_meta_labels_point <- tibble(
  variable_name = c("plot_id_unique",
    "veg_type",
    "mean_burrow_width",
    "min_burrow_width",
    "max_burrow_width",
    "burrow_count",
    "mean_foilage_cover",
    "mean_live_ground_cover",
    "mean_leaf_litter_cover",
    # "max_tall_tree_dbh",
    # "max_tall_tree_ba",
    "max_tall_tree_hgt",
    "mean_dbh",
    "mean_ba",
    "stem_count",
    "ba_per_ha"),
  comment = c("unique plot label: veg type, day group, and team",
    "type of sample site, per course handbook; initial number shows order in which they were encountered on the hike",
    "",
    "",
    "",
    "",
    "foilage projected cover, per densiometer dots",
    "",
    "",
    # "",
    # "",
    "",
    "diameter at breast height",
    "average basal area in the plot",
    "",
    "basal area per hectare (extrapolated from your plot)"),
  units = c("",
    "",
    "mean, mm",
    "min, mm",
    "max, mm",
    "#",
    "mean, %",
    "mean, %",
    "mean, %",
    # "cm",
    # "m^2",
    "m",
    "mean, cm",
    "mean, m^2",
    "# per plot",
    "m^2 per ha"))


write_xlsx(list(metadata = dat_meta_labels_point,
  "plot-level" = dat_plot_level),
  "data/2026_data/processed/BIOL2015-M2-2026_plot-level.xlsx",
  format_headers = FALSE)


##### 6. collate and export observation level dataset #####
# raw data for students to use

dat_meta_labels_obs <- tibble(
  variable_name = c("plot_id_unique",
                    "gps",
                    "veg_type",
                    "burrow_width",
                    "foilage_cover",
                    "live_ground_cover",
                    "leaf_litter_cover",
                    "leaf_litter_decomp",
                    "species",
                    "tree_hgt",
                    "dbh",
                    "ba"),
  comment = c("unique plot label: veg type, day group, and team",
              "standardised plot coordinates",
              "type of sample site, per course handbook; intial number shows order in which they were encountered on the hike",
              "",
              "foilage projected cover",
              "",
              "",
              "See fieldbook for classification definitions",
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



write_xlsx(list(
  metadata = dat_meta_labels_obs,
  "burrows" = dat_burrows %>% 
    left_join(dat_meta, by = "plot_id_unique") %>% 
    relocate(plot_id_unique, gps, veg_type),
  "small-quadrats" = dat_small_quads %>% 
    left_join(dat_meta, by = "plot_id_unique") %>% 
    relocate(plot_id_unique, gps, veg_type),
  "tall-trees" = dat_tall_trees %>% 
    left_join(dat_meta, by = "plot_id_unique") %>% 
    relocate(plot_id_unique, gps, veg_type),
  "veg-structure" = dat_veg_structure %>% 
    left_join(dat_meta, by = "plot_id_unique") %>% 
    relocate(plot_id_unique, gps, veg_type)),
  "data/2026_data/processed/BIOL2015-M2-2026_obs-level.xlsx",
  format_headers = FALSE)
