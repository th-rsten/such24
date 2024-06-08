# set up ------------------------------------------------------------------
library(tidyverse)
library(here)
library(sf)
library(httr)
library(data.table)
library(mapview)
library(furrr)
library(beepr)
library(TSP)
library(geodata)
# library(fst)
here::i_am("1_initialize.R")
brouter_segments_dir =  here("geodata", "brouter_segments")
cp_file = max(list.files(here(),"such24_cps_")) # get latest CP table file

# get basic geodata -------------------------------------------------------

# loading country and canton borders, file modified from swissBOUNDARIES3D
ch_borders = st_read(here("geodata", "ch_cantons.gpkg")) 
plot(ch_borders, graticule = TRUE, axes = TRUE) # overview

# get all necessary osm segments for brouter ------------------------------

download_dir = "https://brouter.de/brouter/segments4/" # direct
rough_bbox = floor(st_bbox(ch_borders))
necessary_brouter_segments = expand_grid(
  x = (rough_bbox$xmin/5):(rough_bbox$xmax/5)*5,
  y = (rough_bbox$ymin/5):(rough_bbox$ymax/5)*5) %>%
  filter(x != 10) |> 
  mutate(dl_url = glue::glue("{download_dir}E{x}_N{y}.rd5"))

if(!dir.exists(brouter_segments_dir)) dir.create(brouter_segments_dir)

# walk(necessary_brouter_segments$dl_url,
#      download.file, method = "curl", 
#      destfile = brouter_segments_dir)


# load CPs ----------------------------------------------------------------

# load table of CP info
cp_table = read_csv(cp_file)
cp_table

# generate possible combinations of skipped and not-skipped CPs
5*5*3*3*3*6*4 # number of combinations
cp_table |> 
  filter(!is.na(group)) |>
  count(group) |> 
  pull(n) |> 
  magrittr::add(1) |>
  prod() 

# all percievalbe combinations

skip_combinations = cp_table |> 
  filter(!is.na(group)) |> 
  group_by(group) |> 
  summarize(ids = list(id)) |>
  pull(ids) |> 
  map(c,"NA") |> 
  expand.grid() |> 
  as_tibble() |> 
  set_names(paste0("skip",1:7)) |> 
  filter(
    # skip4 != "NA", # at least BS or SH are easily skipable
    # skip5 == "NA",
    # skip7 != "NA", # at least GE is easily skipable
    !(skip6 == "VD" & skip7 != "GE"), # have to skip GE when skipping VD
    !(skip2 == "ZH" & skip4 != "SH")) |> # have to skip SH when skipping ZH
  mutate(across(everything(), na_if, "NA")) |> 
  rowwise() |> 
  mutate(skip_n = sum(!is.na(c_across(everything())))) |> 
  ungroup() 

skip_combinations |> count(skip_n)

# CPs as points -------------------------------------------------------------
cp_sf = cp_table %>%
  st_as_sf(coords = c("lon", "lat")) |> 
  st_set_crs(4326)

# generating shapes to check for routes intersecting ----------------------

# shape of swiss border
ch_border = ch_borders |> 
  sf::st_union() |>
  st_cast('MULTILINESTRING')

# shapes of skippable cantons
skippable_cantons = ch_borders |> 
  st_join(cp_sf) |> 
  filter(!is.na(group)) |> 
  select(id, group)

# plot cantons and CPs
mapview(ch_borders, col.regions = "grey", legend = FALSE) +
  mapview(list(skippable_cantons,cp_sf), 
          zcol = "group", 
          legend = c(FALSE, TRUE))

# set up realistic point pairs --------------------------------------------
dis_mat_test =  st_distance(cp_sf, cp_sf)
hist(dis_mat_test/1000)

point_combinations = expand_grid(from = cp_table$id,
                                 to = cp_table$id) %>%
  filter(from != to,
         from != "EN")

all_routing_scenarios = expand_grid(point_combinations, skip_combinations) |> 
  mutate(across(3:9, coalesce, "NA")) |> 
  filter(from != skip1, from != skip2, from != skip3, from != skip4,
         from != skip5, from != skip6, from != skip7,
         to != skip1, to != skip2, to != skip3, to != skip4,
         to != skip5, to != skip6, to != skip7) |> 
  mutate(across(3:9, na_if, "NA"))

# generate realistic point pools ------------------------------------------
# point_pool_test = points %>%
#   st_drop_geometry() %>%
#   select(-no) %>%
#   filter(!canton %in% c("EP", "CP"),
#          # only the ones to filter for border points for now
#          !canton %in% c("TG", "JU", "NE", "OW","AI","AR"),
#          # filtering low priority points
#          !name %in% c("BS1","GL3")) %>%
#   group_by(canton) %>%
#   summarize(points = list(unique(name))) %>%
#   as.list()

# set_names(point_pool_test$points, point_pool_test$canton) %>%
#   lengths() %>% prod()

# # generate all combinations, CAREFUL: needs RAM!
# pool_test2 = set_names(point_pool_test$points, point_pool_test$canton) %>%
#   do.call(data.table::CJ, .) %>%
#   mutate_all(as_factor)
