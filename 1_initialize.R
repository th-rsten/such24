# set up ------------------------------------------------------------------
library(tidyverse)
library(sf)
library(httr)
library(data.table)
library(mapview)
library(furrr)
library(beepr)
library(TSP)
library(geodata)
# library(fst)


workd = "C:/Users/hrzd/Downloads/" #"/Users/opsadmin/Downloads
geodata_dir = "./geodata"
brouter_segments_dir = "./geodata/brouter_segments/"

cp_file = paste0(workd,"such24_cps_202404181.csv")


# get basic geodata -------------------------------------------------------

# getting country and canton borders
ch_borders = gadm("switzerland", path = geodata_dir) |>
  st_as_sf() |> 
  transmute(id = str_sub(HASC_1, 4,5), geometry) # canton ids like in the table
plot(ch_borders, graticule = TRUE, axes = TRUE)

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
  map(c,NA) |> 
  expand.grid() |> 
  as_tibble() |> 
  set_names(paste0("skip",1:7)) |> 
  filter(
    # !is.na(skip4), # at least BS or SH are easily skipable
    # !is.na(skip5),
    # !is.na(skip7), # at least GE is easily skipable
    !(skip6 == "VD" & skip7 != "GE"), # have to skip GE when skipping VD
    !(skip2 == "ZH" & skip4 != "SH")) # have to skip SH when skipping ZH 

# load points -------------------------------------------------------------
cp_sf = cp_table %>%
  st_as_sf(coords = c("lon", "lat")) |> 
  st_set_crs(4326)

# plot cantons and CPs
mapview(list(ch_borders,cp_sf))

# generating shapes to check for routes intersecting ----------------------

# shape of swiss border
ch_border = ch_borders |> 
  sf::st_union() |>
  st_cast('MULTILINESTRING')

# shapes of skippable cantons
skippable_cantons = ch_borders |> 
  left_join(cp_table) |> 
  filter(!is.na(group)) |> 
  select(id)

# set up realistic point pairs --------------------------------------------
dis_mat_test =  st_distance(cp_sf, cp_sf)
hist(dis_mat_test/1000)

point_combinations = expand_grid(from = cp_table$id,
                                 to = cp_table$id) %>%
  filter(from != to,
         from != "EN")

border_pairs = point_combinations %>%
  #  mutate(index_from = map_int(from, ~which(points$name == .x)),
  #        index_to =  map_int(to, ~which(points$name == .x)),
  #        distance = map2_dbl(index_from, index_to, ~dis_mat_test[.x,.y])) %>%
  # filter(distance < 1000) %>%
  select(from, to) |> 
  left_join(select(cp))

# generate realistic point pools ------------------------------------------
point_pool_test = points %>%
  st_drop_geometry() %>%
  select(-no) %>%
  filter(!canton %in% c("EP", "CP"),
         # only the ones to filter for border points for now
         !canton %in% c("TG", "JU", "NE", "OW","AI","AR"),
         # filtering low priority points
         !name %in% c("BS1","GL3")) %>%
  group_by(canton) %>%
  summarize(points = list(unique(name))) %>%
  as.list()

set_names(point_pool_test$points, point_pool_test$canton) %>%
  lengths() %>% prod()

# generate all combinations, CAREFUL: needs RAM!
pool_test2 = set_names(point_pool_test$points, point_pool_test$canton) %>%
  do.call(data.table::CJ, .) %>%
  mutate_all(as_factor)
