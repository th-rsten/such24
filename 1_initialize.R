# set up ------------------------------------------------------------------
library(tidyverse)
library(sf)
library(httr)
library(data.table)
library(mapview)
library(furrr)
library(beepr)
library(TSP)
# library(fst)

workd = "C:/Users/hrzd/Downloads/" #"/Users/opsadmin/Downloads

cp_file = paste0(workd,"such24_cps_20240415.csv")

cp_table = read_csv(cp_file)

# generate possible combinations of skipped and not-skipped CPs
5*5*3*3*3*6*4
cp_table |> filter(!is.na(group)) |> count(group) |> pull(n) |> magrittr::add(1) |> prod() 

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
    !is.na(skip4), # at least BS or SH are easily skipable
    !is.na(skip5),
    !is.na(skip7), # at least GE is easily skipable
    !(skip6 == "VD" & skip7 != "GE")) # have to skip GE when skipping VD


expand_grid(from = cp_table$id[-6:0],
            to = cp_table$id[-6:0]) %>%
  filter(from != to,
         from != "EN")

# load points -------------------------------------------------------------
points = read_sf(point_file) %>%
  select(name = ends_with("text")) %>%
  filter(!is.na(name)) %>%
  mutate(canton = str_sub(name, 1, 2),
         no = str_sub(name, 3, 3),
         x = sf::st_coordinates(.)[,1],
         y = sf::st_coordinates(.)[,2])

skippable_cantons = points$canton[!points$canton %in% c("EP","CP")] %>%
  unique()

# plot points -------------------------------------------------------------

# set up realistic point pairs --------------------------------------------
dis_mat_test =  st_distance(points, points)
hist(dis_mat_test/1000)

point_combinations = expand_grid(from = cp_table$id,
                                 to = cp_table$id) %>%
  filter(from != to,
         from != "EN")

border_pairs = point_combinations %>%
  filter(!str_sub(one,1,2) %in% c("CP", "EP"),
         !str_sub(two,1,2) %in% c("CP", "EP")) %>%
  mutate(index_one = map_int(one, ~which(points$name == .x)),
         index_two =  map_int(two, ~which(points$name == .x)),
         distance = map2_dbl(index_one, index_two, ~dis_mat_test[.x,.y])) %>%
  filter(distance < 1000, one != "ZG1", two != "ZG1") %>%
  select(one, two) %>%
  arrange(str_sub(one,1,2), str_sub(two,1,2), one, two)

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
