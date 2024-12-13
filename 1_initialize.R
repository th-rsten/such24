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
library(tictoc)
library(fst)
here::i_am("1_initialize.R")
options(timeout=10000)
brouter_segments_dir =  here("geodata", "brouter_segments")
cp_file = max(list.files(here(),"such24_cps_.*.csv")) # get latest CP table file

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
4*5*3*3*3*6*4 # number of combinations
cp_table |> 
  filter(!is.na(group)) |>
  count(group) |> 
  pull(n) |> 
  magrittr::add(1) |>
  prod() 

# all perceivable combinations
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
    !(skip6 == "VD" & skip7 != "GE"), # have to skip GE when skipping VD
    !(skip2 == "ZH" & skip4 != "SH"),  # have to skip SH when skipping ZH
    !(skip1 == "OW" & skip2 == "LU"),   # have to have some entry to NW
    !(skip1 == "UR" & skip6 == "GR" & skip7 == "VS"), # blocking off TI
    !(skip1 == "UR" & skip2 == "LU" & skip6 == "AG" & skip7 == "VS"), # blocking
     skip4 != "NA", # at least BS or SH are skipable for no trade off
    # skip5 == "NA",
     skip7 != "NA"#, # at least GE is skipable for no trade off
   ) |>
  mutate(across(everything(),  ~ na_if(.x, "NA"))) |> 
  rowwise() |> 
  mutate(skip_n = sum(!is.na(c_across(everything())))) |> 
  ungroup() 

skip_combinations |> count(skip_n)

# set up realistic point pairs --------------------------------------------
point_combinations = expand_grid(from = cp_table$id,
                                 to = cp_table$id) %>%
  filter(from != to,
         from != "EN")
  
all_routing_scenarios = expand_grid(point_combinations, skip_combinations) |> 
  mutate(across(3:9, ~coalesce(.x, "NA"))) |> 
  filter(from != skip1, from != skip2, from != skip3, from != skip4,
         from != skip5, from != skip6, from != skip7,
         to != skip1, to != skip2, to != skip3, to != skip4,
         to != skip5, to != skip6, to != skip7) |> 
  mutate(across(3:9, na_if, "NA"),
         across(1:9, as.factor),
         id = as.character(NA)) |> 
  arrange(skip_n, from, to)

# CPs as points -------------------------------------------------------------
cp_sf = cp_table %>%
  st_as_sf(coords = c("lon", "lat")) |> 
  st_set_crs(4326)

# store updated CP shape
cp_sf_file = here(str_replace(cp_file,"csv", "geojson"))
if(!file.exists(cp_sf_file)) write_sf(cp_sf, cp_sf_file)

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


# prepare routing API calls -----------------------------------------------

# load in custom no-go zones to avoid leaving the country
nogos_base = read_file("border_nogos.txt") |> 
  str_replace_all("\\;", "\\|")
# load custom no-go zones for every skippable canton
nogos_cantons = cp_table |> 
  filter(!is.na(group)) |> # is skippable
  select(id, nogo_zones) |> 
  mutate(nogo_zones = str_replace_all(nogo_zones, "\\;", "\\|"))

cp_coords = select(cp_table, id, lon, lat)

brouter_profile = "such" # set brouter profile
brouter_hostname = "http://localhost:17777" # set brouter hostname
api_url = paste0(brouter_hostname, # base brouter API url
                 "/brouter?profile=", brouter_profile,
                 "&alternativeidx=0&format=geojson&lonlats=")

# create segments folder if missing
if(!dir.exists(here("segments"))) dir.create(here("segments"))

first_cols = c("id", "from", "to", "skip", "xing", "xing_ch_border",
               "distance", "ascend", "time", "energy",
               "cost")
#  "track-length", "filtered ascend", "total-time", "total-energy",

# test data ---------------------------------------------------------------
# 
#  route_no = 1 # 614 # ge-AI
# from = point_routes$from[route_no]
# to = point_routes$tp[route_no]
# lonfrom = point_routes$lonfrom[route_no]
# latfrom  = point_routes$latfrom[route_no]
# lonto  = point_routes$lonto[route_no]
# latto  = point_routes$latto[route_no]
# skip =  point_routes$skip[route_no]# sort(c(NA, NA, "ZH", "SH", "BS", NA, NA))

# from = "UR"
# to = "ZG"
# lonfrom = 8.572907 
# latfrom =46.96101 
# lonto = 8.504339
# latto = 47.17294 
# skip = "NW"
# table_file = "segments_1.csv"

# function to get route and save result as numbers and shape --------

get_route = function(from, to, lonfrom, latfrom, lonto, latto,
                     skip = "", table_file = "segments_1.csv") {
  # api_url2 = "6.126165,46.317771|7.622538,47.547481"
  
  # combine no-go zones of all cantons to be skipped + general zones
  skipped_cantons = str_replace_all(skip, "-", "\\|") |> str_replace("^$", " ")
  nogos_combo = nogos_cantons |> 
    filter(str_detect(id, skipped_cantons)) |>
    pull("nogo_zones") |> 
    na.omit() |> 
    paste0(collapse = "|")
  nogos_url = paste0("&nogos=", nogos_base, "|", nogos_combo)
  
  # call BRouter API for route
  lonlats = paste0(lonfrom, ",", latfrom, "|", lonto, ",", latto)
  api_response = try(
    # jsonlite::fromJSON(txt = paste0(api_url, lonlats, nogos_url)),
    jsonlite::fromJSON(
      content(
        GET(paste0(api_url, lonlats, nogos_url),timeout(300)),
        as = "text")
      ),
    silent = TRUE
  )
  
  # abort when API gives error
  if(class(api_response) == "try-error") return(NULL)
  
  # extract shape of route
  linestring = try(
    st_linestring(api_response$features$geometry$coordinates[[1]]),
    silent = TRUE)
  
  # if shape is faulty, try fixing it by filling holes
  if(any(class(linestring) == "try-error", na.rm = TRUE)) {
    geo_to_fix = api_response$features$geometry$coordinates[[1]]
    fix_line = lengths(geo_to_fix) == 2
    cat(from, to, sum(fix_line), "\n")
    geo_to_fix[fix_line] <- lapply(geo_to_fix[fix_line], c,NA)
    linestring = geo_to_fix %>% 
      unlist() %>% 
      matrix(ncol = 3, byrow = TRUE) %>% 
      as.data.frame() %>%
      fill(V3, .direction = "downup") %>% 
      as.matrix() %>% 
      st_linestring()
  }
  
  # check if route leaves the country
  xing_ch_border = linestring |> 
    st_zm() |> 
    st_crosses(ch_border, sparse = FALSE) |>
    as.logical()
  
  # check what skippable cantons the route crosses
  xing_cantons_logi = st_intersects(skippable_cantons,
                                    st_zm(linestring), 
                                    sparse = FALSE)
  xing_cantons = pull(skippable_cantons,id)[xing_cantons_logi] |>
    setdiff(c(from, to))
  
  # compile route information
  id = paste0(from, "-", to, "_", skip)
  route_params = api_response$features$properties[,c(3,4,6,7,8)] |> 
    mutate_all(as.integer)# |> 
    # set_names(c("distance","ascend","time","engergy","cost"))
  route_info = cbind(id, from, to, skip,
                     xing = paste0(xing_cantons, collapse = "-"),
                     xing_ch_border,
                     route_params)
  
  # create shape file of route with info
  shapefile = route_info %>% 
    add_column(list(linestring)) %>%
    st_as_sf() %>%
    st_set_crs(4326)
  
  # write route info to table (appending)
  write_csv(route_info, 
            here(table_file),
            append = TRUE)

  # write route shape file
  st_write(shapefile,
           here("segments", 
                paste0(id, ".geojson")),
           quiet = TRUE)
  
  # error when crossing national border or skipped canton border
  if(xing_ch_border) {
    warning(glue::glue("Route {id} crosses Swiss national border. Aborting..."))
  } 
  xing_skipped_cantons = xing_cantons[str_detect(xing_cantons, skipped_cantons)] 
  if(length(xing_skipped_cantons) > 0) {
    warning(glue::glue("Route {id} enters cantons it should skip: 
                    {str_c(xing_skipped_cantons, collapse = ', ')}
                    Aborting..."))
  }
  return(route_info)
}