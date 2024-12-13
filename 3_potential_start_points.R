# prepare potential starting points associated with all CPs ---------------

library(terra)

# load all swiss train stations
ch_stations = read_sf(here("geodata", "ch_stations.gpkg"))

# calculate which are the 15 closest stations to each CP (as the crow flies)
closest = nearby(vect(cp_sf), vect(ch_stations), k = 15)[,-1] # distance matrix
closest_stations = ch_stations |>  # prepare empty table
  mutate(id = "", no = "") |> 
  filter(name == "")
# iterate over distance matrix and assemble file
for(i in 1:nrow(closest)) {
  new = ch_stations[closest[i,],]
  canton_id= pull(cp_sf, "id")[i] # what canton CP is associated
  pt_names = paste0("ST_", canton_id, "_", # unique ID
                    str_pad(1:15, 2, pad = "0"))
  new = cbind(new, "id" = canton_id, "no" = pt_names)
  closest_stations = rbind(closest_stations, new)
}

# filter out stations close to end point
closest_stations = closest_stations |> 
  filter(id != "EN") 
mapview(filter(closest_stations, id == "SZ"), zcol = "id") # vizualize stations


# prepare direct routes from starting points to first CP -----------------------

direct_startpoint_routes = closest_stations |> 
  {\(.) mutate(., 
               lonfrom = st_coordinates(.)[,1],
               latfrom = st_coordinates(.)[,2]
  )}() |> 
  st_drop_geometry() |> 
  left_join(cp_table) |> 
  as_tibble() |> 
  # filter(no == "ST_TI_10") |> 
  mutate(from = paste0("ST", str_sub(no, -2,-1)),
         skip = "",
         table_file = "st_routes1.csv"  ) |> 
  select(from, to = id, lonfrom, latfrom, lonto = lon, latto = lat, 
         skip, table_file)



plan(multisession, workers = 4)

direct_startpoint_routes2 = direct_startpoint_routes %>% 
  sample_frac() |>
  pmap(get_route,
       .progress = TRUE)


# analyze starting routes -------------------------------------------------

# read out routing results
startp_table = read_csv(here("st_routes1.csv"),
                        col_names = first_cols) |> 
  
  filter(!xing_ch_border) |> 
  rename(m = 7, hm = 8, time = 9, energy = 10)

# add ranks, filter and sort for shortest/quickest/easiest routes
startp_ranked = startp_table |>
  group_by(to) |> 
  mutate(across(6:10, percent_rank, .names = "{.col}_rank")) |> 
  ungroup() |> 
  group_by(to, xing) |> 
  mutate(across(6:10, percent_rank, .names = "{.col}_rank_s")) |> 
  ungroup() |> 
  # groups of 1 produce NaN ranks so they need to be fixed
  mutate(across(contains("rank_s"), replace_na,  0L)) |>
  rowwise() |> 
  filter(any(0 %in% c_across(16:21))) |> 
  ungroup() |> 
  arrange(to, (1 + cost_rank) * (1 + time_rank))

view(startp_ranked)


# review routes and confirm selection -------------------------------------

startp_selection =  
  c("ST_AG_01",
    "ST_AI_01",
    "ST_AR_01",
    "ST_BE_01",
    "ST_BE_11",
    "ST_BL_01",
    "ST_BS_01",
    "ST_FR_01",
    "ST_GE_01",
    "ST_GL_05",
    "ST_GR_01",
    "ST_JU_01",
    "ST_LU_02",
    "ST_NE_09",
    "ST_NW_01",
    "ST_OW_13",
    "ST_SG_04",
    "ST_SH_02",
    "ST_SO_01",
    "ST_SZ_01",
    "ST_TG_01",
    "ST_TI_15",
    "ST_UR_13",
    "ST_VD_01",
    "ST_VS_02",
    "ST_ZG_01",
    "ST_ZH_01")

filtered_startp = startp_table |> 
  filter(one %in% startp_selection) |> 
  mutate(file = paste0(str_remove(id, "-"), ".geojson"),
         id = paste0("ST-", two, "_", coalesce(xing, "")),
         file_new = paste0(id, ".geojson"),
         one = "ST")

dir.create(here("segments_startp"))
file.copy(here("segments", filtered_startp$file),
          here("segments_startp", filtered_startp$file_new))

filtered_startp |> 
  select(-file, -file_new) |> 
  write_csv(here("st_routes.csv"))