# prepare routing API calls -----------------------------------------------

# load in custom no-go zones to avoid leaving the country
nogos_base = read_file("border_nogos.txt") |> 
  str_replace_all("\\;", "\\|")
# load custom no-go zones for every skippable canton
nogos_cantons = cp_table |> 
  filter(!is.na(group)) |> # is skippable
  select(id, nogo_zones) |> 
  mutate(nogo_zones = str_replace_all(nogo_zones, "\\;", "\\|"))


brouter_profile = "such" # set brouter profile
brouter_hostname = "http://localhost:17777" # set brouter hostname
api_url = paste0(brouter_hostname, # base brouter API url
                 "/brouter?profile=", brouter_profile,
                 "&alternativeidx=0&format=geojson&lonlats=")

# create segments folder if missing
if(!dir.exists(here("segments"))) dir.create(here("segments"))


# test data ---------------------------------------------------------------

route_no = 1 # 614 # ge-AI
one = point_routes$one[route_no]
two = point_routes$two[route_no]
ytwo = point_routes$ytwo[route_no]
xtwo = point_routes$xtwo[route_no]
yone = point_routes$yone[route_no]
xone = point_routes$xone[route_no]
skip =  point_routes$skip[route_no]# sort(c(NA, NA, "ZH", "SH", "BS", NA, NA))

# function to get route and save result as numbers and shape --------

get_route = function(one, two, xone, yone, xtwo, ytwo, skip) {
  # api_url2 = "6.126165,46.317771|7.622538,47.547481"
  
  lonlats = paste0(xone, ",", yone, "|", xtwo, ",", ytwo)
  # combine no-go zones of all cantons to be skipped + general zones
  nogos_combo = nogos_cantons |> 
    filter(id %in% skip) |>
    pull("nogo_zones") |> 
    na.omit() |> 
    paste0(collapse = "|")
  nogos_url = paste0("&nogos=", nogos_base, "|", nogos_combo)
  
  # call BRouter API for route
  api_response = try(
    jsonlite::fromJSON(txt = paste0(api_url, lonlats, nogos_url)),
    silent = TRUE)
  
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
    cat(one, two, sum(fix_line), "\n")
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
    setdiff(c(one, two))
  
  # commpile route information
  skipped_cantons = paste0(sort(na.omit(skip)), collapse = "-")
  route_info = cbind(paste0(one,"-",two,"_",skipped_cantons),
                     one, two,
                     skip = skipped_cantons,
                     xing = paste0(xing_cantons, collapse = "-"),
                     xing_ch_border,
                     api_response$features$properties[,c(3,4,6,7,8)])
  
  # write route info to table (appending)
  write_csv(route_info, 
            here("cp_routes_test3.csv"),
            append = TRUE)
  
  # create shape file of route with info
  shapefile = route_info %>% 
    add_column(list(linestring)) %>%
    st_as_sf() %>%
    st_set_crs(4326)
  
  # write route shape file
  st_write(shapefile,
           here("segments", 
                paste0(one, two, "_",
                       skipped_cantons,
                       ".geojson")),
           quiet = TRUE)
}

# get routes for point pairs ----------------------------------------------

first_cols = c("id", "one", "two", "skip", "xing", "xing_ch_border",
               "track-length", "filtered ascend", "total-time", "total-energy",
               "cost")
already_routed = read_csv(here("cp_routes_test2.csv"),
                          col_names = first_cols) %>%
  mutate(onetwo = paste0(one, two))

point_routes = point_combinations %>%
  left_join(cp_table, by = c("from" = "id")) %>%
  left_join(cp_table, by = c("to" = "id"), suffix = c("from", "to")) %>%
  select(from, to, lonfrom, latfrom, lonto, latto) |> 
  inner_join(all_single_crosses) |>
  mutate(skip = xing) |> select(-xing) |>
  # mutate(skip = "") |> 
  set_names(c("one", "two", "xone", "yone", "xtwo", "ytwo", "skip")) |> 
  filter(!paste0(one,two,skip) %in% 
           paste0(already_routed$one, 
                  already_routed$two,
                  already_routed$skip))

library(tictoc)

plan(multisession, workers = 4)
tic()
route_shape2 = point_routes %>% 
  # dplyr::arrange(desc(one)) %>% 
  sample_frac() |>
  future_pmap(get_route,
              .progress = TRUE)
 #  filter(one == "GE", two == "VD") |> 
 # pmap(get_route)
toc()

beep(2)

# combine all generated routes
all_routes =  do.call(rbind, route_shape2)


all_single_crosses = already_routed |> filter(one != "one") |> 
  st_drop_geometry() |> 
  filter(is.na(skip)) |> 
  select(from = one, to = two, xing) |> 
  mutate(xing = str_split(xing, "-")) |> 
  unnest(xing) |> 
  filter(xing != "")

all_single_crosses |> count(from, to) |> count(n) |> pull(nn) |> sum()

all_single_crosses |>
  group_by(to, xing) |> summarize(n = n()) |> 
    ggplot(aes(xing, to, fill = n)) + geom_tile() + scale_fill_viridis_c()

# how many times is each canton crossed
all_crosses = all_routes |> pull(xing) |> str_split("-") |> unlist() |> table()
cross_freq = tibble(id = names(all_crosses), n_cross = as.integer(all_crosses))