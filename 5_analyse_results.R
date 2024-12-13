# translate result into route and record value
all_tours_cost = read_csv(here("tours_list_v1_cost.csv")) |> mutate(route_p = "cost")
all_tours_nrg = read_csv(here("tours_list_v1_nrg.csv")) |> mutate(route_p = "energy")
all_tours = bind_rows(all_tours_cost, all_tours_nrg)

all_tours |>
  group_by(skip, parameter) |>
  arrange(tour_length) |> 
  filter(tour_length == first(tour_length),
         route_p == first(route_p)) |> 
  ungroup()  -> all_best_tours

all_tours |> filter(distance == min(distance)) #|> pull(ids)

all_best_tours |> 
  group_by(skip1, skip2, skip3, skip4, skip6, skip7, parameter) |> 
  mutate(tour_len_diff = tour_length - min(tour_length),
         n = n()) |> 
  ungroup() -> tour_length_diff_group5

tour_length_diff_group5 |> 
  filter(n == max(n)) |> 
  ggplot(aes(skip5, tour_len_diff)) +
  # geom_boxplot() +
  geom_jitter(aes(color = as.factor(skip2))) + 
  facet_wrap(~parameter, scales = "free_y")

all_best_tours |> 
  group_by(skip1, skip2, skip3, skip5, skip6, skip7, parameter) |> 
  mutate(tour_len_diff = tour_length - min(tour_length),
         n = n()) |> 
  ungroup() -> tour_length_diff_group4

tour_length_diff_group4 |> 
  filter(n == max(n)) |> 
  ggplot(aes(skip4, tour_len_diff)) +
  # geom_boxplot() +
  scale_color_viridis_c() +
  geom_jitter(aes(color = time)) + 
  facet_wrap(~parameter, scales = "free_y")


all_best_tours |> 
  group_by(skip1, skip2, skip3, skip4, skip5, skip6, parameter) |> 
  mutate(tour_len_diff = tour_length - min(tour_length),
         n = n()) |> 
  ungroup() -> tour_length_diff_group7

tour_length_diff_group7 |> 
  filter(n == max(n)) |> 
  ggplot(aes(skip7, tour_len_diff)) +
  # geom_boxplot() +
  scale_color_viridis_c() +
  geom_jitter(aes(color = time/3600)) + 
  facet_wrap(~parameter, scales = "free_y")


all_tours |> 
  group_by(parameter, tour_rank) |> 
  summarize(ids = str_split(paste0(ids, collapse = "|"), "\\|")) |> 
  unnest(ids) |> 
  count(parameter, tour_rank, ids) |>
  separate(ids, c("fromto", "skip"), "_", remove = FALSE) |> 
  separate(fromto, c("from", "to"), "-") |> 
  group_by(parameter, tour_rank, str_length(skip)) |> 
  summarize(sum(n)) -> dasdad


# main_shapefile = list.files(paste0(workd, "segments/"), full.names = TRUE) %>%
#   map(st_read, quiet = TRUE) %>%
#   data.table::rbindlist(use.names = FALSE) %>%
#   st_as_sf()

# main_shape = main_shapefile %>% set_names(c(names(all_routes), "geometry")) %>%
#   mutate(onetwo = paste0(one, two),
#          label = paste0(one, "->", two))
# st_write(main_shape, paste0(workd, "all_routes.gpkg"))
main_shape = read_sf(paste0(workd, "all_routes.gpkg")) %>%
  mutate(across(3:7, as.integer),
         across(1:2, as.factor))

energy_conv = 0.00000027777777777778
path_way = paste0(pool, lead(pool))[-length(pool)]
tour_sum = main_shape %>% filter(onetwo %in% path_way) %>%
  summarize(across(3:7, sum))

cat(prettyNum(round(tour_sum$distance/1000), ","), "km, ",
    prettyNum(tour_sum$ascend, ","), "hm, ",
    round(tour_sum$time/3600,1), "h, ",
    round(tour_sum$energy*energy_conv, 3), "kWh, ",
    prettyNum(tour_sum$cost, big.mark = ","), "cost")
write_sf(tour_sum, paste0(workd, "jonas_bl3.geojson"))



install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")
my_data <- startp_table[, 7:11]
chart.Correlation(my_data, histogram=TRUE, pch=19)


# saving top 20 as gpx and geojson ----------------------------------------

combine_segment_shp = function(segment_ids, name) {
  shp = str_split(segment_ids, "\\|") |> 
    unlist() %>% 
    str_c(here("segments"),"/",., ".geojson") |> 
    map(~st_read(.x, quiet = TRUE) |> 
          st_zm() |> 
          mutate(across(7:11, as.integer)) |> 
          set_names(c(first_cols,"geometry"))) |> 
    bind_rows() %>%
    summarize()
  # summarize(id = name,
  #           distance = sum(distance)/1000, 
  #           ascend = sum(ascend), 
  #           time = sum(time)/3600,
  #           energy = sum(energy)*energy_conv,
  #           cost = sum(cost))
  write_sf(shp, here("gpx", paste0(name, ".gpx")))
  return(shp)
}
energy_conv = 0.00000027777777777778

tours_prep = all_tours |> 
  distinct(ids, .keep_all = TRUE) |>
  transmute(distance = distance/1000,
            ascend,
            time = time/3600,
            energy = energy * energy_conv,
            cost,
            skip_n, skip, first_cp, optimal_tour,
            ids)

save_top_choice = function(param) {
  tours = tours_prep |> 
    arrange(vars(param), cost) |> 
    head(20) |> 
    mutate(rank = row_number(), .before = distance)
  
  tour_ids = paste0(param, 1:20, "_",
                    str_replace_all(tours$skip, "\\|", "-"))
  
  top_tours = map2(tours$ids, tour_ids, 
                    combine_segment_shp) |>
    bind_rows() |> 
    bind_cols(tours)
  
  write_sf(top_tours, here(paste0("top_", param, ".geojson")))
 return(top_tours) 
}
  
all_top = map(c("distance", "ascend", "time", "cost"), save_top_choice)
  
  