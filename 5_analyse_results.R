# translate result into route and record value


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
