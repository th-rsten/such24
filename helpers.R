skip1_routes_needed = check_routing_scenarios |> 
  # want to add any (theoretical) ZH-skip segment
  # even though they don't appear in the full list due to only combining with SH
  bind_rows(cbind(point_combinations, skip2 = "ZH", skip = "ZH")) |> 
  filter(!((from == "SH" & skip == "ZH") | (to == "SH" & skip == "ZH"))) |> 
  inner_join(all_single_crosses) |> 
  mutate(id = paste0(from, "-", to, "_", skip))

get_already_routed(raw=TRUE) |>
  filter(str_sub(id, 1,2) == "ST" |
           skip == "" |
           id %in% skip1_routes_needed$id) |> 
  mutate(xing = str_remove(xing, "^NW$|(?<=-)NW-|-NW(?=-)")) |> 
  write_csv(here("segments_3.csv"),col_names = FALSE)


already_routed_files = tibble(id = str_remove(list.files(here("segments")),
                                              ".geojson"),
                              file = TRUE) |> 
  separate(id, c("fromto", "skip"), "_", remove = FALSE) |> 
  filter(str_sub(id, 1,2) != "ST",
           skip != "",
           !id %in% skip1_routes_needed$id) |> 
  pull(id)
if(length(already_routed_files) > 0) {
  cat(paste(already_routed_files, collapse = "\n"))
  goodtogo = menu(c("Yes", "No"), 
                  title = "\nResults as shape but missing entry in table!
                  Remove?")
  if(goodtogo == 1) {
    here("segments", paste0(already_routed_files,".geojson")) %>% file.remove()
  }
}


# vizualize and edit nogo zones quickly
nogos_url_prep = nogos_cantons |>
  filter(id == "SH") |>
  pull("nogo_zones") |>
  str_replace("\\|", ";")
nogos_url = paste0("localhost:8080/#map=10/47.5751/9.4016/",
                   "standard&profile=such&nogos=", nogos_url_prep)





skip1_routes_needed = check_routing_scenarios |> 
  # want to add any (theoretical) ZH-skip segment
  # even though they don't appear in the full list due to only combining with SH
  bind_rows(cbind(point_combinations, skip2 = "ZH", skip = "ZH")) |> 
  filter(!((from == "SH" & skip == "ZH") | (to == "SH" & skip == "ZH"))) |> 
  inner_join(all_single_crosses) |> 
  mutate(id = paste0(from, "-", to, "_", skip))

get_already_routed(raw=TRUE) |>
  filter(
    # str_sub(id, 1,2) == "ST" |
    #        skip == "" |
    #        id %in% skip1_routes_needed$id
         !id %in% intersection_issues$id) |> 
 # mutate(xing = str_remove(xing, "^NW$|(?<=-)NW-|-NW(?=-)")) |> 
  write_csv(here("segments_4.csv"),col_names = FALSE)

here("segments", paste0(intersection_issues$id,".geojson")) %>% file.remove()
