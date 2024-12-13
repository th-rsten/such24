# get routes for point pairs ----------------------------------------------

plan(multisession, workers = 4) # set up parallel processing

# function to get all requested route results available 
# convert them to minimal table sorted for shortest routes
get_already_routed = function(segmentfile = "segments_1.csv", 
                              raw = FALSE) {
  csv_output = here(segmentfile) |> 
    read_csv(col_names = first_cols,
             col_types = "cccccliiiii",
             na = "NA")
  if(raw) return(csv_output)
  csv_output |> 
    arrange(from, to, energy, skip) |> # also try cost/energy
    select(id, from1 = from, to1 = to, xing) |> 
    mutate(xing = str_replace_all(xing, "-", "\\|") |> str_replace("^$", " ")) 
}

# function to prepare table of segments to route and trigger routing
route_segments = function(selected_routes) {
  point_routes = selected_routes %>%
    left_join(cp_coords, by = c("from" = "id")) %>%
    left_join(cp_coords, by = c("to" = "id"), 
              suffix = c("from", "to")) %>%
    mutate(skip = paste(skip1, skip2, skip3, skip4, skip5, skip6, skip7, 
                        sep = ",") |> 
             str_remove_all("NA") |> 
             str_remove_all("^,+|,+$") |> 
             str_replace_all(",+", "-"),
           table_file = file_name_save) |> 
    select(from, to, lonfrom, latfrom, lonto, latto, skip, table_file) |> 
    # remove already routed segments from list
    filter(!paste0(from, "-", to, "_", skip) %in% already_routed$id)
  
  n_routes = nrow(point_routes)
  if(n_routes == 0) {
    cat("\nAll segments were already routed.")
  } else {
    goodtogo = menu(c("Yes", "No"), 
                    title = paste("Start routing", n_routes, "segments?"))
    if(goodtogo == 1) {
      cat("\nRouting", n_routes, "segments...")
      tic()
      point_routes %>% 
        sample_frac() |>
         pwalk(get_route, .progress = TRUE)
        # future_pwalk(get_route,
        #              .progress = TRUE)
      toc()
      beep(2)
    }
  }
}

file_name_save = "segments_1.csv"

# check for already routed segments
if(!file.exists(here(file_name_save))) {
  write_file("", here(file_name_save))
}

already_routed = get_already_routed(file_name_save)


# check for already routed segments and route files integrity -------------
route_check = function() {
  already_routed = get_already_routed(file_name_save)
   # check for duplicate entries in segments csv
  dupl_routed = already_routed |> add_count(id) |> filter(n > 1) |> pull(id)
  if(length(dupl_routed) > 0) {
    cat(paste(dupl_routed, collapse = "\n"))
    stop("\nResults in table duplicated.")
  }
  
  # check for mismatch between csv file and route shapes
  already_routed_files = tibble(id = str_remove(list.files(here("segments")),
                                                ".geojson"),
                                file = TRUE)
  already_routed_mismatch = already_routed |>
    full_join(already_routed_files, by = "id")
  
  no_shape = already_routed_mismatch |> filter(is.na(file)) |> pull(id)
  if(length(no_shape) > 0) {
    cat(paste(no_shape, collapse = "\n"))
    stop("\nResults in table but missing shape!")
  }
  
  only_shape = already_routed_mismatch |> 
    filter(is.na(from1), str_sub(id, 1, 2) != "ST") |> 
    pull(id)
  if(length(only_shape) > 0) {
    cat(paste(only_shape, collapse = "\n"))
    goodtogo = menu(c("Yes", "No"), 
                    title = "\nResults as shape but missing entry in table!
                  Remove?")
    if(goodtogo == 1) {
      here("segments", paste0(only_shape,".geojson")) %>% file.remove()
    }
  }
  
  # check routes for leaving the country or crossing cantons they should skip
  intersection_issues = get_already_routed(file_name_save, raw = TRUE) |> 
    mutate(xing2 = str_replace(xing, "^$", " ") |> 
             str_replace_all("-", "\\|")) |> 
    filter(xing_ch_border | str_detect(skip, xing2)) |> 
    arrange(skip, from, to)
  if(nrow(intersection_issues) > 0) {
    intersection_issues
    stop("Routes cross canton they should skip.
         Review the relevant no-go zones.")
  } 
}

# check for routes in combinations with no skipped cantons ----------------

route_check()

# first check no skip scenario for easy no-crossing-routes
all_routing_scenarios |> 
  filter(skip_n < 1) |> 
  route_segments()

already_routed = get_already_routed(file_name_save)
# check which segments never cross any other critical canton
no_xing_routes = already_routed |> filter(xing == " ") 

# edit main table with all those cases
all_routing_scenarios = all_routing_scenarios |> 
  mutate(id =  paste0(from, "-", to, "_"),
         id = if_else(skip_n == 0 | id %in% no_xing_routes$id,
                      id, as.character(NA)))



# preparing batch of combinations with just one skipped canton ------------

# from previous run get all segments which cross any skippable canton 
all_single_crosses = already_routed |> 
  filter(str_detect(id, "_$")) |> 
  select(from = from1, to = to1, skip = xing) |> 
  mutate(skip = str_split(skip, "\\|")) |> 
  unnest(skip) |> 
  filter(skip != " ") |> 
  distinct()

check_routing_scenarios = all_routing_scenarios |> 
  filter(skip_n == 1,
         is.na(id)) |> 
  mutate(skip = paste(skip1, skip2, skip3, skip4, skip5, skip6, skip7, 
                      sep = ",") |> 
           str_remove_all("NA") |> 
           str_remove_all("^,+|,+$") |> 
           str_replace_all(",+", "|")) #|> 
# nest(skips = -c(from, to))

check_routing_scenarios |> 
  # want to add any (theoretical) ZH-skip segment
  # even though they don't appear in the full list due to only combining with SH
  bind_rows(cbind(point_combinations, skip2 = "ZH", skip = "ZH")) |> 
  filter(!((from == "SH" & skip == "ZH") | (to == "SH" & skip == "ZH"))) |> 
  inner_join(all_single_crosses) |> 
  route_segments()

# check for issues with routes
route_check()

# attempt testing all routes shortest to farthest for compatibility ---------
# fetch all already routed segments
already_routed = get_already_routed(file_name_save)

# split up all_combo_list in id = NA and already found ones
split_routing_scenarios = all_routing_scenarios |>
  mutate(skip = paste(skip1, skip2, skip3, skip4, skip5, skip6, skip7, 
                      sep = ",") |> 
           str_remove_all("NA") |> 
           str_remove_all("^,+|,+$") |> 
           str_replace_all(",+", "|")) |> 
  split(is.na(all_routing_scenarios$id))
# only iterate through id = NA part (later merge again)
missing_routing_scenarios = split_routing_scenarios[["TRUE"]]
# group entire list of combos (any skip_n) by from/to
grouped_scenarios = missing_routing_scenarios |> 
  split(paste0(missing_routing_scenarios$from,
               missing_routing_scenarios$to))

# iterate through finished routes for this segment (shortest to longest)
assign_routes = function(fromto_group) {
  filtered_arleady_routed = already_routed |> 
    filter(#!str_detect(id, "-.._..-..-"),
           from1 == fromto_group$from[1],
           to1 == fromto_group$to[1])
  for(i in 1:nrow(filtered_arleady_routed)) {
    #  check for compatibility of xing cantons with all skip combos 
    crossing_skip = str_detect(filtered_arleady_routed$xing[i],
                               fromto_group$skip)
    # assign respective route IDs top-to-bottom
    new_fill = !crossing_skip & is.na(fromto_group$id)
    fromto_group[new_fill,"id"] <- filtered_arleady_routed$id[i]
  }
  # at end of list of already routed ones, return table
  return(fromto_group)
}
tic()
check_scenarios = future_modify(grouped_scenarios, assign_routes)
toc()

# analyze NAs
check_scenarios |>
  bind_rows() |> 
  count(skip_n, covered = !is.na(id)) |>
  pivot_wider(id_cols = skip_n, names_from = covered, values_from = n)

# merge previously id = NA with the rest again
all_routing_scenarios = bind_rows(split_routing_scenarios[["FALSE"]],
                                  bind_rows(check_scenarios))

all_routing_scenarios |> 
  group_by(skip, skip_n) |> 
  summarize(completely_routed = (n()-sum(is.na(id)))/n()) |> 
  ungroup()-> check 

all_routing_scenarios |> filter(skip5 == "AR", !is.na(skip7), !is.na(skip4)) |> 
    group_by(skip, skip_n) |> 
   summarize(completely_routed = ((n()-sum(is.na(id)))/n())==1,
             n_skip = str_length(id)) |> 
  ungroup() |> count(skip_n, n_skip) |> 
  pivot_wider(id_cols = skip_n, names_from = n_skip, values_from = n)
# limit table to desired/next skip_n (just a single one!)
all_routing_scenarios |> 
  filter(skip_n == 5,
         is.na(id)) |>
  # filter(!paste0(from, "-", to, "_", 
  #                str_replace_all(skip,"\\|","-")) %in%
  #          already_routed$id) -> selected_routes
# route these skip-combos
route_segments()

# check for issues with routes
route_check()

# start over from top but this time limit the iterations through already done routes:

# count |s in already routed $ skip and only check the last routed ones (highest count)
# str_count(skip, "|") == max(str_count(skip, "|"))

remove(split_routing_scenarios)
remove(grouped_scenarios)
remove(check_scenarios)
remove(missing_routing_scenarios)
