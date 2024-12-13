
# prepare table for all possible scenarios --------------------------------
already_routed_raw = get_already_routed(raw = TRUE)

all_to_route = all_routing_scenarios |> 
  group_by(skip) |> 
  filter(n() == sum(!is.na(id))) |> 
  ungroup() 

ST_row_prep = get_already_routed("st_routes.csv", raw = TRUE) |> 
  select(id, from, to, xing, 7:11) |> 
  arrange(time)

split_scenarios = split(all_to_route, all_to_route$skip)


# iterate over scenarios --------------------------------------------------
plan(multisession, workers = 13) # with list of scenarios 7050 taking 70 mins
tic()

all_tours = split_scenarios |>  
  sample() |> 
  future_map(function(route_df) {
    # get descriptive data of scenario
    scenario_details = route_df |> head(1) |> select(starts_with("skip"))
    
    point_order = c("ST", intersect(cp_coords$id, unique(route_df$to))) 
    
    ST_row = ST_row_prep |>
      filter(to %in% point_order) |> 
      # filter out start point routes crossing skipped cantons
      filter(!str_detect(xing, str_replace(scenario_details$skip, "^$", " ")) |
               is.na(xing)) |> 
      select(-xing) |> 
      distinct(to, .keep_all = TRUE)
    
    EP_row = set_names(data.frame(NA, "EN", "ST", 0L, 0L, 0L, 0L, 0L),
                       names(ST_row))
    
    # only use routes needed
    routes_selection = already_routed_raw %>%
      filter(id %in% route_df$id) |> 
      select(id, from, to, 7:11) |>
      bind_rows(ST_row, EP_row)
    
    # generate different distance matrices per pool -------------------------
    # distance, hm, time (== energy), cost
    
    c("distance", "ascend", "time", "cost") |> 
      map(function(parameter = "cost") {
        # set up distance matrix for each route parameter
        distance_matrix = routes_selection %>%
          select(from, to, all_of(parameter)) %>%
          # widen table for one row to "distances" to all other points
          pivot_wider(names_from = to, values_from = 3, values_fill = 10^9) %>% 
          # sort rows and column to be symmetrical
          mutate(from = factor(from, point_order)) %>%
          arrange(from) %>%
          select(all_of(point_order)) %>%
          as.matrix()
        
        defined_atsp = ATSP(distance_matrix)
        
        # solve TSP and extract results------------------------------------------------
        
        tour_results = list(x = rep(list(defined_atsp),1000), 
                            method = rep(c("farthest_insertion", 
                                           "cheapest_insertion"), 500),
                            two_opt = c(rep(TRUE, 500), rep(FALSE, 500))) %>%
          pmap(solve_TSP)
        tour_lengths = map_int(tour_results, tour_length)
        
        1L:3L |> 
          map(function(tour_rank = 1L) {
            tour_length_selected = sort(unique(tour_lengths))[tour_rank]
            tour_i = tour_lengths == tour_length_selected
            optimal_tour = tour_results[tour_i][[1]] |> 
              cut_tour("ST", FALSE) |> 
              names()
            # get all parameters for respective tour
            routes_selection |> 
              filter(paste0(from,to) %in%
                       paste0(optimal_tour, lead(optimal_tour))) |> 
              mutate(from = factor(from, optimal_tour)) %>%
              arrange(from) |> 
              select(-2:-3) |> 
              summarise(across(-id, sum),
                        ids = str_c(na.omit(id), collapse = "|")) |> 
              bind_cols(tour_length = tour_length_selected,
                        tour_rank = tour_rank,
                        n_solution = sum(tour_i),
                        optimal_tour = str_c(optimal_tour, collapse = "-"),
                        first_cp = optimal_tour[2])
          }) |> 
          list_rbind() |> 
          bind_cols(parameter = parameter)
      }) |> 
      list_rbind()  |> 
      bind_cols(scenario_details)
  },
  .progress = TRUE) |> 
  list_rbind() |> 
  select(starts_with("skip"), optimal_tour, parameter,
         tour_rank, tour_length, 
         distance, ascend, time, energy, cost,
         first_cp, n_solution, ids)
toc()

write_csv(all_tours, here("tours_list_v1_nrg.csv"))