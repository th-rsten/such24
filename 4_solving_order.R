
# generate different distance matrices per pool -------------------------
# distance
# hm
# energy
# time

all_routes = read_csv(paste0(workd, "routes4.csv"), col_types = "cciiiii") %>%
  arrange(one, two)
EP_row = data.frame("EP", t(rep(0L, 76))) %>%
  set_names(c("one", unique(all_routes$two)))
point_order_def = c("EP","CP1","CP2","CP3","CP4","CP5")
point_order = c(point_order_def,
                setdiff(sort(unique(all_routes$two)),point_order_def))

parameter = "energy"
dismat_distance = all_routes %>%
  select(1:2, all_of(parameter)) %>%
  pivot_wider(names_from = two, values_from = 3, values_fill = 0L) %>%
  rbind(EP_row) %>%
  mutate(one = factor(one, point_order)) %>%
  arrange(one) %>%
  select(all_of(point_order)) %>%
  as.matrix()


# set up tsp solver -------------------------------------------------------

concorde_path(workd)
# filter distance matrices for point pool
#thorsten orig
pool = c("CP4","UR2","SZ3","LU1","NW1","OW1","ZG1","GL2","GR1","CP3","AI2",
         "CP1","AR1",
         "TG1","CP5","AG6","BL1","BS2","SO1","CP2","EP")
#jonas orig
pool = c("CP4", "GR2", "UR2", "VS2", "OW2", "NW1", "LU1", "ZG2", "SZ2", "GL2",
         "CP3", "AI2", "AR1", "CP1",
         "TG1", "CP5", "BL1", "SO1", "NE1", "CP2", "EP")

#jonas neu
pool = c("CP4", "GR2", "UR2", "VS2", "OW2", "NW1", "LU1", "ZG2", "SZ2", "GL2",
         "CP3", "AI2", "AR1", "CP1",
         "TG1", "CP5","AG1", "BL3", "SO1", "NE1", "CP2", "EP")

unique(str_sub(all_routes$two,1,2))[!unique(str_sub(all_routes$two,1,2)) %in%
                                      str_sub(pool,1,2) ]
pool_selec= which(point_order %in% pool)
dismat = dismat_distance[pool_selec, pool_selec] %>% ATSP()

# dismat %>% reformulate_ATSP_as_TSP(cheap=-1, infeasible = 10^7) %>%
#   solve_TSP(method = "concorde",
#             control = list(clo = "-v -B"))

dasd2 = rep(list(dismat),100) %>%
  map(solve_TSP, method = "cheapest_insertion", two_opt = TRUE)
dasd2 %>% map_int(tour_length) %>% sort() %>% hist()
tour_lengths = dasd2 %>% map_int(tour_length)
min_tours = dasd2[tour_lengths==min(tour_lengths)]
length(min_tours)
min(tour_lengths)
optimal_tour = min_tours[[1]] %>% cut_tour("EP", FALSE) %>% .[c(2:21,1)]


# transform to a tsp problem
dismat_distance %>% as.matrix() %>% ATSP()

# solve