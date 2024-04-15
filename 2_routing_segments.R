# prepare routing API calls -----------------------------------------------

api_url = "http://brouter.de/brouter?nogos=8.577919,47.648275,3220|8.372612,47.631504,6187|8.77533,47.743018,6076|8.676882,47.685291,211|9.667282,47.433085,1653|9.624109,47.327366,2294|9.508066,47.206391,780|9.497509,47.058019,1733|8.470459,47.592662,1468|8.229403,47.612383,454|8.060789,47.567868,684|6.133204,46.355697,1733|6.803284,46.390508,2676|8.33313,45.950195,35491|9.387817,46.137972,19234|9.147105,47.113594,874|8.489342,46.978492,1128|8.596802,46.990088,363|8.473806,47.004623,513|8.389778,47.01453,1131|8.618774,47.267765,658|8.575172,47.301558,584|8.678856,47.242877,821|9.596064,47.465032,132|8.611221,46.964029,413&profile=fastbike&alternativeidx=0&format=geojson&lonlats="
api_url2 = ""

get_route = function(one, two, xone, yone, xtwo, ytwo) {
  # api_url2 = "6.126165,46.317771|7.622538,47.547481"
  api_url2 = paste0(xone, ",", yone, "|", xtwo, ",", ytwo)
  
  api_response = try(
    jsonlite::fromJSON(txt = paste0(api_url,api_url2)),
    silent = TRUE)
  
  if(class(api_response) == "try-error") return(NULL)
  
  linestring = try(
    st_linestring(api_response$features$geometry$coordinates[[1]]),
    silent = TRUE)
  if(any(class(linestring) == "try-error", na.rm = TRUE)) {
    geo_to_fix = api_response$features$geometry$coordinates[[1]]
    fix_line = lengths(geo_to_fix) == 2
    cat(one, two, sum(fix_line), "\n")
    geo_to_fix[fix_line] <- lapply(geo_to_fix[fix_line], c,NA)
    linestring = geo_to_fix %>% 
      unlist() %>% 
      matrix(ncol=3, byrow=TRUE) %>% 
      as.data.frame() %>%
      fill(V3, .direction="downup") %>% 
      as.matrix() %>% 
      st_linestring()
  }
  route_info = cbind(one, two, api_response$features$properties[,c(3,4,6,7,8)])
  
  write_csv(route_info,
            paste0(workd, "routes4.csv"),
            append = TRUE)
  
  shapefile = route_info %>%
    add_column(list(linestring)) %>%
    st_as_sf() %>%
    st_set_crs(4326)
  
  st_write(shapefile, paste0(workd, "segments/", one, two, ".geojson"),
           quiet = TRUE)
  
  return(shapefile)
  # shape demo
  # shapefile %>% st_set_crs(4326) %>% plot()# mapview()
  # st_write("C:/Users/th/Downloads/test.geojson")
}

# get routes for point pairs ----------------------------------------------

already_routed = read_csv(paste0(workd, "routes4.csv"),
                          col_names = c("one", "two")) %>%
  mutate(onetwo = paste0(one, two))

point_routes = point_combinations %>%
  filter(!paste0(one, two) %in% already_routed$onetwo) %>%
  left_join(points, by = c("one" = "name")) %>%
  # st_drop_geometry() %>%
  left_join(points, by = c("two" = "name"), suffix = c("one", "two")) %>%
  select(one, two, xone, yone, xtwo, ytwo)

# plan(multisession, workers = 10)
route_shape2 = point_routes %>% # head(10) %>% 
  # dplyr::arrange(desc(one)) %>% 
  # future_pmap(get_route,
  pmap(get_route)#,
# .progress = TRUE)
beep(2)