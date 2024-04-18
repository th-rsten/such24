# prepare routing API calls -----------------------------------------------

nogos = read_file("border_nogos.txt") |> str_replace_all("\\;", "\\|")
api_url = paste0("http://localhost:17777/brouter?nogos=",nogos,
                 "&profile=such&alternativeidx=0&format=geojson&lonlats=")
api_url2 = ""

one =point_routes$from[1]
two =point_routes$to[1]
 # ytwo = point_routes$latto[1]
 #  xtwo = point_routes$lonto[1]
 #   yone = point_routes$latfrom[1]
 # xone = point_routes$lonfrom[1] 

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
            paste0(workd, "routes1.csv"),
            append = TRUE)
  
  shapefile = route_info %>%
    add_column(list(linestring)) %>%
    st_as_sf() %>%
    st_set_crs(4326)
  
  ch_border |> st_crosses(shapefile)
  skippable_cantons |> st_intersects(shapefile)   skippable_cantons[]
  
  st_write(shapefile, paste0(workd, "segments/", one, two, ".geojson"),
           quiet = TRUE)
  
  return(shapefile)
  # shape demo
  # shapefile %>% st_set_crs(4326) %>% plot()# mapview()
  # st_write("C:/Users/th/Downloads/test.geojson")
}

# get routes for point pairs ----------------------------------------------

# already_routed = read_csv(paste0(workd, "routes4.csv"),
#                           col_names = c("one", "two")) %>%
#   mutate(onetwo = paste0(one, two))

point_routes = border_pairs %>%
  # filter(!paste0(one, two) %in% already_routed$onetwo) %>%
  left_join(cp_table, by = c("from" = "id")) %>%
  # st_drop_geometry() %>%
  left_join(cp_table, by = c("to" = "id"), suffix = c("from", "to")) %>%
  select(from, to, lonfrom, latfrom, lonto, latto)

  # plan(multisession, workers = 10)
route_shape2 = point_routes %>% # head(10) %>% 
  # dplyr::arrange(desc(one)) %>% 
  # future_pmap(get_route,
  pmap(get_route)#,
# .progress = TRUE)
beep(2)