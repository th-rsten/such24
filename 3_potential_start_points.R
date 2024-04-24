
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
mapview(closest_stations, zcol = "id") # vizualize stations


# prepare routes from starting points to first CP -------------------------


# route from starting points to CPs ---------------------------------------


# review routes and confirm selection -------------------------------------


# build distance matrix entries for starting point (ST)  ------------------


