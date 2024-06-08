# necessary package to use QGIS processing, connects to your QGIS installation
# install.packages("qgisprocess")
library(qgisprocess)


# count frequency of route segments for 1-skip-routes only ----------------

# load all segment shape files
segment_files = list.files(here("segments"), 
                           full.names = TRUE,
                           pattern = "_.geo") |> #TI.._.geo|TI
  lapply(read_sf) |> 
  lapply(st_zm)

# combine all shape files, routes are duplicated
segments_noskips2 = do.call(rbind, segment_files) |> st_make_valid()

# dissolve to one big multiline with one line for each road covered
dissolved =  qgis_run_algorithm(algorithm = "native:dissolve",
                                INPUT = segments_noskips2) |> 
  st_as_sf()

# split up dissolved shape into every road segment separated with a crossing
unqiue_segments = qgis_run_algorithm(algorithm = "native:splitwithlines",
                                     INPUT = dissolved,
                                     LINES = dissolved) |>
  st_as_sf() |> 
  transmute(num = row_number())

# join the unique road segments with all se
intersection_count = unqiue_segments |> 
  st_join(segments_noskips2,
          join = st_within) |> 
  st_drop_geometry() |> 
  transmute(num,
            seg = paste0(one, "-", two)) |> 
  group_by(num) |> 
  summarize(n = n(),
            segs = paste0(seg, collapse = "\n"))

intersection_count |> count(n) # frequency of road coverages

unqiue_segments |> left_join(intersection_count) |> select(n) |> plot()

# save file to check the exact roads  used most often by routes
unqiue_segments |> 
  left_join(intersection_count) |> 
  write_sf(here("direct_route_frequency_v2.geojson"))