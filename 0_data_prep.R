
# update CP table from Google Sheets --------------------------------------

library(googlesheets4)
cp_sheet = read_sheet("1lYB7hNYPlmCVh7Bn6w4rV9UJFE0MNNXX2kW97pjfN2k")
csv_name =  paste0("such24_cps_", format(today(), "%Y%m%d"))
dupl_number = length(list.files(here(), pattern = csv_name)) + 1
write_csv(cp_sheet,
          here(paste0(csv_name, dupl_number, ".csv")))
         

# ch canton borders file ----------------------------------------------------

# downloaded from 
# https://www.swisstopo.admin.ch/de/landschaftsmodell-swissboundaries3d
ch_borders = st_read(here("geodata", "swissBOUNDARIES3D_1_5_LV95_LN02.gpkg"),
                     layer = "tlm_kantonsgebiet") |> 
  select(name, geom) |> 
  st_transform(4326) |> 
  st_zm() # drop z values

write_sf(ch_borders, here("geodata", "ch_cantons.gpkg"))


# ch train station file ---------------------------------------------------

# https://data.geo.admin.ch/browser/#/collections/ch.bav.haltestellen-oev/items/haltestellen-oev
ch_stations = st_read(here("..", "HaltestellenOeV.gpkg"),
                     layer = "Betriebspunkt") |> 
  filter(str_sub(Verkehrsmittel_Code, 1,1) == "B", # train only
         # filter out steam train stops around Furka pass
        # Transportunternehmen_Abkuerzung != "DFB",
         # no car transfer stations
         !str_detect(Name, "Autoverlad")) |> 
  select(name = Name, geom) |> 
  st_transform(4326)

write_sf(ch_stations, here("geodata", "ch_stations.gpkg"))