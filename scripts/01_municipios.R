# 01_municipios.R -- Limites municipales de Antioquia (DANE MGN 2025)
# Entrada : data/raw/dane/mpio_antioquia.geojson (descargado del FeatureServer del DANE, capa 317)
# Salida  : data/processed/municipios.rds (sf, EPSG:4326) y municipios_centroides.csv
suppressPackageStartupMessages(library(sf))
sf_use_s2(FALSE)
m <- st_read("data/raw/dane/mpio_antioquia.geojson", quiet = TRUE) |> st_make_valid()
m <- m[, c("MPIO_CDPMP", "MPIO_CNMBRE", "MPIO_NAREA")]
names(m)[1:3] <- c("cod_mpio", "Municipio", "area_km2")
pt <- st_point_on_surface(st_geometry(m))   # punto garantizado dentro del poligono
xy <- st_coordinates(pt)
m$lon <- xy[, 1]; m$lat <- xy[, 2]
stopifnot(nrow(m) == 125, !anyDuplicated(m$cod_mpio))
saveRDS(m, "data/processed/municipios.rds")
write.csv(st_drop_geometry(m), "data/processed/municipios_centroides.csv", row.names = FALSE, fileEncoding = "UTF-8")
message("Municipios: ", nrow(m), " | lon [", round(min(m$lon),2), ",", round(max(m$lon),2), "] lat [", round(min(m$lat),2), ",", round(max(m$lat),2), "]")
