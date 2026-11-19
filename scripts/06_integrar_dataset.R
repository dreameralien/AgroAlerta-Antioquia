# 06_integrar_dataset.R -- Integra SMAP + NASA POWER + EVA(UPRA) + DANE en una tabla municipio-dia
# Salidas: data/processed/dataset_modelado.rds, data/processed/smap_municipio_dia.rds, data/processed/calidad_smap.rds
suppressPackageStartupMessages({library(sf); library(dplyr); library(data.table)})
sf_use_s2(FALSE)
mun <- readRDS("data/processed/municipios.rds")

# 1) SMAP: pixeles -> municipio (centro del pixel dentro del poligono DANE) ---------------------
smap <- rbindlist(lapply(list.files("data/raw/smap", "^smap_pixeles_.*rds$", full.names = TRUE), readRDS))
smap <- unique(smap, by = c("ix", "iy", "fecha"))                     # evita duplicados entre bloques
pix  <- unique(smap[, .(ix, iy, lon, lat)])
pix  <- st_join(st_as_sf(pix, coords = c("lon", "lat"), crs = 4326, remove = FALSE), mun["cod_mpio"], join = st_within) |> st_drop_geometry()
smap <- merge(smap, as.data.table(pix)[!is.na(cod_mpio), .(ix, iy, cod_mpio)], by = c("ix", "iy"))

# 2) Filtros de calidad --------------------------------------------------------------------------
# retrieval_qual_flag: bit0 = 0 -> calidad recomendada; bit3 = 1 -> recuperacion exitosa.
# Principal: recuperacion exitosa con valor fisicamente plausible (0 < SM <= 0.6 m3/m3).
# Estricto (sensibilidad): ademas exige bit0 = 0.
valido <- function(sm, q, estricto) { ok <- !is.na(sm) & sm > 0 & sm <= 0.6 & bitwAnd(q, 8L) > 0; if (estricto) ok <- ok & bitwAnd(q, 1L) == 0; ifelse(ok, sm, NA_real_) }
for (e in c(FALSE, TRUE)) {
  s <- if (e) "_estricto" else ""
  smap[, paste0("am", s) := valido(sm_am, q_am, e)]; smap[, paste0("pm", s) := valido(sm_pm, q_pm, e)]
}
smap[, `:=`(sm_pix = rowMeans(cbind(am, pm), na.rm = TRUE), sm_pix_estricto = rowMeans(cbind(am_estricto, pm_estricto), na.rm = TRUE))]
smap[is.nan(sm_pix), sm_pix := NA]; smap[is.nan(sm_pix_estricto), sm_pix_estricto := NA]
sm_mun <- smap[, .(Humedad_Suelo = mean(sm_pix, na.rm = TRUE), n_pix = sum(!is.na(sm_pix)),
                   Humedad_Suelo_Estricto = mean(sm_pix_estricto, na.rm = TRUE), n_pix_estricto = sum(!is.na(sm_pix_estricto))),
               by = .(cod_mpio, fecha)]
sm_mun[is.nan(Humedad_Suelo), Humedad_Suelo := NA]; sm_mun[is.nan(Humedad_Suelo_Estricto), Humedad_Suelo_Estricto := NA]
saveRDS(sm_mun, "data/processed/smap_municipio_dia.rds")
calidad <- smap[, .(retrievals = sum(!is.na(sm_am)) + sum(!is.na(sm_pm)), validos = sum(!is.na(am)) + sum(!is.na(pm)),
                    validos_estricto = sum(!is.na(am_estricto)) + sum(!is.na(pm_estricto)))]
saveRDS(list(global = calidad, n_pixeles_mun = pix |> filter(!is.na(cod_mpio)) |> count(cod_mpio)), "data/processed/calidad_smap.rds")

# 3) NASA POWER y variables derivadas (solo informacion presente y pasada) ---------------------------
pw <- as.data.table(readRDS("data/processed/power_diario.rds")); setorder(pw, cod_mpio, fecha)
pw[, Dias_Sin_Lluvia := { g <- cumsum(Precipitacion >= 1 | is.na(Precipitacion)); sequence(rle(g)$lengths) - 1L }, by = cod_mpio]
pw[, `:=`(Precipitacion_Acum_3d  = frollsum(Precipitacion, 3,  na.rm = FALSE), Precipitacion_Acum_7d  = frollsum(Precipitacion, 7,  na.rm = FALSE),
          Precipitacion_Acum_14d = frollsum(Precipitacion, 14, na.rm = FALSE), Precipitacion_Acum_30d = frollsum(Precipitacion, 30, na.rm = FALSE),
          Precipitacion_Acum_60d = frollsum(Precipitacion, 60, na.rm = FALSE),
          Temperatura_Promedio_7d = frollmean(Temperatura_Media, 7), Humedad_Relativa_Prom_7d = frollmean(Humedad_Relativa, 7),
          Radiacion_Prom_7d = frollmean(Radiacion_Solar, 7), Rango_Termico = Temperatura_Maxima - Temperatura_Minima), by = cod_mpio]
pw[, `:=`(Mes = as.integer(format(fecha, "%m")), DiaAnio = as.integer(format(fecha, "%j")))]
pw[, `:=`(Estacion_Sen = sin(2 * pi * DiaAnio / 365.25), Estacion_Cos = cos(2 * pi * DiaAnio / 365.25))]

# 4) Union con SMAP, DANE y EVA ----------------------------------------------------------------------
eva <- as.data.table(readRDS("data/processed/eva_municipio.rds"))
mm  <- as.data.table(st_drop_geometry(mun))[, .(cod_mpio, Municipio, Latitud = lat, Longitud = lon, Area_km2 = area_km2)]
d <- merge(sm_mun, pw, by = c("cod_mpio", "fecha")); d <- merge(d, mm, by = "cod_mpio"); d <- merge(d, eva, by = "cod_mpio", all.x = TRUE)
d <- d[!is.na(Humedad_Suelo) & complete.cases(d[, .(Precipitacion, Temperatura_Media, Temperatura_Maxima, Humedad_Relativa, Radiacion_Solar, Precipitacion_Acum_60d)])]
setorder(d, cod_mpio, fecha); stopifnot(!anyDuplicated(d, by = c("cod_mpio", "fecha")))
saveRDS(as.data.frame(d), "data/processed/dataset_modelado.rds")
message("Dataset: ", nrow(d), " filas | ", uniqueN(d$cod_mpio), " municipios | ", min(d$fecha), " a ", max(d$fecha))
