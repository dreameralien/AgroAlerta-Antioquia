# 03_power_descarga.R -- NASA POWER (API diaria, comunidad AG) para el punto interior de cada municipio
# Salida: data/raw/nasa_power/power_<cod_mpio>.rds  y  data/processed/power_diario.rds
# Nota: POWER usa una malla ~0.5x0.625 (MERRA-2) / 1x1 (CERES, radiacion): varios municipios
# comparten celda; la elevacion devuelta es la media de la celda, no del municipio.
suppressPackageStartupMessages({library(jsonlite); library(dplyr)})
m <- readRDS("data/processed/municipios.rds") |> sf::st_drop_geometry()
INI <- "20150401"; FIN <- "20261115"
PAR <- c("PRECTOTCORR","T2M","T2M_MAX","T2M_MIN","RH2M","ALLSKY_SFC_SW_DWN","WS2M")
dir.create("data/raw/nasa_power", showWarnings = FALSE)
bajar <- function(i) {
  f <- sprintf("data/raw/nasa_power/power_%s.rds", m$cod_mpio[i]); if (file.exists(f)) return(invisible())
  url <- sprintf("https://power.larc.nasa.gov/api/temporal/daily/point?parameters=%s&community=AG&longitude=%.4f&latitude=%.4f&start=%s&end=%s&format=JSON",
                 paste(PAR, collapse = ","), m$lon[i], m$lat[i], INI, FIN)
  for (k in 1:4) {
    r <- try(fromJSON(url, simplifyVector = TRUE), silent = TRUE)
    if (!inherits(r, "try-error") && !is.null(r$properties$parameter)) {
      p <- r$properties$parameter
      d <- data.frame(fecha = as.Date(names(p[[1]]), "%Y%m%d"), lapply(p, unlist), row.names = NULL)
      d[d == -999] <- NA
      d$cod_mpio <- m$cod_mpio[i]; d$elev_power <- r$geometry$coordinates[3]
      saveRDS(d, f); return(invisible())
    }
    Sys.sleep(5 * k)
  }
  message("FALLO: ", m$Municipio[i])
}
for (i in seq_len(nrow(m))) { bajar(i); if (i %% 25 == 0) message(i, "/", nrow(m)); Sys.sleep(0.5) }
d <- bind_rows(lapply(list.files("data/raw/nasa_power", "^power_.*rds$", full.names = TRUE), readRDS)) |>
  rename(Precipitacion = PRECTOTCORR, Temperatura_Media = T2M, Temperatura_Maxima = T2M_MAX, Temperatura_Minima = T2M_MIN,
         Humedad_Relativa = RH2M, Radiacion_Solar = ALLSKY_SFC_SW_DWN, Viento = WS2M, Elevacion = elev_power)
saveRDS(d, "data/processed/power_diario.rds")
message("POWER: ", nrow(d), " filas, ", n_distinct(d$cod_mpio), " municipios, ", min(d$fecha), " a ", max(d$fecha))
