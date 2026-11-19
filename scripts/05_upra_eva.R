# 05_upra_eva.R -- Evaluaciones Agropecuarias Municipales (EVA) 2019-2025, UPRA/MinAgricultura (datos.gov.co uejq-wxrr)
# Salida: data/raw/upra/eva_antioquia.rds y data/processed/eva_municipio.rds (cultivo predominante y area por municipio)
suppressPackageStartupMessages({library(jsonlite); library(dplyr)})
f <- "data/raw/upra/eva_antioquia.rds"
if (!file.exists(f)) {
  options(timeout = 600)
  where <- URLencode("upper(departamento)='ANTIOQUIA'", reserved = TRUE)
  pags <- list(); off <- 0
  repeat {   # paginacion: el servicio responde lento con consultas grandes
    u <- sprintf("https://www.datos.gov.co/resource/uejq-wxrr.json?$limit=5000&$offset=%d&$order=:id&$where=%s", off, where)
    for (k in 1:3) { r <- try(fromJSON(u), silent = TRUE); if (!inherits(r, "try-error")) break; Sys.sleep(10) }
    if (inherits(r, "try-error")) stop("EVA: fallo la descarga en offset ", off)
    if (!length(r)) break
    pags[[length(pags) + 1]] <- r; off <- off + 5000
    if (nrow(r) < 5000) break
  }
  d <- bind_rows(pags); saveRDS(d, f)
}
d <- readRDS(f) |>
  transmute(cod_mpio = c_digo_dane_municipio, anio = as.integer(a_o), grupo = grupo_cultivo, cultivo,
            area_sembrada = as.numeric(rea_sembrada), area_cosechada = as.numeric(rea_cosechada), produccion = as.numeric(producci_n))
# area sembrada anual por cultivo (suma de periodos) y cultivo predominante del ultimo trienio disponible
rec <- d |> filter(anio >= max(anio) - 2) |> group_by(cod_mpio, cultivo, grupo) |>
  summarise(area = sum(area_sembrada, na.rm = TRUE) / n_distinct(anio), .groups = "drop")
eva <- rec |> group_by(cod_mpio) |>
  summarise(Tipo_Cultivo = cultivo[which.max(area)], Grupo_Cultivo = grupo[which.max(area)],
            Area_Agricola_ha = sum(area), Prop_Cultivo_Dominante = max(area) / sum(area), N_Cultivos = n(), .groups = "drop")
saveRDS(eva, "data/processed/eva_municipio.rds")
message("EVA: ", nrow(d), " registros; municipios con datos: ", nrow(eva), "; anios ", min(d$anio), "-", max(d$anio))
print(count(eva, Tipo_Cultivo, sort = TRUE) |> head(8))
