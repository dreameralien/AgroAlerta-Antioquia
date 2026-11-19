# 04_ideam_descarga.R -- IDEAM (datos.gov.co): precipitacion (s54a-sgyg), temperatura del aire (sbwg-7ju4)
# y humedad del aire (uext-mhny) de estaciones de Antioquia, agregadas a nivel diario en el servidor (SoQL).
# Uso: Rscript 04_ideam_descarga.R [inicio] [fin]   Salida: data/raw/ideam/<var>_<AAAA-MM>.rds (reanudable)
suppressPackageStartupMessages({library(curl); library(jsonlite)})
a <- commandArgs(TRUE); INI <- as.Date(if (length(a) >= 1) a[1] else "2019-01-01"); FIN <- as.Date(if (length(a) >= 2) a[2] else "2025-12-31")
FUENTES <- list(prec = list(id = "s54a-sgyg", f = "sum"), temp = list(id = "sbwg-7ju4", f = "avg"), hr = list(id = "uext-mhny", f = "avg"))
dir.create("data/raw/ideam", showWarnings = FALSE)
consulta <- function(id, fn, d0, d1) {
  q <- c(`$select` = sprintf("codigoestacion,municipio,date_trunc_ymd(fechaobservacion) as dia,%s(valorobservado) as v,count(*) as n", fn),
         `$where`  = sprintf("departamento='ANTIOQUIA' AND fechaobservacion between '%sT00:00:00' and '%sT23:59:59'", d0, d1),
         `$group`  = "codigoestacion,municipio,dia", `$limit` = "50000")
  h <- new_handle(timeout = 600, connecttimeout = 60)
  r <- curl_fetch_memory(paste0("https://www.datos.gov.co/resource/", id, ".json?", paste0(names(q), "=", sapply(q, URLencode, reserved = TRUE), collapse = "&")), handle = h)
  if (r$status_code != 200) stop("HTTP ", r$status_code)
  fromJSON(rawToChar(r$content))
}
meses <- seq(as.Date(format(INI, "%Y-%m-01")), FIN, by = "month")
for (v in names(FUENTES)) for (i in seq_along(meses)) {
  d0 <- meses[i]; d1 <- seq(d0, by = "month", length.out = 2)[2] - 1
  f <- sprintf("data/raw/ideam/%s_%s.rds", v, format(d0, "%Y-%m")); if (file.exists(f)) next
  t0 <- Sys.time(); ok <- FALSE
  for (k in 1:3) { r <- try(consulta(FUENTES[[v]]$id, FUENTES[[v]]$f, d0, d1), silent = TRUE); if (!inherits(r, "try-error")) { ok <- TRUE; break }; Sys.sleep(15) }
  if (!ok) { message("FALLO ", v, " ", format(d0, "%Y-%m")); next }
  saveRDS(r, f); message(v, " ", format(d0, "%Y-%m"), ": ", NROW(r), " filas (", round(difftime(Sys.time(), t0, units = "secs")), " s)")
}
message("IDEAM terminado")
