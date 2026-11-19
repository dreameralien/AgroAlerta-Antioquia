# 02_smap_descarga.R -- NASA SMAP Enhanced L3 (SPL3SMP_E v006), humedad superficial del suelo
# Estrategia: cada granulo global pesa ~550 MB. Se usa el servicio Harmony de NASA para
# recortar por caja (bbox) de Antioquia en el servidor (~2 MB/dia) y se extrae a una tabla
# compacta por bloque de dias (data/raw/smap/smap_pixeles_<inicio>.rds). Los .nc4 se borran.
# Requiere ~/.netrc con credenciales Earthdata. Reanudable: omite bloques ya procesados.
suppressPackageStartupMessages({library(hdf5r); library(sf); library(jsonlite)})
args  <- commandArgs(TRUE)
INI   <- as.Date(if (length(args) >= 1) args[1] else "2015-04-01")
FIN   <- as.Date(if (length(args) >= 2) args[2] else "2026-11-15")
BLOQ  <- 60                                        # dias por trabajo Harmony
CID   <- "C2938664763-NSIDC_CPRD"                  # SPL3SMP_E v006
DIR   <- "data/raw/smap"; TMP <- file.path(tempdir(), "smap"); dir.create(TMP, showWarnings = FALSE)
CJ    <- file.path(TMP, "cookies.txt")
sh <- function(...) system2("curl", shQuote(c("-s", "-L", "-n", "-b", CJ, "-c", CJ, ...)), stdout = TRUE, stderr = FALSE)

# --- Coordenadas de la malla EASE-Grid 2.0 (EPSG:6933), 9 km --------------------------------
coords_malla <- function(f) {
  g <- expand.grid(x = f[["x_global"]]$read(), y = f[["y_global"]]$read())
  xy <- st_coordinates(st_transform(st_as_sf(g, coords = c("x", "y"), crs = 6933), 4326))
  data.frame(ix = rep(seq_along(unique(g$x)), times = 1), iy = rep(seq_along(unique(g$y)), each = length(unique(g$x))),
             lon = xy[, 1], lat = xy[, 2])
}

# --- Extrae AM y PM de un archivo recortado -------------------------------------------------
leer_nc <- function(path, fecha) {
  f <- H5File$new(path, mode = "r"); on.exit(f$close_all())
  m <- coords_malla(f)
  for (h in c("AM", "PM")) {
    g <- f[[paste0("Soil_Moisture_Retrieval_Data_", h)]]; s <- if (h == "AM") "" else "_pm"
    sm <- as.vector(g[[paste0("soil_moisture", s)]]$read()); q <- as.vector(g[[paste0("retrieval_qual_flag", s)]]$read())
    sm[sm <= -9990] <- NA
    m[[paste0("sm_", tolower(h))]] <- sm; m[[paste0("q_", tolower(h))]] <- q
  }
  m$fecha <- fecha
  m[!(is.na(m$sm_am) & is.na(m$sm_pm)), ]          # descarta pixeles sin recuperacion
}

procesar_bloque <- function(ini, fin) {
  out <- file.path(DIR, sprintf("smap_pixeles_%s.rds", format(ini, "%Y%m%d")))
  if (file.exists(out)) return(invisible("ya existe"))
  url <- sprintf(paste0("https://harmony.earthdata.nasa.gov/%s/ogc-api-coverages/1.0.0/collections/all/coverage/rangeset?",
        "subset=lat(5.4:8.9)&subset=lon(-77.2:-73.8)&subset=time(%%22%sT00:00:00Z%%22:%%22%sT23:59:59Z%%22)&format=application/x-netcdf4&forceAsync=true"),
        CID, ini, fin)
  job <- fromJSON(paste(sh(url), collapse = ""))
  if (is.null(job$jobID)) stop("Harmony no creo el trabajo: ", paste(unlist(job), collapse = " "))
  jurl <- paste0("https://harmony.earthdata.nasa.gov/jobs/", job$jobID, "?linktype=https")
  repeat { Sys.sleep(15); st <- fromJSON(paste(sh(jurl), collapse = "")); if (!st$status %in% c("running", "accepted", "running_with_errors")) break }
  if (!st$status %in% c("successful", "complete_with_errors")) stop("Trabajo ", st$status, ": ", st$message)
  # los enlaces pueden estar paginados
  enl <- st$links; enl <- enl[enl$rel == "data", ]; pag <- 1
  while (!is.null(st$paging) && !is.null(st$paging$nextPage) && pag < 20) {
    pag <- pag + 1; st <- fromJSON(paste(sh(paste0(jurl, "&page=", pag)), collapse = "")); enl <- rbind(enl[, c("href","rel")], st$links[st$links$rel == "data", c("href","rel")]) }
  res <- lapply(enl$href, function(u) {
    fecha <- as.Date(sub(".*_(\\d{8})_R.*", "\\1", basename(u)), "%Y%m%d")
    dest <- file.path(TMP, paste0(format(fecha, "%Y%m%d"), ".nc4"))
    for (k in 1:3) { sh("-o", dest, u); if (file.exists(dest) && file.size(dest) > 1e5) break; Sys.sleep(5) }
    r <- tryCatch(leer_nc(dest, fecha), error = function(e) { message("  fallo ", basename(u), ": ", conditionMessage(e)); NULL })
    unlink(dest); r
  })
  d <- do.call(rbind, res); saveRDS(d, out)
  message(format(ini), " -> ", format(fin), ": ", length(enl$href), " granulos, ", nrow(d), " filas")
}

ini <- seq(INI, FIN, by = BLOQ)
for (i in seq_along(ini)) {
  fin <- min(ini[i] + BLOQ - 1, FIN)
  ok <- FALSE
  for (k in 1:3) { r <- try(procesar_bloque(ini[i], fin), silent = TRUE); if (!inherits(r, "try-error")) { ok <- TRUE; break }; message("  reintento ", k, ": ", conditionMessage(attr(r, "condition"))); Sys.sleep(30) }
  if (!ok) message("BLOQUE FALLIDO: ", ini[i])
}
message("SMAP terminado")
