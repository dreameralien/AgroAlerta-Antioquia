# AgroAlerta Antioquia

Modelo de IA para predecir la humedad del suelo (NASA SMAP) y clasificar el riesgo hídrico en zonas agrícolas de Antioquia.
Proyecto final – Inteligencia Artificial y Big Data.

> **Aviso:** la descarga de SMAP quedó incompleta (solo algunas ventanas de ~60 días); los resultados son preliminares.

## Estructura
- `AgroAlerta_Antioquia.Rmd`: informe completo (código + texto).
- `scripts/`: pipeline de descarga e integración de datos (01–06) y `render.sh`.
- `data/raw`, `data/processed`: datos de entrada y procesados.
- `figures/`, `outputs/`: figuras y reportes compilados (HTML/PDF).

## Reproducir
```bash
scripts/render.sh all   # desde la raíz; requiere R, rmarkdown, pandoc y LaTeX
```
Los modelos se guardan en `outputs/cache/` (ignorado en git); si falta, se reentrenan al compilar.
