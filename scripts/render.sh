#!/usr/bin/env bash
# Compila el informe. Uso: scripts/render.sh [html|pdf|all]  (ejecutar desde la raíz del proyecto)
export RSTUDIO_PANDOC=${RSTUDIO_PANDOC:-/usr/lib/rstudio/resources/app/bin/quarto/bin/tools/x86_64}
fmt=${1:-html}
[ "$fmt" = html ] || [ "$fmt" = all ] && Rscript -e 'rmarkdown::render("AgroAlerta_Antioquia.Rmd", output_format="html_document", output_dir="outputs", quiet=TRUE)'
[ "$fmt" = pdf ] || [ "$fmt" = all ] && Rscript -e 'rmarkdown::render("AgroAlerta_Antioquia.Rmd", output_format="pdf_document", output_dir="outputs", quiet=TRUE)'
