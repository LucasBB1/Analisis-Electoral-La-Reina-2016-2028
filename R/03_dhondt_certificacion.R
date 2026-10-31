# =============================================================================
# Análisis Político-Electoral La Reina
# 03_dhondt_certificacion.R
# -----------------------------------------------------------------------------
# Propósito
#   Certificar el motor D'Hondt (R/funciones_dhondt.R): a partir solo de los
#   votos por candidato, debe reproducir los concejales que el SERVEL marca
#   como electos en 2016, 2021 y 2024. Además reconstruye la trayectoria de
#   los integrantes del concejo 2024 (para el límite de reelección).
#
# Entradas  data/processed/lr_votos_comuna_bloques.rds  (de 02_...)
# Salidas   output/tablas/03_*.csv
# =============================================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(stringr)
})
source(file.path("R", "funciones_dhondt.R"))

DIR_PROC <- file.path("data", "processed")
DIR_TAB  <- file.path("output", "tablas")
ESCANOS  <- 8   # concejales de La Reina en 2016, 2021 y 2024

votos <- readRDS(file.path(DIR_PROC, "lr_votos_comuna_bloques.rds"))
clave <- function(x) stringi::stri_trans_general(x, "Latin-ASCII") |> toupper() |> str_squish()

concejales_ids <- c("2016_concejales", "2021_concejales", "2024_concejales")


# 1. Escrutinio simulado y comparación con el SERVEL ---------------------------

escrutinio <- lapply(concejales_ids, function(i) {
  cand <- votos |>
    filter(id == i, tipo_voto == "valido") |>
    select(id, lista, pacto, subpacto, partido, codigo, bloque, candidato, votos, marca)
  asignar_concejo(cand, ESCANOS) |>
    mutate(electo_servel = !is.na(marca) & str_detect(clave(marca), "CONCEJAL|ELECTO"))
}) |> bind_rows()

certificacion <- escrutinio |>
  group_by(id) |>
  summarise(electos_servel = sum(electo_servel),
            electos_modelo = sum(electo),
            coincidencias  = sum(electo & electo_servel),
            discrepancias  = sum(electo != electo_servel),
            .groups = "drop") |>
  mutate(certificado = discrepancias == 0 & electos_servel == ESCANOS)

discrepancias <- escrutinio |> filter(electo != electo_servel)

electos <- escrutinio |>
  filter(electo_servel) |>
  arrange(id, desc(votos)) |>
  select(id, lista, pacto, subpacto, partido, codigo, bloque, candidato, votos, electo_modelo = electo)


# 2. Escaños por lista y por bloque -------------------------------------------

escanos_lista <- escrutinio |>
  group_by(id, unidad_lista, pacto) |>
  summarise(votos = sum(votos), escanos = first(escanos_lista), .groups = "drop") |>
  group_by(id) |>
  mutate(pct = round(100 * votos / sum(votos), 2)) |>
  ungroup() |>
  arrange(id, desc(votos))

escanos_bloque <- escrutinio |>
  filter(electo) |>
  count(id, bloque, name = "escanos") |>
  pivot_wider(names_from = id, values_from = escanos, values_fill = 0)


# 3. Concejo 2024 y trayectoria de sus integrantes ----------------------------
# Trayectoria = elecciones de concejales (2016, 2021, 2024) en que la persona
# resultó electa según el SERVEL. Se identifica por nombre completo
# normalizado; se revisan además coincidencias de apellidos para no perder
# casos con nombres escritos distinto entre años.

electos_k <- electos |>
  mutate(anio = substr(id, 1, 4), k = clave(candidato),
         apellidos = word(k, -2, -1))

concejo_2024 <- electos_k |>
  filter(anio == "2024") |>
  rowwise() |>
  mutate(
    electo_2016 = any(electos_k$anio == "2016" & electos_k$apellidos == apellidos),
    electo_2021 = any(electos_k$anio == "2021" & electos_k$apellidos == apellidos)) |>
  ungroup() |>
  mutate(periodos_consecutivos = 1L + electo_2021 + (electo_2021 & electo_2016),
         trayectoria = str_c(if_else(electo_2016, "2016 · ", ""),
                             if_else(electo_2021, "2021 · ", ""), "2024"),
         # La Ley 21.238 limita a tres períodos consecutivos; la aplicación
         # al caso individual se verifica contra el texto legal en la sección 6.
         alcanza_tres_periodos = periodos_consecutivos >= 3) |>
  select(candidato, partido, codigo, votos, trayectoria, periodos_consecutivos, alcanza_tres_periodos)


# 4. Exportación --------------------------------------------------------------

write_excel_csv2(certificacion, file.path(DIR_TAB, "03_certificacion_dhondt.csv"))
write_excel_csv2(electos,       file.path(DIR_TAB, "03_concejales_electos_2016_2024.csv"))
write_excel_csv2(escanos_lista, file.path(DIR_TAB, "03_escanos_por_lista.csv"))
write_excel_csv2(escanos_bloque,file.path(DIR_TAB, "03_escanos_por_bloque.csv"))
write_excel_csv2(concejo_2024,  file.path(DIR_TAB, "03_concejo_2024_trayectoria.csv"))

cat("\n== Certificación del motor D'Hondt ==\n");   print(certificacion)
if (nrow(discrepancias) > 0) {
  cat("\n!! Discrepancias modelo vs SERVEL:\n")
  print(select(discrepancias, id, lista, integrante, candidato, votos, electo, electo_servel))
}
cat("\n== Escaños por bloque ==\n");                print(escanos_bloque)
cat("\n== Concejo 2024 y trayectoria ==\n");  print(concejo_2024, width = Inf)
