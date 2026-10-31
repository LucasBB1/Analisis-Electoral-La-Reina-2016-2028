# =============================================================================
# Análisis Político-Electoral La Reina
# 02_homologar_bloques.R
# -----------------------------------------------------------------------------
# Propósito
#   1. Asignar a cada fila de votos del SERVEL un código de partido (UDI, RN,
#      PREP, FA...), incluidas las candidaturas independientes, con reglas
#      explícitas y auditables.
#   2. Llevar cada código a su bloque y familia ideológica según la tabla de
#      homologación del autor (data/homologacion/bloques.csv).
#   3. Contrastar la Base Madre partido por partido.
#   4. Generar tablas de votación por bloque y familia para concejales,
#      diputados y consejeros regionales.
#
# Entradas
#   data/processed/lr_votos_comuna.rds, lr_votos_mesa.rds  (de 01_...)
#   data/homologacion/bloques.csv                 código -> bloque/familia
#   data/homologacion/pactos_independientes.csv   bloque de independientes de
#                                                 pactos sin cupo informado
#   data/base_madre/*.xlsx
#
# Salidas
#   data/processed/lr_votos_mesa_bloques.rds
#   data/processed/lr_votos_comuna_bloques.rds
#   output/tablas/02_*.csv
#
# Criterio para candidaturas independientes (decisión del autor, oct-2026)
#   a) Independiente en cupo de un partido, en un subpacto de un partido o en
#      una lista de un solo partido -> se imputa a ese partido.
#   b) Independiente dentro de un pacto de varios partidos sin cupo informado
#      por el SERVEL -> bloque del pacto (pactos_independientes.csv). Si el
#      pacto es de un solo partido a nivel nacional, se declara su código en
#      la columna 'codigo' de ese archivo.
#   c) Independiente en listas sin partidos o fuera de pacto -> categoría
#      'Independientes', sin orientación ideológica.
# =============================================================================


# 0. Configuración ------------------------------------------------------------

suppressPackageStartupMessages({
  library(readxl)
  library(readr)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(purrr)
})

DIR_PROC <- file.path("data", "processed")
DIR_HOM  <- file.path("data", "homologacion")
DIR_BASE <- file.path("data", "base_madre")
DIR_TAB  <- file.path("output", "tablas")

if (!file.exists(file.path(DIR_PROC, "lr_votos_comuna.rds"))) {
  stop("Primero ejecuta R/01_importar_validar.R")
}

votos_comuna <- readRDS(file.path(DIR_PROC, "lr_votos_comuna.rds"))
votos_mesa   <- readRDS(file.path(DIR_PROC, "lr_votos_mesa.rds"))
catalogo     <- readRDS(file.path(DIR_PROC, "catalogo.rds"))
bloques      <- read_csv(file.path(DIR_HOM, "bloques.csv"), show_col_types = FALSE)
pactos_ind   <- read_csv(file.path(DIR_HOM, "pactos_independientes.csv"), show_col_types = FALSE)

clave <- function(x) {
  stringi::stri_trans_general(x, "Latin-ASCII") |> toupper() |> str_squish()
}

ids_lista <- catalogo$id[catalogo$familia == "Lista"]


# 1. Diccionarios -------------------------------------------------------------

# 1a. Nombre oficial del partido (tal como lo escribe el SERVEL) -> código.
dic_partidos <- tribble(
  ~patron,                                   ~codigo,
  "^UNION DEMOCRATA INDEPENDIENTE$",         "UDI",
  "^RENOVACION NACIONAL$",                   "RN",
  "^EVOLUCION POLITICA$",                    "EVOPOLI",
  "^PARTIDO REGIONALISTA INDEPENDIENTE",     "PRI",
  "^PARTIDO REPUBLICANO",                    "PREP",
  "^PARTIDO SOCIAL CRISTIANO",               "PSC",
  "^PARTIDO NACIONAL LIBERTARIO",            "PNL",
  "^PARTIDO DE LA GENTE",                    "PDG",
  "^PARTIDO DEMOCRATA CRISTIANO",            "PDC",
  "^PARTIDO DEMOCRATAS",                     "DEMOCRATAS",
  "^AMARILLOS",                              "AMARILLOS",
  "^PARTIDO SOCIALISTA",                     "PS",
  "^PARTIDO POR LA DEMOCRACIA",              "PPD",
  "^PARTIDO RADICAL",                        "PR",
  "^PARTIDO LIBERAL",                        "PL",
  "^CIUDADANOS$",                            "CIUDADANOS",
  "^PARTIDO PROGRESISTA",                    "PRO",
  "^FEDERACION REGIONALISTA VERDE SOCIAL",   "FRVS",
  "^PARTIDO COMUNISTA",                      "PC",
  "^FRENTE AMPLIO$",                         "FA",
  "^CONVERGENCIA SOCIAL$",                   "CS",
  "^REVOLUCION DEMOCRATICA$",                "RD",
  "^COMUNES$",                               "COMUNES",
  "^PARTIDO ACCION HUMANISTA",               "AH",
  "^PARTIDO HUMANISTA",                      "PH",
  "^(PARTIDO )?IGUALDAD$",                   "PI",
  "^PARTIDO ECOLOGISTA VERDE",               "PEV",
  "^PARTIDO ALIANZA VERDE POPULAR",          "PAVP",
  "^UNION PATRIOTICA",                       "UPA",
  "^PODER$",                                 "PODER",
  "^CENTRO UNIDO$",                          "CU",
  "^PAIS$",                                  "PAIS"
)

nombre_a_codigo <- function(x) {
  k <- clave(x)
  out <- rep(NA_character_, length(k))
  for (i in seq_len(nrow(dic_partidos))) {
    hit <- is.na(out) & !is.na(k) & str_detect(k, dic_partidos$patron[i])
    out[hit] <- dic_partidos$codigo[i]
  }
  out
}

# 1b. Siglas o palabras clave que identifican a un partido dentro del nombre
#     de un subpacto o de un pacto (p. ej. "SUBPACTO PCCH E INDEPENDIENTES").
dic_tokens <- c(
  UDI = "\\bUDI\\b|UNION DEMOCRATA", RN = "\\bRN\\b|RENOVACION NACIONAL",
  EVOPOLI = "EVOPOLI|EVOLUCION POLITICA", PRI = "\\bPRI\\b",
  PREP = "REPUBLICANO", PSC = "SOCIAL CRISTIANO",
  PDC = "\\bPDC\\b|\\bDC\\b|DEMOCRATA CRISTIANO", PS = "\\bPS\\b|SOCIALISTA",
  PPD = "\\bPPD\\b", PR = "\\bPRSD\\b|RADICAL", PL = "LIBERAL",
  CIUDADANOS = "CIUDADANOS", PRO = "\\bPRO\\b", FRVS = "REGIONALISTA",
  PC = "COMUNISTA|\\bPCCH\\b", FA = "FRENTE AMPLIO", CS = "CONVERGENCIA",
  RD = "REVOLUCION DEMOCRATICA", COMUNES = "\\bCOMUNES\\b",
  AH = "ACCION HUMANISTA", PH = "HUMANIST|HUMANICEMOS", PI = "IGUALDAD",
  PEV = "ECOLOGISTA"
)

# Devuelve el código solo si el texto identifica a un único partido.
token_a_codigo <- function(x) {
  map_chr(clave(x), function(k) {
    if (is.na(k)) return(NA_character_)
    hits <- names(dic_tokens)[str_detect(k, dic_tokens)]
    if ("AH" %in% hits) hits <- setdiff(hits, "PH")   # "ACCION HUMANISTA"
    if (length(hits) == 1) hits else NA_character_
  })
}


# 2. Asignación de códigos ----------------------------------------------------
# Se resuelve sobre las combinaciones distintas de (papeleta, lista, pacto,
# subpacto, partido) y luego se une a las tablas de votos.

combos <- votos_comuna |>
  filter(id %in% ids_lista, tipo_voto == "valido") |>
  distinct(id, lista, pacto, subpacto, partido)

combos <- combos |>
  mutate(
    k_partido = clave(partido),
    es_indep  = str_detect(k_partido, "^INDEPENDIENTE"),
    # Regla 1: partido con nombre propio
    cod_r1 = if_else(!es_indep, nombre_a_codigo(partido), NA_character_),
    # Regla 2: "INDEPENDIENTE <nombre de partido>" = cupo de ese partido
    resto  = str_remove(k_partido, "^INDEPENDIENTES? "),
    cod_r2 = if_else(es_indep & !str_detect(resto, "^(LISTA|SUBPACTO)|^-|^INDEPENDIENTE"),
                     nombre_a_codigo(resto), NA_character_),
    # Regla 3: subpacto identificable
    cod_r3 = if_else(es_indep & !is.na(subpacto), token_a_codigo(subpacto), NA_character_)
  ) |>
  group_by(id, lista) |>
  # Regla 4: lista en que compite un solo partido
  mutate(cod_r4 = {
    p <- unique(na.omit(cod_r1))
    if (length(p) == 1) p else NA_character_
  }) |>
  ungroup() |>
  mutate(
    # Regla 5: pacto cuyo nombre identifica a un único partido
    cod_r5 = token_a_codigo(pacto)) |>
  # Regla 5b: pacto de un solo partido a nivel nacional que en La Reina solo
  # llevó independientes (declarado en pactos_independientes.csv, columna codigo)
  left_join(pactos_ind |> filter(!is.na(codigo)) |> select(id, lista, cod_r5b = codigo),
            by = c("id", "lista")) |>
  mutate(
    codigo = case_when(
      !es_indep        ~ cod_r1,
      !is.na(cod_r2)   ~ cod_r2,
      !is.na(cod_r3)   ~ cod_r3,
      !is.na(cod_r4)   ~ cod_r4,
      !is.na(cod_r5)   ~ cod_r5,
      !is.na(cod_r5b)  ~ cod_r5b,
      TRUE             ~ paste0("IND_", lista)),
    regla = case_when(
      !es_indep        ~ "1 partido",
      !is.na(cod_r2)   ~ "2 cupo de partido",
      !is.na(cod_r3)   ~ "3 subpacto",
      !is.na(cod_r4)   ~ "4 lista de un solo partido",
      !is.na(cod_r5)   ~ "5 nombre del pacto",
      !is.na(cod_r5b)  ~ "5b pacto de un partido (declarado)",
      TRUE             ~ "6 independiente sin partido")
  )

sin_codigo <- combos |> filter(is.na(codigo))
if (nrow(sin_codigo) > 0) {
  print(sin_codigo |> select(id, lista, partido))
  stop("Hay partidos sin código: agrégalos a dic_partidos.")
}

# Bloque y familia
combos <- combos |>
  left_join(select(bloques, codigo, bloque, familia), by = "codigo") |>
  left_join(pactos_ind |> filter(is.na(codigo)) |>
              select(id, lista, bloque_p = bloque, familia_p = familia),
            by = c("id", "lista")) |>
  mutate(
    criterio_bloque = case_when(
      !is.na(bloque)                         ~ "tabla de bloques",
      str_starts(codigo, "IND_") & !is.na(bloque_p) ~ "independiente en pacto (pactos_independientes.csv)",
      TRUE                                   ~ "independiente sin partido"),
    bloque  = coalesce(bloque,  bloque_p,  "Independientes"),
    familia = coalesce(familia, familia_p, "Independiente")) |>
  select(id, lista, pacto, subpacto, partido, codigo, regla, bloque, familia, criterio_bloque)

sin_bloque <- combos |> filter(!str_starts(codigo, "IND_"), !codigo %in% bloques$codigo)
if (nrow(sin_bloque) > 0) {
  print(distinct(sin_bloque, codigo))
  stop("Hay códigos de partido sin bloque en bloques.csv.")
}

# Une la clasificación a los votos (comuna y mesa)
unir_bloques <- function(df) {
  df |>
    left_join(combos, by = c("id", "lista", "pacto", "subpacto", "partido")) |>
    mutate(
      codigo  = if_else(tipo_voto != "valido", toupper(tipo_voto), codigo),
      bloque  = if_else(tipo_voto != "valido", "Nulo/Blanco", bloque),
      familia = if_else(tipo_voto != "valido", "Nulo/Blanco", familia))
}

comuna_b <- votos_comuna |> filter(id %in% ids_lista) |> unir_bloques()

# En los archivos por mesa el SERVEL a veces rotula distinto al partido de un
# independiente (p. ej. "INDEPENDIENTES" en vez de "INDEPENDIENTES-LISTA XP"),
# así que la clasificación se traslada por lista y candidato, que en
# 01_importar_validar.R se verificó idéntica entre ambos archivos (V1).
mapa_cand <- comuna_b |>
  filter(tipo_voto == "valido") |>
  distinct(id, lista, k_cand = clave(candidato), codigo, bloque, familia)
stopifnot(!anyDuplicated(select(mapa_cand, id, lista, k_cand)))

mesa_b <- votos_mesa |>
  filter(id %in% ids_lista) |>
  mutate(k_cand = clave(candidato)) |>
  left_join(mapa_cand, by = c("id", "lista", "k_cand")) |>
  mutate(
    codigo  = if_else(tipo_voto != "valido", toupper(tipo_voto), codigo),
    bloque  = if_else(tipo_voto != "valido", "Nulo/Blanco", bloque),
    familia = if_else(tipo_voto != "valido", "Nulo/Blanco", familia)) |>
  select(-k_cand)

stopifnot(!any(is.na(comuna_b$bloque)), !any(is.na(mesa_b$bloque)))
saveRDS(comuna_b, file.path(DIR_PROC, "lr_votos_comuna_bloques.rds"))
saveRDS(mesa_b,   file.path(DIR_PROC, "lr_votos_mesa_bloques.rds"))


# 3. Agregados por partido, bloque y familia ----------------------------------

agregar <- function(df, nivel) {
  df |>
    filter(tipo_voto == "valido") |>
    group_by(id, categoria = .data[[nivel]]) |>
    summarise(votos = sum(votos), .groups = "drop_last") |>
    mutate(pct = 100 * votos / sum(votos)) |>
    ungroup() |>
    left_join(select(catalogo, id, eleccion), by = "id") |>
    relocate(eleccion, .after = id)
}

por_partido <- agregar(comuna_b, "codigo")
por_bloque  <- agregar(comuna_b, "bloque")
por_familia <- agregar(comuna_b, "familia")


# 4. Base Madre vs SERVEL, partido por partido --------------------------------
# Los encabezados de la Base Madre son códigos de partido; las columnas
# INDEPENDIENTES_<lista> corresponden a IND_<lista> y
# INDEPENDIENTES_FUERA_DE_PACTO a la lista ZZI.

ruta_base <- list.files(DIR_BASE, pattern = "\\.xlsx$", full.names = TRUE)[1]

leer_base_lista <- function(hoja) {
  raw <- suppressMessages(read_excel(ruta_base, sheet = hoja, col_names = FALSE,
                                     col_types = "text", .name_repair = "minimal"))
  titulos <- as.character(unlist(raw[1, ])); cols <- as.character(unlist(raw[2, ]))
  d <- raw[-(1:2), ]; d <- d[!is.na(d[[1]]) & !str_detect(d[[1]], "(?i)^total"), ]
  out <- list(); titulo <- NA
  for (j in 5:ncol(raw)) {
    if (!is.na(titulos[j])) titulo <- titulos[j]
    if (is.na(titulo) || is.na(cols[j]) || cols[j] %in% c("TOTAL_VALIDOS", "NULOS_BLANCOS")) next
    out[[length(out) + 1]] <- tibble(base_titulo = titulo, columna = cols[j],
                                     votos = suppressWarnings(as.numeric(d[[j]])))
  }
  bind_rows(out)
}

base_partidos <- map_dfr(paste0("RES_", LETTERS[1:9]), leer_base_lista) |>
  mutate(k_titulo = clave(base_titulo)) |>
  inner_join(catalogo |> filter(familia == "Lista") |>
               transmute(id, k_titulo = clave(base_titulo)), by = "k_titulo") |>
  mutate(codigo = case_when(
    columna == "INDEPENDIENTES_FUERA_DE_PACTO" ~ "IND_ZZI",
    str_starts(columna, "INDEPENDIENTES_")     ~ str_replace(columna, "^INDEPENDIENTES_", "IND_"),
    TRUE                                       ~ columna)) |>
  group_by(id, codigo) |>
  summarise(votos_base = sum(votos, na.rm = TRUE), .groups = "drop")

verif_partidos <- por_partido |>
  select(id, eleccion, codigo = categoria, votos_servel = votos) |>
  # En la Base Madre, la columna RD de CORE 2017 incluye a los independientes
  # de la lista del pacto Frente Amplio (codificados FA por el SERVEL).
  mutate(codigo = if_else(id == "2017_core" & codigo == "FA", "RD", codigo)) |>
  group_by(id, eleccion, codigo) |> summarise(votos_servel = sum(votos_servel), .groups = "drop") |>
  full_join(base_partidos, by = c("id", "codigo")) |>
  mutate(eleccion = coalesce(eleccion, catalogo$eleccion[match(id, catalogo$id)]),
         votos_servel = coalesce(votos_servel, 0L),
         votos_base   = coalesce(votos_base, 0),
         dif = votos_base - votos_servel,
         estado = if_else(dif == 0, "OK", "DIFERENCIA")) |>
  arrange(id, estado != "DIFERENCIA", codigo)


# 5. Exportación --------------------------------------------------------------

write_excel_csv2(combos,         file.path(DIR_TAB, "02_asignacion_codigos.csv"))
write_excel_csv2(por_partido,    file.path(DIR_TAB, "02_votos_por_partido.csv"))
write_excel_csv2(por_bloque,     file.path(DIR_TAB, "02_votos_por_bloque.csv"))
write_excel_csv2(por_familia,    file.path(DIR_TAB, "02_votos_por_familia.csv"))
write_excel_csv2(verif_partidos, file.path(DIR_TAB, "02_base_madre_vs_servel_partidos.csv"))

# Tablas anchas (una columna por elección) para lectura directa
ancha <- function(df, ids) {
  df |> filter(id %in% ids) |>
    select(eleccion, categoria, pct) |>
    mutate(pct = round(pct, 1)) |>
    pivot_wider(names_from = eleccion, values_from = pct, values_fill = 0)
}
tabla_concejales_bloques <- ancha(por_bloque,  c("2016_concejales", "2021_concejales", "2024_concejales"))
tabla_diputados_bloques <- ancha(por_bloque,  c("2017_diputados", "2021_diputados", "2025_diputados"))
tabla_concejales_familias <- ancha(por_familia, c("2016_concejales", "2021_concejales", "2024_concejales"))
tabla_core_familias <- ancha(por_familia, c("2017_core", "2021_core", "2024_core"))
write_excel_csv2(tabla_concejales_bloques, file.path(DIR_TAB, "02_concejales_por_bloque.csv"))
write_excel_csv2(tabla_diputados_bloques, file.path(DIR_TAB, "02_diputados_por_bloque.csv"))
write_excel_csv2(tabla_concejales_familias, file.path(DIR_TAB, "02_concejales_por_familia.csv"))
write_excel_csv2(tabla_core_familias, file.path(DIR_TAB, "02_core_por_familia.csv"))

cat("\n== Independientes: cómo se asignaron ==\n")
print(combos |> filter(str_detect(clave(partido), "^INDEPENDIENTE")) |>
        count(regla, criterio_bloque), n = Inf)
cat("\n== Base Madre vs SERVEL por partido (solo diferencias) ==\n")
print(verif_partidos |> filter(estado != "OK") |>
        select(eleccion, codigo, votos_base, votos_servel, dif), n = Inf)
cat("\n== Concejales por bloque (% válidos) ==\n"); print(tabla_concejales_bloques, n = Inf)
cat("\n== Diputados por bloque (% válidos) ==\n");  print(tabla_diputados_bloques, n = Inf)
