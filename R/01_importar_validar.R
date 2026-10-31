# =============================================================================
# Análisis Político-Electoral La Reina
# 01_importar_validar.R
# -----------------------------------------------------------------------------
# Propósito
#   1. Importar los archivos oficiales del SERVEL (tal como se descargan) para
#      las 29 papeletas del período 2016-2025.
#   2. Extraer la comuna de La Reina a nivel de mesa y de comuna.
#   3. Validar la consistencia interna de los datos oficiales.
#   4. Contrastar la Base Madre (transcripción manual) contra el SERVEL.
#
# Entradas
#   data/raw/        archivos del SERVEL descomprimidos (.xlsx y .txt), en
#                    cualquier subcarpeta; se buscan por nombre de archivo.
#   data/base_madre/ Base_Maestra_Electoral_LaReina_2016-2025.xlsx
#
# Salidas
#   data/processed/lr_votos_mesa.rds        votos por mesa y candidato/opción
#   data/processed/lr_votos_comuna.rds      votos comunales oficiales
#   data/processed/lr_participacion_mesa.rds inscritos y votantes por mesa
#   data/processed/cache/                   un .rds por papeleta (caché)
#   output/tablas/01_*.csv                  resumen y tablas de validación
#
# Uso
#   Abrir el proyecto .Rproj (fija el directorio de trabajo en la raíz) y
#   ejecutar el script completo. La primera corrida lee ~2 GB de archivos y
#   puede tardar varios minutos; las siguientes usan la caché.
#
# Paquetes: readxl, readr, dplyr, tidyr, stringr, stringi, purrr
# Requiere R >= 4.2 (lectura nativa en UTF-8 en Windows).
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

COMUNA        <- "LA REINA"
DIR_RAW       <- file.path("data", "raw")
DIR_BASE      <- file.path("data", "base_madre")
DIR_PROC      <- file.path("data", "processed")
DIR_CACHE     <- file.path(DIR_PROC, "cache")
DIR_TAB       <- file.path("output", "tablas")
FORZAR_LECTURA <- FALSE   # TRUE = ignorar la caché y releer los originales

if (!dir.exists(DIR_RAW)) {
  stop("No se encuentra data/raw. Abre el proyecto .Rproj antes de ejecutar.")
}
for (d in c(DIR_PROC, DIR_CACHE, DIR_TAB)) dir.create(d, recursive = TRUE, showWarnings = FALSE)


# 1. Catálogo de papeletas ----------------------------------------------------
# Una fila por papeleta. 'xlsx' y 'txt' son los nombres de archivo del SERVEL.
# Si 'txt' no es NA, los votos por mesa se leen del .txt (el .xlsx de esas
# elecciones trae solo la hoja comunal); si es NA, se leen de la hoja
# 'hoja_mesa' del .xlsx. 'base_titulo' y 'base_seg' indican dónde está la
# papeleta en la Base Madre (título del bloque y vuelta dentro del bloque).

catalogo <- tribble(
  ~id,                ~fecha,    ~eleccion,                        ~familia,     ~xlsx,                                                   ~txt,                                         ~hoja_mesa,          ~base_titulo,                                                        ~base_seg,
  "2016_alcalde",     "2016-10", "Alcalde 2016",                   "Figura",     "2016_10_Alcaldes_DatosEleccion.xlsx",                   NA,                                           "^Votación$",        "Alcalde 2016",                                                      1,
  "2016_concejales",  "2016-10", "Concejales 2016",                "Lista",      "2016_10_Concejales_DatosEleccion.xlsx",                 "2016_10_Concejales_Votacion.txt",            NA,                  "Concejales 2016",                                                   1,
  "2017_core",        "2017-11", "CORE 2017",                      "Lista",      "2017_11_ConsejerosRegionales_DatosEleccion.xlsx",       "2017_11_ConsejerosRegionales_Votacion.txt",  NA,                  "Consejeros Regionales (CORE) 2017",                                 1,
  "2017_diputados",   "2017-11", "Diputados 2017",                 "Lista",      "2017_11_Diputados_Datos_Eleccion.xlsx",                 "2017_11_Diputados_Votacion.txt",             NA,                  "Diputados/as 2017",                                                 1,
  "2017_pres_1v",     "2017-11", "Presidencial 2017 1V",           "Figura",     "2017_11_Presidencial_Datos_Eleccion.xlsx",              NA,                                           "^Votación en Chile", "Presidente 2017",                                                   1,
  "2017_pres_2v",     "2017-12", "Presidencial 2017 2V",           "Figura",     "2017_12_Presidencial_2V_Datos_Eleccion.xlsx",           NA,                                           "^Votación en Chile", "Presidente 2017",                                                   2,
  "2020_pleb_entrada","2020-10", "Plebiscito 2020 (entrada)",      "Binaria",    "2020_PlebiscitoConstitucionPolitica_DatosEleccion.xlsx", NA,                                          "^Votación en Chile", "Apruebo-Rechazo 2020",                                              1,
  "2020_pleb_organo", "2020-10", "Plebiscito 2020 (tipo órgano)",  "Binaria",    "2020_PlebiscitoTipoOrgano_DatosEleccion.xlsx",          NA,                                           "^Votación en Chile", "Tipo de Órgano (Convención Mixta-Convención Constituyente) 2020",   1,
  "2021_alcalde",     "2021-05", "Alcalde 2021",                   "Figura",     "2021_05_Alcaldes_Datos_Eleccion.xlsx",                  NA,                                           "^Votación$",        "Alcalde 2021",                                                      1,
  "2021_concejales",  "2021-05", "Concejales 2021",                "Lista",      "2021_05_Concejales_DatosEleccion.xlsx",                 "2021_05_Concejales_Votacion.txt",            NA,                  "Concejales 2021",                                                   1,
  "2021_convencionales","2021-05","Convencionales 2021",           "Lista",      "2021_05_CCG_Datos_Eleccion.xlsx",                       "2021_05_CCG_Votacion.txt",                   NA,                  "Convencionales Constituyentes 2021",                                1,
  "2021_gore_1v",     "2021-05", "GORE 2021 1V",                   "Figura",     "2021_05_GobernadoresRegionales_Datos_Eleccion.xlsx",    NA,                                           "^Votación$",        "Gobernadores Regionales (GORE) 2021",                               1,
  "2021_gore_2v",     "2021-06", "GORE 2021 2V",                   "Figura",     "2021_06_GobernadoresRegionales_2V_Datos_Eleccion.xlsx", NA,                                           "^Votación$",        "Gobernadores Regionales (GORE) 2021",                               2,
  "2021_core",        "2021-11", "CORE 2021",                      "Lista",      "2021_11_ConsejerosRegionales_DatosEleccion.xlsx",       "2021_11_ConsejerosRegionales_Votacion.txt",  NA,                  "Consejeros Regionales (CORE) 2021",                                 1,
  "2021_diputados",   "2021-11", "Diputados 2021",                 "Lista",      "2021_11_Diputados_Datos_Eleccion.xlsx",                 "2021_11_Diputados_Votacion.txt",             NA,                  "Diputados/as 2021",                                                 1,
  "2021_senadores",   "2021-11", "Senadores 2021",                 "Lista",      "2021_11_Senadores_Datos_Eleccion.xlsx",                 "2021_11_Senadores_Votacion.txt",             NA,                  "Senadores/as 2021",                                                 1,
  "2021_pres_1v",     "2021-11", "Presidencial 2021 1V",           "Figura",     "2021_11_Presidencial_1V_Datos_Eleccion.xlsx",           NA,                                           "^Votación en Chile", "Presidente 2021",                                                   1,
  "2021_pres_2v",     "2021-12", "Presidencial 2021 2V",           "Figura",     "2021_12_Presidencial_2V_Datos_Eleccion.xlsx",           NA,                                           "^Votación en Chile", "Presidente 2021",                                                   2,
  "2022_pleb_salida", "2022-09", "Plebiscito 2022 (salida)",       "Binaria",    "2022_PlebiscitoConstitucional_DatosPlebiscito.xlsx",    NA,                                           "^Votación en Chile", "Apruebo-Rechazo (Salida) 2022",                                     1,
  "2023_consejeros",  "2023-05", "Consejeros Constitucionales 2023","Lista",     "2023_05_CCG.xlsx",                                      "2023_05_CCG_Votacion.txt",                   NA,                  "Consejeros Constitucionales 2023",                                  1,
  "2023_pleb_salida", "2023-12", "Plebiscito 2023 (salida)",       "Binaria",    "2023_PlebiscitoConstitucional_DatosPlebiscito.xlsx",    NA,                                           "^Votación en Chile", "A favor-En contra (Salida 2) 2023",                                 1,
  "2024_alcalde",     "2024-10", "Alcalde 2024",                   "Figura",     "2024_10_Alcaldes_Datos_Eleccion.xlsx",                  NA,                                           "^Votación$",        "Alcalde 2024",                                                      1,
  "2024_concejales",  "2024-10", "Concejales 2024",                "Lista",      "2024_10_Concejales_DatosEleccion.xlsx",                 "2024_10_Concejales_DatosEleccion.txt",       NA,                  "Concejales 2024",                                                   1,
  "2024_core",        "2024-10", "CORE 2024",                      "Lista",      "2024_10_ConsejerosRegionales_DatosEleccion.xlsx",       "2024_10_ConsejerosRegionales_Votacion.txt",  NA,                  "Consejeros Regionales (CORE) 2024",                                 1,
  "2024_gore_1v",     "2024-10", "GORE 2024 1V",                   "Figura",     "2024_10_GobernadoresRegionales_Datos_Eleccion.xlsx",    NA,                                           "^Votación$",        "Gobernadores Regionales (GORE) 2024",                               1,
  "2024_gore_2v",     "2024-11", "GORE 2024 2V",                   "Figura",     "2024_11_GobernadoresRegionales_2V_Datos_Eleccion.xlsx", NA,                                           "^Votación$",        "Gobernadores Regionales (GORE) 2024",                               2,
  "2025_diputados",   "2025-11", "Diputados 2025",                 "Lista",      "2025_11_Diputados_Datos_Eleccion_v2.xlsx",              "2025_11_Diputados_Votacion_v2.txt",          NA,                  "Diputados/as 2025",                                                 1,
  "2025_pres_1v",     "2025-11", "Presidencial 2025 1V",           "Figura",     "2025_11_Presidencial_1V_Datos_Eleccion_v2.xlsx",        NA,                                           "^Votación en Chile", "Presidente 2025",                                                   1,
  "2025_pres_2v",     "2025-12", "Presidencial 2025 2V",           "Figura",     "2025_12_Presidencial_2V_Datos_Eleccion_v2.xlsx",        NA,                                           "^Votación en Chile", "Presidente 2025",                                                   2
)

# Ubica cada archivo dentro de data/raw (en cualquier subcarpeta).
archivos_raw <- list.files(DIR_RAW, recursive = TRUE, full.names = TRUE)
ubicar <- function(nombre) {
  if (is.na(nombre)) return(NA_character_)
  hit <- archivos_raw[basename(archivos_raw) == nombre]
  if (length(hit) == 0) return(NA_character_)
  hit[1]
}
catalogo <- catalogo |>
  mutate(ruta_xlsx = map_chr(xlsx, ubicar),
         ruta_txt  = map_chr(txt,  ubicar))

faltantes <- catalogo |>
  filter(is.na(ruta_xlsx) | (!is.na(txt) & is.na(ruta_txt)))
if (nrow(faltantes) > 0) {
  print(select(faltantes, id, xlsx, txt))
  stop("Faltan archivos del SERVEL en data/raw (ver lista arriba).")
}


# 2. Funciones de lectura -----------------------------------------------------

# Homologa los encabezados del SERVEL, que cambian de un archivo a otro
# ("Nro.Región" / "Nro Región", "Circunscripción electoral" / variantes con
# errores de tipeo, "Cargo" / "Electo" / "Nominado" / "Resultado"...).
normalizar_nombres <- function(x) {
  x <- stringi::stri_trans_general(x, "Latin-ASCII") |>
    tolower() |>
    str_replace_all("[^a-z0-9]+", "_") |>
    str_replace_all("^_|_$", "")
  case_when(
    x %in% c("nro", "nro_region")             ~ "nro_region",
    str_detect(x, "senatorial")               ~ "circ_senatorial",
    str_detect(x, "provincial")               ~ "circ_provincial",
    str_detect(x, "^circ")                    ~ "circ_electoral",
    str_detect(x, "^nro_?voto")               ~ "nro_voto",
    str_detect(x, "^opcion")                  ~ "opcion",
    x %in% c("cargo", "carrgo", "electo", "nominado", "resultado", "seleccion") ~ "marca",
    x %in% c("votacion", "votantes")          ~ "votantes",
    TRUE                                      ~ x
  )
}

# Texto comparable: mayúsculas, sin tildes, espacios simples.
clave <- function(x) {
  stringi::stri_trans_general(x, "Latin-ASCII") |> toupper() |> str_squish()
}

col_o_na <- function(df, nombre) {
  if (nombre %in% names(df)) df[[nombre]] else rep(NA_character_, nrow(df))
}

# Lee una hoja de un .xlsx del SERVEL: busca la fila de encabezado (la que
# contiene "Comuna"), descarta columnas vacías y filtra La Reina.
leer_hoja_servel <- function(ruta, patron_hoja, comuna = COMUNA) {
  hojas <- excel_sheets(ruta)
  hoja  <- hojas[str_detect(hojas, patron_hoja) & !str_detect(hojas, "(?i)extranjero")][1]
  if (is.na(hoja)) stop("No se encontró la hoja '", patron_hoja, "' en ", basename(ruta))
  df <- suppressMessages(
    read_excel(ruta, sheet = hoja, col_names = FALSE, col_types = "text",
               .name_repair = "minimal")
  )
  fila_hdr <- which(apply(df, 1, function(r) any(str_trim(r) == "Comuna", na.rm = TRUE)))[1]
  if (is.na(fila_hdr)) stop("Sin encabezado 'Comuna' en ", basename(ruta), " / ", hoja)
  hdr <- as.character(unlist(df[fila_hdr, ]))
  df  <- df[-(1:fila_hdr), !is.na(hdr) & str_trim(hdr) != "", drop = FALSE]
  names(df) <- normalizar_nombres(hdr[!is.na(hdr) & str_trim(hdr) != ""])
  df |> filter(str_squish(comuna) == !!comuna)
}

# Lee un .txt del SERVEL por bloques, conservando solo las filas de La Reina
# (los archivos pesan hasta 750 MB; así no se cargan completos en memoria).
leer_txt_servel <- function(ruta, comuna = COMUNA) {
  primera <- readLines(ruta, n = 1, encoding = "UTF-8", warn = FALSE)
  delim   <- if (str_count(primera, fixed("|")) >= str_count(primera, fixed(";"))) "|" else ";"
  comilla <- if (str_detect(primera, fixed("\""))) "\"" else ""
  cb <- DataFrameCallback$new(function(x, pos) {
    x[str_squish(str_remove_all(x[[5]], "\"")) == comuna, , drop = FALSE]
  })
  df <- read_delim_chunked(
    ruta, callback = cb, chunk_size = 250000, delim = delim, quote = comilla,
    col_types = cols(.default = col_character()), na = character(),
    locale = locale(encoding = "UTF-8"), trim_ws = TRUE, progress = FALSE
  )
  names(df) <- normalizar_nombres(names(df))
  df
}

# Clasifica cada fila como voto válido, blanco o nulo.
clasificar_voto <- function(candidato) {
  case_when(
    # Fila sin nombre: nulos y blancos sin desglose (p. ej. Plebiscito 2020
    # tipo de órgano, que el SERVEL publica en una sola fila).
    is.na(candidato) | candidato == ""                      ~ "nulo_blanco",
    str_detect(clave(candidato), "^VOTOS? (EN )?BLANCOS?$") ~ "blanco",
    str_detect(clave(candidato), "^VOTOS? NULOS?$")         ~ "nulo",
    TRUE                                                    ~ "valido")
}

# Lleva cualquier tabla de votos del SERVEL a un formato común.
estandarizar_votos <- function(df, id, nivel = c("mesa", "comuna")) {
  nivel <- match.arg(nivel)
  nombre <- if ("opcion" %in% names(df)) {
    col_o_na(df, "opcion")
  } else {
    str_c(coalesce(col_o_na(df, "nombres"), ""), " ",
          coalesce(col_o_na(df, "primer_apellido"), ""), " ",
          coalesce(col_o_na(df, "segundo_apellido"), ""))
  }
  out <- tibble(
    id        = id,
    local     = if (nivel == "mesa") str_squish(str_remove_all(col_o_na(df, "local"), "\"")) else NA_character_,
    mesa      = if (nivel == "mesa") str_squish(str_remove_all(col_o_na(df, "mesa"),  "\"")) else NA_character_,
    lista     = col_o_na(df, "lista"),
    pacto     = col_o_na(df, "pacto"),
    subpacto  = col_o_na(df, "subpacto"),
    partido   = col_o_na(df, "partido"),
    candidato = str_squish(str_remove_all(nombre, "\"")),
    votos     = as.integer(round(as.numeric(col_o_na(df, "votos")))),
    marca     = col_o_na(df, "marca")
  ) |>
    mutate(across(c(lista, pacto, subpacto, partido, marca),
                  ~ na_if(na_if(str_squish(str_remove_all(.x, "\"")), ""), "---")),
           tipo_voto = clasificar_voto(candidato))
  if (nivel == "comuna") out <- select(out, -local, -mesa)
  out
}

estandarizar_participacion <- function(df, id) {
  tibble(
    id        = id,
    local     = str_squish(col_o_na(df, "local")),
    mesa      = str_squish(str_remove_all(col_o_na(df, "mesa"), "\"")),
    inscritos = as.integer(round(as.numeric(col_o_na(df, "inscritos")))),
    votantes  = as.integer(round(as.numeric(
                  coalesce(col_o_na(df, "votantes"), col_o_na(df, "votos")))))
  )
}


# 3. Importación (con caché) --------------------------------------------------

importar_papeleta <- function(fila) {
  cache <- file.path(DIR_CACHE, paste0(fila$id, ".rds"))
  if (file.exists(cache) && !FORZAR_LECTURA) return(readRDS(cache))
  message("Leyendo ", fila$id, " ...")
  t0 <- Sys.time()

  comuna <- leer_hoja_servel(fila$ruta_xlsx, "(?i)^Votaci[oó]n (por comuna|comunal)") |>
    estandarizar_votos(fila$id, "comuna")

  mesa_raw <- if (!is.na(fila$ruta_txt)) {
    leer_txt_servel(fila$ruta_txt)
  } else {
    leer_hoja_servel(fila$ruta_xlsx, fila$hoja_mesa)
  }
  mesa <- estandarizar_votos(mesa_raw, fila$id, "mesa")

  part <- leer_hoja_servel(fila$ruta_xlsx, "^Participaci") |>
    estandarizar_participacion(fila$id)

  res <- list(mesa = mesa, comuna = comuna, participacion = part)
  saveRDS(res, cache)
  message("   listo en ", round(as.numeric(difftime(Sys.time(), t0, units = "secs"))), " s")
  res
}

datos <- map(split(catalogo, seq_len(nrow(catalogo))), importar_papeleta)
names(datos) <- catalogo$id

votos_mesa    <- map_dfr(datos, "mesa")   |> mutate(tipo_voto = clasificar_voto(candidato))
votos_comuna  <- map_dfr(datos, "comuna") |> mutate(tipo_voto = clasificar_voto(candidato))

# En algunos .txt (p. ej. Concejales 2021) el SERVEL deja vacío el campo de
# votos cuando el candidato obtuvo 0 en la mesa. Se registra y se imputa 0;
# las validaciones V1 y V2 confirman que esos campos corresponden a cero.
votos_vacios <- votos_mesa |> filter(is.na(votos)) |> count(id, name = "celdas_vacias_imputadas_0")
votos_mesa   <- votos_mesa   |> mutate(votos = coalesce(votos, 0L))
votos_comuna <- votos_comuna |> mutate(votos = coalesce(votos, 0L))
participacion <- map_dfr(datos, "participacion")

saveRDS(votos_mesa,    file.path(DIR_PROC, "lr_votos_mesa.rds"))
saveRDS(votos_comuna,  file.path(DIR_PROC, "lr_votos_comuna.rds"))
saveRDS(participacion, file.path(DIR_PROC, "lr_participacion_mesa.rds"))
saveRDS(catalogo,      file.path(DIR_PROC, "catalogo.rds"))


# 4. Validación interna del SERVEL --------------------------------------------
# V1  Suma de mesas por candidato = votación comunal oficial del candidato.
# V2  Por mesa: votos emitidos en la papeleta = votantes en Participación.
# V3  El conjunto de mesas coincide entre votación y participación.

v1 <- votos_mesa |>
  group_by(id, lista = coalesce(lista, ""), k = clave(candidato)) |>
  summarise(votos_mesas = sum(votos, na.rm = TRUE), .groups = "drop") |>
  full_join(
    votos_comuna |>
      group_by(id, lista = coalesce(lista, ""), k = clave(candidato)) |>
      summarise(votos_comuna = sum(votos, na.rm = TRUE), .groups = "drop"),
    by = c("id", "lista", "k")) |>
  group_by(id) |>
  summarise(v1_filas_distintas = sum(coalesce(votos_mesas, -1L) != coalesce(votos_comuna, -1L)),
            v1_max_dif = max(abs(coalesce(votos_mesas, 0L) - coalesce(votos_comuna, 0L))),
            .groups = "drop")

emit_mesa <- votos_mesa |>
  group_by(id, local, mesa) |>
  summarise(emitidos = sum(votos, na.rm = TRUE), .groups = "drop")

v2_v3 <- emit_mesa |>
  full_join(participacion, by = c("id", "local", "mesa")) |>
  group_by(id) |>
  summarise(v2_mesas_dif   = sum(!is.na(emitidos) & !is.na(votantes) & emitidos != votantes),
            v2_max_dif     = max(abs(coalesce(emitidos, 0L) - coalesce(votantes, 0L))),
            v3_solo_votos  = sum(is.na(votantes)),
            v3_solo_partic = sum(is.na(emitidos)),
            .groups = "drop")

validacion_servel <- catalogo |>
  select(id, eleccion) |>
  left_join(v1, by = "id") |>
  left_join(v2_v3, by = "id")


# 5. Resumen por papeleta -----------------------------------------------------
# Totales desde la votación comunal oficial; inscritos desde Participación.

resumen <- votos_comuna |>
  group_by(id) |>
  summarise(validos  = sum(votos[tipo_voto == "valido"]),
            blancos  = sum(votos[tipo_voto == "blanco"]),
            nulos    = sum(votos[tipo_voto == "nulo"]),
            nulos_blancos = sum(votos[tipo_voto != "valido"]),
            emitidos = sum(votos),
            n_opciones = sum(tipo_voto == "valido"),
            .groups = "drop") |>
  left_join(participacion |>
              group_by(id) |>
              summarise(inscritos = sum(inscritos, na.rm = TRUE),
                        votantes_partic = sum(votantes, na.rm = TRUE),
                        n_mesas   = n_distinct(mesa),
                        n_locales = n_distinct(local), .groups = "drop"),
            by = "id") |>
  mutate(participacion_pct = round(100 * emitidos / inscritos, 2),
         nulos_blancos_pct = round(100 * nulos_blancos / emitidos, 2))

resumen <- catalogo |> select(id, fecha, eleccion, familia) |> left_join(resumen, by = "id")


# 6. Contraste con la Base Madre ----------------------------------------------
# Se leen solo las celdas con votos; se ignoran las columnas TOTAL_VALIDOS
# (sus fórmulas tienen errores, ver informe de auditoría) y se recalculan.

ruta_base <- list.files(DIR_BASE, pattern = "\\.xlsx$", full.names = TRUE)[1]
if (is.na(ruta_base)) stop("No se encuentra la Base Madre en data/base_madre.")

leer_bloques_base <- function(hoja) {
  raw <- suppressMessages(read_excel(ruta_base, sheet = hoja, col_names = FALSE,
                                     col_types = "text", .name_repair = "minimal"))
  titulos <- as.character(unlist(raw[1, ]))
  cols    <- as.character(unlist(raw[2, ]))
  datos_b <- raw[-(1:2), ]
  datos_b <- datos_b[!is.na(datos_b[[1]]) & !str_detect(datos_b[[1]], "(?i)^total"), ]
  # Recorre columnas desde la 5ª, asignando título de bloque y segmento (vuelta)
  out <- list(); titulo <- NA; seg <- 1
  for (j in 5:ncol(raw)) {
    if (!is.na(titulos[j])) { titulo <- titulos[j]; seg <- 1 }
    if (is.na(titulo) || is.na(cols[j])) next
    if (cols[j] == "TOTAL_VALIDOS") { seg <- seg + 1; next }
    out[[length(out) + 1]] <- tibble(
      hoja = hoja, base_titulo = titulo, base_seg = seg,
      local = datos_b[[1]], sublocal = datos_b[[2]], mesa = datos_b[[3]],
      columna = cols[j], votos = suppressWarnings(as.numeric(datos_b[[j]])))
  }
  bind_rows(out)
}

# El cruce con el catálogo se hace por título sin tildes ni mayúsculas, para
# no depender de la codificación con que se guardó la planilla.
base_larga <- map_dfr(paste0("RES_", LETTERS[1:9]), leer_bloques_base) |>
  mutate(k_titulo = clave(base_titulo)) |>
  inner_join(catalogo |> transmute(id, k_titulo = clave(base_titulo), base_seg, familia),
             by = c("k_titulo", "base_seg"))

sin_bloque <- setdiff(catalogo$id, unique(base_larga$id))
if (length(sin_bloque) > 0) {
  stop("Papeletas del catálogo sin bloque en la Base Madre: ", paste(sin_bloque, collapse = ", "))
}

# En las hojas por local (RES_D a RES_I) un local sin datos en una elección
# es esperable (no existía ese año). Solo se considera 'celda vacía' la que
# falta en una fila que sí tiene otros datos para esa misma papeleta.
base_larga <- base_larga |>
  group_by(id, local, sublocal, mesa) |>
  mutate(fila_activa = any(!is.na(votos))) |>
  ungroup()

# 6a. Totales por papeleta
base_totales <- base_larga |>
  group_by(id) |>
  summarise(base_validos = sum(votos[columna != "NULOS_BLANCOS"], na.rm = TRUE),
            base_nb      = sum(votos[columna == "NULOS_BLANCOS"], na.rm = TRUE),
            base_celdas_vacias = sum(fila_activa & is.na(votos) & columna != "NULOS_BLANCOS"),
            .groups = "drop") |>
  mutate(base_emitidos = base_validos + base_nb)

verif_papeletas <- resumen |>
  transmute(id, eleccion, emitidos, validos, nb = nulos_blancos) |>
  left_join(base_totales, by = "id") |>
  mutate(dif_emitidos = base_emitidos - emitidos,
         dif_validos  = base_validos  - validos,
         dif_nb       = base_nb       - nb,
         estado = if_else(dif_emitidos == 0 & dif_validos == 0 & dif_nb == 0,
                          "OK", "REVISAR"))

# 6b. Votos por candidato/opción en papeletas de figura y binarias.
# La columna de la Base Madre (p. ej. ORREGO_LARRAIN) se asocia al candidato
# del SERVEL que contiene todos sus términos. Las papeletas de lista se
# contrastan por partido en 02_homologar_bloques.R.
# Equivalencias de rótulo entre la Base Madre y el SERVEL.
alias_base <- c(CONVENCION_CONSTITUYENTE = "CONVENCION_CONSTITUCIONAL")

asociar_columna <- function(columna, candidatos) {
  if (columna %in% names(alias_base)) columna <- alias_base[[columna]]
  exacto <- candidatos[candidatos == str_replace_all(columna, "_", " ")]
  if (length(exacto) == 1) return(exacto)
  terminos <- str_split(columna, "_")[[1]]
  hit <- candidatos[map_lgl(candidatos, ~ all(str_detect(.x, fixed(terminos))))]
  if (length(hit) == 1) hit else NA_character_
}

servel_cand <- votos_comuna |>
  filter(tipo_voto == "valido") |>
  group_by(id, k = clave(candidato)) |>
  summarise(votos_servel = sum(votos), .groups = "drop")

verif_candidatos <- base_larga |>
  filter(familia %in% c("Figura", "Binaria"), columna != "NULOS_BLANCOS") |>
  group_by(id, columna) |>
  summarise(votos_base = sum(votos, na.rm = TRUE),
            celdas_vacias = sum(fila_activa & is.na(votos)), .groups = "drop") |>
  group_by(id) |>
  mutate(k = map_chr(columna, asociar_columna,
                     candidatos = servel_cand$k[servel_cand$id == first(id)])) |>
  ungroup() |>
  left_join(servel_cand, by = c("id", "k")) |>
  mutate(dif = votos_base - votos_servel,
         estado = case_when(is.na(k)        ~ "SIN ASOCIAR",
                            celdas_vacias > 0 ~ "CELDAS VACÍAS",
                            dif == 0         ~ "OK",
                            TRUE             ~ "REVISAR"))

# 6c. Municipales a nivel de mesa: votos emitidos por mesa (alcalde y
# concejales) y votos por candidato a alcalde, mesa a mesa.
municipales <- c("2016_alcalde", "2016_concejales", "2021_alcalde",
                 "2021_concejales", "2024_alcalde", "2024_concejales")

base_mesa <- base_larga |>
  filter(id %in% municipales) |>
  mutate(mesa = str_squish(mesa)) |>
  group_by(id, mesa) |>
  summarise(base_emitidos = sum(votos, na.rm = TRUE), .groups = "drop")

verif_mesas <- emit_mesa |>
  filter(id %in% municipales) |>
  full_join(base_mesa, by = c("id", "mesa")) |>
  mutate(dif = base_emitidos - emitidos,
         estado = case_when(is.na(base_emitidos) ~ "FALTA EN BASE",
                            is.na(emitidos)      ~ "NO EXISTE EN SERVEL",
                            dif == 0             ~ "OK",
                            TRUE                 ~ "REVISAR")) |>
  arrange(id, estado != "OK", mesa)

resumen_mesas <- verif_mesas |> count(id, estado) |>
  pivot_wider(names_from = estado, values_from = n, values_fill = 0)


# 7. Exportación --------------------------------------------------------------
# Formato CSV para Excel en configuración chilena (separador ';', decimal ',').

write_excel_csv2(resumen,           file.path(DIR_TAB, "01_resumen_papeletas.csv"))
write_excel_csv2(validacion_servel, file.path(DIR_TAB, "01_validacion_interna_servel.csv"))
write_excel_csv2(votos_vacios,      file.path(DIR_TAB, "01_votos_vacios_imputados.csv"))
write_excel_csv2(verif_papeletas,   file.path(DIR_TAB, "01_base_madre_vs_servel_papeletas.csv"))
write_excel_csv2(verif_candidatos,  file.path(DIR_TAB, "01_base_madre_vs_servel_candidatos.csv"))
write_excel_csv2(verif_mesas,       file.path(DIR_TAB, "01_base_madre_vs_servel_mesas_municipales.csv"))

cat("\n== Resumen por papeleta ==\n")
print(select(resumen, eleccion, inscritos, emitidos, validos, participacion_pct,
             nulos_blancos_pct, n_mesas, n_locales), n = Inf)
cat("\n== Campos de votos vacíos en el SERVEL (imputados como 0) ==\n")
print(votos_vacios)
cat("\n== Validación interna SERVEL (0 = consistente) ==\n")
print(select(validacion_servel, -id), n = Inf)
cat("\n== Base Madre vs SERVEL: totales por papeleta ==\n")
print(select(verif_papeletas, eleccion, dif_emitidos, dif_validos, dif_nb,
             base_celdas_vacias, estado), n = Inf)
cat("\n== Base Madre vs SERVEL: candidatos (solo diferencias) ==\n")
print(filter(verif_candidatos, estado != "OK"), n = Inf)
cat("\n== Base Madre vs SERVEL: mesas municipales ==\n")
print(resumen_mesas)
