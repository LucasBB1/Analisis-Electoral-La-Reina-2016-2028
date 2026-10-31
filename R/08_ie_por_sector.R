# =============================================================================
# Análisis Político-Electoral La Reina
# 08_ie_por_sector.R
# -----------------------------------------------------------------------------
# ¿Cómo votaron en concejales los electores de Palacios en CADA SECTOR de la
# comuna (2024)? Usa el mismo modelo R×C del script 04 (mismas cadenas y
# semillas, por lo que el total comunal reproduce el del script 04) y agrega
# las tasas de cada mesa por sector, ponderando por los votos de Palacios.
# Sectores: unidad vecinal del local (ver script 07).
# Salida: output/tablas/08_ie_palacios_por_sector_2024.csv
# Tiempo: ~15 minutos (dos cadenas); el resultado queda en caché.
# =============================================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(purrr)
  library(eiPack)
  library(coda)
})

DIR_PROC <- file.path("data", "processed")
DIR_TAB  <- file.path("output", "tablas")
DIR_CACHE <- file.path(DIR_PROC, "cache")
dir.create(DIR_CACHE, recursive = TRUE, showWarnings = FALSE)
SEMILLA <- 20261023   # semilla fija: resultados reproducibles

# Parámetros del MCMC. Con estas cadenas (300.000 iteraciones de calentamiento
# y 2.000 muestras cada 300) las tasas agregadas son estables. Se corren dos
# cadenas independientes para el diagnóstico de Gelman-Rubin (R-hat). Cada
# cadena tarda ~6-7 minutos por elección; los resultados quedan en caché y
# las corridas siguientes son instantáneas.
MCMC <- list(burnin = 300000, sample = 2000, thin = 300, ntunes = 10,
             totaldraws = 10000, cadenas = 2)
FORZAR_MCMC <- FALSE  # TRUE = volver a estimar aunque exista la caché

votos_mesa <- readRDS(file.path(DIR_PROC, "lr_votos_mesa.rds"))
mesa_b     <- readRDS(file.path(DIR_PROC, "lr_votos_mesa_bloques.rds"))
clave <- function(x) stringi::stri_trans_general(x, "Latin-ASCII") |> toupper() |> str_squish()

anios <- c("2016", "2021", "2024")


# 1. Base por mesa ------------------------------------------------------------
# Filas (alcalde):     PALACIOS, OTROS (resto de candidatos), NB_ALC
# Columnas (concejal): CHV (Derecha convencional), ULTRA (Ultraderecha),
#                      RESTO (todas las demás listas), NB_CON
# Las dos papeletas de una misma mesa difieren en unos pocos votos (votos
# depositados en una sola urna, papeletas extraviadas o anuladas por la mesa).
# Para el modelo R×C los márgenes deben sumar lo mismo; se reescalan las
# columnas al total de la papeleta de alcalde (método del mayor resto).

reescalar <- function(m, total) {
  # m: matriz de conteos (mesas x categorías); total: vector objetivo
  t(vapply(seq_len(nrow(m)), function(i) {
    x <- m[i, ] / sum(m[i, ]) * total[i]
    base <- floor(x); resto <- total[i] - sum(base)
    if (resto > 0) base[order(-(x - base))[seq_len(resto)]] <- base[order(-(x - base))[seq_len(resto)]] + 1
    base
  }, numeric(ncol(m))))
}

base_mesa <- function(anio) {
  alc <- votos_mesa |>
    filter(id == paste0(anio, "_alcalde")) |>
    mutate(cat = case_when(tipo_voto != "valido" ~ "NB_ALC",
                           str_detect(clave(candidato), "PALACIOS") ~ "PALACIOS",
                           TRUE ~ "OTROS")) |>
    group_by(mesa, local, cat) |> summarise(v = sum(votos), .groups = "drop") |>
    pivot_wider(names_from = cat, values_from = v, values_fill = 0)

  con <- mesa_b |>
    filter(id == paste0(anio, "_concejales")) |>
    mutate(cat = case_when(familia == "Nulo/Blanco"  ~ "NB_CON",
                           familia == "Derecha"      ~ "CHV",
                           familia == "Ultraderecha" ~ "ULTRA",
                           TRUE                      ~ "RESTO")) |>
    group_by(mesa, cat) |> summarise(v = sum(votos), .groups = "drop") |>
    pivot_wider(names_from = cat, values_from = v, values_fill = 0)
  for (k in c("CHV", "ULTRA", "RESTO", "NB_CON")) if (!k %in% names(con)) con[[k]] <- 0

  d <- inner_join(alc, con, by = "mesa") |>
    mutate(N = PALACIOS + OTROS + NB_ALC,
           N_con = CHV + ULTRA + RESTO + NB_CON)
  esc <- reescalar(as.matrix(d[, c("CHV", "ULTRA", "RESTO", "NB_CON")]), d$N)
  d[, c("CHV_r", "ULTRA_r", "RESTO_r", "NB_CON_r")] <- esc
  d |> mutate(anio = anio)
}

d <- as.data.frame(base_mesa("2024"))
filas <- c("PALACIOS", "OTROS", "NB_ALC")
cols  <- c("CHV_r", "ULTRA_r", "RESTO_r", "NB_CON_r")

# Sectores con códigos ASCII (evita problemas de codificación entre sistemas
# operativos); las etiquetas con tildes se agregan al final.
sector_de <- c(
  "COLEGIO SAN CONSTANTINO" = "villa_la_reina",
  "COMPLEJO EDUCACIONAL LA REINA SECCION BASICA" = "villa_la_reina",
  "COMPLEJO EDUCACIONAL LA REINA SECCION MEDIA L1" = "villa_la_reina",
  "COMPLEJO EDUCACIONAL LA REINA SECCION MEDIA L2" = "villa_la_reina",
  "COLEGIO SAGRADO CORAZON DE JESUS DE LA REINA" = "villa_la_reina",
  "COLEGIO JOSE ARRIETA" = "borde_sur",
  "COLEGIO CONFEDERACION SUIZA" = "borde_sur",
  "COLEGIO SANTA CATALINA LABOURE" = "borde_sur",
  "ESCUELA PALESTINA" = "borde_sur",
  "COLEGIO DE LA SALLE" = "poniente",
  "COLEGIO TERESIANO ENRIQUE DE OSSO L1" = "poniente",
  "COLEGIO TERESIANO ENRIQUE DE OSSO L2" = "poniente",
  "COLEGIO SANTA MARIA REINA" = "centro",
  "ESCUELA BASICA YANGTSE" = "centro",
  "LICEO EUGENIO MARIA DE HOSTOS" = "centro_norte",
  "COLEGIO ANDREE ENGLISH SCHOOL" = "centro_norte",
  "ESCUELA SAN LEONARDO MURIALDO" = "reina_alta")
ETIQUETA <- c(
  borde_sur      = "Borde sur con Pe\u00f1alol\u00e9n (UV 6, 8 y 10)",
  villa_la_reina = "Villa La Reina (UV 13 y 15)",
  centro         = "Centro (UV 11)",
  poniente       = "Poniente, borde \u00d1u\u00f1oa (UV 4)",
  centro_norte   = "Centro-norte (UV 12)",
  reina_alta     = "Reina Alta (UV 14)",
  comuna         = "Comuna")
d$sector <- unname(sector_de[d$local])
stopifnot(!any(is.na(d$sector)))
codigos <- names(ETIQUETA)[names(ETIQUETA) != "comuna"]
grupos <- c(lapply(setNames(codigos, codigos), function(k) which(d$sector == k)),
            list(comuna = seq_len(nrow(d))))

cadena_sector <- function(semilla) {
  set.seed(semilla)
  form <- as.formula(paste0("cbind(", paste(cols, collapse = ", "),
                            ") ~ cbind(", paste(filas, collapse = ", "), ")"))
  tune <- tuneMD(form, data = d, total = "N", ntunes = MCMC$ntunes,
                 totaldraws = MCMC$totaldraws)
  fit <- ei.MD.bayes(form, data = d, total = "N", tune.list = tune,
                     burnin = MCMC$burnin, sample = MCMC$sample,
                     thin = MCMC$thin, ret.beta = "r", verbose = 0)
  b <- fit$draws$Beta; nm <- colnames(b)
  expand_grid(grupo = names(grupos), col = cols) |>
    mutate(draw = map2(grupo, col, function(g, cc) {
      idx <- grep(paste0("^beta\\.PALACIOS\\.", cc, "\\."), nm)
      u <- as.integer(sub(".*\\.", "", nm[idx]))
      keep <- u %in% grupos[[g]]
      w <- d$PALACIOS[u[keep]]
      as.vector(b[, idx[keep], drop = FALSE] %*% w) / sum(w)
    }))
}

cache <- file.path(DIR_CACHE, "ie_sector_2024_v2.rds")
if (file.exists(cache) && !FORZAR_MCMC) {
  cad <- readRDS(cache)
} else {
  cad <- lapply(seq_len(MCMC$cadenas), function(k) {
    message("Cadena ", k, " de ", MCMC$cadenas, " ...")
    cadena_sector(SEMILLA + 2024L * 10 + k)   # misma semilla que el script 04
  })
  saveRDS(cad, cache)
}

res <- cad[[1]] |> select(grupo, col) |>
  mutate(cadenas = map(seq_len(n()), function(i) map(cad, ~ .x$draw[[i]])),
         todo  = map(cadenas, unlist),
         media = 100 * map_dbl(todo, mean),
         q05   = 100 * map_dbl(todo, quantile, 0.05),
         q95   = 100 * map_dbl(todo, quantile, 0.95),
         rhat  = map_dbl(cadenas, ~ gelman.diag(mcmc.list(lapply(.x, as.mcmc)),
                                                autoburnin = FALSE)$psrf[1, 1])) |>
  mutate(votos_palacios = map_dbl(grupos, ~ sum(d$PALACIOS[.x]))[grupo],
         mesas = lengths(grupos)[grupo]) |>
  mutate(destino = recode(col, CHV_r = "CHV", ULTRA_r = "ULTRA", RESTO_r = "RESTO", NB_CON_r = "NB_CON"),
         votos_estimados = round(media / 100 * votos_palacios)) |>
  mutate(sector = unname(ETIQUETA[grupo])) |>
  select(sector, mesas, votos_palacios, destino, media, q05, q95, votos_estimados, rhat) |>
  mutate(across(c(media, q05, q95), ~ round(.x, 1)), rhat = round(rhat, 3))

write_excel_csv2(res, file.path(DIR_TAB, "08_ie_palacios_por_sector_2024.csv"))
print(res |> select(sector, destino, media, q05, q95, votos_estimados, rhat), n = Inf, width = Inf)
