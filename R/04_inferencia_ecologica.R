# =============================================================================
# Análisis Político-Electoral La Reina
# 04_inferencia_ecologica.R
# -----------------------------------------------------------------------------
# Pregunta
#   ¿Cómo votaron en la papeleta de CONCEJALES quienes votaron por Palacios en
#   la papeleta de ALCALDE? (voto cruzado en una misma urna, 2016-2024)
#
#   Esto mide la coincidencia alcalde-lista en cada elección. No mide por sí
#   mismo cuánto de ese voto se transferirá a un sucesor en 2028: ese paso es
#   un supuesto de la proyección y se declara como tal en la sección 5.
#
# Métodos (de menor a mayor exigencia)
#   A. Identidades contables comunales (sin supuestos).
#   B. Cotas deterministas de Duncan y Davis (1953) por mesa, agregadas.
#   C. Regresión de Goodman (1953), interpretada correctamente: su pendiente
#      no es una tasa de transferencia.
#   D. Modelo R×C multinomial-Dirichlet bayesiano (Rosen, Jiang, King y
#      Tanner, 2001; paquete eiPack), que respeta las cotas lógicas.
#
# Unidad: mesa (las dos papeletas se depositan en la misma urna).
# Entradas: data/processed/lr_votos_mesa.rds, lr_votos_mesa_bloques.rds
# Salidas:  output/tablas/04_*.csv, data/processed/ie_draws.rds
#
# Paquetes adicionales: eiPack, coda  ->  install.packages(c("eiPack", "coda"))
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

bases <- map(anios, base_mesa) |> set_names(anios)


# 2. A. Identidades contables comunales ----------------------------------------

contables <- map_dfr(bases, function(d) {
  tibble(
    anio = first(d$anio), mesas = nrow(d),
    palacios = sum(d$PALACIOS),
    palacios_pct_validos = 100 * sum(d$PALACIOS) / sum(d$PALACIOS + d$OTROS),
    chv_votos = sum(d$CHV),
    chv_pct_validos = 100 * sum(d$CHV) / sum(d$CHV + d$ULTRA + d$RESTO),
    ultra_pct_validos = 100 * sum(d$ULTRA) / sum(d$CHV + d$ULTRA + d$RESTO),
    # Máximo lógico de votantes de Palacios que pudieron marcar ChV: todo el
    # voto ChV proviene de votantes de Palacios.
    max_pal_a_chv = 100 * min(1, sum(d$CHV_r) / sum(d$PALACIOS)),
    # Ídem para la derecha total (ChV + ultraderecha)
    max_pal_a_der = 100 * min(1, sum(d$CHV_r + d$ULTRA_r) / sum(d$PALACIOS)))
})


# 3. B. Cotas de Duncan-Davis por mesa (2x2: Palacios / ChV) -------------------
# En cada mesa, la fracción b de votantes de Palacios que marcó ChV cumple
#   max(0, (P + C - N) / P) <= b <= min(1, C / P)
# Se agregan ponderando por los votos de Palacios de cada mesa.

cotas <- map_dfr(bases, function(d) {
  lo <- pmax(0, (d$PALACIOS + d$CHV_r - d$N) / d$PALACIOS)
  hi <- pmin(1, d$CHV_r / d$PALACIOS)
  lo_d <- pmax(0, (d$PALACIOS + d$CHV_r + d$ULTRA_r - d$N) / d$PALACIOS)
  hi_d <- pmin(1, (d$CHV_r + d$ULTRA_r) / d$PALACIOS)
  w <- d$PALACIOS
  tibble(anio = first(d$anio),
         chv_min = 100 * weighted.mean(lo, w),   chv_max = 100 * weighted.mean(hi, w),
         der_min = 100 * weighted.mean(lo_d, w), der_max = 100 * weighted.mean(hi_d, w))
})


# 4. C. Regresión de Goodman ---------------------------------------------------
# y = % ChV sobre votos válidos de concejales; x = % Palacios sobre válidos de
# alcalde; WLS ponderado por válidos de concejales.
# Bajo el supuesto de Goodman (tasas constantes entre mesas):
#   y = b_N + (b_P - b_N) x   =>   intercepto = b_N ; pendiente = b_P - b_N
#   b_P (votantes de Palacios que marcan ChV) = intercepto + pendiente.
# La pendiente sola NO es la tasa de transferencia.

goodman <- map_dfr(bases, function(d) {
  val_con <- d$CHV + d$ULTRA + d$RESTO
  y <- d$CHV / val_con; x <- d$PALACIOS / (d$PALACIOS + d$OTROS)
  m <- lm(y ~ x, weights = val_con)
  co <- coef(m)
  tibble(anio = first(d$anio),
         intercepto_bN = co[[1]], pendiente = co[[2]],
         bP_palacios_a_chv = co[[1]] + co[[2]],
         r2 = summary(m)$r.squared,
         diagnostico = case_when(
           co[[1]] < 0 | co[[1]] + co[[2]] > 1 ~ "Fuera de [0,1]: supuesto de Goodman no se sostiene",
           TRUE ~ "Dentro de [0,1]"))
})


# 5. D. Modelo R×C bayesiano (eiPack) ------------------------------------------

filas <- c("PALACIOS", "OTROS", "NB_ALC")
cols  <- c("CHV_r", "ULTRA_r", "RESTO_r", "NB_CON_r")

# Las categorías sin votos en una elección (p. ej. ultraderecha en 2016) se
# excluyen del modelo de ese año: una columna vacía impide la convergencia.
columnas_activas <- function(d) cols[colSums(as.data.frame(d)[cols]) > 0]

estimar_cadena <- function(d, semilla) {
  set.seed(semilla)
  cols <- columnas_activas(d)
  form <- as.formula(paste0("cbind(", paste(cols, collapse = ", "),
                            ") ~ cbind(", paste(filas, collapse = ", "), ")"))
  tune <- tuneMD(form, data = d, total = "N", ntunes = MCMC$ntunes,
                 totaldraws = MCMC$totaldraws)
  fit <- ei.MD.bayes(form, data = d, total = "N", tune.list = tune,
                     burnin = MCMC$burnin, sample = MCMC$sample,
                     thin = MCMC$thin, ret.beta = "r", verbose = 0)
  # Tasas agregadas de la comuna: promedio de las tasas de cada mesa
  # ponderado por los votos de cada fila (no el parámetro poblacional
  # 'alpha', que es un promedio no ponderado de mesas).
  b <- fit$draws$Beta   # draws x (mesas * R * C)
  nm <- colnames(b)
  agregar_celda <- function(f, cc) {
    idx <- grep(paste0("^beta\\.", f, "\\.", cc, "\\."), nm)
    unidades <- as.integer(sub(".*\\.", "", nm[idx]))
    w <- d[[f]][unidades]
    as.vector(b[, idx, drop = FALSE] %*% w) / sum(w)
  }
  expand_grid(fila = filas, col = cols) |>
    mutate(draw = map2(fila, col, agregar_celda))
}

estimar_rxc <- function(d) {
  anio <- first(d$anio)
  cache <- file.path(DIR_CACHE, paste0("ie_rxc_", anio, ".rds"))
  if (file.exists(cache) && !FORZAR_MCMC) return(readRDS(cache))
  d <- as.data.frame(d)
  cadenas <- lapply(seq_len(MCMC$cadenas), function(k) {
    message("R×C ", anio, ": cadena ", k, " de ", MCMC$cadenas, " ...")
    t0 <- Sys.time()
    out <- estimar_cadena(d, SEMILLA + as.integer(anio) * 10 + k)
    message("   lista en ", round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1), " min")
    out
  })
  # Une las cadenas: una lista de vectores por celda
  draws <- cadenas[[1]] |> select(fila, col)
  draws$cadenas <- pmap(list(seq_len(nrow(draws))), function(i) map(cadenas, ~ .x$draw[[i]]))
  saveRDS(draws, cache)
  draws
}

rxc <- map(bases, estimar_rxc)

resumen_rxc <- imap_dfr(rxc, function(draws, anio) {
  draws |>
    mutate(todo  = map(cadenas, unlist),
           media = map_dbl(todo, mean),
           q05   = map_dbl(todo, quantile, 0.05),  q95  = map_dbl(todo, quantile, 0.95),
           q025  = map_dbl(todo, quantile, 0.025), q975 = map_dbl(todo, quantile, 0.975),
           ess   = map_dbl(cadenas, ~ sum(map_dbl(.x, ~ effectiveSize(as.mcmc(.x))))),
           # Gelman-Rubin (R-hat): < 1,1 indica convergencia aceptable
           rhat  = map_dbl(cadenas, function(ch) {
             if (length(ch) < 2) return(NA_real_)
             gelman.diag(as.mcmc.list(map(ch, as.mcmc)), autoburnin = FALSE)$psrf[1, 1]
           })) |>
    select(-cadenas, -todo) |>
    mutate(anio = anio, col = str_remove(col, "_r$"),
           across(c(media, q05, q95, q025, q975), ~ round(100 * .x, 1)),
           ess = round(ess), rhat = round(rhat, 3))
}) |> relocate(anio)

# Votos de Palacios según destino en concejales (media posterior)
destino_palacios <- resumen_rxc |>
  filter(fila == "PALACIOS") |>
  left_join(transmute(contables, anio, palacios), by = "anio") |>
  mutate(votos_estimados = round(palacios * media / 100)) |>
  select(anio, destino = col, pct = media, ic90_inf = q05, ic90_sup = q95, votos_estimados, ess, rhat)

saveRDS(rxc, file.path(DIR_PROC, "ie_draws.rds"))


# 6. Exportación --------------------------------------------------------------

write_excel_csv2(contables,        file.path(DIR_TAB, "04_ie_identidades_contables.csv"))
write_excel_csv2(cotas,            file.path(DIR_TAB, "04_ie_cotas_duncan_davis.csv"))
write_excel_csv2(goodman,          file.path(DIR_TAB, "04_ie_goodman.csv"))
write_excel_csv2(resumen_rxc,      file.path(DIR_TAB, "04_ie_rxc_matriz.csv"))
write_excel_csv2(destino_palacios, file.path(DIR_TAB, "04_ie_destino_voto_palacios.csv"))

cat("\n== A. Identidades contables ==\n");          print(contables, width = Inf)
cat("\n== B. Cotas Duncan-Davis (% votantes de Palacios) ==\n"); print(cotas)
cat("\n== C. Goodman ==\n");                         print(goodman, width = Inf)
cat("\n== D. R×C: destino en concejales de los votantes de Palacios ==\n")
print(destino_palacios, n = Inf)
