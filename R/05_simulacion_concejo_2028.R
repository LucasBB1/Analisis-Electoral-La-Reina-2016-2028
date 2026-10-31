# =============================================================================
# Análisis Político-Electoral La Reina
# 05_simulacion_concejo_2028.R
# -----------------------------------------------------------------------------
# Propósito
#   Simulación Monte Carlo de la composición del Concejo Municipal 2028 (8
#   escaños) bajo escenarios explícitos de arquitectura de listas y de
#   habilitación de partidos en la Región Metropolitana.
#
# Principios
#   - Cada parámetro tiene su origen declarado: dato (SERVEL, secciones 1-4),
#     calibración con la serie histórica, o supuesto del autor (con análisis
#     de sensibilidad).
#   - La arquitectura de listas NO se promedia con probabilidades inventadas:
#     se reporta el resultado de cada escenario por separado.
#   - Se usan los mismos números aleatorios en todos los escenarios (common
#     random numbers), de modo que las diferencias entre escenarios reflejan
#     solo el cambio de arquitectura.
#
# Entradas  data/processed/lr_votos_comuna_bloques.rds, data/homologacion/bloques.csv
# Salidas   output/tablas/05_*.csv
# =============================================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(purrr)
})

DIR_PROC <- file.path("data", "processed")
DIR_TAB  <- file.path("output", "tablas")
cb       <- readRDS(file.path(DIR_PROC, "lr_votos_comuna_bloques.rds"))
bloques  <- read_csv(file.path("data", "homologacion", "bloques.csv"), show_col_types = FALSE)

SEMILLA <- 2028
N_ITER  <- 5000      # iteraciones por escenario
ESCANOS <- 8


# 1. Parámetros ---------------------------------------------------------------
# Origen: [D] dato SERVEL · [C] calibrado con la serie · [S] supuesto declarado

PAR <- list(
  # [S] Pérdida de votos de Chile Vamos en concejales por la ausencia de
  # Palacios: fracción lambda ~ U(min, max) del voto UDI+RN+Evópoli 2024.
  # La sección 4 mide voto cruzado alcalde-concejal, no el arrastre del
  # alcalde sobre su lista, que no es identificable con datos agregados;
  # por eso es un supuesto con barrido de sensibilidad (sección 7).
  palacios_lambda = c(0, 0.20),
  # [S] Destino de ese voto: ultraderecha / otras listas / nulo-blanco
  palacios_destino = c(ultra = 0.30, otros = 0.20, nb = 0.50),
  # [S] Destino del voto "huérfano" (partidos disueltos y remanente de
  # partidos excluidos): misma familia / otras listas / nulo-blanco
  huerfano_destino = c(familia = 0.45, otros = 0.15, nb = 0.40),
  dirichlet_conc = 20,          # [S] concentración de las Dirichlet (dispersión)
  # [S] Fracción del voto de un partido excluido en la RM que conserva cuando
  # sus candidatos compiten como independientes en una lista aliada
  retencion_excluido = c(0.60, 0.90),
  # [C] Factor de conversión diputados -> concejales para un partido sin
  # concejales previos: rango observado del Partido Republicano en La Reina
  # (0,35 en 2021 y 1,0 en 2024). Se aplica al PNL sobre Diputados 2025.
  pnl_factor = c(0.35, 1.00),
  # [S] Clima nacional: desplazamiento multiplicativo del voto de los partidos
  # del oficialismo (Republicanos, UDI, RN, Evópoli). 0 = sin efecto. Se
  # explora en la sensibilidad (sección 7).
  clima_oficialismo = 0
)

# Partidos disueltos sin sucesor habilitado en la RM: su voto 2024 es huérfano
DISUELTOS <- c("AH", "PH", "PAVP", "PSC")   # PSC -> PCC no constituido en la RM


# 2. Datos base ---------------------------------------------------------------

cuotas <- function(i) {
  cb |> filter(id == i, tipo_voto == "valido") |>
    mutate(codigo = recode(codigo, CS = "FA", RD = "FA", COMUNES = "FA")) |>
    group_by(codigo) |> summarise(v = sum(votos), .groups = "drop") |>
    mutate(s = v / sum(v)) |> select(codigo, s) |> tibble::deframe()
}
familia_de <- function(cod) {
  f <- bloques$familia[match(cod, bloques$codigo)]
  ifelse(is.na(f), "Independiente", f)
}

base_2024 <- cuotas("2024_concejales")
dip_2025  <- cuotas("2025_diputados")


# 3. Calibración de la variabilidad (serie municipal 2016-2021-2024) -----------
# sigma_fam: desviación (ponderada por tamaño) del cambio logarítmico de cada
#   familia entre elecciones de concejales consecutivas.
# sigma_par: desviación (ponderada) de los cambios de cada partido respecto de
#   su familia.

calibrar <- function(ids) {
  # Cuotas por familia y por partido en cada elección
  fam <- map_dfr(ids, function(i) {
    s <- cuotas(i); tibble(id = i, familia = familia_de(names(s)), s = s)
  }) |> group_by(id, familia) |> summarise(s = sum(s), .groups = "drop") |>
    filter(!familia %in% c("Independiente", "Populismo"))
  cambios_fam <- fam |> arrange(id) |> group_by(familia) |>
    mutate(d = log(s) - log(lag(s)), w = (s + lag(s)) / 2) |>
    filter(!is.na(d)) |> ungroup()
  par <- map_dfr(ids, function(i) {
    s <- cuotas(i); tibble(id = i, codigo = names(s), familia = familia_de(names(s)), s = s)
  }) |> filter(familia != "Independiente") |> arrange(id) |>
    group_by(codigo) |> mutate(d = log(s) - log(lag(s)), w = (s + lag(s)) / 2) |>
    ungroup() |> filter(!is.na(d)) |>
    left_join(select(cambios_fam, id, familia, d_fam = d), by = c("id", "familia")) |>
    mutate(resid = d - d_fam) |> filter(!is.na(resid))
  # Desviación ponderada por el tamaño medio de cada familia/partido entre
  # las dos elecciones: los cambios relativos extremos de partidos muy pequeños
  # (p. ej. PRO 2,9 % -> 0,1 %) no deben dominar la variabilidad de los grandes.
  sd_pond <- function(x, w) sqrt(sum(w * x^2) / sum(w))
  list(sigma_fam = sd_pond(cambios_fam$d, cambios_fam$w),
       sigma_par = sd_pond(par$resid, par$w),
       detalle_fam = cambios_fam, detalle_par = par)
}

calib <- calibrar(c("2016_concejales", "2021_concejales", "2024_concejales"))


# 4. Escenarios ---------------------------------------------------------------

ARQ_D <- list(
  D1 = list(c("UDI", "EVOPOLI"), "RN"),                 # réplica 2024
  D2 = list(c("UDI", "RN", "EVOPOLI")),                 # Chile Vamos unido
  D3 = list(c("UDI", "RN", "EVOPOLI", "PREP")))         # derecha total (+PNL si U1)
ARQ_U <- list(U1 = "junto", U2 = "separado")
ARQ_I <- list(
  I1 = list(c("FA", "PC"), c("PS", "PPD", "PDC"), "PR", c("FRVS", "PL")),   # réplica 2024
  I2 = list(c("FA", "PC", "PS", "PPD", "PL", "PDC", "PR", "FRVS")),         # pacto único
  I3 = list(c("FA", "PC", "PS", "FRVS"), c("PPD", "PL", "PDC", "PR")),      # PS hacia la izquierda
  I4 = list(c("FA", "PC", "FRVS"), c("PS", "PPD", "PL", "PDC", "PR")),      # socialdemocracia + centro
  I5 = list("FA", "PC", "PS", "PPD", "PL", "PDC", "PR", "FRVS"),            # dispersión
  I6 = list(c("FA", "PC", "PDC", "FRVS"), c("PS", "PPD", "PL", "PR")))      # comités del Senado 2026
ARQ_H <- list(
  H0 = character(0),                                    # todos habilitados
  H1 = c("EVOPOLI", "PL", "FRVS"),                      # brechas de afiliación grandes
  H2 = c("EVOPOLI", "PL", "FRVS", "PREP", "PR", "PDC")) # todos los bajo 0,15 %

# Lista anfitriona de un partido excluido (sus candidatos como independientes)
ANFITRION <- list(EVOPOLI = c("UDI", "RN"), PL = c("PPD", "PS", "PDC"),
                  FRVS = c("PS", "PPD", "FA"), PREP = c("PNL", "UDI"),
                  PR = c("PPD", "PS"), PDC = c("PPD", "PS"),
                  PPD = c("PS", "PDC"))

armar_listas <- function(d, u, i) {
  der <- ARQ_D[[d]]
  if (d == "D3" && u == "U1") der[[1]] <- c(der[[1]], "PNL")
  ult <- if (d == "D3") { if (u == "U2") list("PNL") else list() } else
         { if (u == "U1") list(c("PREP", "PNL")) else list("PREP", "PNL") }
  c(der, ult, ARQ_I[[i]])
}

escenarios <- expand_grid(D = names(ARQ_D), U = names(ARQ_U),
                          I = names(ARQ_I), H = names(ARQ_H)) |>
  mutate(escenario = paste(D, U, I, H, sep = "·"))


# 5. Motor de simulación -------------------------------------------------------

rdirichlet <- function(n, alpha) {
  g <- matrix(rgamma(n * length(alpha), shape = alpha), ncol = length(alpha), byrow = TRUE)
  g / rowSums(g)
}

# D'Hondt vectorizado: V (iter x unidades), escanos (escalar o vector por fila)
dhondt_mat <- function(V, escanos) {
  n <- nrow(V); S <- matrix(0L, n, ncol(V))
  esc <- rep_len(escanos, n)
  for (k in seq_len(max(esc))) {
    activo <- esc >= k
    if (!any(activo)) break
    j <- max.col(V / (S + 1), ties.method = "first")
    idx <- cbind(which(activo), j[activo])
    S[idx] <- S[idx] + 1L
  }
  S
}

# Sorteos comunes a todos los escenarios
generar_sorteos <- function(n, partidos, calib, par, base_pnl_dip) {
  set.seed(SEMILLA)
  fams <- unique(familia_de(partidos))
  list(
    z_fam  = matrix(rnorm(n * length(fams)), n, dimnames = list(NULL, fams)),
    z_par  = matrix(rnorm(n * length(partidos)), n, dimnames = list(NULL, partidos)),
    lambda = runif(n, par$palacios_lambda[1], par$palacios_lambda[2]),
    d_pal  = rdirichlet(n, par$palacios_destino * par$dirichlet_conc),
    d_hue  = rdirichlet(n, par$huerfano_destino * par$dirichlet_conc),
    rho    = runif(n, par$retencion_excluido[1], par$retencion_excluido[2]),
    phi    = runif(n, par$pnl_factor[1], par$pnl_factor[2]),
    pnl_dip = base_pnl_dip
  )
}

# Reparte el vector 'monto' (por iteración) entre columnas 'dest' de V en
# proporción a sus votos actuales.
repartir <- function(V, monto, dest) {
  dest <- intersect(dest, colnames(V))
  if (length(dest) == 0) return(V)
  w <- V[, dest, drop = FALSE]; tot <- rowSums(w)
  w <- ifelse(tot > 0, w / tot, 1 / length(dest))
  V[, dest] <- V[, dest] + w * monto
  V
}

simular <- function(base, listas, excluidos, sorteos, calib, par,
                    disueltos = DISUELTOS, palacios = TRUE, pnl = TRUE) {
  n <- nrow(sorteos$z_par)
  partidos <- colnames(sorteos$z_par)
  V <- matrix(0, n, length(partidos), dimnames = list(NULL, partidos))
  b <- base[intersect(names(base), partidos)]
  V[, names(b)] <- rep(b, each = n)
  if (pnl && "PNL" %in% partidos) V[, "PNL"] <- sorteos$pnl_dip * sorteos$phi

  # (a) Variabilidad calibrada: choque por familia + choque propio del partido
  fam <- familia_de(partidos)
  V <- V * exp(sorteos$z_fam[, fam, drop = FALSE] * calib$sigma_fam +
               sorteos$z_par * calib$sigma_par)

  # (b) Clima nacional (sensibilidad)
  ofi <- intersect(c("PREP", "UDI", "RN", "EVOPOLI"), partidos)
  V[, ofi] <- V[, ofi] * exp(par$clima_oficialismo)

  # (c) Ausencia de Palacios
  if (palacios) {
    chv <- intersect(c("UDI", "RN", "EVOPOLI"), partidos)
    perdida <- rowSums(V[, chv, drop = FALSE]) * sorteos$lambda
    V[, chv] <- V[, chv] * (1 - sorteos$lambda)
    V <- repartir(V, perdida * sorteos$d_pal[, 1], c("PREP", "PNL"))
    V <- repartir(V, perdida * sorteos$d_pal[, 2], setdiff(partidos, c(chv, "PREP", "PNL", disueltos)))
  }

  # (d) Partidos excluidos en la RM: compiten como independientes en una
  #     lista aliada y conservan rho de su votación; el resto queda huérfano.
  huerf <- list()
  for (e in intersect(excluidos, partidos)) {
    huerf[[e]] <- V[, e] * (1 - sorteos$rho)
    V[, e] <- V[, e] * sorteos$rho
  }

  # (e) Voto huérfano: partidos disueltos + remanente de excluidos
  for (d in intersect(disueltos, partidos)) { huerf[[d]] <- V[, d]; V[, d] <- 0 }
  vivos <- setdiff(partidos, disueltos)
  for (h in names(huerf)) {
    f <- familia_de(h)
    misma <- setdiff(vivos[familia_de(vivos) == f], h)
    if (length(misma) == 0) misma <- vivos[familia_de(vivos) %in% c("Izquierda", "Centroizquierda")]
    V <- repartir(V, huerf[[h]] * sorteos$d_hue[, 1], misma)
    V <- repartir(V, huerf[[h]] * sorteos$d_hue[, 2], setdiff(vivos, c(misma, h)))
  }

  # (f) Listas: ubica a los excluidos en su lista anfitriona si su lista
  #     original queda sin partidos habilitados.
  lista_de <- setNames(rep(NA_integer_, length(partidos)), partidos)
  for (k in seq_along(listas)) lista_de[intersect(listas[[k]], partidos)] <- k
  for (e in intersect(excluidos, partidos)) {
    k <- lista_de[e]
    companeros <- setdiff(listas[[k]], c(excluidos, disueltos))
    if (length(companeros) == 0) {
      host <- intersect(ANFITRION[[e]], setdiff(partidos[!is.na(lista_de)], c(excluidos, disueltos)))[1]
      lista_de[e] <- lista_de[host]
    }
  }
  en_lista <- names(lista_de)[!is.na(lista_de)]
  L <- sort(unique(lista_de[en_lista]))
  M <- sapply(L, function(k) as.numeric(lista_de[en_lista] == k))
  VL <- V[, en_lista, drop = FALSE] %*% M

  # (g) Nivel 1: escaños por lista; nivel 2: entre partidos de la lista
  SL <- dhondt_mat(VL, ESCANOS)
  SP <- matrix(0L, n, length(en_lista), dimnames = list(NULL, en_lista))
  for (j in seq_along(L)) {
    miembros <- en_lista[M[, j] == 1]
    if (length(miembros) == 1) { SP[, miembros] <- SL[, j]; next }
    SP[, miembros] <- dhondt_mat(V[, miembros, drop = FALSE], SL[, j])
  }
  list(escanos = SP, votos = V / rowSums(V))
}

resumir <- function(SP) {
  fams <- c("Derecha", "Ultraderecha", "Izquierda", "Centroizquierda", "Centro")
  F <- sapply(fams, function(f) {
    cols <- colnames(SP)[familia_de(colnames(SP)) == f]
    if (length(cols) == 0) rep(0, nrow(SP)) else rowSums(SP[, cols, drop = FALSE])
  })
  der <- F[, "Derecha"] + F[, "Ultraderecha"]
  tibble(
    esc_derecha = mean(F[, "Derecha"]), esc_ultraderecha = mean(F[, "Ultraderecha"]),
    esc_izquierda = mean(F[, "Izquierda"]), esc_centroizquierda = mean(F[, "Centroizquierda"]),
    esc_centro = mean(F[, "Centro"]),
    p_derecha_total_mayoria = mean(der >= 5),
    p_empate_4_4 = mean(der == 4),
    p_centro_izquierda_mayoria = mean(der <= 3),
    p_derecha_conv_1a_fuerza = mean(F[, "Derecha"] > pmax(F[, "Ultraderecha"], F[, "Izquierda"] + F[, "Centroizquierda"])),
    der_total_p10 = quantile(der, 0.1), der_total_mediana = median(der), der_total_p90 = quantile(der, 0.9))
}


# 6. Proyección 2028 por escenario --------------------------------------------

partidos_2028 <- c("UDI", "RN", "EVOPOLI", "PREP", "PNL", "FA", "PC", "PS", "PPD",
                   "PL", "PDC", "PR", "FRVS", DISUELTOS)
sorteos <- generar_sorteos(N_ITER, partidos_2028, calib, PAR, dip_2025[["PNL"]])

resultados <- escenarios |>
  mutate(res = pmap(list(D, U, I, H), function(d, u, i, h) {
    r <- simular(base_2024, armar_listas(d, u, i), ARQ_H[[h]], sorteos, calib, PAR)
    resumir(r$escanos)
  })) |>
  unnest(res)

# Efectos marginales: cambio respecto del escenario de referencia moviendo
# una sola dimensión (referencia = réplica de 2024: D1·U2·I1·H0)
REF <- c(D = "D1", U = "U2", I = "I1", H = "H0")
marginales <- map_dfr(names(REF), function(dim) {
  otras <- setdiff(names(REF), dim)
  keep <- Reduce(`&`, lapply(otras, function(o) resultados[[o]] == REF[[o]]))
  resultados[keep, ] |>
    mutate(dimension = dim, alternativa = .data[[dim]])
}) |> select(dimension, alternativa, escenario, starts_with("esc_"), starts_with("p_"), der_total_mediana)


# 7. Sensibilidad (sobre el escenario de referencia) ----------------------------

sensibilidad <- list(
  palacios_0    = modifyList(PAR, list(palacios_lambda = c(0, 0.0001))),
  palacios_alto = modifyList(PAR, list(palacios_lambda = c(0.20, 0.35))),
  huerfano_a_nb = modifyList(PAR, list(huerfano_destino = c(familia = 0.25, otros = 0.10, nb = 0.65))),
  huerfano_a_familia = modifyList(PAR, list(huerfano_destino = c(familia = 0.70, otros = 0.15, nb = 0.15))),
  castigo_oficialismo_10 = modifyList(PAR, list(clima_oficialismo = log(0.90))),
  castigo_oficialismo_20 = modifyList(PAR, list(clima_oficialismo = log(0.80))),
  pnl_bajo  = modifyList(PAR, list(pnl_factor = c(0.20, 0.50))),
  pnl_alto  = modifyList(PAR, list(pnl_factor = c(0.80, 1.20)))
)
calib_mitad <- modifyList(calib, list(sigma_fam = calib$sigma_fam / 2, sigma_par = calib$sigma_par / 2))

sens <- imap_dfr(c(list(base = PAR), sensibilidad), function(p, nombre) {
  s <- generar_sorteos(N_ITER, partidos_2028, calib, p, dip_2025[["PNL"]])
  r <- simular(base_2024, armar_listas("D1", "U2", "I1"), ARQ_H$H0, s, calib, p)
  resumir(r$escanos) |> mutate(supuesto = nombre, .before = 1)
})
s_mitad <- simular(base_2024, armar_listas("D1", "U2", "I1"), ARQ_H$H0, sorteos, calib_mitad, PAR)
sens <- bind_rows(sens, resumir(s_mitad$escanos) |> mutate(supuesto = "variabilidad_mitad", .before = 1))

# 7b. Castigo al oficialismo combinado con la arquitectura de la izquierda ------
# Cruza el clima nacional (0%, 10% y 20% de castigo a PREP, UDI, RN y Evópoli)
# con las seis alternativas de la izquierda, con D1·U2·H0 fijos y los mismos
# sorteos del escenario principal (números aleatorios comunes).
castigo_izq <- expand_grid(castigo = c(0, 0.10, 0.20), I = names(ARQ_I)) |>
  pmap_dfr(function(castigo, I) {
    p <- modifyList(PAR, list(clima_oficialismo = log(1 - castigo)))
    r <- simular(base_2024, armar_listas("D1", "U2", I), ARQ_H$H0, sorteos, calib, p)
    resumir(r$escanos) |> mutate(castigo = castigo, I = I, .before = 1)
  })



# 7c. PPD bajo el umbral (afiliados RM al 30-09-2026: 8.985 < 9.121) ----------
# Sensibilidad fuera de la grilla: H2 más el PPD (H2+PPD), con D1·U2 y las
# alternativas de la izquierda, comparado con H0 y H2.
H_PPD <- c(ARQ_H$H2, "PPD")
ppd_bajo_umbral <- expand_grid(H = c("H0", "H2", "H2+PPD"), I = names(ARQ_I)) |>
  pmap_dfr(function(H, I) {
    excl <- if (H == "H2+PPD") H_PPD else ARQ_H[[H]]
    r <- simular(base_2024, armar_listas("D1", "U2", I), excl, sorteos, calib, PAR)
    resumir(r$escanos) |> mutate(H = H, I = I, .before = 1)
  })

# 8. Validación hacia atrás: 2021 -> 2024 --------------------------------------
# Con la información disponible en 2021 (votación 2021 y variabilidad
# calibrada solo con 2016->2021), y conociendo la arquitectura de listas que
# efectivamente se usó en 2024, ¿cae el resultado real dentro del rango?
# Partidos sin continuidad 2021 -> 2024 (Ciudadanos, PRO, Igualdad,
# independientes) se tratan como huérfanos; los entrantes de 2024 (AH, PAVP,
# PSC, PL) no son anticipables y se omiten: es una limitación conocida.

base_2021 <- cuotas("2021_concejales")
calib_bt  <- calibrar(c("2016_concejales", "2021_concejales"))
partidos_bt <- c("UDI", "RN", "EVOPOLI", "PREP", "FA", "PC", "PS", "PPD", "PDC",
                 "PR", "FRVS", "PH", "CIUDADANOS", "PRO", "PI", "IND_ZZI")
listas_2024 <- list(c("UDI", "EVOPOLI"), "RN", "PREP", c("FA", "PC"), "PH",
                    c("PS", "PPD", "PDC"), "PR", "FRVS")
sorteos_bt <- generar_sorteos(N_ITER, partidos_bt, calib_bt, PAR, 0)
bt <- simular(base_2021, listas_2024, character(0), sorteos_bt, calib_bt, PAR,
              disueltos = c("CIUDADANOS", "PRO", "PI", "IND_ZZI"),
              palacios = FALSE, pnl = FALSE)

real_2024_esc <- c(Derecha = 4, Ultraderecha = 2, Izquierda = 1, Centroizquierda = 1, Centro = 0)
real_2024_pct <- tapply(base_2024, familia_de(names(base_2024)), sum)

fam_bt <- c("Derecha", "Ultraderecha", "Izquierda", "Centroizquierda", "Centro")
backtest <- map_dfr(fam_bt, function(f) {
  cols_s <- colnames(bt$escanos)[familia_de(colnames(bt$escanos)) == f]
  cols_v <- colnames(bt$votos)[familia_de(colnames(bt$votos)) == f]
  esc <- rowSums(bt$escanos[, cols_s, drop = FALSE])
  pct <- 100 * rowSums(bt$votos[, cols_v, drop = FALSE])
  tibble(familia = f,
         pct_real_2024 = round(100 * real_2024_pct[[f]], 1),
         pct_p10 = round(quantile(pct, 0.1), 1), pct_mediana = round(median(pct), 1),
         pct_p90 = round(quantile(pct, 0.9), 1),
         dentro_80 = pct_real_2024 >= pct_p10 & pct_real_2024 <= pct_p90,
         escanos_real_2024 = real_2024_esc[[f]],
         p_escanos_reales = mean(esc == real_2024_esc[[f]]),
         escanos_mediana = median(esc))
})


# 9. Exportación --------------------------------------------------------------

parametros <- tibble(
  parametro = c("sigma_fam (calibrado)", "sigma_par (calibrado)",
                "sigma_fam backtest (2016->2021)", "sigma_par backtest",
                names(PAR)),
  valor = c(round(calib$sigma_fam, 3), round(calib$sigma_par, 3),
            round(calib_bt$sigma_fam, 3), round(calib_bt$sigma_par, 3),
            map_chr(PAR, ~ paste(round(.x, 3), collapse = " – "))))

# Distribución completa de escaños de la derecha total (Derecha + Ultraderecha)
# en el escenario de referencia y en cada alternativa de la izquierda, para
# los gráficos de la sección 6.
dist_derecha <- map_dfr(names(ARQ_I), function(i) {
  r <- simular(base_2024, armar_listas("D1", "U2", i), ARQ_H$H0, sorteos, calib, PAR)
  cols <- colnames(r$escanos)[familia_de(colnames(r$escanos)) %in% c("Derecha", "Ultraderecha")]
  tibble(I = i, escanos_derecha_total = rowSums(r$escanos[, cols, drop = FALSE])) |>
    count(I, escanos_derecha_total) |> mutate(p = n / sum(n))
})
saveRDS(list(dist_derecha = dist_derecha, backtest = backtest, marginales = marginales,
             sensibilidad = sens, castigo_izquierda = castigo_izq,
             parametros = parametros),
        file.path(DIR_PROC, "simulacion_2028.rds"))

write_excel_csv2(parametros,  file.path(DIR_TAB, "05_parametros.csv"))
write_excel_csv2(resultados,  file.path(DIR_TAB, "05_resultados_por_escenario.csv"))
write_excel_csv2(marginales,  file.path(DIR_TAB, "05_efectos_marginales.csv"))
write_excel_csv2(sens,        file.path(DIR_TAB, "05_sensibilidad.csv"))
write_excel_csv2(backtest,    file.path(DIR_TAB, "05_backtest_2021_2024.csv"))
write_excel_csv2(castigo_izq, file.path(DIR_TAB, "05_castigo_por_izquierda.csv"))
write_excel_csv2(ppd_bajo_umbral, file.path(DIR_TAB, "05_ppd_bajo_umbral.csv"))

cat("\n== Parámetros ==\n"); print(parametros, n = Inf)
cat("\n== Backtest 2021 -> 2024 ==\n"); print(backtest, width = Inf)
cat("\n== Efectos marginales (referencia D1·U2·I1·H0) ==\n")
print(marginales |> select(dimension, alternativa, esc_derecha, esc_ultraderecha,
                           esc_izquierda, esc_centroizquierda, esc_centro,
                           p_derecha_total_mayoria, p_empate_4_4, p_centro_izquierda_mayoria),
      n = Inf, width = Inf)
cat("\n== Sensibilidad (escenario de referencia) ==\n")
print(sens |> select(supuesto, esc_derecha, esc_ultraderecha, esc_izquierda,
                     esc_centroizquierda, p_derecha_total_mayoria, p_empate_4_4,
                     p_centro_izquierda_mayoria), n = Inf, width = Inf)

cat("\n== Castigo al oficialismo x arquitectura de la izquierda (D1·U2·H0) ==\n")
print(castigo_izq |> select(castigo, I, p_derecha_total_mayoria, p_empate_4_4, p_centro_izquierda_mayoria), n = Inf)

cat("\n== PPD bajo el umbral (D1·U2) ==\n")
print(ppd_bajo_umbral |> select(H, I, p_derecha_total_mayoria, p_empate_4_4, p_centro_izquierda_mayoria), n = Inf)
