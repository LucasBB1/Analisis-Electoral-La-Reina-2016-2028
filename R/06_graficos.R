# =============================================================================
# Análisis Político-Electoral La Reina
# 06_graficos.R  — gráficos finales para el documento
# -----------------------------------------------------------------------------
# Entradas: data/processed/*.rds (secciones 1-5), output/tablas/04_*.csv
# Salidas:  output/graficos/G*.png (300 dpi)
#
# Paleta por familia política validada para daltonismo (deuteranopía,
# protanopía, tritanopía) en el orden izquierda -> derecha. El amarillo y el
# rosado tienen bajo contraste con el fondo: por eso todos los gráficos usan
# etiquetas directas y no dependen solo del color.
# =============================================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(ggplot2)
})

DIR_PROC <- file.path("data", "processed")
DIR_TAB  <- file.path("output", "tablas")
DIR_GRAF <- file.path("output", "graficos")
dir.create(DIR_GRAF, recursive = TRUE, showWarnings = FALSE)

COL_FAMILIA <- c(Izquierda = "#c0392b", Centroizquierda = "#e87ba4", Centro = "#eda100",
                 Derecha = "#2a78d6", Ultraderecha = "#4e36b0",
                 Populismo = "#8a8984", Independiente = "#b4b2a9")
ORDEN_FAM <- names(COL_FAMILIA)
TINTA <- "#0b0b0b"; TINTA_2 <- "#52514e"; GRILLA <- "#e4e3df"; FONDO <- "#fcfcfb"

coma <- function(x, d = 1) formatC(x, format = "f", digits = d, decimal.mark = ",")
pct  <- function(x, d = 0) paste0(coma(x, d), "%")

tema <- theme_minimal(base_size = 11) +
  theme(plot.background = element_rect(fill = FONDO, colour = NA),
        panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(colour = GRILLA, linewidth = 0.3),
        axis.text = element_text(colour = TINTA_2), axis.title = element_text(colour = TINTA_2),
        plot.title = element_text(colour = TINTA, face = "bold", size = 13),
        plot.subtitle = element_text(colour = TINTA_2, size = 10),
        plot.caption = element_text(colour = TINTA_2, size = 8, hjust = 0),
        plot.title.position = "plot", plot.caption.position = "plot",
        legend.position = "top", legend.justification = "left",
        legend.title = element_blank(), legend.text = element_text(colour = TINTA_2))
FUENTE <- "Fuente: elaboración propia sobre resultados oficiales del SERVEL."

guardar <- function(g, nombre, ancho = 8, alto = 4.8) {
  ggsave(file.path(DIR_GRAF, nombre), g, width = ancho, height = alto, dpi = 300, bg = FONDO)
  message("Guardado: ", nombre)
}


# G1. Evolución por familia: concejales 2016-2024 y diputados 2017-2025 --------

fam <- read_csv2(file.path(DIR_TAB, "02_votos_por_familia.csv"), show_col_types = FALSE) |>
  filter(str_detect(eleccion, "Concejales|Diputados"),
         categoria %in% c("Izquierda", "Centroizquierda", "Centro", "Derecha", "Ultraderecha")) |>
  mutate(tipo = if_else(str_detect(eleccion, "Concejales"), "Concejales", "Diputados"),
         anio = as.integer(str_extract(eleccion, "\\d{4}")),
         categoria = factor(categoria, ORDEN_FAM))

# Evita que las etiquetas finales se superpongan: separa al menos 2,2 pp
separar <- function(y, min_sep = 2.2) {
  o <- order(y); y2 <- y[o]
  for (k in seq_along(y2)[-1]) if (y2[k] - y2[k - 1] < min_sep) y2[k] <- y2[k - 1] + min_sep
  y[o] <- y2; y
}
etq1 <- fam |> group_by(tipo) |> filter(anio == max(anio)) |>
  mutate(y_etq = separar(pct)) |> ungroup()

g1 <- ggplot(fam, aes(factor(anio), pct, colour = categoria, group = categoria)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.6, colour = FONDO, stroke = 0) +
  geom_point(size = 2) +
  geom_text(data = etq1, aes(x = factor(anio), y = y_etq, label = pct(pct)), hjust = -0.3, size = 3, show.legend = FALSE) +
  facet_wrap(~ tipo, scales = "free_x") +
  scale_colour_manual(values = COL_FAMILIA) +
  scale_x_discrete(expand = expansion(add = c(0.3, 0.6))) +
  scale_y_continuous(labels = function(x) pct(x), limits = c(0, NA)) +
  labs(title = "La derecha total crece; se recompone entre derecha convencional y ultraderecha",
       subtitle = "% de votos válidos en La Reina, por familia política",
       x = NULL, y = NULL, caption = paste(FUENTE, "Familias según la homologación del autor; el PDG y los independientes sin partido se excluyen.")) +
  tema
guardar(g1, "G1_evolucion_familias.png", 9, 5)


# G2. Destino en concejales de quienes votaron por Palacios (IE R×C) ----------

ie <- read_csv2(file.path(DIR_TAB, "04_ie_destino_voto_palacios.csv"), show_col_types = FALSE) |>
  mutate(destino = recode(destino, CHV = "Chile Vamos", ULTRA = "Ultraderecha",
                          RESTO = "Otras listas", NB_CON = "Nulo o blanco"),
         destino = factor(destino, c("Chile Vamos", "Ultraderecha", "Otras listas", "Nulo o blanco")),
         anio = factor(anio, c("2024", "2021", "2016")))
col_ie <- c("Chile Vamos" = "#2a78d6", "Ultraderecha" = "#4e36b0",
            "Otras listas" = "#e87ba4", "Nulo o blanco" = "#b4b2a9")

g2 <- ggplot(ie, aes(pct, anio, fill = destino)) +
  geom_col(width = 0.62, colour = FONDO, linewidth = 0.6, position = position_stack(reverse = TRUE)) +
  geom_text(aes(label = if_else(pct >= 5, pct(pct), "")), position = position_stack(vjust = 0.5, reverse = TRUE),
            colour = "white", size = 3.2, fontface = "bold") +
  scale_fill_manual(values = col_ie) +
  scale_x_continuous(labels = function(x) pct(x), expand = c(0, 0)) +
  labs(title = "En 2024, menos de la mitad de los votantes de Palacios marcó Chile Vamos en concejales",
       subtitle = "Votantes de Palacios (alcalde) según su voto en concejales, misma urna · inferencia ecológica R×C, media posterior",
       x = NULL, y = NULL,
       caption = paste0(FUENTE, " Modelo multinomial-Dirichlet (Rosen et al., 2001; eiPack),\n231 a 283 mesas por elección. En 2016 no hubo listas de ultraderecha.")) +
  tema + theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(colour = GRILLA, linewidth = 0.3))
guardar(g2, "G2_destino_voto_palacios.png", 9, 4)


# G3. Efectos marginales de la arquitectura de listas -------------------------

sim <- readRDS(file.path(DIR_PROC, "simulacion_2028.rds"))
etiquetas <- c(
  D1 = "D1 · ChV en dos listas (réplica 2024)", D2 = "D2 · Chile Vamos unido", D3 = "D3 · Derecha total unida",
  U1 = "U1 · Republicanos + PNL juntos", U2 = "U2 · Republicanos y PNL separados",
  I1 = "I1 · Réplica 2024 (FA+PC / PS+PPD+PDC / otros)", I2 = "I2 · Pacto único",
  I3 = "I3 · PS con FA y PC", I4 = "I4 · Socialdemocracia + centro consolidados",
  I5 = "I5 · Dispersión", I6 = "I6 · FA+PC+DC / PS+PPD+PL+PR",
  H0 = "H0 · Todos habilitados en la RM", H1 = "H1 · Sin Evópoli, PL, FRVS",
  H2 = "H2 · Además sin Republicanos, PR, PDC")
dims <- c(D = "Derecha tradicional", U = "Ultraderecha", I = "Izquierda y centroizquierda", H = "Habilitación en la RM")
ref_p <- sim$marginales |> filter(dimension == "D", alternativa == "D1") |> pull(p_derecha_total_mayoria)

mg <- sim$marginales |>
  mutate(etiqueta = etiquetas[alternativa], dim = factor(dims[dimension], dims),
         p = 100 * p_derecha_total_mayoria,
         es_ref = alternativa %in% c("D1", "U2", "I1", "H0")) |>
  arrange(dim, p) |> mutate(etiqueta = factor(etiqueta, unique(etiqueta)))

g3 <- ggplot(mg, aes(p, etiqueta)) +
  geom_vline(xintercept = 100 * ref_p, colour = TINTA_2, linetype = "dashed", linewidth = 0.4) +
  geom_segment(aes(x = 100 * ref_p, xend = p, yend = etiqueta), colour = "#2a78d6", linewidth = 0.8) +
  geom_point(aes(shape = es_ref), size = 2.8, colour = "#2a78d6", fill = FONDO, stroke = 1.2) +
  geom_text(aes(label = pct(p), hjust = if_else(p >= 100 * ref_p, -0.45, 1.45)), size = 2.9, colour = TINTA_2) +
  scale_shape_manual(values = c(`TRUE` = 21, `FALSE` = 16), guide = "none") +
  facet_grid(dim ~ ., scales = "free_y", space = "free_y", switch = "y") +
  scale_x_continuous(labels = function(x) pct(x), limits = c(30, 90)) +
  labs(title = "La coordinación de la izquierda es la variable que más mueve el concejo 2028",
       subtitle = "Probabilidad de mayoría de la derecha total (≥5 de 8 escaños) al cambiar una dimensión desde la referencia (círculo vacío, línea punteada)",
       x = NULL, y = NULL,
       caption = paste(FUENTE, "Simulación Monte Carlo, 5.000 iteraciones por escenario, números aleatorios comunes.")) +
  tema + theme(strip.placement = "outside", strip.text.y.left = element_text(angle = 0, hjust = 1, face = "bold", colour = TINTA),
               panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(colour = GRILLA, linewidth = 0.3))
guardar(g3, "G3_efectos_marginales.png", 10, 6.5)


# G4. Distribución de escaños de la derecha total: réplica 2024 vs pacto único -

dd <- sim$dist_derecha |> filter(I %in% c("I1", "I2")) |>
  mutate(escenario = recode(I, I1 = "Izquierda como en 2024 (I1)", I2 = "Izquierda en pacto único (I2)"),
         bloque = case_when(escanos_derecha_total >= 5 ~ "Mayoría de derecha",
                            escanos_derecha_total == 4 ~ "Empate 4-4",
                            TRUE ~ "Mayoría de centro e izquierda"),
         bloque = factor(bloque, c("Mayoría de centro e izquierda", "Empate 4-4", "Mayoría de derecha")))
g4 <- ggplot(dd, aes(factor(escanos_derecha_total), 100 * p, fill = bloque)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = pct(100 * p)), vjust = -0.4, size = 3, colour = TINTA_2) +
  facet_wrap(~ escenario) +
  scale_fill_manual(values = c("Mayoría de derecha" = "#2a78d6", "Empate 4-4" = "#b4b2a9", "Mayoría de centro e izquierda" = "#c0392b")) +
  scale_y_continuous(labels = function(x) pct(x), expand = expansion(mult = c(0, 0.12))) +
  labs(title = "Escaños de la derecha total en el concejo 2028",
       subtitle = "Distribución simulada · resto de la arquitectura como en 2024, todos los partidos habilitados",
       x = "Escaños de Derecha + Ultraderecha (de 8)", y = NULL, caption = FUENTE) +
  tema
guardar(g4, "G4_distribucion_escanos.png", 9, 4.8)


# G5. Validación hacia atrás 2021 -> 2024 --------------------------------------

bt <- sim$backtest |> mutate(familia = factor(familia, rev(ORDEN_FAM[1:5])))
g5 <- ggplot(bt, aes(y = familia)) +
  geom_segment(aes(x = pct_p10, xend = pct_p90, yend = familia, colour = familia), linewidth = 3, alpha = 0.35) +
  geom_point(aes(x = pct_mediana, colour = familia), size = 2.5) +
  geom_point(aes(x = pct_real_2024), shape = 4, size = 3.5, stroke = 1.3, colour = TINTA) +
  geom_text(aes(x = pct_real_2024, label = paste("real", pct(pct_real_2024, 1))), nudge_y = 0.32, size = 2.9, colour = TINTA) +
  scale_colour_manual(values = COL_FAMILIA, guide = "none") +
  scale_x_continuous(labels = function(x) pct(x)) +
  labs(title = "Validación hacia atrás: el modelo desde 2021 frente al resultado de 2024",
       subtitle = "Banda: intervalo 80% simulado · punto: mediana · ×: resultado real 2024. La ultraderecha queda fuera del rango",
       x = "% de votos válidos en concejales", y = NULL,
       caption = paste(FUENTE, "Variabilidad calibrada solo con 2016→2021 y arquitectura de listas efectiva de 2024.")) +
  tema + theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(colour = GRILLA, linewidth = 0.3))
guardar(g5, "G5_backtest.png", 9, 4.2)


# G6. Nulos y blancos: alcalde vs concejales (efecto de papeleta) -------------

res <- read_csv2(file.path(DIR_TAB, "01_resumen_papeletas.csv"), show_col_types = FALSE) |>
  filter(str_detect(eleccion, "^(Alcalde|Concejales)")) |>
  mutate(papeleta = word(eleccion, 1), anio = str_extract(eleccion, "\\d{4}"))
g6 <- ggplot(res, aes(anio, nulos_blancos_pct, fill = papeleta)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7, colour = FONDO, linewidth = 0.6) +
  geom_text(aes(label = pct(nulos_blancos_pct, 1)), position = position_dodge(width = 0.75), vjust = -0.4, size = 3, colour = TINTA_2) +
  scale_fill_manual(values = c(Alcalde = "#2a78d6", Concejales = "#eda100")) +
  scale_y_continuous(labels = function(x) pct(x), expand = expansion(mult = c(0, 0.12))) +
  labs(title = "Con voto obligatorio, el nulo/blanco se dispara en la papeleta de concejales",
       subtitle = "% de votos nulos y blancos sobre emitidos, La Reina",
       x = NULL, y = NULL, caption = FUENTE) +
  tema
guardar(g6, "G6_nulos_blancos.png", 7, 4.2)


# G7. Gradiente territorial: centro e izquierda en concejales y C. Orrego (2V GORE) ----
# por sector (requiere el script 07).

sec <- read.csv2(file.path(DIR_TAB, "07_territorial_sectores_2024.csv"), encoding = "UTF-8") |>
  mutate(sector = str_replace(sector, " \\(UV.*\\)", ""),
         sector = factor(sector, levels = sector[order(opos)]))
g7d <- sec |>
  select(sector, `Centro e izquierda en concejales 2024` = opos, `Claudio Orrego, 2ª vuelta gobernador 2024` = c_orrego) |>
  pivot_longer(-sector, names_to = "serie", values_to = "v")
g7 <- ggplot(g7d, aes(v, sector)) +
  geom_vline(xintercept = 50, colour = TINTA_2, linewidth = 0.4, linetype = "dashed") +
  geom_line(aes(group = sector), colour = GRILLA, linewidth = 1.2) +
  geom_point(aes(colour = serie), size = 3.2) +
  geom_text(aes(label = pct(v, 1), colour = serie), vjust = -1.1, size = 3, show.legend = FALSE) +
  scale_colour_manual(values = c(`Centro e izquierda en concejales 2024` = "#c0392b",
                                 `Claudio Orrego, 2ª vuelta gobernador 2024` = "#e87ba4")) +
  scale_x_continuous(limits = c(28, 66), labels = function(x) paste0(x, "%")) +
  labs(title = "El voto de centro e izquierda crece hacia el límite con Peñalolén",
       subtitle = "Porcentaje de votos válidos por sector (unidad vecinal del local de votación)",
       x = NULL, y = NULL,
       caption = paste(FUENTE, "Centro e izquierda: izquierda, centroizquierda y centro. Línea punteada: 50%.")) +
  tema + theme(panel.grid.major.y = element_blank(),
               panel.grid.major.x = element_line(colour = GRILLA, linewidth = 0.3))
guardar(g7, "G7_gradiente_territorial.png", ancho = 8, alto = 4.6)
