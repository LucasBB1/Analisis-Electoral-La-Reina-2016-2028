# 09_contexto_y_mapa.R - Indicadores de contexto (Anexo B) y mapa territorial con el Censo 2024.
# Requiere haber corrido los scripts 01, 02 y 07.
# Entradas adicionales (data/geo/, ver LEEME.md):
#   censo2024_zonas_lareina.csv   atributos del Censo 2024 por zona censal (INE)
#   zonas_censo2024_vertices.csv  poligonos de las 28 zonas censales urbanas (INE, simplificados)
#   comunas_vertices.csv          limites de La Reina y comunas vecinas (paquete chilemapas)
#   locales_coordenadas.csv       coordenadas de los locales de votacion (OpenStreetMap)
# Salidas: output/tablas/09_*.csv y output/graficos/G8_mapa_territorial.png
# Solo usa dplyr, tidyr y ggplot2 (sin paquetes espaciales).

suppressMessages({library(dplyr); library(tidyr); library(ggplot2); library(grid)})
DIR_TAB <- file.path("output", "tablas"); DIR_GRA <- file.path("output", "graficos")
esc <- function(x, f) write.csv2(x, file.path(DIR_TAB, f), row.names = FALSE, fileEncoding = "UTF-8")

v  <- readRDS("data/processed/lr_votos_comuna.rds")
vb <- readRDS("data/processed/lr_votos_comuna_bloques.rds")
rs <- read.csv2(file.path(DIR_TAB, "01_resumen_papeletas.csv"), fileEncoding = "UTF-8-BOM")
val <- function(i) v %>% filter(id == i, tipo_voto == "valido")

# --- B1. Participacion y padron ----------------------------------------------------------
b1 <- rs %>% filter(id %in% c("2016_concejales","2017_pres_1v","2021_concejales","2021_pres_2v",
                               "2022_pleb_salida","2024_concejales","2025_pres_1v")) %>%
  transmute(id, inscritos, votantes = emitidos, participacion_pct)
print(b1); esc(b1, "09_b1_participacion.csv")

# --- B2. Resultados de alcaldia ------------------------------------------------------------
b2 <- v %>% filter(grepl("_alcalde$", id), tipo_voto == "valido") %>% group_by(id) %>%
  mutate(pct = round(100 * votos / sum(votos), 1)) %>% ungroup() %>%
  arrange(id, desc(votos)) %>% select(id, candidato, partido, votos, pct)
print(as.data.frame(b2)); esc(b2, "09_b2_alcaldia.csv")

# --- B3. Contiendas binarias: opcion de centro e izquierda -----------------------------------
bin <- tribble(~id, ~patron,
  "2017_pres_2v", "GUILLIER", "2020_pleb_entrada", "APRUEBO", "2021_pres_2v", "BORIC",
  "2022_pleb_salida", "APRUEBO", "2023_pleb_salida", "EN CONTRA",
  "2024_gore_2v", "CLAUDIO ORREGO", "2025_pres_2v", "JARA")
b3 <- bin %>% rowwise() %>% mutate(pct = { d <- val(id); round(100 * sum(d$votos[grepl(patron, d$candidato)]) / sum(d$votos), 1) }) %>% ungroup()
print(b3); esc(b3, "09_b3_binarias.csv")

# --- B4. Nulo y blanco por papeleta ----------------------------------------------------------
b4 <- rs %>% filter(id %in% c("2016_alcalde","2016_concejales","2017_core","2021_alcalde","2021_concejales",
                               "2021_core","2021_gore_1v","2024_alcalde","2024_concejales","2024_core","2024_gore_1v")) %>%
  select(id, nulos_blancos_pct)
print(b4); esc(b4, "09_b4_nulo_blanco.csv")

# --- B5. Distancia al escano, concejales 2024 ---------------------------------------------------
L <- val("2024_concejales") %>% group_by(lista, pacto) %>% summarise(votos = sum(votos), .groups = "drop")
q <- L %>% crossing(d = 1:8) %>% mutate(c = votos / d) %>% arrange(desc(c))
cifra <- q$c[8]
b5 <- L %>% left_join(q %>% slice(1:8) %>% count(lista, name = "escanos"), by = "lista") %>%
  mutate(escanos = coalesce(escanos, 0L), faltan = floor((escanos + 1) * cifra - votos) + 1) %>% arrange(desc(votos))
cat("Cifra repartidora del octavo escano:", round(cifra), "\n"); print(as.data.frame(b5)); esc(b5, "09_b5_distancia_escano.csv")

# --- B6. Fragmentacion y volatilidad -----------------------------------------------------------
nep <- function(x) 1 / sum((x / sum(x))^2)
conc <- c("2016_concejales","2021_concejales","2024_concejales")
b6a <- lapply(conc, function(i){ d <- val(i)
  data.frame(id = i, listas = n_distinct(d$lista),
    nep_listas = round(nep(tapply(d$votos, d$lista, sum)), 2),
    nep_partidos = round(nep(tapply(d$votos, ifelse(grepl("INDEPEND", d$partido), "IND", d$partido), sum)), 2)) }) %>% bind_rows()
ped <- function(a, b, k){ s <- function(i) vb %>% filter(id == i, tipo_voto == "valido") %>% group_by(g = .data[[k]]) %>%
    summarise(p = sum(votos), .groups = "drop") %>% mutate(p = p / sum(p))
  full_join(s(a), s(b), by = "g") %>% mutate(across(c(p.x, p.y), ~coalesce(.x, 0))) %>% summarise(50 * sum(abs(p.x - p.y))) %>% pull() }
b6b <- data.frame(tramo = c("2016-2021","2021-2024"),
  pedersen_bloques  = round(c(ped(conc[1], conc[2], "bloque"),  ped(conc[2], conc[3], "bloque")), 1),
  pedersen_familias = round(c(ped(conc[1], conc[2], "familia"), ped(conc[2], conc[3], "familia")), 1))
print(b6a); print(b6b); esc(b6a, "09_b6_fragmentacion.csv"); esc(b6b, "09_b6_volatilidad.csv")

# --- B7. Concentracion del voto preferencial 2024 ---------------------------------------------
b7 <- val("2024_concejales") %>% group_by(lista, pacto) %>% arrange(desc(votos), .by_group = TRUE) %>%
  summarise(votos_lista = sum(votos), primero = first(candidato),
            pct_primero = round(100 * first(votos) / sum(votos), 1),
            pct_dos_primeros = round(100 * sum(head(votos, 2)) / sum(votos), 1), .groups = "drop") %>% arrange(desc(votos_lista))
print(as.data.frame(b7)); esc(b7, "09_b7_concentracion.csv")

# =================================================================================================
# Mapa territorial: voto por local x Censo 2024 por zona censal
# =================================================================================================
zv  <- read.csv2("data/geo/zonas_censo2024_vertices.csv", colClasses = c(ID_ZONA = "character"))
zv$lon <- as.numeric(zv$lon); zv$lat <- as.numeric(zv$lat)
cv  <- read.csv2("data/geo/comunas_vertices.csv", colClasses = c(codigo_comuna = "character"), fileEncoding = "UTF-8")
cv$lon <- as.numeric(cv$lon); cv$lat <- as.numeric(cv$lat)
cen <- read.csv2("data/geo/censo2024_zonas_lareina.csv", dec = ".", colClasses = c(ID_ZONA = "character"), fileEncoding = "UTF-8")
loc <- read.csv2("data/geo/locales_coordenadas.csv", dec = ".")

# Indicadores censales por zona
z <- cen %>% mutate(
  adultos = n_edad_18_24 + n_edad_25_44 + n_edad_45_59 + n_edad_60_mas,
  escolaridad = prom_escolaridad18, edad = prom_edad,
  terciaria = 100 * n_cine_terciaria_maestria_doctorado / (n_cine_primaria + n_cine_secundaria + n_cine_terciaria_maestria_doctorado),
  mayores60 = 100 * n_edad_60_mas / n_per, inmigrantes = 100 * n_inmigrantes / n_per,
  pueblos = 100 * n_pueblos_orig / n_per,
  arriendo = 100 * (n_tenencia_arrendada_contrato + n_tenencia_arrendada_sin_contrato) / n_hog,
  hacinamiento = 100 * n_viv_hacinadas / n_vp) %>%
  select(ID_ZONA, COD_ZONA, DISTRITO, poblacion = n_per, escolaridad, edad, terciaria, mayores60, inmigrantes, pueblos, arriendo, hacinamiento)

# Voto por local fisico (L1/L2 y las dos secciones del Complejo Educacional se agregan)
t <- read.csv2(file.path(DIR_TAB, "07_territorial_locales_2024.csv"))
vars <- c("der","ult","izq","ci","cen","huer","nb","pal","c_orrego","jara2v","opos")
t <- t %>% mutate(local = sub(" L[12]$", "", local))
wavg <- function(x, w){ ok <- !is.na(x); if (!any(ok)) NA_real_ else sum(x[ok] * w[ok]) / sum(w[ok]) }
p <- t %>% left_join(loc, by = "local") %>%
  mutate(local = ifelse(grepl("^COMPLEJO EDUCACIONAL", local), "COMPLEJO EDUCACIONAL LA REINA", local)) %>%
  group_by(local) %>% summarise(across(all_of(vars), ~wavg(.x, emit)), lat = mean(lat), lon = mean(lon),
                                emit = sum(emit), mesas = sum(mesas), .groups = "drop") %>%
  mutate(dertot = der + ult)
stopifnot(nrow(p) == 14, !any(is.na(p$lat)))

# Punto en poligono (ray casting) para ubicar cada local en su zona censal
pip <- function(x, y, px, py){ n <- length(px); j <- n; ins <- FALSE
  for (i in seq_len(n)) { if (((py[i] > y) != (py[j] > y)) && (x < (px[j] - px[i]) * (y - py[i]) / (py[j] - py[i]) + px[i])) ins <- !ins; j <- i }
  ins }
zona_de <- function(x, y){ for (id in unique(zv$ID_ZONA)) { r <- zv[zv$ID_ZONA == id, ]; if (pip(x, y, r$lon, r$lat)) return(id) }; NA }
p$ID_ZONA <- mapply(zona_de, p$lon, p$lat)
pz <- p %>% left_join(z, by = "ID_ZONA")

# Correlaciones de Pearson (n = 14 locales)
indic <- c("escolaridad","terciaria","pueblos","edad","inmigrantes","arriendo")
voto  <- c("opos","dertot","ult","nb","huer","pal")
corr <- expand_grid(indicador = indic, voto = voto) %>% rowwise() %>%
  mutate(r = round(cor(pz[[indicador]], pz[[voto]]), 2)) %>% ungroup() %>% pivot_wider(names_from = voto, values_from = r)
print(as.data.frame(corr)); esc(corr, "09_correlaciones_censo.csv")
esc(pz %>% select(local, ID_ZONA, emit, mesas, all_of(vars), dertot, lat, lon, all_of(indic)), "09_locales_zona_censo.csv")

# Interpolacion IDW (potencia 2; distancias en km)
KX <- 93; KY <- 111
idw <- function(gx, gy, var){ ok <- !is.na(p[[var]]); px <- p$lon[ok]; py <- p$lat[ok]; vv <- p[[var]][ok]
  d <- sqrt(outer((gx), px, "-")^2 * KX^2 + outer(gy, py, "-")^2 * KY^2) + 1e-4
  w <- 1 / d^2; as.vector((w %*% vv) / rowSums(w)) }
dmin <- function(gx, gy) apply(sqrt(outer(gx, p$lon, "-")^2 * KX^2 + outer(gy, p$lat, "-")^2 * KY^2), 1, min)

# Voto estimado por zona (promedio de una grilla dentro de cada zona)
zest <- lapply(unique(zv$ID_ZONA), function(id){ r <- zv[zv$ID_ZONA == id, ]
  g <- expand.grid(x = seq(min(r$lon), max(r$lon), length.out = 25), y = seq(min(r$lat), max(r$lat), length.out = 25))
  g <- g[mapply(pip, g$x, g$y, MoreArgs = list(px = r$lon, py = r$lat)), ]
  if (nrow(g) == 0) g <- data.frame(x = mean(r$lon), y = mean(r$lat))
  out <- data.frame(ID_ZONA = id, dist_km = round(mean(dmin(g$x, g$y)), 2))
  for (vr in c(vars, "dertot")) out[[paste0("v_", vr)]] <- round(mean(idw(g$x, g$y, vr)), 1)
  out }) %>% bind_rows() %>% left_join(z, by = "ID_ZONA")
esc(zest, "09_zonas_censo_voto.csv")

# Figura G8: cuatro paneles
bx <- range(zv$lon); by <- range(zv$lat)
grd <- expand.grid(lon = seq(bx[1], bx[2], length.out = 260), lat = seq(by[1], by[2], length.out = 180))
inside <- rep(FALSE, nrow(grd))
for (id in unique(zv$ID_ZONA)) { r <- zv[zv$ID_ZONA == id, ]; s <- !inside
  inside[s] <- mapply(pip, grd$lon[s], grd$lat[s], MoreArgs = list(px = r$lon, py = r$lat)) }
grd <- grd[inside, ]; grd <- grd[dmin(grd$lon, grd$lat) < 1.3, ]
vec <- cv %>% filter(codigo_comuna != "13113"); lr <- cv %>% filter(codigo_comuna == "13113")
etq <- data.frame(n = c("Providencia","\u00d1u\u00f1oa","Pe\u00f1alol\u00e9n","Las Condes","Macul"),
                  lon = c(-70.5805,-70.5835,-70.548,-70.528,-70.5825), lat = c(-33.4315,-33.4515,-33.4705,-33.4265,-33.4685))
base <- function(){ list(
  geom_polygon(data = vec, aes(lon, lat, group = interaction(codigo_comuna, parte)), fill = "#F1EFEA", colour = "#C9C4BA", linewidth = 0.3),
  coord_quickmap(xlim = c(bx[1] - 0.004, bx[2] - 0.012), ylim = c(by[1] - 0.004, by[2] + 0.006), expand = FALSE),
  theme_void(base_size = 9), theme(plot.title = element_text(face = "bold", colour = "#1F2A37", size = 10),
    legend.key.width = unit(0.25, "cm"), legend.key.height = unit(0.9, "cm"), plot.background = element_rect(fill = "white", colour = NA)),
  geom_text(data = etq, aes(lon, lat, label = n), colour = "#6B6459", fontface = "italic", size = 2.6)) }
contorno <- list(geom_path(data = zv, aes(lon, lat, group = ID_ZONA), colour = "white", linewidth = 0.2, alpha = 0.7),
                 geom_path(data = lr, aes(lon, lat, group = parte), colour = "#1F2A37", linewidth = 0.5))
panel_voto <- function(var, titulo, pal, lab){ g <- grd; g$v <- idw(g$lon, g$lat, var)
  ggplot() + base() + geom_raster(data = g, aes(lon, lat, fill = v), alpha = 0.92) +
    scale_fill_distiller(palette = pal, direction = 1, name = lab) + contorno +
    geom_point(data = p, aes(lon, lat, size = emit), shape = 21, fill = "white", colour = "#1F2A37", stroke = 0.4, show.legend = FALSE) +
    scale_size(range = c(1.2, 3.2)) +
    geom_text(data = p, aes(lon, lat, label = round(.data[[var]])), nudge_x = 0.0011, nudge_y = 0.0009, size = 2.3, colour = "#1F2A37") +
    ggtitle(titulo) }
zpol <- zv %>% left_join(z, by = "ID_ZONA")
p4 <- ggplot() + base() + geom_polygon(data = zpol, aes(lon, lat, group = ID_ZONA, fill = escolaridad), colour = "white", linewidth = 0.2) +
  scale_fill_distiller(palette = "Blues", direction = 1, name = "A\u00f1os") +
  geom_path(data = lr, aes(lon, lat, group = parte), colour = "#1F2A37", linewidth = 0.5) +
  geom_point(data = p, aes(lon, lat), shape = 21, fill = "white", colour = "#1F2A37", size = 1.3) +
  ggtitle("Escolaridad promedio, 18+ a\u00f1os (Censo 2024)")
paneles <- list(panel_voto("opos", "Centro e izquierda, concejales 2024", "YlGnBu", "% v\u00e1lidos"),
                panel_voto("ult", "Ultraderecha, concejales 2024", "Purples", "% v\u00e1lidos"),
                panel_voto("nb", "Nulo y blanco, concejales 2024", "Oranges", "% emitidos"), p4)
png(file.path(DIR_GRA, "G8_mapa_territorial.png"), width = 11, height = 8.6, units = "in", res = 220)
grid.newpage(); pushViewport(viewport(layout = grid.layout(3, 2, heights = unit(c(1, 1, 0.08), "null"))))
for (i in 1:4) print(paneles[[i]], vp = viewport(layout.pos.row = (i - 1) %/% 2 + 1, layout.pos.col = (i - 1) %% 2 + 1))
grid.text(paste("Superficies de voto interpoladas (IDW) desde 14 locales de votaci\u00f3n, solo hasta 1,3 km de un local; cifras = % en cada local; tama\u00f1o del punto = votos emitidos.",
                "Zonas censales urbanas del Censo 2024 (INE). Elaboraci\u00f3n propia con datos del SERVEL e INE.", sep = "\n"),
          x = 0.01, just = "left", gp = gpar(fontsize = 7.5, col = "#5B6670"), vp = viewport(layout.pos.row = 3, layout.pos.col = 1:2))
invisible(dev.off())
cat("Listo: tablas 09_* y G8_mapa_territorial.png\n")
