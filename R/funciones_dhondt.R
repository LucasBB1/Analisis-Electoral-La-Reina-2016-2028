# =============================================================================
# Análisis Político-Electoral La Reina
# funciones_dhondt.R  — motor de asignación de concejales
# -----------------------------------------------------------------------------
# Implementa el escrutinio de concejales de la Ley N° 18.695 (Orgánica
# Constitucional de Municipalidades) en tres niveles:
#   1. Entre listas (y candidaturas independientes fuera de pacto), por
#      cifra repartidora (D'Hondt).
#   2. Dentro de cada lista con escaños, entre sus integrantes: subpactos,
#      partidos que no forman subpacto e independientes que no forman parte
#      de un subpacto. En una lista de un solo partido, partido e
#      independientes de la lista forman un único integrante.
#   3. Dentro de cada integrante, resultan electos los candidatos más votados.
# Los empates en el último escaño se resuelven por sorteo en el escrutinio
# oficial; aquí se informan con una advertencia y se resuelven por orden.
#
# Se usa desde 03_dhondt_certificacion.R y desde la simulación (sección 5).
# =============================================================================

# D'Hondt simple: votos por unidad -> escaños por unidad.
dhondt <- function(votos, unidades, escanos) {
  stopifnot(length(votos) == length(unidades), escanos >= 0)
  out <- setNames(integer(length(unidades)), unidades)
  if (escanos == 0 || length(votos) == 0) return(out)
  cocientes <- data.frame(
    unidad   = rep(unidades, each = escanos),
    cociente = as.vector(t(outer(votos, seq_len(escanos), "/")))
  )
  cocientes <- cocientes[order(-cocientes$cociente), ]
  corte <- cocientes$cociente[escanos]
  if (escanos < nrow(cocientes) && isTRUE(all.equal(corte, cocientes$cociente[escanos + 1]))) {
    warning("Empate en el último cociente (", round(corte, 2),
            "): en el escrutinio oficial se resuelve por sorteo.")
  }
  ganadores <- table(factor(cocientes$unidad[seq_len(escanos)], levels = unidades))
  out[] <- as.integer(ganadores)
  out
}

# Define el integrante (nivel 2) de cada candidato dentro de su lista.
#   cand: data frame con columnas lista, subpacto, partido, candidato, votos
definir_integrantes <- function(cand) {
  es_indep <- function(p) grepl("^INDEPENDIENTE", toupper(p))
  cand |>
    dplyr::mutate(
      unidad_lista = dplyr::if_else(
        # Independientes fuera de pacto: cada candidatura es su propia "lista"
        grepl("^ZZ", lista) | toupper(dplyr::coalesce(pacto, "")) == "INDEPENDIENTES",
        paste0(lista, "::", candidato), lista),
      integrante_base = dplyr::case_when(
        !is.na(subpacto)   ~ paste0("SUBPACTO::", subpacto),
        !es_indep(partido) ~ paste0("PARTIDO::", partido),
        TRUE               ~ NA_character_)) |>
    dplyr::group_by(unidad_lista) |>
    dplyr::mutate(
      n_integrantes = dplyr::n_distinct(integrante_base, na.rm = TRUE),
      integrante = dplyr::case_when(
        !is.na(integrante_base) ~ integrante_base,
        # Lista de un solo partido (o sin partidos): no se fracciona
        n_integrantes <= 1      ~ dplyr::coalesce(dplyr::first(na.omit(integrante_base)),
                                                  paste0("LISTA::", unidad_lista)),
        # Pacto de varios integrantes: el independiente sin subpacto cuenta solo
        TRUE                    ~ paste0("INDEPENDIENTE::", candidato))) |>
    dplyr::ungroup() |>
    dplyr::select(-integrante_base, -n_integrantes)
}

# Escrutinio completo de un concejo.
#   cand: candidatos con lista, pacto, subpacto, partido, candidato, votos
#   escanos: número de concejales de la comuna
# Devuelve 'cand' con columnas escanos_lista, escanos_integrante y electo.
asignar_concejo <- function(cand, escanos) {
  cand <- definir_integrantes(cand)

  # Nivel 1: entre listas
  listas <- cand |>
    dplyr::group_by(unidad_lista) |>
    dplyr::summarise(votos = sum(votos), .groups = "drop")
  listas$escanos_lista <- dhondt(listas$votos, listas$unidad_lista, escanos)

  # Nivel 2: entre integrantes de cada lista
  integ <- cand |>
    dplyr::group_by(unidad_lista, integrante) |>
    dplyr::summarise(votos = sum(votos), .groups = "drop") |>
    dplyr::left_join(dplyr::select(listas, unidad_lista, escanos_lista), by = "unidad_lista") |>
    dplyr::group_by(unidad_lista) |>
    dplyr::mutate(escanos_integrante = dhondt(votos, integrante, dplyr::first(escanos_lista))) |>
    dplyr::ungroup()

  # Nivel 3: candidatos más votados dentro de cada integrante
  cand |>
    dplyr::left_join(dplyr::select(integ, unidad_lista, integrante, escanos_lista, escanos_integrante),
                     by = c("unidad_lista", "integrante")) |>
    dplyr::group_by(unidad_lista, integrante) |>
    dplyr::mutate(orden = rank(-votos, ties.method = "first"),
                  electo = orden <= escanos_integrante) |>
    dplyr::ungroup() |>
    dplyr::select(-orden)
}
