# Análisis político-electoral de La Reina, 2016–2028

Pipeline reproducible en R para analizar los resultados electorales de la comuna de La Reina (Región Metropolitana, Chile) entre 2016 y 2025 y simular escenarios para la elección municipal de 2028.

Todos los datos provienen de los archivos oficiales del Servicio Electoral de Chile (SERVEL). Las cifras se calculan desde los archivos originales, mesa por mesa, y cada paso deja tablas de validación.

**Autor:** Lucas Bottinelli Baltra · [lucasbb1.github.io](https://lucasbb1.github.io)

**Documento de trabajo:** Bottinelli Baltra, L. (2026). *La Reina hacia 2028: realineamiento, voto cruzado y escenarios municipales* (Documento de trabajo N.° 1). [PDF](https://lucasbb1.github.io/documentos/Bottinelli-Baltra_2026_La-Reina-2028.pdf) · [resumen](https://lucasbb1.github.io/posts/la-reina-2028.html) · [mapa interactivo](https://lucasbb1.github.io/mapas/la-reina-2024.html). Para citar el repositorio, ver `CITATION.cff`.

---

## Qué hace

| Script | Qué hace | Tiempo aprox. |
|---|---|---|
| `01_importar_validar.R` | Importa las 29 papeletas del SERVEL (2016–2025), extrae La Reina por mesa, valida la consistencia interna (mesa = comuna, votos = votantes) y contrasta la Base Maestra con el SERVEL | 10–20 min la primera vez; después usa caché |
| `02_homologar_bloques.R` | Asigna cada candidatura a partido, bloque y familia ideológica con reglas explícitas (`data/homologacion/`) | segundos |
| `03_dhondt_certificacion.R` | Motor D'Hondt de tres niveles (Ley 18.695). Reproduce los 24 concejales electos en 2016, 2021 y 2024 y la trayectoria del concejo 2024 | segundos |
| `04_inferencia_ecologica.R` | Voto cruzado alcalde–concejales: identidades contables, cotas de Duncan y Davis, regresión de Goodman y modelo R×C bayesiano (eiPack) | ~80 min (MCMC); después usa caché |
| `05_simulacion_concejo_2028.R` | Monte Carlo del concejo 2028: 108 escenarios de arquitectura de listas, sensibilidad, cruce castigo × coordinación y validación hacia atrás 2021→2024 | segundos |
| `06_graficos.R` | Gráficos G1–G7 (PNG, 300 dpi), paleta validada para daltonismo | segundos |
| `07_territorial.R` | Indicadores por local de votación y por sector (unidad vecinal) | segundos |
| `08_ie_por_sector.R` | Inferencia ecológica R×C desagregada por sector | ~15 min |
| `09_contexto_y_mapa.R` | Indicadores de contexto (participación, alcaldía, contiendas binarias, nulo y blanco, distancia al escaño, fragmentación, volatilidad, voto preferencial) y mapa del voto cruzado con el Censo 2024 por zona censal | ~1 min |

`funciones_dhondt.R` contiene el motor de asignación de escaños, que usan los scripts 03 y 05.

## Cómo replicarlo

1. **Clonar el repositorio** y abrir `LaReina.Rproj` en RStudio, para que el directorio de trabajo sea la raíz.
2. **Instalar los paquetes** (R ≥ 4.2):
   ```r
   install.packages(c("readxl", "readr", "dplyr", "tidyr", "stringr", "stringi",
                      "purrr", "ggplot2", "eiPack", "coda"))
   ```
3. **Descargar los datos del SERVEL.** Los archivos originales pesan unos 6,5 GB y no están en el repositorio.
   - Se descargan desde el sitio del SERVEL, sección de resultados electorales históricos.
   - Los nombres esperados están en el catálogo del script 01 y en `data/raw/LEEME.md`.
   - Se descomprimen dentro de `data/raw/`; el script los busca por nombre en cualquier subcarpeta.
4. **Correr los scripts en orden** (01 → 09).

**Atajo:** `data/processed/` trae los extractos de La Reina ya procesados (en `.rds` y `.csv`). Con ellos se puede partir directamente desde el script 02 sin descargar nada.

## Resultados principales

- **La derecha total creció en concejales de 44,4% (2016) a 57,4% (2024).** Todo el aumento viene de la ultraderecha, que pasó de 0% a 20,1%. La derecha convencional bajó de 44,4% a 37,3%.
- **El voto de alcalde de 2024 no fue un voto de coalición.** Según el modelo R×C, de los votantes del alcalde en 2024:

  | Voto en concejales | Proporción (IC 90%) |
  |---|---|
  | Chile Vamos | 47,4% [46,6–48,1] |
  | Ultraderecha | 24,6% |
  | Otras listas | 15,2% |
  | Nulo o blanco | 12,7% |

- **Concejo 2028 si las listas se arman como en 2024:** la derecha total obtiene mayoría (5 o más de 8 escaños) en 65% de las simulaciones. La variable que más mueve el resultado es la coordinación de la izquierda y la centroizquierda: con lista única, la probabilidad de mayoría de derecha baja a 41%; con dispersión, sube a 81%.
- **Territorio:** el voto de centro e izquierda crece hacia el límite con Peñalolén (47,8%) y cae en La Reina Alta (32,1%). La ultraderecha obtiene entre 18% y 22% en todos los sectores.
- **Territorio y nivel socioeconómico:** cruzando los 14 locales con su zona censal (Censo 2024), la escolaridad se asocia con fuerza al nulo y blanco (r = −0,86) y débilmente al voto de centro e izquierda (r = −0,35).

Las proyecciones son escenarios con supuestos explícitos, no pronósticos. Cada supuesto está declarado en el script 05 y probado en un análisis de sensibilidad.

## Estructura

```
R/                    scripts del pipeline
data/raw/             archivos del SERVEL (no versionados; ver LEEME.md)
data/base_madre/      Base Maestra (Excel): resultados por local y mesa, con libro de códigos
data/homologacion/    reglas de clasificación editables (bloques, pactos)
data/geo/             Censo 2024 por zona censal, polígonos y coordenadas de locales (ver LEEME.md)
data/processed/       extractos de La Reina y resultados intermedios
output/tablas/        tablas CSV (separador ;, decimal ,)
output/graficos/      gráficos PNG
```

## Base Maestra

`data/base_madre/Base_Maestra_Electoral_LaReina_2016-2025.xlsx` reúne los resultados de las 29 papeletas por local de votación (y por mesa en las municipales), con un libro de códigos (hoja LEEME), la clasificación de partidos y un registro de correcciones. Fue transcrita a mano y validada contra los archivos del SERVEL con el script 01: las 29 papeletas coinciden.

## Decisiones de clasificación

Las reglas de clasificación están en `data/homologacion/` y se pueden cambiar sin tocar el código:

- El Partido de la Gente se clasifica como populismo, no en el eje izquierda–derecha.
- Las candidaturas independientes sin partido van como independientes.
- En la Convención 2021, los candidatos se asignan según la lista a la que pertenecen.

## Referencias metodológicas

- Cox, G. W. (1997). *Making votes count*. Cambridge University Press.
- Duncan, O. D., & Davis, B. (1953). An alternative to ecological correlation. *American Sociological Review, 18*(6), 665–666.
- Goodman, L. A. (1953). Ecological regressions and behavior of individuals. *American Sociological Review, 18*(6), 663–664.
- King, G. (1997). *A solution to the ecological inference problem*. Princeton University Press.
- Lau, O., Moore, R. T., & Kellermann, M. (2007). eiPack: R×C ecological inference and higher-dimension data management. *R News, 7*(2), 43–47.
- Rosen, O., Jiang, W., King, G., & Tanner, M. A. (2001). Bayesian and frequentist inference for ecological inference: The R×C case. *Statistica Neerlandica, 55*(2), 134–156.

## Licencia

- **Código** (`R/`): licencia MIT (ver `LICENSE`).
- **Datos, tablas, gráficos y textos**: CC BY 4.0 (ver `LICENSE-DATOS.md`). Se pueden reutilizar citando la autoría.
