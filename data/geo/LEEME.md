# Datos geográficos y censales (script 09)

| Archivo | Contenido | Fuente |
|---|---|---|
| `censo2024_zonas_lareina.csv` | 40 variables del Censo de Población y Vivienda 2024 para las 28 zonas censales urbanas de La Reina (CUT 13113) | INE, servicio público `Censo2024_v2/FeatureServer/2` (capa zonal) del visualizador del Censo 2024, consultado el 26 de octubre de 2026 |
| `zonas_censo2024_vertices.csv` | Vértices de los polígonos de esas 28 zonas (WGS84, simplificados) | INE, misma capa |
| `comunas_vertices.csv` | Límite de La Reina y de las comunas vecinas (recortadas) | Paquete R `chilemapas` (límites comunales) |
| `locales_coordenadas.csv` | Coordenadas de los 15 locales de votación de 2024 | OpenStreetMap (Nominatim), consultado el 26 de octubre de 2026 |

Notas:

- Las dos secciones del Complejo Educacional La Reina se tratan como un solo local en el mapa.
- El voto por zona es una interpolación por distancia inversa desde los locales: es una estimación, no un dato observado.
