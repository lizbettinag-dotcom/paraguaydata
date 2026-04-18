# =============================================================================
# Datos PISA 2022 - America Latina y el Caribe
# Fuente: OCDE, PISA 2022 Results (Volume I)
# Publicado: Diciembre 2023
# Paraguay participo por segunda vez en 2022 (primera vez en 2017)
# =============================================================================

pisa_latam_2022 <- data.frame(
  pais = c(
    "Chile", "Uruguay", "Mexico", "Peru",
    "Costa Rica", "Colombia", "Brasil", "Argentina",
    "Panama", "Guatemala", "El Salvador",
    "Republica Dominicana", "Paraguay", "Jamaica"
  ),
  codigo_iso = c(
    "CHL", "URY", "MEX", "PER",
    "CRI", "COL", "BRA", "ARG",
    "PAN", "GTM", "SLV",
    "DOM", "PRY", "JAM"
  ),
  matematica = c(
    412, 409, 395, 391,
    385, 383, 379, 378,
    357, 344, 343,
    339, 338, NA
  ),
  lectura = c(
    448, 444, 415, 408,
    415, 409, 410, 401,
    377, 371, 373,
    351, 388, NA
  ),
  ciencias = c(
    444, 435, 410, 408,
    411, 411, 403, 406,
    388, 373, 373,
    360, 368, NA
  ),
  ranking_matematica = c(
    52, 53, 57, 59,
    63, 64, 65, 66,
    74, 77, 78,
    79, 80, NA
  ),
  primera_participacion = c(
    2000, 2003, 2000, 2001,
    2009, 2006, 2000, 2000,
    2009, 2022, 2022,
    2009, 2017, 2022
  ),
  stringsAsFactors = FALSE
)

# Datos historicos de Paraguay en PISA (2017 y 2022)
pisa_paraguay <- data.frame(
  anio = c(2017, 2022),
  matematica = c(326, 338),
  lectura    = c(370, 388),
  ciencias   = c(345, 368),
  promedio_ocde_matematica = c(490, 472),
  promedio_ocde_lectura    = c(493, 476),
  promedio_ocde_ciencias   = c(489, 485),
  pct_bajo_rendimiento_matematica = c(91, 85),
  pct_bajo_rendimiento_lectura    = c(72, 66),
  pct_bajo_rendimiento_ciencias   = c(NA, 71),
  n_estudiantes = c(6174, 5084),
  n_escuelas    = c(200, 281),
  stringsAsFactors = FALSE
)

# Guardar como datos internos del paquete
usethis::use_data(
  pisa_latam_2022,
  pisa_paraguay,
  overwrite = TRUE
)

message("Datasets PISA guardados correctamente")
