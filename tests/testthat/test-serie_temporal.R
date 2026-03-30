# Tests para funciones de serie temporal
# Usamos datos simulados para no depender de conexion a internet

# Datos simulados para dos periodos
ephc_sim_t1 <- data.frame(
  P02    = c(10, 12, 15, 17, 30, 45),
  A05    = c(1, 1, 1, 6, 6, 6),
  P06    = c(1, 6, 1, 6, 1, 6),
  AREA   = c(1, 1, 6, 6, 1, 6),
  A14REC = c(2, 3, 4, 4, 5, 6),
  A17A   = c(6, 9, 12, 12, 15, 17),
  Factor = rep(100, 6),
  PERIODO = "2023-T1"
)

ephc_sim_t2 <- data.frame(
  P02    = c(10, 12, 15, 17, 30, 45),
  A05    = c(1, 1, 6, 6, 6, 6),
  P06    = c(1, 6, 1, 6, 1, 6),
  AREA   = c(1, 1, 6, 6, 1, 6),
  A14REC = c(2, 3, 4, 4, 5, 6),
  A17A   = c(6, 9, 12, 12, 15, 17),
  Factor = rep(100, 6),
  PERIODO = "2024-T1"
)

serie_sim <- dplyr::bind_rows(ephc_sim_t1, ephc_sim_t2)

# --- ine_serie ---

test_that("ine_serie falla con anio fuera de rango", {
  expect_error(
    ine_serie(anio_inicio = 2000, anio_fin = 2024),
    "Los anios deben estar entre 2017 y 2025"
  )
})

test_that("ine_serie falla con anio_inicio mayor a anio_fin", {
  expect_error(
    ine_serie(anio_inicio = 2024, anio_fin = 2022),
    "anio_inicio debe ser menor o igual a anio_fin"
  )
})

test_that("ine_serie falla con trimestre invalido", {
  expect_error(
    ine_serie(anio_inicio = 2023, anio_fin = 2024, trimestres = 5),
    "Los trimestres deben ser 1, 2, 3 o 4"
  )
})

# --- tendencia_escolarizacion ---

test_that("tendencia_escolarizacion retorna 2 periodos", {
  resultado <- tendencia_escolarizacion(serie_sim)

  expect_s3_class(resultado, "data.frame")
  expect_equal(nrow(resultado), 2)
  expect_true(all(c("PERIODO", "valor") %in% names(resultado)))
})

test_that("tendencia_escolarizacion por area retorna 4 filas", {
  resultado <- tendencia_escolarizacion(serie_sim, grupo = "area")

  expect_equal(nrow(resultado), 4)
  expect_true("grupo" %in% names(resultado))
})

test_that("tendencia_escolarizacion por sexo retorna 4 filas", {
  resultado <- tendencia_escolarizacion(serie_sim, grupo = "sexo")

  expect_equal(nrow(resultado), 4)
  expect_true(all(c("Hombre", "Mujer") %in% resultado$grupo))
})

test_that("tendencia_escolarizacion anos_estudio retorna 2 periodos", {
  resultado <- tendencia_escolarizacion(serie_sim,
                                        indicador = "anos_estudio")
  expect_equal(nrow(resultado), 2)
  expect_gt(resultado$valor[1], 0)
})

test_that("tendencia_escolarizacion rezago retorna valores entre 0 y 100", {
  resultado <- tendencia_escolarizacion(serie_sim,
                                        indicador = "rezago")
  expect_gte(min(resultado$valor), 0)
  expect_lte(max(resultado$valor), 100)
})

test_that("tendencia_escolarizacion falla sin columna PERIODO", {
  datos_mal <- serie_sim[, !names(serie_sim) %in% "PERIODO"]
  expect_error(
    tendencia_escolarizacion(datos_mal),
    "columna PERIODO"
  )
})

test_that("tendencia_escolarizacion falla con indicador invalido", {
  expect_error(
    tendencia_escolarizacion(serie_sim, indicador = "pobreza"),
    "indicador debe ser"
  )
})
