# Tests para funciones de departamentos

# Datos simulados con ESTGEO
dpto_sim <- data.frame(
  ESTGEO       = c(1, 11, 16, 111, 116, 31, 36, 51, 56, 101),
  P02          = c(10, 12, 15, 17, 30, 45, 60, 11, 14, 16),
  A05          = c(1, 1, 1, 6, 6, 6, 6, 1, 6, 1),
  P06          = c(1, 6, 1, 6, 1, 6, 1, 6, 1, 6),
  AREA         = c(1, 1, 6, 6, 1, 6, 1, 6, 1, 6),
  A14REC       = c(2, 3, 4, 4, 5, 6, 7, 2, 3, 4),
  A17A         = c(6, 9, 12, 12, 15, 17, 17, 6, 9, 12),
  PEAD         = c(1, 1, 2, 1, 1, 2, 1, 3, 1, 1),
  Informalidad = c(1, 2, NA, 1, 1, NA, 2, NA, 1, 2),
  HORAB        = c(40, 25, NA, 35, 20, NA, 45, NA, 28, 50),
  Factor       = rep(100, 10)
)

# --- agregar_departamento ---

test_that("agregar_departamento agrega columna departamento", {
  resultado <- agregar_departamento(dpto_sim)

  expect_true("departamento" %in% names(resultado))
  expect_equal(nrow(resultado), nrow(dpto_sim))
})

test_that("agregar_departamento asigna nombres correctos", {
  resultado <- agregar_departamento(dpto_sim)

  expect_true("Asuncion" %in% resultado$departamento)
  expect_true("Concepcion" %in% resultado$departamento)
  expect_true("Central" %in% resultado$departamento)
})

test_that("agregar_departamento falla sin ESTGEO", {
  datos_mal <- dpto_sim[, !names(dpto_sim) %in% "ESTGEO"]
  expect_error(
    agregar_departamento(datos_mal),
    "Variable ESTGEO no encontrada"
  )
})

# --- educacion_por_departamento ---

test_that("educacion_por_departamento retorna data.frame", {
  resultado <- educacion_por_departamento(dpto_sim)

  expect_s3_class(resultado, "data.frame")
  expect_true("departamento" %in% names(resultado))
})

test_that("educacion_por_departamento anos_estudio funciona", {
  resultado <- educacion_por_departamento(dpto_sim,
                                          indicador = "anos_estudio")
  expect_true("anos_promedio_estudio" %in% names(resultado))
  expect_gt(nrow(resultado), 0)
})

test_that("educacion_por_departamento rezago funciona", {
  resultado <- educacion_por_departamento(dpto_sim,
                                          indicador = "rezago")
  expect_true("tasa_rezago" %in% names(resultado))
})

test_that("educacion_por_departamento falla con indicador invalido", {
  expect_error(
    educacion_por_departamento(dpto_sim, indicador = "pobreza"),
    "indicador debe ser"
  )
})

# --- empleo_por_departamento ---

test_that("empleo_por_departamento retorna data.frame", {
  resultado <- empleo_por_departamento(dpto_sim)

  expect_s3_class(resultado, "data.frame")
  expect_true("departamento" %in% names(resultado))
  expect_true("tasa_informalidad" %in% names(resultado))
})

test_that("empleo_por_departamento desempleo funciona", {
  resultado <- empleo_por_departamento(dpto_sim,
                                       indicador = "desempleo")
  expect_true("tasa_desempleo" %in% names(resultado))
})

test_that("empleo_por_departamento falla con indicador invalido", {
  expect_error(
    empleo_por_departamento(dpto_sim, indicador = "pobreza"),
    "indicador debe ser"
  )
})

# --- grafico_departamentos ---

test_that("grafico_departamentos educacion retorna ggplot", {
  g <- grafico_departamentos(dpto_sim)
  expect_s3_class(g, "ggplot")
})

test_that("grafico_departamentos empleo retorna ggplot", {
  g <- grafico_departamentos(dpto_sim,
                             tipo      = "empleo",
                             indicador = "informalidad")
  expect_s3_class(g, "ggplot")
})

test_that("grafico_departamentos falla con tipo invalido", {
  expect_error(
    grafico_departamentos(dpto_sim, tipo = "salud"),
    "tipo debe ser"
  )
})
