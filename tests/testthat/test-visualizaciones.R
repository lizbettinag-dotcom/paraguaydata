# Tests para funciones de visualizacion
# Verificamos que retornan objetos ggplot2 validos

# Datos simulados
viz_sim <- data.frame(
  P02          = c(10, 12, 15, 17, 30, 45, 60, 11, 14, 16),
  A05          = c(1, 1, 1, 6, 6, 6, 6, 1, 6, 1),
  P06          = c(1, 6, 1, 6, 1, 6, 1, 6, 1, 6),
  AREA         = c(1, 1, 6, 6, 1, 6, 1, 6, 1, 6),
  A14REC       = c(2, 3, 4, 4, 5, 6, 7, 2, 3, 4),
  A17A         = c(6, 9, 12, 12, 15, 17, 17, 6, 9, 12),
  PEAD         = c(1, 1, 2, 1, 1, 2, 1, 3, 1, 1),
  Informalidad = c(1, 2, NA, 1, 1, NA, 2, NA, 1, 2),
  HORAB        = c(40, 25, NA, 35, 20, NA, 45, NA, 28, 50),
  RAMA_PEA     = c(1, 2, NA, 5, 5, NA, 7, NA, 1, 4),
  Factor       = rep(100, 10)
)

serie_sim <- dplyr::bind_rows(
  dplyr::mutate(viz_sim, PERIODO = "2023-T1"),
  dplyr::mutate(viz_sim, PERIODO = "2024-T1")
)

# --- tema_paraguaydata ---

test_that("tema_paraguaydata retorna objeto theme", {
  tema <- tema_paraguaydata()
  expect_s3_class(tema, "theme")
})

# --- grafico_escolarizacion ---

test_that("grafico_escolarizacion retorna objeto ggplot", {
  g <- grafico_escolarizacion(viz_sim)
  expect_s3_class(g, "ggplot")
})

test_that("grafico_escolarizacion por sexo retorna objeto ggplot", {
  g <- grafico_escolarizacion(viz_sim, grupo = "sexo")
  expect_s3_class(g, "ggplot")
})

test_that("grafico_escolarizacion falla con grupo invalido", {
  expect_error(
    grafico_escolarizacion(viz_sim, grupo = "region"),
    "grupo debe ser"
  )
})

# --- grafico_brecha ---

test_that("grafico_brecha urbano_rural retorna objeto ggplot", {
  g <- grafico_brecha(viz_sim)
  expect_s3_class(g, "ggplot")
})

test_that("grafico_brecha genero retorna objeto ggplot", {
  g <- grafico_brecha(viz_sim, tipo = "genero")
  expect_s3_class(g, "ggplot")
})

test_that("grafico_brecha anos_estudio retorna objeto ggplot", {
  g <- grafico_brecha(viz_sim, indicador = "anos_estudio")
  expect_s3_class(g, "ggplot")
})

test_that("grafico_brecha falla con tipo invalido", {
  expect_error(
    grafico_brecha(viz_sim, tipo = "edad"),
    "tipo debe ser"
  )
})

# --- grafico_empleo ---

test_that("grafico_empleo general retorna objeto ggplot", {
  g <- grafico_empleo(viz_sim)
  expect_s3_class(g, "ggplot")
})

test_that("grafico_empleo por sexo retorna objeto ggplot", {
  g <- grafico_empleo(viz_sim, grupo = "sexo")
  expect_s3_class(g, "ggplot")
})

test_that("grafico_empleo por area retorna objeto ggplot", {
  g <- grafico_empleo(viz_sim, grupo = "area")
  expect_s3_class(g, "ggplot")
})

test_that("grafico_empleo falla con grupo invalido", {
  expect_error(
    grafico_empleo(viz_sim, grupo = "region"),
    "grupo debe ser"
  )
})

# --- grafico_tendencia ---

test_that("grafico_tendencia retorna objeto ggplot", {
  g <- grafico_tendencia(serie_sim)
  expect_s3_class(g, "ggplot")
})

test_that("grafico_tendencia por area retorna objeto ggplot", {
  g <- grafico_tendencia(serie_sim, grupo = "area")
  expect_s3_class(g, "ggplot")
})

test_that("grafico_tendencia falla sin columna PERIODO", {
  datos_mal <- serie_sim[, !names(serie_sim) %in% "PERIODO"]
  expect_error(
    grafico_tendencia(datos_mal),
    "columna PERIODO"
  )
})

# --- grafico_rama ---

test_that("grafico_rama retorna objeto ggplot", {
  g <- grafico_rama(viz_sim)
  expect_s3_class(g, "ggplot")
})

test_that("grafico_rama con titulo personalizado retorna ggplot", {
  g <- grafico_rama(viz_sim, titulo = "Mi grafico")
  expect_s3_class(g, "ggplot")
})
