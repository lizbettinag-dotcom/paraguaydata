# Tests para funciones de resumen departamental

resumen_sim <- data.frame(
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
  ANIO         = rep(2024, 10),
  Factor       = rep(100, 10)
)

# --- resumen_departamental ---

test_that("resumen_departamental retorna data.frame", {
  resultado <- resumen_departamental(resumen_sim)
  expect_s3_class(resultado, "data.frame")
})

test_that("resumen_departamental retorna columnas correctas", {
  resultado <- resumen_departamental(resumen_sim)
  expect_true(all(c("departamento",
                    "tasa_escolarizacion",
                    "anos_promedio_estudio",
                    "tasa_rezago",
                    "tasa_desempleo",
                    "tasa_informalidad",
                    "tasa_subempleo") %in% names(resultado)))
})

test_that("resumen_departamental falla sin ESTGEO", {
  datos_mal <- resumen_sim[, !names(resumen_sim) %in% "ESTGEO"]
  expect_error(
    resumen_departamental(datos_mal),
    "Variable ESTGEO no encontrada"
  )
})

test_that("resumen_departamental tiene una fila por departamento", {
  resultado <- resumen_departamental(resumen_sim)
  expect_equal(nrow(resultado),
               length(unique(resultado$departamento)))
})

# --- grafico_resumen_departamental ---

test_that("grafico_resumen_departamental retorna ggplot desde EPHC", {
  g <- grafico_resumen_departamental(resumen_sim)
  expect_s3_class(g, "ggplot")
})

test_that("grafico_resumen_departamental retorna ggplot desde resumen", {
  resumen <- resumen_departamental(resumen_sim)
  g <- grafico_resumen_departamental(resumen)
  expect_s3_class(g, "ggplot")
})

test_that("grafico_resumen_departamental con indicadores custom", {
  g <- grafico_resumen_departamental(
    resumen_sim,
    indicadores = c("tasa_escolarizacion", "tasa_informalidad")
  )
  expect_s3_class(g, "ggplot")
})

test_that("grafico_resumen_departamental falla sin indicadores validos", {
  resumen <- resumen_departamental(resumen_sim)
  expect_error(
    grafico_resumen_departamental(resumen,
                                  indicadores = "pobreza"),
    "Ningun indicador valido"
  )
})
