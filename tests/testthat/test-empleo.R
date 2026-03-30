# Tests para funciones de empleo
# Datos simulados compatibles con EPHC

empleo_sim <- data.frame(
  PEAD         = c(1, 1, 2, 1, 1, 2, 1, 3, 1, 1),
  Informalidad = c(1, 2, NA, 1, 1, NA, 2, NA, 1, 2),
  HORAB        = c(40, 25, NA, 35, 20, NA, 45, NA, 28, 50),
  RAMA_PEA     = c(1, 2, NA, 5, 5, NA, 7, NA, 1, 4),
  P06          = c(1, 6, 1, 6, 1, 6, 1, 6, 1, 6),
  AREA         = c(1, 1, 6, 6, 1, 6, 1, 6, 1, 6),
  Factor       = rep(100, 10)
)

# --- tasa_desempleo ---

test_that("tasa_desempleo retorna columnas correctas", {
  resultado <- tasa_desempleo(empleo_sim)

  expect_s3_class(resultado, "data.frame")
  expect_true(all(c("grupo", "n_desocupados", "n_pea",
                    "tasa_desempleo") %in% names(resultado)))
})

test_that("tasa_desempleo retorna valor entre 0 y 100", {
  resultado <- tasa_desempleo(empleo_sim)

  expect_gte(resultado$tasa_desempleo, 0)
  expect_lte(resultado$tasa_desempleo, 100)
})

test_that("tasa_desempleo por sexo retorna 2 filas", {
  resultado <- tasa_desempleo(empleo_sim, grupo = "sexo")

  expect_equal(nrow(resultado), 2)
  expect_true(all(c("Hombre", "Mujer") %in% resultado$grupo))
})

test_that("tasa_desempleo por area retorna 2 filas", {
  resultado <- tasa_desempleo(empleo_sim, grupo = "area")

  expect_equal(nrow(resultado), 2)
  expect_true(all(c("Urbana", "Rural") %in% resultado$grupo))
})

test_that("tasa_desempleo falla sin variable PEAD", {
  datos_mal <- empleo_sim[, !names(empleo_sim) %in% "PEAD"]
  expect_error(
    tasa_desempleo(datos_mal),
    "Variable PEAD no encontrada"
  )
})

test_that("tasa_desempleo falla con grupo invalido", {
  expect_error(
    tasa_desempleo(empleo_sim, grupo = "region"),
    "grupo debe ser"
  )
})

# --- tasa_informalidad ---

test_that("tasa_informalidad retorna columnas correctas", {
  resultado <- tasa_informalidad(empleo_sim)

  expect_s3_class(resultado, "data.frame")
  expect_true(all(c("grupo", "n_informales", "n_ocupados",
                    "tasa_informalidad") %in% names(resultado)))
})

test_that("tasa_informalidad retorna valor entre 0 y 100", {
  resultado <- tasa_informalidad(empleo_sim)

  expect_gte(resultado$tasa_informalidad, 0)
  expect_lte(resultado$tasa_informalidad, 100)
})

test_that("tasa_informalidad por sexo retorna 2 filas", {
  resultado <- tasa_informalidad(empleo_sim, grupo = "sexo")

  expect_equal(nrow(resultado), 2)
})

test_that("tasa_informalidad falla sin variable Informalidad", {
  datos_mal <- empleo_sim[, !names(empleo_sim) %in% "Informalidad"]
  expect_error(
    tasa_informalidad(datos_mal),
    "Variable Informalidad no encontrada"
  )
})

# --- tasa_subempleo ---

test_that("tasa_subempleo retorna columnas correctas", {
  resultado <- tasa_subempleo(empleo_sim)

  expect_s3_class(resultado, "data.frame")
  expect_true(all(c("grupo", "n_subempleados", "n_ocupados",
                    "tasa_subempleo") %in% names(resultado)))
})

test_that("tasa_subempleo retorna valor entre 0 y 100", {
  resultado <- tasa_subempleo(empleo_sim)

  expect_gte(resultado$tasa_subempleo, 0)
  expect_lte(resultado$tasa_subempleo, 100)
})

test_that("tasa_subempleo umbral personalizado funciona", {
  resultado_30 <- tasa_subempleo(empleo_sim, umbral_horas = 30)
  resultado_20 <- tasa_subempleo(empleo_sim, umbral_horas = 20)

  expect_gte(resultado_30$tasa_subempleo, resultado_20$tasa_subempleo)
})

test_that("tasa_subempleo falla sin variables requeridas", {
  datos_mal <- empleo_sim[, !names(empleo_sim) %in% "HORAB"]
  expect_error(
    tasa_subempleo(datos_mal),
    "Faltan variables requeridas"
  )
})

# --- distribucion_rama ---

test_that("distribucion_rama retorna columnas correctas", {
  resultado <- distribucion_rama(empleo_sim)

  expect_s3_class(resultado, "data.frame")
  expect_true(all(c("rama", "descripcion", "n",
                    "porcentaje") %in% names(resultado)))
})

test_that("distribucion_rama porcentajes suman aproximadamente 100", {
  resultado <- distribucion_rama(empleo_sim)

  expect_equal(sum(resultado$porcentaje), 100, tolerance = 0.1)
})

test_that("distribucion_rama falla sin variable RAMA_PEA", {
  datos_mal <- empleo_sim[, !names(empleo_sim) %in% "RAMA_PEA"]
  expect_error(
    distribucion_rama(datos_mal),
    "Variable RAMA_PEA no encontrada"
  )
})
