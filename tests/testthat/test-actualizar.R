# Tests para funciones de actualizacion automatica

# --- ine_trimestres_disponibles ---

test_that("ine_trimestres_disponibles retorna data.frame", {
  skip_if_offline()
  resultado <- ine_trimestres_disponibles()
  expect_s3_class(resultado, "data.frame")
})

test_that("ine_trimestres_disponibles retorna columnas correctas", {
  skip_if_offline()
  resultado <- ine_trimestres_disponibles()
  expect_true(all(c("anio", "trimestre", "periodo", "url") %in%
                    names(resultado)))
})

test_that("ine_trimestres_disponibles trimestres entre 1 y 4", {
  skip_if_offline()
  resultado <- ine_trimestres_disponibles()
  expect_true(all(resultado$trimestre %in% 1:4))
})

test_that("ine_trimestres_disponibles anios desde 2017", {
  skip_if_offline()
  resultado <- ine_trimestres_disponibles()
  expect_gte(min(resultado$anio), 2017)
})

test_that("ine_trimestres_disponibles ordenado mas reciente primero", {
  skip_if_offline()
  resultado <- ine_trimestres_disponibles()
  expect_gte(resultado$anio[1], resultado$anio[nrow(resultado)])
})

# --- ine_hay_actualizacion ---

test_that("ine_hay_actualizacion TRUE con datos viejos", {
  skip_if_offline()
  resultado <- ine_hay_actualizacion(
    anio_actual = 2017, trimestre_actual = 1
  )
  expect_true(resultado)
})

test_that("ine_hay_actualizacion FALSE con ultimo trimestre", {
  skip_if_offline()
  disponibles <- ine_trimestres_disponibles()
  ultimo_anio <- disponibles$anio[1]
  ultimo_trim <- disponibles$trimestre[1]
  resultado <- ine_hay_actualizacion(
    anio_actual = ultimo_anio,
    trimestre_actual = ultimo_trim
  )
  expect_false(resultado)
})

# --- ine_anio_completo ---

test_that("ine_anio_completo falla con anio inexistente", {
  skip_if_offline()
  expect_error(
    ine_anio_completo(anio = 2000),
    "No se encontraron datos para el anio"
  )
})
