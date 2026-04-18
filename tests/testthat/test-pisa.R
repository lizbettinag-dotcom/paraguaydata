# Tests para funciones PISA

# --- datasets ---

test_that("pisa_latam_2022 tiene 14 paises", {
  expect_equal(nrow(pisa_latam_2022), 14)
})

test_that("pisa_latam_2022 tiene columnas correctas", {
  expect_true(all(c("pais", "codigo_iso", "matematica",
                    "lectura", "ciencias") %in%
                    names(pisa_latam_2022)))
})

test_that("pisa_paraguay tiene 2 anios", {
  expect_equal(nrow(pisa_paraguay), 2)
  expect_true(all(c(2017, 2022) %in% pisa_paraguay$anio))
})

test_that("pisa_paraguay Paraguay mejoro en matematica", {
  expect_gt(
    pisa_paraguay$matematica[pisa_paraguay$anio == 2022],
    pisa_paraguay$matematica[pisa_paraguay$anio == 2017]
  )
})

# --- pisa_comparar_latam ---

test_that("pisa_comparar_latam retorna data.frame", {
  resultado <- pisa_comparar_latam()
  expect_s3_class(resultado, "data.frame")
})

test_that("pisa_comparar_latam con OCDE tiene 15 filas", {
  resultado <- pisa_comparar_latam(incluir_ocde = TRUE)
  expect_equal(nrow(resultado), 15)
})

test_that("pisa_comparar_latam sin OCDE tiene 14 filas", {
  resultado <- pisa_comparar_latam(incluir_ocde = FALSE)
  expect_equal(nrow(resultado), 14)
})

test_that("pisa_comparar_latam por materia funciona", {
  resultado <- pisa_comparar_latam(materia = "matematica")
  expect_true("puntaje" %in% names(resultado))
  expect_true("brecha_ocde" %in% names(resultado))
})

test_that("pisa_comparar_latam falla con materia invalida", {
  expect_error(
    pisa_comparar_latam(materia = "historia"),
    "materia debe ser"
  )
})

# --- pisa_evolucion_paraguay ---

test_that("pisa_evolucion_paraguay retorna 2 filas", {
  resultado <- pisa_evolucion_paraguay()
  expect_equal(nrow(resultado), 2)
})

test_that("pisa_evolucion_paraguay tiene brechas OCDE", {
  resultado <- pisa_evolucion_paraguay()
  expect_true("brecha_ocde_matematica" %in% names(resultado))
  expect_true(all(resultado$brecha_ocde_matematica < 0))
})

# --- grafico_pisa_latam ---

test_that("grafico_pisa_latam retorna ggplot", {
  g <- grafico_pisa_latam()
  expect_s3_class(g, "ggplot")
})

test_that("grafico_pisa_latam lectura retorna ggplot", {
  g <- grafico_pisa_latam(materia = "lectura")
  expect_s3_class(g, "ggplot")
})

test_that("grafico_pisa_latam ciencias retorna ggplot", {
  g <- grafico_pisa_latam(materia = "ciencias")
  expect_s3_class(g, "ggplot")
})

test_that("grafico_pisa_latam falla con materia invalida", {
  expect_error(
    grafico_pisa_latam(materia = "historia"),
    "materia debe ser"
  )
})
