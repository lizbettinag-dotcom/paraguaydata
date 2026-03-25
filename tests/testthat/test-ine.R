# Tests para funciones de acceso a datos INE

test_that("ine_catalogo retorna un data.frame con columnas correctas", {
  skip_on_cran()
  skip_if_offline()

  resultado <- ine_catalogo()

  expect_s3_class(resultado, "data.frame")
  expect_true(all(c("url", "formato", "dataset") %in% names(resultado)))
  expect_gt(nrow(resultado), 100)
})

test_that("ine_geo departamentos retorna 18 filas", {
  skip_on_cran()
  skip_if_offline()

  resultado <- ine_geo("departamentos")

  expect_s3_class(resultado, "data.frame")
  expect_equal(nrow(resultado), 18)
  expect_true(all(c("codigo_dpto", "descripcion_dpto") %in% names(resultado)))
})

test_that("ine_geo distritos retorna columnas correctas", {
  skip_on_cran()
  skip_if_offline()

  resultado <- ine_geo("distritos")

  expect_s3_class(resultado, "data.frame")
  expect_gt(nrow(resultado), 200)
})

test_that("ine_geo barrios retorna columnas correctas", {
  skip_on_cran()
  skip_if_offline()

  resultado <- suppressWarnings(ine_geo("barrios"))

  expect_s3_class(resultado, "data.frame")
  expect_gt(nrow(resultado), 1000)
})

test_that("ine_geo falla con nivel invalido", {
  expect_error(
    ine_geo("paises"),
    "El nivel debe ser uno de"
  )
})

test_that("ine_ephc retorna data.frame con dimensiones correctas", {
  skip_on_cran()
  skip_if_offline()

  resultado <- suppressWarnings(ine_ephc(anio = 2024, trimestre = 1))

  expect_s3_class(resultado, "data.frame")
  expect_gt(nrow(resultado), 1000)
  expect_gt(ncol(resultado), 50)
  expect_true("Factor" %in% names(resultado))
  expect_true("AREA" %in% names(resultado))
  expect_true("P06" %in% names(resultado))
})

test_that("ine_ephc falla con anio invalido", {
  expect_error(
    ine_ephc(anio = 2000, trimestre = 1),
    "El anio debe estar entre 2017 y 2025"
  )
})

test_that("ine_ephc falla con trimestre invalido", {
  expect_error(
    ine_ephc(anio = 2024, trimestre = 5),
    "El trimestre debe ser 1, 2, 3 o 4"
  )
})

test_that("ine_ipm retorna data.frame con columnas clave", {
  skip_on_cran()
  skip_if_offline()

  resultado <- ine_ipm(anio = 2024)

  expect_s3_class(resultado, "data.frame")
  expect_gt(nrow(resultado), 1000)
  expect_true("pobrezai" %in% names(resultado))
  expect_true("hhid" %in% names(resultado))
})

test_that("ine_ipm falla con anio invalido", {
  expect_error(
    ine_ipm(anio = 2010),
    "El anio debe estar entre 2016 y 2024"
  )
})
