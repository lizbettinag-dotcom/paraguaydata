# Tests para funciones analiticas
# Usamos datos simulados para no depender de conexion a internet

# Datos simulados compatibles con EPHC
ephc_sim <- data.frame(
  P02    = c(10, 12, 15, 17, 30, 45, 60, 11, 14, 16),
  A05    = c(1, 1, 1, 6, 6, 6, 6, 1, 6, 1),
  P06    = c(1, 6, 1, 6, 1, 6, 1, 6, 1, 6),
  AREA   = c(1, 1, 6, 6, 1, 6, 1, 6, 1, 6),
  A14REC = c(2, 3, 4, 4, 5, 6, 7, 2, 3, 4),
  A17A   = c(6, 9, 12, 12, 15, 17, 17, 6, 9, 12),
  UPM    = c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5),
  NVIVI  = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
  NHOGA  = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
  Factor = rep(100, 10)
)

# Datos simulados compatibles con IPM
ipm_sim <- data.frame(
  hhid     = c("00001_001_1", "00002_001_1", "00003_001_1",
               "00004_001_1", "00005_001_1"),
  pobrezai = c(0, 1, 2, 3, 0)
)

# --- tasa_escolarizacion ---

test_that("tasa_escolarizacion retorna tibble con columnas correctas", {
  resultado <- tasa_escolarizacion(ephc_sim)

  expect_s3_class(resultado, "data.frame")
  expect_true(all(c("grupo", "n_asiste", "n_total",
                    "tasa_escolarizacion") %in% names(resultado)))
})

test_that("tasa_escolarizacion por area retorna 2 filas", {
  resultado <- tasa_escolarizacion(ephc_sim, grupo = "area")

  expect_equal(nrow(resultado), 2)
  expect_true(all(c("Urbana", "Rural") %in% resultado$grupo))
})

test_that("tasa_escolarizacion por sexo retorna 2 filas", {
  resultado <- tasa_escolarizacion(ephc_sim, grupo = "sexo")

  expect_equal(nrow(resultado), 2)
  expect_true(all(c("Hombre", "Mujer") %in% resultado$grupo))
})

test_that("tasa_escolarizacion falla con grupo invalido", {
  expect_error(
    tasa_escolarizacion(ephc_sim, grupo = "region"),
    "grupo debe ser"
  )
})

test_that("tasa_escolarizacion falla sin variable P02", {
  datos_mal <- ephc_sim[, !names(ephc_sim) %in% "P02"]
  expect_error(
    tasa_escolarizacion(datos_mal),
    "Faltan variables requeridas"
  )
})

test_that("tasa_escolarizacion retorna valores entre 0 y 100", {
  resultado <- tasa_escolarizacion(ephc_sim)

  expect_gte(resultado$tasa_escolarizacion, 0)
  expect_lte(resultado$tasa_escolarizacion, 100)
})

# --- anios_promedio_estudio ---

test_that("anios_promedio_estudio retorna tibble con columnas correctas", {
  resultado <- anios_promedio_estudio(ephc_sim, edad_min = 10)

  expect_s3_class(resultado, "data.frame")
  expect_true(all(c("n", "anos_promedio_estudio",
                    "error_estandar") %in% names(resultado)))
})

test_that("anios_promedio_estudio retorna valor positivo", {
  resultado <- anios_promedio_estudio(ephc_sim, edad_min = 10)

  expect_gt(resultado$anos_promedio_estudio, 0)
})

test_that("anios_promedio_estudio falla sin variable A17A", {
  datos_mal <- ephc_sim[, !names(ephc_sim) %in% "A17A"]
  expect_error(
    anios_promedio_estudio(datos_mal),
    "Faltan variables requeridas"
  )
})

# --- rezago_educativo ---

test_that("rezago_educativo retorna tibble con columnas correctas", {
  resultado <- rezago_educativo(ephc_sim)

  expect_s3_class(resultado, "data.frame")
  expect_true(all(c("n_total", "n_rezago",
                    "tasa_rezago") %in% names(resultado)))
})

test_that("rezago_educativo retorna tasa entre 0 y 100", {
  resultado <- rezago_educativo(ephc_sim)

  expect_gte(resultado$tasa_rezago, 0)
  expect_lte(resultado$tasa_rezago, 100)
})

# --- brecha_urbano_rural ---

test_that("brecha_urbano_rural retorna 2 filas con columnas correctas", {
  resultado <- brecha_urbano_rural(ephc_sim)

  expect_equal(nrow(resultado), 2)
  expect_true(all(c("grupo", "valor", "brecha_absoluta",
                    "brecha_relativa") %in% names(resultado)))
})

test_that("brecha_urbano_rural anos_estudio retorna 2 filas", {
  resultado <- brecha_urbano_rural(ephc_sim,
                                   indicador = "anos_estudio")
  expect_equal(nrow(resultado), 2)
})

test_that("brecha_urbano_rural falla con indicador invalido", {
  expect_error(
    brecha_urbano_rural(ephc_sim, indicador = "pobreza"),
    "indicador debe ser"
  )
})

# --- brecha_genero ---

test_that("brecha_genero retorna 2 filas con columnas correctas", {
  resultado <- brecha_genero(ephc_sim)

  expect_equal(nrow(resultado), 2)
  expect_true(all(c("grupo", "valor", "brecha_absoluta",
                    "indice_paridad_genero") %in% names(resultado)))
})

test_that("brecha_genero anos_estudio retorna indice positivo", {
  resultado <- brecha_genero(ephc_sim,
                             indicador = "anos_estudio")
  expect_gt(resultado$indice_paridad_genero[1], 0)
})

test_that("brecha_genero falla sin variable P06", {
  datos_mal <- ephc_sim[, !names(ephc_sim) %in% "P06"]
  expect_error(
    brecha_genero(datos_mal),
    "Variable P06"
  )
})

# --- educacion_pobreza ---

test_that("educacion_pobreza retorna data.frame con columnas correctas", {
  resultado <- educacion_pobreza(ephc_sim, ipm_sim, edad_min = 10)

  expect_s3_class(resultado, "data.frame")
  expect_true("nivel_educativo" %in% names(resultado))
})

test_that("educacion_pobreza falla sin variable pobrezai", {
  ipm_mal <- ipm_sim[, !names(ipm_sim) %in% "pobrezai"]
  expect_error(
    educacion_pobreza(ephc_sim, ipm_mal),
    "Variable pobrezai no encontrada"
  )
})
