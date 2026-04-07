# Tests para funciones ODS 4

ephc_ods_sim <- data.frame(
  ESTGEO       = c(1, 11, 16, 111, 116, 31, 36, 51, 56, 101),
  P02          = c(10, 16, 20, 17, 30, 45, 60, 11, 14, 22),
  A05          = c(1, 1, 1, 6, 6, 6, 6, 1, 6, 1),
  P06          = c(1, 6, 1, 6, 1, 6, 1, 6, 1, 6),
  AREA         = c(1, 1, 6, 6, 1, 6, 1, 6, 1, 6),
  A14REC       = c(2, 3, 4, 4, 5, 6, 7, 2, 3, 4),
  A17A         = c(6, 9, 12, 12, 15, 17, 17, 6, 9, 12),
  PEAD         = c(1, 1, 2, 1, 1, 2, 1, 3, 1, 1),
  Informalidad = c(1, 2, NA, 1, 1, NA, 2, NA, 1, 2),
  HORAB        = c(40, 25, NA, 35, 20, NA, 45, NA, 28, 50),
  ANIO         = rep(2024, 10),
  TRIMESTRE    = rep(1, 10),
  Factor       = rep(100, 10)
)

mec_ods_sim <- data.frame(
  anio                    = rep(2023, 4),
  nombre_departamento     = c("Central", "Central",
                              "Capital", "Capital"),
  nombre_zona             = c("Urbana", "Rural",
                              "Urbana", "Rural"),
  sector_o_tipo_gestion   = c("Oficial", "Oficial",
                              "Privado", "Oficial"),
  anho_cod_geo            = rep(2023, 4),
  primer_grado_hombre     = c(100, 80, 120, 60),
  primer_grado_mujer      = c(95,  75, 115, 55),
  noveno_grado_hombre     = c(70,  50,  90, 40),
  noveno_grado_mujer      = c(65,  45,  85, 35),
  total_matriculados_hombre = c(100, 80, 120, 60),
  total_matriculados_mujer  = c(95,  75, 115, 55)
)

# --- ods4_ephc ---

test_that("ods4_ephc retorna 6 indicadores", {
  resultado <- ods4_ephc(ephc_ods_sim)
  expect_equal(nrow(resultado), 6)
})

test_that("ods4_ephc retorna columnas correctas", {
  resultado <- ods4_ephc(ephc_ods_sim)
  expect_true(all(c("codigo_ods", "indicador", "valor",
                    "unidad", "fuente", "anio") %in%
                    names(resultado)))
})

test_that("ods4_ephc incluye anio correcto", {
  resultado <- ods4_ephc(ephc_ods_sim)
  expect_true(all(resultado$anio == 2024, na.rm = TRUE))
})

test_that("ods4_ephc falla sin datos EPHC", {
  expect_error(
    ods4_ephc(data.frame(x = 1)),
    "no parecen ser de la EPHC"
  )
})

# --- ods4_mec ---

test_that("ods4_mec retorna indicadores con data_basica", {
  resultado <- ods4_mec(data_basica = mec_ods_sim)
  expect_s3_class(resultado, "data.frame")
  expect_gt(nrow(resultado), 0)
})

test_that("ods4_mec falla sin datos", {
  expect_error(
    ods4_mec(),
    "al menos un archivo"
  )
})

test_that("ods4_mec incluye codigo_ods 4.1b con basica", {
  resultado <- ods4_mec(data_basica = mec_ods_sim)
  expect_true("4.1b" %in% resultado$codigo_ods)
})

# --- ods4_resumen ---

test_that("ods4_resumen combina EPHC y MEC", {
  resultado <- ods4_resumen(
    ephc        = ephc_ods_sim,
    data_basica = mec_ods_sim
  )
  expect_s3_class(resultado, "data.frame")
  expect_gt(nrow(resultado), 6)
})

test_that("ods4_resumen ordenado por codigo_ods", {
  resultado <- ods4_resumen(ephc = ephc_ods_sim)
  expect_equal(resultado$codigo_ods,
               sort(resultado$codigo_ods))
})

test_that("ods4_resumen falla sin datos", {
  expect_error(
    ods4_resumen(),
    "al menos una fuente"
  )
})

test_that("ods4_resumen solo con EPHC retorna 6 filas", {
  resultado <- ods4_resumen(ephc = ephc_ods_sim)
  expect_equal(nrow(resultado), 6)
})
