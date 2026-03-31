# Tests para funciones del MEC
# Usamos datos simulados para no depender de archivos locales

mec_sim <- data.frame(
  anio                    = c(2023, 2023, 2023, 2023),
  codigo_establecimiento  = c(1001, 1002, 1003, 1004),
  codigo_departamento     = c(11, 11, 1, 1),
  nombre_departamento     = c("Central", "Central",
                              "Capital", "Capital"),
  codigo_distrito         = c(1, 2, 1, 2),
  nombre_distrito         = c("Dist1", "Dist2",
                              "Dist3", "Dist4"),
  codigo_zona             = c(1, 2, 1, 2),
  nombre_zona             = c("Urbana", "Rural",
                              "Urbana", "Rural"),
  codigo_barrio_localidad = c(1, 2, 3, 4),
  nombre_barrio_localidad = c("B1", "B2", "B3", "B4"),
  codigo_institucion      = c(101, 102, 103, 104),
  nombre_institucion      = c("Esc1", "Esc2",
                              "Esc3", "Esc4"),
  sector_o_tipo_gestion   = c("Oficial", "Oficial",
                              "Privado", "Oficial"),
  anho_cod_geo            = c(2023, 2023, 2023, 2023),
  primer_grado_hombre     = c(50, 30, 80, 20),
  primer_grado_mujer      = c(48, 28, 75, 18),
  segundo_grado_hombre    = c(45, 25, 70, 15),
  segundo_grado_mujer     = c(43, 24, 68, 14),
  total_matriculados_hombre = c(95, 55, 150, 35),
  total_matriculados_mujer  = c(91, 52, 143, 32)
)

est_sim <- data.frame(
  anio                    = c(2023, 2023, 2023, 2023),
  codigo_establecimiento  = c(1001, 1002, 1003, 1004),
  nombre_departamento     = c("Central", "Central",
                              "Capital", "Capital"),
  nombre_zona             = c("Urbana", "Rural",
                              "Urbana", "Rural")
)

# --- mec_leer ---

test_that("mec_leer falla si archivo no existe", {
  expect_error(
    mec_leer("archivo_inexistente.csv"),
    "Archivo no encontrado"
  )
})

test_that("mec_leer falla con nivel invalido", {
  tmp <- tempfile(fileext = ".csv")
  write.csv(mec_sim, tmp, row.names = FALSE)
  expect_error(
    mec_leer(tmp, nivel = "universitario"),
    "nivel debe ser uno de"
  )
  file.remove(tmp)
})

# --- mec_matriculas_departamento ---

test_that("mec_matriculas_departamento retorna columnas correctas", {
  resultado <- mec_matriculas_departamento(mec_sim)

  expect_s3_class(resultado, "data.frame")
  expect_true(all(c("departamento", "total_hombres",
                    "total_mujeres", "total",
                    "porcentaje", "ipg") %in% names(resultado)))
})

test_that("mec_matriculas_departamento retorna 2 departamentos", {
  resultado <- mec_matriculas_departamento(mec_sim)
  expect_equal(nrow(resultado), 2)
})

test_that("mec_matriculas_departamento porcentajes suman 100", {
  resultado <- mec_matriculas_departamento(mec_sim)
  expect_equal(sum(resultado$porcentaje), 100, tolerance = 0.1)
})

test_that("mec_matriculas_departamento por_sector funciona", {
  resultado <- mec_matriculas_departamento(mec_sim,
                                           por_sector = TRUE)
  expect_true("sector_o_tipo_gestion" %in% names(resultado))
})

test_that("mec_matriculas_departamento falla sin nombre_departamento", {
  datos_mal <- mec_sim[, !names(mec_sim) %in% "nombre_departamento"]
  expect_error(
    mec_matriculas_departamento(datos_mal),
    "nombre_departamento no encontrada"
  )
})

# --- mec_matriculas_grado ---

test_that("mec_matriculas_grado retorna columnas correctas", {
  resultado <- mec_matriculas_grado(mec_sim)

  expect_s3_class(resultado, "data.frame")
  expect_true(all(c("grado", "total_hombres",
                    "total_mujeres", "total",
                    "ipg") %in% names(resultado)))
})

test_that("mec_matriculas_grado retorna 2 grados", {
  resultado <- mec_matriculas_grado(mec_sim)
  expect_equal(nrow(resultado), 2)
})

# --- mec_establecimientos_departamento ---

test_that("mec_establecimientos_departamento retorna columnas correctas", {
  resultado <- mec_establecimientos_departamento(est_sim)

  expect_s3_class(resultado, "data.frame")
  expect_true(all(c("departamento", "nombre_zona",
                    "n_establecimientos",
                    "porcentaje") %in% names(resultado)))
})

test_that("mec_establecimientos_departamento sin zona funciona", {
  resultado <- mec_establecimientos_departamento(est_sim,
                                                 por_zona = FALSE)
  expect_equal(nrow(resultado), 2)
  expect_false("nombre_zona" %in% names(resultado))
})

# --- grafico_mec_departamento ---

test_that("grafico_mec_departamento retorna ggplot", {
  g <- grafico_mec_departamento(mec_sim)
  expect_s3_class(g, "ggplot")
})

test_that("grafico_mec_departamento por_sector retorna ggplot", {
  g <- grafico_mec_departamento(mec_sim, por_sector = TRUE)
  expect_s3_class(g, "ggplot")
})

# --- grafico_mec_grado ---

test_that("grafico_mec_grado retorna ggplot", {
  g <- grafico_mec_grado(mec_sim)
  expect_s3_class(g, "ggplot")
})
