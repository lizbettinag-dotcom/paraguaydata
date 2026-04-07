# Tests para funciones de abandono escolar

abandono_sim <- data.frame(
  anio                      = c(2023, 2023, 2023, 2023),
  nombre_departamento       = c("Central", "Central",
                                "Capital", "Capital"),
  nombre_zona               = c("Urbana", "Rural",
                                "Urbana", "Rural"),
  sector_o_tipo_gestion     = c("Oficial", "Oficial",
                                "Privado", "Oficial"),
  anho_cod_geo              = c(2023, 2023, 2023, 2023),
  primer_grado_hombre       = c(100, 80, 120, 60),
  primer_grado_mujer        = c(95,  75, 115, 55),
  segundo_grado_hombre      = c(90,  70, 110, 55),
  segundo_grado_mujer       = c(85,  65, 105, 50),
  noveno_grado_hombre       = c(70,  50,  90, 40),
  noveno_grado_mujer        = c(65,  45,  85, 35),
  total_matriculados_hombre = c(100, 80, 120, 60),
  total_matriculados_mujer  = c(95,  75, 115, 55)
)

# --- mec_tasa_abandono ---

test_that("mec_tasa_abandono retorna columnas correctas", {
  resultado <- mec_tasa_abandono(abandono_sim)

  expect_s3_class(resultado, "data.frame")
  expect_true(all(c("anio", "grado", "total",
                    "tasa_abandono",
                    "abandono_entre_grados") %in%
                    names(resultado)))
})

test_that("mec_tasa_abandono incluye anio correcto", {
  resultado <- mec_tasa_abandono(abandono_sim)
  expect_true(all(resultado$anio == "2023"))
})

test_that("mec_tasa_abandono primer grado tiene tasa 0", {
  resultado <- mec_tasa_abandono(abandono_sim)
  expect_equal(resultado$tasa_abandono[1], 0)
})

test_that("mec_tasa_abandono tasa aumenta con los grados", {
  resultado <- mec_tasa_abandono(abandono_sim)
  expect_gte(resultado$tasa_abandono[nrow(resultado)],
             resultado$tasa_abandono[1])
})

# --- mec_abandono_departamento ---

test_that("mec_abandono_departamento retorna columnas correctas", {
  resultado <- mec_abandono_departamento(abandono_sim)

  expect_s3_class(resultado, "data.frame")
  expect_true(all(c("anio", "departamento",
                    "matricula_primer_grado",
                    "matricula_ultimo_grado",
                    "abandono_acumulado",
                    "tasa_abandono") %in% names(resultado)))
})

test_that("mec_abandono_departamento retorna 2 departamentos", {
  resultado <- mec_abandono_departamento(abandono_sim)
  expect_equal(nrow(resultado), 2)
})

test_that("mec_abandono_departamento incluye anio", {
  resultado <- mec_abandono_departamento(abandono_sim)
  expect_false(all(is.na(resultado$anio)))
})

test_that("mec_abandono_departamento tasa entre 0 y 100", {
  resultado <- mec_abandono_departamento(abandono_sim)
  expect_gte(min(resultado$tasa_abandono), 0)
  expect_lte(max(resultado$tasa_abandono), 100)
})

test_that("mec_abandono_departamento falla sin nombre_departamento", {
  datos_mal <- abandono_sim[,
                            !names(abandono_sim) %in% "nombre_departamento"]
  expect_error(
    mec_abandono_departamento(datos_mal),
    "nombre_departamento no encontrada"
  )
})

# --- grafico_abandono_grado ---

test_that("grafico_abandono_grado retorna ggplot", {
  g <- grafico_abandono_grado(abandono_sim)
  expect_s3_class(g, "ggplot")
})

test_that("grafico_abandono_grado con titulo personalizado", {
  g <- grafico_abandono_grado(abandono_sim,
                              titulo = "Mi grafico")
  expect_s3_class(g, "ggplot")
})

# --- grafico_abandono_departamento ---

test_that("grafico_abandono_departamento retorna ggplot", {
  g <- grafico_abandono_departamento(abandono_sim)
  expect_s3_class(g, "ggplot")
})
