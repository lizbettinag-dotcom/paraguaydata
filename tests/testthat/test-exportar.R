# Tests para funciones de exportacion

exp_sim <- data.frame(
  grupo          = c("Hombre", "Mujer"),
  tasa_desempleo = c(5.04, 7.70)
)

# --- exportar_word ---

test_that("exportar_word crea archivo .docx", {
  tmp <- tempfile(fileext = ".docx")
  exportar_word(exp_sim, archivo = tmp,
                titulo = "Tabla 1. Test")
  expect_true(file.exists(tmp))
  unlink(tmp)
})

test_that("exportar_word retorna ruta invisible", {
  tmp <- tempfile(fileext = ".docx")
  resultado <- exportar_word(exp_sim, archivo = tmp,
                             titulo = "Tabla 1. Test")
  expect_equal(resultado, tmp)
  unlink(tmp)
})

test_that("exportar_word acepta nota personalizada", {
  tmp <- tempfile(fileext = ".docx")
  exportar_word(exp_sim, archivo = tmp,
                titulo = "Tabla 1.",
                nota   = "Nota. Fuente propia.")
  expect_true(file.exists(tmp))
  unlink(tmp)
})

# --- exportar_word_multiples ---

test_that("exportar_word_multiples crea archivo con varias tablas", {
  tmp <- tempfile(fileext = ".docx")
  mis_tablas <- list(
    "Tabla 1. Test A" = exp_sim,
    "Tabla 2. Test B" = exp_sim
  )
  exportar_word_multiples(mis_tablas, archivo = tmp)
  expect_true(file.exists(tmp))
  unlink(tmp)
})

test_that("exportar_word_multiples falla con lista vacia", {
  expect_error(
    exportar_word_multiples(list()),
    "lista no vacia"
  )
})

# --- exportar_ods4_word ---

test_that("exportar_ods4_word crea archivo .docx", {
  ods_sim <- data.frame(
    codigo_ods = c("4.1", "4.3"),
    indicador  = c("Finalizacion basica", "Escolarizacion"),
    valor      = c(75.5, 6.64),
    unidad     = c("%", "%"),
    fuente     = c("EPHC", "EPHC"),
    anio       = c(2024, 2024)
  )
  tmp <- tempfile(fileext = ".docx")
  exportar_ods4_word(ods_sim,
                     archivo = tmp,
                     titulo  = "Tabla 1. ODS 4 Test")
  expect_true(file.exists(tmp))
  unlink(tmp)
})
