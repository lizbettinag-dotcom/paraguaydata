# =============================================================================
# Datos y funciones PISA - Paraguay y America Latina
# Fuente: OCDE, PISA 2022 Results (Volume I), diciembre 2023
# =============================================================================

#' Resultados PISA 2022 - America Latina y el Caribe
#'
#' @description
#' Dataset con los puntajes promedio de PISA 2022 para los 14 paises
#' de America Latina y el Caribe que participaron. Incluye puntajes
#' en matematica, lectura y ciencias, ranking global y anio de primera
#' participacion. Paraguay participo por segunda vez en 2022.
#'
#' @format Un data.frame con 14 filas y 7 columnas:
#' \describe{
#'   \item{pais}{Nombre del pais}
#'   \item{codigo_iso}{Codigo ISO 3 del pais}
#'   \item{matematica}{Puntaje promedio en matematica}
#'   \item{lectura}{Puntaje promedio en lectura}
#'   \item{ciencias}{Puntaje promedio en ciencias}
#'   \item{ranking_matematica}{Posicion en ranking global de matematica
#'     (de 81 paises)}
#'   \item{primera_participacion}{Anio de primera participacion en PISA}
#' }
#'
#' @source OCDE (2023). PISA 2022 Results (Volume I): The State of
#'   Learning and Equity in Education. OECD Publishing, Paris.
#'   https://doi.org/10.1787/53f23881-en
"pisa_latam_2022"


#' Resultados historicos PISA - Paraguay 2017 y 2022
#'
#' @description
#' Dataset con los puntajes promedio de Paraguay en las dos ediciones
#' de PISA en que ha participado (2017 y 2022). Incluye comparacion
#' con el promedio OCDE y porcentaje de estudiantes con bajo rendimiento
#' (por debajo del nivel 2 de competencia).
#'
#' @format Un data.frame con 2 filas y 12 columnas:
#' \describe{
#'   \item{anio}{Anio de aplicacion de la prueba}
#'   \item{matematica}{Puntaje promedio en matematica}
#'   \item{lectura}{Puntaje promedio en lectura}
#'   \item{ciencias}{Puntaje promedio en ciencias}
#'   \item{promedio_ocde_matematica}{Promedio OCDE en matematica}
#'   \item{promedio_ocde_lectura}{Promedio OCDE en lectura}
#'   \item{promedio_ocde_ciencias}{Promedio OCDE en ciencias}
#'   \item{pct_bajo_rendimiento_matematica}{Porcentaje bajo nivel 2
#'     en matematica}
#'   \item{pct_bajo_rendimiento_lectura}{Porcentaje bajo nivel 2
#'     en lectura}
#'   \item{pct_bajo_rendimiento_ciencias}{Porcentaje bajo nivel 2
#'     en ciencias}
#'   \item{n_estudiantes}{Numero de estudiantes evaluados}
#'   \item{n_escuelas}{Numero de escuelas participantes}
#' }
#'
#' @source OCDE (2023). PISA 2022 Results (Volume I). OECD Publishing.
#'   MEC Paraguay (2023). Informe Nacional PISA 2022.
"pisa_paraguay"


#' Comparacion de Paraguay con America Latina en PISA 2022
#'
#' @description
#' Genera una tabla comparativa de Paraguay con los paises de America
#' Latina en PISA 2022. Muestra la posicion de Paraguay en la region
#' y la brecha con el promedio OCDE. Util para articulos sobre calidad
#' educativa y desarrollo humano en Paraguay.
#'
#' @param materia Materia a comparar: "matematica", "lectura",
#'   "ciencias" o "todas". Por defecto "todas".
#' @param incluir_ocde Logico. Si TRUE incluye el promedio OCDE
#'   como referencia. Por defecto TRUE.
#'
#' @return Un tibble con la comparacion regional ordenada por puntaje.
#'
#' @export
#' @examples
#' pisa_comparar_latam()
#' pisa_comparar_latam(materia = "matematica")
pisa_comparar_latam <- function(materia      = "todas",
                                incluir_ocde = TRUE) {

  if (!materia %in% c("todas", "matematica", "lectura", "ciencias")) {
    stop("materia debe ser 'todas', 'matematica', 'lectura' o 'ciencias'")
  }

  datos <- pisa_latam_2022

  if (incluir_ocde) {
    ocde <- data.frame(
      pais                  = "Promedio OCDE",
      codigo_iso            = "OCDE",
      matematica            = 472L,
      lectura               = 476L,
      ciencias              = 485L,
      ranking_matematica    = NA_integer_,
      primera_participacion = 2000L,
      stringsAsFactors      = FALSE
    )
    datos <- dplyr::bind_rows(datos, ocde)
  }

  if (materia == "todas") {
    datos |>
      dplyr::select(
        "pais", "matematica", "lectura",
        "ciencias", "ranking_matematica"
      ) |>
      dplyr::mutate(
        brecha_ocde_mat = .data$matematica - 472L
      ) |>
      dplyr::arrange(dplyr::desc(.data$matematica))
  } else {
    ref_ocde <- dplyr::case_when(
      materia == "matematica" ~ 472L,
      materia == "lectura"    ~ 476L,
      materia == "ciencias"   ~ 485L
    )
    datos |>
      dplyr::select("pais", "codigo_iso",
                    puntaje = dplyr::all_of(materia)) |>
      dplyr::mutate(
        brecha_ocde = .data$puntaje - ref_ocde
      ) |>
      dplyr::arrange(dplyr::desc(.data$puntaje))
  }
}


#' Evolucion historica de Paraguay en PISA
#'
#' @description
#' Genera una tabla con la evolucion de los puntajes de Paraguay en
#' PISA 2017 y 2022, incluyendo la variacion entre ediciones y la
#' brecha con el promedio OCDE. Paraguay mejoro en matematica (+12) y
#' ciencias (+23) entre 2017 y 2022 (OCDE, 2023).
#'
#' @return Un tibble con la evolucion historica de Paraguay en PISA.
#'
#' @export
#' @examples
#' pisa_evolucion_paraguay()
pisa_evolucion_paraguay <- function() {
  pisa_paraguay |>
    dplyr::mutate(
      brecha_ocde_matematica = .data$matematica -
        .data$promedio_ocde_matematica,
      brecha_ocde_lectura    = .data$lectura -
        .data$promedio_ocde_lectura,
      brecha_ocde_ciencias   = .data$ciencias -
        .data$promedio_ocde_ciencias
    )
}


#' Grafico comparativo PISA America Latina 2022
#'
#' @description
#' Genera un grafico de barras horizontales comparando los puntajes
#' PISA 2022 de los paises de America Latina. Paraguay aparece
#' destacado en color diferente. Incluye linea de referencia del
#' promedio OCDE. Disenado para publicaciones academicas.
#'
#' @param materia Materia a graficar: "matematica", "lectura" o
#'   "ciencias". Por defecto "matematica".
#' @param titulo Titulo del grafico. Opcional.
#'
#' @return Un objeto ggplot2.
#'
#' @export
#' @examples
#' grafico_pisa_latam()
#' grafico_pisa_latam(materia = "lectura")
#' grafico_pisa_latam(materia = "ciencias")
grafico_pisa_latam <- function(materia = "matematica",
                               titulo  = NULL) {

  if (!materia %in% c("matematica", "lectura", "ciencias")) {
    stop("materia debe ser 'matematica', 'lectura' o 'ciencias'")
  }

  datos <- pisa_latam_2022 |>
    dplyr::filter(!is.na(.data[[materia]])) |>
    dplyr::mutate(
      destacado = dplyr::if_else(
        .data$pais == "Paraguay", "Paraguay", "Otros"
      )
    )

  promedio_ocde <- dplyr::case_when(
    materia == "matematica" ~ 472,
    materia == "lectura"    ~ 476,
    materia == "ciencias"   ~ 485
  )

  etiqueta_x <- dplyr::case_when(
    materia == "matematica" ~ "Puntaje promedio en Matematica",
    materia == "lectura"    ~ "Puntaje promedio en Lectura",
    materia == "ciencias"   ~ "Puntaje promedio en Ciencias"
  )

  if (is.null(titulo)) {
    titulo <- paste0(
      "PISA 2022 - ", tools::toTitleCase(materia),
      " - America Latina y el Caribe"
    )
  }

  ggplot2::ggplot(
    datos,
    ggplot2::aes(
      x    = .data[[materia]],
      y    = stats::reorder(.data$pais, .data[[materia]]),
      fill = .data$destacado
    )
  ) +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::geom_text(
      ggplot2::aes(label = .data[[materia]]),
      hjust = -0.2, size = 3.5
    ) +
    ggplot2::geom_vline(
      xintercept = promedio_ocde,
      linetype   = "dashed",
      color      = "grey40",
      linewidth  = 0.8
    ) +
    ggplot2::annotate(
      "text",
      x     = promedio_ocde + 2,
      y     = 0.5,
      label = paste0("OCDE: ", promedio_ocde),
      hjust = 0, size = 3, color = "grey40"
    ) +
    ggplot2::scale_fill_manual(
      values = c("Paraguay" = "#E53935", "Otros" = "#90CAF9")
    ) +
    ggplot2::scale_x_continuous(
      limits = c(300, max(datos[[materia]], na.rm = TRUE) * 1.1)
    ) +
    ggplot2::labs(
      title   = titulo,
      x       = etiqueta_x,
      y       = NULL,
      caption = "Fuente: OCDE, PISA 2022 Results (Volume I)"
    ) +
    tema_paraguaydata()
}
