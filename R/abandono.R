# =============================================================================
# Funciones de analisis de abandono escolar - Paraguay
# Fuente: Datos MEC (mec_leer)
# El abandono se calcula como la reduccion de matricula entre grados
# =============================================================================

#' Tasa de abandono escolar por grado
#'
#' @description
#' Calcula la tasa de abandono escolar acumulado entre grados a partir
#' de datos de matriculacion del MEC. Se define como la reduccion
#' porcentual de matriculados entre el primer grado de referencia y
#' cada grado subsiguiente. Este indicador es clave para identificar
#' los puntos criticos de desercion en el sistema educativo paraguayo
#' (UNESCO, 2012; MEC Paraguay).
#'
#' @param data data.frame cargado con mec_leer(). Debe contener
#'   columnas de grados por sexo.
#' @param grado_base Grado de referencia para calcular el abandono.
#'   Por defecto "primer_grado" para educacion basica.
#'
#' @return Un tibble con columnas: anio, grado, total_hombres,
#'   total_mujeres, total, tasa_abandono, abandono_entre_grados.
#'
#' @export
#' @examples
#' \dontrun{
#' basica <- mec_leer("matriculaciones_educacion_escolar_basica.csv")
#' mec_tasa_abandono(basica)
#' }
mec_tasa_abandono <- function(data, grado_base = "primer_grado") {

  datos_grado <- mec_matriculas_grado(data)

  if (nrow(datos_grado) == 0) {
    stop("No se encontraron datos de grados en el archivo.")
  }

  # Obtener anio de los datos
  anio_datos <- if ("anio" %in% names(data)) {
    paste(unique(data$anio), collapse = "-")
  } else NA_character_

  # Ordenar grados correctamente
  orden_grados <- c(
    "Primer Grado", "Segundo Grado", "Tercer Grado",
    "Cuarto Grado", "Quinto Grado", "Sexto Grado",
    "Septimo Grado", "Octavo Grado", "Noveno Grado",
    "Maternal", "Prejardin", "Jardin", "Preescolar",
    "Matricula Cientifico", "Matricula Tecnico",
    "Matricula Media Abierta",
    "Matricula Formacion Profesional Media"
  )

  grados_presentes <- orden_grados[orden_grados %in% datos_grado$grado]

  if (length(grados_presentes) == 0) {
    grados_presentes <- datos_grado$grado
  }

  datos_ord <- datos_grado[match(grados_presentes, datos_grado$grado), ]
  datos_ord <- datos_ord[!is.na(datos_ord$grado), ]

  datos_ord |>
    dplyr::mutate(
      anio                  = anio_datos,
      tasa_abandono         = round(
        (1 - .data$total / .data$total[1]) * 100, 2
      ),
      abandono_entre_grados = dplyr::if_else(
        dplyr::row_number() == 1,
        0,
        round(
          (dplyr::lag(.data$total) - .data$total) /
            dplyr::lag(.data$total) * 100, 2
        )
      )
    ) |>
    dplyr::select(
      "anio", "grado", "total_hombres", "total_mujeres",
      "total", "tasa_abandono", "abandono_entre_grados"
    )
}


#' Tasa de abandono por departamento
#'
#' @description
#' Calcula la tasa de abandono escolar acumulado entre el primer y
#' el ultimo grado disponible, desagregado por departamento.
#' Permite identificar los departamentos con mayor desercion escolar
#' en Paraguay, informacion clave para politicas educativas
#' focalizadas territorialmente.
#'
#' @param data data.frame cargado con mec_leer(). Debe contener
#'   columnas de grados y nombre_departamento.
#'
#' @return Un tibble con columnas: anio, departamento,
#'   matricula_primer_grado, matricula_ultimo_grado,
#'   abandono_acumulado, tasa_abandono, ordenado de mayor
#'   a menor abandono.
#'
#' @export
#' @examples
#' \dontrun{
#' basica <- mec_leer("matriculaciones_educacion_escolar_basica.csv")
#' mec_abandono_departamento(basica)
#' }
mec_abandono_departamento <- function(data) {

  if (!"nombre_departamento" %in% names(data)) {
    stop("Variable nombre_departamento no encontrada.")
  }

  # Obtener anio de los datos
  anio_datos <- if ("anio" %in% names(data)) {
    paste(unique(data$anio), collapse = "-")
  } else NA_character_

  # Detectar columnas de primer y ultimo grado
  cols_hombre <- names(data)[
    grepl("_hombre$", names(data)) &
      !grepl("total_", names(data))
  ]

  if (length(cols_hombre) < 2) {
    stop("Se necesitan al menos 2 grados para calcular abandono.")
  }

  # Ordenar columnas segun orden natural de grados
  orden <- c("primer", "segundo", "tercer", "cuarto", "quinto",
             "sexto", "septimo", "octavo", "noveno",
             "maternal", "prejardin", "jardin", "preescolar")

  cols_ord <- c()
  for (o in orden) {
    col <- cols_hombre[grepl(o, cols_hombre, ignore.case = TRUE)]
    if (length(col) > 0) cols_ord <- c(cols_ord, col)
  }

  if (length(cols_ord) < 2) cols_ord <- cols_hombre

  primer_grado_h <- cols_ord[1]
  ultimo_grado_h <- cols_ord[length(cols_ord)]
  primer_grado_m <- gsub("_hombre$", "_mujer", primer_grado_h)
  ultimo_grado_m <- gsub("_hombre$", "_mujer", ultimo_grado_h)

  data |>
    dplyr::group_by(.data$nombre_departamento) |>
    dplyr::summarise(
      matricula_primer_grado = sum(
        .data[[primer_grado_h]] + .data[[primer_grado_m]],
        na.rm = TRUE
      ),
      matricula_ultimo_grado = sum(
        .data[[ultimo_grado_h]] + .data[[ultimo_grado_m]],
        na.rm = TRUE
      ),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      anio               = anio_datos,
      abandono_acumulado = .data$matricula_primer_grado -
        .data$matricula_ultimo_grado,
      tasa_abandono      = round(
        (1 - .data$matricula_ultimo_grado /
           .data$matricula_primer_grado) * 100, 2
      )
    ) |>
    dplyr::arrange(dplyr::desc(.data$tasa_abandono)) |>
    dplyr::rename(departamento = "nombre_departamento") |>
    dplyr::select(
      "anio", "departamento",
      "matricula_primer_grado", "matricula_ultimo_grado",
      "abandono_acumulado", "tasa_abandono"
    )
}


#' Grafico de tasa de abandono por grado
#'
#' @description
#' Genera un grafico de lineas mostrando la evolucion de la
#' matricula a lo largo de los grados, con la tasa de abandono
#' acumulado. Permite visualizar los puntos criticos de desercion
#' en el sistema educativo. Disenado para publicaciones academicas.
#'
#' @param data data.frame cargado con mec_leer().
#' @param titulo Titulo del grafico. Opcional.
#'
#' @return Un objeto ggplot2.
#'
#' @export
#' @examples
#' \dontrun{
#' basica <- mec_leer("matriculaciones_educacion_escolar_basica.csv")
#' grafico_abandono_grado(basica)
#' }
grafico_abandono_grado <- function(data, titulo = NULL) {

  datos <- mec_tasa_abandono(data)

  anio_datos <- if ("anio" %in% names(data)) {
    paste0(" (", paste(unique(data$anio), collapse = "-"), ")")
  } else ""

  if (is.null(titulo)) {
    titulo <- paste0(
      "Evolucion de matricula y abandono escolar por grado",
      anio_datos
    )
  }

  datos$grado <- factor(datos$grado, levels = unique(datos$grado))

  ggplot2::ggplot(
    datos,
    ggplot2::aes(x = .data$grado, y = .data$total, group = 1)
  ) +
    ggplot2::geom_line(linewidth = 1.2, color = "#1976D2") +
    ggplot2::geom_point(size = 3, color = "#1976D2") +
    ggplot2::geom_text(
      ggplot2::aes(
        label = paste0(
          scales::comma(.data$total),
          "\n(-", .data$tasa_abandono, "%)"
        )
      ),
      vjust = -0.8, size = 3, color = "#1565C0"
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::comma,
      limits = c(
        min(datos$total) * 0.85,
        max(datos$total) * 1.15
      )
    ) +
    ggplot2::labs(
      title    = titulo,
      subtitle = paste0(
        "Abandono acumulado del ciclo: ",
        round((1 - min(datos$total) / max(datos$total)) * 100, 1),
        "%"
      ),
      x        = NULL,
      y        = "Total matriculados",
      caption  = "Fuente: MEC Paraguay"
    ) +
    tema_paraguaydata() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )
}


#' Grafico de abandono por departamento
#'
#' @description
#' Genera un grafico de barras horizontales mostrando la tasa de
#' abandono escolar por departamento. Permite identificar los
#' departamentos con mayor desercion escolar en Paraguay.
#'
#' @param data data.frame cargado con mec_leer().
#' @param titulo Titulo del grafico. Opcional.
#'
#' @return Un objeto ggplot2.
#'
#' @export
#' @examples
#' \dontrun{
#' basica <- mec_leer("matriculaciones_educacion_escolar_basica.csv")
#' grafico_abandono_departamento(basica)
#' }
grafico_abandono_departamento <- function(data, titulo = NULL) {

  datos <- mec_abandono_departamento(data)

  anio_datos <- if ("anio" %in% names(data)) {
    paste0(" (", paste(unique(data$anio), collapse = "-"), ")")
  } else ""

  if (is.null(titulo)) {
    titulo <- paste0(
      "Tasa de abandono escolar por departamento",
      anio_datos
    )
  }

  ggplot2::ggplot(
    datos,
    ggplot2::aes(
      x    = stats::reorder(.data$departamento, .data$tasa_abandono),
      y    = .data$tasa_abandono,
      fill = .data$tasa_abandono
    )
  ) +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::geom_text(
      ggplot2::aes(
        label = paste0(round(.data$tasa_abandono, 1), "%")
      ),
      hjust = -0.1, size = 3.5
    ) +
    ggplot2::scale_fill_gradient(
      low = "#FFF9C4", high = "#F57F17"
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, max(datos$tasa_abandono) * 1.2),
      labels = function(x) paste0(x, "%")
    ) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title   = titulo,
      x       = NULL,
      y       = "Tasa de abandono (%)",
      caption = "Fuente: MEC Paraguay"
    ) +
    tema_paraguaydata()
}
