# =============================================================================
# Funciones de visualizacion - paraguaydata
# Graficos listos para publicacion academica usando ggplot2
# =============================================================================

#' Tema visual para graficos de paraguaydata
#'
#' @description
#' Tema personalizado basado en ggplot2::theme_minimal() optimizado
#' para publicaciones academicas. Tipografia limpia, sin fondo gris,
#' con grilla suave y etiquetas legibles.
#'
#' @return Un objeto theme de ggplot2.
#' @export
#' @examples
#' \dontrun{
#' library(ggplot2)
#' ggplot() + tema_paraguaydata()
#' }
tema_paraguaydata <- function() {
  ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title      = ggplot2::element_text(
        size = 13, face = "bold", margin = ggplot2::margin(b = 8)
      ),
      plot.subtitle   = ggplot2::element_text(
        size = 11, color = "grey40", margin = ggplot2::margin(b = 12)
      ),
      plot.caption    = ggplot2::element_text(
        size = 9, color = "grey50", hjust = 0,
        margin = ggplot2::margin(t = 10)
      ),
      axis.text       = ggplot2::element_text(size = 10),
      axis.title      = ggplot2::element_text(size = 11),
      panel.grid.major = ggplot2::element_line(color = "grey90"),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "bottom",
      legend.title    = ggplot2::element_text(size = 10),
      legend.text     = ggplot2::element_text(size = 9)
    )
}


#' Grafico de tasa de escolarizacion
#'
#' @description
#' Genera un grafico de barras con la tasa de escolarizacion
#' desagregada por area geografica o sexo. Listo para publicacion
#' academica con fuente y titulo incluidos.
#'
#' @param data data.frame de ine_ephc(). Requiere P02, A05 y
#'   opcionalmente Factor, P06, AREA.
#' @param grupo Desagregacion: "area" o "sexo". Por defecto "area".
#' @param titulo Titulo del grafico. Opcional.
#'
#' @return Un objeto ggplot2.
#' @export
#' @examples
#' \dontrun{
#' ephc <- ine_ephc(2024, 1)
#' grafico_escolarizacion(ephc)
#' grafico_escolarizacion(ephc, grupo = "sexo")
#' }
grafico_escolarizacion <- function(data,
                                   grupo  = "area",
                                   titulo = NULL) {

  if (!grupo %in% c("area", "sexo")) {
    stop("grupo debe ser 'area' o 'sexo'")
  }

  datos <- tasa_escolarizacion(data, grupo = grupo)

  if (is.null(titulo)) {
    titulo <- paste0("Tasa de escolarizacion por ",
                     ifelse(grupo == "area", "area geografica", "sexo"))
  }

  colores <- if (grupo == "area") {
    c("Urbana" = "#2196F3", "Rural" = "#4CAF50")
  } else {
    c("Hombre" = "#1976D2", "Mujer" = "#E91E63")
  }

  ggplot2::ggplot(
    datos,
    ggplot2::aes(
      x    = .data$grupo,
      y    = .data$tasa_escolarizacion,
      fill = .data$grupo
    )
  ) +
    ggplot2::geom_col(width = 0.6, show.legend = FALSE) +
    ggplot2::geom_text(
      ggplot2::aes(
        label = paste0(round(.data$tasa_escolarizacion, 1), "%")
      ),
      vjust = -0.5, size = 4, fontface = "bold"
    ) +
    ggplot2::scale_fill_manual(values = colores) +
    ggplot2::scale_y_continuous(
      limits = c(0, max(datos$tasa_escolarizacion) * 1.2),
      labels = function(x) paste0(x, "%")
    ) +
    ggplot2::labs(
      title    = titulo,
      subtitle = "Poblacion de 10 a 18 anos",
      x        = NULL,
      y        = "Tasa de escolarizacion (%)",
      caption  = "Fuente: EPHC - INE Paraguay"
    ) +
    tema_paraguaydata()
}


#' Grafico de brechas educativas
#'
#' @description
#' Genera un grafico de barras comparativo mostrando brechas
#' educativas entre grupos. Incluye la brecha absoluta como
#' anotacion para facilitar la interpretacion academica.
#'
#' @param data data.frame de ine_ephc().
#' @param tipo Tipo de brecha: "urbano_rural" o "genero".
#'   Por defecto "urbano_rural".
#' @param indicador Indicador: "escolarizacion" o "anos_estudio".
#'   Por defecto "escolarizacion".
#' @param titulo Titulo del grafico. Opcional.
#'
#' @return Un objeto ggplot2.
#' @export
#' @examples
#' \dontrun{
#' ephc <- ine_ephc(2024, 1)
#' grafico_brecha(ephc)
#' grafico_brecha(ephc, tipo = "genero")
#' grafico_brecha(ephc, indicador = "anos_estudio")
#' }
grafico_brecha <- function(data,
                           tipo      = "urbano_rural",
                           indicador = "escolarizacion",
                           titulo    = NULL) {

  if (!tipo %in% c("urbano_rural", "genero")) {
    stop("tipo debe ser 'urbano_rural' o 'genero'")
  }

  if (!indicador %in% c("escolarizacion", "anos_estudio")) {
    stop("indicador debe ser 'escolarizacion' o 'anos_estudio'")
  }

  # Obtener datos segun tipo
  datos <- if (tipo == "urbano_rural") {
    brecha_urbano_rural(data, indicador = indicador)
  } else {
    brecha_genero(data, indicador = indicador)
  }

  brecha <- round(abs(datos$brecha_absoluta[1]), 2)

  etiqueta_y <- if (indicador == "escolarizacion") {
    "Tasa (%)"
  } else {
    "Anos promedio de estudio"
  }

  if (is.null(titulo)) {
    titulo <- paste0(
      "Brecha ",
      ifelse(tipo == "urbano_rural", "urbano-rural", "de genero"),
      " en ",
      ifelse(indicador == "escolarizacion",
             "escolarizacion", "anos de estudio")
    )
  }

  colores <- if (tipo == "urbano_rural") {
    c("Urbana" = "#2196F3", "Rural" = "#4CAF50")
  } else {
    c("Hombre" = "#1976D2", "Mujer" = "#E91E63")
  }

  ggplot2::ggplot(
    datos,
    ggplot2::aes(
      x    = .data$grupo,
      y    = .data$valor,
      fill = .data$grupo
    )
  ) +
    ggplot2::geom_col(width = 0.6, show.legend = FALSE) +
    ggplot2::geom_text(
      ggplot2::aes(label = round(.data$valor, 1)),
      vjust = -0.5, size = 4, fontface = "bold"
    ) +
    ggplot2::scale_fill_manual(values = colores) +
    ggplot2::scale_y_continuous(
      limits = c(0, max(datos$valor) * 1.25)
    ) +
    ggplot2::annotate(
      "text",
      x     = 1.5,
      y     = max(datos$valor) * 1.15,
      label = paste0("Brecha: ", brecha),
      size  = 4,
      color = "grey30"
    ) +
    ggplot2::labs(
      title    = titulo,
      x        = NULL,
      y        = etiqueta_y,
      caption  = "Fuente: EPHC - INE Paraguay"
    ) +
    tema_paraguaydata()
}


#' Grafico de indicadores de empleo
#'
#' @description
#' Genera un grafico de barras con los principales indicadores
#' del mercado laboral: desempleo, informalidad y subempleo.
#' Disenado para comparacion rapida en publicaciones academicas.
#'
#' @param data data.frame de ine_ephc().
#' @param grupo Desagregacion: "ninguno", "area" o "sexo".
#'   Por defecto "ninguno".
#'
#' @return Un objeto ggplot2.
#' @export
#' @examples
#' \dontrun{
#' ephc <- ine_ephc(2024, 1)
#' grafico_empleo(ephc)
#' grafico_empleo(ephc, grupo = "sexo")
#' }
grafico_empleo <- function(data, grupo = "ninguno") {

  if (!grupo %in% c("ninguno", "area", "sexo")) {
    stop("grupo debe ser 'ninguno', 'area' o 'sexo'")
  }

  # Calcular los tres indicadores
  desempleo    <- tasa_desempleo(data, grupo = grupo)
  informalidad <- tasa_informalidad(data, grupo = grupo)
  subempleo    <- tasa_subempleo(data, grupo = grupo)

  # Renombrar columna de valor
  desempleo$tasa    <- desempleo$tasa_desempleo
  informalidad$tasa <- informalidad$tasa_informalidad
  subempleo$tasa    <- subempleo$tasa_subempleo

  # Agregar etiqueta de indicador
  desempleo$indicador    <- "Desempleo"
  informalidad$indicador <- "Informalidad"
  subempleo$indicador    <- "Subempleo"

  # Combinar
  cols_comunes <- c("grupo", "tasa", "indicador")
  datos <- dplyr::bind_rows(
    desempleo[, cols_comunes],
    informalidad[, cols_comunes],
    subempleo[, cols_comunes]
  )

  colores_indicador <- c(
    "Desempleo"    = "#F44336",
    "Informalidad" = "#FF9800",
    "Subempleo"    = "#FFC107"
  )

  if (grupo == "ninguno") {
    ggplot2::ggplot(
      datos,
      ggplot2::aes(
        x    = .data$indicador,
        y    = .data$tasa,
        fill = .data$indicador
      )
    ) +
      ggplot2::geom_col(width = 0.6, show.legend = FALSE) +
      ggplot2::geom_text(
        ggplot2::aes(label = paste0(round(.data$tasa, 1), "%")),
        vjust = -0.5, size = 4, fontface = "bold"
      ) +
      ggplot2::scale_fill_manual(values = colores_indicador) +
      ggplot2::scale_y_continuous(
        limits = c(0, max(datos$tasa) * 1.2),
        labels = function(x) paste0(x, "%")
      ) +
      ggplot2::labs(
        title   = "Indicadores del mercado laboral",
        x       = NULL,
        y       = "Tasa (%)",
        caption = "Fuente: EPHC - INE Paraguay"
      ) +
      tema_paraguaydata()

  } else {
    ggplot2::ggplot(
      datos,
      ggplot2::aes(
        x    = .data$grupo,
        y    = .data$tasa,
        fill = .data$indicador
      )
    ) +
      ggplot2::geom_col(
        position = "dodge", width = 0.7
      ) +
      ggplot2::geom_text(
        ggplot2::aes(label = paste0(round(.data$tasa, 1), "%")),
        position = ggplot2::position_dodge(width = 0.7),
        vjust = -0.5, size = 3
      ) +
      ggplot2::scale_fill_manual(
        values = colores_indicador,
        name   = "Indicador"
      ) +
      ggplot2::scale_y_continuous(
        limits = c(0, max(datos$tasa) * 1.2),
        labels = function(x) paste0(x, "%")
      ) +
      ggplot2::labs(
        title   = "Indicadores del mercado laboral",
        x       = NULL,
        y       = "Tasa (%)",
        caption = "Fuente: EPHC - INE Paraguay"
      ) +
      tema_paraguaydata()
  }
}


#' Grafico de tendencia temporal
#'
#' @description
#' Genera un grafico de lineas mostrando la evolucion temporal
#' de un indicador educativo. Usa los datos de ine_serie() y
#' tendencia_escolarizacion(). Ideal para articulos que analizan
#' cambios en el tiempo.
#'
#' @param serie data.frame de ine_serie(). Debe tener columna PERIODO.
#' @param indicador Indicador: "escolarizacion", "anos_estudio" o
#'   "rezago". Por defecto "escolarizacion".
#' @param grupo Desagregacion: "ninguno", "area" o "sexo".
#'   Por defecto "ninguno".
#' @param titulo Titulo del grafico. Opcional.
#'
#' @return Un objeto ggplot2.
#' @export
#' @examples
#' \dontrun{
#' serie <- ine_serie(2022, 2024, trimestres = 1)
#' grafico_tendencia(serie)
#' grafico_tendencia(serie, grupo = "area")
#' }
grafico_tendencia <- function(serie,
                              indicador = "escolarizacion",
                              grupo     = "ninguno",
                              titulo    = NULL) {

  if (!"PERIODO" %in% names(serie)) {
    stop("El data.frame debe tener columna PERIODO.",
         " Usa ine_serie() para descargar la serie temporal.")
  }

  datos <- tendencia_escolarizacion(serie,
                                    indicador = indicador,
                                    grupo     = grupo)

  etiqueta_y <- dplyr::case_when(
    indicador == "escolarizacion" ~ "Tasa de escolarizacion (%)",
    indicador == "anos_estudio"   ~ "Anos promedio de estudio",
    indicador == "rezago"         ~ "Tasa de rezago (%)",
    TRUE                          ~ "Valor"
  )

  if (is.null(titulo)) {
    titulo <- dplyr::case_when(
      indicador == "escolarizacion" ~ "Evolucion de la tasa de escolarizacion",
      indicador == "anos_estudio"   ~ "Evolucion de anos promedio de estudio",
      indicador == "rezago"         ~ "Evolucion del rezago educativo",
      TRUE                          ~ "Evolucion del indicador"
    )
  }

  if ("grupo" %in% names(datos)) {
    colores <- c(
      "Urbana" = "#2196F3", "Rural"  = "#4CAF50",
      "Hombre" = "#1976D2", "Mujer"  = "#E91E63",
      "Total"  = "#9C27B0"
    )

    ggplot2::ggplot(
      datos,
      ggplot2::aes(
        x     = .data$PERIODO,
        y     = .data$valor,
        color = .data$grupo,
        group = .data$grupo
      )
    ) +
      ggplot2::geom_line(linewidth = 1.2) +
      ggplot2::geom_point(size = 3) +
      ggplot2::geom_text(
        ggplot2::aes(label = round(.data$valor, 1)),
        vjust = -0.8, size = 3.5
      ) +
      ggplot2::scale_color_manual(
        values = colores, name = NULL
      ) +
      ggplot2::labs(
        title    = titulo,
        x        = "Periodo",
        y        = etiqueta_y,
        caption  = "Fuente: EPHC - INE Paraguay"
      ) +
      tema_paraguaydata()

  } else {
    ggplot2::ggplot(
      datos,
      ggplot2::aes(x = .data$PERIODO, y = .data$valor, group = 1)
    ) +
      ggplot2::geom_line(linewidth = 1.2, color = "#2196F3") +
      ggplot2::geom_point(size = 3, color = "#2196F3") +
      ggplot2::geom_text(
        ggplot2::aes(label = round(.data$valor, 1)),
        vjust = -0.8, size = 3.5, color = "#1565C0"
      ) +
      ggplot2::labs(
        title    = titulo,
        x        = "Periodo",
        y        = etiqueta_y,
        caption  = "Fuente: EPHC - INE Paraguay"
      ) +
      tema_paraguaydata()
  }
}


#' Grafico de distribucion por rama de actividad
#'
#' @description
#' Genera un grafico de barras horizontales con la distribucion
#' de la poblacion ocupada por rama de actividad economica.
#' Las barras horizontales facilitan la lectura de etiquetas largas
#' en publicaciones academicas.
#'
#' @param data data.frame de ine_ephc(). Requiere RAMA_PEA.
#' @param titulo Titulo del grafico. Opcional.
#'
#' @return Un objeto ggplot2.
#' @export
#' @examples
#' \dontrun{
#' ephc <- ine_ephc(2024, 1)
#' grafico_rama(ephc)
#' }
grafico_rama <- function(data, titulo = NULL) {

  datos <- distribucion_rama(data)

  if (is.null(titulo)) {
    titulo <- "Distribucion de ocupados por rama de actividad"
  }

  ggplot2::ggplot(
    datos,
    ggplot2::aes(
      x    = stats::reorder(.data$descripcion, .data$porcentaje),
      y    = .data$porcentaje,
      fill = .data$porcentaje
    )
  ) +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::geom_text(
      ggplot2::aes(label = paste0(round(.data$porcentaje, 1), "%")),
      hjust = -0.1, size = 3.5
    ) +
    ggplot2::scale_fill_gradient(
      low = "#90CAF9", high = "#1565C0"
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, max(datos$porcentaje) * 1.2),
      labels = function(x) paste0(x, "%")
    ) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title   = titulo,
      x       = NULL,
      y       = "Porcentaje de ocupados (%)",
      caption = "Fuente: EPHC - INE Paraguay"
    ) +
    tema_paraguaydata()
}
