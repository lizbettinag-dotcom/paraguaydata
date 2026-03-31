# =============================================================================
# Funciones de desagregacion por departamento - Paraguay EPHC INE
# Basado en variable ESTGEO del diccionario oficial EPHC 2025
# Codigos: Asuncion=1, Concepcion=11/16, San Pedro=21/26, etc.
# Nota: La EPHC trimestral no cubre Boqueron ni Alto Paraguay (Chaco)
# =============================================================================

#' Agregar columna de departamento a datos EPHC
#'
#' @description
#' Deriva el nombre del departamento a partir de la variable ESTGEO
#' de la EPHC del INE de Paraguay. La variable ESTGEO codifica el
#' estrato geografico combinando departamento y area (urbano/rural).
#' Esta funcion es la base para todas las desagregaciones
#' departamentales. Nota: la EPHC trimestral no cubre los
#' departamentos de Boqueron y Alto Paraguay.
#'
#' @param data data.frame de ine_ephc(). Requiere variable ESTGEO.
#'
#' @return El mismo data.frame con una columna adicional
#'   "departamento" con el nombre del departamento.
#'
#' @export
#' @examples
#' \dontrun{
#' ephc <- ine_ephc(2024, 1)
#' ephc <- agregar_departamento(ephc)
#' table(ephc$departamento)
#' }
agregar_departamento <- function(data) {

  if (!"ESTGEO" %in% names(data)) {
    stop("Variable ESTGEO no encontrada.",
         " Esta variable es requerida para identificar departamentos.")
  }

  data |>
    dplyr::mutate(
      departamento = dplyr::case_when(
        .data$ESTGEO == 1              ~ "Asuncion",
        .data$ESTGEO %in% c(11, 16)   ~ "Concepcion",
        .data$ESTGEO %in% c(21, 26)   ~ "San Pedro",
        .data$ESTGEO %in% c(31, 36)   ~ "Cordillera",
        .data$ESTGEO %in% c(41, 46)   ~ "Guaira",
        .data$ESTGEO %in% c(51, 56)   ~ "Caaguazu",
        .data$ESTGEO %in% c(61, 66)   ~ "Caazapa",
        .data$ESTGEO %in% c(71, 76)   ~ "Itapua",
        .data$ESTGEO %in% c(81, 86)   ~ "Misiones",
        .data$ESTGEO %in% c(91, 96)   ~ "Paraguari",
        .data$ESTGEO %in% c(101, 106) ~ "Alto Parana",
        .data$ESTGEO %in% c(111, 116) ~ "Central",
        .data$ESTGEO %in% c(121, 126) ~ "Neembucu",
        .data$ESTGEO %in% c(131, 136) ~ "Amambay",
        .data$ESTGEO %in% c(141, 146) ~ "Canindeyu",
        .data$ESTGEO %in% c(151, 156) ~ "Presidente Hayes",
        TRUE                           ~ NA_character_
      )
    )
}


#' Indicadores educativos por departamento
#'
#' @description
#' Calcula indicadores educativos desagregados por departamento.
#' Permite comparar el nivel educativo entre los 16 departamentos
#' cubiertos por la EPHC trimestral del INE de Paraguay. Es un
#' insumo clave para investigacion sobre desigualdades territoriales
#' en educacion (PNUD, 2019).
#'
#' @param data data.frame de ine_ephc(). Requiere ESTGEO y variables
#'   educativas segun indicador seleccionado.
#' @param indicador Indicador a calcular. Valores posibles:
#'   "escolarizacion", "anos_estudio" o "rezago".
#'   Por defecto "escolarizacion".
#' @param ponderado Logico. Si TRUE usa Factor de expansion.
#'   Por defecto TRUE.
#'
#' @return Un tibble con columnas: departamento, valor,
#'   ordenado de mayor a menor.
#'
#' @export
#' @examples
#' \dontrun{
#' ephc <- ine_ephc(2024, 1)
#' educacion_por_departamento(ephc)
#' educacion_por_departamento(ephc, indicador = "anos_estudio")
#' educacion_por_departamento(ephc, indicador = "rezago")
#' }
educacion_por_departamento <- function(data,
                                       indicador = "escolarizacion",
                                       ponderado = TRUE) {

  if (!indicador %in% c("escolarizacion", "anos_estudio", "rezago")) {
    stop("indicador debe ser 'escolarizacion', 'anos_estudio' o 'rezago'")
  }

  # Agregar columna de departamento
  datos <- agregar_departamento(data) |>
    dplyr::filter(!is.na(.data$departamento))

  departamentos <- sort(unique(datos$departamento))
  lista <- vector("list", length(departamentos))

  for (i in seq_along(departamentos)) {
    datos_dpto <- datos[datos$departamento == departamentos[i], ]

    valor <- tryCatch({
      if (indicador == "escolarizacion") {
        res <- tasa_escolarizacion(datos_dpto,
                                   ponderado = ponderado)
        if (nrow(res) == 0) NA_real_ else res$tasa_escolarizacion

      } else if (indicador == "anos_estudio") {
        res <- anios_promedio_estudio(datos_dpto,
                                      ponderado = ponderado)
        if (nrow(res) == 0) NA_real_ else res$anos_promedio_estudio

      } else {
        res <- rezago_educativo(datos_dpto,
                                ponderado = ponderado)
        if (nrow(res) == 0) NA_real_ else res$tasa_rezago
      }
    }, error = function(e) NA_real_)

    if (length(valor) == 0) valor <- NA_real_

    lista[[i]] <- data.frame(
      departamento = departamentos[i],
      valor        = valor
    )
  }

  resultado <- dplyr::bind_rows(lista) |>
    dplyr::filter(!is.na(.data$valor)) |>
    dplyr::arrange(dplyr::desc(.data$valor))

  nombres_indicador <- c(
    "escolarizacion" = "tasa_escolarizacion",
    "anos_estudio"   = "anos_promedio_estudio",
    "rezago"         = "tasa_rezago"
  )

  names(resultado)[2] <- nombres_indicador[indicador]
  resultado
}


#' Indicadores de empleo por departamento
#'
#' @description
#' Calcula indicadores del mercado laboral desagregados por
#' departamento. Permite analizar las heterogeneidades territoriales
#' en empleo, informalidad y subempleo en Paraguay.
#'
#' @param data data.frame de ine_ephc(). Requiere ESTGEO y variables
#'   de empleo segun indicador seleccionado.
#' @param indicador Indicador a calcular. Valores posibles:
#'   "desempleo", "informalidad" o "subempleo".
#'   Por defecto "informalidad".
#' @param ponderado Logico. Si TRUE usa Factor de expansion.
#'   Por defecto TRUE.
#'
#' @return Un tibble con columnas: departamento, valor,
#'   ordenado de mayor a menor.
#'
#' @export
#' @examples
#' \dontrun{
#' ephc <- ine_ephc(2024, 1)
#' empleo_por_departamento(ephc)
#' empleo_por_departamento(ephc, indicador = "desempleo")
#' empleo_por_departamento(ephc, indicador = "subempleo")
#' }
empleo_por_departamento <- function(data,
                                    indicador = "informalidad",
                                    ponderado = TRUE) {

  if (!indicador %in% c("desempleo", "informalidad", "subempleo")) {
    stop("indicador debe ser 'desempleo', 'informalidad' o 'subempleo'")
  }

  datos <- agregar_departamento(data) |>
    dplyr::filter(!is.na(.data$departamento))

  departamentos <- sort(unique(datos$departamento))
  lista <- vector("list", length(departamentos))

  for (i in seq_along(departamentos)) {
    datos_dpto <- datos[datos$departamento == departamentos[i], ]

    valor <- tryCatch({
      if (indicador == "desempleo") {
        res <- tasa_desempleo(datos_dpto, ponderado = ponderado)
        res$tasa_desempleo

      } else if (indicador == "informalidad") {
        res <- tasa_informalidad(datos_dpto, ponderado = ponderado)
        res$tasa_informalidad

      } else {
        res <- tasa_subempleo(datos_dpto, ponderado = ponderado)
        res$tasa_subempleo
      }
    }, error = function(e) NA_real_)

    lista[[i]] <- data.frame(
      departamento = departamentos[i],
      valor        = valor
    )
  }

  resultado <- dplyr::bind_rows(lista) |>
    dplyr::filter(!is.na(.data$valor)) |>
    dplyr::arrange(dplyr::desc(.data$valor))

  nombres_indicador <- c(
    "desempleo"    = "tasa_desempleo",
    "informalidad" = "tasa_informalidad",
    "subempleo"    = "tasa_subempleo"
  )

  names(resultado)[2] <- nombres_indicador[indicador]
  resultado
}


#' Grafico de indicador por departamento
#'
#' @description
#' Genera un grafico de barras horizontales con un indicador
#' educativo o de empleo desagregado por departamento. Las barras
#' horizontales facilitan la lectura de nombres de departamentos
#' en publicaciones academicas. Los departamentos se ordenan
#' de mayor a menor valor.
#'
#' @param data data.frame de ine_ephc().
#' @param tipo Tipo de indicador: "educacion" o "empleo".
#'   Por defecto "educacion".
#' @param indicador Indicador especifico segun tipo seleccionado.
#'   Para educacion: "escolarizacion", "anos_estudio", "rezago".
#'   Para empleo: "desempleo", "informalidad", "subempleo".
#'   Por defecto "escolarizacion".
#' @param titulo Titulo del grafico. Opcional.
#'
#' @return Un objeto ggplot2.
#'
#' @export
#' @examples
#' \dontrun{
#' ephc <- ine_ephc(2024, 1)
#' grafico_departamentos(ephc)
#' grafico_departamentos(ephc, indicador = "anos_estudio")
#' grafico_departamentos(ephc, tipo = "empleo",
#'                       indicador = "informalidad")
#' }
grafico_departamentos <- function(data,
                                  tipo      = "educacion",
                                  indicador = "escolarizacion",
                                  titulo    = NULL) {

  if (!tipo %in% c("educacion", "empleo")) {
    stop("tipo debe ser 'educacion' o 'empleo'")
  }

  # Obtener datos segun tipo
  if (tipo == "educacion") {
    datos <- educacion_por_departamento(data,
                                        indicador = indicador)
    col_valor <- names(datos)[2]
    color_barra <- "#2196F3"

    etiqueta_x <- dplyr::case_when(
      indicador == "escolarizacion" ~ "Tasa de escolarizacion (%)",
      indicador == "anos_estudio"   ~ "Anos promedio de estudio",
      indicador == "rezago"         ~ "Tasa de rezago (%)",
      TRUE ~ "Valor"
    )

    if (is.null(titulo)) {
      titulo <- dplyr::case_when(
        indicador == "escolarizacion" ~
          "Tasa de escolarizacion por departamento",
        indicador == "anos_estudio"   ~
          "Anos promedio de estudio por departamento",
        indicador == "rezago"         ~
          "Tasa de rezago educativo por departamento",
        TRUE ~ "Indicador educativo por departamento"
      )
    }

  } else {
    datos <- empleo_por_departamento(data,
                                     indicador = indicador)
    col_valor <- names(datos)[2]
    color_barra <- "#FF9800"

    etiqueta_x <- dplyr::case_when(
      indicador == "desempleo"    ~ "Tasa de desempleo (%)",
      indicador == "informalidad" ~ "Tasa de informalidad (%)",
      indicador == "subempleo"    ~ "Tasa de subempleo (%)",
      TRUE ~ "Valor"
    )

    if (is.null(titulo)) {
      titulo <- dplyr::case_when(
        indicador == "desempleo"    ~
          "Tasa de desempleo por departamento",
        indicador == "informalidad" ~
          "Tasa de informalidad laboral por departamento",
        indicador == "subempleo"    ~
          "Tasa de subempleo por departamento",
        TRUE ~ "Indicador de empleo por departamento"
      )
    }
  }

  # Renombrar columna de valor para ggplot
  datos$valor_plot <- datos[[col_valor]]

  ggplot2::ggplot(
    datos,
    ggplot2::aes(
      x    = stats::reorder(.data$departamento, .data$valor_plot),
      y    = .data$valor_plot,
      fill = .data$valor_plot
    )
  ) +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::geom_text(
      ggplot2::aes(label = round(.data$valor_plot, 1)),
      hjust = -0.1, size = 3.2
    ) +
    ggplot2::scale_fill_gradient(
      low  = if (tipo == "educacion") "#BBDEFB" else "#FFE0B2",
      high = if (tipo == "educacion") "#1565C0" else "#E65100"
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, max(datos$valor_plot) * 1.2),
      labels = function(x) paste0(x, "%")
    ) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title   = titulo,
      x       = NULL,
      y       = etiqueta_x,
      caption = "Fuente: EPHC - INE Paraguay"
    ) +
    tema_paraguaydata()
}
