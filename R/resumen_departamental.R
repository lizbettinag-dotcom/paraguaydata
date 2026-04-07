# =============================================================================
# Funcion de resumen integrado por departamento
# Combina indicadores de educacion, empleo y pobreza de la EPHC
# Base para analisis multidimensional del desarrollo humano en Paraguay
# =============================================================================

#' Resumen integrado de indicadores por departamento
#'
#' @description
#' Genera una tabla resumen con los principales indicadores de
#' educacion, empleo y pobreza desagregados por departamento.
#' Integra datos de la EPHC del INE para producir un perfil
#' territorial del desarrollo humano en Paraguay. Esta funcion
#' es el insumo principal para analisis multidimensionales y
#' articulos sobre desigualdad territorial (Sen, 1999; PNUD, 2019).
#'
#' @param data data.frame de ine_ephc(). Requiere ESTGEO y
#'   variables de educacion y empleo.
#' @param ponderado Logico. Si TRUE usa Factor de expansion.
#'   Por defecto TRUE.
#'
#' @return Un tibble con una fila por departamento y columnas:
#'   departamento, tasa_escolarizacion, anos_promedio_estudio,
#'   tasa_rezago, tasa_desempleo, tasa_informalidad,
#'   tasa_subempleo. Ordenado por anos_promedio_estudio.
#'
#' @export
#' @examples
#' \dontrun{
#' ephc <- ine_ephc(2024, 1)
#' resumen <- resumen_departamental(ephc)
#' print(resumen)
#' }
resumen_departamental <- function(data, ponderado = TRUE) {

  if (!"ESTGEO" %in% names(data)) {
    stop("Variable ESTGEO no encontrada.",
         " Usa datos de ine_ephc() para esta funcion.")
  }

  # Agregar departamento
  datos <- agregar_departamento(data) |>
    dplyr::filter(!is.na(.data$departamento))

  departamentos <- sort(unique(datos$departamento))
  message("Calculando indicadores para ",
          length(departamentos), " departamentos...")

  lista <- vector("list", length(departamentos))

  for (i in seq_along(departamentos)) {
    dpto_i  <- departamentos[i]
    datos_i <- datos[datos$departamento == dpto_i, ]

    # Educacion
    escolarizacion <- tryCatch({
      tasa_escolarizacion(datos_i,
                          ponderado = ponderado)$tasa_escolarizacion
    }, error = function(e) NA_real_)

    anos_estudio <- tryCatch({
      anios_promedio_estudio(datos_i,
                             ponderado = ponderado)$anos_promedio_estudio
    }, error = function(e) NA_real_)

    rezago <- tryCatch({
      rezago_educativo(datos_i,
                       ponderado = ponderado)$tasa_rezago
    }, error = function(e) NA_real_)

    # Empleo
    desempleo <- tryCatch({
      tasa_desempleo(datos_i,
                     ponderado = ponderado)$tasa_desempleo
    }, error = function(e) NA_real_)

    informalidad <- tryCatch({
      tasa_informalidad(datos_i,
                        ponderado = ponderado)$tasa_informalidad
    }, error = function(e) NA_real_)

    subempleo <- tryCatch({
      tasa_subempleo(datos_i,
                     ponderado = ponderado)$tasa_subempleo
    }, error = function(e) NA_real_)

    # Corregir vectores vacios
    escolarizacion <- if (length(escolarizacion) == 0) NA_real_ else escolarizacion
    anos_estudio   <- if (length(anos_estudio) == 0)   NA_real_ else anos_estudio
    rezago         <- if (length(rezago) == 0)         NA_real_ else rezago
    desempleo      <- if (length(desempleo) == 0)      NA_real_ else desempleo
    informalidad   <- if (length(informalidad) == 0)   NA_real_ else informalidad
    subempleo      <- if (length(subempleo) == 0)      NA_real_ else subempleo

    lista[[i]] <- data.frame(
      departamento          = dpto_i,
      tasa_escolarizacion   = escolarizacion,
      anos_promedio_estudio = anos_estudio,
      tasa_rezago           = rezago,
      tasa_desempleo        = desempleo,
      tasa_informalidad     = informalidad,
      tasa_subempleo        = subempleo
    )
  }

  dplyr::bind_rows(lista) |>
    dplyr::arrange(dplyr::desc(.data$anos_promedio_estudio))
}


#' Grafico de resumen departamental
#'
#' @description
#' Genera un grafico de puntos (dot plot) comparando multiples
#' indicadores entre departamentos. Permite visualizar de forma
#' compacta el perfil de desarrollo humano territorial de Paraguay.
#' Disenado para publicaciones academicas con multiples dimensiones.
#'
#' @param data data.frame de ine_ephc() o resultado de
#'   resumen_departamental().
#' @param indicadores Vector de indicadores a graficar. Por defecto
#'   los cuatro principales.
#' @param titulo Titulo del grafico. Opcional.
#'
#' @return Un objeto ggplot2.
#'
#' @export
#' @examples
#' \dontrun{
#' ephc <- ine_ephc(2024, 1)
#' grafico_resumen_departamental(ephc)
#'
#' # Solo educacion
#' grafico_resumen_departamental(ephc,
#'   indicadores = c("tasa_escolarizacion",
#'                   "anos_promedio_estudio"))
#' }
grafico_resumen_departamental <- function(data,
                                          indicadores = c(
                                            "tasa_escolarizacion",
                                            "anos_promedio_estudio",
                                            "tasa_informalidad",
                                            "tasa_desempleo"
                                          ),
                                          titulo = NULL) {

  # Si recibe EPHC crudo, obtener anio y calcular resumen
  if ("ESTGEO" %in% names(data)) {
    anio_datos <- if ("ANIO" %in% names(data)) {
      paste0(" (", paste(unique(data$ANIO), collapse = "-"), ")")
    } else if ("TRIMESTRE" %in% names(data) && "ANIO" %in% names(data)) {
      paste0(" (", paste(unique(data$ANIO), collapse = "-"), ")")
    } else {
      # Buscar columna de anio con cualquier variante
      col_anio <- names(data)[grepl("^anio$|^ANIO$|^ano$|^ANO$",
                                    names(data),
                                    ignore.case = TRUE)][1]
      if (!is.na(col_anio)) {
        paste0(" (", paste(unique(data[[col_anio]]), collapse = "-"), ")")
      } else ""
    }
    datos <- resumen_departamental(data)
  } else {
    anio_datos <- ""
    datos <- data
  }

  indicadores <- indicadores[indicadores %in% names(datos)]

  if (length(indicadores) == 0) {
    stop("Ningun indicador valido encontrado en los datos.")
  }

  etiquetas <- c(
    "tasa_escolarizacion"   = "Escolarizacion (%)",
    "anos_promedio_estudio" = "Anos de estudio",
    "tasa_rezago"           = "Rezago (%)",
    "tasa_desempleo"        = "Desempleo (%)",
    "tasa_informalidad"     = "Informalidad (%)",
    "tasa_subempleo"        = "Subempleo (%)"
  )

  # Formato largo para facetas
  datos_long <- datos |>
    dplyr::select("departamento", dplyr::all_of(indicadores)) |>
    tidyr::pivot_longer(
      cols      = dplyr::all_of(indicadores),
      names_to  = "indicador",
      values_to = "valor"
    ) |>
    dplyr::mutate(
      indicador = etiquetas[.data$indicador]
    )

  if (is.null(titulo)) {
    titulo <- paste0(
      "Perfil de desarrollo humano por departamento",
      anio_datos
    )
  }

  ggplot2::ggplot(
    datos_long,
    ggplot2::aes(
      x     = .data$valor,
      y     = stats::reorder(.data$departamento, .data$valor),
      color = .data$indicador
    )
  ) +
    ggplot2::geom_point(size = 3, show.legend = FALSE) +
    ggplot2::facet_wrap(
      ~ .data$indicador,
      scales = "free_x",
      ncol   = 2
    ) +
    ggplot2::labs(
      title   = titulo,
      x       = NULL,
      y       = NULL,
      caption = "Fuente: EPHC - INE Paraguay"
    ) +
    tema_paraguaydata() +
    ggplot2::theme(
      strip.text = ggplot2::element_text(
        size = 10, face = "bold"
      )
    )
}
