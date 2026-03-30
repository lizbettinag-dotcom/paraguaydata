#' Descargar serie temporal de la EPHC
#'
#' @description
#' Descarga y combina multiples trimestres de la Encuesta Permanente
#' de Hogares Continua (EPHC) del INE de Paraguay en un solo
#' data.frame. Permite construir series temporales para analisis
#' de tendencias en educacion, empleo y condiciones de vida.
#'
#' @param anio_inicio Anio de inicio de la serie. Por defecto 2022.
#' @param anio_fin Anio de fin de la serie. Por defecto 2024.
#' @param trimestres Vector de trimestres a incluir. Por defecto
#'   c(1, 2, 3, 4).
#'
#' @return Un data.frame combinado con todos los trimestres
#'   solicitados. Incluye columna PERIODO en formato "AAAA-TT"
#'   para identificar cada observacion.
#'
#' @export
#' @examples
#' \dontrun{
#' # Serie 2022-2024
#' serie <- ine_serie(anio_inicio = 2022, anio_fin = 2024)
#'
#' # Solo primeros trimestres
#' serie_t1 <- ine_serie(2022, 2024, trimestres = 1)
#' }
ine_serie <- function(anio_inicio = 2022,
                      anio_fin    = 2024,
                      trimestres  = c(1, 2, 3, 4)) {

  # Validaciones
  if (anio_inicio < 2017 || anio_fin > 2025) {
    stop("Los anios deben estar entre 2017 y 2025")
  }

  if (anio_inicio > anio_fin) {
    stop("anio_inicio debe ser menor o igual a anio_fin")
  }

  if (!all(trimestres %in% 1:4)) {
    stop("Los trimestres deben ser 1, 2, 3 o 4")
  }

  # Generar combinaciones anio-trimestre
  periodos <- expand.grid(
    anio      = anio_inicio:anio_fin,
    trimestre = trimestres
  )
  periodos <- periodos[order(periodos$anio, periodos$trimestre), ]

  message("Descargando ", nrow(periodos), " trimestres...")

  # Descargar cada trimestre
  lista <- vector("list", nrow(periodos))

  for (i in seq_len(nrow(periodos))) {
    anio_i <- periodos$anio[i]
    trim_i <- periodos$trimestre[i]

    message("  -> ", anio_i, " T", trim_i)

    tryCatch({
      datos <- ine_ephc(anio = anio_i, trimestre = trim_i)
      datos$PERIODO <- paste0(anio_i, "-T", trim_i)
      lista[[i]] <- datos
    }, error = function(e) {
      message("  !! Error en ", anio_i, " T", trim_i, ": ", e$message)
      lista[[i]] <<- NULL
    })
  }

  # Eliminar trimestres que fallaron
  lista <- lista[!sapply(lista, is.null)]

  if (length(lista) == 0) {
    stop("No se pudo descargar ningun trimestre")
  }

  message("Combinando ", length(lista), " trimestres...")
  dplyr::bind_rows(lista)
}


#' Calcular indicador educativo en serie temporal
#'
#' @description
#' Calcula un indicador educativo para cada periodo de una serie
#' temporal descargada con ine_serie(). Permite visualizar la
#' evolucion de indicadores educativos a lo largo del tiempo,
#' con desagregacion opcional por area o sexo.
#'
#' @param serie data.frame de ine_serie(). Debe contener columna
#'   PERIODO.
#' @param indicador Indicador a calcular. Valores posibles:
#'   "escolarizacion", "anos_estudio", "rezago". Por defecto
#'   "escolarizacion".
#' @param grupo Desagregacion opcional: "ninguno", "area" o "sexo".
#'   Por defecto "ninguno".
#' @param ponderado Logico. Si TRUE usa Factor de expansion.
#'   Por defecto TRUE.
#'
#' @return Un tibble con el indicador calculado por periodo.
#'   Columnas: PERIODO y el valor del indicador seleccionado.
#'
#' @export
#' @examples
#' \dontrun{
#' serie <- ine_serie(2022, 2024, trimestres = 1)
#'
#' # Evolucion de escolarizacion
#' tendencia_escolarizacion(serie)
#'
#' # Por area geografica
#' tendencia_escolarizacion(serie, grupo = "area")
#'
#' # Anos de estudio
#' tendencia_escolarizacion(serie, indicador = "anos_estudio")
#' }
tendencia_escolarizacion <- function(serie,
                                     indicador = "escolarizacion",
                                     grupo     = "ninguno",
                                     ponderado = TRUE) {

  if (!"PERIODO" %in% names(serie)) {
    stop("El data.frame debe tener columna PERIODO.",
         " Usa ine_serie() para descargar la serie temporal.")
  }

  if (!indicador %in% c("escolarizacion", "anos_estudio", "rezago")) {
    stop("indicador debe ser 'escolarizacion', 'anos_estudio' o 'rezago'")
  }

  if (!grupo %in% c("ninguno", "area", "sexo")) {
    stop("grupo debe ser 'ninguno', 'area' o 'sexo'")
  }

  periodos <- unique(serie$PERIODO)
  periodos <- sort(periodos)

  lista <- vector("list", length(periodos))

  for (i in seq_along(periodos)) {
    datos_periodo <- serie[serie$PERIODO == periodos[i], ]

    resultado <- tryCatch({

      if (indicador == "escolarizacion") {
        res <- tasa_escolarizacion(datos_periodo,
                                   grupo     = grupo,
                                   ponderado = ponderado)
        res$valor <- res$tasa_escolarizacion
        res <- res[, c(if (grupo != "ninguno") "grupo", "valor"),
                   drop = FALSE]

      } else if (indicador == "anos_estudio") {
        if (grupo == "ninguno") {
          res <- anios_promedio_estudio(datos_periodo,
                                        ponderado = ponderado)
          res$valor <- res$anos_promedio_estudio
          res <- res[, "valor", drop = FALSE]
        } else {
          res <- brecha_urbano_rural(datos_periodo,
                                     indicador = "anos_estudio",
                                     ponderado = ponderado)
          if (grupo == "sexo") {
            res <- brecha_genero(datos_periodo,
                                 indicador = "anos_estudio",
                                 ponderado = ponderado)
          }
          res <- res[, c("grupo", "valor"), drop = FALSE]
        }

      } else {
        res <- rezago_educativo(datos_periodo,
                                ponderado = ponderado)
        res$valor <- res$tasa_rezago
        res <- res[, "valor", drop = FALSE]
      }

      res$PERIODO <- periodos[i]
      res

    }, error = function(e) {
      message("Error en periodo ", periodos[i], ": ", e$message)
      NULL
    })

    lista[[i]] <- resultado
  }

  lista <- lista[!sapply(lista, is.null)]

  if (length(lista) == 0) {
    stop("No se pudo calcular el indicador para ningun periodo")
  }

  resultado_final <- dplyr::bind_rows(lista)

  # Ordenar columnas: PERIODO primero
  cols <- c("PERIODO",
            if ("grupo" %in% names(resultado_final)) "grupo",
            "valor")
  resultado_final[, cols]
}
