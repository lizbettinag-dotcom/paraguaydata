# =============================================================================
# Funciones de actualizacion automatica de datos del INE Paraguay
# Detecta automaticamente los ultimos trimestres disponibles
# sin necesidad de conocer las URLs exactas
# =============================================================================

#' Listar trimestres disponibles de la EPHC
#'
#' @description
#' Consulta el catalogo del INE Paraguay y devuelve todos los
#' trimestres de la EPHC disponibles para descarga. Permite
#' verificar que datos nuevos estan disponibles sin necesidad
#' de visitar el sitio web del INE manualmente.
#'
#' @return Un tibble con columnas: anio, trimestre, periodo,
#'   url, ordenado del mas reciente al mas antiguo.
#'
#' @export
#' @examples
#' \dontrun{
#' ine_trimestres_disponibles()
#' }
ine_trimestres_disponibles <- function() {

  catalogo <- ine_catalogo()

  # Filtrar solo CSVs de EPHC trimestral
  ephc <- catalogo |>
    dplyr::filter(
      .data$formato == "CSV",
      grepl("REG02_EPHC", .data$dataset, ignore.case = TRUE)
    )

  # Extraer anio y trimestre desde la URL
  ephc <- ephc |>
    dplyr::mutate(
      anio = as.integer(
        stringr::str_extract(.data$url, "EPHC-(\\d{4})", group = 1)
      ),
      trimestre_texto = dplyr::case_when(
        grepl("Primer|1er", .data$url,
              ignore.case = TRUE) ~ "T1",
        grepl("Segundo", .data$url,
              ignore.case = TRUE) ~ "T2",
        grepl("Tercer|3er", .data$url,
              ignore.case = TRUE) ~ "T3",
        grepl("Cuarto|4to|4To", .data$url,
              ignore.case = TRUE) ~ "T4",
        TRUE ~ NA_character_
      ),
      trimestre = as.integer(
        stringr::str_extract(.data$trimestre_texto, "\\d")
      ),
      periodo = paste0(.data$anio, "-", .data$trimestre_texto)
    ) |>
    dplyr::filter(
      !is.na(.data$anio),
      !is.na(.data$trimestre)
    ) |>
    dplyr::select(
      "anio", "trimestre", "periodo", "url"
    ) |>
    dplyr::arrange(
      dplyr::desc(.data$anio),
      dplyr::desc(.data$trimestre)
    )

  ephc
}


#' Obtener el ultimo trimestre disponible de la EPHC
#'
#' @description
#' Descarga automaticamente el trimestre mas reciente de la
#' EPHC disponible en el catalogo del INE Paraguay. Es la
#' forma mas sencilla de trabajar siempre con datos actualizados
#' sin necesidad de conocer el numero de trimestre o anio.
#'
#' @param verbose Logico. Si TRUE muestra informacion sobre
#'   el trimestre descargado. Por defecto TRUE.
#'
#' @return Un data.frame con los datos del ultimo trimestre
#'   disponible de la EPHC.
#'
#' @export
#' @examples
#' \dontrun{
#' # Siempre descarga los datos mas recientes disponibles
#' ephc <- ine_ultimo_trimestre()
#' }
ine_ultimo_trimestre <- function(verbose = TRUE) {

  disponibles <- ine_trimestres_disponibles()

  if (nrow(disponibles) == 0) {
    stop("No se encontraron trimestres disponibles en el catalogo del INE.")
  }

  ultimo <- disponibles[1, ]

  if (verbose) {
    message("Ultimo trimestre disponible: ", ultimo$periodo)
    message("Descargando datos...")
  }

  datos <- ine_ephc(
    anio      = ultimo$anio,
    trimestre = ultimo$trimestre
  )

  if (verbose) {
    message("Datos cargados: ", nrow(datos), " observaciones")
  }

  datos
}


#' Verificar si hay datos nuevos disponibles
#'
#' @description
#' Compara el trimestre mas reciente disponible en el catalogo
#' del INE con el ultimo trimestre descargado. Util para
#' automatizar la actualizacion de analisis cuando el INE
#' publica nuevos datos trimestrales.
#'
#' @param anio_actual Anio del ultimo trimestre que tenemos.
#' @param trimestre_actual Trimestre del ultimo dato que tenemos.
#'
#' @return Logico. TRUE si hay datos mas recientes disponibles,
#'   FALSE si ya tenemos los datos mas recientes.
#'
#' @export
#' @examples
#' \dontrun{
#' # Verificar si hay datos mas nuevos que los de 2024 T1
#' ine_hay_actualizacion(anio_actual = 2024, trimestre_actual = 1)
#' }
ine_hay_actualizacion <- function(anio_actual, trimestre_actual) {

  disponibles <- ine_trimestres_disponibles()

  if (nrow(disponibles) == 0) return(FALSE)

  ultimo <- disponibles[1, ]

  hay_nuevo <- (ultimo$anio > anio_actual) ||
    (ultimo$anio == anio_actual &&
       ultimo$trimestre > trimestre_actual)

  if (hay_nuevo) {
    message("Hay datos nuevos disponibles: ", ultimo$periodo,
            " (actualmente tienes: ",
            anio_actual, "-T", trimestre_actual, ")")
  } else {
    message("Ya tienes los datos mas recientes: ",
            anio_actual, "-T", trimestre_actual)
  }

  invisible(hay_nuevo)
}


#' Descargar todos los trimestres disponibles de un anio
#'
#' @description
#' Descarga y combina todos los trimestres disponibles de un
#' anio especifico de la EPHC. Alternativa mas flexible que
#' ine_serie() cuando se quiere trabajar con un anio completo
#' de datos sin especificar trimestres manualmente.
#'
#' @param anio Anio a descargar. Por defecto el anio mas reciente
#'   disponible en el catalogo.
#' @param verbose Logico. Si TRUE muestra progreso. Por defecto TRUE.
#'
#' @return Un data.frame con todos los trimestres del anio
#'   combinados, con columna PERIODO.
#'
#' @export
#' @examples
#' \dontrun{
#' # Descargar todos los trimestres disponibles de 2024
#' ephc_2024 <- ine_anio_completo(2024)
#'
#' # Descargar el anio mas reciente automaticamente
#' ephc_reciente <- ine_anio_completo()
#' }
ine_anio_completo <- function(anio = NULL, verbose = TRUE) {

  disponibles <- ine_trimestres_disponibles()

  if (nrow(disponibles) == 0) {
    stop("No se encontraron datos en el catalogo del INE.")
  }

  # Si no se especifica anio usar el mas reciente
  if (is.null(anio)) {
    anio <- max(disponibles$anio)
    if (verbose) message("Usando anio mas reciente: ", anio)
  }

  trimestres_anio <- disponibles |>
    dplyr::filter(.data$anio == !!anio) |>
    dplyr::arrange(.data$trimestre)

  if (nrow(trimestres_anio) == 0) {
    stop("No se encontraron datos para el anio ", anio,
         ". Anios disponibles: ",
         paste(sort(unique(disponibles$anio)), collapse = ", "))
  }

  if (verbose) {
    message("Descargando ", nrow(trimestres_anio),
            " trimestres de ", anio, "...")
  }

  lista <- vector("list", nrow(trimestres_anio))

  for (i in seq_len(nrow(trimestres_anio))) {
    t_i <- trimestres_anio$trimestre[i]
    if (verbose) message("  -> Trimestre ", t_i)

    datos_i <- tryCatch(
      ine_ephc(anio = anio, trimestre = t_i),
      error = function(e) {
        message("  Error en T", t_i, ": ", e$message)
        NULL
      }
    )

    if (!is.null(datos_i)) {
      datos_i$PERIODO <- trimestres_anio$periodo[i]
      lista[[i]] <- datos_i
    }
  }

  # Eliminar trimestres que fallaron (type-safe con vapply)
  lista <- lista[!vapply(lista, is.null, logical(1))]

  if (length(lista) == 0) {
    stop("No se pudo descargar ningun trimestre de ", anio)
  }

  resultado <- dplyr::bind_rows(lista)

  if (verbose) {
    message("Combinados ", length(lista), " trimestres: ",
            nrow(resultado), " observaciones totales")
  }

  resultado
}
