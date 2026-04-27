#' URL base de la API de datos abiertos de Paraguay
#'
#' @noRd
base_url <- function() {
  "https://www.datos.gov.py/api/3/action"
}

#' Realizar una consulta a la API
#'
#' @param endpoint Nombre del endpoint de la API
#' @param params Lista de parametros opcionales
#' @return Lista con la respuesta de la API
#' @noRd
consultar_api <- function(endpoint, params = list()) {
  url <- httr2::request(base_url()) |>
    httr2::req_url_path_append(endpoint) |>
    httr2::req_url_query(!!!params) |>
    httr2::req_perform()

  httr2::resp_body_json(url)
}

#' Obtener el resource_id real de un dataset
#'
#' @param dataset_name Nombre del dataset en la API
#' @return El ID del primer recurso disponible
#' @noRd
obtener_resource_id <- function(dataset_name) {
  resultado <- consultar_api(
    "package_show",
    list(id = dataset_name)
  )

  if (!resultado$success) {
    stop("Dataset no encontrado: ", dataset_name)
  }

  recursos <- resultado$result[[1]]$resources

  if (length(recursos) == 0) {
    stop("El dataset no tiene recursos disponibles")
  }

  list(
    id     = recursos[[1]]$id,
    url    = recursos[[1]]$url,
    format = recursos[[1]]$format
  )
}

#' Descargar datos de un dataset
#'
#' @param dataset_name Nombre del dataset en la API
#' @return Un data.frame con los datos
#' @noRd
descargar_dataset <- function(dataset_name) {
  recurso <- obtener_resource_id(dataset_name)

  # Si el recurso es CSV descargamos directo
  if (toupper(recurso$format) == "CSV") {
    return(readr::read_csv(recurso$url, show_col_types = FALSE))
  }

  # Si tiene ID intentamos datastore_search
  resultado <- consultar_api(
    "datastore_search",
    list(resource_id = recurso$id, limit = 10000)
  )

  if (!is.null(resultado$success) && resultado$success) {
    registros <- resultado$result$records
    return(do.call(rbind, lapply(registros, as.data.frame)))
  }

  stop(
    "No se pudo descargar el dataset '", dataset_name,
    "'. URL del recurso: ", recurso$url
  )
}
