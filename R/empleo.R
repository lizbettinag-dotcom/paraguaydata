#' Obtener datos de ocupacion informal
#'
#' @return Un data.frame con datos de ocupacion informal
#' @export
#' @examples
#' \dontrun{
#' empleo <- get_ocupacion_informal()
#' }
get_ocupacion_informal <- function() {
  resultado <- consultar_api(
    "datastore_search",
    list(resource_id = "ocupacion-informal-ephc-2015-2020")
  )

  do.call(rbind, lapply(resultado$result$records, as.data.frame))
}

#' Obtener boletin trimestral de empleo
#'
#' @param anio Año del boletin (2017, 2018, 2019 o 2020)
#' @return Un data.frame con resultados del boletin de empleo
#' @export
#' @examples
#' \dontrun{
#' empleo <- get_boletin_empleo(2019)
#' }
get_boletin_empleo <- function(anio = 2019) {
  anios_validos <- c(2017, 2018, 2019, 2020)
  if (!anio %in% anios_validos) {
    stop("El anio debe ser uno de: ", paste(anios_validos, collapse = ", "))
  }

  resource_id <- paste0(
    "principales-resultados-de-la-ephc-boletín-trimestral-de-empleo-año-",
    anio
  )

  resultado <- consultar_api(
    "datastore_search",
    list(resource_id = resource_id)
  )

  do.call(rbind, lapply(resultado$result$records, as.data.frame))
}
