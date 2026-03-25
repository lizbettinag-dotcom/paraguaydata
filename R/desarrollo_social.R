#' Obtener indice de pobreza multidimensional
#'
#' @return Un data.frame con el IPM por departamento
#' @export
#' @examples
#' \dontrun{
#' pobreza <- get_pobreza_multidimensional()
#' }
get_pobreza_multidimensional <- function() {
  resultado <- consultar_api(
    "datastore_search",
    list(resource_id = "índice-de-pobreza-multidimensional-ipm-paraguay")
  )

  do.call(rbind, lapply(resultado$result$records, as.data.frame))
}

#' Obtener condiciones de vida
#'
#' @return Un data.frame con indicadores de condiciones de vida
#' @export
#' @examples
#' \dontrun{
#' condiciones <- get_condiciones_vida()
#' }
get_condiciones_vida <- function() {
  resultado <- consultar_api(
    "datastore_search",
    list(resource_id = "condiciones-de-vida-ephc-2009-2018")
  )

  do.call(rbind, lapply(resultado$result$records, as.data.frame))
}
