#' Obtener matricula escolar por departamento y distrito
#'
#' @return Un data.frame con los datos de matricula
#' @export
#' @examples
#' \dontrun{
#' matricula <- get_matricula()
#' }
get_matricula <- function() {
  descargar_dataset("matricula-por-departamento-y-distrito")
}

#' Obtener listado de establecimientos escolares
#'
#' @return Un data.frame con los establecimientos escolares
#' @export
#' @examples
#' \dontrun{
#' escuelas <- get_establecimientos()
#' }
get_establecimientos <- function() {
  descargar_dataset("establecimientos-escolares")
}
