#' Listar microdatos disponibles en el INE
#'
#' @return Un data.frame con todos los archivos disponibles
#' @export
#' @examples
#' \dontrun{
#' catalogo <- ine_catalogo()
#' }
ine_catalogo <- function() {

  pagina <- httr2::request("https://www.ine.gov.py/microdatos/") |>
    httr2::req_perform()

  enlaces <- pagina |>
    httr2::resp_body_string() |>
    rvest::read_html() |>
    rvest::html_nodes("a") |>
    rvest::html_attr("href")

  # Filtrar solo archivos descargables
  enlaces_datos <- enlaces[grepl(
    "\\.(zip|csv|xlsx|sav|dta)",
    enlaces,
    ignore.case = TRUE
  )]

  # Construir tabla con metadatos
  data.frame(
    url      = enlaces_datos,
    formato  = toupper(tools::file_ext(enlaces_datos)),
    dataset  = basename(enlaces_datos),
    stringsAsFactors = FALSE
  )
}

#' Descargar microdatos de la EPHC trimestral del INE
#'
#' @param anio Año de la encuesta (2017 a 2025)
#' @param trimestre Numero de trimestre (1, 2, 3 o 4)
#' @return Un data.frame con los microdatos de la EPHC
#' @export
#' @examples
#' \dontrun{
#' ephc <- ine_ephc(anio = 2024, trimestre = 1)
#' }
ine_ephc <- function(anio = 2024, trimestre = 1) {

  anios_validos      <- 2017:2025
  trimestres_validos <- 1:4

  if (!anio %in% anios_validos) {
    stop("El anio debe estar entre 2017 y 2025")
  }

  if (!trimestre %in% trimestres_validos) {
    stop("El trimestre debe ser 1, 2, 3 o 4")
  }

  # Obtener catalogo y filtrar CSV de EPHC trimestral
  catalogo <- ine_catalogo()

  archivos <- catalogo[
    grepl(paste0("EPHC-", anio), catalogo$url, ignore.case = TRUE) &
      grepl("\\.csv$", catalogo$url, ignore.case = TRUE) &
      grepl("REG02", catalogo$url, ignore.case = TRUE),
  ]

  # Filtrar por trimestre
  patrones_trim <- c("1er", "2", "3er", "4")
  archivos <- archivos[
    grepl(patrones_trim[trimestre], archivos$url, ignore.case = TRUE),
  ]

  if (nrow(archivos) == 0) {
    stop("No se encontraron datos para el anio ", anio,
         " trimestre ", trimestre)
  }

  # Codificar espacios en la URL
  url_csv <- utils::URLencode(archivos$url[1])
  message("Descargando: ", basename(archivos$url[1]))

  datos <- readr::read_csv2(
    url_csv,
    locale = readr::locale(encoding = "latin1"),
    show_col_types = FALSE
  )

  # Corregir nombre de columna con encoding incorrecto
  names(datos) <- gsub("A\u00c3\u00b1oest", "Añoest", names(datos))

  datos
}

#' Descargar indice de pobreza multidimensional del INE
#'
#' @param anio Año del IPM (2016 a 2024)
#' @return Un data.frame con los datos del IPM
#' @export
#' @examples
#' \dontrun{
#' ipm <- ine_ipm(anio = 2024)
#' }
ine_ipm <- function(anio = 2024) {

  anios_validos <- 2016:2024

  if (!anio %in% anios_validos) {
    stop("El anio debe estar entre 2016 y 2024")
  }

  catalogo <- ine_catalogo()

  archivos <- catalogo[
    grepl(paste0("MPI", anio), catalogo$url, ignore.case = TRUE) &
      grepl("\\.csv$", catalogo$url, ignore.case = TRUE),
  ]

  if (nrow(archivos) == 0) {
    stop("No se encontraron datos del IPM para el anio ", anio)
  }

  # Codificar espacios en la URL
  url_csv <- utils::URLencode(archivos$url[1])
  message("Descargando: ", basename(archivos$url[1]))

  datos <- readr::read_csv2(
    url_csv,
    locale = readr::locale(encoding = "latin1"),
    show_col_types = FALSE
  )

  # Corregir nombres de columnas con encoding incorrecto
  names(datos) <- gsub("A\u00c3\u00b1oest", "Añoest", names(datos))

  datos
}

#' Descargar codigos geograficos del INE
#'
#' @param nivel Nivel geografico: "departamentos", "distritos" o "barrios"
#' @return Un data.frame con los codigos geograficos
#' @export
#' @examples
#' \dontrun{
#' deptos <- ine_geo("departamentos")
#' }
ine_geo <- function(nivel = "departamentos") {

  niveles_validos <- c("departamentos", "distritos", "barrios")

  if (!nivel %in% niveles_validos) {
    stop("El nivel debe ser uno de: ",
         paste(niveles_validos, collapse = ", "))
  }

  urls <- list(
    departamentos = "https://www.ine.gov.py/microdatos/register/localidades/Departamentos_Paraguay_Codigos_DGEEC.csv",
    distritos     = "https://www.ine.gov.py/microdatos/register/localidades/Distritos_Paraguay_Codigos_DGEEC.csv",
    barrios       = "https://www.ine.gov.py/microdatos/register/localidades/Barrios_Localidades_Paraguay_Codigos_DGEEC.csv"
  )

  message("Descargando codigos de ", nivel, "...")

  # Leer líneas crudas
  lineas <- readLines(urls[[nivel]], encoding = "latin1")

  # Limpiar comillas externas y duplicadas
  lineas_limpias <- gsub('^"|"$', '', lineas)
  lineas_limpias <- gsub('""', '"', lineas_limpias)

  # Parsear como CSV desde texto
  datos <- readr::read_csv(
    I(paste(lineas_limpias, collapse = "\n")),
    show_col_types = FALSE,
    locale = readr::locale(encoding = "UTF-8")
  )

  # Limpiar caracteres de control invisibles en columnas de texto
  datos <- dplyr::mutate(
    datos,
    dplyr::across(
      dplyr::where(is.character),
      ~ gsub("[[:cntrl:]]", "", .x)
    )
  )

  datos
}
