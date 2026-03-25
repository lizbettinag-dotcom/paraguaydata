#' Listar microdatos disponibles en el INE
#'
#' Descarga el catalogo completo de microdatos disponibles en el portal
#' del Instituto Nacional de Estadistica (INE) de Paraguay.
#'
#' @return Un data.frame con tres columnas:
#' \describe{
#'   \item{url}{URL completa del archivo}
#'   \item{formato}{Formato del archivo (CSV, SAV, ZIP, etc.)}
#'   \item{dataset}{Nombre del archivo}
#' }
#' @export
#' @examples
#' \dontrun{
#' catalogo <- ine_catalogo()
#' csvs <- catalogo[catalogo$formato == "CSV", ]
#' ephc <- catalogo[grepl("EPHC", catalogo$dataset), ]
#' }
ine_catalogo <- function() {

  pagina <- httr2::request("https://www.ine.gov.py/microdatos/") |>
    httr2::req_perform()

  enlaces <- pagina |>
    httr2::resp_body_string() |>
    rvest::read_html() |>
    rvest::html_nodes("a") |>
    rvest::html_attr("href")

  enlaces_datos <- enlaces[grepl(
    "\\.(zip|csv|xlsx|sav|dta)",
    enlaces,
    ignore.case = TRUE
  )]

  data.frame(
    url     = enlaces_datos,
    formato = toupper(tools::file_ext(enlaces_datos)),
    dataset = basename(enlaces_datos),
    stringsAsFactors = FALSE
  )
}

#' Descargar microdatos de la EPHC trimestral del INE
#'
#' Descarga los microdatos de la Encuesta Permanente de Hogares Continua
#' (EPHC) del Instituto Nacional de Estadistica (INE) de Paraguay.
#' La EPHC contiene informacion sobre empleo, educacion y condiciones
#' de vida de los hogares paraguayos.
#'
#' @param anio Anio de la encuesta. Valores disponibles: 2017 a 2025.
#' @param trimestre Numero de trimestre. Valores: 1, 2, 3 o 4.
#'
#' @return Un data.frame con los microdatos de la EPHC. Las principales
#' variables incluyen:
#' \describe{
#'   \item{ANIO}{Anio de la encuesta}
#'   \item{AREA}{Area geografica (1 = urbana, 2 = rural)}
#'   \item{P02}{Edad de la persona}
#'   \item{P03}{Sexo (1 = masculino, 2 = femenino)}
#'   \item{A01}{Nivel educativo}
#'   \item{OCUP_PEA}{Ocupacion de la poblacion economicamente activa}
#'   \item{Informalidad}{Indicador de informalidad laboral}
#'   \item{Factor}{Factor de expansion}
#' }
#'
#' @export
#' @examples
#' \dontrun{
#' ephc_2024_t1 <- ine_ephc(anio = 2024, trimestre = 1)
#' dim(ephc_2024_t1)
#' table(ephc_2024_t1$AREA)
#' mean(ephc_2024_t1$P02, na.rm = TRUE)
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

  catalogo <- ine_catalogo()

  archivos <- catalogo[
    grepl(paste0("EPHC-", anio), catalogo$url, ignore.case = TRUE) &
      grepl("\\.csv$", catalogo$url, ignore.case = TRUE) &
      grepl("REG02", catalogo$url, ignore.case = TRUE),
  ]

  patrones_trim <- c("1er", "2", "3er", "4")
  archivos <- archivos[
    grepl(patrones_trim[trimestre], archivos$url, ignore.case = TRUE),
  ]

  if (nrow(archivos) == 0) {
    stop("No se encontraron datos para el anio ", anio,
         " trimestre ", trimestre)
  }

  url_csv <- utils::URLencode(archivos$url[1])
  message("Descargando: ", basename(archivos$url[1]))

  datos <- readr::read_csv2(
    url_csv,
    locale = readr::locale(encoding = "latin1"),
    show_col_types = FALSE
  )

  names(datos) <- gsub("A\u00c3\u00b1oest", "A\u00f1oest", names(datos))
  datos
}

#' Descargar indice de pobreza multidimensional del INE
#'
#' Descarga los microdatos del Indice de Pobreza Multidimensional (IPM)
#' del Instituto Nacional de Estadistica (INE) de Paraguay.
#' El IPM mide la pobreza considerando multiples dimensiones como
#' educacion, salud, vivienda y empleo.
#'
#' @param anio Anio del IPM. Valores disponibles: 2016 a 2024.
#'
#' @return Un data.frame con los indicadores del IPM por hogar.
#' Las principales variables incluyen:
#' \describe{
#'   \item{hhid}{Identificador unico del hogar}
#'   \item{dpto}{Codigo de departamento}
#'   \item{area}{Area geografica (1 = urbana, 2 = rural)}
#'   \item{pobrezai}{Indicador de pobreza}
#'   \item{hh_d_ni_noasis}{Privacion en asistencia escolar de ninos}
#'   \item{hh_d_materialidad}{Privacion en materialidad de vivienda}
#'   \item{hh_d_no_afil}{Privacion en afiliacion a seguro de salud}
#'   \item{fex_2022}{Factor de expansion}
#' }
#'
#' @export
#' @examples
#' \dontrun{
#' ipm_2024 <- ine_ipm(anio = 2024)
#' dim(ipm_2024)
#' table(ipm_2024$pobrezai)
#' mean(ipm_2024$hh_d_ni_noasis, na.rm = TRUE)
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

  url_csv <- utils::URLencode(archivos$url[1])
  message("Descargando: ", basename(archivos$url[1]))

  datos <- readr::read_csv2(
    url_csv,
    locale = readr::locale(encoding = "latin1"),
    show_col_types = FALSE
  )

  names(datos) <- gsub("A\u00c3\u00b1oest", "A\u00f1oest", names(datos))
  datos
}

#' Descargar codigos geograficos del INE
#'
#' Descarga los codigos geograficos oficiales del Instituto Nacional
#' de Estadistica (INE) de Paraguay, utilizados en todos los
#' microdatos del INE para identificar departamentos, distritos
#' y barrios.
#'
#' @param nivel Nivel geografico a descargar. Opciones:
#' \describe{
#'   \item{"departamentos"}{18 departamentos del Paraguay}
#'   \item{"distritos"}{Distritos con codigo concatenado}
#'   \item{"barrios"}{Barrios y localidades del Paraguay}
#' }
#'
#' @return Un data.frame con los codigos geograficos del nivel
#' seleccionado.
#'
#' @export
#' @examples
#' \dontrun{
#' deptos <- ine_geo("departamentos")
#' distritos <- ine_geo("distritos")
#' barrios <- ine_geo("barrios")
#' ephc <- ine_ephc(2024, 1)
#' ephc_geo <- merge(ephc, deptos,
#'                   by.x = "ESTGEO",
#'                   by.y = "codigo_dpto")
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

  lineas <- readLines(urls[[nivel]], encoding = "latin1")

  lineas_limpias <- gsub('^"|"$', '', lineas)
  lineas_limpias <- gsub('""', '"', lineas_limpias)

  datos <- readr::read_csv(
    I(paste(lineas_limpias, collapse = "\n")),
    show_col_types = FALSE,
    locale = readr::locale(encoding = "UTF-8")
  )

  datos <- dplyr::mutate(
    datos,
    dplyr::across(
      dplyr::where(is.character),
      ~ gsub("[[:cntrl:]]", "", .x)
    )
  )

  datos
}
