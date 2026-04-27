# =============================================================================
# Funciones de analisis de empleo - Paraguay EPHC INE
# Codigos: PEAD (1=ocupado, 2=desocupado, 3=inactivo, 4=desoc.oculto)
#          Informalidad (1=informal, 2=formal)
#          HORAB (horas trabajadas), CATE_PEA (categoria ocupacional)
#          RAMA_PEA (rama actividad), Factor (ponderador)
# =============================================================================

#' Tasa de desempleo
#'
#' @description
#' Calcula la tasa de desempleo abierto de la poblacion economicamente
#' activa (PEA). Se define como el cociente entre la poblacion
#' desocupada abierta y la PEA total (ocupados + desocupados abiertos).
#' Usa la variable PEAD de la EPHC donde 1 = ocupado y
#' 2 = desocupado abierto (OIT, 2013).
#'
#' @param data data.frame de ine_ephc(). Requiere PEAD y opcionalmente
#'   Factor, P06, AREA.
#' @param grupo Desagregacion opcional: "ninguno", "sexo" o "area".
#'   Por defecto "ninguno".
#' @param ponderado Logico. Si TRUE usa Factor de expansion.
#'   Por defecto TRUE.
#'
#' @return Un tibble con columnas: grupo, n_desocupados, n_pea,
#'   tasa_desempleo (en porcentaje).
#'
#' @export
#' @examples
#' \donttest{
#' ephc <- ine_ephc(2024, 1)
#' tasa_desempleo(ephc)
#' tasa_desempleo(ephc, grupo = "sexo")
#' tasa_desempleo(ephc, grupo = "area")
#' }
tasa_desempleo <- function(data,
                           grupo     = "ninguno",
                           ponderado = TRUE) {

  if (!"PEAD" %in% names(data)) {
    stop("Variable PEAD no encontrada en los datos")
  }

  if (!grupo %in% c("ninguno", "sexo", "area")) {
    stop("grupo debe ser 'ninguno', 'sexo' o 'area'")
  }

  # PEAD: 1 = ocupado, 2 = desocupado abierto
  datos <- data |>
    dplyr::filter(
      !is.na(.data$PEAD),
      .data$PEAD %in% c(1, 2)
    ) |>
    dplyr::mutate(
      desocupado = dplyr::if_else(.data$PEAD == 2, 1L, 0L),
      peso       = if (ponderado && "Factor" %in% names(data))
        .data$Factor else 1
    )

  # Variable de agrupacion
  if (grupo == "sexo") {
    if (!"P06" %in% names(data)) stop("Variable P06 (sexo) no encontrada")
    datos <- dplyr::mutate(
      datos,
      grupo = dplyr::case_when(
        .data$P06 == 1 ~ "Hombre",
        .data$P06 == 6 ~ "Mujer",
        TRUE           ~ NA_character_
      )
    )
  } else if (grupo == "area") {
    if (!"AREA" %in% names(data)) stop("Variable AREA no encontrada")
    datos <- dplyr::mutate(
      datos,
      grupo = dplyr::case_when(
        .data$AREA == 1 ~ "Urbana",
        .data$AREA == 6 ~ "Rural",
        TRUE            ~ NA_character_
      )
    )
  } else {
    datos <- dplyr::mutate(datos, grupo = "Total")
  }

  datos |>
    dplyr::filter(!is.na(.data$grupo)) |>
    dplyr::group_by(.data$grupo) |>
    dplyr::summarise(
      n_desocupados = sum(.data$peso[.data$desocupado == 1],
                          na.rm = TRUE),
      n_pea         = sum(.data$peso, na.rm = TRUE),
      tasa_desempleo = round(
        .data$n_desocupados / .data$n_pea * 100, 2
      ),
      .groups = "drop"
    )
}


#' Tasa de informalidad laboral
#'
#' @description
#' Calcula la tasa de informalidad laboral de la poblacion ocupada.
#' Se define como la proporcion de trabajadores ocupados en empleos
#' informales respecto al total de ocupados. Usa la variable
#' Informalidad de la EPHC donde 1 = informal y 2 = formal
#' (OIT, 2015; INE Paraguay).
#'
#' @param data data.frame de ine_ephc(). Requiere Informalidad y
#'   opcionalmente Factor, P06, AREA.
#' @param grupo Desagregacion opcional: "ninguno", "sexo" o "area".
#'   Por defecto "ninguno".
#' @param ponderado Logico. Si TRUE usa Factor de expansion.
#'   Por defecto TRUE.
#'
#' @return Un tibble con columnas: grupo, n_informales, n_ocupados,
#'   tasa_informalidad (en porcentaje).
#'
#' @export
#' @examples
#' \donttest{
#' ephc <- ine_ephc(2024, 1)
#' tasa_informalidad(ephc)
#' tasa_informalidad(ephc, grupo = "sexo")
#' tasa_informalidad(ephc, grupo = "area")
#' }
tasa_informalidad <- function(data,
                              grupo     = "ninguno",
                              ponderado = TRUE) {

  if (!"Informalidad" %in% names(data)) {
    stop("Variable Informalidad no encontrada en los datos")
  }

  if (!grupo %in% c("ninguno", "sexo", "area")) {
    stop("grupo debe ser 'ninguno', 'sexo' o 'area'")
  }

  # Informalidad: 1 = informal, 2 = formal
  datos <- data |>
    dplyr::filter(
      !is.na(.data$Informalidad),
      .data$Informalidad %in% c(1, 2)
    ) |>
    dplyr::mutate(
      informal = dplyr::if_else(.data$Informalidad == 1, 1L, 0L),
      peso     = if (ponderado && "Factor" %in% names(data))
        .data$Factor else 1
    )

  # Variable de agrupacion
  if (grupo == "sexo") {
    if (!"P06" %in% names(data)) stop("Variable P06 (sexo) no encontrada")
    datos <- dplyr::mutate(
      datos,
      grupo = dplyr::case_when(
        .data$P06 == 1 ~ "Hombre",
        .data$P06 == 6 ~ "Mujer",
        TRUE           ~ NA_character_
      )
    )
  } else if (grupo == "area") {
    if (!"AREA" %in% names(data)) stop("Variable AREA no encontrada")
    datos <- dplyr::mutate(
      datos,
      grupo = dplyr::case_when(
        .data$AREA == 1 ~ "Urbana",
        .data$AREA == 6 ~ "Rural",
        TRUE            ~ NA_character_
      )
    )
  } else {
    datos <- dplyr::mutate(datos, grupo = "Total")
  }

  datos |>
    dplyr::filter(!is.na(.data$grupo)) |>
    dplyr::group_by(.data$grupo) |>
    dplyr::summarise(
      n_informales     = sum(.data$peso[.data$informal == 1],
                             na.rm = TRUE),
      n_ocupados       = sum(.data$peso, na.rm = TRUE),
      tasa_informalidad = round(
        .data$n_informales / .data$n_ocupados * 100, 2
      ),
      .groups = "drop"
    )
}


#' Tasa de subempleo visible
#'
#' @description
#' Calcula la tasa de subempleo visible de la poblacion ocupada.
#' Se define como la proporcion de trabajadores ocupados que trabajan
#' menos de 30 horas semanales y desean trabajar mas horas
#' (OIT, 2013). Usa la variable HORAB (horas trabajadas) de la EPHC.
#'
#' @param data data.frame de ine_ephc(). Requiere HORAB, PEAD y
#'   opcionalmente Factor, P06, AREA.
#' @param umbral_horas Umbral de horas semanales para definir
#'   subempleo. Por defecto 30.
#' @param grupo Desagregacion opcional: "ninguno", "sexo" o "area".
#'   Por defecto "ninguno".
#' @param ponderado Logico. Si TRUE usa Factor de expansion.
#'   Por defecto TRUE.
#'
#' @return Un tibble con columnas: grupo, n_subempleados, n_ocupados,
#'   tasa_subempleo (en porcentaje).
#'
#' @export
#' @examples
#' \donttest{
#' ephc <- ine_ephc(2024, 1)
#' tasa_subempleo(ephc)
#' tasa_subempleo(ephc, umbral_horas = 20)
#' tasa_subempleo(ephc, grupo = "area")
#' }
tasa_subempleo <- function(data,
                           umbral_horas = 30,
                           grupo        = "ninguno",
                           ponderado    = TRUE) {

  vars_requeridas <- c("HORAB", "PEAD")
  vars_faltantes  <- vars_requeridas[!vars_requeridas %in% names(data)]
  if (length(vars_faltantes) > 0) {
    stop("Faltan variables requeridas: ",
         paste(vars_faltantes, collapse = ", "))
  }

  if (!grupo %in% c("ninguno", "sexo", "area")) {
    stop("grupo debe ser 'ninguno', 'sexo' o 'area'")
  }

  # Solo ocupados (PEAD == 1) con horas reportadas
  datos <- data |>
    dplyr::filter(
      !is.na(.data$HORAB),
      !is.na(.data$PEAD),
      .data$PEAD == 1
    ) |>
    dplyr::mutate(
      subempleado = dplyr::if_else(
        .data$HORAB < umbral_horas, 1L, 0L
      ),
      peso = if (ponderado && "Factor" %in% names(data))
        .data$Factor else 1
    )

  # Variable de agrupacion
  if (grupo == "sexo") {
    if (!"P06" %in% names(data)) stop("Variable P06 (sexo) no encontrada")
    datos <- dplyr::mutate(
      datos,
      grupo = dplyr::case_when(
        .data$P06 == 1 ~ "Hombre",
        .data$P06 == 6 ~ "Mujer",
        TRUE           ~ NA_character_
      )
    )
  } else if (grupo == "area") {
    if (!"AREA" %in% names(data)) stop("Variable AREA no encontrada")
    datos <- dplyr::mutate(
      datos,
      grupo = dplyr::case_when(
        .data$AREA == 1 ~ "Urbana",
        .data$AREA == 6 ~ "Rural",
        TRUE            ~ NA_character_
      )
    )
  } else {
    datos <- dplyr::mutate(datos, grupo = "Total")
  }

  datos |>
    dplyr::filter(!is.na(.data$grupo)) |>
    dplyr::group_by(.data$grupo) |>
    dplyr::summarise(
      n_subempleados = sum(.data$peso[.data$subempleado == 1],
                           na.rm = TRUE),
      n_ocupados     = sum(.data$peso, na.rm = TRUE),
      tasa_subempleo = round(
        .data$n_subempleados / .data$n_ocupados * 100, 2
      ),
      .groups = "drop"
    )
}


#' Distribucion por rama de actividad economica
#'
#' @description
#' Calcula la distribucion de la poblacion ocupada segun rama de
#' actividad economica. Las ramas corresponden a la Clasificacion
#' Industrial Internacional Uniforme (CIIU) adaptada para la EPHC
#' de Paraguay. Permite identificar la estructura productiva del
#' mercado laboral.
#'
#' @param data data.frame de ine_ephc(). Requiere RAMA_PEA y
#'   opcionalmente Factor.
#' @param ponderado Logico. Si TRUE usa Factor de expansion.
#'   Por defecto TRUE.
#'
#' @return Un tibble con columnas: rama, descripcion, n, porcentaje.
#'
#' @export
#' @examples
#' \donttest{
#' ephc <- ine_ephc(2024, 1)
#' distribucion_rama(ephc)
#' }
distribucion_rama <- function(data, ponderado = TRUE) {

  if (!"RAMA_PEA" %in% names(data)) {
    stop("Variable RAMA_PEA no encontrada en los datos")
  }

  # Etiquetas de rama de actividad EPHC Paraguay
  etiquetas_rama <- c(
    "1" = "Agricultura y ganaderia",
    "2" = "Industria manufacturera",
    "3" = "Construccion",
    "4" = "Comercio",
    "5" = "Servicios",
    "6" = "Administracion publica",
    "7" = "Ensenanza y salud",
    "8" = "Otras actividades",
    "99" = "No especificado"
  )

  data |>
    dplyr::filter(
      !is.na(.data$RAMA_PEA),
      .data$RAMA_PEA != 99
    ) |>
    dplyr::mutate(
      peso        = if (ponderado && "Factor" %in% names(data))
        .data$Factor else 1,
      descripcion = etiquetas_rama[as.character(.data$RAMA_PEA)]
    ) |>
    dplyr::group_by(.data$RAMA_PEA, .data$descripcion) |>
    dplyr::summarise(
      n = sum(.data$peso, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      porcentaje = round(.data$n / sum(.data$n) * 100, 2)
    ) |>
    dplyr::arrange(dplyr::desc(.data$n)) |>
    dplyr::rename(rama = "RAMA_PEA")
}
