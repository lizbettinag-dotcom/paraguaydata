# =============================================================================
# Funciones analiticas para investigacion en educacion y desarrollo - Paraguay
# Instituto Nacional de Estadistica (INE) - EPHC e IPM
# Codigos EPHC: A05 asistencia (1=si, 6=no), AREA (1=urbana, 6=rural),
#               P06 sexo (1=hombre, 6=mujer), A14REC nivel educativo (1-8),
#               A17A anos de estudio acumulados, Factor ponderador
# =============================================================================

# -----------------------------------------------------------------------------
# EDUCACION
# -----------------------------------------------------------------------------

#' Tasa de escolarizacion
#'
#' @description
#' Calcula la proporcion de personas que asisten a un establecimiento
#' educativo dentro de un rango de edad determinado. Corresponde a la
#' Tasa Neta de Escolarizacion (TNE), definida como el cociente entre
#' la poblacion en edad escolar que asiste y la poblacion total en ese
#' rango de edad. Usa la variable A05 de la EPHC (1 = asiste, 6 = no
#' asiste) y permite ponderacion por factor de expansion.
#'
#' @param data data.frame de ine_ephc(). Requiere P02, A05 y opcionalmente
#'   Factor, P06, AREA.
#' @param edad_min Edad minima del rango escolar. Por defecto 10.
#' @param edad_max Edad maxima del rango escolar. Por defecto 18.
#' @param grupo Variable de agrupacion: "ninguno", "sexo" o "area".
#'   Por defecto "ninguno".
#' @param ponderado Logico. Si TRUE usa Factor de expansion. Por defecto TRUE.
#'
#' @return Un tibble con columnas: grupo, n_asiste, n_total,
#'   tasa_escolarizacion (en porcentaje).
#'
#' @export
#' @examples
#' \dontrun{
#' ephc <- ine_ephc(2024, 1)
#' tasa_escolarizacion(ephc)
#' tasa_escolarizacion(ephc, grupo = "area")
#' tasa_escolarizacion(ephc, grupo = "sexo", ponderado = FALSE)
#' }
tasa_escolarizacion <- function(data,
                                edad_min  = 10,
                                edad_max  = 18,
                                grupo     = "ninguno",
                                ponderado = TRUE) {

  vars_requeridas <- c("P02", "A05")
  vars_faltantes  <- vars_requeridas[!vars_requeridas %in% names(data)]
  if (length(vars_faltantes) > 0) {
    stop("Faltan variables requeridas: ",
         paste(vars_faltantes, collapse = ", "))
  }

  if (!grupo %in% c("ninguno", "sexo", "area")) {
    stop("grupo debe ser 'ninguno', 'sexo' o 'area'")
  }

  # A05: 1 = asiste, 6 = no asiste
  datos <- data |>
    dplyr::filter(
      !is.na(.data$P02),
      !is.na(.data$A05),
      .data$A05 %in% c(1, 6),
      .data$P02 >= edad_min,
      .data$P02 <= edad_max
    ) |>
    dplyr::mutate(
      asiste = dplyr::if_else(.data$A05 == 1, 1L, 0L),
      peso   = if (ponderado && "Factor" %in% names(data))
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
      n_asiste            = sum(.data$peso[.data$asiste == 1], na.rm = TRUE),
      n_total             = sum(.data$peso, na.rm = TRUE),
      tasa_escolarizacion = round(.data$n_asiste / .data$n_total * 100, 2),
      .groups             = "drop"
    )
}


#' Anos promedio de estudio
#'
#' @description
#' Calcula el promedio de anos de escolaridad acumulados por la poblacion
#' de 25 anos o mas. Indicador de capital humano ampliamente usado en
#' investigacion sobre desarrollo economico (UNDP, 2010). Usa la variable
#' A17A de la EPHC que registra directamente los anos de estudio
#' acumulados de cada persona.
#'
#' @param data data.frame de ine_ephc(). Requiere A17A, P02 y opcionalmente
#'   Factor.
#' @param edad_min Edad minima para el calculo. Por defecto 25.
#' @param ponderado Logico. Si TRUE usa Factor de expansion. Por defecto TRUE.
#'
#' @return Un tibble con columnas: n, anos_promedio_estudio, error_estandar.
#'
#' @export
#' @examples
#' \dontrun{
#' ephc <- ine_ephc(2024, 1)
#' anios_promedio_estudio(ephc)
#' anios_promedio_estudio(ephc, edad_min = 15, ponderado = FALSE)
#' }
anios_promedio_estudio <- function(data,
                                   edad_min  = 25,
                                   ponderado = TRUE) {

  vars_requeridas <- c("A17A", "P02")
  vars_faltantes  <- vars_requeridas[!vars_requeridas %in% names(data)]
  if (length(vars_faltantes) > 0) {
    stop("Faltan variables requeridas: ",
         paste(vars_faltantes, collapse = ", "))
  }

  datos <- data |>
    dplyr::filter(
      !is.na(.data$A17A),
      !is.na(.data$P02),
      .data$P02 >= edad_min,
      .data$A17A >= 0,
      .data$A17A <= 25
    ) |>
    dplyr::mutate(
      peso = if (ponderado && "Factor" %in% names(data))
        .data$Factor else 1
    )

  datos |>
    dplyr::summarise(
      n                     = dplyr::n(),
      anos_promedio_estudio = round(
        sum(.data$A17A * .data$peso, na.rm = TRUE) /
          sum(.data$peso, na.rm = TRUE), 2
      ),
      error_estandar = round(
        sqrt(
          sum(.data$peso * (.data$A17A -
                              sum(.data$A17A * .data$peso) /
                              sum(.data$peso))^2, na.rm = TRUE) /
            sum(.data$peso, na.rm = TRUE)
        ), 2
      )
    )
}


#' Rezago educativo
#'
#' @description
#' Identifica la proporcion de poblacion en edad escolar con sobreedad
#' o atraso escolar. Se define como rezago la condicion en que una
#' persona asiste a un nivel educativo para el cual tiene mas anos de
#' los normativamente esperados (UNESCO, 2012). Criterio: edad observada
#' mayor a (edad normativa del nivel + 2 anos de tolerancia).
#' Usa A14REC para el nivel educativo (1-8) y A05 para asistencia
#' (1 = asiste).
#'
#' @param data data.frame de ine_ephc(). Requiere P02, A14REC, A05 y
#'   opcionalmente Factor.
#' @param ponderado Logico. Si TRUE usa Factor de expansion. Por defecto TRUE.
#'
#' @return Un tibble con columnas: n_total, n_rezago, tasa_rezago (porcentaje).
#'
#' @export
#' @examples
#' \dontrun{
#' ephc <- ine_ephc(2024, 1)
#' rezago_educativo(ephc)
#' }
rezago_educativo <- function(data, ponderado = TRUE) {

  vars_requeridas <- c("P02", "A14REC", "A05")
  vars_faltantes  <- vars_requeridas[!vars_requeridas %in% names(data)]
  if (length(vars_faltantes) > 0) {
    stop("Faltan variables requeridas: ",
         paste(vars_faltantes, collapse = ", "))
  }

  # Edad normativa maxima por nivel A14REC (1-8)
  edad_normativa <- c(
    "1" = 5,
    "2" = 11,
    "3" = 14,
    "4" = 17,
    "5" = 20,
    "6" = 22,
    "7" = 23,
    "8" = 26
  )

  datos <- data |>
    dplyr::filter(
      !is.na(.data$P02),
      !is.na(.data$A14REC),
      !is.na(.data$A05),
      .data$A05 == 1,
      .data$P02 >= 6,
      .data$P02 <= 24
    ) |>
    dplyr::mutate(
      edad_max_nivel = edad_normativa[as.character(.data$A14REC)],
      rezago = dplyr::if_else(
        !is.na(.data$edad_max_nivel) &
          .data$P02 > (.data$edad_max_nivel + 2),
        1L, 0L
      ),
      peso = if (ponderado && "Factor" %in% names(data))
        .data$Factor else 1
    ) |>
    dplyr::filter(!is.na(.data$edad_max_nivel))

  datos |>
    dplyr::summarise(
      n_total     = sum(.data$peso, na.rm = TRUE),
      n_rezago    = sum(.data$peso[.data$rezago == 1], na.rm = TRUE),
      tasa_rezago = round(.data$n_rezago / .data$n_total * 100, 2)
    )
}


# -----------------------------------------------------------------------------
# DESIGUALDAD
# -----------------------------------------------------------------------------

#' Brecha urbano-rural en educacion
#'
#' @description
#' Compara indicadores educativos entre poblacion urbana y rural.
#' La brecha territorial es un indicador clave de desigualdad en
#' informes de desarrollo humano (PNUD, 2019). Usa AREA de la EPHC
#' donde 1 = urbana y 6 = rural. Se calculan diferencias absolutas
#' (puntos porcentuales) y relativas (ratio urbano/rural).
#'
#' @param data data.frame de ine_ephc(). Requiere AREA y variables
#'   educativas segun indicador seleccionado.
#' @param indicador Indicador a comparar: "escolarizacion" o "anos_estudio".
#'   Por defecto "escolarizacion".
#' @param ponderado Logico. Si TRUE usa Factor de expansion. Por defecto TRUE.
#'
#' @return Un tibble con columnas: grupo, valor, brecha_absoluta,
#'   brecha_relativa.
#'
#' @export
#' @examples
#' \dontrun{
#' ephc <- ine_ephc(2024, 1)
#' brecha_urbano_rural(ephc)
#' brecha_urbano_rural(ephc, indicador = "anos_estudio")
#' }
brecha_urbano_rural <- function(data,
                                indicador = "escolarizacion",
                                ponderado = TRUE) {

  if (!"AREA" %in% names(data)) {
    stop("Variable AREA no encontrada en los datos")
  }

  if (!indicador %in% c("escolarizacion", "anos_estudio")) {
    stop("indicador debe ser 'escolarizacion' o 'anos_estudio'")
  }

  if (indicador == "escolarizacion") {
    resultado <- tasa_escolarizacion(data,
                                     grupo     = "area",
                                     ponderado = ponderado) |>
      dplyr::rename(valor = "tasa_escolarizacion") |>
      dplyr::select("grupo", "valor")

  } else {

    resultado <- data |>
      dplyr::filter(
        !is.na(.data$A17A),
        !is.na(.data$AREA),
        .data$P02 >= 25,
        .data$A17A >= 0,
        .data$A17A <= 25
      ) |>
      dplyr::mutate(
        peso  = if (ponderado && "Factor" %in% names(data))
          .data$Factor else 1,
        grupo = dplyr::case_when(
          .data$AREA == 1 ~ "Urbana",
          .data$AREA == 6 ~ "Rural",
          TRUE            ~ NA_character_
        )
      ) |>
      dplyr::filter(!is.na(.data$grupo)) |>
      dplyr::group_by(.data$grupo) |>
      dplyr::summarise(
        valor = round(
          sum(.data$A17A * .data$peso, na.rm = TRUE) /
            sum(.data$peso, na.rm = TRUE), 2
        ),
        .groups = "drop"
      )
  }

  if (!all(c("Urbana", "Rural") %in% resultado$grupo)) {
    stop("No se encontraron datos para ambas areas (Urbana y Rural)")
  }

  urbana <- resultado$valor[resultado$grupo == "Urbana"]
  rural  <- resultado$valor[resultado$grupo == "Rural"]

  resultado |>
    dplyr::mutate(
      brecha_absoluta = round(urbana - rural, 2),
      brecha_relativa = round(urbana / rural, 2)
    )
}


#' Brecha de genero en educacion
#'
#' @description
#' Compara indicadores educativos entre hombres y mujeres. Indicador
#' fundamental para el monitoreo de los ODS 4 y 5 (ONU, 2015).
#' Se calcula la diferencia absoluta y el Indice de Paridad de Genero
#' (IPG = valor femenino / valor masculino). Un IPG = 1 indica
#' paridad; menor a 1 indica desventaja femenina. Usa P06 de la EPHC
#' donde 1 = hombre y 6 = mujer.
#'
#' @param data data.frame de ine_ephc(). Requiere P06 y variables
#'   educativas segun indicador seleccionado.
#' @param indicador Indicador a comparar: "escolarizacion" o "anos_estudio".
#'   Por defecto "escolarizacion".
#' @param ponderado Logico. Si TRUE usa Factor de expansion. Por defecto TRUE.
#'
#' @return Un tibble con columnas: grupo, valor, brecha_absoluta
#'   (hombre - mujer), indice_paridad_genero.
#'
#' @export
#' @examples
#' \dontrun{
#' ephc <- ine_ephc(2024, 1)
#' brecha_genero(ephc)
#' brecha_genero(ephc, indicador = "anos_estudio")
#' }
brecha_genero <- function(data,
                          indicador = "escolarizacion",
                          ponderado = TRUE) {

  if (!"P06" %in% names(data)) {
    stop("Variable P06 (sexo) no encontrada en los datos")
  }

  if (!indicador %in% c("escolarizacion", "anos_estudio")) {
    stop("indicador debe ser 'escolarizacion' o 'anos_estudio'")
  }

  if (indicador == "escolarizacion") {
    resultado <- tasa_escolarizacion(data,
                                     grupo     = "sexo",
                                     ponderado = ponderado) |>
      dplyr::rename(valor = "tasa_escolarizacion") |>
      dplyr::select("grupo", "valor")

  } else {

    resultado <- data |>
      dplyr::filter(
        !is.na(.data$A17A),
        !is.na(.data$P06),
        .data$P02 >= 25,
        .data$A17A >= 0,
        .data$A17A <= 25
      ) |>
      dplyr::mutate(
        peso  = if (ponderado && "Factor" %in% names(data))
          .data$Factor else 1,
        grupo = dplyr::case_when(
          .data$P06 == 1 ~ "Hombre",
          .data$P06 == 6 ~ "Mujer",
          TRUE           ~ NA_character_
        )
      ) |>
      dplyr::filter(!is.na(.data$grupo)) |>
      dplyr::group_by(.data$grupo) |>
      dplyr::summarise(
        valor = round(
          sum(.data$A17A * .data$peso, na.rm = TRUE) /
            sum(.data$peso, na.rm = TRUE), 2
        ),
        .groups = "drop"
      )
  }

  if (!all(c("Hombre", "Mujer") %in% resultado$grupo)) {
    stop("No se encontraron datos para ambos sexos (Hombre y Mujer)")
  }

  hombre <- resultado$valor[resultado$grupo == "Hombre"]
  mujer  <- resultado$valor[resultado$grupo == "Mujer"]

  resultado |>
    dplyr::mutate(
      brecha_absoluta       = round(hombre - mujer, 2),
      indice_paridad_genero = round(mujer / hombre, 2)
    )
}


# -----------------------------------------------------------------------------
# POBREZA + EDUCACION
# -----------------------------------------------------------------------------

#' Educacion y pobreza multidimensional
#'
#' @description
#' Integra informacion educativa de la EPHC con el Indice de Pobreza
#' Multidimensional (IPM) del INE. Analiza la relacion entre nivel
#' educativo alcanzado y condicion de pobreza multidimensional del hogar.
#' Fundamental para investigacion sobre trampas de pobreza y retornos
#' a la educacion (Sen, 1999; Alkire y Foster, 2011). La union se
#' realiza por identificador de hogar construido desde UPM, NVIVI y NHOGA.
#'
#' @param data_ephc data.frame de ine_ephc(). Requiere UPM, NVIVI, NHOGA,
#'   A14REC y P02.
#' @param data_ipm data.frame de ine_ipm(). Requiere hhid y pobrezai.
#' @param edad_min Edad minima para el calculo. Por defecto 25.
#'
#' @return Un tibble con nivel educativo vs condicion de pobreza.
#'   Columnas: nivel_educativo, condicion_pobreza, n, porcentaje.
#'
#' @export
#' @examples
#' \dontrun{
#' ephc <- ine_ephc(2024, 1)
#' ipm  <- ine_ipm(2024)
#' educacion_pobreza(ephc, ipm)
#' }
educacion_pobreza <- function(data_ephc,
                              data_ipm,
                              edad_min = 25) {

  vars_ephc <- c("UPM", "NVIVI", "NHOGA", "A14REC", "P02")
  vars_faltantes <- vars_ephc[!vars_ephc %in% names(data_ephc)]
  if (length(vars_faltantes) > 0) {
    stop("Faltan variables en data_ephc: ",
         paste(vars_faltantes, collapse = ", "))
  }

  if (!"pobrezai" %in% names(data_ipm)) {
    stop("Variable pobrezai no encontrada en data_ipm")
  }

  etiquetas_nivel <- c(
    "1" = "Inicial",
    "2" = "Primaria",
    "3" = "Basica",
    "4" = "Media",
    "5" = "Tecnica",
    "6" = "Universitaria incompleta",
    "7" = "Universitaria completa",
    "8" = "Postgrado"
  )

  etiquetas_pobreza <- c(
    "0" = "No pobre",
    "1" = "Vulnerable",
    "2" = "Pobre moderado",
    "3" = "Pobre extremo"
  )

  ephc_hogar <- data_ephc |>
    dplyr::filter(.data$P02 >= edad_min, !is.na(.data$A14REC)) |>
    dplyr::mutate(
      hhid_ephc = paste0(
        formatC(.data$UPM,   width = 5, flag = "0"),
        "_",
        formatC(.data$NVIVI, width = 3, flag = "0"),
        "_",
        .data$NHOGA
      ),
      nivel_educativo = etiquetas_nivel[as.character(.data$A14REC)]
    ) |>
    dplyr::filter(!is.na(.data$nivel_educativo))

  ipm_prep <- data_ipm |>
    dplyr::mutate(
      condicion_pobreza = etiquetas_pobreza[as.character(.data$pobrezai)]
    ) |>
    dplyr::filter(!is.na(.data$condicion_pobreza)) |>
    dplyr::select("hhid", "condicion_pobreza")

  combinado <- dplyr::inner_join(
    ephc_hogar,
    ipm_prep,
    by = c("hhid_ephc" = "hhid")
  )

  if (nrow(combinado) == 0) {
    message("No se encontraron coincidencias entre EPHC e IPM.")
    message("Retornando tabla de nivel educativo solo con EPHC.")
    return(
      ephc_hogar |>
        dplyr::count(.data$nivel_educativo, name = "n") |>
        dplyr::mutate(
          porcentaje = round(.data$n / sum(.data$n) * 100, 2)
        ) |>
        dplyr::arrange(dplyr::desc(.data$n))
    )
  }

  combinado |>
    dplyr::count(.data$nivel_educativo,
                 .data$condicion_pobreza,
                 name = "n") |>
    dplyr::group_by(.data$nivel_educativo) |>
    dplyr::mutate(
      porcentaje = round(.data$n / sum(.data$n) * 100, 2)
    ) |>
    dplyr::ungroup() |>
    dplyr::arrange(.data$nivel_educativo, .data$condicion_pobreza)
}
