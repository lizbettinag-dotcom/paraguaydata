# =============================================================================
# Funciones analiticas para investigacion en educacion y desarrollo - Paraguay
# Instituto Nacional de Estadistica (INE) - EPHC e IPM
# =============================================================================

# -----------------------------------------------------------------------------
# EDUCACION
# -----------------------------------------------------------------------------

#' Tasa de escolarizacion
#'
#' @description
#' Calcula la proporcion de personas que asisten a un establecimiento
#' educativo dentro de un rango de edad determinado. Este indicador
#' corresponde a la Tasa Neta de Escolarizacion (TNE), definida como
#' el cociente entre la poblacion en edad escolar que asiste a la
#' escuela y la poblacion total en ese rango de edad. Permite el uso
#' de factores de expansion para estimaciones poblacionales.
#'
#' @param data data.frame proveniente de ine_ephc(). Debe contener
#'   las variables P02 (edad), A05 (asistencia escolar) y opcionalmente
#'   Factor (factor de expansion), P03 (sexo) y AREA (area geografica).
#' @param edad_min Edad minima del rango escolar. Por defecto 6.
#' @param edad_max Edad maxima del rango escolar. Por defecto 18.
#' @param grupo Variable de agrupacion opcional. Valores posibles:
#'   "ninguno", "sexo", "area". Por defecto "ninguno".
#' @param ponderado Logico. Si TRUE usa el factor de expansion.
#'   Por defecto TRUE.
#'
#' @return Un tibble con la tasa de escolarizacion. Si grupo es
#'   "ninguno" retorna una fila con tasa general. Si se especifica
#'   grupo retorna una fila por categoria con las columnas:
#'   grupo, n_asiste, n_total, tasa_escolarizacion.
#'
#' @export
#' @examples
#' \dontrun{
#' ephc <- ine_ephc(2024, 1)
#'
#' # Tasa general
#' tasa_escolarizacion(ephc)
#'
#' # Por area geografica
#' tasa_escolarizacion(ephc, grupo = "area")
#'
#' # Por sexo sin ponderar
#' tasa_escolarizacion(ephc, grupo = "sexo", ponderado = FALSE)
#' }
tasa_escolarizacion <- function(data,
                                edad_min  = 6,
                                edad_max  = 18,
                                grupo     = "ninguno",
                                ponderado = TRUE) {

  # Validaciones
  vars_requeridas <- c("P02", "A05")
  vars_faltantes  <- vars_requeridas[!vars_requeridas %in% names(data)]
  if (length(vars_faltantes) > 0) {
    stop("Faltan variables requeridas: ",
         paste(vars_faltantes, collapse = ", "))
  }

  if (!grupo %in% c("ninguno", "sexo", "area")) {
    stop("grupo debe ser 'ninguno', 'sexo' o 'area'")
  }

  # Filtrar por rango de edad
  datos <- data |>
    dplyr::filter(
      !is.na(.data$P02),
      !is.na(.data$A05),
      .data$P02 >= edad_min,
      .data$P02 <= edad_max
    ) |>
    dplyr::mutate(
      asiste = dplyr::if_else(.data$A05 == 1, 1L, 0L),
      peso   = if (ponderado && "Factor" %in% names(data))
        .data$Factor else 1
    )

  # Definir variable de agrupacion
  if (grupo == "sexo") {
    if (!"P03" %in% names(data)) stop("Variable P03 (sexo) no encontrada")
    datos <- dplyr::mutate(
      datos,
      grupo = dplyr::case_when(
        .data$P03 == 1 ~ "Hombre",
        .data$P03 == 2 ~ "Mujer",
        TRUE           ~ NA_character_
      )
    )
  } else if (grupo == "area") {
    if (!"AREA" %in% names(data)) stop("Variable AREA no encontrada")
    datos <- dplyr::mutate(
      datos,
      grupo = dplyr::case_when(
        .data$AREA == 1 ~ "Urbana",
        .data$AREA == 2 ~ "Rural",
        TRUE            ~ NA_character_
      )
    )
  } else {
    datos <- dplyr::mutate(datos, grupo = "Total")
  }

  # Calcular tasa
  datos |>
    dplyr::filter(!is.na(.data$grupo)) |>
    dplyr::group_by(.data$grupo) |>
    dplyr::summarise(
      n_asiste           = sum(.data$peso[.data$asiste == 1], na.rm = TRUE),
      n_total            = sum(.data$peso, na.rm = TRUE),
      tasa_escolarizacion = round(.data$n_asiste / .data$n_total * 100, 2),
      .groups            = "drop"
    )
}


#' Anos promedio de estudio
#'
#' @description
#' Calcula el promedio de anos de escolaridad acumulados por la
#' poblacion de 25 anos o mas. Este indicador es ampliamente
#' utilizado en investigacion sobre capital humano y desarrollo
#' economico (UNDP, 2010). Se basa en la variable de nivel
#' educativo maximo alcanzado (A01) de la EPHC.
#'
#' @param data data.frame proveniente de ine_ephc(). Debe contener
#'   las variables A01 (nivel educativo), P02 (edad) y opcionalmente
#'   Factor (factor de expansion).
#' @param edad_min Edad minima para el calculo. Por defecto 25.
#' @param ponderado Logico. Si TRUE usa el factor de expansion.
#'   Por defecto TRUE.
#'
#' @return Un tibble con las columnas:
#'   n (poblacion), anos_promedio_estudio, error_estandar.
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

  vars_requeridas <- c("A01", "P02")
  vars_faltantes  <- vars_requeridas[!vars_requeridas %in% names(data)]
  if (length(vars_faltantes) > 0) {
    stop("Faltan variables requeridas: ",
         paste(vars_faltantes, collapse = ", "))
  }

  # Tabla de conversion nivel educativo a anos de estudio (EPHC Paraguay)
  tabla_anos <- c(
    "0"  = 0,   # Sin instruccion
    "1"  = 3,   # Educacion inicial
    "2"  = 6,   # Primaria completa
    "3"  = 9,   # Basica completa
    "4"  = 12,  # Media completa
    "5"  = 15,  # Tecnica
    "6"  = 17,  # Universitaria incompleta
    "7"  = 17,  # Universitaria completa
    "8"  = 19,  # Postgrado
    "9"  = NA_real_  # Ignorado
  )

  datos <- data |>
    dplyr::filter(
      !is.na(.data$A01),
      !is.na(.data$P02),
      .data$P02 >= edad_min
    ) |>
    dplyr::mutate(
      anos_estudio = tabla_anos[as.character(.data$A01)],
      peso         = if (ponderado && "Factor" %in% names(data))
        .data$Factor else 1
    ) |>
    dplyr::filter(!is.na(.data$anos_estudio))

  datos |>
    dplyr::summarise(
      n                    = dplyr::n(),
      anos_promedio_estudio = round(
        sum(.data$anos_estudio * .data$peso, na.rm = TRUE) /
          sum(.data$peso, na.rm = TRUE), 2
      ),
      error_estandar = round(
        sqrt(sum(.data$peso * (.data$anos_estudio -
                                 sum(.data$anos_estudio * .data$peso) /
                                 sum(.data$peso))^2) /
               sum(.data$peso)), 2
      )
    )
}


#' Rezago educativo
#'
#' @description
#' Identifica y cuantifica la proporcion de poblacion en edad escolar
#' que presenta sobreedad o atraso escolar. Se define como rezago
#' educativo la condicion en que una persona asiste a un nivel
#' educativo para el cual tiene mas anos de los normativamente
#' esperados (UNESCO, 2012). El criterio utilizado es: edad observada
#' mayor a (edad normativa del nivel + 2 anos de tolerancia).
#'
#' @param data data.frame proveniente de ine_ephc(). Debe contener
#'   P02 (edad), A01 (nivel educativo), A05 (asistencia escolar)
#'   y opcionalmente Factor (factor de expansion).
#' @param ponderado Logico. Si TRUE usa el factor de expansion.
#'   Por defecto TRUE.
#'
#' @return Un tibble con las columnas:
#'   n_total, n_rezago, tasa_rezago (en porcentaje).
#'
#' @export
#' @examples
#' \dontrun{
#' ephc <- ine_ephc(2024, 1)
#' rezago_educativo(ephc)
#' }
rezago_educativo <- function(data, ponderado = TRUE) {

  vars_requeridas <- c("P02", "A01", "A05")
  vars_faltantes  <- vars_requeridas[!vars_requeridas %in% names(data)]
  if (length(vars_faltantes) > 0) {
    stop("Faltan variables requeridas: ",
         paste(vars_faltantes, collapse = ", "))
  }

  # Edad normativa maxima por nivel educativo (Paraguay)
  edad_normativa <- c(
    "1" = 5,   # Inicial
    "2" = 11,  # Primaria
    "3" = 14,  # Basica
    "4" = 17,  # Media
    "5" = 20,  # Tecnica
    "6" = 22,  # Universitaria
    "7" = 23,  # Universitaria completa
    "8" = 26   # Postgrado
  )

  datos <- data |>
    dplyr::filter(
      !is.na(.data$P02),
      !is.na(.data$A01),
      !is.na(.data$A05),
      .data$A05 == 1,          # Solo quienes asisten
      .data$P02 >= 6,
      .data$P02 <= 24
    ) |>
    dplyr::mutate(
      edad_max_nivel = edad_normativa[as.character(.data$A01)],
      rezago         = dplyr::if_else(
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
      n_total    = sum(.data$peso, na.rm = TRUE),
      n_rezago   = sum(.data$peso[.data$rezago == 1], na.rm = TRUE),
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
#' La brecha educativa urbano-rural es un indicador clave de
#' desigualdad territorial ampliamente utilizado en informes de
#' desarrollo humano (PNUD, 2019). Se calculan diferencias absolutas
#' (en puntos porcentuales) y relativas (ratio urbano/rural).
#'
#' @param data data.frame proveniente de ine_ephc(). Debe contener
#'   AREA y al menos una variable educativa (A05 o A01).
#' @param indicador Indicador a comparar. Valores posibles:
#'   "escolarizacion" o "anos_estudio". Por defecto "escolarizacion".
#' @param ponderado Logico. Si TRUE usa el factor de expansion.
#'   Por defecto TRUE.
#'
#' @return Un tibble con las columnas: area, valor, brecha_absoluta,
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
      dplyr::rename(valor = .data$tasa_escolarizacion) |>
      dplyr::select(.data$grupo, .data$valor)

  } else {

    tabla_anos <- c("0"=0,"1"=3,"2"=6,"3"=9,"4"=12,
                    "5"=15,"6"=17,"7"=17,"8"=19,"9"=NA_real_)

    resultado <- data |>
      dplyr::filter(!is.na(.data$A01), !is.na(.data$AREA),
                    .data$P02 >= 25) |>
      dplyr::mutate(
        anos_estudio = tabla_anos[as.character(.data$A01)],
        peso         = if (ponderado && "Factor" %in% names(data))
          .data$Factor else 1,
        grupo        = dplyr::case_when(
          .data$AREA == 1 ~ "Urbana",
          .data$AREA == 2 ~ "Rural",
          TRUE            ~ NA_character_
        )
      ) |>
      dplyr::filter(!is.na(.data$grupo), !is.na(.data$anos_estudio)) |>
      dplyr::group_by(.data$grupo) |>
      dplyr::summarise(
        valor = round(
          sum(.data$anos_estudio * .data$peso, na.rm = TRUE) /
            sum(.data$peso, na.rm = TRUE), 2
        ),
        .groups = "drop"
      )
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
#' Compara indicadores educativos entre hombres y mujeres.
#' La brecha de genero en educacion es un indicador fundamental
#' para el monitoreo de los Objetivos de Desarrollo Sostenible
#' (ODS 4 y ODS 5). Se calcula como la diferencia absoluta y
#' el ratio entre el indicador femenino y masculino (indice de
#' paridad de genero, IPG).
#'
#' @param data data.frame proveniente de ine_ephc(). Debe contener
#'   P03 (sexo) y al menos una variable educativa.
#' @param indicador Indicador a comparar. Valores posibles:
#'   "escolarizacion" o "anos_estudio". Por defecto "escolarizacion".
#' @param ponderado Logico. Si TRUE usa el factor de expansion.
#'   Por defecto TRUE.
#'
#' @return Un tibble con las columnas: grupo (sexo), valor,
#'   brecha_absoluta (hombre - mujer), indice_paridad_genero.
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

  if (!"P03" %in% names(data)) {
    stop("Variable P03 (sexo) no encontrada en los datos")
  }

  if (!indicador %in% c("escolarizacion", "anos_estudio")) {
    stop("indicador debe ser 'escolarizacion' o 'anos_estudio'")
  }

  if (indicador == "escolarizacion") {
    resultado <- tasa_escolarizacion(data,
                                     grupo     = "sexo",
                                     ponderado = ponderado) |>
      dplyr::rename(valor = .data$tasa_escolarizacion) |>
      dplyr::select(.data$grupo, .data$valor)

  } else {

    tabla_anos <- c("0"=0,"1"=3,"2"=6,"3"=9,"4"=12,
                    "5"=15,"6"=17,"7"=17,"8"=19,"9"=NA_real_)

    resultado <- data |>
      dplyr::filter(!is.na(.data$A01), !is.na(.data$P03),
                    .data$P02 >= 25) |>
      dplyr::mutate(
        anos_estudio = tabla_anos[as.character(.data$A01)],
        peso         = if (ponderado && "Factor" %in% names(data))
          .data$Factor else 1,
        grupo        = dplyr::case_when(
          .data$P03 == 1 ~ "Hombre",
          .data$P03 == 2 ~ "Mujer",
          TRUE           ~ NA_character_
        )
      ) |>
      dplyr::filter(!is.na(.data$grupo), !is.na(.data$anos_estudio)) |>
      dplyr::group_by(.data$grupo) |>
      dplyr::summarise(
        valor = round(
          sum(.data$anos_estudio * .data$peso, na.rm = TRUE) /
            sum(.data$peso, na.rm = TRUE), 2
        ),
        .groups = "drop"
      )
  }

  hombre <- resultado$valor[resultado$grupo == "Hombre"]
  mujer  <- resultado$valor[resultado$grupo == "Mujer"]

  resultado |>
    dplyr::mutate(
      brecha_absoluta      = round(hombre - mujer, 2),
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
#' Multidimensional (IPM) del INE. Permite analizar la relacion entre
#' nivel educativo alcanzado y la condicion de pobreza multidimensional
#' del hogar. Esta integracion es fundamental para investigacion sobre
#' trampas de pobreza y retornos a la educacion (Sen, 1999;
#' Alkire y Foster, 2011).
#'
#' @param data_ephc data.frame proveniente de ine_ephc(). Debe contener
#'   UPM, NVIVI, NHOGA, A01 (nivel educativo) y P02 (edad).
#' @param data_ipm data.frame proveniente de ine_ipm(). Debe contener
#'   hhid y pobrezai.
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

  vars_ephc <- c("UPM", "NVIVI", "NHOGA", "A01", "P02")
  vars_faltantes <- vars_ephc[!vars_ephc %in% names(data_ephc)]
  if (length(vars_faltantes) > 0) {
    stop("Faltan variables en data_ephc: ",
         paste(vars_faltantes, collapse = ", "))
  }

  if (!"pobrezai" %in% names(data_ipm)) {
    stop("Variable pobrezai no encontrada en data_ipm")
  }

  # Etiquetas de nivel educativo
  etiquetas_nivel <- c(
    "0" = "Sin instruccion",
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

  # Construir ID de hogar en EPHC
  ephc_hogar <- data_ephc |>
    dplyr::filter(.data$P02 >= edad_min, !is.na(.data$A01)) |>
    dplyr::mutate(
      hhid_ephc = paste0(
        formatC(.data$UPM,   width = 5, flag = "0"),
        "_",
        formatC(.data$NVIVI, width = 3, flag = "0"),
        "_",
        .data$NHOGA
      ),
      nivel_educativo = etiquetas_nivel[as.character(.data$A01)]
    ) |>
    dplyr::filter(!is.na(.data$nivel_educativo))

  # Preparar IPM
  ipm_prep <- data_ipm |>
    dplyr::mutate(
      condicion_pobreza = etiquetas_pobreza[as.character(.data$pobrezai)]
    ) |>
    dplyr::filter(!is.na(.data$condicion_pobreza)) |>
    dplyr::select(.data$hhid, .data$condicion_pobreza)

  # Unir por ID de hogar
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

  # Tabla resumen
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
