# =============================================================================
# Indicadores ODS 4 - Educacion de Calidad - Paraguay
# Alineados con el marco de monitoreo UNESCO/ONU Agenda 2030
# Fuente: EPHC INE Paraguay y datos MEC
# Referencias: UNESCO UIS (2018), CEPAL (2022), INE Paraguay (2021)
# =============================================================================

#' Indicadores ODS 4 desde la EPHC
#'
#' @description
#' Calcula los indicadores del Objetivo de Desarrollo Sostenible 4
#' (Educacion de Calidad) disponibles desde la Encuesta Permanente
#' de Hogares Continua (EPHC) del INE de Paraguay. Los indicadores
#' estan alineados con el marco de monitoreo UNESCO/ONU Agenda 2030
#' y con los que Paraguay reporta oficialmente a la ONU.
#'
#' Indicadores calculados:
#' - 4.1: Tasa de finalizacion educacion basica (proxy)
#' - 4.3: Participacion en educacion (tasa escolarizacion)
#' - 4.4: Anos promedio de estudio (proxy capital humano)
#' - 4.5: Indice de paridad de genero en escolarizacion
#' - 4.5b: Brecha urbano-rural en escolarizacion
#' - 4.6: Tasa de rezago educativo (proxy alfabetizacion funcional)
#'
#' @param data data.frame de ine_ephc(). Requiere variables
#'   educativas y de identificacion.
#' @param anio Anio de los datos. Si NULL se detecta automaticamente.
#' @param trimestre Trimestre de los datos. Si NULL se detecta
#'   automaticamente.
#'
#' @return Un tibble con columnas: codigo_ods, meta_ods,
#'   indicador, valor, unidad, fuente, anio, trimestre.
#'
#' @export
#' @examples
#' \dontrun{
#' ephc <- ine_ephc(2024, 1)
#' ods4_ephc(ephc)
#' }
ods4_ephc <- function(data, anio = NULL, trimestre = NULL) {

  if (!"ESTGEO" %in% names(data) && !"A17A" %in% names(data)) {
    stop("Los datos no parecen ser de la EPHC.",
         " Usa ine_ephc() para obtener los datos.")
  }

  # Detectar anio y trimestre
  if (is.null(anio)) {
    anio <- if ("ANIO" %in% names(data))
      unique(data$ANIO)[1]
    else NA_integer_
  }

  if (is.null(trimestre)) {
    trimestre <- if ("TRIMESTRE" %in% names(data))
      unique(data$TRIMESTRE)[1]
    else NA_integer_
  }

  # --- Calcular indicadores ---

  # 4.3 Tasa de escolarizacion (10-18 anos)
  ind_43 <- tryCatch(
    tasa_escolarizacion(data)$tasa_escolarizacion,
    error = function(e) NA_real_
  )

  # 4.4 Anos promedio de estudio
  ind_44 <- tryCatch(
    anios_promedio_estudio(data)$anos_promedio_estudio,
    error = function(e) NA_real_
  )

  # 4.5 Indice de paridad de genero
  ipg <- tryCatch({
    esc_sexo <- tasa_escolarizacion(data, grupo = "sexo")
    mujer  <- esc_sexo$tasa_escolarizacion[esc_sexo$grupo == "Mujer"]
    hombre <- esc_sexo$tasa_escolarizacion[esc_sexo$grupo == "Hombre"]
    if (length(mujer) > 0 && length(hombre) > 0 && hombre > 0)
      round(mujer / hombre, 3)
    else NA_real_
  }, error = function(e) NA_real_)

  # 4.5b Brecha urbano-rural
  brecha_ur <- tryCatch({
    br <- brecha_urbano_rural(data)
    br$brecha_absoluta[1]
  }, error = function(e) NA_real_)

  # 4.6 Tasa de rezago educativo
  ind_46 <- tryCatch(
    rezago_educativo(data)$tasa_rezago,
    error = function(e) NA_real_
  )

  # 4.1 Proxy finalizacion basica (% con 9 o mas anos de estudio)
  ind_41 <- tryCatch({
    if (!"A17A" %in% names(data)) return(NA_real_)
    datos_pob <- data |>
      dplyr::filter(!is.na(.data$A17A), .data$P02 >= 15,
                    .data$P02 <= 24)
    if (nrow(datos_pob) == 0) return(NA_real_)
    peso <- if ("Factor" %in% names(data))
      datos_pob$Factor else rep(1, nrow(datos_pob))
    completo <- datos_pob$A17A >= 9
    round(sum(peso[completo], na.rm = TRUE) /
            sum(peso, na.rm = TRUE) * 100, 2)
  }, error = function(e) NA_real_)

  # Construir tabla
  tibble::tibble(
    codigo_ods = c("4.1", "4.3", "4.4", "4.5", "4.5b", "4.6"),
    meta_ods   = c(
      "Finalizacion educacion primaria y secundaria",
      "Acceso igualitario a formacion tecnica y superior",
      "Competencias para empleo, trabajo decente y emprendimiento",
      "Eliminar disparidades de genero en educacion",
      "Eliminar disparidades urbano-rurales en educacion",
      "Garantizar alfabetizacion y nociones aritmeticas"
    ),
    indicador  = c(
      "Poblacion 15-24 con 9+ anos de estudio (proxy)",
      "Tasa de escolarizacion 10-18 anos",
      "Anos promedio de estudio poblacion 25+",
      "Indice de paridad de genero (mujer/hombre)",
      "Brecha escolarizacion urbano-rural (pp)",
      "Tasa de rezago educativo"
    ),
    valor      = c(ind_41, ind_43, ind_44, ipg,
                   brecha_ur, ind_46),
    unidad     = c("%", "%", "anos", "indice", "pp", "%"),
    fuente     = "EPHC - INE Paraguay",
    anio       = anio,
    trimestre  = trimestre
  )
}


#' Indicadores ODS 4 desde datos MEC
#'
#' @description
#' Calcula indicadores del ODS 4 disponibles desde los datos de
#' matriculaciones del Ministerio de Educacion y Ciencias (MEC)
#' de Paraguay. Complementa los indicadores de la EPHC con datos
#' administrativos del sistema educativo.
#'
#' Indicadores calculados:
#' - 4.1b: Tasa de abandono escolar por ciclo
#' - 4.2: Cobertura educacion inicial
#' - 4.5c: Indice de paridad de genero en matricula
#'
#' @param data_basica data.frame de educacion basica cargado con
#'   mec_leer(). Opcional.
#' @param data_inicial data.frame de educacion inicial cargado con
#'   mec_leer(). Opcional.
#' @param data_media data.frame de educacion media cargado con
#'   mec_leer(). Opcional.
#'
#' @return Un tibble con columnas: codigo_ods, meta_ods,
#'   indicador, valor, unidad, fuente, anio.
#'
#' @export
#' @examples
#' \dontrun{
#' basica <- mec_leer("matriculaciones_educacion_escolar_basica.csv")
#' inicial <- mec_leer("matriculaciones_inicial.csv")
#' ods4_mec(data_basica = basica, data_inicial = inicial)
#' }
ods4_mec <- function(data_basica = NULL,
                     data_inicial = NULL,
                     data_media   = NULL) {

  if (is.null(data_basica) && is.null(data_inicial) &&
      is.null(data_media)) {
    stop("Debes proporcionar al menos un archivo de datos del MEC.")
  }

  resultados <- list()

  # 4.1b Tasa de abandono basica
  if (!is.null(data_basica)) {
    abandono <- tryCatch({
      ab <- mec_tasa_abandono(data_basica)
      ab$tasa_abandono[nrow(ab)]
    }, error = function(e) NA_real_)

    anio_basica <- if ("anio" %in% names(data_basica))
      unique(data_basica$anio)[1] else NA_integer_

    resultados[["basica"]] <- tibble::tibble(
      codigo_ods = "4.1b",
      meta_ods   = "Finalizacion educacion primaria y secundaria",
      indicador  = "Tasa de abandono educacion basica (1ro a 9no)",
      valor      = abandono,
      unidad     = "%",
      fuente     = "MEC Paraguay",
      anio       = anio_basica
    )

    # 4.5c IPG basica
    ipg_basica <- tryCatch({
      mat <- mec_matriculas_departamento(data_basica)
      round(sum(mat$total_mujeres) / sum(mat$total_hombres), 3)
    }, error = function(e) NA_real_)

    resultados[["ipg_basica"]] <- tibble::tibble(
      codigo_ods = "4.5c",
      meta_ods   = "Eliminar disparidades de genero en educacion",
      indicador  = "IPG matricula educacion basica",
      valor      = ipg_basica,
      unidad     = "indice",
      fuente     = "MEC Paraguay",
      anio       = anio_basica
    )
  }

  # 4.2 Cobertura inicial
  if (!is.null(data_inicial)) {
    anio_inicial <- if ("anio" %in% names(data_inicial))
      unique(data_inicial$anio)[1] else NA_integer_

    total_inicial <- tryCatch({
      mat <- mec_matriculas_departamento(data_inicial)
      sum(mat$total)
    }, error = function(e) NA_real_)

    resultados[["inicial"]] <- tibble::tibble(
      codigo_ods = "4.2",
      meta_ods   = "Acceso a educacion preescolar de calidad",
      indicador  = "Total matriculados educacion inicial",
      valor      = total_inicial,
      unidad     = "personas",
      fuente     = "MEC Paraguay",
      anio       = anio_inicial
    )
  }

  # 4.1c Abandono media
  if (!is.null(data_media)) {
    anio_media <- if ("anio" %in% names(data_media))
      unique(data_media$anio)[1] else NA_integer_

    abandono_media <- tryCatch({
      ab <- mec_tasa_abandono(data_media)
      ab$tasa_abandono[nrow(ab)]
    }, error = function(e) NA_real_)

    resultados[["media"]] <- tibble::tibble(
      codigo_ods = "4.1c",
      meta_ods   = "Finalizacion educacion secundaria",
      indicador  = "Tasa de abandono educacion media",
      valor      = abandono_media,
      unidad     = "%",
      fuente     = "MEC Paraguay",
      anio       = anio_media
    )
  }

  dplyr::bind_rows(resultados)
}


#' Tabla resumen ODS 4 combinando EPHC y MEC
#'
#' @description
#' Genera una tabla resumen completa de los indicadores ODS 4
#' disponibles para Paraguay, combinando datos de la EPHC (INE)
#' y del MEC. Presenta los indicadores con su codigo oficial,
#' meta, valor y fuente. Lista para incluir en articulos
#' academicos e informes de politica publica.
#'
#' @param ephc data.frame de ine_ephc(). Opcional.
#' @param data_basica data.frame MEC educacion basica. Opcional.
#' @param data_inicial data.frame MEC educacion inicial. Opcional.
#' @param data_media data.frame MEC educacion media. Opcional.
#'
#' @return Un tibble con todos los indicadores ODS 4 disponibles
#'   ordenados por codigo.
#'
#' @export
#' @examples
#' \dontrun{
#' ephc   <- ine_ephc(2024, 1)
#' basica <- mec_leer("matriculaciones_educacion_escolar_basica.csv")
#' ods4_resumen(ephc = ephc, data_basica = basica)
#' }
ods4_resumen <- function(ephc        = NULL,
                         data_basica = NULL,
                         data_inicial = NULL,
                         data_media  = NULL) {

  if (is.null(ephc) && is.null(data_basica) &&
      is.null(data_inicial) && is.null(data_media)) {
    stop("Debes proporcionar al menos una fuente de datos.")
  }

  resultados <- list()

  if (!is.null(ephc)) {
    resultados[["ephc"]] <- ods4_ephc(ephc)
  }

  if (!is.null(data_basica) || !is.null(data_inicial) ||
      !is.null(data_media)) {
    resultados[["mec"]] <- ods4_mec(
      data_basica  = data_basica,
      data_inicial = data_inicial,
      data_media   = data_media
    )
  }

  dplyr::bind_rows(resultados) |>
    dplyr::arrange(.data$codigo_ods) |>
    dplyr::mutate(
      valor = round(.data$valor, 2)
    )
}
