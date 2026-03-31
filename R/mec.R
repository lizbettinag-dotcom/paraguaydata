# =============================================================================
# Funciones para datos del Ministerio de Educacion y Ciencias (MEC) Paraguay
# Fuente: https://datos.mec.gov.py/data
# Los archivos CSV deben ser descargados manualmente del portal del MEC
# ya que el portal no expone una API de descarga directa.
# =============================================================================

#' Leer datos de matriculaciones del MEC
#'
#' @description
#' Lee y limpia un archivo CSV descargado manualmente del portal de
#' datos abiertos del Ministerio de Educacion y Ciencias (MEC) de
#' Paraguay. Estandariza los nombres de columnas y maneja
#' correctamente el encoding UTF-8 de los archivos del MEC.
#'
#' @param ruta Ruta al archivo CSV descargado del portal del MEC.
#'   Descarga disponible en https://datos.mec.gov.py/data
#' @param nivel Nivel educativo del archivo. Valores posibles:
#'   "inicial", "basica", "media", "permanente", "inclusiva",
#'   "departamentos", "establecimientos". Por defecto "auto".
#'
#' @return Un tibble con los datos del MEC limpios y listos
#'   para analizar.
#'
#' @export
#' @examples
#' \dontrun{
#' # Descargar CSV desde https://datos.mec.gov.py/data/matriculaciones_inicial
#' inicial <- mec_leer("matriculaciones_inicial_2023.csv")
#'
#' # Especificar nivel explicitamente
#' basica <- mec_leer("mi_archivo.csv", nivel = "basica")
#' }
mec_leer <- function(ruta, nivel = "auto") {

  if (!file.exists(ruta)) {
    stop("Archivo no encontrado: ", ruta,
         "\nDescarga el CSV desde https://datos.mec.gov.py/data")
  }

  niveles_validos <- c("auto", "inicial", "basica", "media",
                       "permanente", "inclusiva",
                       "departamentos", "establecimientos")

  if (!nivel %in% niveles_validos) {
    stop("nivel debe ser uno de: ",
         paste(niveles_validos, collapse = ", "))
  }

  # Leer CSV con encoding UTF-8 (formato nativo del MEC)
  datos <- readr::read_csv(
    ruta,
    locale         = readr::locale(encoding = "UTF-8"),
    show_col_types = FALSE
  )

  # Limpiar nombres de columnas
  names(datos) <- tolower(names(datos))
  names(datos) <- gsub(" ", "_", names(datos))

  message("Datos cargados: ", nrow(datos), " filas, ",
          ncol(datos), " columnas")
  datos
}


#' Resumen de matriculas por departamento
#'
#' @description
#' Calcula el total de matriculados por departamento a partir de
#' datos del MEC. Permite comparar la cobertura educativa entre
#' los 18 departamentos del Paraguay incluyendo el Chaco
#' (Boqueron y Alto Paraguay) que no cubre la EPHC trimestral.
#' Calcula totales por sexo y paridad de genero.
#'
#' @param data data.frame cargado con mec_leer(). Debe contener
#'   columnas nombre_departamento y columnas de matriculados.
#' @param por_sector Logico. Si TRUE desagrega por sector publico
#'   y privado. Por defecto FALSE.
#'
#' @return Un tibble con columnas: departamento, total_hombres,
#'   total_mujeres, total, porcentaje, ipg (indice paridad genero).
#'
#' @export
#' @examples
#' \dontrun{
#' basica <- mec_leer("matriculaciones_educacion_escolar_basica.csv")
#' mec_matriculas_departamento(basica)
#' mec_matriculas_departamento(basica, por_sector = TRUE)
#' }
mec_matriculas_departamento <- function(data, por_sector = FALSE) {

  if (!"nombre_departamento" %in% names(data)) {
    stop("Variable nombre_departamento no encontrada.",
         " Verifica que el archivo sea del MEC.")
  }

  # Detectar columnas de matriculados hombre y mujer
  cols_hombre <- names(data)[grepl("_hombre$", names(data))]
  cols_mujer  <- names(data)[grepl("_mujer$", names(data))]

  # Excluir columna total si existe
  cols_hombre <- cols_hombre[!grepl("total_", cols_hombre)]
  cols_mujer  <- cols_mujer[!grepl("total_", cols_mujer)]

  if (length(cols_hombre) == 0) {
    stop("No se encontraron columnas de matriculados.",
         " Verifica que el archivo contenga datos de matriculaciones.")
  }

  # Calcular totales
  datos <- data |>
    dplyr::mutate(
      total_hombres = rowSums(
        dplyr::across(dplyr::all_of(cols_hombre)),
        na.rm = TRUE
      ),
      total_mujeres = rowSums(
        dplyr::across(dplyr::all_of(cols_mujer)),
        na.rm = TRUE
      )
    )

  if (por_sector && "sector_o_tipo_gestion" %in% names(datos)) {
    resultado <- datos |>
      dplyr::group_by(.data$nombre_departamento,
                      .data$sector_o_tipo_gestion) |>
      dplyr::summarise(
        total_hombres = sum(.data$total_hombres, na.rm = TRUE),
        total_mujeres = sum(.data$total_mujeres, na.rm = TRUE),
        .groups = "drop"
      )
  } else {
    resultado <- datos |>
      dplyr::group_by(.data$nombre_departamento) |>
      dplyr::summarise(
        total_hombres = sum(.data$total_hombres, na.rm = TRUE),
        total_mujeres = sum(.data$total_mujeres, na.rm = TRUE),
        .groups = "drop"
      )
  }

  resultado |>
    dplyr::mutate(
      total      = .data$total_hombres + .data$total_mujeres,
      porcentaje = round(.data$total / sum(.data$total) * 100, 2),
      ipg        = round(.data$total_mujeres / .data$total_hombres, 2)
    ) |>
    dplyr::arrange(dplyr::desc(.data$total)) |>
    dplyr::rename(departamento = "nombre_departamento")
}


#' Resumen de matriculas por nivel y grado
#'
#' @description
#' Calcula el total de matriculados por grado o modalidad dentro
#' de un nivel educativo. Permite analizar la distribucion de
#' matriculas a lo largo del ciclo educativo e identificar
#' patrones de abandono entre grados.
#'
#' @param data data.frame cargado con mec_leer(). Debe contener
#'   columnas de grados o modalidades del nivel educativo.
#'
#' @return Un tibble con columnas: grado, total_hombres,
#'   total_mujeres, total, ipg (indice paridad genero).
#'
#' @export
#' @examples
#' \dontrun{
#' basica <- mec_leer("matriculaciones_educacion_escolar_basica.csv")
#' mec_matriculas_grado(basica)
#'
#' media <- mec_leer("matriculaciones_educacion_media.csv")
#' mec_matriculas_grado(media)
#' }
mec_matriculas_grado <- function(data) {

  # Detectar columnas de grados (excluir totales e identificadores)
  cols_excluir <- c("anio", "codigo_establecimiento",
                    "codigo_departamento", "nombre_departamento",
                    "codigo_distrito", "nombre_distrito",
                    "codigo_zona", "nombre_zona",
                    "codigo_barrio_localidad",
                    "nombre_barrio_localidad",
                    "codigo_institucion", "nombre_institucion",
                    "sector_o_tipo_gestion", "anho_cod_geo",
                    "total_matriculados_hombre",
                    "total_matriculados_mujer",
                    "cantidad_matriculados_hombre",
                    "cantidad_matriculados_mujer")

  cols_hombre <- names(data)[
    grepl("_hombre$", names(data)) &
      !names(data) %in% cols_excluir
  ]

  cols_mujer <- names(data)[
    grepl("_mujer$", names(data)) &
      !names(data) %in% cols_excluir
  ]

  if (length(cols_hombre) == 0) {
    stop("No se encontraron columnas de grados o modalidades.")
  }

  # Construir tabla por grado
  grados <- gsub("_hombre$", "", cols_hombre)
  grados <- gsub("_", " ", grados)
  grados <- tools::toTitleCase(grados)

  resultado <- data.frame(
    grado         = grados,
    total_hombres = colSums(data[, cols_hombre], na.rm = TRUE),
    total_mujeres = colSums(data[, cols_mujer],  na.rm = TRUE)
  )

  resultado |>
    dplyr::mutate(
      total = .data$total_hombres + .data$total_mujeres,
      ipg   = round(.data$total_mujeres / .data$total_hombres, 2)
    ) |>
    dplyr::arrange(dplyr::desc(.data$total))
}


#' Resumen de establecimientos por departamento
#'
#' @description
#' Calcula la cantidad de establecimientos escolares por
#' departamento y zona (urbana/rural) a partir del archivo
#' de establecimientos del MEC. Permite analizar la distribucion
#' territorial de la infraestructura educativa en Paraguay.
#'
#' @param data data.frame cargado con mec_leer() del archivo
#'   establecimientos del MEC.
#' @param por_zona Logico. Si TRUE desagrega por zona urbana y
#'   rural. Por defecto TRUE.
#'
#' @return Un tibble con columnas: departamento, zona (si
#'   por_zona = TRUE), n_establecimientos, porcentaje.
#'
#' @export
#' @examples
#' \dontrun{
#' est <- mec_leer("establecimientos_2023.csv")
#' mec_establecimientos_departamento(est)
#' mec_establecimientos_departamento(est, por_zona = FALSE)
#' }
mec_establecimientos_departamento <- function(data,
                                              por_zona = TRUE) {

  if (!"nombre_departamento" %in% names(data)) {
    stop("Variable nombre_departamento no encontrada.")
  }

  if (por_zona && "nombre_zona" %in% names(data)) {
    resultado <- data |>
      dplyr::group_by(.data$nombre_departamento,
                      .data$nombre_zona) |>
      dplyr::summarise(
        n_establecimientos = dplyr::n(),
        .groups = "drop"
      )
  } else {
    resultado <- data |>
      dplyr::group_by(.data$nombre_departamento) |>
      dplyr::summarise(
        n_establecimientos = dplyr::n(),
        .groups = "drop"
      )
  }

  resultado |>
    dplyr::mutate(
      porcentaje = round(
        .data$n_establecimientos /
          sum(.data$n_establecimientos) * 100, 2
      )
    ) |>
    dplyr::arrange(dplyr::desc(.data$n_establecimientos)) |>
    dplyr::rename(departamento = "nombre_departamento")
}


#' Grafico de matriculas del MEC por departamento
#'
#' @description
#' Genera un grafico de barras horizontales con el total de
#' matriculados por departamento. Permite comparar la cobertura
#' educativa entre departamentos incluyendo el Chaco.
#'
#' @param data data.frame cargado con mec_leer().
#' @param titulo Titulo del grafico. Opcional.
#' @param por_sector Logico. Si TRUE muestra barras apiladas
#'   por sector publico y privado. Por defecto FALSE.
#'
#' @return Un objeto ggplot2.
#'
#' @export
#' @examples
#' \dontrun{
#' basica <- mec_leer("matriculaciones_educacion_escolar_basica.csv")
#' grafico_mec_departamento(basica)
#' grafico_mec_departamento(basica, por_sector = TRUE)
#' }
grafico_mec_departamento <- function(data,
                                     titulo     = NULL,
                                     por_sector = FALSE) {

  datos <- mec_matriculas_departamento(data,
                                       por_sector = por_sector)

  if (is.null(titulo)) {
    titulo <- "Matriculados por departamento"
  }

  if (por_sector && "sector_o_tipo_gestion" %in% names(datos)) {
    ggplot2::ggplot(
      datos,
      ggplot2::aes(
        x    = stats::reorder(.data$departamento, .data$total),
        y    = .data$total,
        fill = .data$sector_o_tipo_gestion
      )
    ) +
      ggplot2::geom_col(position = "stack") +
      ggplot2::coord_flip() +
      ggplot2::scale_fill_manual(
        values = c("#1976D2", "#E91E63", "#4CAF50", "#FF9800"),
        name   = "Sector"
      ) +
      ggplot2::scale_y_continuous(labels = scales::comma) +
      ggplot2::labs(
        title   = titulo,
        x       = NULL,
        y       = "Total matriculados",
        caption = "Fuente: MEC Paraguay"
      ) +
      tema_paraguaydata()

  } else {
    datos_total <- datos |>
      dplyr::group_by(.data$departamento) |>
      dplyr::summarise(total = sum(.data$total), .groups = "drop")

    ggplot2::ggplot(
      datos_total,
      ggplot2::aes(
        x    = stats::reorder(.data$departamento, .data$total),
        y    = .data$total,
        fill = .data$total
      )
    ) +
      ggplot2::geom_col(show.legend = FALSE) +
      ggplot2::geom_text(
        ggplot2::aes(label = scales::comma(.data$total)),
        hjust = -0.1, size = 3
      ) +
      ggplot2::scale_fill_gradient(
        low = "#BBDEFB", high = "#1565C0"
      ) +
      ggplot2::scale_y_continuous(
        limits = c(0, max(datos_total$total) * 1.2),
        labels = scales::comma
      ) +
      ggplot2::coord_flip() +
      ggplot2::labs(
        title   = titulo,
        x       = NULL,
        y       = "Total matriculados",
        caption = "Fuente: MEC Paraguay"
      ) +
      tema_paraguaydata()
  }
}


#' Grafico de matriculas por grado
#'
#' @description
#' Genera un grafico de barras con la distribucion de matriculados
#' por grado o modalidad dentro de un nivel educativo. Permite
#' visualizar patrones de abandono entre grados.
#'
#' @param data data.frame cargado con mec_leer().
#' @param titulo Titulo del grafico. Opcional.
#'
#' @return Un objeto ggplot2.
#'
#' @export
#' @examples
#' \dontrun{
#' basica <- mec_leer("matriculaciones_educacion_escolar_basica.csv")
#' grafico_mec_grado(basica)
#' }
grafico_mec_grado <- function(data, titulo = NULL) {

  datos <- mec_matriculas_grado(data)

  datos_long <- dplyr::bind_rows(
    dplyr::mutate(datos, sexo = "Hombre", n = .data$total_hombres),
    dplyr::mutate(datos, sexo = "Mujer",  n = .data$total_mujeres)
  )

  if (is.null(titulo)) {
    titulo <- "Matriculados por grado"
  }

  ggplot2::ggplot(
    datos_long,
    ggplot2::aes(
      x    = stats::reorder(.data$grado, .data$total),
      y    = .data$n,
      fill = .data$sexo
    )
  ) +
    ggplot2::geom_col(position = "dodge") +
    ggplot2::scale_fill_manual(
      values = c("Hombre" = "#1976D2", "Mujer" = "#E91E63"),
      name   = "Sexo"
    ) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title   = titulo,
      x       = NULL,
      y       = "Total matriculados",
      caption = "Fuente: MEC Paraguay"
    ) +
    tema_paraguaydata()
}
