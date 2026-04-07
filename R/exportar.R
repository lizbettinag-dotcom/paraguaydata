# =============================================================================
# Funciones de exportacion de tablas para publicaciones academicas
# Genera tablas formateadas en Word (.docx) y PDF listas para articulos
# Usa flextable y officer para formato APA 7ma edicion
# =============================================================================

#' Exportar tabla a Word formateada para publicacion
#'
#' @description
#' Exporta cualquier data.frame a un archivo Word (.docx) con
#' formato APA 7ma edicion. La tabla incluye titulo, nota al pie
#' con la fuente, bordes y tipografia adecuados para publicaciones
#' academicas.
#'
#' @param data data.frame a exportar.
#' @param archivo Ruta del archivo de salida. Por defecto
#'   "tabla.docx" en el directorio actual.
#' @param titulo Titulo de la tabla (requerido para APA).
#' @param nota Nota al pie de la tabla. Por defecto incluye
#'   la fuente de datos.
#' @param fuente Fuente de los datos para la nota. Por defecto
#'   "EPHC - INE Paraguay".
#'
#' @return Invisible. Guarda el archivo .docx en la ruta indicada
#'   y muestra un mensaje de confirmacion.
#'
#' @export
#' @examples
#' \dontrun{
#' ephc <- ine_ephc(2024, 1)
#' tabla <- tasa_desempleo(ephc, grupo = "sexo")
#' exportar_word(tabla,
#'   archivo = "tabla_desempleo.docx",
#'   titulo  = "Tabla 1. Tasa de desempleo por sexo, Paraguay 2024",
#'   fuente  = "EPHC T1 2024 - INE Paraguay")
#' }
exportar_word <- function(data,
                          archivo = "tabla.docx",
                          titulo  = "Tabla",
                          nota    = NULL,
                          fuente  = "EPHC - INE Paraguay") {

  if (!requireNamespace("flextable", quietly = TRUE)) {
    stop("Instala el paquete flextable: install.packages('flextable')")
  }

  if (!requireNamespace("officer", quietly = TRUE)) {
    stop("Instala el paquete officer: install.packages('officer')")
  }

  if (is.null(nota)) {
    nota <- paste0("Nota. Elaboracion propia con datos de ", fuente, ".")
  }

  # Crear tabla con flextable
  ft <- flextable::flextable(data) |>
    # Fuente y tamano
    flextable::font(fontname = "Times New Roman", part = "all") |>
    flextable::fontsize(size = 11, part = "all") |>
    flextable::fontsize(size = 11, part = "header") |>
    # Negrita en encabezado
    flextable::bold(part = "header") |>
    # Bordes APA (solo horizontal arriba, abajo encabezado, abajo tabla)
    flextable::border_remove() |>
    flextable::hline_top(
      border = officer::fp_border(width = 1.5),
      part   = "header"
    ) |>
    flextable::hline(
      border = officer::fp_border(width = 1),
      part   = "header"
    ) |>
    flextable::hline_bottom(
      border = officer::fp_border(width = 1.5),
      part   = "body"
    ) |>
    # Alineacion
    flextable::align(align = "left",  part = "header") |>
    flextable::align(align = "right", part = "body",
                     j = which(sapply(data, is.numeric))) |>
    flextable::align(align = "left",  part = "body",
                     j = which(sapply(data, is.character))) |>
    # Ancho automatico
    flextable::autofit()

  # Crear documento Word
  doc <- officer::read_docx() |>
    # Titulo de la tabla (formato APA: negrita, antes de la tabla)
    officer::body_add_par(titulo,
                          style = "Normal") |>
    officer::body_add_flextable(ft) |>
    # Nota al pie
    officer::body_add_par(nota, style = "Normal")

  # Aplicar italica a "Nota."
  print(doc, target = archivo)

  message("Tabla exportada a: ", archivo)
  invisible(archivo)
}


#' Exportar multiples tablas a un documento Word
#'
#' @description
#' Exporta una lista de tablas a un unico documento Word (.docx)
#' con formato APA 7ma edicion. Util para generar el apendice de
#' tablas de un articulo academico o capitulo de tesis en un
#' solo archivo.
#'
#' @param tablas Lista nombrada de data.frames. Los nombres se
#'   usan como titulos de cada tabla.
#' @param archivo Ruta del archivo de salida. Por defecto
#'   "tablas.docx".
#' @param fuente Fuente de los datos para las notas al pie.
#'
#' @return Invisible. Guarda el archivo .docx.
#'
#' @export
#' @examples
#' \dontrun{
#' ephc <- ine_ephc(2024, 1)
#' mis_tablas <- list(
#'   "Tabla 1. Desempleo por sexo" =
#'     tasa_desempleo(ephc, grupo = "sexo"),
#'   "Tabla 2. Informalidad por area" =
#'     tasa_informalidad(ephc, grupo = "area"),
#'   "Tabla 3. Indicadores ODS 4" =
#'     ods4_ephc(ephc)
#' )
#' exportar_word_multiples(mis_tablas,
#'   archivo = "tablas_articulo.docx")
#' }
exportar_word_multiples <- function(tablas,
                                    archivo = "tablas.docx",
                                    fuente  = "EPHC - INE Paraguay") {

  if (!is.list(tablas) || length(tablas) == 0) {
    stop("tablas debe ser una lista no vacia de data.frames.")
  }

  if (!requireNamespace("flextable", quietly = TRUE)) {
    stop("Instala el paquete flextable: install.packages('flextable')")
  }

  doc <- officer::read_docx()

  for (i in seq_along(tablas)) {
    titulo_i <- if (!is.null(names(tablas)[i]) &&
                    names(tablas)[i] != "")
      names(tablas)[i]
    else paste0("Tabla ", i)

    nota_i <- paste0("Nota. Elaboracion propia con datos de ",
                     fuente, ".")

    ft_i <- flextable::flextable(tablas[[i]]) |>
      flextable::font(fontname = "Times New Roman",
                      part = "all") |>
      flextable::fontsize(size = 11, part = "all") |>
      flextable::bold(part = "header") |>
      flextable::border_remove() |>
      flextable::hline_top(
        border = officer::fp_border(width = 1.5),
        part   = "header"
      ) |>
      flextable::hline(
        border = officer::fp_border(width = 1),
        part   = "header"
      ) |>
      flextable::hline_bottom(
        border = officer::fp_border(width = 1.5),
        part   = "body"
      ) |>
      flextable::align(align = "left",  part = "header") |>
      flextable::align(
        align = "right", part = "body",
        j = which(sapply(tablas[[i]], is.numeric))
      ) |>
      flextable::align(
        align = "left",  part = "body",
        j = which(sapply(tablas[[i]], is.character))
      ) |>
      flextable::autofit()

    # Agregar salto de pagina entre tablas (excepto la primera)
    if (i > 1) {
      doc <- officer::body_add_break(doc)
    }

    doc <- doc |>
      officer::body_add_par(titulo_i, style = "Normal") |>
      officer::body_add_flextable(ft_i) |>
      officer::body_add_par(nota_i, style = "Normal")
  }

  print(doc, target = archivo)
  message(length(tablas), " tablas exportadas a: ", archivo)
  invisible(archivo)
}


#' Exportar tabla de ODS 4 formateada para publicacion
#'
#' @description
#' Exporta la tabla de indicadores ODS 4 a Word con formato
#' especial que resalta los codigos ODS y presenta los valores
#' con sus unidades de medida. Disenada especificamente para
#' articulos sobre desarrollo humano y politica educativa.
#'
#' @param data data.frame resultado de ods4_resumen() o ods4_ephc().
#' @param archivo Ruta del archivo de salida.
#' @param titulo Titulo de la tabla.
#'
#' @return Invisible. Guarda el archivo .docx.
#'
#' @export
#' @examples
#' \dontrun{
#' ephc <- ine_ephc(2024, 1)
#' resumen_ods <- ods4_resumen(ephc = ephc)
#' exportar_ods4_word(resumen_ods,
#'   archivo = "tabla_ods4.docx",
#'   titulo  = "Tabla 1. Indicadores ODS 4 Paraguay 2024")
#' }
exportar_ods4_word <- function(data,
                               archivo = "tabla_ods4.docx",
                               titulo  = "Indicadores ODS 4 - Paraguay") {

  if (!requireNamespace("flextable", quietly = TRUE)) {
    stop("Instala flextable: install.packages('flextable')")
  }

  # Seleccionar y formatear columnas relevantes
  cols_mostrar <- intersect(
    c("codigo_ods", "indicador", "valor", "unidad",
      "fuente", "anio"),
    names(data)
  )

  data_fmt <- data[, cols_mostrar] |>
    dplyr::mutate(
      valor = round(.data$valor, 2)
    )

  ft <- flextable::flextable(data_fmt) |>
    flextable::font(fontname = "Times New Roman", part = "all") |>
    flextable::fontsize(size = 11, part = "all") |>
    flextable::bold(part = "header") |>
    flextable::bold(j = "codigo_ods", part = "body") |>
    flextable::border_remove() |>
    flextable::hline_top(
      border = officer::fp_border(width = 1.5),
      part   = "header"
    ) |>
    flextable::hline(
      border = officer::fp_border(width = 1),
      part   = "header"
    ) |>
    flextable::hline_bottom(
      border = officer::fp_border(width = 1.5),
      part   = "body"
    ) |>
    flextable::set_header_labels(
      codigo_ods = "Codigo ODS",
      indicador  = "Indicador",
      valor      = "Valor",
      unidad     = "Unidad",
      fuente     = "Fuente",
      anio       = "Anio"
    ) |>
    flextable::autofit()

  doc <- officer::read_docx() |>
    officer::body_add_par(titulo, style = "Normal") |>
    officer::body_add_flextable(ft) |>
    officer::body_add_par(
      paste0("Nota. Indicadores alineados con el marco de ",
             "monitoreo UNESCO/ONU Agenda 2030. ",
             "Elaboracion propia con datos de EPHC-INE y MEC Paraguay."),
      style = "Normal"
    )

  print(doc, target = archivo)
  message("Tabla ODS 4 exportada a: ", archivo)
  invisible(archivo)
}
