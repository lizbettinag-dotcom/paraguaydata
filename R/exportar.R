# =============================================================================
# Funciones de exportacion de tablas para publicaciones academicas
# Genera tablas formateadas en Word (.docx) listas para articulos
# Usa flextable 0.9.x y officer 0.7.x
# =============================================================================

#' Exportar tabla a Word formateada para publicacion
#'
#' @description
#' Exporta cualquier data.frame a un archivo Word (.docx) con
#' formato APA 7ma edicion. La tabla incluye titulo, nota al pie
#' con la fuente, bordes y tipografia adecuados para publicaciones
#' academicas. Compatible con los formatos del CIDUNAE-UNAE.
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
    nota <- paste0("Nota. Elaboracion propia con datos de ",
                   fuente, ".")
  }

  # Identificar columnas numericas y de texto (type-safe con vapply)
  cols_num <- which(vapply(data, is.numeric, logical(1)))
  cols_chr <- which(vapply(data, is.character, logical(1)))

  # Crear tabla base con formato APA
  ft <- flextable::flextable(data) |>
    flextable::font(fontname = "Times New Roman", part = "all") |>
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
    flextable::align(align = "left", part = "header")

  if (length(cols_num) > 0) {
    ft <- flextable::align(ft, align = "right",
                           part = "body", j = cols_num)
  }
  if (length(cols_chr) > 0) {
    ft <- flextable::align(ft, align = "left",
                           part = "body", j = cols_chr)
  }

  ft <- flextable::autofit(ft)

  # Agregar titulo arriba y nota abajo
  ft_apa <- ft |>
    flextable::add_header_lines(values = titulo) |>
    flextable::add_footer_lines(values = nota) |>
    flextable::bold(part = "header", i = 1) |>
    flextable::align(part = "header", i = 1, align = "left") |>
    flextable::font(fontname = "Times New Roman",
                    part = "header", i = 1) |>
    flextable::fontsize(size = 11, part = "header", i = 1) |>
    flextable::border(
      part          = "header", i = 1,
      border.top    = officer::fp_border(width = 0),
      border.bottom = officer::fp_border(width = 0)
    ) |>
    flextable::italic(part = "footer", i = 1) |>
    flextable::align(part = "footer", i = 1, align = "left") |>
    flextable::font(fontname = "Times New Roman",
                    part = "footer", i = 1) |>
    flextable::fontsize(size = 10, part = "footer", i = 1) |>
    flextable::border(
      part          = "footer", i = 1,
      border.top    = officer::fp_border(width = 0),
      border.bottom = officer::fp_border(width = 0)
    )

  flextable::save_as_docx(ft_apa, path = archivo)

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
#' @param tablas Lista nombrada de data.frames.
#' @param archivo Ruta del archivo de salida.
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
#'     tasa_informalidad(ephc, grupo = "area")
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
    stop("Instala flextable: install.packages('flextable')")
  }

  lista_ft <- vector("list", length(tablas))

  for (i in seq_along(tablas)) {
    titulo_i <- if (!is.null(names(tablas)[i]) &&
                    names(tablas)[i] != "")
      names(tablas)[i]
    else
      paste0("Tabla ", i)

    nota_i <- paste0("Nota. Elaboracion propia con datos de ",
                     fuente, ".")

    # Identificar columnas (type-safe con vapply)
    cols_num_i <- which(vapply(tablas[[i]], is.numeric, logical(1)))
    cols_chr_i <- which(vapply(tablas[[i]], is.character, logical(1)))

    ft_i <- flextable::flextable(tablas[[i]]) |>
      flextable::font(fontname = "Times New Roman", part = "all") |>
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
      flextable::align(align = "left", part = "header")

    if (length(cols_num_i) > 0) {
      ft_i <- flextable::align(ft_i, align = "right",
                               part = "body", j = cols_num_i)
    }
    if (length(cols_chr_i) > 0) {
      ft_i <- flextable::align(ft_i, align = "left",
                               part = "body", j = cols_chr_i)
    }

    ft_i <- ft_i |>
      flextable::autofit() |>
      flextable::add_header_lines(values = titulo_i) |>
      flextable::add_footer_lines(values = nota_i) |>
      flextable::bold(part = "header", i = 1) |>
      flextable::align(part = "header", i = 1, align = "left") |>
      flextable::italic(part = "footer", i = 1) |>
      flextable::align(part = "footer", i = 1, align = "left") |>
      flextable::border(
        part          = "header", i = 1,
        border.top    = officer::fp_border(width = 0),
        border.bottom = officer::fp_border(width = 0)
      ) |>
      flextable::border(
        part          = "footer", i = 1,
        border.top    = officer::fp_border(width = 0),
        border.bottom = officer::fp_border(width = 0)
      )

    lista_ft[[i]] <- ft_i
  }

  do.call(
    flextable::save_as_docx,
    c(lista_ft, list(path = archivo))
  )

  message(length(tablas), " tablas exportadas a: ", archivo)
  invisible(archivo)
}


#' Exportar tabla de ODS 4 formateada para publicacion
#'
#' @description
#' Exporta la tabla de indicadores ODS 4 a Word con formato
#' especial que resalta los codigos ODS. Disenada para articulos
#' sobre desarrollo humano y politica educativa.
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

  cols_mostrar <- intersect(
    c("codigo_ods", "indicador", "valor", "unidad",
      "fuente", "anio"),
    names(data)
  )

  data_fmt <- data[, cols_mostrar] |>
    dplyr::mutate(valor = round(.data$valor, 2))

  nota <- paste0(
    "Nota. Indicadores alineados con el marco de monitoreo ",
    "UNESCO/ONU Agenda 2030. Elaboracion propia con datos de ",
    "EPHC-INE y MEC Paraguay."
  )

  exportar_word(
    data_fmt,
    archivo = archivo,
    titulo  = titulo,
    nota    = nota,
    fuente  = "EPHC-INE y MEC Paraguay"
  )

  message("Tabla ODS 4 exportada a: ", archivo)
  invisible(archivo)
}
