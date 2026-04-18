# paraguaydata

<!-- badges: start -->
![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)
![R CMD Check](https://img.shields.io/badge/R%20CMD%20Check-passing-brightgreen)
![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)
![Version](https://img.shields.io/badge/version-0.1.0-blue)
<!-- badges: end -->

## Descripcion

`paraguaydata` es un paquete de R que facilita el acceso y analisis de
datos abiertos educativos y laborales de Paraguay. Provee funciones para
descargar microdatos oficiales del INE y MEC, calcular indicadores
academicos reproducibles, monitorear los Objetivos de Desarrollo Sostenible
(ODS 4) y exportar tablas con formato APA para publicaciones academicas.

## Instalacion

```r
# Instalar desde GitHub
# install.packages("devtools")
devtools::install_github("lizbettinag-dotcom/paraguaydata")
```

## Fuentes de datos

| Fuente | Descripcion | Periodo |
|--------|-------------|---------|
| EPHC | Encuesta Permanente de Hogares Continua (INE) | 2017-2025 |
| IPM | Indice de Pobreza Multidimensional (INE) | 2016-2024 |
| GEO | Codigos geograficos oficiales (DGEEC) | Actual |
| MEC | Matriculas y establecimientos educativos | 2022-2025 |
| PISA | Resultados Paraguay y America Latina | 2017, 2022 |

Los datos del INE y MEC son de acceso publico y gratuito. El Instituto
Nacional de Estadistica de Paraguay autoriza su uso para fines academicos
y de investigacion. Los datos PISA provienen de la OCDE y se distribuyen
bajo los terminos de uso de esa organizacion.

## Funciones disponibles

### Acceso a datos (INE)

| Funcion | Descripcion |
|---------|-------------|
| `ine_catalogo()` | Lista todos los microdatos disponibles |
| `ine_ephc(anio, trimestre)` | Descarga microdatos EPHC trimestral |
| `ine_ipm(anio)` | Descarga microdatos del IPM |
| `ine_geo(nivel)` | Descarga codigos geograficos |
| `ine_serie()` | Construye serie temporal de la EPHC |
| `ine_trimestres_disponibles()` | Lista trimestres disponibles |
| `ine_ultimo_trimestre()` | Descarga el trimestre mas reciente |
| `ine_hay_actualizacion()` | Verifica si hay datos nuevos |
| `ine_anio_completo()` | Descarga todos los trimestres de un anio |

### Indicadores educativos

| Funcion | Descripcion |
|---------|-------------|
| `tasa_escolarizacion()` | Tasa Neta de Escolarizacion |
| `anios_promedio_estudio()` | Anos promedio de escolaridad |
| `rezago_educativo()` | Proporcion de poblacion con sobreedad |
| `brecha_urbano_rural()` | Brecha educativa urbano-rural |
| `brecha_genero()` | Indice de Paridad de Genero |
| `educacion_pobreza()` | Relacion educacion-pobreza |
| `tendencia_escolarizacion()` | Tendencia temporal de indicadores |

### Indicadores laborales

| Funcion | Descripcion |
|---------|-------------|
| `tasa_desempleo()` | Tasa de desempleo abierto (OIT) |
| `tasa_informalidad()` | Tasa de informalidad laboral |
| `tasa_subempleo()` | Tasa de subempleo |
| `distribucion_rama()` | Distribucion por rama de actividad |

### Datos del MEC

| Funcion | Descripcion |
|---------|-------------|
| `mec_leer()` | Lee archivos CSV del portal datos.mec.gov.py |
| `mec_matriculas_departamento()` | Matriculas por departamento |
| `mec_matriculas_grado()` | Matriculas por grado |
| `mec_establecimientos_departamento()` | Establecimientos por departamento |
| `mec_tasa_abandono()` | Tasa de abandono escolar |
| `mec_abandono_departamento()` | Abandono por departamento |

### PISA 2022

| Funcion / Dataset | Descripcion |
|-------------------|-------------|
| `pisa_comparar_latam()` | Compara Paraguay con America Latina |
| `pisa_evolucion_paraguay()` | Evolucion 2017-2022 con brecha OCDE |
| `grafico_pisa_latam()` | Grafico comparativo regional |
| `pisa_latam_2022` | Dataset: 14 paises ALC, PISA 2022 |
| `pisa_paraguay` | Dataset: Paraguay 2017 y 2022 |

### ODS 4

| Funcion | Descripcion |
|---------|-------------|
| `ods4_ephc()` | Indicadores ODS 4 desde EPHC |
| `ods4_mec()` | Indicadores ODS 4 desde MEC |
| `ods4_resumen()` | Resumen integrado ODS 4 |

### Analisis territorial

| Funcion | Descripcion |
|---------|-------------|
| `agregar_departamento()` | Agrega datos por departamento |
| `educacion_por_departamento()` | Indicadores educativos por departamento |
| `empleo_por_departamento()` | Indicadores laborales por departamento |
| `resumen_departamental()` | Perfil integrado por departamento |
| `grafico_departamentos()` | Visualizacion por departamento |

### Exportacion y visualizacion

| Funcion | Descripcion |
|---------|-------------|
| `exportar_word()` | Exporta tabla a Word formato APA |
| `exportar_word_multiples()` | Exporta multiples tablas a Word |
| `exportar_ods4_word()` | Exporta tabla ODS 4 a Word |
| `tema_paraguaydata()` | Tema ggplot2 para publicaciones |

## Uso basico

```r
library(paraguaydata)

# Descargar datos EPHC 2024 T1
ephc <- ine_ephc(2024, 1)

# Calcular indicadores educativos
tasa_escolarizacion(ephc)
anios_promedio_estudio(ephc)
rezago_educativo(ephc)

# Comparar Paraguay en PISA 2022
pisa_comparar_latam()
grafico_pisa_latam()

# Verificar si hay datos nuevos del INE
ine_hay_actualizacion(2024, 1)

# Exportar tabla a Word formato APA
tabla <- tasa_desempleo(ephc, grupo = "sexo")
exportar_word(tabla,
  archivo = "tabla_desempleo.docx",
  titulo  = "Tabla 1. Tasa de desempleo por sexo, Paraguay 2024",
  fuente  = "EPHC T1 2024 - INE Paraguay")
```

## Ejemplo: analisis completo

```r
library(paraguaydata)

# Cargar datos
ephc <- ine_ultimo_trimestre()

# Indicadores educativos por departamento
edu_dep <- educacion_por_departamento(ephc)

# Indicadores laborales por departamento
emp_dep <- empleo_por_departamento(ephc)

# Perfil departamental integrado
perfil <- resumen_departamental(ephc)

# Grafico comparativo
grafico_departamentos(perfil, indicador = "escolarizacion")
```

## Codigos geograficos

```r
# Departamentos, distritos y barrios
geo <- ine_geo()
geo$departamentos  # 18 departamentos
geo$distritos      # 254 distritos
```

## Indicadores disponibles

| Indicador | Funcion | Fuente |
|-----------|---------|--------|
| Tasa Neta de Escolarizacion | `tasa_escolarizacion()` | EPHC |
| Anos promedio de estudio | `anios_promedio_estudio()` | EPHC |
| Proporcion con sobreedad | `rezago_educativo()` | EPHC |
| Tasa de desempleo | `tasa_desempleo()` | EPHC |
| Tasa de informalidad | `tasa_informalidad()` | EPHC |
| Tasa de subempleo | `tasa_subempleo()` | EPHC |
| Indice de Pobreza Multidim. | `ine_ipm()` | IPM |
| Indice de Paridad de Genero | `brecha_genero()` | EPHC |
| Puntajes PISA | `pisa_comparar_latam()` | OCDE |
| Abandono escolar | `mec_tasa_abandono()` | MEC |
| Matriculacion | `mec_matriculas_departamento()` | MEC |

## Citacion

Si usas este paquete en tu investigacion, por favor citalo:

```r
citation("paraguaydata")
```

```
Garcia Martinez L (2026). paraguaydata: Access and Analyze Open
Educational and Labor Data from Paraguay. R package version 0.1.0.
https://github.com/lizbettinag-dotcom/paraguaydata
```

## Licencia

MIT License. Copyright (c) 2026 Liz Bettina Garcia Martinez.

Los datos accedidos a traves de este paquete provienen de fuentes
oficiales del gobierno de Paraguay (INE, MEC) y de la OCDE (PISA).
El paquete en si esta bajo licencia MIT y autoriza su uso, redistribucion
y transformacion con atribucion. Los datos tienen sus propias licencias
de uso segun cada institucion proveedora.
