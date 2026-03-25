# paraguaydata

<!-- badges: start -->
![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)
![R CMD Check](https://img.shields.io/badge/R%20CMD%20Check-passing-brightgreen)
<!-- badges: end -->

## Descripcion

`paraguaydata` es un paquete de R que facilita el acceso y analisis de
datos abiertos del Instituto Nacional de Estadistica (INE) de Paraguay.
Provee funciones para descargar microdatos oficiales y calcular
indicadores academicos reproducibles en las areas de educacion,
empleo y desarrollo social.

## Instalacion
```r
# Instalar desde GitHub
# install.packages("devtools")
devtools::install_github("lizbettinag-dotcom/paraguaydata")
```

## Fuentes de datos

| Fuente | Descripcion | Periodo |
|--------|-------------|---------|
| EPHC   | Encuesta Permanente de Hogares Continua | 2017-2025 |
| IPM    | Indice de Pobreza Multidimensional | 2016-2024 |
| GEO    | Codigos geograficos DGEEC | Actual |

## Funciones disponibles

### Nivel 1 — Acceso a datos

| Funcion | Descripcion |
|---------|-------------|
| `ine_catalogo()` | Lista todos los microdatos disponibles en el INE |
| `ine_ephc(anio, trimestre)` | Descarga microdatos EPHC trimestral |
| `ine_ipm(anio)` | Descarga microdatos del IPM |
| `ine_geo(nivel)` | Descarga codigos geograficos |

### Nivel 2 — Analisis academico

| Funcion | Descripcion |
|---------|-------------|
| `tasa_escolarizacion()` | Tasa Neta de Escolarizacion con ponderadores |
| `anios_promedio_estudio()` | Anos promedio de escolaridad |
| `rezago_educativo()` | Proporcion de poblacion con sobreedad escolar |
| `brecha_urbano_rural()` | Brecha educativa entre area urbana y rural |
| `brecha_genero()` | Indice de Paridad de Genero en educacion |
| `educacion_pobreza()` | Relacion entre nivel educativo y pobreza multidimensional |

## Uso basico
```r
library(paraguaydata)

# Descargar datos
ephc <- ine_ephc(anio = 2024, trimestre = 1)
ipm  <- ine_ipm(anio = 2024)

# Indicadores educativos
tasa_escolarizacion(ephc)
tasa_escolarizacion(ephc, grupo = "area")
tasa_escolarizacion(ephc, grupo = "sexo")

# Anos promedio de estudio
anios_promedio_estudio(ephc)

# Rezago educativo
rezago_educativo(ephc)

# Brechas
brecha_urbano_rural(ephc)
brecha_urbano_rural(ephc, indicador = "anos_estudio")
brecha_genero(ephc)
brecha_genero(ephc, indicador = "anos_estudio")

# Educacion y pobreza
educacion_pobreza(ephc, ipm)
```

## Ejemplo de resultados
```r
# Tasa de escolarizacion por area geografica
tasa_escolarizacion(ephc, grupo = "area")
#> # A tibble: 2 x 4
#>   grupo  n_asiste n_total tasa_escolarizacion
#>   <chr>     <dbl>   <dbl>               <dbl>
#> 1 Rural     14529  241019                6.03
#> 2 Urbana    37362  540773                6.91

# Brecha de genero en anos de estudio
brecha_genero(ephc, indicador = "anos_estudio")
#> # A tibble: 2 x 4
#>   grupo  valor brecha_absoluta indice_paridad_genero
#>   <chr>  <dbl>           <dbl>                 <dbl>
#> 1 Hombre  4.82           -1.26                  1.26
#> 2 Mujer   6.08           -1.26                  1.26
```

## Codigos de variables EPHC

| Variable | Descripcion | Codigos |
|----------|-------------|---------|
| `A05` | Asistencia escolar | 1 = asiste, 6 = no asiste |
| `AREA` | Area geografica | 1 = urbana, 6 = rural |
| `P06` | Sexo | 1 = hombre, 6 = mujer |
| `A14REC` | Nivel educativo | 1-8 (inicial a postgrado) |
| `A17A` | Anos de estudio acumulados | Numerico |
| `Factor` | Factor de expansion | Numerico |

## Licencia de datos

Los datos utilizados por este paquete provienen del Instituto Nacional
de Estadistica (INE) de Paraguay y estan disponibles bajo la
Licencia de Uso de la Informacion Publica del Estado Paraguayo, que
autoriza su uso, transformacion y redistribucion citando la fuente.
Mas informacion en: https://www.paraguay.gov.py/datos-abiertos/licencias

## Citacion

Si usas este paquete en tu investigacion, por favor citalo como:

    Garcia, L. (2025). paraguaydata: Acceso y analisis de datos abiertos
    del INE Paraguay. R package version 0.1.0.
    https://github.com/lizbettinag-dotcom/paraguaydata

    Fuente de datos: Instituto Nacional de Estadistica (INE) de Paraguay.
    https://www.ine.gov.py

## Licencia del paquete

MIT (c) Liz Garcia
