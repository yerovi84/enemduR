# enemduR

`enemduR` es una librería R orientada a construir una base analítica reusable para trabajar con microdatos ENEMDU.

## Estado actual

La versión actual corresponde al andamiaje estructural inicial del paquete.

En esta fase el proyecto ya incluye:

- estructura base del paquete,
- API pública inicial,
- lectura de archivos `.dta`,
- estandarización básica de nombres,
- validación estructural mínima,
- diagnóstico básico,
- metadatos semilla para tipos de encuesta y alertas de comparabilidad,
- y funciones declaradas para las capas analíticas y de presentación que se implementarán en fases posteriores.

## Principios de diseño

- separación estricta entre núcleo analítico y capa de presentación,
- soporte genérico para ENEMDU mensual, trimestral y anual,
- metadatos explícitos,
- outputs estables,
- errores informativos,
- y crecimiento por fases.

## Estructura prevista

- `R/`: funciones del paquete
- `inst/extdata/`: metadatos y registros auxiliares
- `tests/testthat/`: pruebas unitarias
- `man/`: documentación generada
- `vignettes/`: documentación narrativa futura

## API pública inicial

### Ingesta y preparación
- `enemdu_read_data()`
- `enemdu_standardize_names()`
- `enemdu_clean_data()`

### Validación
- `enemdu_validate_structure()`
- `enemdu_validate_content()`
- `enemdu_diagnose_data()`

### Derivación e indicadores
- `enemdu_build_variables()`
- `enemdu_build_quintiles()`
- `enemdu_build_household_profile()`
- `enemdu_kpi_general()`
- `enemdu_kpi_employment()`
- `enemdu_kpi_households()`

### Tabulación y calidad
- `enemdu_tabulate()`
- `enemdu_tabulate_two_way()`
- `enemdu_check_quality()`

### Presentación Quarto
- `enemdu_card_kpi()`
- `enemdu_value_box()`
- `enemdu_badge_quality()`
- `enemdu_note_method()`
- `enemdu_section_header()`

## Alcance de la v1

La v1 está pensada para cubrir únicamente lo esencial:

- infraestructura formal del paquete,
- lectura reproducible,
- validación estructural y diagnóstica básica,
- metadatos mínimos,
- primeras funciones derivadas e indicadores en fases posteriores,
- y una base limpia para futuros productos consumidores.

## Fuera de alcance en esta etapa

- dashboards complejos,
- theming rico,
- mapas,
- visualización avanzada,
- series históricas complejas,
- y cálculo inferencial avanzado de diseño muestral.

## Próximo bloque de trabajo

La siguiente fase debe cerrar:

- contratos de entrada y salida,
- catálogos internos,
- clases de errores y advertencias,
- reglas de representatividad,
- y especificación formal de variables derivadas.
=======
Infrastructure tools for reproducible ENEMDU microdata analysis.

