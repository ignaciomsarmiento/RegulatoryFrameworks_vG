# =============================================================================
# global.R - Pre-carga de datos para la aplicación Shiny
# =============================================================================
# Este archivo se ejecuta UNA SOLA VEZ cuando la app inicia.
# Los datos quedan disponibles para TODAS las sesiones de usuario.
# =============================================================================

library(shiny)
library(dplyr)
library(tidyr)
library(plotly)
library(reactable)
library(readxl)
library(shinyjs)

# -----------------------------------------------------------------------------
# 1. DATOS PRINCIPALES (RDS)
# -----------------------------------------------------------------------------

message("Cargando datos principales...")

# Normaliza la base total para que use `type` con valores t_min / t_max.
normalize_total_non_salary <- function(df) {
  if (is.null(df)) {
    return(df)
  }
  if ("min_max_total" %in% names(df) && !"type" %in% names(df)) {
    df <- dplyr::rename(df, type = min_max_total)
  }
  if ("type" %in% names(df)) {
    df <- dplyr::mutate(
      df,
      type = dplyr::recode(
        type,
        total_cost_min = "t_min",
        total_cost_max = "t_max",
        .default = type
      )
    )
  }
  df
}

# Datos base que se usaban al inicio del server
DATA_TABLA <- readRDS("data/non_salary/bonuses_and_benefits_component.rds")
DATA_NON_SALARY <- readRDS("data/non_salary/1. total_non_salary_costs.rds")
DATA_NON_SALARY_PAYER <- readRDS("data/non_salary/2. total_ns_costs_by_payer.rds")
DATA_NON_SALARY_COMPONENT <- readRDS("data/non_salary/total_ns_costs_by_component.rds")

# Datos within (tenure)
DATA_NON_SALARY_WITHIN <- normalize_total_non_salary(readRDS(
  "data/non_salary/wage_tenure_variation/total_non_salary_costs_within_tenure.rds"
))
DATA_NON_SALARY_PAYER_WITHIN <- readRDS(
  "data/non_salary/wage_tenure_variation/total_ns_costs_by_payer_within_tenure.rds"
)
DATA_NON_SALARY_COMPONENT_WITHIN <- readRDS(
  "data/non_salary/wage_tenure_variation/total_ns_costs_by_component_within_tenure.rds"
)

# Datos within (sin tenure)
DATA_NON_SALARY_WITHIN_NO_TENURE <- normalize_total_non_salary(readRDS(
  "data/non_salary/wage_variation/total_non_salary_costs_within.rds"
))
DATA_NON_SALARY_PAYER_WITHIN_NO_TENURE <- readRDS(
  "data/non_salary/wage_variation/total_ns_costs_by_payer_within.rds"
)
DATA_NON_SALARY_COMPONENT_WITHIN_NO_TENURE <- readRDS(
  "data/non_salary/wage_variation/total_ns_costs_by_component_within.rds"
)

# -----------------------------------------------------------------------------
# 2. DATOS DINÁMICOS (los que se cargaban dentro de renderPlotly)
# -----------------------------------------------------------------------------

message("Cargando datos por componente...")

# Archivos *_all.rds
DATA_BY_GROUP <- list(

  bonuses_and_benefits = tryCatch(
    readRDS("data/non_salary/bonuses_and_benefits_all.rds"),
    error = function(e) NULL
  ),
  payroll_taxes = tryCatch(
    readRDS("data/non_salary/payroll_taxes_all.rds"),
    error = function(e) NULL
  ),
  pensions = tryCatch(
    readRDS("data/non_salary/pensions_all.rds"),
    error = function(e) NULL
  ),
  health = tryCatch(
    readRDS("data/non_salary/health_all.rds"),
    error = function(e) NULL
  ),
  occupational_risk = tryCatch(
    readRDS("data/non_salary/occupational_risk_all.rds"),
    error = function(e) NULL
  )
)

# Archivos *_within_tenure.rds
DATA_BY_GROUP_WITHIN <- list(
  bonuses_and_benefits = tryCatch(
    readRDS("data/non_salary/wage_tenure_variation/bonuses_and_benefits_within_tenure.rds"),
    error = function(e) NULL
  ),
  payroll_taxes = tryCatch(
    readRDS("data/non_salary/wage_tenure_variation/payroll_taxes_within_tenure.rds"),
    error = function(e) NULL
  ),
  pensions = tryCatch(
    readRDS("data/non_salary/wage_tenure_variation/pensions_within_tenure.rds"),
    error = function(e) NULL
  ),
  health = tryCatch(
    readRDS("data/non_salary/wage_tenure_variation/health_within_tenure.rds"),
    error = function(e) NULL
  ),
  occupational_risk = tryCatch(
    readRDS("data/non_salary/wage_tenure_variation/occupational_risk_within_tenure.rds"),
    error = function(e) NULL
  )
)

# Archivos *_within.rds (sin tenure)
DATA_BY_GROUP_WITHIN_NO_TENURE <- list(
  bonuses_and_benefits = tryCatch(
    readRDS("data/non_salary/wage_variation/bonuses_and_benefits_within.rds"),
    error = function(e) NULL
  ),
  payroll_taxes = tryCatch(
    readRDS("data/non_salary/wage_variation/payroll_taxes_within.rds"),
    error = function(e) NULL
  ),
  pensions = tryCatch(
    readRDS("data/non_salary/wage_variation/pensions_within.rds"),
    error = function(e) NULL
  ),
  health = tryCatch(
    readRDS("data/non_salary/wage_variation/health_within.rds"),
    error = function(e) NULL
  ),
  occupational_risk = tryCatch(
    readRDS("data/non_salary/wage_variation/occupational_risk_within.rds"),
    error = function(e) NULL
  )
)

# Archivos *_component.rds
DATA_BY_COMPONENT <- list(
  bonuses_and_benefits = tryCatch(
    readRDS("data/non_salary/bonuses_and_benefits_component.rds"),
    error = function(e) NULL
  )#,
  # pensions = tryCatch(
  #   readRDS("data/non_salary/pensions_component.rds"),
  #   error = function(e) NULL
  # ),
  # health = tryCatch(
  #   readRDS("data/non_salary/health_component.rds"),
  #   error = function(e) NULL
  # ),
  # occupational_risk = tryCatch(
  #   readRDS("data/non_salary/occupational_risk_component.rds"),
  #   error = function(e) NULL
  # )
)

# Archivos *_by_component_within_tenure.rds
DATA_BY_COMPONENT_WITHIN <- list(
  bonuses_and_benefits = tryCatch(
    readRDS("data/non_salary/wage_tenure_variation/bonuses_and_benefits_by_component_within_tenure.rds"),
    error = function(e) NULL
  )
)

# Archivos *_by_component_within.rds (sin tenure)
DATA_BY_COMPONENT_WITHIN_NO_TENURE <- list(
  bonuses_and_benefits = tryCatch(
    readRDS("data/non_salary/wage_variation/bonuses_and_benefits_by_component_within.rds"),
    error = function(e) NULL
  ),
  payroll_taxes = tryCatch(
    readRDS("data/non_salary/wage_variation/payroll_taxes_by_component_within.rds"),
    error = function(e) NULL
  )
)

# Archivos *_payer.rds
DATA_BY_PAYER <- list(
  pensions = tryCatch(
    readRDS("data/non_salary/pensions_payer.rds"),
    error = function(e) NULL
  ),
  health = tryCatch(
    readRDS("data/non_salary/health_payer.rds"),
    error = function(e) NULL
  ),
  payroll_taxes = tryCatch(
    readRDS("data/non_salary/payroll_taxes_payer.rds"),
    error = function(e) NULL
  )
)

# Archivos *_by_payer_within_tenure.rds
DATA_BY_PAYER_WITHIN <- list(
  pensions = tryCatch(
    readRDS("data/non_salary/wage_tenure_variation/pensions_by_payer_within_tenure.rds"),
    error = function(e) NULL
  ),
  health = tryCatch(
    readRDS("data/non_salary/wage_tenure_variation/health_by_payer_within_tenure.rds"),
    error = function(e) NULL
  ),
  payroll_taxes = tryCatch(
    readRDS("data/non_salary/wage_tenure_variation/payroll_taxes_by_payer_within_tenure.rds"),
    error = function(e) NULL
  )
)

# Archivos *_by_payer_within.rds (sin tenure)
DATA_BY_PAYER_WITHIN_NO_TENURE <- list(
  pensions = tryCatch(
    readRDS("data/non_salary/wage_variation/pensions_by_payer_within.rds"),
    error = function(e) NULL
  ),
  health = tryCatch(
    readRDS("data/non_salary/wage_variation/health_by_payer_within.rds"),
    error = function(e) NULL
  ),
  payroll_taxes = tryCatch(
    readRDS("data/non_salary/wage_variation/payroll_taxes_by_payer_within.rds"),
    error = function(e) NULL
  )
)

# -----------------------------------------------------------------------------
# 3. TABLAS DE EXCEL (el cuello de botella más grande)
# -----------------------------------------------------------------------------

message("Cargando tablas de Excel...")

# Cargar todas las hojas del Excel una sola vez
EXCEL_TABLES <- tryCatch({
  excel_path <- "data/non_salary/tables.xlsx"
  list(
    "TL All B"  = as.data.frame(read_excel(excel_path, sheet = "TL All B")),
    "TL ab"     = as.data.frame(read_excel(excel_path, sheet = "TL ab")),
    "TL pl"     = as.data.frame(read_excel(excel_path, sheet = "TL pl")),
    "TL up"     = as.data.frame(read_excel(excel_path, sheet = "TL up")),
    "TL Or"     = as.data.frame(read_excel(excel_path, sheet = "TL Or")),
    "TL H"      = as.data.frame(read_excel(excel_path, sheet = "TL H")),
    "TL Pt"     = as.data.frame(read_excel(excel_path, sheet = "TL Pt")),
    "TL All P"  = as.data.frame(read_excel(excel_path, sheet = "TL All P"))
  )
}, error = function(e) {
  warning("No se pudo cargar tables.xlsx: ", e$message)
  list()
})

# -----------------------------------------------------------------------------
# 4. CONSTANTES Y CONFIGURACIÓN
# -----------------------------------------------------------------------------

WAGE_LEVELS <- c("1sm", "2sm", "5sm", "10sm", "15sm")
WAGE_LABELS <- paste0(sub("sm", "", WAGE_LEVELS), " MW")
WAGE_CHOICES <- setNames(WAGE_LEVELS, WAGE_LABELS)

COUNTRIES_LIST <- c("All", unique(DATA_NON_SALARY$country))
COUNTRIES_LIST_WITHIN <- c("All", unique(DATA_NON_SALARY_WITHIN$country))
COUNTRIES_LIST_WITHIN_NO_TENURE <- c("All", unique(DATA_NON_SALARY_WITHIN_NO_TENURE$country))
TENURE_LEVELS <- sort(unique(DATA_NON_SALARY_WITHIN$tenure))

PLOTLY_FONT_FAMILY <- "National Park, 'Source Sans Pro', -apple-system, BlinkMacSystemFont, sans-serif"

COMPONENT_PALETTE <- c(

"Pension" = "#00C1FF",
"Health" = "#002244",
"Occupational Risk" = "#B9BAB5",
"Bonuses and Benefits" = "#335B8E",
"Payroll Taxes" = "#726AA8"
)

COMPONENT_STACK_ORDER <- c(
"Pension",
"Health",
"Occupational Risk",
"Bonuses and Benefits",
"Payroll Taxes"
)

COMPONENT_LEGEND_ORDER <- c(
"Bonuses and Benefits",
"Pension",
"Health",
"Occupational Risk",
"Payroll Taxes"
)

BONUS_PALETTE <- c(
"Annual and other periodic bonuses" = "#002244",
"Paid Leave" = "#8EA2BF",
"Unemployment Protection" = "#B9BAB5",
"Other bonuses" = "#6F6779"
)

BONUS_STACK_ORDER <- c(
"Annual and other periodic bonuses",
"Paid Leave",
"Unemployment Protection",
"Other bonuses"
)

COUNTRY_NAME_MAP <- c(
ARG = "Argentina",
BOL = "Bolivia",
BRA = "Brazil",
CHL = "Chile",
COL = "Colombia",
CRI = "Costa Rica",
DOM = "Dominican Republic",
ECU = "Ecuador",
ESP = "Spain",
SLV = "El Salvador",
GTM = "Guatemala",
HND = "Honduras",
MEX = "Mexico",
NIC = "Nicaragua",
PAN = "Panama",
PRY = "Paraguay",
PER = "Peru",
URY = "Uruguay",
US = "United States",
VEN = "Venezuela"
)

COUNTRY_FLAG_MAP <- c(
ARG = "ar",
BOL = "bo",
BRA = "br",
CHL = "cl",
COL = "co",
CRI = "cr",
DOM = "do",
ECU = "ec",
ESP = "es",
SLV = "sv",
GTM = "gt",
HND = "hn",
MEX = "mx",
NIC = "ni",
PAN = "pa",
PRY = "py",
PER = "pe",
URY = "uy",
US = "us",
VEN = "ve"
)

# -----------------------------------------------------------------------------
# 4b. DATA SOURCES FOR LABOR MODULES
# -----------------------------------------------------------------------------

LABOR_DATA_SOURCES_ACROSS <- list(
  tabla = DATA_TABLA,
  non_salary = DATA_NON_SALARY,
  non_salary_payer = DATA_NON_SALARY_PAYER,
  non_salary_component = DATA_NON_SALARY_COMPONENT,
  group_data = DATA_BY_GROUP,
  component_data = DATA_BY_COMPONENT,
  payer_data = DATA_BY_PAYER,
  countries = COUNTRIES_LIST,
  bonus_hover_source = DATA_BY_GROUP$bonuses_and_benefits
)

LABOR_DATA_SOURCES_WITHIN <- list(
  tabla = DATA_TABLA,
  non_salary = DATA_NON_SALARY_WITHIN,
  non_salary_payer = DATA_NON_SALARY_PAYER_WITHIN,
  non_salary_component = DATA_NON_SALARY_COMPONENT_WITHIN,
  group_data = DATA_BY_GROUP_WITHIN,
  component_data = DATA_BY_COMPONENT_WITHIN,
  payer_data = DATA_BY_PAYER_WITHIN,
  countries = COUNTRIES_LIST_WITHIN,
  bonus_hover_source = DATA_BY_GROUP_WITHIN$bonuses_and_benefits
)

LABOR_DATA_SOURCES_WITHIN_NO_TENURE <- list(
  tabla = DATA_TABLA,
  non_salary = DATA_NON_SALARY_WITHIN_NO_TENURE,
  non_salary_payer = DATA_NON_SALARY_PAYER_WITHIN_NO_TENURE,
  non_salary_component = DATA_NON_SALARY_COMPONENT_WITHIN_NO_TENURE,
  group_data = DATA_BY_GROUP_WITHIN_NO_TENURE,
  component_data = DATA_BY_COMPONENT_WITHIN_NO_TENURE,
  payer_data = DATA_BY_PAYER_WITHIN_NO_TENURE,
  countries = COUNTRIES_LIST_WITHIN_NO_TENURE,
  bonus_hover_source = DATA_BY_GROUP_WITHIN_NO_TENURE$bonuses_and_benefits
)

# -----------------------------------------------------------------------------
# 5. FUNCIONES HELPER (compartidas entre sesiones)
# -----------------------------------------------------------------------------

#' Obtener nombre completo del país
country_display_name <- function(country_code) {
  if (is.null(country_code) || country_code == "") {
    return(country_code)
  }
  code <- toupper(country_code)
  mapped <- COUNTRY_NAME_MAP[[code]]
  if (!is.null(mapped)) {
    return(mapped)
  }
  country_code
}

#' Formatear código de salario a etiqueta
format_wage_label <- function(wage_code) {
  if (is.null(wage_code) || length(wage_code) == 0) {
    return(character(0))
  }
  wage_code <- wage_code[!is.na(wage_code)]
  if (length(wage_code) == 0) {
    return(character(0))
  }
  paste0(substr(wage_code, 1, nchar(wage_code) - 2), " MW")
}

#' Formatear código de salario a frase
format_wage_phrase <- function(wage_code) {
  if (is.null(wage_code) || length(wage_code) == 0) {
    return("selected minimum wage levels")
  }
  wage_code <- wage_code[!is.na(wage_code)]
  if (length(wage_code) == 0) {
    return("selected minimum wage levels")
  }
  if (length(wage_code) > 1) {
    return("selected minimum wage levels")
  }
  wage_value <- suppressWarnings(as.integer(sub("sm", "", wage_code)))
  wage_word <- switch(
    as.character(wage_value),
    "1" = "one",
    "2" = "two",
    "5" = "five",
    "10" = "ten",
    "15" = "fifteen",
    as.character(wage_value)
  )
  if (is.na(wage_value)) {
    return(format_wage_label(wage_code))
  }
  if (wage_value == 1) {
    return(paste(wage_word, "minimum wage"))
  }
  paste(wage_word, "minimum wages")
}

#' Formatear países a frase
format_country_phrase <- function(countries) {
  if (is.null(countries) || length(countries) == 0 || "All" %in% countries) {
    return("across countries")
  }
  if (length(countries) == 1) {
    return(paste0("in ", country_display_name(countries[1])))
  }
  "across selected countries"
}

#' Formatear tenure a etiqueta
format_tenure_label <- function(tenure_value) {
  if (is.null(tenure_value) || length(tenure_value) == 0 || is.na(tenure_value)) {
    return("")
  }
  tenure_value <- as.integer(tenure_value[1])
  if (is.na(tenure_value)) {
    return("")
  }
  if (tenure_value == 1) {
    return("1 year")
  }
  paste0(tenure_value, " years")
}

#' Formatear tenure a frase
format_tenure_phrase <- function(tenure_value) {
  label <- format_tenure_label(tenure_value)
  if (label == "") {
    return("")
  }
  paste0("(tenure: ", label, ")")
}

#' Obtener datos por grupo (reemplaza readRDS dinámico)
get_group_data <- function(group_name) {
  DATA_BY_GROUP[[group_name]]
}

#' Obtener datos de componente (reemplaza readRDS dinámico)
get_component_data <- function(component_name) {
  DATA_BY_COMPONENT[[component_name]]
}

#' Obtener datos por payer (reemplaza readRDS dinámico)
get_payer_data <- function(payer_name) {
  DATA_BY_PAYER[[payer_name]]
}

#' Obtener tabla de Excel (reemplaza read_excel dinámico)
get_excel_table <- function(sheet_name) {
  EXCEL_TABLES[[sheet_name]]
}

# -----------------------------------------------------------------------------
message("✓ Datos cargados exitosamente")
message(sprintf("  - %d países disponibles", length(COUNTRIES_LIST) - 1))
message(sprintf("  - %d tablas de Excel cargadas", length(EXCEL_TABLES)))
# -----------------------------------------------------------------------------
