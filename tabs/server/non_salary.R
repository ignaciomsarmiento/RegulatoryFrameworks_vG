# =============================================================================
# labor_server.R - Versión optimizada del módulo server
# =============================================================================
# REQUIERE: global.R debe estar cargado antes de este archivo
# =============================================================================

non_salary_server_core <- function(input, output, session, data_sources = NULL, enable_tenure = FALSE) {
  
  # COLORS
  # Total

  ns <- session$ns

  if (is.null(data_sources)) {
    data_sources <- LABOR_DATA_SOURCES_ACROSS
  }
  resolve_sources <- function() {
    if (is.function(data_sources)) {
      data_sources()
    }
    else {
      data_sources
    }
  }
  tenure_enabled <- reactive({
    isTRUE(enable_tenure) && isTRUE(input$show_by_tenure)
  })
  
  # ============================================================================
  # DATOS: Ahora vienen de global.R (pre-cargados)
  # ============================================================================
  
  # Constantes de global.R
  wage_levels <- WAGE_LEVELS
  wage_labels <- WAGE_LABELS
  wage_choices <- WAGE_CHOICES
  plotly_font_family <- PLOTLY_FONT_FAMILY
  component_palette <- COMPONENT_PALETTE
  component_stack_order <- COMPONENT_STACK_ORDER
  component_legend_order <- COMPONENT_LEGEND_ORDER
  bonus_palette <- BONUS_PALETTE
  bonus_stack_order <- BONUS_STACK_ORDER
  country_name_map <- COUNTRY_NAME_MAP
  
  build_bonus_hover_lookup <- function(bonus_hover_source) {
    required_bonus_columns <- c(
      "min_max_total",
      "annual_or_periodic_bonuses",
      "paid_leave",
      "unemployment_protection",
      "other_bonuses",
      "legislation"
    )
    if (is.null(bonus_hover_source) ||
        !all(required_bonus_columns %in% names(bonus_hover_source))) {
      return(data.frame(
        country = character(0),
        wage = character(0),
        group = character(0),
        Type = character(0),
        hover_text = character(0),
        stringsAsFactors = FALSE
      ))
    }
    bonus_hover_source %>%
      mutate(
        group = ifelse(grepl("_min$", min_max_total), "Min", "Max")
      ) %>%
      select(
        country,
        wage,
        group,
        legislation,
        annual_or_periodic_bonuses,
        paid_leave,
        unemployment_protection,
        other_bonuses
      ) %>%
      tidyr::pivot_longer(
        cols = c(
          annual_or_periodic_bonuses,
          paid_leave,
          unemployment_protection,
          other_bonuses
        ),
        names_to = "Type",
        values_to = "detail_text"
      ) %>%
      mutate(
        Type = dplyr::recode(
          Type,
          annual_or_periodic_bonuses = "Annual and other periodic bonuses",
          paid_leave = "Paid Leave",
          unemployment_protection = "Unemployment Protection",
          other_bonuses = "Other bonuses"
        ),
        detail_text = dplyr::coalesce(detail_text, ""),
        legislation = dplyr::coalesce(legislation, ""),
        hover_text = dplyr::case_when(
          detail_text != "" & legislation != "" ~ paste0(detail_text, "<br><br>", legislation),
          detail_text != "" ~ detail_text,
          legislation != "" ~ legislation,
          TRUE ~ ""
        )
      ) %>%
      select(country, wage, group, Type, hover_text)
  }
  
  # non_salary variables
  initial_countries <- if (is.function(data_sources)) character(0) else data_sources$countries
  ns_variables <- reactiveValues(
    order_country = NULL,
    country_sel = "All",
    countries = initial_countries,
    df_final = NULL,
    df_final_tabla = NULL,
    missing_payroll_countries = NULL,
    missing_occ_risk_countries = NULL
  )

  if (is.function(data_sources)) {
    observeEvent(data_sources(), {
      sources <- resolve_sources()
      if (!is.null(sources$countries)) {
        ns_variables$countries <- sources$countries
      }
      if (isTRUE(enable_tenure) && identical(input$compare_mode, "wage")) {
        choices <- ns_variables$countries[ns_variables$countries != "All"]
        preferred <- if ("ARG" %in% choices) "ARG" else choices[1]
        if (!is.null(preferred) &&
            preferred != "" &&
            (is.null(ns_variables$country_sel) || ns_variables$country_sel == "All")) {
          ns_variables$country_sel <- preferred
          last_country_selection(preferred)
          last_single_country(preferred)
          updateSelectizeInput(session, ns("country_selection_user"), selected = preferred)
        }
      }
    }, ignoreInit = FALSE)
  }
  
  # ---- Selection Groups: Button results ----
  selected_group0 <- reactiveVal("all") # First Filter
  selected_groupA <- reactiveVal("total") # Total, by Payer, By Component   
  selected_groupB <- reactiveVal("1sm") # 1 MW / 2 MW / 5 MW / 10 MW / 15 MW
  selected_groupC <- reactiveVal("all_component")
  selected_groupD <- reactiveVal("all_bonuses")
  selected_groupE <- reactiveVal("pensions")
  option1_selected <- reactiveVal(FALSE)
  table_visible <- reactiveVal(FALSE)
  last_country_selection <- reactiveVal("All")
  last_wage_selection <- reactiveVal("1sm")
  last_single_country <- reactiveVal(NULL)
  last_compare_mode <- reactiveVal("country")
  option1_selected(TRUE)

  reset_across_defaults <- function() {
    ns_variables$country_sel <- "All"
    ns_variables$order_country <- NULL
    last_country_selection("All")
    last_wage_selection("1sm")
    last_single_country(NULL)
    selected_group0("all")
    selected_groupA("total")
    selected_groupB("1sm")
    selected_groupC("all_component")
    selected_groupD("all_bonuses")
    selected_groupE("pensions")
    option1_selected(TRUE)
    updateSelectizeInput(session, ns("mw_selection"), selected = "1sm")
    updateSelectizeInput(session, ns("country_selection_user"), selected = "All")
    shinyjs::runjs(sprintf("$('#%s').click();", ns("all")))
  }
  
  # Funciones helper - usan las de global.R
  # country_display_name, format_wage_label, format_wage_phrase, format_country_phrase
  # ya están definidas en global.R
  
  safe_value <- function(value, fallback) {
    if (is.null(value) || length(value) == 0) {
      return(fallback)
    }
    value
  }

  apply_tenure_filter <- function(df) {
    if (!tenure_enabled() || is.null(df) || !"tenure" %in% names(df)) {
      return(df)
    }
    tenure_value <- input$tenure_selection
    if (is.null(tenure_value) || length(tenure_value) == 0) {
      return(df)
    }
    df %>% dplyr::filter(tenure == tenure_value)
  }

  exclude_health_countries <- function(df) {
    if (is.null(df) || nrow(df) == 0 || !"country" %in% names(df)) {
      return(df)
    }
    mode <- safe_value(input$compare_mode, "country")
    group0 <- safe_value(selected_group0(), "all")
    groupE <- safe_value(selected_groupE(), "pensions")
    if (identical(mode, "country") && group0 == "social" && groupE == "health") {
      df <- df %>% dplyr::filter(!country %in% c("US", "BRA", "ESP"))
    }
    df
  }

  normalize_hetero_key <- function(text) {
    if (is.null(text) || length(text) == 0) return("")
    tolower(gsub("\\s+", " ", trimws(text)))
  }

  hetero_cache <- NULL
  get_heterogeneity_sections <- function() {
    if (!is.null(hetero_cache)) {
      return(hetero_cache)
    }
    html_path <- "data/non_salary/tables_html/tables_heterogeneity.html"
    if (!file.exists(html_path)) {
      hetero_cache <<- list(sections = list(), countries = character(0))
      return(hetero_cache)
    }
    html_raw <- paste(readLines(html_path, warn = FALSE), collapse = "\n")
    sections_raw <- strsplit(html_raw, "<!-- \\*+ -->")[[1]]

    parse_section <- function(s) {
      name_match <- regmatches(s, regexpr("<em>[^<]+</em>", s))
      if (length(name_match) == 0) return(NULL)
      sheet_name <- gsub("</?em>", "", name_match)

      table_match <- regmatches(s, regexpr("<table[\\s\\S]*?</table>", s, perl = TRUE))
      if (length(table_match) == 0) return(NULL)

      parts <- strsplit(sheet_name, " - ")[[1]]
      if (length(parts) != 2) return(NULL)
      country <- trimws(parts[2])
      cat_raw <- trimws(parts[1])
      category <- gsub("^TL Het\\.?\\s*", "", cat_raw)
      category <- trimws(category)

      list(
        sheet_name = sheet_name,
        country = country,
        category = category,
        category_key = normalize_hetero_key(category),
        table_html = table_match[1]
      )
    }

    sections <- Filter(Negate(is.null), lapply(sections_raw, parse_section))
    countries <- sort(unique(vapply(sections, `[[`, character(1), "country")))
    hetero_cache <<- list(sections = sections, countries = countries)
    hetero_cache
  }

  hetero_category_for_selection <- function(group0, groupE) {
    if (group0 == "all") return("All")
    if (group0 == "bonuses_and_benefits") return("Bonuses")
    if (group0 == "payroll_taxes") return("Payroll Taxes")
    if (group0 == "social") {
      return(switch(
        groupE,
        pensions = "Pension",
        health = "Health",
        occupational_risk = "Oc Risk",
        NULL
      ))
    }
    NULL
  }

  find_missing_countries <- function(df, candidates = NULL) {
    if (!is.null(candidates)) {
      candidates <- unique(candidates)
      candidates <- candidates[!is.na(candidates) & candidates != "All"]
    }
    if (is.null(df) || nrow(df) == 0 || !"country" %in% names(df) || !"value" %in% names(df)) {
      return(if (is.null(candidates)) character(0) else candidates)
    }
    summary <- df %>%
      group_by(country) %>%
      summarize(
        max_val = suppressWarnings(max(value, na.rm = TRUE)),
        .groups = "drop"
      )
    if (is.null(candidates) || length(candidates) == 0) {
      candidates <- summary$country
    }
    missing <- summary %>%
      filter(!is.finite(max_val) | max_val <= 0) %>%
      pull(country)
    missing <- union(missing, setdiff(candidates, summary$country))
    missing
  }

  get_group_data <- function(group_name) {
    sources <- resolve_sources()
    df <- apply_tenure_filter(sources$group_data[[group_name]])
    exclude_health_countries(df)
  }

  get_component_data <- function(component_name) {
    sources <- resolve_sources()
    apply_tenure_filter(sources$component_data[[component_name]])
  }

  get_payer_data <- function(payer_name) {
    sources <- resolve_sources()
    df <- apply_tenure_filter(sources$payer_data[[payer_name]])
    exclude_health_countries(df)
  }
  
  plot_title_text <- function() {
    group0 <- safe_value(selected_group0(), "all")
    groupA <- safe_value(selected_groupA(), "total")
    groupD <- safe_value(selected_groupD(), "all_bonuses")
    groupE <- safe_value(selected_groupE(), "pensions")

    subject <- switch(
      group0,
      all = "Non-salary labor costs",
      bonuses_and_benefits = "Bonuses and benefits",
      social = switch(
        groupE,
        pensions = "Pensions contributions",
        health = "Health contributions",
        occupational_risk = "Occupational risk contributions",
        "Social security contributions"
      ),
      payroll_taxes = "Payroll taxes",
      "Non-salary labor costs"
    )

    if (group0 == "bonuses_and_benefits" && groupA == "component") {
      subject <- switch(
        groupD,
        all_bonuses = "Bonuses and benefits",
        ab = "Annual and other periodic bonuses",
        pl = "Paid leave",
        up = "Unemployment protection",
        ob = "Other bonuses and benefits",
        subject
      )
    }

    view_phrase <- ""
    if (groupA == "payer") {
      view_phrase <- " by payer"
    } else if (groupA == "component") {
      if (group0 == "all") {
        view_phrase <- " by component"
      } else if (group0 == "bonuses_and_benefits" && groupD == "all_bonuses") {
        view_phrase <- " by component"
      }
    }

    country_phrase <- format_country_phrase(ns_variables$country_sel)
    wage_phrase <- format_wage_phrase(selected_groupB())
    tenure_phrase <- ""
    if (tenure_enabled()) {
      tenure_phrase <- format_tenure_phrase(input$tenure_selection)
      if (!is.null(tenure_phrase) && tenure_phrase != "") {
        tenure_phrase <- paste0(" ", tenure_phrase)
      }
    }

    paste0(
      subject,
      view_phrase,
      " ",
      country_phrase,
      " as a percentage of ",
      wage_phrase,
      " (%)",
      tenure_phrase
    )
  }
  
  y_axis_title_text <- function() {
    group0 <- safe_value(selected_group0(), "all")

    if (group0 == "bonuses_and_benefits") {
      return("Bonuses and benefits as share of wages (%)")
    }
    if (group0 == "social") {
      groupE <- safe_value(selected_groupE(), "pensions")
      if (groupE == "pensions") {
        return("Pensions contribution as share of wages (%)")
      }
      if (groupE == "health") {
        return("Health contribution as share of wages (%)")
      }
      if (groupE == "occupational_risk") {
        return("Occupational risk as share of wages (%)")
      }
      return("Social security contributions as share of wages (%)")
    }
    if (group0 == "payroll_taxes") {
      return("Payroll taxes as share of wages (%)")
    }
    "Non-salary costs as share of wages (%)"
  }
  
  plot_footer_annotations <- function() {
    access_date <- format(Sys.Date(), "%Y-%m-%d")
    mode <- safe_value(input$compare_mode, "country")
    is_cross_country <- identical(mode, "country")
    group0 <- safe_value(selected_group0(), "all")
    groupA <- safe_value(selected_groupA(), "total")
    groupD <- safe_value(selected_groupD(), "all_bonuses")
    groupE <- safe_value(selected_groupE(), "pensions")
    tenure_suffix <- ""
    if (!is_cross_country && tenure_enabled()) {
      tenure_suffix <- " and tenure (in years)"
    }

    country_label <- "the selected country"
    if (!is_cross_country) {
      country_sel <- ns_variables$country_sel
      if (!is.null(country_sel) && length(country_sel) > 0 && !"All" %in% country_sel) {
        country_label <- country_display_name(country_sel[1])
      } else if (!is.null(country_sel) && length(country_sel) > 1 && !"All" %in% country_sel) {
        country_label <- "selected countries"
      }
    }
    location_suffix <- if (is_cross_country) "" else paste0(" in ", country_label)
    wage_clause <- paste0("by wage level", tenure_suffix, location_suffix)

    us_clause <- "US denotes the simple average across the states of New York, California, Texas, and Florida."
    missing_health_clause <- "Data are not available for the United States, Brazil, and Spain."
    shared_costs_clause <- paste(
      "These costs are shared between employers and employees and include mandatory contributions for pensions,",
      "health insurance, occupational risk coverage, payroll taxes, and legally required bonuses and benefits."
    )
    shared_short_clause <- "These costs are shared between employers and employees."
    paid_exclusively_employers <- "This component is paid exclusively by employers."
    paid_exclusively_employer <- "This component is paid exclusively by employer."
    missing_payroll_clause <- NULL
    non_quantifiable_clause <- paste(
      "Non-quantifiable non-wage benefits include profit sharing bonuses (Chile, Dominican Republic, Ecuador,",
      "Mexico, Peru, Bolivia), family allowances/subsidies (Bolivia, Colombia), transport subsidies (Brazil),",
      "and relocation expenses (Ecuador)."
    )

    note_text <- ""
    note_sentences <- character(0)
    plot_output_id <- ns("plot")
    plot_width_px <- session$clientData[[paste0("output_", plot_output_id, "_width")]]
    if (is.null(plot_width_px) || !is.finite(plot_width_px)) {
      plot_width_px <- 900
    }
    note_width_px <- max(360, plot_width_px - 20)
    avg_char_px <- 5.0
    note_wrap_width <- floor(note_width_px / avg_char_px)
    note_wrap_width <- max(90, min(note_wrap_width, 260))
    if (group0 == "all") {
      note_sentences <- c(
        paste0(
          "Note: Bars show the statutory minimum and maximum non-wage labor costs as a percentage of wages for formal employees ",
          wage_clause,
          "."
        ),
        shared_costs_clause
      )
      if (is_cross_country) {
        note_sentences <- c(note_sentences, us_clause)
      }
    } else if (group0 == "bonuses_and_benefits") {
      if (groupA == "component" && groupD %in% c("ab", "pl", "up", "ob")) {
        component_label <- switch(
          groupD,
          ab = "annual and other periodic bonuses",
          pl = "paid leave",
          up = "unemployment protection",
          ob = "other bonuses and benefits",
          "bonuses and benefits"
        )
        payer_clause <- if (groupD == "pl" && is_cross_country) {
          paid_exclusively_employer
        } else {
          paid_exclusively_employers
        }
        note_sentences <- c(
          paste0(
            "Note: Bars show the statutory minimum and maximum cost of legally mandated ",
            component_label,
            " as a percentage of wages for formal employees ",
            wage_clause,
            "."
          ),
          payer_clause
        )
        if (is_cross_country) {
          note_sentences <- c(note_sentences, us_clause)
        }
      } else {
      note_sentences <- c(
        paste0(
          "Note: Bars show the statutory minimum and maximum cost of legally mandated bonuses and benefits as a percentage of wages for formal employees ",
          wage_clause,
            "."
          ),
          paid_exclusively_employers
        )
        if (is_cross_country) {
          note_sentences <- c(note_sentences, non_quantifiable_clause, us_clause)
        }
      }
    } else if (group0 == "social") {
      component_label <- switch(
        groupE,
        pensions = "pension contributions",
        health = "health contributions",
        occupational_risk = "occupational risk contributions",
        "social security contributions"
      )
      payer_clause <- if (groupE == "occupational_risk") {
        paid_exclusively_employers
      } else {
        shared_short_clause
      }
      note_sentences <- c(
        paste0(
          "Note: Bars show the statutory minimum and maximum ",
          component_label,
          " as a percentage of wages for formal employees ",
          wage_clause,
          "."
        ),
        payer_clause
      )
      if (is_cross_country) {
        note_sentences <- c(note_sentences, us_clause)
      }
      if (is_cross_country && groupE == "health") {
        note_sentences <- c(note_sentences, missing_health_clause)
      }
      if (is_cross_country && groupE == "occupational_risk") {
        missing_occ_risk <- ns_variables$missing_occ_risk_countries
        if (!is.null(missing_occ_risk) && length(missing_occ_risk) > 0) {
          missing_names <- vapply(missing_occ_risk, country_display_name, character(1))
          missing_list <- paste(missing_names, collapse = ", ")
          note_sentences <- c(note_sentences, paste0("Data are not available for ", missing_list, "."))
        }
      }
    } else if (group0 == "payroll_taxes") {
      note_sentences <- c(
        paste0(
          "Note: Bars show the statutory minimum and maximum payroll taxes as a percentage of wages for formal employees ",
          wage_clause,
          "."
        ),
        shared_short_clause
      )
      if (is_cross_country) {
        note_sentences <- c(note_sentences, us_clause)
      }
      missing_payroll_countries <- ns_variables$missing_payroll_countries
      if (!is.null(missing_payroll_countries) && length(missing_payroll_countries) > 0) {
        missing_names <- vapply(missing_payroll_countries, country_display_name, character(1))
        missing_list <- paste(missing_names, collapse = ", ")
        missing_payroll_clause <- paste0("Data are not available for ", missing_list, ".")
      }
      if (is_cross_country && groupA %in% c("total", "payer") && !is.null(missing_payroll_clause)) {
        note_sentences <- c(note_sentences, missing_payroll_clause)
      }
    } else {
      note_sentences <- c(
        paste0(
          "Note: Bars show the statutory minimum and maximum non-wage labor costs as a percentage of wages for formal employees ",
          wage_clause,
          "."
        )
      )
      if (is_cross_country) {
        note_sentences <- c(note_sentences, us_clause)
      }
    }

    note_text <- paste(note_sentences, collapse = " ")
    note_lines <- strwrap(note_text, width = note_wrap_width)
    if (length(note_lines) == 0) {
      note_lines <- ""
    }
    note_text <- paste(note_lines, collapse = "<br>")
    note_line_count <- length(note_lines)
    line_height_px <- 14
    note_yshift <- -100
    source_padding_px <- 10
    source_yshift <- note_yshift - (line_height_px * note_line_count) - source_padding_px
    margin_b <- max(240, abs(source_yshift) + line_height_px + 40)

    annotations <- list(
      list(
        text = note_text,
        xref = "paper",
        yref = "paper",
        x = 0,
        y = 0,
        xanchor = "left",
        yanchor = "top",
        align = "left",
        yshift = note_yshift,
        width = note_width_px,
        showarrow = FALSE,
        font = list(family = plotly_font_family, size = 10)
      ),
      list(
        text = paste0(
          "Source: World Bank, Regulatory Frameworks Database, 2026. Access date: ",
          access_date
        ),
        xref = "paper",
        yref = "paper",
        x = 0,
        y = 0,
        xanchor = "left",
        yanchor = "top",
        align = "left",
        yshift = source_yshift,
        showarrow = FALSE,
        font = list(family = plotly_font_family, size = 10)
      )
    )
    attr(annotations, "margin_b") <- margin_b
    annotations
  }
  
  apply_labor_plot_theme <- function(fig) {
    traces <- fig$x$data
    idx <- which(vapply(traces, function(tr) is.null(tr$hovertemplate), logical(1)))
    if (length(idx) > 0) {
      fig <- plotly::style(
        fig,
        hovertemplate = "%{x}<br>%{y:.2f}<extra>%{fullData.name}</extra>",
        traces = idx
      )
    }
    annotations <- plot_footer_annotations()
    margin_b <- attr(annotations, "margin_b")
    if (is.null(margin_b) || is.na(margin_b)) {
      margin_b <- 230
    }
    fig %>%
      layout(
        font = list(family = plotly_font_family),
        title = list(
          text = plot_title_text(),
          x = 0.5,
          xanchor = "center"
        ),
        annotations = annotations,
        margin = list(t = 60, b = margin_b)
      )
  }

  resolve_social_colors <- function(groupE) {
    component_label <- switch(
      groupE,
      pensions = "Pensions",
      health = "Health",
      occupational_risk = "Occupational Risk",
      "Social"
    )
    base_color <- component_palette[[component_label]]
    if (is.null(base_color) || is.na(base_color)) {
      base_color <- "#002244"
    }
    c(
      "Min" = base_color,
      "Max" = base_color
    )
  }
  
  
  output$country_selection <- renderUI({
    mode <- input$compare_mode
    if (is.null(mode) || length(mode) == 0) {
      mode <- "country"
    }
    choices <- ns_variables$countries
    selected_country <- "All"
    options <- NULL
    if (identical(mode, "wage")) {
      choices <- ns_variables$countries[ns_variables$countries != "All"]
      preferred <- "ARG"
      if (!preferred %in% choices) {
        preferred <- last_single_country()
      }
      if (is.null(preferred) || preferred == "" || !preferred %in% choices) {
        preferred <- choices[1]
      }
      selected_country <- preferred
      options <- list(maxItems = 1)
    }
    div(
      class = "pretty-select",
      selectizeInput(
        inputId = ns("country_selection_user"),
        label = "Country Analysis by:",
        choices = choices,
        selected = selected_country,
        multiple = TRUE,
        options = options
      )
    )
  })

  output$mw_selection_ui <- renderUI({
    mode <- input$compare_mode
    if (is.null(mode) || length(mode) == 0) {
      mode <- "country"
    }
    tenure_view <- isTRUE(input$show_by_tenure)
    allow_multiple <- identical(mode, "wage") && !tenure_view
    selection <- selected_groupB()
    if (is.null(selection) || length(selection) == 0) {
      selection <- last_wage_selection()
    }
    selection <- selection[selection %in% wage_levels]
    if (length(selection) == 0) {
      selection <- "1sm"
    }
    if (!allow_multiple && length(selection) > 1) {
      selection <- selection[1]
    }

    selectize_options <- NULL
    if (allow_multiple) {
      selectize_options <- list(plugins = list("remove_button"))
    }
    div(
      class = "pretty-select mw-select",
      selectizeInput(
        inputId = ns("mw_selection"),
        label = NULL,
        choices = wage_choices,
        selected = selection,
        multiple = allow_multiple,
        options = selectize_options
      )
    )
  })

  observeEvent(input$show_by_tenure, {
    if (!isTRUE(input$show_by_tenure)) {
      mode <- input$compare_mode
      if (is.null(mode) || length(mode) == 0) {
        mode <- "country"
      }
      if (identical(mode, "wage")) {
        updateSelectizeInput(session, ns("mw_selection"), selected = wage_levels)
        selected_groupB(wage_levels)
        last_wage_selection(wage_levels)
      }
      return()
    }
    selection <- selected_groupB()
    if (is.null(selection) || length(selection) == 0) {
      selection <- "1sm"
    }
    selection <- selection[selection %in% wage_levels]
    if (length(selection) == 0) {
      selection <- "1sm"
    }
    if (length(selection) > 1) {
      selection <- selection[1]
    }
    updateSelectizeInput(session, ns("mw_selection"), selected = selection)
  })
  
  
  # ---- First Selection ----
  observeEvent(input$btn_total,  { selected_groupA("total") })
  observeEvent(input$btn_payer,  { selected_groupA("payer") })
  observeEvent(input$btn_component,  { 
    selected_groupA("component") 
  })
  observeEvent(input$all,  {
    selected_group0("all")
    selected_groupC("all_component")
    option1_selected(TRUE)
  })
  observeEvent(input$country_selection_user, {
    selection <- input$country_selection_user
    if (is.null(selection) || length(selection) == 0) {
      selection <- "All"
    }

    previous <- last_country_selection()
    if ("All" %in% selection && length(selection) > 1) {
      if (!("All" %in% previous)) {
        selection <- "All"
      } else {
        selection <- setdiff(selection, "All")
      }
    }

    mode <- input$compare_mode
    if (is.null(mode) || length(mode) == 0) {
      mode <- "country"
    }
    if (identical(mode, "wage")) {
      if (is.null(selection) || length(selection) == 0 || "All" %in% selection) {
        preferred <- last_single_country()
        if (is.null(preferred) || preferred == "") {
          preferred <- ns_variables$countries[ns_variables$countries != "All"][1]
        }
        selection <- preferred
      } else if (length(selection) > 1) {
        selection <- selection[1]
      }
    }

    if (!identical(selection, input$country_selection_user)) {
      updateSelectizeInput(session, ns("country_selection_user"), selected = selection)
    }

    ns_variables$country_sel <- selection
    last_country_selection(selection)
    if (length(selection) == 1 && selection != "All") {
      last_single_country(selection)
    }
  })

  observeEvent(input$country_button, {
    code <- input$country_button
    if (is.null(code) || length(code) == 0 || code == "") {
      return()
    }
    ns_variables$country_sel <- code
    last_country_selection(code)
    last_single_country(code)
    updateSelectizeInput(session, ns("country_selection_user"), selected = code)
  })

  option2_choices_for_group <- function(group0) {
    switch(
      group0,
      all = c("total", "payer", "component"),
      bonuses_and_benefits = c("total", "component"),
      social = c("total", "payer"),
      payroll_taxes = c("total", "payer"),
      c("total")
    )
  }
  if (!exists("get_payer_data", mode = "function")) {
    get_payer_data <- function(payer_name) {
      if (exists("DATA_BY_PAYER", inherits = TRUE)) {
        return(DATA_BY_PAYER[[payer_name]])
      }
      payer_paths <- list(
        pensions = "data/non_salary/pensions_payer.rds",
        health = "data/non_salary/health_payer.rds",
        payroll_taxes = "data/non_salary/payroll_taxes_payer.rds"
      )
      path <- payer_paths[[payer_name]]
      if (is.null(path)) {
        return(NULL)
      }
      tryCatch(readRDS(path), error = function(e) NULL)
    }
  }

  observeEvent(selected_group0(), {
    valid_choices <- option2_choices_for_group(selected_group0())
    if (!selected_groupA() %in% valid_choices) {
      selected_groupA(valid_choices[1])
    }
  })

  observeEvent(input$compare_mode, {
    mode <- input$compare_mode
    if (is.null(mode) || length(mode) == 0) {
      return()
    }
    previous_mode <- last_compare_mode()
    if (identical(previous_mode, "wage") && identical(mode, "country")) {
      reset_across_defaults()
      return()
    }
    if (identical(mode, "wage")) {
      preferred <- "ARG"
      if (!preferred %in% ns_variables$countries) {
        preferred <- last_single_country()
      }
      if (is.null(preferred) || preferred == "" || !preferred %in% ns_variables$countries) {
        preferred <- ns_variables$countries[ns_variables$countries != "All"][1]
      }
      updateSelectizeInput(session, ns("country_selection_user"), selected = preferred)
      updateSelectizeInput(session, ns("mw_selection"), selected = wage_levels)
      selected_groupB(wage_levels)
      last_wage_selection(wage_levels)
      if (!is.null(preferred) && preferred != "") {
        last_single_country(preferred)
      }
    } else {
      wages <- selected_groupB()
      if (length(wages) > 1) {
        wages <- wages[1]
        updateSelectizeInput(session, ns("mw_selection"), selected = wages)
        selected_groupB(wages)
        last_wage_selection(wages)
      }
      if (!is.null(ns_variables$order_country) &&
          length(ns_variables$order_country) > 0 &&
          any(grepl("\\bMW\\b", ns_variables$order_country))) {
        ns_variables$order_country <- NULL
      }
    }
    last_compare_mode(mode)
  })
  
  
  # ---- MW Selection ----
  observeEvent(input$mw_selection, {
    selection <- input$mw_selection
    if (is.null(selection) || length(selection) == 0) {
      selection <- last_wage_selection()
    }
    selection <- unique(selection)
    selection <- selection[selection %in% wage_levels]
    if (length(selection) == 0) {
      selection <- last_wage_selection()
    }

    mode <- input$compare_mode
    if (is.null(mode)) {
      mode <- "country"
    }

    if (identical(mode, "country") && length(selection) > 1) {
      selection <- selection[1]
      showNotification("Across-country comparisons use one wage level.", type = "message")
    }

    if (identical(mode, "wage")) {
      countries <- ns_variables$country_sel
      if (is.null(countries) || length(countries) != 1 || "All" %in% countries) {
        selection <- last_wage_selection()
      }
    }

    if (!identical(selection, input$mw_selection)) {
      updateSelectizeInput(session, ns("mw_selection"), selected = selection)
    }

    selected_groupB(selection)
    last_wage_selection(selection)
  })
  
  # ---- Components ----
  observeEvent(input$all_component,  { selected_groupC("all_component") })
  observeEvent(input$bonus,  { 
    selected_groupC("bonuses_and_benefits")
    selected_group0("bonuses_and_benefits")
    option1_selected(TRUE)
  })
  observeEvent(input$social,  { 
    selected_group0("social")
    selected_groupC("social")
    selected_groupE("pensions")
    option1_selected(TRUE)
  })
  observeEvent(input$occupational_risk_main, {
    selected_group0("social")
    selected_groupC("social")
    selected_groupE("occupational_risk")
    selected_groupA("total")
    option1_selected(TRUE)
  })
  observeEvent(input$payroll, {
    selected_group0("payroll_taxes")
    selected_groupC("all_component")
    option1_selected(TRUE)
  })
  observeEvent(input$pensions,  { selected_groupE("pensions") })
  observeEvent(input$health, { selected_groupE("health") })
  observeEvent(input$occupational_risk, { selected_groupE("occupational_risk") })
  observeEvent(selected_groupE(), {
    groupE <- safe_value(selected_groupE(), "pensions")
    if (selected_group0() == "social" &&
        groupE == "occupational_risk" &&
        selected_groupA() == "payer") {
      selected_groupA("total")
    }
  })
  
  # ---- Bonuses and Benefits ----
  observeEvent(input$all_bonuses,  { selected_groupD("all_bonuses") })
  observeEvent(input$ab,  { selected_groupD("ab") })
  observeEvent(input$pl,  { selected_groupD("pl") })
  observeEvent(input$ob,  { selected_groupD("ob") })
  observeEvent(input$up,  { selected_groupD("up") })
  
  # ---- Graph ----
  output$plot <- renderPlotly({
    
    # Requirements
    req(selected_groupA())
    req(selected_groupB())
    
    # Results from user click
    group0 <- safe_value(selected_group0(), "all")
    groupA <- safe_value(selected_groupA(), "total")
    groupB <- safe_value(selected_groupB(), "1sm")
    groupC <- safe_value(selected_groupC(), "all_component")
    groupD <- safe_value(selected_groupD(), "all_bonuses")
    groupE <- safe_value(selected_groupE(), "pensions")
    is_cross_country <- identical(input$compare_mode, "country")
    ns_variables$missing_payroll_countries <- NULL
    ns_variables$missing_occ_risk_countries <- NULL

    sources <- resolve_sources()
    df_non_salary <- sources$non_salary
    df_non_salary_payer <- sources$non_salary_payer
    df_non_salary_component <- sources$non_salary_component
    bonus_hover_lookup <- build_bonus_hover_lookup(sources$bonus_hover_source)

    if (tenure_enabled()) {
      df_non_salary <- apply_tenure_filter(df_non_salary)
      df_non_salary_payer <- apply_tenure_filter(df_non_salary_payer)
      df_non_salary_component <- apply_tenure_filter(df_non_salary_component)
    }
    
    country_sel <- ns_variables$country_sel
    if (is.null(country_sel) || length(country_sel) == 0) {
      country_sel <- "All"
      ns_variables$country_sel <- country_sel
      last_country_selection(country_sel)
    }
    
    wage_codes <- groupB
    if (is.null(wage_codes) || length(wage_codes) == 0) {
      wage_codes <- "1sm"
    }
    wage_codes <- as.character(wage_codes)
    wage_codes <- wage_codes[!is.na(wage_codes)]
    if (length(wage_codes) == 0) {
      wage_codes <- "1sm"
    }
    wage_codes <- wage_levels[wage_levels %in% wage_codes]
    if (length(wage_codes) == 0) {
      wage_codes <- "1sm"
    }
    compare_wages <- identical(input$compare_mode, "wage") && length(wage_codes) > 1
    if (tenure_enabled() && length(wage_codes) > 1) {
      wage_codes <- wage_codes[1]
      compare_wages <- FALSE
    }
    if (!compare_wages && length(wage_codes) > 1) {
      wage_codes <- wage_codes[1]
    }
    if (!compare_wages &&
        !is.null(ns_variables$order_country) &&
        length(ns_variables$order_country) > 0 &&
        any(grepl("\\bMW\\b", ns_variables$order_country))) {
      ns_variables$order_country <- NULL
    }
    # Transform values from "1sm" → "1 MW"
    wage_filter <- format_wage_label(wage_codes)
    panel_order <- function() {
      if (compare_wages) {
        return(wage_filter)
      }
      current <- ns_variables$order_country
      if (is.null(current) || length(current) == 0) {
        return(NULL)
      }
      if (any(grepl("\\bMW\\b", current))) {
        return(NULL)
      }
      current
    }
    if (compare_wages) {
      ns_variables$order_country <- wage_filter
      if (length(ns_variables$country_sel) != 1 || "All" %in% ns_variables$country_sel) {
        return(NULL)
      }
    }
    y_axis_title <- y_axis_title_text()

    apply_wage_panels <- function(df) {
      if (!compare_wages || !"wage" %in% names(df)) {
        return(df)
      }
      df$country <- df$wage
      df
    }

    filter_missing_payroll <- function(df, candidates = NULL) {
      if (!is_cross_country || group0 != "payroll_taxes") {
        return(df)
      }
      missing <- find_missing_countries(df, candidates)
      ns_variables$missing_payroll_countries <- missing
      if (length(missing) > 0) {
        df <- df %>% filter(!country %in% missing)
      }
      df
    }

    filter_missing_occ_risk <- function(df, candidates = NULL) {
      if (!is_cross_country || group0 != "social" || groupE != "occupational_risk") {
        return(df)
      }
      missing <- find_missing_countries(df, candidates)
      ns_variables$missing_occ_risk_countries <- missing
      if (length(missing) > 0) {
        df <- df %>% filter(!country %in% missing)
      }
      df
    }

    filter_bonus_component <- function(df, component_code) {
      if (is.null(df) || is.null(component_code) || component_code == "") {
        return(df)
      }
      if ("component" %in% names(df)) {
        return(df %>% filter(component == component_code))
      }
      if (!"min_max_component" %in% names(df)) {
        return(df)
      }
      component_key <- switch(
        component_code,
        ab = "annual_bonuses",
        pl = "paid_leave",
        up = "unemployment_protection",
        ob = "other_bonuses",
        component_code
      )
      df %>% filter(grepl(component_key, min_max_component))
    }

    prepare_tenure_axis <- function(df) {
      df <- df %>%
        mutate(tenure_value = suppressWarnings(as.numeric(as.character(tenure))))
      use_numeric_tenure <- all(!is.na(df$tenure_value))
      if (use_numeric_tenure) {
        df <- df %>%
          mutate(tenure_plot = tenure_value) %>%
          arrange(tenure_value)
      } else {
        df <- df %>%
          mutate(tenure_plot = as.character(tenure)) %>%
          arrange(tenure_plot)
      }
      df
    }

    build_tenure_bar <- function(df, scenario_label, color, y_axis_title, y_range, show_y_title) {
      sub <- df %>% filter(Scenario == scenario_label)
      if (nrow(sub) == 0) {
        return(NULL)
      }
      plot_ly(
        data = sub,
        x = ~tenure_plot,
        y = ~value,
        type = "bar",
        marker = list(color = color),
        showlegend = FALSE
      ) %>%
        layout(
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor  = "rgba(0,0,0,0)",
          xaxis = list(
            title = "Tenure (in years)",
            showgrid = FALSE,
            zeroline = FALSE,
            showline = FALSE
          ),
          yaxis = list(
            title = ifelse(show_y_title, y_axis_title, ""),
            range = y_range,
            showgrid = FALSE,
            zeroline = FALSE,
            showline = FALSE
          )
        )
    }

    build_tenure_stack <- function(df, scenario_label, colors, stack_order, y_axis_title, y_range, show_y_title, show_legend) {
      sub <- df %>% filter(Scenario == scenario_label)
      if (nrow(sub) == 0) {
        return(NULL)
      }
      sub <- sub %>%
        mutate(Type = factor(Type, levels = stack_order))
      fig <- plot_ly(type = "bar")
      for (type in levels(sub$Type)) {
        sub_type <- sub %>% filter(Type == type)
        if (nrow(sub_type) == 0) {
          next
        }
        color <- colors[[type]]
        if (is.null(color)) {
          color <- "#00C1FF"
        }
        fig <- fig %>% add_trace(
          x = sub_type$tenure_plot,
          y = sub_type$value,
          name = type,
          marker = list(color = color),
          showlegend = show_legend,
          legendgroup = type
        )
      }
      fig %>%
        layout(
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor  = "rgba(0,0,0,0)",
          barmode = "stack",
          xaxis = list(
            title = "Tenure (in years)",
            showgrid = FALSE,
            zeroline = FALSE,
            showline = FALSE
          ),
          yaxis = list(
            title = ifelse(show_y_title, y_axis_title, ""),
            range = y_range,
            showgrid = FALSE,
            zeroline = FALSE,
            showline = FALSE
          )
        )
    }

    build_tenure_figure <- function(p_min, p_max, country_label, show_legend = FALSE) {
      fig <- subplot(
        p_min,
        p_max,
        nrows = 1,
        shareY = TRUE,
        titleX = TRUE,
        margin = 0.01
      ) %>%
        layout(
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor  = "rgba(0,0,0,0)",
          margin = list(l = 70, r = 30, b = 90, t = 40)
        )
      if (show_legend) {
        fig <- fig %>% layout(
          legend = list(
            orientation = "h",
            x = 0.5,
            xanchor = "center",
            y = -0.25
          )
        )
      }
      fig <- apply_labor_plot_theme(fig)

      title_annotations <- list(
        list(
          text = paste0(country_label, " - Min"),
          x = 0.23,
          y = 1.02,
          xref = "paper",
          yref = "paper",
          showarrow = FALSE,
          font = list(family = plotly_font_family, size = 14)
        ),
        list(
          text = paste0(country_label, " - Max"),
          x = 0.77,
          y = 1.02,
          xref = "paper",
          yref = "paper",
          showarrow = FALSE,
          font = list(family = plotly_font_family, size = 14)
        )
      )
      existing_annotations <- fig$x$layout$annotations
      if (is.null(existing_annotations)) {
        existing_annotations <- list()
      }
      fig %>% layout(annotations = c(existing_annotations, title_annotations))
    }

    build_exclusion_notice <- function(message) {
      annotations <- list(
        list(
          text = message,
          xref = "paper",
          yref = "paper",
          x = 0.5,
          y = 0.5,
          xanchor = "center",
          yanchor = "middle",
          align = "center",
          showarrow = FALSE,
          font = list(family = plotly_font_family, size = 18, color = "#0f3b66")
        )
      )
      plot_ly(
        type = "scatter",
        mode = "markers",
        x = numeric(0),
        y = numeric(0),
        showlegend = FALSE
      ) %>%
        layout(
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor  = "rgba(0,0,0,0)",
          xaxis = list(visible = FALSE, showgrid = FALSE, zeroline = FALSE, showline = FALSE),
          yaxis = list(visible = FALSE, showgrid = FALSE, zeroline = FALSE, showline = FALSE),
          annotations = annotations,
          margin = list(l = 40, r = 20, t = 40, b = 40)
        )
    }

    if (tenure_enabled()) {
      if (length(ns_variables$country_sel) != 1 || "All" %in% ns_variables$country_sel) {
        showNotification("Please select one country.", type = "error")
        return(NULL)
      }

      country_sel <- ns_variables$country_sel
      country_label <- country_display_name(country_sel)
      excluded_countries <- c("BRA", "CHL", "COL", "PER")
      if (country_sel %in% excluded_countries) {
        message <- paste0(
          country_label,
          " is excluded since its statutory non-salary labor costs do not vary with years of employee tenure."
        )
        return(build_exclusion_notice(message))
      }

      if (groupA == "total") {
        df_src <- NULL
        if (group0 == "all") {
          df_src <- df_non_salary
        } else if (group0 %in% c("bonuses_and_benefits", "payroll_taxes", "social")) {
          data_key <- if (group0 == "social") groupE else group0
          df_src <- get_group_data(data_key)
        }
        if (is.null(df_src)) {
          showNotification("Data not available for this selection.", type = "error")
          return(NULL)
        }
        scenario_source <- if ("type" %in% names(df_src)) {
          "type"
        } else if ("min_max_total" %in% names(df_src)) {
          "min_max_total"
        } else {
          NULL
        }
        if (is.null(scenario_source) || !"tenure" %in% names(df_src)) {
          showNotification("Tenure data not available for this selection.", type = "error")
          return(NULL)
        }
        df_tenure <- df_src %>%
          filter(
            country == country_sel,
            wage %in% wage_filter
          )
        if (nrow(df_tenure) == 0) {
          showNotification("No Data for this combination.", type = "error")
          return(NULL)
        }
        df_tenure <- df_tenure %>%
          mutate(
            Scenario = ifelse(grepl("_min$", .data[[scenario_source]]), "Min", "Max")
          ) %>%
          prepare_tenure_axis()

        ns_variables$df_final <- df_tenure
        y_max <- max(df_tenure$value, na.rm = TRUE)
        y_range <- if (is.finite(y_max)) c(0, y_max * 1.1) else NULL

        p_min <- build_tenure_bar(df_tenure, "Min", "#00C1FF", y_axis_title, y_range, TRUE)
        p_max <- build_tenure_bar(df_tenure, "Max", "#002244", y_axis_title, y_range, FALSE)

        if (is.null(p_min) || is.null(p_max)) {
          showNotification("No Data for this combination.", type = "error")
          return(NULL)
        }
        return(build_tenure_figure(p_min, p_max, country_label))
      }

      if (groupA == "payer") {
        if (group0 == "social" && identical(groupE, "occupational_risk")) {
          showNotification("Occupational risk contributions are only available in total.", type = "message")
          return(NULL)
        }
        df_src <- NULL
        if (group0 == "all") {
          df_src <- df_non_salary_payer
        } else if (group0 == "social") {
          df_src <- get_payer_data(groupE)
        } else if (group0 == "payroll_taxes") {
          df_src <- get_payer_data("payroll_taxes")
        }
        if (is.null(df_src) || !"min_max_payer" %in% names(df_src)) {
          showNotification("Data not available for this selection.", type = "error")
          return(NULL)
        }
        if (!"tenure" %in% names(df_src)) {
          showNotification("Tenure data not available for this selection.", type = "error")
          return(NULL)
        }
        df_tenure <- df_src %>%
          filter(
            country == country_sel,
            wage %in% wage_filter
          ) %>%
          mutate(
            Scenario = ifelse(grepl("_min$", min_max_payer), "Min", "Max"),
            Type = ifelse(grepl("employer", min_max_payer), "Employer", "Employee")
          ) %>%
          prepare_tenure_axis()

        if (nrow(df_tenure) == 0) {
          showNotification("No Data for this combination.", type = "error")
          return(NULL)
        }

        ns_variables$df_final <- df_tenure
        y_max <- df_tenure %>%
          group_by(Scenario, tenure_plot) %>%
          summarize(total = sum(value, na.rm = TRUE), .groups = "drop") %>%
          summarize(max_total = max(total, na.rm = TRUE), .groups = "drop") %>%
          pull(max_total)
        y_range <- if (is.finite(y_max)) c(0, y_max * 1.1) else NULL

        payer_colors <- c("Employer" = "#002244", "Employee" = "#00C1FF")
        p_min <- build_tenure_stack(
          df_tenure,
          "Min",
          payer_colors,
          c("Employee", "Employer"),
          y_axis_title,
          y_range,
          TRUE,
          TRUE
        )
        p_max <- build_tenure_stack(
          df_tenure,
          "Max",
          payer_colors,
          c("Employee", "Employer"),
          y_axis_title,
          y_range,
          FALSE,
          FALSE
        )
        if (is.null(p_min) || is.null(p_max)) {
          showNotification("No Data for this combination.", type = "error")
          return(NULL)
        }
        return(build_tenure_figure(p_min, p_max, country_label, show_legend = TRUE))
      }

      if (groupA == "component") {
        df_src <- NULL
        stack_order <- NULL
        colors <- NULL
        component_mode <- "stack"
        if (group0 == "all") {
          df_src <- df_non_salary_component
          stack_order <- component_stack_order
          colors <- component_palette
        } else if (group0 == "bonuses_and_benefits") {
          df_src <- get_component_data("bonuses_and_benefits")
          stack_order <- bonus_stack_order
          colors <- bonus_palette
        }
        if (is.null(df_src) || !"min_max_component" %in% names(df_src)) {
          showNotification("Data not available for this selection.", type = "error")
          return(NULL)
        }
        if (!"tenure" %in% names(df_src)) {
          showNotification("Tenure data not available for this selection.", type = "error")
          return(NULL)
        }
        df_tenure <- df_src %>%
          filter(
            country == country_sel,
            wage %in% wage_filter
          )
        if (group0 == "bonuses_and_benefits" && groupD != "all_bonuses") {
          component_key <- switch(
            groupD,
            ab = "annual_bonuses",
            pl = "paid_leave",
            up = "unemployment_protection",
            ob = "other_bonuses",
            NULL
          )
          if (!is.null(component_key)) {
            df_tenure <- df_tenure %>% filter(grepl(component_key, min_max_component))
          }
          component_mode <- "single"
        }

        if (nrow(df_tenure) == 0) {
          showNotification("No Data for this combination.", type = "error")
          return(NULL)
        }

        if (group0 == "all") {
          df_tenure <- df_tenure %>%
            mutate(
              Scenario = ifelse(grepl("_min$", min_max_component), "Min", "Max"),
              Type = dplyr::case_when(
                grepl("_pension", min_max_component) ~ "Pensions",
                grepl("_health", min_max_component) ~ "Health",
                grepl("_bonuses_and_benefits", min_max_component) ~ "Bonuses and Benefits",
                grepl("_occupational_risk", min_max_component) ~ "Occupational Risk",
                grepl("_payroll_taxes", min_max_component) ~ "Payroll Taxes",
                TRUE ~ NA_character_
              )
            )
        } else {
          df_tenure <- df_tenure %>%
            mutate(
              Scenario = ifelse(grepl("_min$", min_max_component), "Min", "Max"),
              Type = dplyr::case_when(
                grepl("annual_bonuses", min_max_component) ~ "Annual and other periodic bonuses",
                grepl("paid_leave", min_max_component) ~ "Paid Leave",
                grepl("unemployment_protection", min_max_component) ~ "Unemployment Protection",
                grepl("other_bonuses", min_max_component) ~ "Other bonuses",
                TRUE ~ NA_character_
              )
            )
        }

        df_tenure <- df_tenure %>% filter(!is.na(Scenario)) %>% prepare_tenure_axis()
        if (nrow(df_tenure) == 0) {
          showNotification("No Data for this combination.", type = "error")
          return(NULL)
        }

        ns_variables$df_final <- df_tenure
        if (component_mode == "single") {
          y_max <- max(df_tenure$value, na.rm = TRUE)
        } else {
          y_max <- df_tenure %>%
            group_by(Scenario, tenure_plot) %>%
            summarize(total = sum(value, na.rm = TRUE), .groups = "drop") %>%
            summarize(max_total = max(total, na.rm = TRUE), .groups = "drop") %>%
            pull(max_total)
        }
        y_range <- if (is.finite(y_max)) c(0, y_max * 1.1) else NULL

        if (component_mode == "single") {
          component_label <- unique(na.omit(df_tenure$Type))
          component_label <- if (length(component_label) > 0) component_label[1] else "Component"
          color <- colors[[component_label]]
          if (is.null(color)) {
            color <- "#00C1FF"
          }
          p_min <- build_tenure_bar(df_tenure, "Min", color, y_axis_title, y_range, TRUE)
          p_max <- build_tenure_bar(df_tenure, "Max", color, y_axis_title, y_range, FALSE)
          if (is.null(p_min) || is.null(p_max)) {
            showNotification("No Data for this combination.", type = "error")
            return(NULL)
          }
          return(build_tenure_figure(p_min, p_max, country_label))
        }

        p_min <- build_tenure_stack(
          df_tenure,
          "Min",
          colors,
          stack_order,
          y_axis_title,
          y_range,
          TRUE,
          TRUE
        )
        p_max <- build_tenure_stack(
          df_tenure,
          "Max",
          colors,
          stack_order,
          y_axis_title,
          y_range,
          FALSE,
          FALSE
        )
        if (is.null(p_min) || is.null(p_max)) {
          showNotification("No Data for this combination.", type = "error")
          return(NULL)
        }
        return(build_tenure_figure(p_min, p_max, country_label, show_legend = TRUE))
      }

      showNotification("Tenure view not available for this selection.", type = "message")
      return(NULL)
    }
    build_multicategory_stack <- function(df, colors, y_axis_title,
                                          stack_order = names(colors),
                                          legend_order = names(colors),
                                          legend_traceorder = "normal") {
      df <- df %>%
        filter(!is.na(Type), !is.na(value)) %>%
        mutate(
          Scenario = factor(Scenario, levels = c("Min", "Max")),
          wage = factor(wage, levels = wage_filter),
          Type = factor(Type, levels = stack_order)
        ) %>%
        arrange(Scenario, wage)

      fig <- plot_ly(type = "bar")
      for (type in levels(df$Type)) {
        sub <- df %>% filter(Type == type)
        if (nrow(sub) == 0) {
          next
        }
        hover_text <- NULL
        if ("hover_text" %in% names(sub)) {
          hover_text <- sub$hover_text
        }
        fig <- fig %>% add_trace(
          x = list(sub$Scenario, sub$wage),
          y = sub$value,
          name = type,
          marker = list(color = colors[[type]]),
          customdata = hover_text,
          hovertemplate = if (!is.null(hover_text)) {
            "%{x}<br>%{y:.2f}<br>%{customdata}<extra>%{fullData.name}</extra>"
          } else {
            NULL
          },
          hoverinfo = "y+name",
          legendrank = match(type, legend_order)
        )
      }

      fig <- fig %>%
        layout(
          barmode = "stack",
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor  = "rgba(0,0,0,0)",
          xaxis = list(
            title = "",
            type = "multicategory",
            showgrid = FALSE,
            zeroline = FALSE,
            showline = FALSE
          ),
          yaxis = list(
            title = y_axis_title,
            showgrid = FALSE,
            zeroline = FALSE,
            showline = FALSE
          ),
          legend = list(
            orientation = "h",
            x = 0.5,
            xanchor = "center",
            y = -0.25,
            traceorder = legend_traceorder
          )
        )

      fig <- apply_labor_plot_theme(fig)
      fig <- fig %>% layout(annotations = plot_footer_annotations())
      fig
    }
    build_component_subplot <- function(df, show_legend, y_axis_title, legend_order = component_legend_order) {
      fig <- plot_ly(type = "bar")
      for (type in component_stack_order) {
        sub <- df %>% filter(Type == type)
        if (nrow(sub) == 0) {
          next
        }
        fig <- fig %>% add_trace(
          x = ~Scenario,
          y = ~value,
          data = sub,
          name = type,
          marker = list(color = component_palette[[type]]),
          showlegend = show_legend,
          legendgroup = type,
          legendrank = match(type, legend_order),
          hoverinfo = "y+name"
        )
      }
      fig %>%
        layout(
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor  = "rgba(0,0,0,0)",
          xaxis = list(
            showgrid = FALSE,
            zeroline = FALSE,
            showline = FALSE,
            tickangle = 90
          ),
          yaxis = list(
            title = y_axis_title,
            showgrid = FALSE,
            zeroline = FALSE,
            showline = FALSE
          ),
          barmode = "stack"
        )
    }
    build_bonus_subplot <- function(df, show_legend, y_axis_title, legend_order = bonus_stack_order) {
      fig <- plot_ly(type = "bar")
      for (type in bonus_stack_order) {
        sub <- df %>% filter(Type == type)
        if (nrow(sub) == 0) {
          sub <- data.frame(
            Scenario = factor(c("Min", "Max"), levels = c("Min", "Max")),
            value = c(0, 0),
            hover_text = ""
          )
        } else {
          sub$Scenario <- factor(sub$Scenario, levels = c("Min", "Max"))
          sub$value[is.na(sub$value)] <- 0
          if (!"hover_text" %in% names(sub)) {
            sub$hover_text <- ""
          } else {
            sub$hover_text[is.na(sub$hover_text)] <- ""
          }
        }
        fig <- fig %>% add_trace(
          x = ~Scenario,
          y = ~value,
          data = sub,
          name = type,
          marker = list(color = bonus_palette[[type]]),
          customdata = ~hover_text,
          hovertemplate = "%{x}<br>%{y:.2f}<br>%{customdata}<extra>%{fullData.name}</extra>",
          showlegend = show_legend,
          legendgroup = type,
          legendrank = match(type, legend_order),
          hoverinfo = "y+name"
        )
      }
      fig %>%
        layout(
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor  = "rgba(0,0,0,0)",
          xaxis = list(
            showgrid = FALSE,
            zeroline = FALSE,
            showline = FALSE,
            tickangle = 90
          ),
          yaxis = list(
            title = y_axis_title,
            showgrid = FALSE,
            zeroline = FALSE,
            showline = FALSE
          ),
          barmode = "stack"
        )
    }

    prepare_payer_data <- function(df, aggregate = FALSE) {
      if (is.null(df)) {
        return(NULL)
      }
      df <- df %>%
        filter(wage %in% wage_filter)
      if (aggregate) {
        df <- df %>%
          group_by(country, wage, min_max_payer) %>%
          summarize(
            value = sum(value, na.rm = TRUE),
            has_value = any(!is.na(value)),
            .groups = "drop"
          ) %>%
          mutate(value = ifelse(has_value, value, NA_real_)) %>%
          select(-has_value)
      }
      df %>%
        apply_wage_panels() %>%
        mutate(
          group = ifelse(grepl("_min$", min_max_payer), "Min", "Max"),
          payer = ifelse(grepl("employer", min_max_payer), "Employer", "Employee"),
          group = factor(group, levels = c("Min", "Max"))
        ) %>%
        filter(!is.na(value))
    }

    prepare_all_payer_data <- function(df) {
      if (is.null(df)) {
        return(NULL)
      }
      if ("type_by_payer" %in% names(df)) {
        return(df %>%
          select(country, type_by_payer, value) %>%
          mutate(
            group = ifelse(grepl("_min$", type_by_payer), "Min", "Max"),
            payer = ifelse(grepl("^st_er", type_by_payer), "Employer", "Employee"),
            group = factor(group, levels = c("Min", "Max"))
          ))
      }
      if ("min_max_payer" %in% names(df)) {
        return(df %>%
          select(country, min_max_payer, value) %>%
          mutate(
            group = ifelse(grepl("_min$", min_max_payer), "Min", "Max"),
            payer = ifelse(grepl("employer", min_max_payer), "Employer", "Employee"),
            group = factor(group, levels = c("Min", "Max"))
          ))
      }
      NULL
    }

    prepare_all_component_data <- function(df) {
      if (is.null(df)) {
        return(NULL)
      }
      if ("type_by_component" %in% names(df)) {
        return(df %>%
          select(any_of(c("country", "wage", "type_by_component", "value"))) %>%
          mutate(
            group = ifelse(grepl("_min$", type_by_component), "Min", "Max"),
            payer = ifelse(grepl("_pension", type_by_component), "Pensions",
                           ifelse(grepl("_health", type_by_component), "Health",
                                  ifelse(grepl("_bonuses", type_by_component), "Bonuses and Benefits",
                                         ifelse(grepl("_occupational", type_by_component), "Occupational Risk", "Payroll Taxes")))),
            group = factor(group, levels = c("Min", "Max"))
          ))
      }
      if ("min_max_component" %in% names(df)) {
        return(df %>%
          select(any_of(c("country", "wage", "min_max_component", "value"))) %>%
          mutate(
            group = ifelse(grepl("_min$", min_max_component), "Min", "Max"),
            payer = dplyr::case_when(
              grepl("_pension", min_max_component) ~ "Pensions",
              grepl("_health", min_max_component) ~ "Health",
              grepl("_bonuses_and_benefits", min_max_component) ~ "Bonuses and Benefits",
              grepl("_occupational_risk", min_max_component) ~ "Occupational Risk",
              grepl("_payroll_taxes", min_max_component) ~ "Payroll Taxes",
              TRUE ~ "Payroll Taxes"
            ),
            group = factor(group, levels = c("Min", "Max"))
          ))
      }
      NULL
    }

    plot_payer_subplots <- function(df, y_axis_title) {
      if (is.null(df) || nrow(df) == 0) {
        showNotification("No Data for this combination.", type = "error")
        return(NULL)
      }
      country_levels <- panel_order()
      if (is.null(country_levels) || length(country_levels) == 0) {
        country_levels <- unique(df$country)
      }
      df <- df %>%
        mutate(
          country = factor(country, levels = country_levels),
          Type = factor(payer, levels = c("Employee", "Employer")),
          Scenario = factor(group, levels = c("Min", "Max")),
          Scenario = as.character(Scenario)
        ) %>%
        arrange(country)

      colors <- c("Employer" = "#002244", "Employee" = "#00C1FF")
      paises <- unique(df$country)
      plot_list <- list()

      for (i in seq_along(paises)) {
        pais <- paises[i]
        data_pais <- df %>% filter(country == pais)

        if (nrow(data_pais) == 0) next

        show_legend <- i == 1

        p <- plot_ly(
          data = data_pais,
          x = ~Scenario,
          y = ~value,
          type = "bar",
          color = ~Type,
          colors = colors,
          legendgroup = ~Type,
          showlegend = show_legend,
          hoverinfo = "y+name"
        ) %>%
          layout(
            paper_bgcolor = "rgba(0,0,0,0)",
            plot_bgcolor  = "rgba(0,0,0,0)",
            xaxis = list(
              title = pais,
              type = "category",
              categoryorder = "array",
              categoryarray = c("Min", "Max"),
              tickvals = c("Min", "Max"),
              ticktext = c("Min", "Max"),
              showgrid = FALSE,
              zeroline = FALSE,
              showline = FALSE,
              tickangle = 90
            ),
            yaxis = list(
              title = ifelse(i == 1, y_axis_title, ""),
              showgrid = FALSE,
              zeroline = FALSE,
              showline = FALSE
            ),
            barmode = "stack"
          )

        plot_list[[i]] <- p
      }

      n_plots <- length(plot_list)
      fig <- subplot(
        plot_list,
        nrows = 1,
        shareY = TRUE,
        titleX = TRUE,
        widths = rep(1 / n_plots, n_plots),
        margin = 0.01
      ) %>%
        layout(
          title = "",
          legend = list(
            orientation = "h",
            x = 0.5,
            xanchor = "center",
            y = -0.25
          ),
          margin = list(
            l = 70,
            r = 30,
            b = 110,
            t = 20
          )
        )

      apply_labor_plot_theme(fig)
    }

    # ========================================================================
    # COMPARE WAGES CASES
    # ========================================================================

    if (compare_wages && groupA == "total") {
      if (length(ns_variables$country_sel) != 1 || "All" %in% ns_variables$country_sel) {
        return(NULL)
      }

      if (group0 == "all") {
        df <- df_non_salary %>%
          dplyr::filter(
            wage %in% wage_filter,
            country == ns_variables$country_sel
          ) %>%
          mutate(
            Scenario = ifelse(type == "t_min", "Min", "Max")
          ) %>%
          select(wage, Scenario, value)
      } else if (group0 == "social") {
        df <- get_group_data(groupE)
        if (is.null(df)) {
          showNotification("Data not available for this selection.", type = "error")
          return(NULL)
        }
        df <- df %>%
          dplyr::filter(
            wage %in% wage_filter,
            country == ns_variables$country_sel
          ) %>%
          mutate(
            Scenario = ifelse(grepl("_min$", min_max_total), "Min", "Max")
          ) %>%
          select(wage, Scenario, value)
      } else {
        # OPTIMIZADO: get_group_data() en lugar de readRDS()
        df <- get_group_data(group0)
        if (is.null(df)) {
          showNotification("Data not available for this selection.", type = "error")
          return(NULL)
        }
        df <- df %>%
          dplyr::filter(
            wage %in% wage_filter,
            country == ns_variables$country_sel
          ) %>%
          mutate(
            Scenario = ifelse(grepl("_min$", min_max_total), "Min", "Max")
          ) %>%
          select(wage, Scenario, value)
      }

      if (nrow(df) == 0) {
        showNotification("No Data for this combination.", type = "error")
        return(NULL)
      }

      df <- df %>%
        mutate(
          wage = factor(wage, levels = wage_filter),
          Scenario = factor(Scenario, levels = c("Min", "Max"))
        ) %>%
        arrange(Scenario, wage)

      if (group0 == "social") {
        social_colors <- resolve_social_colors(groupE)
        fig <- plot_ly(
          data = df,
          x = list(df$Scenario, df$wage),
          y = ~value,
          type = "bar",
          color = ~Scenario,
          colors = social_colors,
          showlegend = FALSE
        ) %>%
          layout(
            paper_bgcolor = "rgba(0,0,0,0)",
            plot_bgcolor  = "rgba(0,0,0,0)",
            xaxis = list(
              title = "",
              type = "multicategory",
              showgrid = FALSE,
              zeroline = FALSE,
              showline = FALSE
            ),
            yaxis = list(
              title = y_axis_title,
              showgrid = FALSE,
              zeroline = FALSE,
              showline = FALSE
            )
          )
      } else {
        fig <- plot_ly(
          data = df,
          x = list(df$Scenario, df$wage),
          y = ~value,
          type = "bar",
          marker = list(color = "#002244"),
          showlegend = FALSE
        ) %>%
          layout(
            paper_bgcolor = "rgba(0,0,0,0)",
            plot_bgcolor  = "rgba(0,0,0,0)",
            xaxis = list(
              title = "",
              type = "multicategory",
              showgrid = FALSE,
              zeroline = FALSE,
              showline = FALSE
            ),
            yaxis = list(
              title = y_axis_title,
              showgrid = FALSE,
              zeroline = FALSE,
              showline = FALSE
            )
          )
      }

      fig <- apply_labor_plot_theme(fig)
      fig <- fig %>% layout(annotations = plot_footer_annotations())
      return(fig)
    }

    if (compare_wages && groupA == "payer" && group0 == "all") {
      if (length(ns_variables$country_sel) != 1 || "All" %in% ns_variables$country_sel) {
        return(NULL)
      }

      df <- df_non_salary_payer %>%
        dplyr::filter(
          wage %in% wage_filter,
          country == ns_variables$country_sel
        ) %>%
        mutate(
          Scenario = ifelse(grepl("_min$", min_max_payer), "Min", "Max"),
          Type = ifelse(grepl("employer", min_max_payer), "Employer", "Employee")
        ) %>%
        select(wage, Scenario, Type, value)

      if (nrow(df) == 0) {
        showNotification("No Data for this combination.", type = "error")
        return(NULL)
      }

      colors <- c("Employer" = "#002244", "Employee" = "#00C1FF")
      return(build_multicategory_stack(
        df,
        colors,
        y_axis_title,
        stack_order = c("Employee", "Employer"),
        legend_order = c("Employee", "Employer")
      ))
    }

    if (compare_wages && groupA == "payer" && group0 %in% c("social", "payroll_taxes")) {
      if (group0 == "social" && groupE == "occupational_risk") {
        showNotification("Occupational risk contributions are only available in total.", type = "message")
        return(NULL)
      }
      if (length(ns_variables$country_sel) != 1 || "All" %in% ns_variables$country_sel) {
        return(NULL)
      }

      payer_key <- if (group0 == "social") {
        groupE
      } else {
        "payroll_taxes"
      }
      df_raw <- get_payer_data(payer_key)
      if (is.null(df_raw)) {
        showNotification("Data not available for this selection.", type = "error")
        return(NULL)
      }
      df_raw <- df_raw %>% filter(country == ns_variables$country_sel)

      df_long <- prepare_payer_data(
        df_raw,
        aggregate = identical(payer_key, "payroll_taxes")
      )
      if (is.null(df_long) || nrow(df_long) == 0) {
        showNotification("No Data for this combination.", type = "error")
        return(NULL)
      }

      df <- df_long %>%
        transmute(
          wage,
          Scenario = group,
          Type = payer,
          value
        )

      colors <- c("Employer" = "#002244", "Employee" = "#00C1FF")
      return(build_multicategory_stack(
        df,
        colors,
        y_axis_title,
        stack_order = c("Employee", "Employer"),
        legend_order = c("Employee", "Employer")
      ))
    }

    if (compare_wages && groupA == "component" && group0 == "all") {
      if (length(ns_variables$country_sel) != 1 || "All" %in% ns_variables$country_sel) {
        return(NULL)
      }

      df <- df_non_salary_component %>%
        dplyr::filter(
          wage %in% wage_filter,
          country == ns_variables$country_sel
        ) %>%
        apply_wage_panels()
      df <- prepare_all_component_data(df)
      if (!is.null(df)) {
        df <- df %>%
          transmute(
            wage,
            Scenario = group,
            Type = payer,
            value
          )
      }

      if (is.null(df) || nrow(df) == 0) {
        showNotification("No Data for this combination.", type = "error")
        return(NULL)
      }

      return(build_multicategory_stack(
        df,
        component_palette,
        y_axis_title,
        stack_order = component_stack_order,
        legend_order = component_legend_order
      ))
    }

    if (compare_wages && groupA == "component" && group0 == "bonuses_and_benefits") {
      if (length(ns_variables$country_sel) != 1 || "All" %in% ns_variables$country_sel) {
        return(NULL)
      }

      # OPTIMIZADO: get_component_data() en lugar de readRDS()
      df <- get_component_data("bonuses_and_benefits")
      if (is.null(df)) {
        showNotification("Data not available.", type = "error")
        return(NULL)
      }
      df <- df %>%
        dplyr::filter(
          wage %in% wage_filter,
          country == ns_variables$country_sel
        ) %>%
        mutate(
          Scenario = ifelse(grepl("_min$", min_max_component), "Min", "Max"),
          Type = dplyr::case_when(
            grepl("annual_bonuses", min_max_component) ~ "Annual and other periodic bonuses",
            grepl("paid_leave", min_max_component) ~ "Paid Leave",
            grepl("unemployment_protection", min_max_component) ~ "Unemployment Protection",
            grepl("other_bonuses", min_max_component) ~ "Other bonuses",
            TRUE ~ NA_character_
          )
        ) %>%
        select(country, wage, Scenario, Type, value)

      if (groupD != "all_bonuses") {
        component_label <- switch(
          groupD,
          ab = "Annual and other periodic bonuses",
          pl = "Paid Leave",
          up = "Unemployment Protection",
          ob = "Other bonuses",
          groupD
        )
        df <- df %>% filter(Type == component_label)
      }

      df <- df %>%
        left_join(
          bonus_hover_lookup,
          by = c("country", "wage", "Scenario" = "group", "Type" = "Type")
        )

      if (nrow(df) == 0) {
        showNotification("No Data for this combination.", type = "error")
        return(NULL)
      }

      colors <- c(
        "Annual and other periodic bonuses" = "#002244",
        "Paid Leave" = "#8EA2BF",
        "Unemployment Protection" = "#B9BAB5",
        "Other bonuses" = "#6F6779"
      )
      return(build_multicategory_stack(df, colors, y_axis_title))
    }

    if (compare_wages && groupA == "component" && group0 == "social") {
      if (length(ns_variables$country_sel) != 1 || "All" %in% ns_variables$country_sel) {
        return(NULL)
      }

      # OPTIMIZADO: get_group_data() en lugar de readRDS()
      df <- get_group_data(groupE)
      if (is.null(df)) {
        showNotification("Data not available.", type = "error")
        return(NULL)
      }
      df <- df %>%
        dplyr::filter(
          wage %in% wage_filter,
          country == ns_variables$country_sel
        ) %>%
        mutate(
          Scenario = ifelse(grepl("_min$", min_max_total), "Min", "Max")
        ) %>%
        select(wage, Scenario, value)

      if (nrow(df) == 0) {
        showNotification("No Data for this combination.", type = "error")
        return(NULL)
      }

      df <- df %>%
        mutate(
          wage = factor(wage, levels = wage_filter),
          Scenario = factor(Scenario, levels = c("Min", "Max"))
        ) %>%
        arrange(Scenario, wage)

      fig <- plot_ly(
        data = df,
        x = list(df$Scenario, df$wage),
        y = ~value,
        type = "bar",
        marker = list(color = "#002244"),
        showlegend = FALSE
      ) %>%
        layout(
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor  = "rgba(0,0,0,0)",
          xaxis = list(
            title = "",
            type = "multicategory",
            showgrid = FALSE,
            zeroline = FALSE,
            showline = FALSE
          ),
          yaxis = list(
            title = y_axis_title,
            showgrid = FALSE,
            zeroline = FALSE,
            showline = FALSE
          )
        )

      fig <- apply_labor_plot_theme(fig)
      fig <- fig %>% layout(annotations = plot_footer_annotations())
      return(fig)
    }
    
    if (groupA == "payer" && group0 %in% c("social", "payroll_taxes")) {
      if (group0 == "social" && groupE == "occupational_risk") {
        showNotification("Occupational risk contributions are only available in total.", type = "message")
        return(NULL)
      }
      payer_key <- if (group0 == "social") {
        groupE
      } else {
        "payroll_taxes"
      }
      df_raw <- get_payer_data(payer_key)
      if (is.null(df_raw)) {
        showNotification("Data not available for this selection.", type = "error")
        return(NULL)
      }
      if (length(ns_variables$country_sel) > 1) {
        if ("All" %in%  ns_variables$country_sel) {
          showNotification("Please select only countries.", type = "error")
          return(NULL)
        }
        df_raw <- df_raw %>% filter(country %in% ns_variables$country_sel)
      } else if (length(ns_variables$country_sel) == 1 && ns_variables$country_sel != "All") {
        df_raw <- df_raw %>% filter(country == ns_variables$country_sel)
      } else {
        ns_variables$countries <- c("All", unique(df_raw$country))
      }
      if (is_cross_country && group0 == "payroll_taxes") {
        candidate_countries <- if (length(ns_variables$country_sel) == 0 ||
          "All" %in% ns_variables$country_sel) {
          sources$countries
        } else {
          ns_variables$country_sel
        }
        df_missing_src <- df_raw %>% filter(wage %in% wage_filter)
        df_missing_src <- filter_missing_payroll(df_missing_src, candidate_countries)
        missing <- ns_variables$missing_payroll_countries
        if (length(missing) > 0) {
          df_raw <- df_raw %>% filter(!country %in% missing)
        }
      }

      df_long <- prepare_payer_data(
        df_raw,
        aggregate = identical(payer_key, "payroll_taxes")
      )
      if (is.null(df_long) || nrow(df_long) == 0) {
        showNotification("No Data for this combination.", type = "error")
        return(NULL)
      }

      return(plot_payer_subplots(df_long, y_axis_title))
    }
    
    
    # ---- ALL and Total ----
    
    if (group0=="all" & groupA == "total" & length(ns_variables$country_sel)==1) {
      
      if(ns_variables$country_sel=="All"){
        
        # Filtering total non salary
        df <- df_non_salary %>%
          dplyr::filter(
            wage %in% wage_filter
          ) %>%
          apply_wage_panels()
        
        if (nrow(df) == 0) {
          showNotification("No Data for this combination.", type = "error")
          return(NULL)
        }
        
        df_wide <- df %>%
          tidyr::pivot_wider(
            names_from = type,
            values_from = value
          ) %>%
          arrange(t_min) %>%
          mutate(country = factor(country, levels = country))
        
        ns_variables$order_country <- unique(as.character(df_wide$country))
        
        df_mm <- df_wide %>%
          tidyr::pivot_longer(
            cols = c(t_min, t_max),
            names_to = "Scenario",
            values_to = "value"
          ) %>%
          mutate(
            Scenario = ifelse(Scenario == "t_min", "Min", "Max"),
            Scenario = factor(Scenario, levels = c("Min", "Max")),
            country  = factor(country, levels = panel_order())
          )
        
        ns_variables$df_final=df_mm
        scenario_colors <- if (group0 == "social") {
          resolve_social_colors(groupE)
        } else {
          c("Min" = "#00C1FF", "Max" = "#002244")
        }

        paises <- unique(df_mm$country)
        plot_list <- list()
        
        for (i in seq_along(paises)) {
          
          pais <- paises[i]
          data_pais <- df_mm %>% filter(country == pais)
          
          p <- plot_ly(
            data = data_pais,
            x = ~Scenario,
            y = ~value,
            type = "bar",
            color = ~Scenario,
            colors = scenario_colors,
            showlegend = FALSE
          ) %>%
            layout(
              barmode = "stack",   
              
              paper_bgcolor = "rgba(0,0,0,0)",
              plot_bgcolor  = "rgba(0,0,0,0)",
              
              xaxis = list(
                title = pais,
                showgrid = FALSE,
                zeroline = FALSE,
                showline = FALSE,
                tickangle = 90
              ),
              
              yaxis = list(
                title = ifelse(i == 1, y_axis_title, ""),
                showgrid = FALSE,
                zeroline = FALSE,
                showline = FALSE
              )
            )
          
          plot_list[[i]] <- p
        }
        
        fig <- subplot(
          plot_list,
          nrows = 1,
          shareY = TRUE,
          titleX = TRUE,
          widths = rep(1 / length(plot_list), length(plot_list)),
          margin = 0.01
        ) %>%
          layout(
            margin = list(l = 70, r = 30, b = 110, t = 20),
            paper_bgcolor = "rgba(0,0,0,0)",
            plot_bgcolor  = "rgba(0,0,0,0)"
          )
        
        return(apply_labor_plot_theme(fig))
      }
      
      else{
        df <- df_non_salary %>%
          filter(
            wage %in% wage_filter,
            country == ns_variables$country_sel
          ) %>%
          apply_wage_panels()
        
        if (nrow(df) == 0) {
          showNotification("No Data for this combination.", type = "error")
          return(NULL)
        }
        
        df_wide <- df %>%
          tidyr::pivot_wider(
            names_from = type,
            values_from = value
          ) %>%
          arrange(t_min) %>%
          mutate(country = factor(country, levels = country))
        
        df_mm <- df_wide %>%
          tidyr::pivot_longer(
            cols = c(t_min, t_max),
            names_to = "Scenario",
            values_to = "value"
          ) %>%
          mutate(
            Scenario = ifelse(Scenario == "t_min", "Min", "Max"),
            Scenario = factor(Scenario, levels = c("Min", "Max")),
            country  = factor(country, levels = panel_order())
          )
        
        ns_variables$df_final=df_mm
        scenario_colors <- if (group0 == "social") {
          resolve_social_colors(groupE)
        } else {
          c("Min" = "#00C1FF", "Max" = "#002244")
        }

        paises <- unique(df_mm$country)
        plot_list <- list()
        
        for (i in seq_along(paises)) {
          
          pais <- paises[i]
          data_pais <- df_mm %>% filter(country == pais)
          
          p <- plot_ly(
            data = data_pais,
            x = ~Scenario,
            y = ~value,
            type = "bar",
            color = ~Scenario,
            colors = scenario_colors,
            showlegend = FALSE
          ) %>%
            layout(
              barmode = "stack",
              
              paper_bgcolor = "rgba(0,0,0,0)",
              plot_bgcolor  = "rgba(0,0,0,0)",
              
              xaxis = list(
                title = pais,
                showgrid = FALSE,
                zeroline = FALSE,
                showline = FALSE,
                tickangle = 90
              ),
              
              yaxis = list(
                title = ifelse(i == 1, y_axis_title, ""),
                showgrid = FALSE,
                zeroline = FALSE,
                showline = FALSE
              )
            )
          
          plot_list[[i]] <- p
        }
        
        fig <- subplot(
          plot_list,
          nrows = 1,
          shareY = TRUE,
          titleX = TRUE,
          widths = rep(1 / length(plot_list), length(plot_list)),
          margin = 0.01
        ) %>%
          layout(
            margin = list(l = 70, r = 30, b = 110, t = 20),
            paper_bgcolor = "rgba(0,0,0,0)",
            plot_bgcolor  = "rgba(0,0,0,0)"
          )
        
        return(apply_labor_plot_theme(fig))
      }
      
    }
    
    if (group0=="all" & groupA == "total" & length(ns_variables$country_sel)>1) {
      if ("All" %in%  ns_variables$country_sel) {
        showNotification("Please select only countries.", type = "error")
        return(NULL)
      }
      ns_variables$countries=c("All",unique(df_non_salary$country))
      # Filtering total non salary
      df <- df_non_salary %>%
        filter(
          wage %in% wage_filter,
          country %in% ns_variables$country_sel
        ) %>%
        apply_wage_panels()
      
      if (nrow(df) == 0) {
        showNotification("No Data for this combination.", type = "error")
        return(NULL)
      }
      
      df_wide <- df %>%
        tidyr::pivot_wider(
          names_from = type,
          values_from = value
        ) %>%
        arrange(t_min) %>%
        mutate(country = factor(country, levels = country))
      
      ns_variables$order_country <- unique(as.character(df_wide$country))
      
      df_mm <- df_wide %>%
        tidyr::pivot_longer(
          cols = c(t_min, t_max),
          names_to = "Scenario",
          values_to = "value"
        ) %>%
        mutate(
          Scenario = ifelse(Scenario == "t_min", "Min", "Max"),
          Scenario = factor(Scenario, levels = c("Min", "Max")),
          country  = factor(country, levels = panel_order())
        )
      
      ns_variables$df_final=df_mm
      
      paises <- unique(df_mm$country)
      plot_list <- list()
      
      for (i in seq_along(paises)) {
        
        pais <- paises[i]
        data_pais <- df_mm %>% filter(country == pais)
        
        p <- plot_ly(
          data = data_pais,
          x = ~Scenario,
          y = ~value,
          type = "bar",
          color = ~Scenario,
          colors = c("Min" = "#00C1FF", "Max" = "#002244"),
          showlegend = FALSE
        ) %>%
          layout(
            barmode = "stack",
            
            paper_bgcolor = "rgba(0,0,0,0)",
            plot_bgcolor  = "rgba(0,0,0,0)",
            
            xaxis = list(
              title = pais,
              showgrid = FALSE,
              zeroline = FALSE,
              showline = FALSE,
              tickangle = 90
            ),
            
            yaxis = list(
              title = ifelse(i == 1, y_axis_title, ""),
              showgrid = FALSE,
              zeroline = FALSE,
              showline = FALSE
            )
          )
        
        plot_list[[i]] <- p
      }
      
      fig <- subplot(
        plot_list,
        nrows = 1,
        shareY = TRUE,
        titleX = TRUE,
        widths = rep(1 / length(plot_list), length(plot_list)),
        margin = 0.01
      ) %>%
        layout(
          margin = list(l = 70, r = 30, b = 110, t = 20),
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor  = "rgba(0,0,0,0)"
        )
      
      return(apply_labor_plot_theme(fig))
    }
    
    # ---- ALL By Payer ----
    
    if (group0=="all" & groupA == "payer" & length(ns_variables$country_sel)==1) {
      
      ns_variables$countries=c("All",unique(df_non_salary$country))
      
      if(ns_variables$country_sel=="All"){
        df_long <- df_non_salary_payer %>%
          filter(
            wage %in% wage_filter
          ) %>%
          apply_wage_panels()
        df_long <- prepare_all_payer_data(df_long)
        
        if (is.null(df_long) || nrow(df_long) == 0) {
          showNotification("No Data for this combination.", type = "error")
          return(NULL)
        }

        df_long <- df_long %>%
          mutate(country = factor(country, levels = panel_order())) %>% 
          arrange(country)
        
        df <- df_long
        df$Type <- factor(df$payer, levels = c("Employee", "Employer"))
        df$Scenario <- factor(df$group, levels = c("Min", "Max"))
        
        colors <- c("Employer" = "#002244", "Employee" = "#00C1FF")
        
        paises <- unique(df$country)
        plot_list <- list()
        
        for (i in seq_along(paises)) {
          
          pais <- paises[i]
          data_pais <- df %>% filter(country == pais)
          
          if (nrow(data_pais) == 0) next
          
          show_legend <- i == 1
          
          p <- plot_ly(
            data = data_pais,
            x = ~Scenario,
            y = ~value,
            type = "bar",
            color = ~Type,
            colors = colors,
            legendgroup = ~Type,
            showlegend = show_legend,
            hoverinfo = "y+name"
          ) %>%
            layout(
              paper_bgcolor = "rgba(0,0,0,0)",
              plot_bgcolor  = "rgba(0,0,0,0)",
              xaxis = list(
                title = pais,
                showgrid = FALSE,
                zeroline = FALSE,
                showline = FALSE,
                tickangle = 90
              ),
              
              yaxis = list(
                title = ifelse(i == 1, y_axis_title, ""),
                showgrid = FALSE,
                zeroline = FALSE,
                showline = FALSE
              ),
              
              barmode = "stack"
            )
          
          plot_list[[i]] <- p
        }
        
        n_plots <- length(plot_list)
        fig <- subplot(
          plot_list,
          nrows = 1,
          shareY = TRUE,
          titleX = TRUE,
          widths = rep(1 / n_plots, n_plots), 
          margin = 0.01
        ) %>%
          layout(
            title = "",
            
            legend = list(
              orientation = "h",
              x = 0.5,
              xanchor = "center",
              y = -0.25
            ),
            
            margin = list(
              l = 70,
              r = 30,
              b = 110,
              t = 20
            )
          )
        return(apply_labor_plot_theme(fig))
      }
      
      else{
        
        df_long <- df_non_salary_payer %>%
          filter(
            wage %in% wage_filter,
            country== ns_variables$country_sel
          ) %>%
          apply_wage_panels()
        df_long <- prepare_all_payer_data(df_long)

        if (is.null(df_long) || nrow(df_long) == 0) {
          showNotification("No Data for this combination.", type = "error")
          return(NULL)
        }
        
        
        df <- df_long
        df$Type <- factor(df$payer, levels = c("Employee", "Employer"))
        df$Scenario <- factor(df$group, levels = c("Min", "Max"))
        
        colors <- c("Employer" = "#002244", "Employee" = "#00C1FF")
        
        paises <- unique(df$country)
        plot_list <- list()
        
        for (i in seq_along(paises)) {
          
          pais <- paises[i]
          data_pais <- df %>% filter(country == pais)
          
          if (nrow(data_pais) == 0) next
          
          show_legend <- i == 1
          
          p <- plot_ly(
            data = data_pais,
            x = ~Scenario,
            y = ~value,
            type = "bar",
            color = ~Type,
            colors = colors,
            legendgroup = ~Type,
            showlegend = show_legend,
            hoverinfo = "y+name"
          ) %>%
            layout(
              paper_bgcolor = "rgba(0,0,0,0)",
              plot_bgcolor  = "rgba(0,0,0,0)",
              
              xaxis = list(
                title = pais,
                showgrid = FALSE,
                zeroline = FALSE,
                showline = FALSE,
                tickangle = 90
              ),
              
              yaxis = list(
                title = ifelse(i == 1, y_axis_title, ""),
                showgrid = FALSE,
                zeroline = FALSE,
                showline = FALSE
              ),
              
              barmode = "stack"
            )
          
          plot_list[[i]] <- p
        }
        
        n_plots <- length(plot_list)
        fig <- subplot(
          plot_list,
          nrows = 1,
          shareY = TRUE,
          titleX = TRUE,
          widths = rep(1 / n_plots, n_plots), 
          margin = 0.01
        ) %>%
          layout(
            title = "",
            
            legend = list(
              orientation = "h",
              x = 0.5,
              xanchor = "center",
              y = -0.25
            ),
            
            margin = list(
              l = 70,
              r = 30,
              b = 110,
              t = 20
            )
          )
        return(apply_labor_plot_theme(fig))
      }
      
    }
    
    if (group0=="all" & groupA == "payer" & length(ns_variables$country_sel)>1) {
      
      ns_variables$countries=c("All",unique(df_non_salary$country))
      
      if ("All" %in%  ns_variables$country_sel) {
        showNotification("Please select only countries.", type = "error")
        return(NULL)
      }
      
      df_long <- df_non_salary_payer %>%
        filter(
          wage %in% wage_filter,
          country %in% ns_variables$country_sel
        ) %>%
        apply_wage_panels()
      df_long <- prepare_all_payer_data(df_long)
      
      if (is.null(df_long) || nrow(df_long) == 0) {
        showNotification("No Data for this combination.", type = "error")
        return(NULL)
      }
      
      df_long <- df_long %>%
        mutate(country = factor(country, levels = panel_order())) %>% 
        arrange(country)
      
      
      df <- df_long
      df$Type <- factor(df$payer, levels = c("Employee", "Employer"))
      df$Scenario <- factor(df$group, levels = c("Min", "Max"))
      
      colors <- c("Employer" = "#002244", "Employee" = "#00C1FF")
      
      paises <- unique(df$country)
      plot_list <- list()
      
      ns_variables$df_final=df
      for (i in seq_along(paises)) {
        pais <- paises[i]
        data_pais <- df %>% filter(country == pais)
        
        show_legend <- ifelse(i == 1, TRUE, FALSE)
        
        p <- plot_ly(data_pais, x = ~Scenario, y = ~value, type = 'bar',
                     color = ~Type, colors = colors, legendgroup = ~Type,
                     showlegend = show_legend, text = ~value,
                     hoverinfo = "text+y+name") %>%
          layout(
            paper_bgcolor = "rgba(0,0,0,0)",   
            plot_bgcolor  = "rgba(0,0,0,0)", 
            xaxis = list(
              title = pais,
              showgrid = FALSE,
              zeroline = FALSE,
              showline = FALSE
            ),
            yaxis = list(
              title = ifelse(i == 1, y_axis_title, ""),
              range = c(0, 140),
              showgrid = FALSE,
              zeroline = FALSE,
              showline = FALSE
            ),
            barmode = 'stack'
          )
        
        plot_list[[i]] <- p
      }
      
      n_plots <- length(plot_list)
      fig <- subplot(
        plot_list,
        nrows = 1,
        shareY = TRUE,
        titleX = TRUE,
        widths = rep(1 / n_plots, n_plots), 
        margin = 0.01
      ) %>%
        layout(
          title = "",
          
          legend = list(
            orientation = "h",
            x = 0.5,
            xanchor = "center",
            y = -0.25
          ),
          
          margin = list(
            l = 70,
            r = 30,
            b = 110,
            t = 20
          )
        )
      
      return(apply_labor_plot_theme(fig))
    }
    
    # ---- ALL by Component ----

    if (group0=="all" & groupA == "component" & length(ns_variables$country_sel)==1) {
      ns_variables$countries=c("All",unique(df_non_salary$country))
      if(ns_variables$country_sel=="All"){
        df_long <- df_non_salary_component %>%
          filter(
            wage %in% wage_filter
          ) %>%
          apply_wage_panels()
        df_long <- prepare_all_component_data(df_long)
        
        if (is.null(df_long) || nrow(df_long) == 0) {
          showNotification("No Data for this combination.", type = "error")
          return(NULL)
        }

        df_long <- df_long %>%
          mutate(country = factor(country, levels = panel_order())) %>% 
          arrange(country)
        
        df <- df_long
        df$Type <- factor(df$payer, levels = component_stack_order)
        df$Scenario <- factor(df$group, levels = c("Min", "Max"))
        
        paises <- unique(df$country)
        plot_list <- list()
        
        for (i in seq_along(paises)) {
          
          pais <- paises[i]
          data_pais <- df %>% filter(country == pais)
          
          if (nrow(data_pais) == 0) next
          
          show_legend <- i == 1
          
          p <- build_component_subplot(
            data_pais,
            show_legend = show_legend,
            y_axis_title = ifelse(i == 1, y_axis_title, ""),
            legend_order = component_legend_order
          ) %>%
            layout(
              xaxis = list(
                title = pais,
                showgrid = FALSE,
                zeroline = FALSE,
                showline = FALSE,
                tickangle = 90
              )
            )
          
          plot_list[[i]] <- p
        }
        
        n_plots <- length(plot_list)
        fig <- subplot(
          plot_list,
          nrows = 1,
          shareY = TRUE,
          titleX = TRUE,
          widths = rep(1 / n_plots, n_plots), 
          margin = 0.01
        ) %>%
          layout(
            title = "",
            
            legend = list(
              orientation = "h",
              x = 0.5,
              xanchor = "center",
              y = -0.25,
              traceorder = "normal"
            ),
            
            margin = list(
              l = 70,
              r = 30,
              b = 110,
              t = 20
            )
          )
        return(apply_labor_plot_theme(fig))
      }
      else{
        df_long <- df_non_salary_component %>%
          filter(
            wage %in% wage_filter,
            country==ns_variables$country_sel
          ) %>%
          apply_wage_panels()
        df_long <- prepare_all_component_data(df_long)

        if (is.null(df_long) || nrow(df_long) == 0) {
          showNotification("No Data for this combination.", type = "error")
          return(NULL)
        }
        
        
        df <- df_long
        df$Type <- factor(df$payer, levels = component_stack_order)
        df$Scenario <- factor(df$group, levels = c("Min", "Max"))
        
        paises <- unique(df$country)
        plot_list <- list()
        
        for (i in seq_along(paises)) {
          
          pais <- paises[i]
          data_pais <- df %>% filter(country == pais)
          
          if (nrow(data_pais) == 0) next
          
          show_legend <- i == 1
          
          p <- build_component_subplot(
            data_pais,
            show_legend = show_legend,
            y_axis_title = ifelse(i == 1, y_axis_title, ""),
            legend_order = component_legend_order
          ) %>%
            layout(
              xaxis = list(
                title = pais,
                showgrid = FALSE,
                zeroline = FALSE,
                showline = FALSE,
                tickangle = 90
              )
            )
          
          plot_list[[i]] <- p
        }
        
        n_plots <- length(plot_list)
        fig <- subplot(
          plot_list,
          nrows = 1,
          shareY = TRUE,
          titleX = TRUE,
          widths = rep(1 / n_plots, n_plots), 
          margin = 0.01
        ) %>%
          layout(
            title = "",
            
            legend = list(
              orientation = "h",
              x = 0.5,
              xanchor = "center",
              y = -0.25,
              traceorder = "normal"
            ),
            
            margin = list(
              l = 70,
              r = 30,
              b = 110,
              t = 20
            )
          )
        return(apply_labor_plot_theme(fig))
      }
    }
    
    if (group0=="all" & groupA == "component" & length(ns_variables$country_sel)>1) {
      ns_variables$countries=c("All",unique(df_non_salary$country))
      
      if ("All" %in%  ns_variables$country_sel) {
        showNotification("Please select only countries.", type = "error")
        return(NULL)
      }
      df_long <- df_non_salary_component %>%
        filter(
          wage %in% wage_filter,
          country %in% ns_variables$country_sel 
        ) %>%
        apply_wage_panels()
      df_long <- prepare_all_component_data(df_long)
      
      if (is.null(df_long) || nrow(df_long) == 0) {
        showNotification("No Data for this combination.", type = "error")
        return(NULL)
      }
      
      
      df <- df_long
      df$Type <- factor(df$payer, levels = component_stack_order)
      df$Scenario <- factor(df$group, levels = c("Min", "Max"))
      
      paises <- unique(df$country)
      plot_list <- list()
      
      ns_variables$df_final=df
      for (i in seq_along(paises)) {
        pais <- paises[i]
        data_pais <- df %>% filter(country == pais)
        
        show_legend <- ifelse(i == 1, TRUE, FALSE)
        
        p <- build_component_subplot(
          data_pais,
          show_legend = show_legend,
          y_axis_title = ifelse(i == 1, y_axis_title, ""),
          legend_order = component_legend_order
        ) %>%
          layout(
            xaxis = list(
              title = pais,
              showgrid = FALSE,
              zeroline = FALSE,
              showline = FALSE
            ),
            yaxis = list(
              title = ifelse(i == 1, y_axis_title, ""),
              range = c(0, 140),
              showgrid = FALSE,
              zeroline = FALSE,
              showline = FALSE
            )
          )
        
        plot_list[[i]] <- p
      }
      
      n_plots <- length(plot_list)
      fig <- subplot(
        plot_list,
        nrows = 1,
        shareY = TRUE,
        titleX = TRUE,
        widths = rep(1 / n_plots, n_plots), 
        margin = 0.01
      ) %>%
        layout(
          title = "",
          
          legend = list(
            orientation = "h",
            x = 0.5,
            xanchor = "center",
            y = -0.25
          ),
          
          margin = list(
            l = 70,
            r = 30,
            b = 110,
            t = 20
          )
        )
      
      return(apply_labor_plot_theme(fig))
    }
    
    # ---- bonuses and benefits/Payroll and Total ----
    
    if (groupA == "total" &&
        group0 %in% c("bonuses_and_benefits", "payroll_taxes", "social") &&
        length(ns_variables$country_sel) == 1) {
    
    if(ns_variables$country_sel=="All"){
        # OPTIMIZADO: get_group_data() en lugar de readRDS()
        data_key <- if (group0 == "social") groupE else group0
        df <- get_group_data(data_key)
        if (is.null(df)) {
          showNotification("Data not available.", type = "error")
          return(NULL)
        }
        df <- df %>%
          filter(
            wage %in% wage_filter
          ) %>%
          apply_wage_panels() %>%
          select(country, min_max_total, value) %>%
          mutate(
            type = ifelse(grepl("_min$", min_max_total), "Min", "Max")
          )
        all_countries <- unique(df$country)
        df <- filter_missing_payroll(df, sources$countries)
        df <- filter_missing_occ_risk(df, sources$countries)
        ns_variables$countries=c("All", all_countries)
      
    
        
        if (nrow(df) == 0) {
          showNotification("No Data for this combination.", type = "error")
          return(NULL)
        }
        
        df_wide=df %>%
          group_by(country) %>%
          summarize(
            t_min = min(value, na.rm = TRUE),
            t_max = max(value, na.rm = TRUE)
          )%>%
          arrange(t_min) %>%
          mutate(country = factor(country, levels = country))
        
        df_mm <- df_wide %>%
          tidyr::pivot_longer(
            cols = c(t_min, t_max),
            names_to = "Scenario",
            values_to = "value"
          ) %>%
          mutate(
            Scenario = ifelse(Scenario == "t_min", "Min", "Max"),
            Scenario = factor(Scenario, levels = c("Min", "Max")),
            country  = factor(country, levels = panel_order())
          )
        
        ns_variables$df_final=df_mm
        scenario_colors <- if (group0 == "social") {
          resolve_social_colors(groupE)
        } else {
          c("Min" = "#00C1FF", "Max" = "#002244")
        }

        paises <- unique(df_mm$country)
        plot_list <- list()
        
        for (i in seq_along(paises)) {
          
          pais <- paises[i]
          data_pais <- df_mm %>% filter(country == pais)
          
          p <- plot_ly(
            data = data_pais,
            x = ~Scenario,
            y = ~value,
            type = "bar",
            color = ~Scenario,
            colors = scenario_colors,
            showlegend = FALSE
          ) %>%
            layout(
              barmode = "stack",   
              
              paper_bgcolor = "rgba(0,0,0,0)",
              plot_bgcolor  = "rgba(0,0,0,0)",
              
              xaxis = list(
                title = pais,
                showgrid = FALSE,
                zeroline = FALSE,
                showline = FALSE,
                tickangle = 90
              ),
              
              yaxis = list(
                title = ifelse(i == 1, y_axis_title, ""),
                showgrid = FALSE,
                zeroline = FALSE,
                showline = FALSE
              )
            )
          
          plot_list[[i]] <- p
        }
        
        fig <- subplot(
          plot_list,
          nrows = 1,
          shareY = TRUE,
          titleX = TRUE,
          widths = rep(1 / length(plot_list), length(plot_list)),
          margin = 0.01
        ) %>%
          layout(
            margin = list(l = 70, r = 30, b = 110, t = 20),
            paper_bgcolor = "rgba(0,0,0,0)",
            plot_bgcolor  = "rgba(0,0,0,0)"
          )
        
        return(apply_labor_plot_theme(fig))
    }
    else{
        # OPTIMIZADO: get_group_data() en lugar de readRDS()
        data_key <- if (group0 == "social") groupE else group0
        df <- get_group_data(data_key)
        if (is.null(df)) {
          showNotification("Data not available.", type = "error")
          return(NULL)
        }
        df <- df %>%
          filter(
            wage %in% wage_filter,
            country==ns_variables$country_sel
          ) %>%
          apply_wage_panels() %>%
          select(country, min_max_total, value) %>%
          mutate(
            type = ifelse(grepl("_min$", min_max_total), "Min", "Max")
          )
        df <- filter_missing_payroll(df, ns_variables$country_sel)
        df <- filter_missing_occ_risk(df, ns_variables$country_sel)
        df <- filter_missing_occ_risk(df, ns_variables$country_sel)
      
        
        if (nrow(df) == 0) {
          showNotification("No Data for this combination.", type = "error")
          return(NULL)
        }
        
        
        df_wide=df %>%
          group_by(country) %>%
          summarize(
            t_min = min(value, na.rm = TRUE),
            t_max = max(value, na.rm = TRUE)
          )%>%
          arrange(t_min) %>%
          mutate(country = factor(country, levels = country))
        
        df_mm <- df_wide %>%
          tidyr::pivot_longer(
            cols = c(t_min, t_max),
            names_to = "Scenario",
            values_to = "value"
          ) %>%
          mutate(
            Scenario = ifelse(Scenario == "t_min", "Min", "Max"),
            Scenario = factor(Scenario, levels = c("Min", "Max")),
            country  = factor(country, levels = panel_order())
          )
        
        ns_variables$df_final=df_mm
        
        paises <- unique(df_mm$country)
        plot_list <- list()
        
        for (i in seq_along(paises)) {
          
          pais <- paises[i]
          data_pais <- df_mm %>% filter(country == pais)
          
          p <- plot_ly(
            data = data_pais,
            x = ~Scenario,
            y = ~value,
            type = "bar",
            color = ~Scenario,
            colors = c("Min" = "#00C1FF", "Max" = "#002244"),
            showlegend = FALSE
          ) %>%
            layout(
              barmode = "stack",   
              
              paper_bgcolor = "rgba(0,0,0,0)",
              plot_bgcolor  = "rgba(0,0,0,0)",
              
              xaxis = list(
                title = pais,
                showgrid = FALSE,
                zeroline = FALSE,
                showline = FALSE,
                tickangle = 90
              ),
              
              yaxis = list(
                title = ifelse(i == 1, y_axis_title, ""),
                showgrid = FALSE,
                zeroline = FALSE,
                showline = FALSE
              )
            )
          
          plot_list[[i]] <- p
        }
        
        fig <- subplot(
          plot_list,
          nrows = 1,
          shareY = TRUE,
          titleX = TRUE,
          widths = rep(1 / length(plot_list), length(plot_list)),
          margin = 0.01
        ) %>%
          layout(
            margin = list(l = 70, r = 30, b = 110, t = 20),
            paper_bgcolor = "rgba(0,0,0,0)",
            plot_bgcolor  = "rgba(0,0,0,0)"
          )
        
        return(apply_labor_plot_theme(fig))
    }
    }
    
    if (groupA == "total" &&
        group0 %in% c("bonuses_and_benefits", "payroll_taxes", "social") &&
        length(ns_variables$country_sel) > 1) {
      
      if ("All" %in%  ns_variables$country_sel) {
        showNotification("Please select only countries.", type = "error")
        return(NULL)
      }
        # OPTIMIZADO: get_group_data() en lugar de readRDS()
        data_key <- if (group0 == "social") groupE else group0
        df <- get_group_data(data_key)
        if (is.null(df)) {
          showNotification("Data not available.", type = "error")
          return(NULL)
        }
        df <- df %>%
          filter(
            wage %in% wage_filter,
            country %in% ns_variables$country_sel
          ) %>%
          apply_wage_panels() %>%
          select(country, min_max_total, value) %>%
          mutate(
            type = ifelse(grepl("_min$", min_max_total), "Min", "Max")
          )
        df <- filter_missing_payroll(df, ns_variables$country_sel)
          
        
        if (nrow(df) == 0) {
          showNotification("No Data for this combination.", type = "error")
          return(NULL)
        }
        
        
        df_wide=df %>%
          group_by(country) %>%
          summarize(
            t_min = min(value, na.rm = TRUE),
            t_max = max(value, na.rm = TRUE)
          )%>%
          arrange(t_min) %>%
          mutate(country = factor(country, levels = country))
        
        df_mm <- df_wide %>%
          tidyr::pivot_longer(
            cols = c(t_min, t_max),
            names_to = "Scenario",
            values_to = "value"
          ) %>%
          mutate(
            Scenario = ifelse(Scenario == "t_min", "Min", "Max"),
            Scenario = factor(Scenario, levels = c("Min", "Max")),
            country  = factor(country, levels = panel_order())
          )
        
        ns_variables$df_final=df_mm
        
        paises <- unique(df_mm$country)
        plot_list <- list()
        
        for (i in seq_along(paises)) {
          
          pais <- paises[i]
          data_pais <- df_mm %>% filter(country == pais)
          
          p <- plot_ly(
            data = data_pais,
            x = ~Scenario,
            y = ~value,
            type = "bar",
            color = ~Scenario,
            colors = c("Min" = "#00C1FF", "Max" = "#002244"),
            showlegend = FALSE
          ) %>%
            layout(
              barmode = "stack",   
              
              paper_bgcolor = "rgba(0,0,0,0)",
              plot_bgcolor  = "rgba(0,0,0,0)",
              
              xaxis = list(
                title = pais,
                showgrid = FALSE,
                zeroline = FALSE,
                showline = FALSE,
                tickangle = 90
              ),
              
              yaxis = list(
                title = ifelse(i == 1, y_axis_title, ""),
                showgrid = FALSE,
                zeroline = FALSE,
                showline = FALSE
              )
            )
          
          plot_list[[i]] <- p
        }
        
        fig <- subplot(
          plot_list,
          nrows = 1,
          shareY = TRUE,
          titleX = TRUE,
          widths = rep(1 / length(plot_list), length(plot_list)),
          margin = 0.01
        ) %>%
          layout(
            margin = list(l = 70, r = 30, b = 110, t = 20),
            paper_bgcolor = "rgba(0,0,0,0)",
            plot_bgcolor  = "rgba(0,0,0,0)"
          )
        
        return(apply_labor_plot_theme(fig))
    }
    
    # ---- bonuses and benefits and Components ----
    
    if ((group0=="bonuses_and_benefits") & groupA == "component" & groupD == "all_bonuses") {
      # OPTIMIZADO: get_component_data() en lugar de readRDS()
      df <- get_component_data("bonuses_and_benefits")
      if (is.null(df)) {
        showNotification("Data not available.", type = "error")
        return(NULL)
      }
      df_long <- df  %>%
        filter(
          wage %in% wage_filter
        ) %>%
        apply_wage_panels()
      
      if ("component" %in% names(df_long)) {
        df_long <- df_long %>%
          mutate(
            payer = dplyr::case_when(
              component == "ab" ~ "Annual and other periodic bonuses",
              component == "pl" ~ "Paid Leave",
              component == "up" ~ "Unemployment Protection",
              component == "ob" ~ "Other bonuses",
              TRUE ~ NA_character_
            )
          )
      } else {
        df_long <- df_long %>%
          mutate(
            payer = dplyr::case_when(
              grepl("annual_bonuses", min_max_component) ~ "Annual and other periodic bonuses",
              grepl("paid_leave", min_max_component) ~ "Paid Leave",
              grepl("unemployment_protection", min_max_component) ~ "Unemployment Protection",
              grepl("other_bonuses", min_max_component) ~ "Other bonuses",
              TRUE ~ NA_character_
            )
          )
      }

      df_long <- df_long %>%
        select(country, wage, min_max_component, value, payer) %>%
        mutate(
          group = ifelse(grepl("_min$", min_max_component), "Min", "Max"),
          group = factor(group, levels = c("Min", "Max"))
        )
      
      df_long <- df_long %>%
        left_join(
          bonus_hover_lookup,
          by = c("country", "wage", "group" = "group", "payer" = "Type")
        )
      
      if (length(ns_variables$country_sel) > 1) {
        if ("All" %in%  ns_variables$country_sel) {
          showNotification("Please select only countries.", type = "error")
          return(NULL)
        }
        df_long <- df_long %>% filter(country %in% ns_variables$country_sel)
      } else if (length(ns_variables$country_sel) == 1 && ns_variables$country_sel != "All") {
        df_long <- df_long %>% filter(country == ns_variables$country_sel)
      } else {
        ns_variables$countries=c("All",unique(df$country))
      }
      
      country_levels <- panel_order()
      if (is.null(country_levels) || length(country_levels) == 0) {
        country_levels <- unique(df_long$country)
      }
      df_long <- df_long %>%
        mutate(country = factor(country, levels = country_levels)) %>% 
        arrange(country)
      
      if (nrow(df_long) == 0) {
        showNotification("No Data for this combination.", type = "error")
        return(NULL)
      }
      
      df <- df_long
      df$Type <- factor(df$payer, levels = bonus_stack_order)
      df$Scenario <- factor(df$group, levels = c("Min", "Max"))
      
      paises <- unique(df$country)
      plot_list <- list()
      
      for (i in seq_along(paises)) {
        pais <- paises[i]
        data_pais <- df %>% filter(country == pais)
        
        if (nrow(data_pais) == 0) next
        
        show_legend <- i == 1
        
        p <- build_bonus_subplot(
          data_pais,
          show_legend = show_legend,
          y_axis_title = ifelse(i == 1, y_axis_title, "")
        ) %>%
          layout(
            xaxis = list(
              title = pais,
              showgrid = FALSE,
              zeroline = FALSE,
              showline = FALSE,
              tickangle = 90
            ),
            yaxis = list(
              title = ifelse(i == 1, y_axis_title, ""),
              showgrid = FALSE,
              zeroline = FALSE,
              showline = FALSE
            )
          )
        
        plot_list[[i]] <- p
      }
      
      n_plots <- length(plot_list)
      fig <- subplot(
        plot_list,
        nrows = 1,
        shareY = TRUE,
        titleX = TRUE,
        widths = rep(1 / n_plots, n_plots), 
        margin = 0.01
      ) %>%
        layout(
          title = "",
          
          legend = list(
            orientation = "h",
            x = 0.5,
            xanchor = "center",
            y = -0.25
          ),
          
          margin = list(
            l = 70,
            r = 30,
            b = 110,
            t = 20
          )
        )
      return(apply_labor_plot_theme(fig))
    }
    if (groupA == "component" & groupC!="all_component" & length(ns_variables$country_sel)==1) {
      
      if(ns_variables$country_sel=="All"){
        if(groupC=="bonuses_and_benefits" & groupD!="all_bonuses"){
          # OPTIMIZADO: get_component_data() en lugar de readRDS()
          df <- get_component_data("bonuses_and_benefits")
          if (is.null(df)) {
            showNotification("Data not available.", type = "error")
            return(NULL)
          }
          df <- df %>%
            filter(
              wage %in% wage_filter
            )
          df <- filter_bonus_component(df, groupD) %>%
            apply_wage_panels() %>%
            select(country, min_max_component, value) %>%
            mutate(
              type = ifelse(grepl("_min$", min_max_component), "Min", "Max")
            )
          ns_variables$countries=c("All",unique(df$country))
        } else if(groupC=="social"){
          # OPTIMIZADO: get_group_data() en lugar de readRDS()
          df <- get_group_data(groupE)
          if (is.null(df)) {
            showNotification("Data not available.", type = "error")
            return(NULL)
          }
          df <- df %>%
            filter(
              wage %in% wage_filter
            ) %>%
            apply_wage_panels() %>%
            select(country, min_max_total, value) %>%
            mutate(
              type = ifelse(grepl("_min$", min_max_total), "Min", "Max")
            )
          ns_variables$countries=c("All",unique(df$country))
        } else if (!is.null(input$component_type) &&
                   length(input$component_type) > 0 &&
                   identical(input$component_type, "Total")){
            # OPTIMIZADO: get_group_data() en lugar de readRDS()
            df <- get_group_data(groupC)
            if (is.null(df)) {
              showNotification("Data not available.", type = "error")
              return(NULL)
            }
            df <- df %>%
              filter(
                wage %in% wage_filter
              ) %>%
              apply_wage_panels() %>%
              select(country, min_max_total, value) %>%
              mutate(
                type = ifelse(grepl("_min$", min_max_total), "Min", "Max")
              )
            ns_variables$countries=c("All",unique(df$country))
        }
        
        if (is.null(df) || nrow(df) == 0) {
          showNotification("No Data for this combination.", type = "error")
          return(NULL)
        }
        
        df_wide=df %>%
          group_by(country) %>%
          summarize(
            t_min = min(value, na.rm = TRUE),
            t_max = max(value, na.rm = TRUE)
          )%>%
          arrange(t_min) %>%
          mutate(country = factor(country, levels = country))
        
        df_mm <- df_wide %>%
          tidyr::pivot_longer(
            cols = c(t_min, t_max),
            names_to = "Scenario",
            values_to = "value"
          ) %>%
          mutate(
            Scenario = ifelse(Scenario == "t_min", "Min", "Max"),
            Scenario = factor(Scenario, levels = c("Min", "Max")),
            country  = factor(country, levels = panel_order())
          )
        
        ns_variables$df_final=df_mm
        
        paises <- unique(df_mm$country)
        plot_list <- list()
        
        for (i in seq_along(paises)) {
          
          pais <- paises[i]
          data_pais <- df_mm %>% filter(country == pais)
          
          p <- plot_ly(
            data = data_pais,
            x = ~Scenario,
            y = ~value,
            type = "bar",
            color = ~Scenario,
            colors = c("Min" = "#00C1FF", "Max" = "#002244"),
            showlegend = FALSE
          ) %>%
            layout(
              barmode = "stack",   
              
              paper_bgcolor = "rgba(0,0,0,0)",
              plot_bgcolor  = "rgba(0,0,0,0)",
              
              xaxis = list(
                title = pais,
                showgrid = FALSE,
                zeroline = FALSE,
                showline = FALSE,
                tickangle = 90
              ),
              
              yaxis = list(
                title = ifelse(i == 1, y_axis_title, ""),
                showgrid = FALSE,
                zeroline = FALSE,
                showline = FALSE
              )
            )
          
          plot_list[[i]] <- p
        }
        
        fig <- subplot(
          plot_list,
          nrows = 1,
          shareY = TRUE,
          titleX = TRUE,
          widths = rep(1 / length(plot_list), length(plot_list)),
          margin = 0.01
        ) %>%
          layout(
            margin = list(l = 70, r = 30, b = 110, t = 20),
            paper_bgcolor = "rgba(0,0,0,0)",
            plot_bgcolor  = "rgba(0,0,0,0)"
          )
        
        return(apply_labor_plot_theme(fig))
      }
      else{
        if(groupC=="bonuses_and_benefits" & groupD!="all_bonuses"){
          # OPTIMIZADO
          df <- get_component_data("bonuses_and_benefits")
          if (is.null(df)) {
            showNotification("Data not available.", type = "error")
            return(NULL)
          }
          df <- df %>%
            filter(
              wage %in% wage_filter,
              country==ns_variables$country_sel
            )
          df <- filter_bonus_component(df, groupD) %>%
            apply_wage_panels() %>%
            select(country, min_max_component, value) %>%
            mutate(
              type = ifelse(grepl("_min$", min_max_component), "Min", "Max")
            )
        } else if(groupC=="social"){
          # OPTIMIZADO
          df <- get_group_data(groupE)
          if (is.null(df)) {
            showNotification("Data not available.", type = "error")
            return(NULL)
          }
          df <- df %>%
            filter(
              wage %in% wage_filter,
              country==ns_variables$country_sel
            ) %>%
            apply_wage_panels() %>%
            select(country, min_max_total, value) %>%
            mutate(
              type = ifelse(grepl("_min$", min_max_total), "Min", "Max")
            )
        } else if (!is.null(input$component_type) &&
                   length(input$component_type) > 0 &&
                   identical(input$component_type, "Total")){
            # OPTIMIZADO
            df <- get_group_data(groupC)
            if (is.null(df)) {
              showNotification("Data not available.", type = "error")
              return(NULL)
            }
            df <- df %>%
              filter(
                wage %in% wage_filter,
                country==ns_variables$country_sel
              ) %>%
              apply_wage_panels() %>%
              select(country, min_max_total, value) %>%
              mutate(
                type = ifelse(grepl("_min$", min_max_total), "Min", "Max")
              )
        }
        if (is.null(df) || nrow(df) == 0) {
          showNotification("No Data for this combination.", type = "error")
          return(NULL)
        }
        
        
        df_wide=df %>%
          group_by(country) %>%
          summarize(
            t_min = min(value, na.rm = TRUE),
            t_max = max(value, na.rm = TRUE)
          )%>%
          arrange(t_min) %>%
          mutate(country = factor(country, levels = country))
        
        df_mm <- df_wide %>%
          tidyr::pivot_longer(
            cols = c(t_min, t_max),
            names_to = "Scenario",
            values_to = "value"
          ) %>%
          mutate(
            Scenario = ifelse(Scenario == "t_min", "Min", "Max"),
            Scenario = factor(Scenario, levels = c("Min", "Max")),
            country  = factor(country, levels = panel_order())
          )
        
        ns_variables$df_final=df_mm
        
        paises <- unique(df_mm$country)
        plot_list <- list()
        
        for (i in seq_along(paises)) {
          
          pais <- paises[i]
          data_pais <- df_mm %>% filter(country == pais)
          
          p <- plot_ly(
            data = data_pais,
            x = ~Scenario,
            y = ~value,
            type = "bar",
            color = ~Scenario,
            colors = c("Min" = "#00C1FF", "Max" = "#002244"),
            showlegend = FALSE
          ) %>%
            layout(
              barmode = "stack",   
              
              paper_bgcolor = "rgba(0,0,0,0)",
              plot_bgcolor  = "rgba(0,0,0,0)",
              
              xaxis = list(
                title = pais,
                showgrid = FALSE,
                zeroline = FALSE,
                showline = FALSE,
                tickangle = 90
              ),
              
              yaxis = list(
                title = ifelse(i == 1, y_axis_title, ""),
                showgrid = FALSE,
                zeroline = FALSE,
                showline = FALSE
              )
            )
          
          plot_list[[i]] <- p
        }
        
        fig <- subplot(
          plot_list,
          nrows = 1,
          shareY = TRUE,
          titleX = TRUE,
          widths = rep(1 / length(plot_list), length(plot_list)),
          margin = 0.01
        ) %>%
          layout(
            margin = list(l = 70, r = 30, b = 110, t = 20),
            paper_bgcolor = "rgba(0,0,0,0)",
            plot_bgcolor  = "rgba(0,0,0,0)"
          )
        
        return(apply_labor_plot_theme(fig))
      }
      
    }
    if (groupA == "component" & groupC!="all_component" & length(ns_variables$country_sel)>1) {
      
      if ("All" %in%  ns_variables$country_sel) {
        showNotification("Please select only countries.", type = "error")
        return(NULL)
      }
      if(groupC=="bonuses_and_benefits" & groupD!="all_bonuses"){
        # OPTIMIZADO
        df <- get_component_data("bonuses_and_benefits")
        if (is.null(df)) {
          showNotification("Data not available.", type = "error")
          return(NULL)
        }
        df <- df %>%
          filter(
            wage %in% wage_filter,
            country %in% ns_variables$country_sel 
          )
        df <- filter_bonus_component(df, groupD) %>%
          apply_wage_panels() %>%
          select(country, min_max_component, value) %>%
          mutate(
            type = ifelse(grepl("_min$", min_max_component), "Min", "Max")
          )
      } else if(groupC=="social"){
        # OPTIMIZADO
        df <- get_component_data(groupE)
        if (is.null(df)) {
          showNotification("Data not available.", type = "error")
          return(NULL)
        }
        df <- df %>%
          filter(
            wage %in% wage_filter,
            country %in% ns_variables$country_sel 
          )
        if ("component" %in% names(df)) {
          df <- df %>% filter(component == groupD)
        }
        df <- df %>%
          apply_wage_panels() %>%
          select(country, min_max_component, value) %>%
          mutate(
            type = ifelse(grepl("_min$", min_max_component), "Min", "Max")
          )
      } else if (!is.null(input$component_type) &&
                 length(input$component_type) > 0 &&
                 identical(input$component_type, "Total")){
          # OPTIMIZADO
          df <- get_group_data(groupC)
          if (is.null(df)) {
            showNotification("Data not available.", type = "error")
            return(NULL)
          }
          df <- df %>%
            filter(
              wage %in% wage_filter,
              country %in% ns_variables$country_sel
            ) %>%
            apply_wage_panels() %>%
            select(country, min_max_total, value) %>%
            mutate(
              type = ifelse(grepl("_min$", min_max_total), "Min", "Max")
            )
      }
      if (is.null(df) || nrow(df) == 0) {
        showNotification("No Data for this combination.", type = "error")
        return(NULL)
      }
      
      
      df_wide=df %>%
        group_by(country) %>%
        summarize(
          t_min = min(value, na.rm = TRUE),
          t_max = max(value, na.rm = TRUE)
        )%>%
        arrange(t_min) %>%
        mutate(country = factor(country, levels = country))
      
      df_mm <- df_wide %>%
        tidyr::pivot_longer(
          cols = c(t_min, t_max),
          names_to = "Scenario",
          values_to = "value"
        ) %>%
        mutate(
          Scenario = ifelse(Scenario == "t_min", "Min", "Max"),
          Scenario = factor(Scenario, levels = c("Min", "Max")),
          country  = factor(country, levels = panel_order())
        )
      
      ns_variables$df_final=df_mm
      
      paises <- unique(df_mm$country)
      plot_list <- list()
      
      for (i in seq_along(paises)) {
        
        pais <- paises[i]
        data_pais <- df_mm %>% filter(country == pais)
        
        p <- plot_ly(
          data = data_pais,
          x = ~Scenario,
          y = ~value,
          type = "bar",
          color = ~Scenario,
          colors = c("Min" = "#00C1FF", "Max" = "#002244"),
          showlegend = FALSE
        ) %>%
          layout(
            barmode = "stack",   
            
            paper_bgcolor = "rgba(0,0,0,0)",
            plot_bgcolor  = "rgba(0,0,0,0)",
            
            xaxis = list(
              title = pais,
              showgrid = FALSE,
              zeroline = FALSE,
              showline = FALSE,
              tickangle = 90
            ),
            
            yaxis = list(
              title = ifelse(i == 1, y_axis_title, ""),
              showgrid = FALSE,
              zeroline = FALSE,
              showline = FALSE
            )
          )
        
        plot_list[[i]] <- p
      }
      
      fig <- subplot(
        plot_list,
        nrows = 1,
        shareY = TRUE,
        titleX = TRUE,
        widths = rep(1 / length(plot_list), length(plot_list)),
        margin = 0.01
      ) %>%
        layout(
          margin = list(l = 70, r = 30, b = 110, t = 20),
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor  = "rgba(0,0,0,0)"
        )
      
      return(apply_labor_plot_theme(fig))
    }
    
  })
  
  
  # ============================================================================
  # RENDER REACTABLE - OPTIMIZADO
  # ============================================================================
  # CAMBIO PRINCIPAL:
  # ANTES:  data <- read_excel("data/non_salary/tables.xlsx", sheet = "TL All B")
  # AHORA:  data <- get_excel_table("TL All B")
  # ============================================================================
  
  output$tabla_detalle <- renderUI({
    
    table_visible(FALSE)
    ns_variables$df_final_tabla <- NULL
    
    group0 <- safe_value(selected_group0(), "all")
    groupA <- safe_value(selected_groupA(), "total")
    groupC <- safe_value(selected_groupC(), "all_component")
    groupD <- safe_value(selected_groupD(), "all_bonuses")
    groupE <- safe_value(selected_groupE(), "pensions")

    if (tenure_enabled()) {
      country_sel <- ns_variables$country_sel
      excluded_countries <- c("BRA", "CHL", "COL", "PER")
      if (!is.null(country_sel) &&
          length(country_sel) == 1 &&
          country_sel %in% excluded_countries) {
        return(NULL)
      }
    }

    if (identical(safe_value(input$compare_mode, "country"), "wage")) {
      hetero_meta <- get_heterogeneity_sections()
      country_sel <- ns_variables$country_sel
      if (is.null(country_sel) || length(country_sel) == 0 || "All" %in% country_sel) {
        return(NULL)
      }
      country_code <- country_sel[1]
      if (!(country_code %in% hetero_meta$countries)) {
        return(NULL)
      }
      category <- hetero_category_for_selection(group0, groupE)
      if (is.null(category)) {
        return(NULL)
      }
      category_key <- normalize_hetero_key(category)
      matching <- Filter(function(s) {
        identical(s$country, country_code) && identical(s$category_key, category_key)
      }, hetero_meta$sections)

      if (length(matching) == 0) {
        return(NULL)
      }

      hetero_section <- matching[[1]]
      hetero_title <- NULL
      country_label <- country_display_name(country_code)
      if (identical(category_key, normalize_hetero_key("all"))) {
        hetero_title <- paste0("Detailed Regulatory Information on Heterogeneity for ", country_label)
      } else {
        hetero_title <- paste0(
          "Detailed Regulatory Information on Heterogeneity of ",
          hetero_section$category,
          " for ",
          country_label
        )
      }

      return(tagList(
        tags$style(HTML(sprintf("
          .excel-table table {
            border-collapse: collapse;
            width: 80%%;
            margin: 0 auto 20px auto;
            font-family: 'Aptos Narrow', 'Calibri', Arial, sans-serif;
            font-size: 12px;
          }
          .excel-table td, .excel-table th {
            border: 1px solid #000;
            padding: 6px 8px;
            vertical-align: middle;
          }
          .excel-table tr:first-child td {
            background-color: #f5f5f5;
            font-weight: bold;
            text-align: center;
          }
          .excel-table td.cell-empty,
          .excel-table th.cell-empty,
          .excel-table tr.row-empty td,
          .excel-table tr.row-empty th {
            border: none !important;
            background: transparent !important;
          }
          .section-title {
            background: transparent;
            color: #0f3b66;
            font-weight: 600;
            margin: 8px 0 12px 0;
            text-align: center;
            font-size: 20px;
            font-family: %s;
          }
        ", plotly_font_family))),
        tags$script(HTML("
          (function() {
            var tables = document.querySelectorAll('.excel-table table');
            tables.forEach(function(table) {
              var rows = table.querySelectorAll('tr');
              rows.forEach(function(row) {
                var cells = row.querySelectorAll('td, th');
                if (!cells.length) return;
                var allEmpty = true;
                cells.forEach(function(cell) {
                  var text = (cell.textContent || '').replace(/\\u00a0/g, ' ').trim();
                  if (text.length === 0) {
                    cell.classList.add('cell-empty');
                  } else {
                    allEmpty = false;
                  }
                });
                if (allEmpty) {
                  row.classList.add('row-empty');
                }
              });
            });
          })();
        ")),
        div(class = "section-title", hetero_title),
        div(class = "excel-table", HTML(hetero_section$table_html))
      ))
    }
    
    con_sel <- ns_variables$country_sel
    if (is.null(con_sel) || length(con_sel) == 0) {
      con_sel <- "All"
    }
    con_sel_names <- con_sel
    if (!"All" %in% con_sel) {
      con_sel_names <- vapply(con_sel, country_display_name, character(1))
    }
    
    title_case_simple <- function(text) {
      if (is.null(text) || length(text) == 0 || !nzchar(text)) return(text)
      minor_words <- c("a", "an", "and", "as", "at", "but", "by", "for", "from",
                       "if", "in", "nor", "of", "on", "or", "per", "the", "to",
                       "vs", "via", "with")
      original_words <- strsplit(text, "\\s+")[[1]]
      words <- strsplit(tolower(text), "\\s+")[[1]]
      total <- length(words)
      for (i in seq_along(words)) {
        orig <- original_words[i]
        w <- words[i]
        if (grepl("^[A-Z]{2,}$", orig)) {
          words[i] <- orig
        } else if (i != 1 && i != total && w %in% minor_words) {
          words[i] <- w
        } else {
          words[i] <- paste0(toupper(substr(w, 1, 1)), substr(w, 2, nchar(w)))
        }
      }
      paste(words, collapse = " ")
    }
    
    # ---- Title ----
    component_label <- NULL
    if (groupC == "bonuses_and_benefits") {
      component_label <- switch(
        groupD,
        all_bonuses = "All Bonuses",
        ab = "Annual and other periodic bonuses",
        pl = "Paid Leave",
        up = "Unemployment Protection",
        ob = "Other bonuses and benefits",
        "Bonuses and benefits"
      )
    } else if (groupE == "health") {
      component_label <- "Health"
    } else if (groupE == "payroll_taxes") {
      component_label <- "Payroll Taxes"
    } else if (groupE == "pensions") {
      component_label <- "Pensions"
    } else if (groupE == "occupational_risk") {
      component_label <- "Occupational Risk Insurance"
    }
    
    title_text <- NULL
    if (!is.null(component_label)) {
      title_text <- paste0("Detailed regulatory information on ", component_label)
      if (length(con_sel_names) == 1 && con_sel_names[1] != "All") {
        title_text <- paste0(title_text, " in ", con_sel_names[1])
      }
      title_text <- title_case_simple(title_text)
    }
    
    title_ui <- NULL
    if (!is.null(title_text)) {
      title_ui <- tags$div(
        style = paste(
          "font-weight: 600;",
          "margin: 8px 0 12px 0;",
          "color: #0f3b66;",
          "text-align: center;",
          "font-size: 20px;",
          "font-family:", plotly_font_family, ";"
        ),
        title_text
      )
    }
    
    # ---- Helper: load sheet, filter by country, store in ns_variables ----
    load_and_filter <- function(sheet_name) {
      data <- get_excel_table(sheet_name)
      if (is.null(data)) return(NULL)
      data <- as.data.frame(data)
      if (!"All" %in% con_sel) {
        data <- data %>% dplyr::filter(Country %in% con_sel_names)
      }
      ns_variables$df_final_tabla <- data
      data
    }
    
    # ---- Helper: build a select-input filter function for Country ----
    # This creates a native <select> dropdown inside reactable's filter row
    # using only reactable's built-in reactable::JS — no external wiring needed.
    country_select_filter <- function(countries) {
      opts <- sort(unique(countries))
      opts_js <- paste0('"', opts, '"', collapse = ", ")
      reactable::JS(sprintf(
        'function(column, state) {
           var opts = [%s];
           var onChange = function(e) {
             column.setFilter(e.target.value || undefined);
           };
           return React.createElement("select", {
             value: column.filterValue || "",
             onChange: onChange,
             style: {
               width: "100%%",
               padding: "4px 6px",
               border: "1px solid #ccc",
               borderRadius: "4px",
               fontSize: "12px",
               fontFamily: "%s",
               background: "#fff",
               cursor: "pointer"
             }
           }, [
             React.createElement("option", { value: "", key: "_all" }, "All"),
             opts.map(function(o) {
               return React.createElement("option", { value: o, key: o }, o);
             })
           ]);
         }', opts_js, plotly_font_family
      ))
    }
    
    # Exact-match filter method for the dropdown (not substring)
    exact_filter_method <- reactable::JS(
      'function(rows, columnId, filterValue) {
         if (!filterValue) return rows;
         return rows.filter(function(row) {
           return row.values[columnId] === filterValue;
         });
       }'
    )
    
    # ---- Helper: build reactable ----
    build_reactable <- function(data, merge_country = FALSE) {
      
      is_all_bonuses_table <- identical(groupA, "component") &&
        identical(groupC, "bonuses_and_benefits") &&
        identical(groupD, "all_bonuses")
      
      table_class     <- NULL
      table_style_tag <- NULL
      
      # --- Default column definition ---
      table_class <- "all-bonuses-table"
      table_style_tag <- tags$style(HTML(paste(
        ".all-bonuses-table .rt-th,",
        ".all-bonuses-table .rt-th .rt-resizable-header-content {",
        "  justify-content: center !important;",
        "  text-align: center !important;",
        "}",
        ".all-bonuses-table .rt-td {",
        "  justify-content: flex-start !important;",
        "  text-align: left !important;",
        "}",
        ".all-bonuses-table .rt-td:first-child {",
        "  justify-content: center !important;",
        "  text-align: center !important;",
        "  font-weight: 600 !important;",
        "}",
        sep = "\n"
      )))
      table_default_coldef <- reactable::colDef(
        html        = TRUE,
        minWidth    = 140,
        #maxWidth    = 260,
        align       = "left",
        headerStyle = list(textAlign = "center", fontWeight = "600"),
        style       = list(
          whiteSpace = "normal",
          lineHeight = "1.35",
          fontSize   = "12px",
          padding    = "6px",
          textAlign  = "left",
          fontFamily = plotly_font_family
        )
      )
      first_col <- names(data)[1]
      if (!is.null(first_col) && nzchar(first_col)) {
        table_columns <- list()
        table_columns[[first_col]] <- reactable::colDef(
          align       = "center",
          headerStyle = list(textAlign = "center", fontWeight = "600"),
          style       = list(fontWeight = "600", textAlign = "center")
        )
      }
      table_theme <- reactable::reactableTheme(
        style       = list(fontFamily = plotly_font_family),
        headerStyle = list(fontFamily = plotly_font_family, textAlign = "center")
      )
    
      
      # ---- "All Bonuses" special styling ----
      if (is_all_bonuses_table) {
        table_class <- "all-bonuses-table"
        table_style_tag <- tags$style(HTML(paste(
          ".all-bonuses-table .rt-th,",
          ".all-bonuses-table .rt-th .rt-resizable-header-content {",
          "  justify-content: center !important;",
          "  text-align: center !important;",
          "}",
          ".all-bonuses-table .rt-td {",
          "  justify-content: flex-start !important;",
          "  text-align: left !important;",
          "}",
          ".all-bonuses-table .rt-td:first-child {",
          "  justify-content: center !important;",
          "  text-align: center !important;",
          "  font-weight: 600 !important;",
          "}",
          sep = "\n"
        )))
        table_default_coldef <- reactable::colDef(
          html        = TRUE,
          minWidth    = 140,
          #maxWidth    = 260,
          align       = "left",
          headerStyle = list(textAlign = "center", fontWeight = "600"),
          style       = list(
            whiteSpace = "normal",
            lineHeight = "1.35",
            fontSize   = "12px",
            padding    = "6px",
            textAlign  = "left",
            fontFamily = plotly_font_family
          )
        )
        first_col <- names(data)[1]
        if (!is.null(first_col) && nzchar(first_col)) {
          table_columns <- list()
          table_columns[[first_col]] <- reactable::colDef(
            align       = "center",
            headerStyle = list(textAlign = "center", fontWeight = "600"),
            style       = list(fontWeight = "600", textAlign = "center")
          )
        }
        table_theme <- reactable::reactableTheme(
          style       = list(fontFamily = plotly_font_family),
          headerStyle = list(fontFamily = plotly_font_family, textAlign = "center")
        )
      }
      
      # ---- Visual merge of Country column (TL Pt) ----
      if (merge_country && "Country" %in% names(data)) {
        if (is.null(table_columns)) table_columns <- list()
        
        countries <- data$Country
        n         <- length(countries)
        span_vec  <- integer(n)
        i <- 1
        while (i <= n) {
          run <- 1L
          while (i + run <= n && !is.na(countries[i + run]) &&
                 countries[i + run] == countries[i]) {
            run <- run + 1L
          }
          span_vec[i] <- run
          if (run > 1) span_vec[(i + 1):(i + run - 1)] <- 0L
          i <- i + run
        }
        spans_json <- jsonlite::toJSON(span_vec, auto_unbox = FALSE)
        
        js_style <- htmlwidgets::JS(sprintf(
          "function(rowInfo) {
             var spans = %s;
             var idx   = rowInfo.index;
             if (spans[idx] === 0) return { display: 'none' };
             return { fontWeight: '600', textAlign: 'center', verticalAlign: 'middle' };
           }",
          spans_json
        ))
        
        table_columns[["Country"]] <- reactable::colDef(
          filterable    = TRUE,
          filterInput   = country_select_filter(data$Country),
          filterMethod  = exact_filter_method,
          align         = "center",
          headerStyle   = list(textAlign = "center", fontWeight = "600"),
          style         = js_style
        )
        if (is.null(table_class)) table_class <- "merged-country-table"
        
        # ---- Standard Country column with dropdown filter ----
      } else if ("Country" %in% names(data)) {
        if (is.null(table_columns)) table_columns <- list()
        table_columns[["Country"]] <- reactable::colDef(
          filterable    = TRUE,
          filterInput   = country_select_filter(data$Country),
          filterMethod  = exact_filter_method,
          sticky        = "left",
          minWidth      = 130,
          #maxWidth      = 160,
          align         = "center",
          headerStyle   = list(
            textAlign  = "center",
            fontWeight = "600",
            position   = "sticky",
            left       = "0",
            background = "#fff",
            zIndex     = "1"
          ),
          style = list(
            fontWeight = "600",
            textAlign  = "center",
            position   = "sticky",
            left       = "0",
            background = "#fff",
            zIndex     = "1"
          )
        )
      }
      
      # --- Scroll CSS: applied to the wrapper div ---
      scroll_css <- tags$style(HTML(
        ".tbl-scroll-wrap {
           width: 100%;
           max-height: 520px;
           overflow-y: auto;
           border: 1px solid #e5e7eb;
           border-radius: 6px;
         }
         .tbl-scroll-wrap .rt-thead {
           position: sticky;
           top: 0;
           z-index: 2;
           background: #fff;
         }
         .tbl-scroll-wrap .rt-tr-filters {
           position: sticky;
           top: 0;
           z-index: 2;
           background: #fff;
         }"
      ))
      
      tagList(
        title_ui,
        table_style_tag,
        scroll_css,
        tags$div(
          class = "tbl-scroll-wrap",
          style = "display:flex; justify-content:center; width:100%;",
          reactable::reactable(
            data,
            width           = "100%",
            defaultColDef   = table_default_coldef,
            columns         = table_columns,
            theme           = table_theme,
            class           = table_class,
            bordered        = TRUE,
            striped         = TRUE,
            highlight       = TRUE,
            resizable       = TRUE,
            pagination      = FALSE,
            defaultPageSize = nrow(data)
          )
        )
      )
    }
    
    # =========================================================================
    # DISPATCH
    # =========================================================================
    group0 <- safe_value(selected_group0(), "all")  
    
    show_table <- (groupA == "component") ||
      (group0 == "social") ||
      (group0 == "payroll_taxes")
    
    if (!show_table) return()
    if (groupC == "all_component" && group0 == "all") return()
    
    data <- NULL
    
    if (groupC == "bonuses_and_benefits") {
      if (groupD == "all_bonuses") {
        data <- load_and_filter("TL All B")
      } else if (groupD == "ab") {
        data <- load_and_filter("TL ab")
      } else if (groupD == "pl") {
        data <- load_and_filter("TL pl")
      } else if (groupD == "up") {
        data <- load_and_filter("TL up")
      } else if (groupD == "ob") {
        data <- load_and_filter("TL ob")
      }
    } else if (group0 == "payroll_taxes") {
      data <- load_and_filter("TL Pt")
    } else if (group0 == "social" && groupE == "health") {
      data <- load_and_filter("TL H")
    } else if (group0 == "social" && groupE == "pensions") {
      data <- load_and_filter("TL All P")
    } else if (group0 == "social" && groupE == "occupational_risk") {
      data <- load_and_filter("TL Or")
    }
    
    if (is.null(data)) return(NULL)
    table_visible(TRUE)
    
    needs_merge <- identical(groupE, "payroll_taxes")
    build_reactable(data, merge_country = needs_merge)
  })
  
  output$option2_buttons <- renderUI({
    if (!option1_selected()) {
      return(div(style = "display:none;"))
    }

    group0 <- selected_group0()
    valid_choices <- option2_choices_for_group(group0)
    if (group0 == "social" &&
        safe_value(selected_groupE(), "pensions") == "occupational_risk") {
      valid_choices <- setdiff(valid_choices, "payer")
    }
    button_style <- paste(
      "background-color: #e6f4ff;",
      "color: #0f3b66;",
      "border: 1px solid #0f3b66;",
      "border-radius: 20px;",
      "padding: 6px 18px;",
      "font-weight: 600;"
    )

    option_button <- function(id, label, value, title) {
      btn_class <- if (identical(selected_groupA(), value)) {
        "pill-button subcomponent-btn active"
      } else {
        "pill-button subcomponent-btn"
      }

      tags$div(
        style = "display: flex; flex-direction: column; gap: 4px;",
        actionButton(ns(id), label, class = btn_class, title = title, style = button_style)
      )
    }

    tags$div(
      class = "option2-group",
      style = "display: flex; flex-direction: column; gap: 8px;",
      tags$span("2. Explore by:", style = "font-weight: bold; color: #b0b0b0; font-size: 14px;"),
      if ("total" %in% valid_choices) option_button("btn_total", "TOTAL", "total", "Show total non-salary costs."),
      if ("payer" %in% valid_choices) option_button("btn_payer", "BY PAYER", "payer", "Split costs by payer (employer vs. employee)."),
      if ("component" %in% valid_choices && group0 != "social") {
        option_button("btn_component", "BY COMPONENT", "component", "Break down costs by component.")
      }
    )
  })
  
  
  # --- Components ----
  output$component_buttons <- renderUI({
    group0 <- selected_group0()
    
    if (group0 != "social" || identical(selected_groupE(), "occupational_risk")) {
      return(NULL)
    }
    button_class <- function(value) {
      if (identical(selected_groupE(), value)) {
        "component-btn active"
      } else {
        "component-btn"
      }
    }
    
    div(
      style = "margin-top: 6px;",
      div(
        class = "horizontal-container",
        style = "display:flex; align-items:flex-start; justify-content:flex-start; gap:20px; width:100%;",
        
        div(
        tags$div(
          HTML("Social Security<br>Contributions<br>Components"),
          style = "font-weight: bold; color: #b0b0b0; font-size: 14px; margin-bottom: 5px;"
        )
        ),
        
        div(
          class = "component-buttons-container",
          style = "display:flex; flex-wrap:wrap; gap:8px;",
        actionButton(
          ns("pensions"),
          "Pensions",
          class = button_class("pensions")
        ),
          
          actionButton(
            ns("health"),
            "Health",
            class = button_class("health")
          )
        )
      )
    )
  })

  output$country_buttons <- renderUI({
    if (!isTRUE(enable_tenure)) {
      return(NULL)
    }
    countries <- ns_variables$countries
    countries <- countries[!is.na(countries) & countries != "All"]
    if (length(countries) == 0) {
      return(NULL)
    }

    current <- ns_variables$country_sel
    if (is.null(current) || length(current) == 0 || "All" %in% current) {
      current <- countries[1]
    }
    active_country <- current[1]

    buttons <- lapply(countries, function(code) {
      flag_code <- COUNTRY_FLAG_MAP[[code]]
      if (is.null(flag_code) || is.na(flag_code)) {
        flag_code <- tolower(substr(code, 1, 2))
      }
      btn_class <- "labor-country-button"
      if (identical(code, active_country)) {
        btn_class <- paste(btn_class, "active")
      }
      tags$button(
        type = "button",
        class = btn_class,
        title = country_display_name(code),
        onclick = sprintf(
          "Shiny.setInputValue('%s', '%s', {priority: 'event'})",
          ns("country_button"),
          code
        ),
        tags$img(
          src = sprintf("https://flagcdn.com/24x18/%s.png", flag_code),
          class = "labor-flag",
          alt = country_display_name(code)
        ),
        tags$span(class = "labor-country-label", toupper(code)),
        tags$span(class = "labor-country-hover", country_display_name(code))
      )
    })

    tags$div(
      class = "labor-country-panel",
      tags$div(class = "labor-country-title", "Country Analysis by:"),
      tags$div(class = "labor-country-buttons", buttons)
    )
  })
  
  output$bonus_buttons <- renderUI({
    group0 <- selected_group0()
    groupA <- selected_groupA()
    
    if(group0 != "bonuses_and_benefits"){
      return(div(style="visibility:hidden;"))
    }
    else if (groupA =="component") {
      bonus_class <- function(value) {
        if (identical(selected_groupD(), value)) {
          "component-btn active"
        } else {
          "component-btn"
        }
      }
      div(
        class = "horizontal-container",
        style = "display:flex; align-items:flex-start; justify-content:space-between; width:100%;",
        
        div(
          tags$div(
            HTML("Bonuses<br>and<br>Benefits<br>Components"),
            style = "font-weight: bold; color: #b0b0b0; font-size: 14px; margin-bottom: 5px;"
          )
        ),
        
        div(
          class = "component-buttons-container",
          style = "display:flex; flex-wrap:wrap; gap:8px;",
          
          actionButton(
            ns("all_bonuses"),
            "All Bonuses",
            class = bonus_class("all_bonuses")
          ),
          
          actionButton(
            ns("ab"),
            "Annual and other periodic bonuses",
            class = bonus_class("ab")
          ),
          
          actionButton(
            ns("pl"),
            "Paid Leave",
            class = bonus_class("pl")
          ),
          
          actionButton(
            ns("up"),
            "Unemployment Protection",
            class = bonus_class("up")
          ),
          
          actionButton(
            ns("ob"),
            "other bonuses and benefits",
            class = bonus_class("ob")
          )
        )
      )
    }
  })
  
  
  
  output$download_df <- downloadHandler(
    filename = function() {
      paste0("Regulatory_Frameworks_Data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(ns_variables$df_final, file, row.names = FALSE)
    },
    contentType = "text/csv"
  )

  output$download_table_ui <- renderUI({
    if (!table_visible()) {
      return(NULL)
    }
    downloadButton(
      outputId = ns("download_table"),
      label = "DOWNLOAD TABLE",
      style = "background-color: #1e3a5f; color: white; border-radius: 25px; padding: 10px 20px; font-weight: bold; border: none;"
    )
  })
  
  output$download_table <- downloadHandler(
    filename = function() {
      paste0("Regulatory_Frameworks_Legislation_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(ns_variables$df_final_tabla, file, row.names = FALSE)
    },
    contentType = "text/csv"
  )
}

non_salary_across_server <- function(input, output, session) {
  non_salary_server_core(
    input,
    output,
    session,
    data_sources = LABOR_DATA_SOURCES_ACROSS,
    enable_tenure = FALSE
  )
}

non_salary_within_server <- function(input, output, session) {
  current_data_source <- reactive({
    if (isTRUE(input$show_by_tenure)) {
      LABOR_DATA_SOURCES_WITHIN
    } else {
      LABOR_DATA_SOURCES_WITHIN_NO_TENURE
    }
  })
  non_salary_server_core(
    input,
    output,
    session,
    data_sources = current_data_source,
    enable_tenure = TRUE
  )
}

labor_server <- function(input, output, session) {
  if (!exists("labor_landing_ui", mode = "function")) {
    source("tabs/ui/non_salary.R", local = TRUE)$value
  }
  ns <- session$ns
  selected_view <- reactiveVal(NULL)
  across_initialized <- reactiveVal(FALSE)
  within_initialized <- reactiveVal(FALSE)

  observeEvent(input$choose_across, {
    selected_view("across")
    updateQueryString("?view=across", mode = "push", session = session)
  })
  observeEvent(input$choose_within, {
    selected_view("within")
    updateQueryString("?view=within", mode = "push", session = session)
  })
  observeEvent(session$clientData$url_search, {
    query <- parseQueryString(session$clientData$url_search)
    view <- query$view
    if (!is.null(view) && view %in% c("across", "within")) {
      selected_view(view)
    } else {
      selected_view(NULL)
    }
  })

  output$labor_content <- renderUI({
    view <- selected_view()
    tagList(
      labor_view_selector_ui(ns, view),
      if (!is.null(view) && identical(view, "across")) {
        non_salary_across_ui(ns("across"), show_header = FALSE)
      } else if (!is.null(view) && identical(view, "within")) {
        non_salary_within_ui(ns("within"), show_header = FALSE)
      } else {
        NULL
      }
    )
  })

  observeEvent(selected_view(), {
    view <- selected_view()
    if (identical(view, "across") && !across_initialized()) {
      callModule(non_salary_across_server, "across")
      across_initialized(TRUE)
    }
    if (identical(view, "within") && !within_initialized()) {
      callModule(non_salary_within_server, "within")
      within_initialized(TRUE)
    }
  })
}
