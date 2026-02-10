labor_shared_scripts <- function() {
  tags$script(HTML("
    $(document).on('click', '.topic-page .option1-group .pill-button', function(e) {
      var $container = $(this).closest('.option1-group');
      $container.find('.pill-button').removeClass('active');
      $(this).addClass('active');
    });
    $(document).on('click', '.topic-page .option2-group .pill-button', function(e) {
      var $container = $(this).closest('.option2-group');
      $container.find('.pill-button').removeClass('active');
      $(this).addClass('active');
    });
    $(document).on('click', '.component-btn', function(e) {
      $('.component-btn').removeClass('active');
      $(this).addClass('active');
    });
  "))
}

labor_header_ui <- function(title, subtitle) {
  tags$div(
    style = "margin-bottom: 20px; background: #fff; padding: 16px; border-radius: 12px;",
    h1(class = "topic-title", title),
    p(class = "topic-subtitle", subtitle)
  )
}

labor_layout_ui <- function(ns, title, subtitle, filters_ui, plot_controls_ui = NULL, show_header = TRUE) {
  tagList(
    labor_shared_scripts(),
    tags$div(
      class = "topic-page",
      if (isTRUE(show_header)) {
        fluidRow(
          column(
            width = 12,
            labor_header_ui(title, subtitle)
          )
        )
      },
      fluidRow(
        column(
          width = 3,
          class = "left-panel",
          tags$div(
            style = "margin-bottom: 30px;",
            h3("FILTERS", style = "color: #1e3a5f; font-weight: bold; margin-top: 0; margin-bottom: 6px;"),
            tags$hr(style = "border-top: 2px solid #00b8d4; margin-top: 0; margin-bottom: 14px;"),
            filters_ui
          ),
          tags$div(
            style = "display: flex; gap: 10px;",
            downloadButton(
              outputId = ns("download_df"),
              label = "Download Data",
              style = "background-color: #1e3a5f; color: white; border-radius: 25px; padding: 10px 20px; font-weight: bold; border: none;"
            ),
            uiOutput(ns("download_table_ui"))
          )
        ),
        column(
          width = 9,
          class = "right-panel",
          style = "padding-top: 34px;",
          tags$hr(style = "border-top: 2px solid #00b8d4; margin-top: 0; margin-bottom: 14px;"),
          if (!is.null(plot_controls_ui)) {
            tags$div(class = "labor-plot-controls", plot_controls_ui)
          },
          div(
            class = "plot-spinner plot-scroll",
            plotlyOutput(ns("plot"), height = "520px")
          ),
          div(
            style = "margin-top:30px;",
            uiOutput(ns("tabla_detalle"))
          )
        )
      )
    )
  )
}

labor_choice_cards_ui <- function(ns, active_view = NULL) {
  across_class <- "labor-choice-row"
  within_class <- "labor-choice-row"
  if (!is.null(active_view) && active_view != "") {
    if (identical(active_view, "across")) {
      across_class <- paste(across_class, "active")
    }
    if (identical(active_view, "within")) {
      within_class <- paste(within_class, "active")
    }
  }
  tags$div(
    class = "labor-explore-by",
    tags$span(class = "labor-explore-label", "EXPLORE BY"),
    tags$div(
      class = "labor-choice-list",
      actionButton(
        ns("choose_across"),
        label = tagList(
          tags$span(class = "labor-choice-dot"),
          tags$span(class = "labor-choice-title", "CROSS-COUNTRY VIEW"),
          tags$span(
            class = "labor-choice-desc",
            "Compare non-salary labor costs across countries at a selected minimum wage level"
          )
        ),
        class = across_class
      ),
      actionButton(
        ns("choose_within"),
        label = tagList(
          tags$span(class = "labor-choice-dot"),
          tags$span(class = "labor-choice-title", "WITHIN-COUNTRY VIEW"),
          tags$span(
            class = "labor-choice-desc",
            "Compare wage levels within a single country and explore how costs change by tenure and minimum wage."
          )
        ),
        class = within_class
      )
    )
  )
}

labor_view_selector_ui <- function(ns, active_view = NULL) {
  tags$div(
    class = "labor-view-selector",
    labor_header_ui(
      "Non-salary Labor Costs",
      paste(
        "Explore non-wage labor costs employers incur beyond direct wages,",
        "including legally required social contributions by payer",
        "(employer vs. employee) and by benefit component."
      )
    ),
    labor_choice_cards_ui(ns, active_view)
  )
}

labor_landing_ui <- function(ns) {
  tags$div(
    class = "topic-page",
    fluidRow(
      column(
        width = 12,
        labor_view_selector_ui(ns)
      )
    ),
    fluidRow(column(width = 12))
  )
}

labor_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("labor_content"))
  )
}

labor_common_filters_ui <- function(ns,
                                    description_text,
                                    wage_help_text,
                                    country_help_text,
                                    tenure_block = NULL,
                                    country_section_ui = NULL,
                                    prepend_block = NULL) {
  if (is.null(country_section_ui)) {
    country_section_ui <- tags$div(
      style = "margin-top: 5px;",
      tags$span("4. Country:", style = "font-weight: bold; color: #b0b0b0; font-size: 14px;"),
      tags$p(
        country_help_text,
        style = "font-size: 12px; color: #555; margin: 0 0 6px 0;"
      ),
      uiOutput(ns("country_selection"))
    )
  }
  tagList(
    prepend_block,
    tags$span("1. Categories:", style = "font-weight: bold; color: #b0b0b0; font-size: 14px;"),
    tags$p(
      style = "font-size: 13px; line-height: 1.5; margin-bottom: 15px;",
      description_text
    ),
    tags$div(
      class = "option1-group",
      style = "display: flex; flex-direction: column; gap: 8px; margin-bottom: 12px;",
      tags$div(
        class = "labor-primary-buttons",
        style = "display: flex; flex-direction: column; gap: 8px;",
        tags$div(
          style = "display: flex; flex-direction: column; gap: 4px;",
          actionButton(
            ns("all"),
            "ALL",
            class = "pill-button active",
            title = "Show total non-salary costs across all components.",
            style = "background-color: #e6f4ff; color: #0f3b66; border: 1px solid #0f3b66; border-radius: 20px; padding: 6px 18px; font-weight: 600;"
          )
        ),
        tags$div(
          style = "display: flex; flex-direction: column; gap: 4px;",
          actionButton(
            ns("bonus"),
            "BONUSES AND BENEFITS",
            class = "pill-button",
            title = "Focus on bonuses and benefits costs.",
            style = "background-color: #e6f4ff; color: #0f3b66; border: 1px solid #0f3b66; border-radius: 20px; padding: 6px 18px; font-weight: 600;"
          )
        ),
        tags$div(
          style = "display: flex; flex-direction: column; gap: 4px;",
          actionButton(
            ns("social"),
            "SOCIAL SECURITY CONTRIBUTIONS",
            class = "pill-button",
            title = "Focus on social security contributions.",
            style = "background-color: #e6f4ff; color: #0f3b66; border: 1px solid #0f3b66; border-radius: 20px; padding: 6px 18px; font-weight: 600;"
          ),
          uiOutput(ns("component_buttons"))
        ),
        tags$div(
          style = "display: flex; flex-direction: column; gap: 4px;",
          actionButton(
            ns("occupational_risk_main"),
            "OCCUPATIONAL RISK",
            class = "pill-button",
            title = "Focus on occupational risk contributions.",
            style = "background-color: #e6f4ff; color: #0f3b66; border: 1px solid #0f3b66; border-radius: 20px; padding: 6px 18px; font-weight: 600;"
          )
        ),
        tags$div(
          style = "display: flex; flex-direction: column; gap: 4px;",
          actionButton(
            ns("payroll"),
            "PAYROLL TAXES",
            class = "pill-button",
            title = "Focus on payroll tax costs.",
            style = "background-color: #e6f4ff; color: #0f3b66; border: 1px solid #0f3b66; border-radius: 20px; padding: 6px 18px; font-weight: 600;"
          )
        )
      )
    ),
    uiOutput(ns("option2_buttons")),
    tags$div(
      class = "component-wrapper-fixed",
      uiOutput(ns("bonus_buttons"))
    ),
    tags$div(
      style = "display: flex; flex-direction: column; gap: 8px; margin-bottom: 12px;",
      tags$span("3. Minimum Wage:", style = "font-weight: bold; color: #b0b0b0; font-size: 14px;"),
      tags$p(
        wage_help_text,
        style = "font-size: 12px; color: #555; margin: 0 0 4px 0;"
      ),
      uiOutput(ns("mw_selection_ui"))
    ),
    tenure_block,
    country_section_ui
  )
}

non_salary_across_ui <- function(id, show_header = TRUE) {
  ns <- NS(id)

  filters_ui <- tagList(
    div(
      style = "display:none;",
      selectInput(
        inputId = ns("compare_mode"),
        label = NULL,
        choices = c("ACROSS COUNTRIES" = "country"),
        selected = "country",
        width = "100%"
      )
    ),
    labor_common_filters_ui(
      ns,
      description_text = "Use these filters to compare non-salary labor costs across countries",
      wage_help_text = paste(
        "Hold the job constant and change only earnings:",
        "choose a multiple of the minimum wage (MW) to see how statutory costs scale with pay."
      ),
      country_help_text = paste(
        "Switch countries to see how different regulatory frameworks",
        "change the composition and level of non-wage costs."
      )
    )
  )

  labor_layout_ui(
    ns,
    "Non-salary Labor Costs: Across Countries",
    "Compare non-wage labor costs across countries at a single wage level.",
    filters_ui,
    show_header = show_header
  )
}

non_salary_within_ui <- function(id, show_header = TRUE) {
  ns <- NS(id)
  show_by_tenure_id <- ns("show_by_tenure")

  tenure_block <- tags$div(
    class = "labor-tenure-toggle",
    checkboxInput(show_by_tenure_id, "EXPLORE BY JOB TENURE", FALSE),
    tags$p(
      "Explore how non-salary costs vary by job tenure (years).",
      style = "font-size: 12px; color: #555; margin: 0;"
    )
  )

  filters_ui <- tagList(
    div(
      style = "display:none;",
      selectInput(
        inputId = ns("compare_mode"),
        label = NULL,
        choices = c("WITHIN A COUNTRY" = "wage"),
        selected = "wage",
        width = "100%"
      )
    ),
    labor_common_filters_ui(
      ns,
      description_text = "Use these filters to compare wage levels within a country and explore tenure patterns.",
      wage_help_text = "Choose one or more wage levels to compare within the selected country.",
      country_help_text = "Select one country to compare wage levels and tenure within it.",
      tenure_block = NULL,
      prepend_block = tenure_block,
      country_section_ui = tags$div(
        style = "display:none;",
        uiOutput(ns("country_selection"))
      )
    )
  )

  labor_layout_ui(
    ns,
    "Non-salary Labor Costs: Within a Country",
    "Compare wage levels within one country and explore tenure variation.",
    filters_ui,
    plot_controls_ui = uiOutput(ns("country_buttons")),
    show_header = show_header
  )
}
