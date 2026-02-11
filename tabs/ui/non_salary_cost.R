labor_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    
    tags$style(HTML("
      .pill-button.active {
        background-color: #00C1FF !important;
        color: white !important;
      }
      .pill-button:hover {
        opacity: 0.85;
      }
      .pill-button {
        font-size: 11px;
      }
      .subcomponent-btn {
        font-size: 9px;
      }
      
      .component-btn.active {
        background-color: #00C1FF !important;
        color: white !important;
      }
      .component-btn:hover {
        opacity: 0.85;
      }
      .component-btn {
        font-size: 9px;
      }

      /* =====================================================
         LOADING MESSAGE — pure CSS, uses Shiny's built-in
         .recalculating class (added automatically to outputs)
         ===================================================== */

      .plot-spinner .recalculating,
      .table-spinner .recalculating {
        position: relative;
        opacity: 0.35 !important;
        pointer-events: none;
        transition: opacity 0.2s;
      }

      /* White overlay */
      .plot-spinner .recalculating::before,
      .table-spinner .recalculating::before {
        content: '';
        position: absolute;
        inset: 0;
        background: rgba(255, 255, 255, 0.7);
        z-index: 10;
      }

      /* Spinner circle + text message */
      .plot-spinner .recalculating::after {
        content: 'Loading chart, please wait...';
        position: absolute;
        top: 45%;
        left: 50%;
        transform: translate(-50%, -50%);
        z-index: 11;
        font-size: 15px;
        font-weight: 500;
        color: #1e3a5f;
        font-family: 'National Park', sans-serif;
        background: white;
        padding: 14px 28px;
        border-radius: 8px;
        box-shadow: 0 2px 12px rgba(0,0,0,0.10);
        white-space: nowrap;
      }

      .table-spinner .recalculating::after {
        content: 'Loading table, please wait...';
        position: absolute;
        top: 45%;
        left: 50%;
        transform: translate(-50%, -50%);
        z-index: 11;
        font-size: 15px;
        font-weight: 500;
        color: #1e3a5f;
        font-family: 'National Park', sans-serif;
        background: white;
        padding: 14px 28px;
        border-radius: 8px;
        box-shadow: 0 2px 12px rgba(0,0,0,0.10);
        white-space: nowrap;
      }

      @media (max-width: 768px) {
        .plot-scroll {
          overflow-x: auto;
        }
        .plot-scroll .html-widget {
          min-width: 900px;
        }
      }
    ")),
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
    ")),
    
    tags$div(class = "topic-page", # nolint
             # Header across the full page
             fluidRow(
               column(
                 width = 12,
                 tags$div(
                   style = "margin-bottom: 20px; background: #fff; padding: 16px; border-radius: 12px;",
                   h1(class = "topic-title", "Non-salary Labor Costs"),
                   p(class = "topic-subtitle",
                     "Explore non-salary labor costs separate from direct wages, with a breakdown of legally mandated social contributions by statutory payer (employer vs. employee) and by benefit category.")
                 )
               )
             ),
             
             # ============================================================
             # LAYOUT PRINCIPAL (2 COLUMNAS)
             # ============================================================
             fluidRow(
               
               # ----------------------------------------------------------
               # COLUMNA IZQUIERDA
               # ----------------------------------------------------------
              column(
                width = 3,
                class = "left-panel",
                 # -------- FILTERS --------
                 tags$div(
                   style = "margin-bottom: 30px;",
                   h3("FILTERS", style = "color: #1e3a5f; font-weight: bold; margin-top: 0; margin-bottom: 6px;"),
                   tags$hr(style = "border-top: 2px solid #00b8d4; margin-top: 0; margin-bottom: 14px;"),
                   
                   
                   
                   # ---- COMPARISON MODE ----
                   tags$div(
                     style = "display: flex; flex-direction: column; gap: 8px; margin-bottom: 12px;",
                     tags$span("Comparison focus:", style = "font-weight: bold; color: #b0b0b0; font-size: 14px;"),
                     tags$p(
                       "Compare countries at one wage level, or compare wage levels within a single country.",
                       style = "font-size: 12px; color: #555; margin: 0 0 4px 0;"
                     ),
                     div(
                       class = "pretty-select",
                       selectInput(
                         inputId = ns("compare_mode"),
                         label = NULL,
                         choices = c(
                           "ACROSS COUNTRIES" = "country",
                           "WITHIN A COUNTRY" = "wage"
                         ),
                         selected = "country",
                         width = "100%"
                       )
                     )
                   ),
                   
                   tags$span("1. Categories:", style = "font-weight: bold; color: #b0b0b0; font-size: 14px;"),
                   tags$p(
                     style = "font-size: 13px; line-height: 1.5; margin-bottom: 15px;",
                     "Use these filters to compare non-salary labor costs across countries."
                   ),
                   
                    # ---- SUMMARY FILTER ----
                    tags$div(
                      class = "option1-group",
                      style = "display: flex; flex-direction: column; gap: 8px; margin-bottom: 12px;",
                      #tags$span("Option 1:", style = "font-weight: bold; color: #b0b0b0; font-size: 14px;"),
                      tags$div(
                        class = "labor-primary-buttons",
                        style = "display: flex; flex-direction: column; gap: 8px;",
                        tags$div(
                          style = "display: flex; flex-direction: column; gap: 4px;",
                          actionButton(ns("all"), "ALL",
                                       class = "pill-button active",
                                      title = "Show total non-salary costs across all components.",
                                      style = "background-color: #e6f4ff; color: #0f3b66; border: 1px solid #0f3b66; border-radius: 20px; padding: 6px 18px; font-weight: 600;")
                       ),
                       tags$div(
                         style = "display: flex; flex-direction: column; gap: 4px;",
                         actionButton(ns("bonus"), "BONUSES AND BENEFITS",
                                      class = "pill-button",
                                      title = "Focus on bonuses and benefits costs.",
                                      style = "background-color: #e6f4ff; color: #0f3b66; border: 1px solid #0f3b66; border-radius: 20px; padding: 6px 18px; font-weight: 600;")
                       ),
                        tags$div(
                          style = "display: flex; flex-direction: column; gap: 4px;",
                          actionButton(ns("social"), "SOCIAL SECURITY CONTRIBUTIONS",
                                       class = "pill-button",
                                       title = "Focus on social security contributions.",
                                       style = "background-color: #e6f4ff; color: #0f3b66; border: 1px solid #0f3b66; border-radius: 20px; padding: 6px 18px; font-weight: 600;"),
                          uiOutput(ns("component_buttons"))
                        ),
                        tags$div(
                          style = "display: flex; flex-direction: column; gap: 4px;",
                          actionButton(ns("occupational_risk_main"), "OCCUPATIONAL RISK",
                                       class = "pill-button",
                                       title = "Focus on occupational risk contributions.",
                                       style = "background-color: #e6f4ff; color: #0f3b66; border: 1px solid #0f3b66; border-radius: 20px; padding: 6px 18px; font-weight: 600;")
                        ),
                        tags$div(
                          style = "display: flex; flex-direction: column; gap: 4px;",
                          actionButton(ns("payroll"), "PAYROLL TAXES",
                                       class = "pill-button",
                                       title = "Focus on payroll tax costs.",
                                       style = "background-color: #e6f4ff; color: #0f3b66; border: 1px solid #0f3b66; border-radius: 20px; padding: 6px 18px; font-weight: 600;")
                        )
                      )
                    ),
                   uiOutput(ns("option2_buttons")),
                   tags$div(
                     class = "component-wrapper-fixed",
                     uiOutput(ns("bonus_buttons"))
                   ),
                   
                   
                   # ---- WAGE FILTER ----
                   tags$div(
                     style = "display: flex; flex-direction: column; gap: 8px; margin-bottom: 12px;",
                     tags$span("3. Minimum Wage:", style = "font-weight: bold; color: #b0b0b0; font-size: 14px;"),
                     tags$p(
                       "Hold the job constant and change only earnings: choose a multiple of the minimum wage (MW) to see how statutory costs scale with pay.",
                       style = "font-size: 12px; color: #555; margin: 0 0 4px 0;"
                     ),
                     uiOutput(ns("mw_selection_ui"))
                   ),
                   
                   # ---- COUNTRY FILTER ----
                   tags$div(
                     style = "margin-top: 5px;",
                     tags$span("4. Country:", style = "font-weight: bold; color: #b0b0b0; font-size: 14px;"),
                     tags$p(
                       "Switch countries to see how different regulatory frameworks change the composition and level of non-wage costs.",
                       style = "font-size: 12px; color: #555; margin: 0 0 6px 0;"
                     ),
                     uiOutput(ns("country_selection"))
                   )
                 ),
                 
                 
                 
                 
                 # -------- DOWNLOAD & SHARE BUTTONS --------
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
               
               # ----------------------------------------------------------
               # COLUMNA DERECHA
               # ----------------------------------------------------------
              column(
                width = 9,
                class = "right-panel",
                 style = "padding-top: 34px;",
                 tags$hr(style = "border-top: 2px solid #00b8d4; margin-top: 0; margin-bottom: 14px;"),
                 
                 # -------- GRÁFICO --------
                 div(
                   class = "plot-spinner plot-scroll",
                   plotlyOutput(ns("plot"), height = "520px")
                 ),
                 div(
                   class = "table-spinner",
                   style = "margin-top:15px;",
                   uiOutput(ns("tabla_detalle"))
                 )
               )
             )
    )
  )
}
