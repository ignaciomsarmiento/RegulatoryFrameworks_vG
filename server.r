shinyServer(function(input, output, session) {
  
  # =========================================================================
  # REACTIVE VALUES
  # =========================================================================
  
  navigation <- reactiveValues(
    current_view = "landing",
    previous_view = NULL,
    selected_topic = NULL,
    selected_country = NULL
  )
  
  # =========================================================================
  # CONFIGURATION
  # =========================================================================
  
  topics <- list(
    labor = list(
      title = "Non-salary Labor Costs",
      description = "Yearly bonuses, social security contributions, and employment benefits",
      ui_function = "render_labor_ui",
      server_function = "labor_server"
    ),
    minwage = list(
      title = "Minimum Wages",
      description = "Minimum wage policies and trends across the region",
      ui_function = "render_minwage_ui",
      server_function = "minwage_server"
    ),
    btax = list(
      title = "Business Taxes",
      description = "Corporate tax rates, incentives, and fiscal policies",
      ui_function = "render_btax_ui",
      server_function = "btax_server"
    ),
    social = list(
      title = "Social Assistance",
      description = "Social programs, welfare systems, and poverty alleviation policies",
      ui_function = "render_social_ui",
      server_function = "social_server"
    )
  )
  
  countries <- list(
    argentina = "Argentina",
    bolivia = "Bolivia",
    brazil = "Brazil",
    chile = "Chile",
    colombia = "Colombia",
    costa_rica = "Costa Rica",
    dominican = "Dominican Republic",
    ecuador = "Ecuador",
    el_salvador = "El Salvador",
    guatemala = "Guatemala",
    honduras = "Honduras",
    mexico = "Mexico",
    nicaragua = "Nicaragua",
    panama = "Panama",
    paraguay = "Paraguay",
    peru = "Peru",
    uruguay = "Uruguay",
    venezuela = "Venezuela"
  )
  
  # =========================================================================
  # NAVIGATION HANDLERS
  # =========================================================================
  
  observeEvent(input$main_tabs, {
    if (input$main_tabs == "content" &&
        is.null(navigation$selected_topic) &&
        is.null(navigation$selected_country)) {
      navigation$selected_topic <- "labor"
      navigation$current_view <- "content"
    }
  }, ignoreInit = FALSE)
  
  observeEvent(input$topic_selected, {
    navigation$selected_topic <- input$topic_selected
    navigation$selected_country <- NULL
    navigation$previous_view <- "topics"
    navigation$current_view <- "content"
    
    updateTabsetPanel(session, "main_tabs", selected = "content")
  })
  
  # =========================================================================
  # DYNAMIC CONTENT RENDERING  (REACTIVATED FOR TOPICS)
  # =========================================================================
  
  output$dynamic_content <- renderUI({
    
    # ---- TOPIC SELECTED ----
    if (!is.null(navigation$selected_topic)) {
      topic_id <- navigation$selected_topic
      
      # LABOR — Non-Salary Costs
      if (topic_id == "labor") {
        source("tabs/ui/non_salary.R", local = TRUE)$value
        return(labor_ui("labor"))
      }
      
      # Other topic UIs
      ui_func_name <- topics[[topic_id]]$ui_function
      if (exists(ui_func_name)) {
        return(do.call(ui_func_name, list()))
      }
      
      return(
        div(class = "text-center", style = "padding: 50px;",
            h3("Topic not implemented"))
      )
    }
    
    # ---- COUNTRY SELECTED ----
    if (!is.null(navigation$selected_country)) {
      country_id <- navigation$selected_country
      country_name <- countries[[country_id]]
      return(render_country_dashboard(country_name, country_id))
    }
    
    return(
      div(class = "text-center", style = "padding: 50px;",
          h3("No selection made"))
    )
  })
  
  # =========================================================================
  # UI RENDERING FUNCTIONS
  # =========================================================================
  
  render_labor_ui <- function() {
    source("tabs/ui/non_salary.R", local = TRUE)$value
    labor_ui("labor")
  }
  
  render_minwage_ui <- function() {
    source("modules/minwage/ui.R", local = TRUE)$value
    minwage_ui("minwage")
  }
  
  render_btax_ui <- function() {
    source("modules/btax/ui.R", local = TRUE)$value
    btax_ui("btax")
  }
  
  render_social_ui <- function() {
    source("modules/social/ui.R", local = TRUE)$value
    social_ui("social")
  }
  
  # =========================================================================
  # MODULE SERVER CALLS  (SIMPLIFIED)
  # =========================================================================
  
  observeEvent(navigation$selected_topic, {
    topic_id <- navigation$selected_topic
    
    if (topic_id == "labor") {
      source("tabs/server/non_salary.R", local = TRUE)$value
      callModule(labor_server, "labor")
    }
    
    if (topic_id == "minwage") {
      source("modules/minwage/server.R", local = TRUE)$value
      callModule(minwage_server, "minwage")
    }
    
    if (topic_id == "btax") {
      source("modules/btax/server.R", local = TRUE)$value
      callModule(btax_server, "btax")
    }
    
    if (topic_id == "social") {
      source("modules/social/server.R", local = TRUE)$value
      callModule(social_server, "social")
    }
  })
  
})
