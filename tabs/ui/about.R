about<- tags$div(
  class = "main-content",
  
  # Hero Banner
  #tags$div(class = "hero-banner"),
  
  # Page Section
  tags$div(
    class = "page-section",
    
    # Title and Description
    h1(class = "page-title", "The project"),
    p(
      class = "page-subtitle",
      "Explore comprehensive data on non-salary labor costs, minimum wages, business taxes, and social assistance across Latin American countries. Dive into interactive visualizations and detailed analyses to understand regional regulatory frameworks."
    ),
    
    # Info Sections
    tags$div(
      class = "info-sections",
      
      # Contact Section
      tags$div(
        class = "info-section",
        h3(class = "info-section-title", "CONTACT"),
        tags$div(
          class = "info-section-content",
          p("For more information about this project, please contact us at:"),
          p(tags$strong("Email:"), " contact@worldbank.org"),
          p(tags$strong("Phone:"), " +1 (202) 473-1000")
        )
      ),
      
      # Team Section
      tags$div(
        class = "info-section",
        h3(class = "info-section-title", "TEAM"),
        tags$div(
          class = "info-section-content",
          p("This project was developed by a dedicated team of researchers and data scientists at the World Bank Group."),
          tags$ul(
            tags$li("Project Lead: [Name]"),
            tags$li("Data Analysis: [Name]"),
            tags$li("Development: [Name]"),
            tags$li("Design: [Name]")
          )
        )
      )
    )
  ),
  
  # Footer
  tags$div(
    class = "footer",
    tags$p(class = "footer-text", "© 2025 World Bank Group")
  )
)