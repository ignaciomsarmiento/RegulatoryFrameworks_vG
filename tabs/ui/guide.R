guide <- tags$div(
  class = "main-content",
  
  # Hero Banner
  #tags$div(class = "hero-banner"),
  
  # Page Section
  tags$div(
    class = "page-section",
    
    # Title and Description
    h1(class = "page-title", "How to read the data"),
    p(
      class = "page-subtitle",
      "Explore comprehensive data on non-salary labor costs, minimum wages, business taxes, and social assistance across Latin American countries. Dive into interactive visualizations and detailed analyses to understand regional regulatory frameworks."
    ),
    
    # Questions List
    tags$div(
      class = "questions-list",
      
      tags$div(
        class = "question-item",
        tags$p(class = "question-text", "QUESTION 1")
      ),
      
      tags$div(
        class = "question-item",
        tags$p(class = "question-text", "QUESTION 2")
      ),
      
      tags$div(
        class = "question-item",
        tags$p(class = "question-text", "QUESTION 3")
      ),
      
      tags$div(
        class = "question-item",
        tags$p(class = "question-text", "QUESTION 4")
      ),
      
      tags$div(
        class = "question-item",
        tags$p(class = "question-text", "QUESTION 5")
      ),
      
      tags$div(
        class = "question-item",
        tags$p(class = "question-text", "QUESTION 6")
      )
    )
  ),
  
  # Footer
  tags$div(
    class = "footer",
    tags$p(class = "footer-text", "© 2025 World Bank Group")
  )
)
