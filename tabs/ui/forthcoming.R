forthcoming <- fluidPage(
  tags$div(
    class = "main-content",
    
    # Hero Banner
    tags$div(class = "hero-banner"),
    
    # Page Section
    tags$div(
      class = "page-section",
      style = "text-align: center;",
      
      # Icon o imagen
      tags$div(
        style = "font-size: 5rem; color: var(--bright-blue); margin-bottom: 30px;",
        "🚧"
      ),
      
      # Title
      h1(
        class = "page-title",
        style = "text-align: center;",
        "Coming Soon"
      ),
      
      # Description
      p(
        class = "page-subtitle",
        style = "text-align: center; max-width: 600px; margin: 0 auto 40px auto;",
        "This feature is currently under development and will be available in an upcoming release. Stay tuned!"
      ),
      
      
      # Back button
      tags$div(
        style = "margin-top: 60px;",
        tags$button(
          class = "btn btn-primary",
          onclick = "var tabBtn = document.querySelector('a[data-value=\"landing\"]'); if (tabBtn) { if (window.jQuery && window.jQuery(tabBtn).tab) { window.jQuery(tabBtn).tab('show'); } tabBtn.click(); }",
          "← Back to Home"
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
