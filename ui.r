# ============================
# UI sources for each page
# ============================


source("tabs/ui/guide.R", local = TRUE)
source("tabs/ui/about.R", local = TRUE)
source("tabs/ui/forthcoming.R", local = TRUE) 

# ============================
# MAIN UI
# ============================

shinyUI(
  fluidPage(
    shinyjs::useShinyjs(),
    
    # ---- HEAD ----
    tags$head(
      tags$title("Regulatory Frameworks Explorer"),
      tags$link(
        href = "https://fonts.googleapis.com/css2?family=National+Park&display=swap",
        rel = "stylesheet"
      ),
      includeCSS("www/styles.css"),
      includeCSS("www/labor.css"),
      
      # ---- CSS + JS FOR LOADING MESSAGES ON OUTPUTS ----
      tags$style(HTML("
        /* Loading message box */
        .output-loading-msg {
          display: none;
          align-items: center;
          justify-content: center;
          gap: 14px;
          padding: 48px 20px;
          width: 100%;
          background: rgba(255,255,255,0.95);
          border-radius: 8px;
        }
        .output-loading-msg.is-visible {
          display: flex !important;
        }
        .output-loading-msg .ld-dots {
          display: inline-flex;
          gap: 6px;
        }
        .output-loading-msg .ld-dots span {
          width: 10px;
          height: 10px;
          border-radius: 50%;
          background-color: #00C1FF;
          animation: ldBounce 1.2s ease-in-out infinite;
        }
        .output-loading-msg .ld-dots span:nth-child(2) { animation-delay: 0.2s; }
        .output-loading-msg .ld-dots span:nth-child(3) { animation-delay: 0.4s; }
        @keyframes ldBounce {
          0%, 80%, 100% { opacity: 0.3; transform: scale(0.8); }
          40% { opacity: 1; transform: scale(1.2); }
        }
        .output-loading-msg .ld-text {
          font-size: 15px;
          font-weight: 500;
          color: #1e3a5f;
          font-family: 'National Park', sans-serif;
        }
      ")),
      
      # JS that listens for Shiny recalculating/idle events on ANY output.
      # Works even when the outputs are created dynamically inside renderUI.
      tags$script(HTML("
        $(document).on('shiny:recalculating', function(e) {
          var id = e.target.id || '';
          // Match the plot and table outputs (namespaced as labor-plot, labor-tabla_detalle)
          if (id.indexOf('-plot') !== -1) {
            var msg = document.getElementById(id.replace('-plot', '-loading_plot_msg'));
            if (msg) msg.classList.add('is-visible');
          }
          if (id.indexOf('-tabla_detalle') !== -1) {
            var msg = document.getElementById(id.replace('-tabla_detalle', '-loading_table_msg'));
            if (msg) msg.classList.add('is-visible');
          }
        });

        $(document).on('shiny:value shiny:error', function(e) {
          var id = e.target.id || '';
          if (id.indexOf('-plot') !== -1) {
            var msg = document.getElementById(id.replace('-plot', '-loading_plot_msg'));
            if (msg) msg.classList.remove('is-visible');
          }
          if (id.indexOf('-tabla_detalle') !== -1) {
            var msg = document.getElementById(id.replace('-tabla_detalle', '-loading_table_msg'));
            if (msg) msg.classList.remove('is-visible');
          }
        });
      ")),
      
      # ---- JAVASCRIPT TO CONTROL TABS ----
      tags$script(HTML("
        Shiny.addCustomMessageHandler('trigger-download', function(id) {
          var el = document.getElementById(id);
          if (el) el.click();
        });
      ")),
      tags$script(HTML("
      document.addEventListener('DOMContentLoaded', function() {
    
        // Function to update active state
        function updateActiveNav(activeTab) {
          // Remove active class from all nav links
          document.querySelectorAll('.nav-link').forEach(function(link) {
            link.classList.remove('active');
          });
          
          // Add active class to the current tab
          var activeLink = document.querySelector('.nav-link[data-tab=\"' + activeTab + '\"]');
          if (activeLink) {
            activeLink.classList.add('active');
          }
        }
    
        // Set initial active state (landing page)
        updateActiveNav('landing');
    
        // Attach click handler to each header nav link
        document.querySelectorAll('.nav-link').forEach(function(link) {
          link.addEventListener('click', function(e) {
            e.preventDefault();
    
            // Get the tab name from data-tab attribute
            var tab = this.getAttribute('data-tab');
    
            // Update active state immediately
            updateActiveNav(tab);
    
            // Find the hidden Shiny tab button that matches this name
            var tabButton = document.querySelector(
              'a[data-value=\"' + tab + '\"]'
            );
    
            // Simulate a click to switch the tab
            if (tabButton) {
              tabButton.click();
            }
          });
        });
    
        // Listen for tab changes (in case tabs are changed programmatically)
        var observer = new MutationObserver(function(mutations) {
          mutations.forEach(function(mutation) {
            if (mutation.attributeName === 'class') {
              var tabs = document.querySelectorAll('#main_tabs > .tab-pane');
              tabs.forEach(function(tab) {
                if (tab.classList.contains('active')) {
                  var tabValue = tab.getAttribute('data-value');
                  if (tabValue) {
                    updateActiveNav(tabValue);
                  }
                }
              });
            }
          });
        });
    
        // Observe tab changes
        var tabPanes = document.querySelectorAll('#main_tabs > .tab-pane');
        tabPanes.forEach(function(pane) {
          observer.observe(pane, { attributes: true });
        });
    
      });
    ")),
      tags$script(HTML("
      document.addEventListener('DOMContentLoaded', function() {
        var TAB_PARAM = 'tab';
        var tabContainer = document.getElementById('main_tabs');
        var isHistoryNav = false;
        var currentTab = null;

        function getActiveTab() {
          if (!tabContainer) return null;
          var active = tabContainer.querySelector('.tab-pane.active');
          return active ? active.getAttribute('data-value') : null;
        }

        function syncUrl(tab, replace) {
          if (!tab) return;
          var url = new URL(window.location);
          url.searchParams.set(TAB_PARAM, tab);
          var method = replace ? 'replaceState' : 'pushState';
          window.history[method]({ tab: tab }, '', url);
        }

        function switchToTab(tab, fromPop) {
          if (!tab) return;
          if (fromPop) isHistoryNav = true;
          if (tab === currentTab) {
            if (fromPop) isHistoryNav = false;
            return;
          }
          var btn = document.querySelector('a[data-value=\"' + tab + '\"]');
          if (btn) btn.click();
        }

        // Hook Bootstrap tab events (Shiny uses BS tabs under the hood)
        if (window.jQuery) {
          window.jQuery(document).on('shown.bs.tab', 'a[data-toggle=\"tab\"]', function(e) {
            var tab = window.jQuery(e.target).data('value');
            if (!tab) return;
            if (isHistoryNav) {
              syncUrl(tab, true);
              isHistoryNav = false;
            } else if (tab !== currentTab) {
              syncUrl(tab, false);
            }
            currentTab = tab;
          });
        }

        if (tabContainer) {
          var observer = new MutationObserver(function() {
            var tab = getActiveTab();
            if (!tab) return;
            if (tab === currentTab) {
              if (isHistoryNav) isHistoryNav = false;
              return;
            }
            if (isHistoryNav) {
              syncUrl(tab, true);
              isHistoryNav = false;
            } else {
              syncUrl(tab, false);
            }
            currentTab = tab;
          });
          tabContainer.querySelectorAll('.tab-pane').forEach(function(pane) {
            observer.observe(pane, { attributes: true, attributeFilter: ['class'] });
          });
        }

        var initialTab = new URL(window.location).searchParams.get(TAB_PARAM) || getActiveTab() || 'landing';
        syncUrl(initialTab, true);
        isHistoryNav = true;
        switchToTab(initialTab, true);
        setTimeout(function() { isHistoryNav = false; }, 0);
        currentTab = initialTab;

        window.addEventListener('popstate', function(event) {
          var tabFromState = event.state && event.state.tab;
          var tabFromUrl = new URL(window.location).searchParams.get(TAB_PARAM);
          switchToTab(tabFromState || tabFromUrl || 'landing', true);
        });
      });
    "))
    ),
    tags$script(HTML("
  document.addEventListener('DOMContentLoaded', function () {
    var btn = document.querySelector('.hamburger-btn');
    var menu = document.querySelector('.hamburger-dropdown');

    btn.addEventListener('click', function (e) {
      e.stopPropagation();
      menu.classList.toggle('hidden');
    });

    document.addEventListener('click', function () {
      menu.classList.add('hidden');
    });
  });
")),
    
    
    # ---- HEADER ----
    tags$div(
      class = "header",
      tags$div(
        class = "header-content",
        
        # Logo (izquierda)
        tags$img(
          src = "WB.png",
          class = "wb-logo",
          style = "cursor: pointer;",
          onclick = "document.querySelector('a[data-value=\"landing\"]').click();"
        ),
        
        # Spacer (centro)
        tags$div(),
        
        # Hamburger (derecha)
        tags$div(
          class = "hamburger-menu",
          
          tags$div(
            class = "hamburger-btn",
            HTML("&#9776;")
          ),
          
          tags$div(
            class = "hamburger-dropdown hidden",
            
            tags$a(
              "Home",
              class = "nav-link",
              onclick = "document.querySelector('a[data-value=\"landing\"]').click();"
            ),
            
            tags$a(
              "Non-Salary Labor Costs",
              class = "nav-link",
              onclick = "Shiny.setInputValue('topic_selected', 'labor', {priority: 'event'})"
            ),
            tags$a(
              "About",
              class = "nav-link",
              onclick = "document.querySelector('a[data-value=\"About\"]').click();"
            ),
            
          )
        )
      )
    ),
    
    # ---- MAIN BODY ----
    div(
      class = "main-content",
      tabsetPanel(
        id = "main_tabs",
        type="hidden",
        selected = "landing",
        
        # ============================
        # 1. LANDING PAGE
        # ============================
        tabPanel(
          "landing",
          tags$div(
            class = "landing-container",
            
            tags$div(
              class = "landing-left-col",
              tags$div(class = "landing-image-box")
            ),
            
            tags$div(
              class = "landing-right-col",
              
              tags$div(
                class = "landing-header-row",
                tags$div(class = "landing-eyebrow", "WELCOME TO "),
                tags$div(class = "landing-title", h1("The Market and Social Protection Regulatory Frameworks Explorer"))
              ),
              
              tags$div(
                class = "landing-desc-row",
                p("Explore a comprehensive inventory on non-salary labor costs in eleven Latin American countries. Dive into the regulations using interactive visualizations to understand their regional frameworks."
                )
              ),
              
              tags$div(
                class = "landing-widget-grid",
                
                tags$div(
                  class = "widget-icon",
                  tags$img(src = "Flecha.png")
                ),
                
                tags$a(
                  class = "widget-card",
                  href = "#",
                  onclick = "Shiny.setInputValue('topic_selected', 'labor', {priority: 'event'});",
                  h3("Non-Salary Labor Costs"),
                  p("Social security and insurance contributions, labor taxes and other costs related to employing.")
                ),
                
                tags$a(
                  class = "widget-btn",
                  href = "#",
                  onclick = "Shiny.setInputValue('topic_selected', 'labor', {priority: 'event'});",
                  "Explore"
                )
              )
            )
          ),
          
          tags$div(class = "footer", tags$p("© 2026 World Bank Group"))
        ),
        
        # ============================
        # 3. About
        # ============================
        tabPanel(
          "About", about
        ),
        
        # ============================
        # 4. CONTENT MODULE PAGE
        # ============================
        tabPanel(
          "content",
          div(
            class = "content-area",
            uiOutput("dynamic_content")
          ),
          tags$div(
            class = "footer",
            tags$p(class = "footer-text", "© 2026 World Bank Group")
          )
        )
      )
    )
  )
)