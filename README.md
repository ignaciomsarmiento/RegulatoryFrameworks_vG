# Regulatory Frameworks Explorer - Technical Documentation

## Table of Contents

1. [Architecture Overview](#1-architecture-overview)
2. [Technology Stack](#2-technology-stack)
3. [Project Structure](#3-project-structure)
4. [Installation Guide](#4-installation-guide)


---

## 1. Architecture Overview

### 1.1 System Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                         Browser                              │
│  ┌──────────────────────────────────────────────────────┐  │
│  │  HTML5 + CSS3 + JavaScript (Vanilla)                 │  │
│  │  - Navigation State Management                        │  │
│  │  - Active Tab Highlighting                           │  │
│  │  - Event Handlers                                    │  │
│  └────────────────────┬─────────────────────────────────┘  │
└─────────────────────────┼─────────────────────────────────┘
                          │ HTTP/WebSocket
                          ▼
┌─────────────────────────────────────────────────────────────┐
│                    Shiny Server                              │
│  ┌──────────────────────────────────────────────────────┐  │
│  │  server.R - Reactive Logic                           │  │
│  │  - Navigation Management (reactiveValues)            │  │
│  │  - Event Observers                                   │  │
│  │  - Dynamic Content Rendering                         │  │
│  │  - Module Server Calls                               │  │
│  └──────────────────────┬───────────────────────────────┘  │
└─────────────────────────┼─────────────────────────────────┘
                          │
                          ▼
┌─────────────────────────────────────────────────────────────┐
│                    R Backend                                 │
│  ┌──────────────────────────────────────────────────────┐  │
│  │  Data Processing & Visualization                      │  │
│  │  - ggplot2, plotly (Interactive Charts)             │  │
│  │  - reactable (Interactive Tables)                    │  │
│  │  - tidyverse (Data Manipulation)                     │  │
│  └──────────────────────┬───────────────────────────────┘  │
└─────────────────────────┼─────────────────────────────────┘
                          │
                          ▼
┌─────────────────────────────────────────────────────────────┐
│                    Data Layer                                │
│  - Excel Files (.xlsx)                                      │
│  - CSV Files                                                │
│  - Located in: data/non_salary/                            │
└─────────────────────────────────────────────────────────────┘
```

### 1.2 Key Technologies

- **Frontend Framework**: Shiny (Reactive Web Framework)
- **Backend Language**: R (Statistical Computing)
- **Styling**: Custom CSS3 with World Bank Group Design Standards
- **JavaScript**: Vanilla JS (Navigation & State Management)
- **Data Visualization**: plotly, ggplot2
- **Data Tables**: reactable, DT



## 2. Technology Stack

### 2.1 Backend

**R Version**: 4.4.3 (2025-02-28)
**Platform**: x86_64-apple-darwin20
**OS**: macOS Sequoia 15.6.1

**Core Shiny Packages**:
```r
shiny       1.8.1.1     # Web application framework
shinyjs     2.1.0       # JavaScript operations from R
```





### 2.2 Frontend

**HTML5**: Semantic markup
**CSS3**: Custom styling with CSS Variables
**JavaScript**: 
- Vanilla JavaScript (no external libraries)
- Navigation state management
- Tab switching logic
- Active state highlighting

### 2.3 Design System

**World Bank Group Design Standards**:
- Color Palette:
  - Light Gray: `#F1F3F5`
  - Border Gray: `#B9BAB5`
  - Solid Blue: `#002244`
  - Bright Blue: `#00C1FF`
  - White: `#FFFFFF`
  
- Typography: 
  - Primary: 'National Park'
  - Fallback: 'Source Sans Pro', -apple-system, BlinkMacSystemFont




## 3. Project Structure

```
regulatory-frameworks-explorer/
│
├── ui.r                          # Main UI definition
├── server.r                      # Server logic
├── global.R                      # Global configuration & libraries
├── RegulatoryFrameworks.Rproj   # RStudio project file
├── README.md                     # Project documentation
├── .gitignore                    # Git ignore rules
│
├── tabs/
│   ├── ui/                       # Modular UI components
│   │   ├── guide.R              # "How to read the data" page
│   │   ├── about.R              # "The project" page
│   │   ├── forthcoming.R        # "Coming Soon" page
│   │   └── non_salary_cost.R    # Labor costs module UI
│   │
│   └── server/                   # Modular server logic
│       └── non_salary_cost.R    # Labor costs module server
│
├── data/
│   └── non_salary/               # Labor cost datasets
│       └── [Excel/CSV files]
│
└── www/                          # Static assets
    ├── styles.css               # Custom CSS styling
    └── WB.png                   # World Bank Group logo
```

### 3.1 File Descriptions

**Root Level**:
- `ui.r`: Defines the entire user interface structure, including header, navigation, and page containers
- `server.r`: Contains all server-side logic, reactive values, event handlers, and module coordination
- `global.R`: Loads all required R packages at application startup
- `RegulatoryFrameworks.Rproj`: RStudio project configuration

**tabs/ui/**: Contains modular page definitions
- `guide.R`: Static page with questions about data interpretation
- `about.R`: Static page with project information and team details
- `forthcoming.R`: Placeholder page for features under development
- `non_salary_cost.R`: Complex interactive module for labor cost analysis

**tabs/server/**: Contains server logic for modules
- `non_salary_cost.R`: Handles data processing, filtering, and visualization for labor costs

**data/non_salary/**: Data storage
- Excel files with labor cost data by country
- CSV files for additional datasets

**www/**: Static web assets
- `styles.css`: All custom CSS styling following World Bank Group design standards
- `WB.png`: Logo image used in header

---

## 4. Installation Guide

### 4.1 Prerequisites

- **R**: Version 4.4.3 or higher (tested with 4.4.3)
- **RStudio**: Latest version (recommended for development)
- **Git**: For version control (optional)
- **Operating System**: 
  - macOS (tested on macOS Sequoia 15.6.1)
  - Windows 10/11
  - Linux (Ubuntu 20.04+, Debian, etc.)




### 4.4 Running Locally

#### Method 1: RStudio (Recommended)
1. Open `RegulatoryFrameworks.Rproj` in RStudio
2. Open `ui.r` or `server.r`
3. Click **"Run App"** button (top-right corner)
4. Select "Run in Window" or "Run in Browser"

#### Method 2: R Console
```r
# Set working directory to project root
setwd("/path/to/App")

# Run the application
shiny::runApp()
```

