# Heart Disease Dashboard with Local Development Files
library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)
library(DT)
library(rsconnect)

# Load jsonlite for JSON handling
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  install.packages("jsonlite")
}
library(jsonlite)

# Install digest if needed
if (!requireNamespace("digest", quietly = TRUE)) {
  install.packages("digest")
}
library(digest)

# Dashboard dimensions
DASHBOARD_WIDTH <- 1920
DASHBOARD_HEIGHT <- 1080
HEADER_HEIGHT <- DASHBOARD_HEIGHT * 0.06
VALUE_BOX_HEIGHT <- DASHBOARD_HEIGHT * 0.10
MAIN_BOX_HEIGHT <- DASHBOARD_HEIGHT * 0.52
DATA_BOX_HEIGHT <- DASHBOARD_HEIGHT * 0.25
PLOT_HEIGHT_LARGE <- MAIN_BOX_HEIGHT * 0.75
PLOT_HEIGHT_MEDIUM <- MAIN_BOX_HEIGHT * 0.65

# ==========================================
# LOCAL DEVELOPMENT DETECTION & FILE CREATION
# ==========================================

is_running_locally <- function() {
  local_indicators <- c(
    is.na(Sys.getenv("SHINYAPPS_USER", unset = NA)),  # shinyapps.io
    is.na(Sys.getenv("RSTUDIO_CONNECT_URL", unset = NA)),  # RStudio Connect
    is.na(Sys.getenv("POSIT_CLOUD", unset = NA)),  # Posit Cloud
    interactive(),  # Interactive session
    grepl("^(/home|/Users|C:|D:)", getwd())  # Local file paths
  )
  sum(local_indicators) >= 3  # Most indicators suggest local
}

create_local_files <- function() {
  if (!is_running_locally()) {
    cat("🚀 Running in deployed environment\n")
    return(invisible(NULL))
  }
  
  cat("🏠 LOCAL DEVELOPMENT DETECTED - Creating development files...\n")
  
  # 1. Create development log
  dev_log <- paste0(
    "=== HEART DISEASE DASHBOARD - DEV LOG ===\n",
    "Session Started: ", Sys.time(), "\n",
    "Working Directory: ", getwd(), "\n",
    "R Version: ", R.version.string, "\n",
    "Platform: ", R.version$platform, "\n",
    "User: ", Sys.getenv("USER", Sys.getenv("USERNAME", "Unknown")), "\n",
    "Session ID: ", substr(digest(Sys.time()), 1, 8), "\n",
    "\n=== PACKAGE STATUS ===\n"
  )
  
  packages <- c("shiny", "shinydashboard", "ggplot2", "plotly", "dplyr", "tidyr", "DT")
  for (pkg in packages) {
    version <- if (requireNamespace(pkg, quietly = TRUE)) packageVersion(pkg) else "NOT INSTALLED"
    dev_log <- paste0(dev_log, pkg, ": ", version, "\n")
  }
  
  dev_log <- paste0(dev_log, 
    "\n=== FILES IN DIRECTORY ===\n",
    paste(list.files(), collapse = "\n"), "\n",
    "\n=== SESSION ACTIVITIES ===\n",
    "App initialized at: ", Sys.time(), "\n"
  )
  
  writeLines(dev_log, "dev_session.log")
  cat("📝 Created: dev_session.log\n")
  
  # 2. Create debug configuration
  debug_config <- list(
    app_name = "Heart Disease Dashboard",
    environment = "local_development", 
    created_at = Sys.time(),
    settings = list(
      debug_mode = TRUE,
      show_warnings = TRUE,
      enable_reactlog = TRUE,
      port = 3838
    ),
    paths = list(
      working_dir = getwd(),
      data_file = "heart_disease_uci.csv",
      app_file = "app.R"
    ),
    developer_info = list(
      r_version = R.version.string,
      platform = R.version$platform,
      user = Sys.getenv("USER", "Unknown")
    )
  )
  
  writeLines(jsonlite::toJSON(debug_config, pretty = TRUE, auto_unbox = TRUE), "local_debug.json")
  cat("🔧 Created: local_debug.json\n")
  
  # 3. Create local backup directory
  backup_dir <- paste0("backup_", format(Sys.Date(), "%Y%m%d"))
  if (!dir.exists(backup_dir)) {
    dir.create(backup_dir)
    
    # Backup important files
    files_to_backup <- c("app.R", "heart_disease_uci.csv")
    for (file in files_to_backup) {
      if (file.exists(file)) {
        file.copy(file, file.path(backup_dir, paste0(file, ".backup")))
      }
    }
    cat("📁 Created backup directory:", backup_dir, "\n")
  }
  
  # 4. Create development HTML dashboard
  html_dashboard <- paste0(
    '<!DOCTYPE html>
<html>
<head>
    <title>Heart Disease Dashboard - Local Dev</title>
    <style>
        body { font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif; 
               margin: 0; padding: 20px; background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); 
               min-height: 100vh; }
        .container { max-width: 1000px; margin: 0 auto; background: white; 
                    border-radius: 15px; overflow: hidden; box-shadow: 0 20px 40px rgba(0,0,0,0.1); }
        .header { background: linear-gradient(135deg, #ff6b6b, #ee5a24); color: white; 
                 padding: 30px; text-align: center; }
        .header h1 { margin: 0; font-size: 2.5em; }
        .header p { margin: 10px 0 0 0; opacity: 0.9; }
        .grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(300px, 1fr)); gap: 20px; padding: 30px; }
        .card { background: #f8f9fa; border-radius: 10px; padding: 20px; border-left: 5px solid #3498db; }
        .card h3 { margin-top: 0; color: #2c3e50; }
        .status { display: inline-block; padding: 8px 16px; border-radius: 20px; 
                 background: #27ae60; color: white; font-weight: bold; font-size: 0.9em; }
        .file-list { background: #2c3e50; color: #ecf0f1; padding: 15px; border-radius: 8px; 
                    font-family: "Monaco", "Consolas", monospace; font-size: 0.9em; line-height: 1.4; }
        .btn { display: inline-block; padding: 12px 24px; background: #3498db; color: white; 
              text-decoration: none; border-radius: 6px; font-weight: bold; margin: 5px; }
        .btn:hover { background: #2980b9; }
        .highlight { background: #fff3cd; padding: 10px; border-radius: 5px; border-left: 4px solid #ffc107; }
    </style>
</head>
<body>
    <div class="container">
        <div class="header">
            <h1>🏠 Local Development Dashboard</h1>
            <p>Heart Disease Risk Analytics - Development Environment</p>
            <span class="status">RUNNING LOCALLY</span>
        </div>
        
        <div class="grid">
            <div class="card">
                <h3>📊 Application Status</h3>
                <p><strong>Created:</strong> ', Sys.time(), '</p>
                <p><strong>Directory:</strong> ', getwd(), '</p>
                <p><strong>R Version:</strong> ', R.version.string, '</p>
                <p><strong>Platform:</strong> ', R.version$platform, '</p>
            </div>
            
            <div class="card">
                <h3>🚀 Quick Actions</h3>
                <a href="http://127.0.0.1:3838" class="btn" target="_blank">Launch App</a>
                <a href="http://localhost:3838" class="btn" target="_blank">Alternative Port</a>
                <div class="highlight" style="margin-top: 15px;">
                    <strong>In R Console:</strong><br>
                    <code>runApp()</code> or <code>runApp(port = 3838)</code>
                </div>
            </div>
            
            <div class="card">
                <h3>📁 Project Files</h3>
                <div class="file-list">
', paste(list.files(pattern = "\\.(R|csv|json|log|html)$"), collapse = "<br>"), '
                </div>
            </div>
            
            <div class="card">
                <h3>🔧 Development Files Created</h3>
                <ul>
                    <li>📝 <strong>dev_session.log</strong> - Session activity log</li>
                    <li>🔧 <strong>local_debug.json</strong> - Debug configuration</li>
                    <li>📁 <strong>backup_', format(Sys.Date(), "%Y%m%d"), '</strong> - File backups</li>
                    <li>🌐 <strong>local_dev_dashboard.html</strong> - This dashboard</li>
                </ul>
            </div>
            
            <div class="card">
                <h3>💡 Development Tips</h3>
                <ul>
                    <li>These files are <strong>only created locally</strong></li>
                    <li>They won\'t be included in deployments</li>
                    <li>Refresh this page to update info</li>
                    <li>Check dev_session.log for detailed activity</li>
                </ul>
            </div>
            
            <div class="card">
                <h3>📋 Package Status</h3>
                <div class="file-list" style="font-size: 0.8em;">
', paste(sapply(packages, function(pkg) {
    version <- if (requireNamespace(pkg, quietly = TRUE)) packageVersion(pkg) else "❌ NOT INSTALLED"
    paste0(pkg, ": ", version)
  }), collapse = "<br>"), '
                </div>
            </div>
        </div>
    </div>
</body>
</html>'
  )
  
  writeLines(html_dashboard, "local_dev_dashboard.html")
  cat("🌐 Created: local_dev_dashboard.html\n")
  cat("   💻 Open in browser: file://", file.path(getwd(), "local_dev_dashboard.html"), "\n")
  
  # 5. Update session log
  log_update <- paste0("\nFile creation completed at: ", Sys.time(), "\n")
  cat(log_update, file = "dev_session.log", append = TRUE)
  
  cat("\n✅ LOCAL DEVELOPMENT SETUP COMPLETE!\n")
  cat("🎯 Files created for local development only\n")
  cat("🚀 Ready to start development!\n\n")
}

# ==========================================
# DATA PREPARATION WITH LOCAL FILE CREATION
# ==========================================

prepare_data <- function() {
  
  # Create local development files if running locally
  create_local_files()
  
  # Log app startup to dev file (if local)
  if (is_running_locally() && file.exists("dev_session.log")) {
    cat("App data preparation started at: ", as.character(Sys.time()), "\n", 
        file = "dev_session.log", append = TRUE)
  }
  
  # Load data
  if (file.exists("heart_disease_uci.csv")) {
    df <- read.csv("heart_disease_uci.csv", stringsAsFactors = FALSE)
    cat("📊 CSV data loaded successfully\n")
    
    # Log to dev file
    if (is_running_locally() && file.exists("dev_session.log")) {
      cat("Data loaded from CSV: ", nrow(df), " rows, ", ncol(df), " columns\n", 
          file = "dev_session.log", append = TRUE)
    }
  } else {
    cat("⚠️ heart_disease_uci.csv not found, creating sample data\n")
    set.seed(123)
    df <- data.frame(
      age = sample(30:80, 920),
      sex = sample(c("Male", "Female"), 920, replace = TRUE),
      cp = sample(c("typical angina", "atypical angina", "non-anginal", "asymptomatic"), 920, replace = TRUE),
      trestbps = sample(90:200, 920),
      chol = sample(150:400, 920),
      fbs = sample(c(TRUE, FALSE), 920, replace = TRUE),
      restecg = sample(c("normal", "having ST-T", "hypertrophy"), 920, replace = TRUE),
      thalch = sample(70:200, 920),
      exang = sample(c(TRUE, FALSE), 920, replace = TRUE),
      oldpeak = runif(920, 0, 6),
      slope = sample(1:3, 920, replace = TRUE),
      ca = sample(0:3, 920, replace = TRUE),
      thal = sample(c("normal", "fixed defect", "reversable defect"), 920, replace = TRUE),
      num = sample(0:4, 920, replace = TRUE, prob = c(0.45, 0.15, 0.15, 0.15, 0.10))
    )
    
    # Log sample data creation
    if (is_running_locally() && file.exists("dev_session.log")) {
      cat("Sample data created: ", nrow(df), " rows\n", file = "dev_session.log", append = TRUE)
    }
  }
  
  # Process data
  df$hasDisease <- ifelse(df$num > 0, "Disease", "No Disease")
  
  df <- df %>%
    mutate(
      fbs = ifelse(fbs == "", NA, fbs),
      restecg = ifelse(restecg == "", NA, restecg),
      exang = ifelse(exang == "", NA, exang),
      slope = ifelse(slope == "", NA, slope),
      thal = ifelse(thal == "", NA, thal)
    )
  
  df$fbs <- ifelse(df$fbs == TRUE, ">120 mg/dl", "<=120 mg/dl")
  df$exang <- ifelse(df$exang == TRUE, "Yes", "No")
  
  df$trestbps[df$trestbps == 0] <- NA
  df$chol[df$chol == 0] <- NA
  
  return(df)
}

# ==========================================
# SHINY UI (Your existing UI code)
# ==========================================

ui <- dashboardPage(
  dashboardHeader(
    title = "Heart Disease Risk Dashboard - A Health Crisis Visualization",
    titleWidth = DASHBOARD_WIDTH * 0.4
  ),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    # Add local development indicator
    conditionalPanel(
      condition = "true",  # Always show, but content depends on environment
      tags$div(
        id = "dev-indicator",
        style = if (is_running_locally()) {
          "position: fixed; top: 10px; right: 10px; background: #e74c3c; color: white; padding: 5px 10px; border-radius: 15px; font-size: 12px; z-index: 9999;"
        } else {
          "display: none;"
        },
        if (is_running_locally()) "🏠 LOCAL DEV" else ""
      )
    ),
    
    tags$head(
      tags$style(HTML(paste0("
        body {
          overflow: hidden !important;
          margin: 0;
          padding: 0;
        }
        
        .wrapper {
          height: ", DASHBOARD_HEIGHT, "px !important;
          width: ", DASHBOARD_WIDTH, "px !important;
          overflow: hidden !important;
          position: fixed;
          top: 0;
          left: 0;
        }
        
        .main-header {
          position: fixed;
          width: ", DASHBOARD_WIDTH, "px !important;
          z-index: 1000;
        }
        
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
          margin-left: 0 !important;
          padding-top: 50px;
          height: ", DASHBOARD_HEIGHT, "px !important;
          width: ", DASHBOARD_WIDTH, "px !important;
          overflow: hidden !important;
          position: fixed;
        }
        
        .content {
          padding: 10px;
          overflow: hidden !important;
          height: 100%;
        }
        
        .small-box {
          border-radius: 10px;
          box-shadow: 0 4px 6px rgba(0,0,0,0.1);
          height: ", VALUE_BOX_HEIGHT * 0.9, "px !important;
          margin-bottom: 10px;
        }
        
        .small-box .inner {
          padding: 5px;
        }
        
        .small-box h3 {
          font-size: ", VALUE_BOX_HEIGHT * 0.3, "px;
          margin: 5px 0;
        }
        
        .small-box p {
          font-size: ", VALUE_BOX_HEIGHT * 0.15, "px;
          margin: 0;
        }
        
        .small-box .icon {
          font-size: ", VALUE_BOX_HEIGHT * 0.5, "px;
          top: 10px;
        }
        
        .box {
          border-radius: 10px;
          box-shadow: 0 4px 6px rgba(0,0,0,0.1);
          margin-bottom: 10px;
          overflow: hidden;
        }
        
        .box-body {
          overflow-y: auto;
          overflow-x: hidden;
        }
        
        .col-sm-1, .col-sm-2, .col-sm-3, .col-sm-4, .col-sm-5, .col-sm-6,
        .col-sm-7, .col-sm-8, .col-sm-9, .col-sm-10, .col-sm-11, .col-sm-12 {
          padding-left: 5px;
          padding-right: 5px;
        }
        
        .row {
          margin-left: -5px;
          margin-right: -5px;
        }
        
        h3 {
          font-weight: bold;
          color: #2c3e50;
          font-size: ", DASHBOARD_HEIGHT * 0.022, "px;
        }
        
        h4 {
          font-size: ", DASHBOARD_HEIGHT * 0.018, "px;
        }
        
        p, .form-control, label {
          font-size: ", DASHBOARD_HEIGHT * 0.014, "px;
        }
        
        .warning-text {
          color: #e74c3c;
          font-weight: bold;
          font-size: ", DASHBOARD_HEIGHT * 0.018, "px;
        }
        
        .nav-tabs-custom > .tab-content {
          overflow: hidden;
        }
        
        .dataTables_wrapper {
          overflow: hidden;
        }
        
        .form-control {
          height: ", DASHBOARD_HEIGHT * 0.03, "px !important;
          padding: 2px 5px;
        }
        
        .btn {
          padding: 5px 10px;
          font-size: ", DASHBOARD_HEIGHT * 0.015, "px;
        }
        
        * {
          max-height: 100%;
          overflow-x: hidden;
        }
      ")))
    ),
    
    fluidRow(
      column(3,
             valueBox(
               value = "55.3%",
               subtitle = "Have Heart Disease",
               icon = icon("heart-broken"),
               color = "red",
               width = 12
             )
      ),
      column(3,
             valueBox(
               value = "63.2%",
               subtitle = "Males with Disease",
               icon = icon("male"),
               color = "yellow",
               width = 12
             )
      ),
      column(3,
             valueBox(
               value = "25.8%",
               subtitle = "Females with Disease",
               icon = icon("female"),
               color = "green",
               width = 12
             )
      ),
      column(3,
             valueBox(
               value = "920",
               subtitle = "Patients Analyzed",
               icon = icon("users"),
               color = "blue",
               width = 12
             )
      )
    ),
    
    fluidRow(
      column(4,
             box(
               title = "Risk Factor Analysis",
               status = "danger",
               solidHeader = TRUE,
               width = 12,
               height = paste0(MAIN_BOX_HEIGHT, "px"),
               
               h4("Select Risk Factor to Analyze:"),
               selectInput("riskFactor", 
                           label = NULL,
                           choices = list(
                             "Age" = "age",
                             "Chest Pain Type (CP)" = "cp",
                             "Resting Blood Pressure (trestbps)" = "trestbps",
                             "Cholesterol (chol)" = "chol",
                             "Fasting Blood Sugar (fbs)" = "fbs",
                             "Resting ECG Results (restecg)" = "restecg",
                             "Max Heart Rate Achieved (thalch)" = "thalch",
                             "Exercise Induced Angina (exang)" = "exang"
                           ),
                           selected = "age"
               ),
               
               plotlyOutput("riskFactorPlot", height = paste0(PLOT_HEIGHT_MEDIUM, "px")),
               
               hr(),
               
               h4("Risk Factor Description:"),
               htmlOutput("riskFactorDesc")
             )
      ),
      
      column(4,
             box(
               title = "Heart Disease Distribution & Correlations",
               status = "primary",
               solidHeader = TRUE,
               width = 12,
               height = paste0(MAIN_BOX_HEIGHT, "px"),
               
               tabsetPanel(
                 tabPanel("Overview",
                          plotlyOutput("overviewPlot", height = paste0(PLOT_HEIGHT_LARGE, "px")),
                          br(),
                          p(class = "warning-text", 
                            "⚠️ Over half of patients show signs of heart disease!")
                 ),
                 tabPanel("Correlations",
                          plotlyOutput("correlationPlot", height = paste0(PLOT_HEIGHT_LARGE * 1.1, "px"))
                 ),
                 tabPanel("Age Groups",
                          plotlyOutput("ageGroupPlot", height = paste0(PLOT_HEIGHT_LARGE * 1.1, "px"))
                 )
               )
             )
      ),
      
      column(4,
             box(
               title = "Personal Risk Assessment",
               status = "warning",
               solidHeader = TRUE,
               width = 12,
               height = paste0(MAIN_BOX_HEIGHT, "px"),
               
               h4("Enter Your Health Metrics:"),
               
               numericInput("userAge", "Age:", value = 50, min = 20, max = 80),
               
               selectInput("userSex", "Sex:", 
                           choices = c("Male", "Female")),
               
               selectInput("userCP", "Chest Pain Type:",
                           choices = c("typical angina", "atypical angina", 
                                       "non-anginal", "asymptomatic")),
               
               numericInput("userBP", "Resting Blood Pressure (mm Hg):", 
                            value = 120, min = 80, max = 200),
               
               numericInput("userChol", "Cholesterol (mg/dl):", 
                            value = 200, min = 100, max = 400),
               
               br(),
               
               actionButton("assessRisk", "Assess My Risk", 
                            class = "btn-warning btn-lg btn-block"),
               
               br(),
               
               htmlOutput("riskAssessment")
             )
      )
    ),
    
    fluidRow(
      column(12,
             box(
               title = "Dataset Information & Sample Data",
               status = "info",
               solidHeader = TRUE,
               collapsible = TRUE,
               collapsed = FALSE,
               width = 12,
               height = paste0(DATA_BOX_HEIGHT, "px"),
               
               fluidRow(
                 column(6,
                        p("Data Source: UCI Machine Learning Repository - Heart Disease Dataset"),
                        p("This dataset contains 920 patient records from multiple locations."),
                        
                        h4("Column Descriptions:"),
                        tags$div(style = paste0("font-size: ", DASHBOARD_HEIGHT * 0.012, "px; column-count: 2;"),
                                 tags$ul(style = "margin: 0; padding-left: 20px;",
                                         tags$li("age: Age in years"),
                                         tags$li("sex: Gender (Male/Female)"),
                                         tags$li("cp: Chest Pain Type"),
                                         tags$li("trestbps: Resting Blood Pressure (mm Hg)"),
                                         tags$li("chol: Serum Cholesterol (mg/dl)"),
                                         tags$li("fbs: Fasting Blood Sugar > 120 mg/dl"),
                                         tags$li("restecg: Resting ECG Results"),
                                         tags$li("thalch: Maximum Heart Rate Achieved"),
                                         tags$li("exang: Exercise Induced Angina"),
                                         tags$li("oldpeak: ST Depression"),
                                         tags$li("slope: Slope of Peak Exercise ST"),
                                         tags$li("ca: Number of Major Vessels"),
                                         tags$li("thal: Thalassemia"),
                                         tags$li("num: Disease Diagnosis (0=no, 1-4=yes)")
                                 )
                        ),
                        
                        # Show development status only when local
                        conditionalPanel(
                          condition = "true",
                          if (is_running_locally()) {
                            tags$div(
                              br(),
                              h4("🏠 Local Development Status:"),
                              verbatimTextOutput("devStatus")
                            )
                          } else {
                            tags$div()
                          }
                        )
                 ),
                 column(6,
                        h4("Sample Data (First 5 Rows):"),
                        div(style = paste0("height: ", DATA_BOX_HEIGHT * 0.6, "px; overflow: hidden;"),
                            DT::dataTableOutput("sampleData")
                        )
                 )
               )
             )
      )
    )
  )
)

# ==========================================
# SHINY SERVER (Your existing server code + dev status)
# ==========================================

server <- function(input, output, session) {
  
  heart_data <- reactive({
    prepare_data()
  })
  
  # Development status output (only shown when local)
  output$devStatus <- renderText({
    if (!is_running_locally()) return("")
    
    dev_files <- c("dev_session.log", "local_debug.json", "local_dev_dashboard.html")
    existing_files <- dev_files[file.exists(dev_files)]
    
    paste0(
      "✅ Environment: LOCAL DEVELOPMENT\n",
      "📁 Dev files created: ", length(existing_files), "/", length(dev_files), "\n",
      "🕐 Session started: ", format(Sys.time(), "%H:%M:%S"), "\n",
      "📝 Log file: ", if("dev_session.log" %in% existing_files) "Available" else "Not found", "\n",
      "🌐 Dashboard: ", if("local_dev_dashboard.html" %in% existing_files) "local_dev_dashboard.html" else "Not created"
    )
  })
  
  # Log server startup to dev file
  if (is_running_locally() && file.exists("dev_session.log")) {
    cat("Shiny server started at: ", as.character(Sys.time()), "\n", 
        file = "dev_session.log", append = TRUE)
  }
  
  # Your existing server code continues here...
  output$sampleData <- DT::renderDataTable({
    df <- heart_data()
    
    display_df <- df %>%
      select(age, sex, cp, trestbps, chol, fbs, thalch, hasDisease) %>%
      head(5)
    
    DT::datatable(
      display_df,
      options = list(
        dom = 't',
        paging = FALSE,
        searching = FALSE,
        info = FALSE,
        scrollY = FALSE,
        scrollX = TRUE,
        columnDefs = list(
          list(className = 'dt-center', targets = '_all')
        )
      ),
      rownames = FALSE,
      class = 'compact'
    )
  })
  
  output$riskFactorPlot <- renderPlotly({
    df <- heart_data()
    
    if(input$riskFactor %in% c("age", "trestbps", "chol", "thalch")) {
      p <- df %>%
        filter(!is.na(!!sym(input$riskFactor))) %>%
        ggplot(aes(x = !!sym(input$riskFactor), fill = hasDisease)) +
        geom_histogram(bins = 20, alpha = 0.7, position = "dodge") +
        scale_fill_manual(values = c("No Disease" = "#27ae60", "Disease" = "#e74c3c")) +
        labs(
          x = case_when(
            input$riskFactor == "age" ~ "Age (years)",
            input$riskFactor == "trestbps" ~ "Resting Blood Pressure (mm Hg)",
            input$riskFactor == "chol" ~ "Cholesterol (mg/dl)",
            input$riskFactor == "thalch" ~ "Maximum Heart Rate"
          ),
          y = "Number of Patients",
          fill = "Diagnosis"
        ) +
        theme_minimal() +
        theme(text = element_text(size = DASHBOARD_HEIGHT * 0.012))
    } else {
      p <- df %>%
        filter(!is.na(!!sym(input$riskFactor))) %>%
        group_by(!!sym(input$riskFactor), hasDisease) %>%
        summarize(count = n(), .groups = "drop") %>%
        group_by(!!sym(input$riskFactor)) %>%
        mutate(percentage = count / sum(count) * 100) %>%
        ggplot(aes(x = !!sym(input$riskFactor), y = percentage, fill = hasDisease)) +
        geom_bar(stat = "identity", position = "dodge") +
        scale_fill_manual(values = c("No Disease" = "#27ae60", "Disease" = "#e74c3c")) +
        labs(
          x = case_when(
            input$riskFactor == "cp" ~ "Chest Pain Type",
            input$riskFactor == "fbs" ~ "Fasting Blood Sugar",
            input$riskFactor == "restecg" ~ "Resting ECG Results",
            input$riskFactor == "exang" ~ "Exercise Induced Angina"
          ),
          y = "Percentage (%)",
          fill = "Diagnosis"
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              text = element_text(size = DASHBOARD_HEIGHT * 0.012))
    }
    
    ggplotly(p, height = PLOT_HEIGHT_MEDIUM) %>% 
      layout(
        margin = list(l = 40, r = 20, t = 20, b = 40),
        font = list(size = DASHBOARD_HEIGHT * 0.012)
      )
  })
  
  output$riskFactorDesc <- renderUI({
    desc <- switch(input$riskFactor,
                   "age" = "Age is a major risk factor. Heart disease risk increases significantly after age 45 for men and 55 for women.",
                   "cp" = "Chest Pain Types: Typical angina is classic heart-related pain. Atypical angina is less typical. Non-anginal pain is not heart-related. Asymptomatic means no chest pain.",
                   "trestbps" = "Resting blood pressure above 140/90 mm Hg is considered high and increases heart disease risk.",
                   "chol" = "High cholesterol (>240 mg/dl) significantly increases heart disease risk by causing plaque buildup in arteries.",
                   "fbs" = "Fasting blood sugar >120 mg/dl indicates diabetes or pre-diabetes, major risk factors for heart disease.",
                   "restecg" = "Resting ECG can show heart abnormalities. LV hypertrophy and ST-T abnormalities indicate heart stress.",
                   "thalch" = "Maximum heart rate achieved during exercise. Lower max heart rate may indicate poor cardiovascular fitness.",
                   "exang" = "Exercise-induced angina (chest pain) is a strong indicator of coronary artery disease."
    )
    HTML(paste0("<p style='color: #34495e; font-size: ", DASHBOARD_HEIGHT * 0.013, "px;'>", desc, "</p>"))
  })
  
  output$overviewPlot <- renderPlotly({
    df <- heart_data()
    
    summary_df <- df %>%
      mutate(age_group = cut(age, breaks = c(0, 40, 50, 60, 70, 80), 
                             labels = c("<40", "40-50", "50-60", "60-70", "70+"))) %>%
      group_by(sex, age_group, hasDisease) %>%
      summarize(count = n(), .groups = "drop") %>%
      group_by(sex, age_group) %>%
      mutate(total = sum(count),
             percentage = count / total * 100) %>%
      filter(hasDisease == "Disease")
    
    p <- ggplot(summary_df, aes(x = age_group, y = percentage, fill = sex)) +
      geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
      scale_fill_manual(values = c("Male" = "#3498db", "Female" = "#e74c3c")) +
      geom_hline(yintercept = 50, linetype = "dashed", color = "red", linewidth = 1) +
      annotate("text", x = 3, y = 55, label = "50% Disease Threshold", color = "red",
               size = DASHBOARD_HEIGHT * 0.004) +
      labs(
        title = "Heart Disease Prevalence by Age and Gender",
        x = "Age Group",
        y = "Disease Prevalence (%)",
        fill = "Gender"
      ) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            text = element_text(size = DASHBOARD_HEIGHT * 0.012))
    
    ggplotly(p, height = PLOT_HEIGHT_LARGE) %>% 
      layout(
        margin = list(l = 40, r = 20, t = 40, b = 40),
        font = list(size = DASHBOARD_HEIGHT * 0.012)
      )
  })
  
  output$correlationPlot <- renderPlotly({
    df <- heart_data()
    
    numeric_vars <- df %>%
      select(age, trestbps, chol, thalch, oldpeak) %>%
      na.omit()
    
    cor_matrix <- cor(numeric_vars)
    
    plot_ly(
      x = colnames(cor_matrix),
      y = colnames(cor_matrix),
      z = cor_matrix,
      type = "heatmap",
      colorscale = "RdBu",
      zmin = -1, zmax = 1,
      text = round(cor_matrix, 2),
      texttemplate = "%{text}",
      textfont = list(size = DASHBOARD_HEIGHT * 0.015),
      height = PLOT_HEIGHT_LARGE * 1.1
    ) %>%
      layout(
        title = list(
          text = "Correlation Between Numeric Risk Factors",
          font = list(size = DASHBOARD_HEIGHT * 0.018)
        ),
        xaxis = list(title = "", tickfont = list(size = DASHBOARD_HEIGHT * 0.012)),
        yaxis = list(title = "", tickfont = list(size = DASHBOARD_HEIGHT * 0.012)),
        margin = list(l = 80, r = 20, t = 40, b = 40)
      )
  })
  
  output$ageGroupPlot <- renderPlotly({
    df <- heart_data()
    
    age_summary <- df %>%
      mutate(age_group = cut(age, breaks = seq(20, 80, by = 10), include.lowest = TRUE)) %>%
      group_by(age_group) %>%
      summarize(
        total = n(),
        disease_count = sum(hasDisease == "Disease"),
        disease_rate = disease_count / total * 100,
        .groups = "drop"
      )
    
    p <- ggplot(age_summary, aes(x = age_group)) +
      geom_bar(aes(y = total), stat = "identity", fill = "#3498db", alpha = 0.5) +
      geom_line(aes(y = disease_rate * max(total) / 100, group = 1), 
                color = "#e74c3c", linewidth = 2) +
      geom_point(aes(y = disease_rate * max(total) / 100), 
                 color = "#e74c3c", size = 4) +
      scale_y_continuous(
        name = "Number of Patients",
        sec.axis = sec_axis(~ . * 100 / max(age_summary$total), 
                            name = "Disease Rate (%)")
      ) +
      labs(
        title = "Patient Count and Disease Rate by Age Group",
        x = "Age Group"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.y.right = element_text(color = "#e74c3c"),
        axis.text.y.right = element_text(color = "#e74c3c"),
        text = element_text(size = DASHBOARD_HEIGHT * 0.012)
      )
    
    ggplotly(p, height = PLOT_HEIGHT_LARGE * 1.1) %>% 
      layout(
        margin = list(l = 50, r = 50, t = 40, b = 60),
        font = list(size = DASHBOARD_HEIGHT * 0.012)
      )
  })
  
  output$riskAssessment <- renderUI({
    req(input$assessRisk)
    
    # Log risk assessment to dev file
    if (is_running_locally() && file.exists("dev_session.log")) {
      cat("Risk assessment performed at: ", as.character(Sys.time()), 
          " for age:", input$userAge, "sex:", input$userSex, "\n", 
          file = "dev_session.log", append = TRUE)
    }
    
    risk_score <- 0
    risk_factors <- c()
    
    if(input$userAge >= 45 & input$userSex == "Male") {
      risk_score <- risk_score + 20
      risk_factors <- c(risk_factors, "Age ≥ 45 (Male)")
    }
    if(input$userAge >= 55 & input$userSex == "Female") {
      risk_score <- risk_score + 20
      risk_factors <- c(risk_factors, "Age ≥ 55 (Female)")
    }
    
    if(input$userSex == "Male") {
      risk_score <- risk_score + 15
      risk_factors <- c(risk_factors, "Male gender")
    }
    
    if(input$userCP == "typical angina" | input$userCP == "asymptomatic") {
      risk_score <- risk_score + 25
      risk_factors <- c(risk_factors, paste("Chest pain:", input$userCP))
    }
    
    if(input$userBP >= 140) {
      risk_score <- risk_score + 20
      risk_factors <- c(risk_factors, "High blood pressure")
    }
    
    if(input$userChol >= 240) {
      risk_score <- risk_score + 20
      risk_factors <- c(risk_factors, "High cholesterol")
    }
    
    risk_level <- case_when(
      risk_score >= 60 ~ "HIGH",
      risk_score >= 30 ~ "MODERATE",
      TRUE ~ "LOW"
    )
    
    risk_color <- case_when(
      risk_level == "HIGH" ~ "#e74c3c",
      risk_level == "MODERATE" ~ "#f39c12",
      TRUE ~ "#27ae60"
    )
    
    HTML(paste0(
      "<div style='padding: 10px; border-radius: 10px; background-color: ", risk_color, "20;'>",
      "<h3 style='color: ", risk_color, "; text-align: center; font-size: ", DASHBOARD_HEIGHT * 0.025, "px;'>",
      "Risk Level: ", risk_level, " (", risk_score, "%)</h3>",
      "<h4 style='font-size: ", DASHBOARD_HEIGHT * 0.018, "px;'>Your Risk Factors:</h4>",
      "<ul style='font-size: ", DASHBOARD_HEIGHT * 0.014, "px;'>",
      paste0("<li>", risk_factors, "</li>", collapse = ""),
      "</ul>",
      "<p style='font-weight: bold; font-size: ", DASHBOARD_HEIGHT * 0.016, "px;'>",
      ifelse(risk_level == "HIGH", 
             "⚠️ Please consult a healthcare provider immediately!",
             ifelse(risk_level == "MODERATE",
                    "Consider lifestyle changes and regular check-ups.",
                    "✓ Keep up the healthy lifestyle!")),
      "</p>",
      "<p style='font-size: ", DASHBOARD_HEIGHT * 0.012, "px; color: #7f8c8d;'>",
      "Note: This is a simplified assessment. Always consult healthcare professionals for accurate diagnosis.",
      "</p>",
      "</div>"
    ))
  })
}

# Print startup message
if (is_running_locally()) {
  cat("🏠 STARTING IN LOCAL DEVELOPMENT MODE\n")
  cat("=====================================\n")
  cat("📁 Development files will be created automatically\n")
  cat("🌐 Check local_dev_dashboard.html for full development info\n")
  cat("📝 Activity logged to dev_session.log\n")
} else {
  cat("🚀 STARTING IN PRODUCTION MODE\n")
  cat("==============================\n")
  cat("📦 No development files will be created\n")
}

cat("✅ Heart Disease Dashboard Ready!\n\n")

shinyApp(ui = ui, server = server)