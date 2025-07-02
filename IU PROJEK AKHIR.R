# Load required libraries
library(shiny)
library(readr)
library(readxl)
library(dplyr)
library(ggplot2)
library(plotly)
library(rstatix)
library(bslib)
library(DT)
library(reshape2)
library(shinyFeedback)
library(rmarkdown)
library(knitr)
library(tools)
library(tinytex)
library(shinydashboard)
library(shinyWidgets)

#UI
ui <- fluidPage(
  useShinyFeedback(),
  theme = bs_theme(
    bootswatch = "superhero",
    primary = "#2E8B57",
    success = "#28A745",
    info = "#17A2B8",
    warning = "#FFC107",
    danger = "#DC3545"
  ),
  
  # Custom CSS untuk mengatur warna teks di verbatimTextOutput
  tags$head(
    tags$style(HTML("
      .shiny-text-output {
        background-color: #f8f9fa !important;
        color: #000000 !important;
        border: 1px solid #dee2e6 !important;
        border-radius: 8px !important;
        padding: 15px !important;
        font-family: 'Courier New', monospace !important;
        font-weight: bold !important;
        font-size: 14px !important;
      }
      
      pre {
        color: #000000 !important;
        background-color: #f8f9fa !important;
      }
    "))
  ),
  
  # Header 
  div(
    class = "hero-section",
    style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); 
             padding: 40px 0; margin-bottom: 30px; color: white; text-align: center;
             box-shadow: 0 4px 15px rgba(0,0,0,0.2);",
    h1("???? Uji Korelasi & Asosiasi", 
       style = "font-weight: bold; font-size: 2.5rem; margin-bottom: 10px; color: white;"),
    p("Evaluasi hubungan antar variabel numerik dan kategorik dengan visualisasi interaktif",
      style = "font-size: 1.2rem; opacity: 0.9; margin: 0; color: white;")
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 4,
      style = "background-color: #f8f9fa; border-radius: 10px; padding: 20px; box-shadow: 0 2px 10px rgba(0,0,0,0.1);",
      
      # File upload dengan styling yang lebih baik
      div(
        style = "margin-bottom: 25px;",
        h4("???? Data Input", style = "color: #212529; margin-bottom: 15px; font-weight: bold;"),
        fileInput(
          "file", 
          label = NULL,
          placeholder = "Pilih file CSV atau Excel...",
          accept = c(".csv", ".xlsx"),
          buttonLabel = "Browse...",
          multiple = FALSE
        ),
        div(
          style = "font-size: 0.9rem; color: #495057; margin-top: 5px; font-weight: 500;",
          "Format yang didukung: CSV, XLSX"
        )
      ),
      
      # Spearman section dengan card styling
      div(
        class = "analysis-card",
        style = "background: white; padding: 20px; border-radius: 8px; margin-bottom: 20px; border-left: 4px solid #17a2b8; border: 1px solid #dee2e6;",
        h4("???? Uji Korelasi Spearman", style = "color: #212529; margin-bottom: 15px; font-weight: bold;"),
        p("Untuk variabel numerik vs numerik", style = "font-size: 0.9rem; color: #495057; margin-bottom: 15px; font-weight: 500;"),
        uiOutput("spearman_ui")
      ),
      
      # Cramer's V section dengan card styling
      div(
        class = "analysis-card",
        style = "background: white; padding: 20px; border-radius: 8px; margin-bottom: 25px; border-left: 4px solid #28a745; border: 1px solid #dee2e6;",
        h4("???? Uji Asosiasi Cramér's V", style = "color: #212529; margin-bottom: 15px; font-weight: bold;"),
        p("Untuk variabel kategorik vs kategorik", style = "font-size: 0.9rem; color: #495057; margin-bottom: 15px; font-weight: 500;"),
        uiOutput("cramer_ui")
      ),
      
      # Action buttons
      div(
        style = "text-align: center;",
        actionButton(
          "analyze", 
          label = tagList(icon("play-circle"), "Lakukan Analisis"),
          class = "btn btn-success btn-lg",
          style = "width: 100%; font-weight: bold; margin-bottom: 15px; padding: 12px; color: white; background-color: #28a745; border-color: #28a745;"
        ),
        uiOutput("download_ui")
      )
    ),
    
    mainPanel(
      width = 8,
      
      # Tab panel dengan styling yang lebih baik
      tabsetPanel(
        id = "main_tabs",
        type = "pills",
        
        tabPanel(
          "Preview Data",
          icon = icon("table"),
          br(),
          div(
            style = "background: white; padding: 20px; border-radius: 10px; box-shadow: 0 2px 10px rgba(0,0,0,0.05); border: 1px solid #dee2e6;",
            DT::dataTableOutput("preview")
          )
        ),
        
        tabPanel(
          "Ringkasan Data",
          icon = icon("chart-bar"),
          br(),
          div(
            style = "background: white; padding: 20px; border-radius: 10px; box-shadow: 0 2px 10px rgba(0,0,0,0.05); border: 1px solid #dee2e6;",
            verbatimTextOutput("summary")
          )
        ),
        
        tabPanel(
          "Hasil Spearman",
          icon = icon("link"),
          br(),
          fluidRow(
            column(
              12,
              div(
                style = "background: white; padding: 20px; border-radius: 10px; box-shadow: 0 2px 10px rgba(0,0,0,0.05); margin-bottom: 20px; border: 1px solid #dee2e6;",
                h4("Hasil Uji Statistik", style = "color: #212529; margin-bottom: 15px; font-weight: bold;"),
                verbatimTextOutput("spearman_result")
              )
            )
          ),
          fluidRow(
            column(
              12,
              div(
                style = "background: white; padding: 20px; border-radius: 10px; box-shadow: 0 2px 10px rgba(0,0,0,0.05); margin-bottom: 20px; border: 1px solid #dee2e6;",
                h4("Visualisasi", style = "color: #212529; margin-bottom: 15px; font-weight: bold;"),
                plotlyOutput("spearman_plot", height = "500px")
              )
            )
          ),
          fluidRow(
            column(
              12,
              div(
                style = "background: white; padding: 20px; border-radius: 10px; box-shadow: 0 2px 10px rgba(0,0,0,0.05); border: 1px solid #dee2e6;",
                h4("Interpretasi", style = "color: #212529; margin-bottom: 15px; font-weight: bold;"),
                uiOutput("spearman_interpretation"),
                br(),
                uiOutput("spearman_detail")
              )
            )
          )
        ),
        
        tabPanel(
          "Hasil Cramér's V",
          icon = icon("puzzle-piece"),
          br(),
          fluidRow(
            column(
              12,
              div(
                style = "background: white; padding: 20px; border-radius: 10px; box-shadow: 0 2px 10px rgba(0,0,0,0.05); margin-bottom: 20px; border: 1px solid #dee2e6;",
                h4("Hasil Uji Statistik", style = "color: #212529; margin-bottom: 15px; font-weight: bold;"),
                verbatimTextOutput("cramer_result")
              )
            )
          ),
          fluidRow(
            column(
              12,
              div(
                style = "background: white; padding: 20px; border-radius: 10px; box-shadow: 0 2px 10px rgba(0,0,0,0.05); margin-bottom: 20px; border: 1px solid #dee2e6;",
                h4("Visualisasi", style = "color: #212529; margin-bottom: 15px; font-weight: bold;"),
                plotOutput("cramer_plot", height = "500px")
              )
            )
          ),
          fluidRow(
            column(
              12,
              div(
                style = "background: white; padding: 20px; border-radius: 10px; box-shadow: 0 2px 10px rgba(0,0,0,0.05); border: 1px solid #dee2e6;",
                h4("Interpretasi", style = "color: #212529; margin-bottom: 15px; font-weight: bold;"),
                uiOutput("cramer_interpretation"),
                br(),
                uiOutput("cramer_detail")
              )
            )
          )
        ),
        
        tabPanel(
          "Heatmap Korelasi",
          icon = icon("th"),
          br(),
          div(
            style = "background: white; padding: 20px; border-radius: 10px; box-shadow: 0 2px 10px rgba(0,0,0,0.05); border: 1px solid #dee2e6;",
            h4("Heatmap Korelasi Spearman", style = "color: #212529; margin-bottom: 15px; font-weight: bold;"),
            verbatimTextOutput("correlation_text"),
            br(),
            plotOutput("correlation_heatmap", height = "500px"),
            br(),
            uiOutput("heatmap_description")
          )
        )
      )
    )
  ),
  
  # Footer
  div(
    style = "background-color: #343a40; color: white; text-align: center; padding: 20px; margin-top: 40px;",
    p("© 2025 Aplikasi Uji Korelasi & Asosiasi", style = "margin: 0; font-size: 0.9rem; color: white; font-weight: 500;")
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive values untuk menyimpan hasil
  values <- reactiveValues(
    data = NULL,
    spearman_result = NULL,
    cramer_result = NULL,
    correlation_matrix = NULL,
    download_ready = FALSE
  )
  
  # Load data dengan error handling
  observeEvent(input$file, {
    tryCatch({
      ext <- tools::file_ext(input$file$name)
      if (ext == "csv") {
        values$data <- read_csv(input$file$datapath, locale = locale(encoding = "UTF-8"))
      } else if (ext == "xlsx") {
        values$data <- read_excel(input$file$datapath)
      }
      
      # Reset download status ketika file baru diupload
      values$download_ready <- FALSE
      
      # Gunakan showModal untuk menampilkan pesan sukses
      showModal(modalDialog(
        title = "??? Upload Berhasil",
        "Data berhasil dimuat!",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      
    }, error = function(e) {
      # Gunakan showModal untuk menampilkan pesan error
      showModal(modalDialog(
        title = "??? Error Loading File",
        paste("Terjadi kesalahan saat memuat file:", e$message),
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
    })
  })
  
  output$preview <- DT::renderDataTable({
    req(values$data)
    DT::datatable(
      values$data,
      options = list(
        scrollX = TRUE,
        pageLength = 10,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
      ),
      class = "display table-striped table-hover"
    )
  })
  
  output$summary <- renderPrint({
    req(values$data)
    summary(values$data)
  })
  
  output$spearman_ui <- renderUI({
    req(values$data)
    num_vars <- names(select_if(values$data, is.numeric))
    
    if(length(num_vars) < 2) {
      div(
        style = "color: #dc3545; font-style: italic; font-weight: 500;",
        "Minimal diperlukan 2 variabel numerik untuk uji Spearman"
      )
    } else {
      tagList(
        div(
          style = "margin-bottom: 10px;",
          strong("Variabel Numerik X:", style = "color: #212529; font-size: 14px; font-weight: bold;")
        ),
        selectInput(
          "spearman_x", 
          label = NULL,
          choices = num_vars,
          selected = num_vars[1]
        ),
        div(
          style = "margin-bottom: 10px; margin-top: 15px;",
          strong("Variabel Numerik Y:", style = "color: #212529; font-size: 14px; font-weight: bold;")
        ),
        selectInput(
          "spearman_y", 
          label = NULL,
          choices = num_vars,
          selected = if(length(num_vars) > 1) num_vars[2] else num_vars[1]
        )
      )
    }
  })
  
  output$cramer_ui <- renderUI({
    req(values$data)
    # Identifikasi variabel kategorik dengan lebih robust
    cat_vars <- names(values$data)[sapply(values$data, function(x) {
      is.factor(x) || is.character(x) || (is.numeric(x) && length(unique(x)) <= 10)
    })]
    
    if(length(cat_vars) < 2) {
      div(
        style = "color: #dc3545; font-style: italic; font-weight: 500;",
        "Minimal diperlukan 2 variabel kategorik untuk uji Cram?r's V"
      )
    } else {
      tagList(
        div(
          style = "margin-bottom: 10px;",
          strong("Variabel Kategorik A:", style = "color: #212529; font-size: 14px; font-weight: bold;")
        ),
        selectInput(
          "cramer_x", 
          label = NULL,
          choices = cat_vars,
          selected = cat_vars[1]
        ),
        div(
          style = "margin-bottom: 10px; margin-top: 15px;",
          strong("Variabel Kategorik B:", style = "color: #212529; font-size: 14px; font-weight: bold;")
        ),
        selectInput(
          "cramer_y", 
          label = NULL,
          choices = cat_vars,
          selected = if(length(cat_vars) > 1) cat_vars[2] else cat_vars[1]
        )
      )
    }
  })
  
  output$download_ui <- renderUI({
    # Tampilkan tombol download SETELAH analisis berhasil
    if(values$download_ready) {
      downloadButton(
        "download_report",
        label = tagList(icon("download"), "Download Laporan Word"),
        class = "btn btn-info btn-lg",
        style = "width: 100%; font-weight: bold; margin-top: 10px; padding: 12px; color: white; background-color: #17a2b8; border-color: #17a2b8;"
      )
    } else {
      div(
        style = "color: #495057; font-style: italic; text-align: center; margin-top: 10px; font-weight: 500;",
        "Lakukan analisis terlebih dahulu untuk mengunduh laporan"
      )
    }
  })
  
  output$download_report <- downloadHandler(
    filename = function() {
      paste("Laporan_Analisis_", Sys.Date(), ".docx", sep = "")
    },
    content = function(file) {
      # Show progress message menggunakan showNotification
      notification_id <- showNotification(
        "???? Sedang mempersiapkan laporan Word...", 
        duration = NULL, 
        type = "message"
      )
      
      tryCatch({
        # Create a temporary Rmd file
        tempReport <- file.path(tempdir(), "report.Rmd")
        
        # Write the RMD content to temp file
        writeLines(rmd_content, tempReport)
        
        # Render the document with proper parameters
        rmarkdown::render(
          input = tempReport,
          output_format = rmarkdown::word_document(),
          output_file = file,
          params = list(
            spearman = values$spearman_result,
            cramer = values$cramer_result,
            correlation = values$correlation_matrix
          ),
          envir = new.env(parent = globalenv()),
          quiet = TRUE
        )
        
        # Remove progress notification
        removeNotification(notification_id)
        
        # Show success notification
        showNotification(
          "??? Laporan Word berhasil diunduh!", 
          duration = 5, 
          type = "message"
        )
        
      }, error = function(e) {
        # Remove progress notification
        removeNotification(notification_id)
        
        # Show error notification
        showNotification(
          paste("??? Error membuat laporan:", e$message), 
          duration = 10, 
          type = "error"
        )
        
        print(paste("Debug error:", e$message))  # For debugging
      })
    }
  )
  
  observeEvent(input$analyze, {
    req(values$data)
    
    # Validasi input
    if(is.null(input$spearman_x) || is.null(input$spearman_y) || 
       is.null(input$cramer_x) || is.null(input$cramer_y)) {
      showNotification(
        "??? Silakan pilih variabel untuk analisis", 
        duration = 5, 
        type = "error"
      )
      return()
    }
    
    if(input$spearman_x == input$spearman_y) {
      showNotification(
        "??? Pilih variabel yang berbeda untuk uji Spearman", 
        duration = 5, 
        type = "error"
      )
      return()
    }
    
    if(input$cramer_x == input$cramer_y) {
      showNotification(
        "??? Pilih variabel yang berbeda untuk uji Cram?r's V", 
        duration = 5, 
        type = "error"
      )
      return()
    }
    
    # Show loading notification
    loading_id <- showNotification(
      "???? Sedang melakukan analisis, mohon tunggu...", 
      duration = NULL, 
      type = "message"
    )
    
    tryCatch({
      # Spearman Correlation
      spearman_data <- values$data %>% 
        select(x = all_of(input$spearman_x), y = all_of(input$spearman_y)) %>% 
        filter(!is.na(x) & !is.na(y))
      
      if(nrow(spearman_data) < 3) {
        removeNotification(loading_id)
        showNotification(
          "??? Data tidak cukup untuk uji Spearman (minimal 3 observasi)", 
          duration = 8, 
          type = "error"
        )
        return()
      }
      
      spearman_test <- cor.test(spearman_data$x, spearman_data$y, method = "spearman")
      
      values$spearman_result <- list(
        test = spearman_test, 
        x = input$spearman_x, 
        y = input$spearman_y,
        data = spearman_data
      )
      
      # Cram?r's V
      cramer_data <- values$data %>% 
        select(x = all_of(input$cramer_x), y = all_of(input$cramer_y)) %>%
        filter(!is.na(x) & !is.na(y)) %>%
        mutate(
          x = as.factor(x),
          y = as.factor(y)
        )
      
      if(nrow(cramer_data) < 4) {
        removeNotification(loading_id)
        showNotification(
          "??? Data tidak cukup untuk uji Cram?r's V (minimal 4 observasi)", 
          duration = 8, 
          type = "error"
        )
        return()
      }
      
      tbl <- table(cramer_data$x, cramer_data$y)
      
      # Pastikan tidak ada sel dengan frekuensi 0 yang berlebihan
      if(any(dim(tbl) < 2)) {
        removeNotification(loading_id)
        showNotification(
          "??? Variabel kategorik harus memiliki minimal 2 kategori", 
          duration = 8, 
          type = "error"
        )
        return()
      }
      
      chisq_test <- chisq.test(tbl, correct = FALSE)
      n <- sum(tbl)
      V <- sqrt(chisq_test$statistic / (n * (min(nrow(tbl), ncol(tbl)) - 1)))
      
      values$cramer_result <- list(
        V = as.numeric(V), 
        pval = chisq_test$p.value, 
        x = input$cramer_x, 
        y = input$cramer_y,
        data = cramer_data,
        table = tbl,
        chisq = chisq_test
      )
      
      # Correlation matrix untuk heatmap
      num_data <- values$data %>% 
        select_if(is.numeric) %>% 
        select_if(~ sum(!is.na(.)) >= 3)  # Minimal 3 observasi valid
      
      if(ncol(num_data) >= 2) {
        corr_matrix <- cor(num_data, use = "pairwise.complete.obs", method = "spearman")
        values$correlation_matrix <- round(corr_matrix, 2)
      }
      
      # Set download ready flag
      values$download_ready <- TRUE
      
      # Remove loading notification
      removeNotification(loading_id)
      
      # Show success notification
      showNotification(
        "??? Analisis berhasil diselesaikan! Lihat hasil di tab yang tersedia.", 
        duration = 8, 
        type = "message"
      )
      
    }, error = function(e) {
      # Remove loading notification
      removeNotification(loading_id)
      
      # Show error notification
      showNotification(
        paste("??? Error saat analisis:", e$message), 
        duration = 10, 
        type = "error"
      )
    })
  })
  
  
