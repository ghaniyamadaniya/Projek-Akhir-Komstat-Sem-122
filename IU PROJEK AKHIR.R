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
