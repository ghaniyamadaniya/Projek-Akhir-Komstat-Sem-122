# Load library
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

# UI
ui <- fluidPage(
  useShinyFeedback(),
  theme = bs_theme(bootswatch = "superhero"),
  titlePanel("Uji Korelasi Spearman dan Asosiasi Cramér's V"),
  p("Aplikasi ini digunakan untuk mengevaluasi hubungan antara dua variabel numerik (Spearman) atau dua variabel kategorik (Cramér's V). Unggah data Anda dan pilih variabel yang ingin diuji."),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Unggah File CSV / Excel", accept = c(".csv", ".xlsx")),
      h4("Uji Korelasi Spearman (Numerik vs Numerik)"),
      uiOutput("spearman_ui"),
      h4("Uji Asosiasi Cramér's V (Kategori vs Kategori)"),
      uiOutput("cramer_ui"),
      actionButton("analyze", label = tagList(icon("play"), "Lakukan Analisis"), class = "btn btn-success"),
      br(), br(),
      uiOutput("download_ui")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Preview Data", DT::dataTableOutput("preview")),
        tabPanel("Ringkasan Data", verbatimTextOutput("summary")),
        tabPanel("Spearman",
                 verbatimTextOutput("spearman_result"),
                 plotlyOutput("spearman_plot"),
                 uiOutput("spearman_interpretation"),
                 uiOutput("spearman_detail")),
        tabPanel("Cramér's V",
                 verbatimTextOutput("cramer_result"),
                 plotOutput("cramer_plot"),
                 uiOutput("cramer_interpretation"),
                 uiOutput("cramer_detail")),
        tabPanel("Heatmap Korelasi", 
                 plotOutput("correlation_heatmap"),
                 uiOutput("heatmap_description"))
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  data_input <- reactive({
    req(input$file)
    ext <- file_ext(input$file$name)
    if (ext == "csv") read_csv(input$file$datapath) else read_excel(input$file$datapath)
  })
  
  output$preview <- DT::renderDataTable({
    req(data_input())
    DT::datatable(data_input())
  })
  
  output$summary <- renderPrint({
    req(data_input())
    summary(data_input())
  })
  
  output$spearman_ui <- renderUI({
    req(data_input())
    num_vars <- names(Filter(is.numeric, data_input()))
    tagList(
      selectInput("spearman_x", "Pilih Variabel Numerik X", choices = num_vars),
      selectInput("spearman_y", "Pilih Variabel Numerik Y", choices = num_vars)
    )
  })
  
  output$cramer_ui <- renderUI({
    req(data_input())
    cat_vars <- names(Filter(function(x) is.factor(x) || is.character(x), data_input()))
    tagList(
      selectInput("cramer_x", "Pilih Variabel Kategorik A", choices = cat_vars),
      selectInput("cramer_y", "Pilih Variabel Kategorik B", choices = cat_vars)
    )
  })
  
  spearman_test_result <- reactiveVal(NULL)
  cramer_test_result <- reactiveVal(NULL)
  spearman_plot_data <- reactiveVal(NULL)
  cramer_plot_data <- reactiveVal(NULL)
  correlation_data <- reactiveVal(NULL)
  
  analysis_ready <- reactive({
    !is.null(spearman_test_result()) &&
      !is.null(cramer_test_result()) &&
      !is.null(correlation_data())
  })
  
  output$download_ui <- renderUI({
    downloadButton("download_report", "Download Laporan (WORD)", disabled = !analysis_ready())
  })
  
  observeEvent(input$analyze, {
    req(input$spearman_x, input$spearman_y, input$cramer_x, input$cramer_y)
    
    # Spearman
    spearman_data <- data_input() %>% select(x = input$spearman_x, y = input$spearman_y) %>% na.omit()
    test <- cor.test(~ x + y, data = spearman_data, method = "spearman")
    spearman_test_result(list(test = test, x = input$spearman_x, y = input$spearman_y))
    
    output$spearman_result <- renderPrint({ print(test) })
    
    output$spearman_plot <- renderPlotly({
      p <- ggplot(spearman_data, aes(x = x, y = y)) +
        geom_point(color = "#E74C3C") +
        theme_minimal() +
        labs(title = "Scatterplot Uji Spearman", x = input$spearman_x, y = input$spearman_y)
      spearman_plot_data(p)
      ggplotly(p)
    })
    
    output$spearman_interpretation <- renderUI({
      r <- round(test$estimate, 3)
      pval <- test$p.value
      interpret <- if (pval < 0.05) {
        paste0("Korelasi signifikan ditemukan antara ", input$spearman_x, " dan ", input$spearman_y, " (r = ", r, ", p < 0.05)")
      } else {
        paste0("Tidak ditemukan korelasi signifikan antara ", input$spearman_x, " dan ", input$spearman_y, " (r = ", r, ", p = ", round(pval, 3), ")")
      }
      HTML(paste("<b>Interpretasi:</b>", interpret))
    })
    
    output$spearman_detail <- renderUI({
      HTML(paste0("<p><b>Penjelasan Hasil:</b> Uji korelasi Spearman menunjukkan bahwa hubungan antara variabel ", input$spearman_x, " dan ", input$spearman_y, " memiliki nilai rho = ", round(test$estimate, 3), ". Nilai ini merepresentasikan kekuatan dan arah hubungan monotonic. p-value menunjukkan apakah hubungan ini signifikan secara statistik.</p>"))
    })
    
    # Cramer's V
    data <- data_input() %>% mutate(across(all_of(c(input$cramer_x, input$cramer_y)), as.factor))
    tbl <- table(data[[input$cramer_x]], data[[input$cramer_y]])
    chisq <- chisq.test(tbl, correct = FALSE)
    n <- sum(tbl)
    V <- sqrt(chisq$statistic / (n * (min(nrow(tbl), ncol(tbl)) - 1)))
    cramer_test_result(list(V = V, pval = chisq$p.value, x = input$cramer_x, y = input$cramer_y))
    
    output$cramer_result <- renderPrint({
      list("Cramer's V" = V, "p-value" = chisq$p.value)
    })
    
    output$cramer_plot <- renderPlot({
      p <- ggplot(data, aes_string(x = input$cramer_x, fill = input$cramer_y)) +
        geom_bar(position = "dodge", color = "black") +
        theme_minimal() +
        scale_fill_brewer(palette = "Set1") +
        labs(title = "Barplot Uji Cramer's V", x = input$cramer_x, fill = input$cramer_y)
      cramer_plot_data(p)
      p
    })
    
    output$cramer_interpretation <- renderUI({
      interpret <- if (chisq$p.value < 0.05) {
        paste0("Asosiasi signifikan ditemukan antara ", input$cramer_x, " dan ", input$cramer_y, " (V = ", round(V, 3), ", p < 0.05)")
      } else {
        paste0("Tidak ditemukan asosiasi signifikan antara ", input$cramer_x, " dan ", input$cramer_y, " (V = ", round(V, 3), ", p = ", round(chisq$p.value, 3), ")")
      }
      HTML(paste("<b>Interpretasi:</b>", interpret))
    })
    
    output$cramer_detail <- renderUI({
      HTML(paste0("<p><b>Penjelasan Hasil:</b> Nilai Cramér's V sebesar ", round(V, 3), " menunjukkan kekuatan asosiasi antara variabel ", input$cramer_x, " dan ", input$cramer_y, ". p-value yang dihasilkan adalah ", round(chisq$p.value, 4), ", sehingga uji ini ", ifelse(chisq$p.value < 0.05, "signifikan", "tidak signifikan"), ".</p>"))
    })
    
    # Heatmap
    num_data <- data_input() %>% select(where(is.numeric)) %>% na.omit()
    if (ncol(num_data) >= 2) {
      corr <- round(cor(num_data, use = "complete.obs", method = "spearman"), 2)
      correlation_data(corr)
    }
  })
  
  output$download_report <- downloadHandler(
    filename = function() { "laporan_analisis.docx" },  # Ganti ekstensi!
    content = function(file) {
      if (is.null(spearman_test_result()) || 
          is.null(cramer_test_result()) || 
          is.null(correlation_data())) {
        showNotification("Silakan lakukan analisis terlebih dahulu sebelum mengunduh laporan.", type = "error")
        return()
      }
      
      tempReport <- tempfile(fileext = ".Rmd")
      template_path <- file.path(getwd(), "report_template.Rmd")
      
      if (!file.exists(template_path)) {
        showNotification("Template laporan tidak ditemukan!", type = "error")
        return()
      }
      
      file.copy(template_path, tempReport, overwrite = TRUE)
      
      params <- list(
        spearman = spearman_test_result(),
        cramer = cramer_test_result(),
        correlation = correlation_data()
      )
      
      print("==== DEBUG PARAMS ====")
      print(str(params))
      
      tryCatch({
        rmarkdown::render(
          input = tempReport,
          output_format = "word_document",
          output_file = file,  # file ini udah bernama .docx
          params = params,
          envir = new.env(parent = globalenv())
        )
      }, error = function(e) {
        showNotification(paste("Render gagal:", conditionMessage(e)), type = "error")
      })
    }
  )
  
  output$correlation_heatmap <- renderPlot({
    req(correlation_data())
    ggplot(melt(correlation_data()), aes(Var1, Var2, fill = value)) +
      geom_tile() +
      geom_text(aes(label = value)) +
      scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
      theme_minimal() +
      labs(title = "Heatmap Korelasi Spearman")
  })
  
  output$heatmap_description <- renderUI({
    req(correlation_data())
    HTML("<p><b>Penjelasan:</b> Heatmap di atas menunjukkan kekuatan dan arah korelasi antar variabel numerik berdasarkan koefisien Spearman. Nilai mendekati +1 atau -1 menunjukkan korelasi kuat positif atau negatif, sedangkan nilai mendekati 0 menunjukkan korelasi lemah atau tidak ada.</p>")
  })
}

# Jalankan aplikasi
shinyApp(ui = ui, server = server)

params <- list(
  spearman = list(
    test = cor.test(mtcars$mpg, mtcars$hp, method = "spearman"),
    x = "mpg",
    y = "hp"
  ),
  cramer = list(
    V = 0.23,
    pval = 0.03,
    x = "gear",
    y = "cyl"
  ),
  correlation = round(cor(mtcars[, sapply(mtcars, is.numeric)], method = "spearman"), 2)
)

rmarkdown::render("report_template.Rmd", output_format = "word_document", params = params)
