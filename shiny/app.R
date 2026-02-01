# Llama Throughput Lab - R Shiny Application
# A benchmarking and testing framework for llama.cpp inference servers

library(shiny)
library(bslib)
library(DT)
library(ggplot2)
library(jsonlite)
library(httr)
library(future)
library(promises)

# Enable async execution
plan(multisession)

# Source modules
source("modules/utils.R")
source("modules/server_manager.R")
source("modules/test_runner.R")
source("modules/sweep_runner.R")

# UI Definition
ui <- page_navbar(
  title = "Llama Throughput Lab",
  theme = bs_theme(
    version = 5,
    bootswatch = "darkly",
    primary = "#7c3aed",
    "navbar-bg" = "#1e1e2e"
  ),

  # Configuration Tab
  nav_panel(
    title = "Configuration",
    icon = icon("gear"),
    layout_sidebar(
      sidebar = sidebar(
        title = "Paths & Detection",
        width = 350,

        # Model Selection
        card(
          card_header("Model Selection"),
          card_body(
            selectInput("model_select", "Available Models",
                        choices = NULL,
                        width = "100%"),
            actionButton("refresh_models", "Refresh List",
                         class = "btn-outline-primary btn-sm"),
            hr(),
            textInput("model_path", "Or enter path manually:",
                      placeholder = "/path/to/model.gguf"),
            verbatimTextOutput("model_status", placeholder = TRUE)
          )
        ),

        # Server Binary
        card(
          card_header("Server Binary"),
          card_body(
            textInput("server_bin", "llama-server Path",
                      placeholder = "Auto-detected or enter path"),
            actionButton("detect_server", "Auto-Detect",
                         class = "btn-outline-primary btn-sm"),
            verbatimTextOutput("server_status", placeholder = TRUE)
          )
        ),

        # Quick Start Actions
        card(
          card_header(class = "bg-primary text-white", "Quick Start"),
          card_body(
            actionButton("quick_start_server", "Start Server & Run Test",
                         class = "btn-success btn-lg w-100 mb-2",
                         icon = icon("rocket")),
            actionButton("quick_benchmark", "Run Quick Benchmark",
                         class = "btn-primary btn-lg w-100",
                         icon = icon("bolt")),
            hr(),
            uiOutput("quick_status"),
            uiOutput("test_progress_bar")
          )
        )
      ),

      # Main Configuration Panel
      layout_columns(
        col_widths = c(6, 6),

        # Server Settings
        card(
          card_header("Server Settings"),
          card_body(
            layout_columns(
              col_widths = c(6, 6),
              textInput("server_host", "Host", value = "127.0.0.1"),
              numericInput("base_port", "Base Port", value = 8080, min = 1024, max = 65535)
            ),
            layout_columns(
              col_widths = c(6, 6),
              numericInput("instances", "Instances", value = 1, min = 1, max = 16),
              numericInput("parallel", "Parallel Slots", value = 1, min = 1, max = 64)
            ),
            layout_columns(
              col_widths = c(6, 6),
              numericInput("nginx_port", "Nginx Port", value = 8090, min = 1024, max = 65535),
              numericInput("ctx_size", "Context Size", value = 2048, min = 512, max = 131072)
            ),
            textInput("extra_args", "Extra Server Args",
                      placeholder = "--threads 8 --gpu-layers 99")
          )
        ),

        # Request Settings
        card(
          card_header("Request Settings"),
          card_body(
            textAreaInput("prompt", "Prompt",
                          value = "Write a short story about a robot learning to paint.",
                          rows = 3),
            layout_columns(
              col_widths = c(6, 6),
              numericInput("n_predict", "Tokens to Generate", value = 50, min = 1, max = 4096),
              numericInput("temperature", "Temperature", value = 0.7, min = 0, max = 2, step = 0.1)
            ),
            layout_columns(
              col_widths = c(6, 6),
              numericInput("concurrency", "Concurrency", value = 4, min = 1, max = 256),
              numericInput("num_requests", "Total Requests", value = 16, min = 1, max = 1000)
            ),
            layout_columns(
              col_widths = c(6, 6),
              numericInput("request_timeout", "Request Timeout (s)", value = 120, min = 10, max = 600),
              numericInput("ready_timeout", "Server Ready Timeout (s)", value = 300, min = 30, max = 600)
            )
          )
        ),

        # Sweep Parameters
        card(
          card_header("Sweep Parameters"),
          card_body(
            textInput("batch_list", "Batch Sizes",
                      value = "512, 1024, 2048",
                      placeholder = "Comma-separated, 'default' allowed"),
            textInput("ubatch_list", "UBatch Sizes",
                      value = "256, 512",
                      placeholder = "Comma-separated, 'default' allowed"),
            textInput("max_tokens_list", "Max Tokens List",
                      value = "50, 100, 200",
                      placeholder = "Comma-separated values"),
            textInput("concurrency_list", "Concurrency List",
                      value = "1, 2, 4, 8",
                      placeholder = "Comma-separated values"),
            textInput("instances_list", "Instances List",
                      value = "1, 2, 4",
                      placeholder = "Comma-separated values"),
            textInput("parallel_list", "Parallel List",
                      value = "1, 2, 4",
                      placeholder = "Comma-separated values")
          )
        ),

        # Advanced Settings
        card(
          card_header("Advanced Settings"),
          card_body(
            layout_columns(
              col_widths = c(6, 6),
              numericInput("warmup_requests", "Warmup Requests", value = 2, min = 0, max = 10),
              numericInput("cell_pause", "Cell Pause (s)", value = 1, min = 0, max = 10)
            ),
            layout_columns(
              col_widths = c(6, 6),
              numericInput("retry_attempts", "Retry Attempts", value = 3, min = 0, max = 10),
              numericInput("retry_sleep", "Retry Sleep (s)", value = 2, min = 1, max = 30)
            ),
            checkboxInput("continue_on_error", "Continue on Error", value = TRUE),
            textInput("results_dir", "Results Directory", value = "results")
          )
        )
      )
    )
  ),

  # Server Control Tab
  nav_panel(
    title = "Server Control",
    icon = icon("server"),
    layout_sidebar(
      sidebar = sidebar(
        title = "Server Actions",
        width = 300,

        card(
          card_header("Single Server"),
          card_body(
            actionButton("start_server", "Start Server",
                         class = "btn-success w-100 mb-2", icon = icon("play")),
            actionButton("stop_server", "Stop Server",
                         class = "btn-danger w-100", icon = icon("stop"))
          )
        ),

        card(
          card_header("Round-Robin Cluster"),
          card_body(
            actionButton("start_cluster", "Start Cluster",
                         class = "btn-success w-100 mb-2", icon = icon("play")),
            actionButton("stop_cluster", "Stop Cluster",
                         class = "btn-danger w-100", icon = icon("stop"))
          )
        ),

        card(
          card_header("Quick Actions"),
          card_body(
            actionButton("health_check", "Health Check",
                         class = "btn-info w-100 mb-2", icon = icon("heartbeat")),
            actionButton("refresh_status", "Refresh Status",
                         class = "btn-secondary w-100", icon = icon("refresh"))
          )
        )
      ),

      # Server Status Panel
      layout_columns(
        col_widths = c(12),
        card(
          card_header("Server Status"),
          card_body(
            DTOutput("server_table")
          )
        ),
        card(
          card_header("Server Logs"),
          card_body(
            verbatimTextOutput("server_logs", placeholder = TRUE) |>
              tagAppendAttributes(style = "height: 400px; overflow-y: auto;")
          )
        )
      )
    )
  ),

  # Tests Tab
  nav_panel(
    title = "Tests",
    icon = icon("flask"),
    layout_sidebar(
      sidebar = sidebar(
        title = "Test Selection",
        width = 300,

        radioButtons("test_type", "Select Test",
                     choices = c(
                       "Single Request" = "single",
                       "Concurrent Requests" = "concurrent",
                       "Round-Robin (Nginx)" = "round_robin"
                     ),
                     selected = "single"),

        hr(),

        actionButton("run_test", "Run Test",
                     class = "btn-primary w-100 mb-2", icon = icon("play")),
        actionButton("stop_test", "Stop Test",
                     class = "btn-danger w-100", icon = icon("stop")),

        hr(),

        card(
          card_header("Test Info"),
          card_body(
            uiOutput("test_description")
          )
        )
      ),

      # Test Results Panel
      layout_columns(
        col_widths = c(12),

        card(
          card_header("Test Progress"),
          card_body(
            uiOutput("test_progress_ui")
          )
        ),

        card(
          card_header("Test Results"),
          card_body(
            verbatimTextOutput("test_output", placeholder = TRUE) |>
              tagAppendAttributes(style = "height: 300px; overflow-y: auto;")
          )
        ),

        card(
          card_header("Metrics"),
          card_body(
            layout_columns(
              col_widths = c(3, 3, 3, 3),
              value_box(
                title = "Tokens Generated",
                value = textOutput("metric_tokens"),
                showcase = icon("coins"),
                theme = "primary"
              ),
              value_box(
                title = "Throughput (tok/s)",
                value = textOutput("metric_throughput"),
                showcase = icon("gauge-high"),
                theme = "success"
              ),
              value_box(
                title = "Elapsed Time",
                value = textOutput("metric_elapsed"),
                showcase = icon("clock"),
                theme = "info"
              ),
              value_box(
                title = "Errors",
                value = textOutput("metric_errors"),
                showcase = icon("exclamation-triangle"),
                theme = "danger"
              )
            )
          )
        )
      )
    )
  ),

  # Sweeps Tab
  nav_panel(
    title = "Sweeps",
    icon = icon("chart-line"),
    layout_sidebar(
      sidebar = sidebar(
        title = "Sweep Selection",
        width = 300,

        radioButtons("sweep_type", "Select Sweep",
                     choices = c(
                       "Threads Sweep" = "threads",
                       "Round-Robin Sweep" = "round_robin",
                       "Full Sweep" = "full"
                     ),
                     selected = "round_robin"),

        hr(),

        actionButton("run_sweep", "Run Sweep",
                     class = "btn-primary w-100 mb-2", icon = icon("play")),
        actionButton("stop_sweep", "Stop Sweep",
                     class = "btn-danger w-100", icon = icon("stop")),

        hr(),

        uiOutput("sweep_description"),

        hr(),

        uiOutput("sweep_estimate")
      ),

      # Sweep Results Panel
      layout_columns(
        col_widths = c(12),

        card(
          card_header("Sweep Progress"),
          card_body(
            uiOutput("sweep_progress_ui"),
            verbatimTextOutput("sweep_status", placeholder = TRUE)
          )
        ),

        navset_card_tab(
          title = "Results",

          nav_panel(
            title = "Table",
            DTOutput("sweep_results_table")
          ),

          nav_panel(
            title = "Throughput Chart",
            plotOutput("sweep_throughput_plot", height = "400px")
          ),

          nav_panel(
            title = "Heatmap",
            plotOutput("sweep_heatmap", height = "400px")
          )
        ),

        card(
          card_header("Best Configuration"),
          card_body(
            verbatimTextOutput("best_config", placeholder = TRUE)
          )
        )
      )
    )
  ),

  # Results Tab
  nav_panel(
    title = "Results",
    icon = icon("table"),
    layout_sidebar(
      sidebar = sidebar(
        title = "Result Files",
        width = 300,

        actionButton("refresh_results", "Refresh List",
                     class = "btn-secondary w-100 mb-2", icon = icon("refresh")),

        selectInput("result_file", "Select Result File",
                    choices = NULL),

        hr(),

        downloadButton("download_csv", "Download CSV",
                       class = "btn-primary w-100 mb-2"),
        downloadButton("download_plot", "Download Plot",
                       class = "btn-secondary w-100")
      ),

      # Results Display
      layout_columns(
        col_widths = c(12),

        card(
          card_header("Result Data"),
          card_body(
            DTOutput("result_data_table")
          )
        ),

        navset_card_tab(
          title = "Visualizations",

          nav_panel(
            title = "Throughput by Config",
            plotOutput("result_throughput_plot", height = "400px")
          ),

          nav_panel(
            title = "Comparison",
            plotOutput("result_comparison_plot", height = "400px")
          )
        )
      )
    )
  ),

  # Analysis Tab
  nav_panel(
    title = "Analysis",
    icon = icon("chart-bar"),
    layout_sidebar(
      sidebar = sidebar(
        title = "Analysis Options",
        width = 300,

        selectInput("analysis_file", "Select Result File",
                    choices = NULL),

        actionButton("load_analysis", "Load Data",
                     class = "btn-primary w-100 mb-2", icon = icon("upload")),

        hr(),

        h6("Grouping Variables"),
        checkboxGroupInput("group_vars", NULL,
                           choices = c(
                             "Batch Size" = "batch",
                             "UBatch Size" = "ubatch",
                             "Concurrency" = "concurrency",
                             "Max Tokens" = "max_tokens",
                             "Instances" = "instances",
                             "Parallel" = "parallel"
                           ),
                           selected = c("concurrency")),

        hr(),

        h6("Metric to Analyze"),
        radioButtons("analysis_metric", NULL,
                     choices = c(
                       "Throughput (tok/s)" = "throughput_tps",
                       "Total Tokens" = "total_tokens",
                       "Elapsed Time (s)" = "elapsed_s",
                       "Error Count" = "errors"
                     ),
                     selected = "throughput_tps"),

        hr(),

        downloadButton("download_analysis", "Download Report",
                       class = "btn-outline-primary w-100")
      ),

      # Analysis Display
      layout_columns(
        col_widths = c(12),

        # Summary Statistics
        card(
          card_header("Summary Statistics"),
          card_body(
            layout_columns(
              col_widths = c(3, 3, 3, 3),
              value_box(
                title = "Best Throughput",
                value = textOutput("analysis_best"),
                showcase = icon("trophy"),
                theme = "success"
              ),
              value_box(
                title = "Average",
                value = textOutput("analysis_avg"),
                showcase = icon("calculator"),
                theme = "primary"
              ),
              value_box(
                title = "Std Deviation",
                value = textOutput("analysis_sd"),
                showcase = icon("chart-line"),
                theme = "info"
              ),
              value_box(
                title = "Total Configs",
                value = textOutput("analysis_count"),
                showcase = icon("list"),
                theme = "secondary"
              )
            )
          )
        ),

        # Charts Row
        layout_columns(
          col_widths = c(6, 6),

          card(
            card_header("Distribution"),
            card_body(
              plotOutput("analysis_histogram", height = "350px")
            )
          ),

          card(
            card_header("By Group"),
            card_body(
              plotOutput("analysis_boxplot", height = "350px")
            )
          )
        ),

        # Detailed Analysis
        navset_card_tab(
          title = "Detailed Analysis",

          nav_panel(
            title = "Correlation Matrix",
            plotOutput("analysis_correlation", height = "400px")
          ),

          nav_panel(
            title = "Trend Analysis",
            plotOutput("analysis_trend", height = "400px")
          ),

          nav_panel(
            title = "Top Configurations",
            DTOutput("analysis_top_configs")
          ),

          nav_panel(
            title = "Statistical Summary",
            verbatimTextOutput("analysis_summary")
          )
        ),

        # Best Configuration Details
        card(
          card_header("Optimal Configuration"),
          card_body(
            layout_columns(
              col_widths = c(6, 6),
              div(
                h5("Best Parameters"),
                verbatimTextOutput("analysis_best_config")
              ),
              div(
                h5("Recommendations"),
                uiOutput("analysis_recommendations")
              )
            )
          )
        )
      )
    )
  ),

  # Footer
  nav_spacer(),
  nav_item(
    tags$span(
      class = "navbar-text",
      "llama-throughput-lab v1.0"
    )
  )
)

# Server Logic
server <- function(input, output, session) {

  # Reactive values for state management
  rv <- reactiveValues(
    # Server state
    servers = data.frame(
      id = integer(),
      port = integer(),
      status = character(),
      pid = integer(),
      stringsAsFactors = FALSE
    ),
    nginx_running = FALSE,
    nginx_pid = NULL,

    # Test/Sweep state
    test_running = FALSE,
    sweep_running = FALSE,
    current_results = NULL,
    sweep_progress = 0,

    # Progress tracking
    progress_status = "",
    progress_percent = 0,
    progress_start_time = NULL,
    sweep_total = 0,

    # Logs
    server_log = character(),
    test_log = character(),

    # Metrics
    tokens = 0,
    throughput = 0,
    elapsed = 0,
    errors = 0,

    # Results files
    result_files = character()
  )

  # ============================================
  # Configuration Detection
  # ============================================

  # Refresh available models list
  refresh_model_list <- function() {
    models <- list_available_models()
    if (length(models) > 0) {
      updateSelectInput(session, "model_select",
                        choices = models,
                        selected = models[1])
      output$model_status <- renderText(paste(length(models), "model(s) found"))
    } else {
      updateSelectInput(session, "model_select",
                        choices = c("No models found" = ""))
      output$model_status <- renderText("No models found in search paths")
    }
  }

  # Handle model dropdown selection
  observeEvent(input$model_select, {
    if (!is.null(input$model_select) && input$model_select != "") {
      updateTextInput(session, "model_path", value = input$model_select)
    }
  })

  # Handle manual path entry
  observeEvent(input$model_path, {
    path <- input$model_path
    if (!is.null(path) && path != "") {
      if (file.exists(path)) {
        output$model_status <- renderText(paste("Selected:", basename(path)))
      } else if (nchar(path) > 5) {
        output$model_status <- renderText("File not found")
      }
    }
  }, ignoreInit = TRUE)

  # Refresh models button
  observeEvent(input$refresh_models, {
    refresh_model_list()
  })

  observeEvent(input$detect_server, {
    result <- detect_server_binary()
    if (!is.null(result)) {
      updateTextInput(session, "server_bin", value = result)
      output$server_status <- renderText("Server binary found!")
    } else {
      output$server_status <- renderText("Not found. Please specify path.")
    }
  })

  # Auto-detect on startup
  observe({
    isolate({
      # Populate model dropdown
      refresh_model_list()

      # Also set the text input to first model
      model <- detect_model_path()
      if (!is.null(model)) {
        updateTextInput(session, "model_path", value = model)
      }

      # Detect server binary
      server <- detect_server_binary()
      if (!is.null(server)) {
        updateTextInput(session, "server_bin", value = server)
        output$server_status <- renderText("Auto-detected!")
      }
    })
  }) |> bindEvent(TRUE, once = TRUE)

  # ============================================
  # Server Control
  # ============================================

  output$server_table <- renderDT({
    datatable(
      rv$servers,
      options = list(
        pageLength = 10,
        dom = 't'
      ),
      rownames = FALSE
    )
  })

  output$server_logs <- renderText({
    paste(rv$server_log, collapse = "\n")
  })

  observeEvent(input$start_server, {
    req(input$model_path, input$server_bin)

    add_log <- function(msg) {
      rv$server_log <- c(rv$server_log, paste(Sys.time(), "-", msg))
    }

    add_log("Starting llama-server...")

    result <- start_llama_server(
      server_bin = input$server_bin,
      model_path = input$model_path,
      host = input$server_host,
      port = input$base_port,
      ctx_size = input$ctx_size,
      parallel = input$parallel,
      extra_args = input$extra_args
    )

    if (result$success) {
      rv$servers <- rbind(rv$servers, data.frame(
        id = nrow(rv$servers) + 1,
        port = input$base_port,
        status = "running",
        pid = result$pid,
        stringsAsFactors = FALSE
      ))
      add_log(paste("Server started on port", input$base_port, "PID:", result$pid))
    } else {
      add_log(paste("Failed to start server:", result$error))
    }
  })

  observeEvent(input$stop_server, {
    add_log <- function(msg) {
      rv$server_log <- c(rv$server_log, paste(Sys.time(), "-", msg))
    }

    if (nrow(rv$servers) > 0) {
      for (i in seq_len(nrow(rv$servers))) {
        pid <- rv$servers$pid[i]
        stop_process(pid)
        add_log(paste("Stopped server PID:", pid))
      }
      rv$servers <- rv$servers[0, ]
    }
  })

  observeEvent(input$start_cluster, {
    req(input$model_path, input$server_bin)

    add_log <- function(msg) {
      rv$server_log <- c(rv$server_log, paste(Sys.time(), "-", msg))
    }

    add_log(paste("Starting cluster with", input$instances, "instances..."))

    # Start multiple servers
    for (i in seq_len(input$instances)) {
      port <- input$base_port + i - 1

      result <- start_llama_server(
        server_bin = input$server_bin,
        model_path = input$model_path,
        host = input$server_host,
        port = port,
        ctx_size = input$ctx_size,
        parallel = input$parallel,
        extra_args = input$extra_args
      )

      if (result$success) {
        rv$servers <- rbind(rv$servers, data.frame(
          id = nrow(rv$servers) + 1,
          port = port,
          status = "starting",
          pid = result$pid,
          stringsAsFactors = FALSE
        ))
        add_log(paste("Started instance", i, "on port", port))
      }

      Sys.sleep(1)  # Stagger startup
    }

    # Start nginx
    add_log("Starting nginx load balancer...")
    ports <- rv$servers$port
    nginx_result <- start_nginx(
      ports = ports,
      nginx_port = input$nginx_port,
      host = input$server_host
    )

    if (nginx_result$success) {
      rv$nginx_running <- TRUE
      rv$nginx_pid <- nginx_result$pid
      add_log(paste("Nginx started on port", input$nginx_port))
    } else {
      add_log(paste("Failed to start nginx:", nginx_result$error))
    }
  })

  observeEvent(input$stop_cluster, {
    add_log <- function(msg) {
      rv$server_log <- c(rv$server_log, paste(Sys.time(), "-", msg))
    }

    # Stop nginx
    if (rv$nginx_running && !is.null(rv$nginx_pid)) {
      stop_process(rv$nginx_pid)
      rv$nginx_running <- FALSE
      rv$nginx_pid <- NULL
      add_log("Stopped nginx")
    }

    # Stop all servers
    if (nrow(rv$servers) > 0) {
      for (i in seq_len(nrow(rv$servers))) {
        stop_process(rv$servers$pid[i])
      }
      add_log(paste("Stopped", nrow(rv$servers), "server instances"))
      rv$servers <- rv$servers[0, ]
    }
  })

  observeEvent(input$health_check, {
    add_log <- function(msg) {
      rv$server_log <- c(rv$server_log, paste(Sys.time(), "-", msg))
    }

    if (nrow(rv$servers) > 0) {
      for (i in seq_len(nrow(rv$servers))) {
        port <- rv$servers$port[i]
        url <- paste0("http://", input$server_host, ":", port, "/health")

        result <- tryCatch({
          response <- httr::GET(url, httr::timeout(5))
          httr::status_code(response) == 200
        }, error = function(e) FALSE)

        status <- if (result) "healthy" else "unhealthy"
        rv$servers$status[i] <- status
        add_log(paste("Port", port, ":", status))
      }
    }
  })

  # ============================================
  # Tests
  # ============================================

  output$test_description <- renderUI({
    desc <- switch(input$test_type,
      "single" = "Send a single request to validate basic server functionality.",
      "concurrent" = "Send multiple concurrent requests to measure throughput.",
      "round_robin" = "Test nginx load-balancing across multiple server instances."
    )
    tags$p(desc)
  })

  output$metric_tokens <- renderText(rv$tokens)
  output$metric_throughput <- renderText(sprintf("%.2f", rv$throughput))
  output$metric_elapsed <- renderText(sprintf("%.2fs", rv$elapsed))
  output$metric_errors <- renderText(rv$errors)

  observeEvent(input$run_test, {
    req(!rv$test_running)

    rv$test_running <- TRUE
    rv$test_log <- character()
    rv$tokens <- 0
    rv$throughput <- 0
    rv$elapsed <- 0
    rv$errors <- 0

    add_log <- function(msg) {
      rv$test_log <- c(rv$test_log, msg)
    }

    # Determine target URL
    if (input$test_type == "round_robin") {
      base_url <- paste0("http://", input$server_host, ":", input$nginx_port)
    } else {
      base_url <- paste0("http://", input$server_host, ":", input$base_port)
    }

    add_log(paste("Starting", input$test_type, "test..."))
    add_log(paste("Target:", base_url))

    # Run test based on type
    result <- switch(input$test_type,
      "single" = run_single_test(
        base_url = base_url,
        prompt = input$prompt,
        n_predict = input$n_predict,
        temperature = input$temperature,
        timeout = input$request_timeout
      ),
      "concurrent" = run_concurrent_test(
        base_url = base_url,
        prompt = input$prompt,
        n_predict = input$n_predict,
        temperature = input$temperature,
        concurrency = input$concurrency,
        num_requests = input$num_requests,
        timeout = input$request_timeout
      ),
      "round_robin" = run_concurrent_test(
        base_url = base_url,
        prompt = input$prompt,
        n_predict = input$n_predict,
        temperature = input$temperature,
        concurrency = input$concurrency,
        num_requests = input$num_requests,
        timeout = input$request_timeout
      )
    )

    # Update metrics
    rv$tokens <- result$tokens
    rv$throughput <- result$throughput
    rv$elapsed <- result$elapsed
    rv$errors <- result$errors

    add_log(paste("Completed:", result$tokens, "tokens in",
                  sprintf("%.2fs", result$elapsed)))
    add_log(paste("Throughput:", sprintf("%.2f", result$throughput), "tok/s"))

    if (result$errors > 0) {
      add_log(paste("Errors:", result$errors))
    }

    rv$test_running <- FALSE
  })

  output$test_output <- renderText({
    paste(rv$test_log, collapse = "\n")
  })

  output$test_progress_ui <- renderUI({
    if (rv$test_running) {
      div(
        class = "d-flex align-items-center",
        div(class = "spinner-border text-primary me-2"),
        span("Test running...")
      )
    } else {
      span("Ready", class = "text-success")
    }
  })

  # ============================================
  # Sweeps
  # ============================================

  output$sweep_description <- renderUI({
    desc <- switch(input$sweep_type,
      "threads" = "Sweep --threads and --threads-http parameters to find optimal thread configuration.",
      "round_robin" = "Sweep batch, ubatch, max_tokens, and concurrency with round-robin load balancing.",
      "full" = "Comprehensive sweep: instances × parallel × batch × ubatch × concurrency."
    )
    tags$p(desc, class = "small text-muted")
  })

  output$sweep_estimate <- renderUI({
    # Calculate total combinations
    n_batch <- length(parse_list(input$batch_list))
    n_ubatch <- length(parse_list(input$ubatch_list))
    n_tokens <- length(parse_list(input$max_tokens_list))
    n_conc <- length(parse_list(input$concurrency_list))
    n_inst <- length(parse_list(input$instances_list))
    n_par <- length(parse_list(input$parallel_list))

    total <- switch(input$sweep_type,
      "threads" = 9,  # Example: 3 threads × 3 http threads
      "round_robin" = n_batch * n_ubatch * n_tokens * n_conc,
      "full" = n_inst * n_par * n_batch * n_ubatch * n_conc
    )

    tags$div(
      tags$strong("Estimated runs: "), total,
      tags$br(),
      tags$small(class = "text-muted",
                 paste("Plus", input$warmup_requests, "warmup requests per config"))
    )
  })

  observeEvent(input$run_sweep, {
    req(!rv$sweep_running)

    rv$sweep_running <- TRUE
    rv$current_results <- NULL
    rv$sweep_progress <- 0

    # Parse parameters
    batch_list <- parse_list(input$batch_list)
    ubatch_list <- parse_list(input$ubatch_list)
    tokens_list <- parse_list(input$max_tokens_list)
    conc_list <- parse_list(input$concurrency_list)

    # Calculate total
    rv$sweep_total <- switch(input$sweep_type,
      "threads" = 9,
      "round_robin" = length(batch_list) * length(ubatch_list) *
                      length(tokens_list) * length(conc_list),
      "full" = {
        inst_list <- parse_list(input$instances_list)
        par_list <- parse_list(input$parallel_list)
        length(inst_list) * length(par_list) * length(batch_list) *
          length(ubatch_list) * length(conc_list)
      }
    )

    # Target URL
    if (input$sweep_type == "round_robin") {
      base_url <- paste0("http://", input$server_host, ":", input$nginx_port)
    } else {
      base_url <- paste0("http://", input$server_host, ":", input$base_port)
    }

    # Run sweep (this would be async in production)
    result <- run_sweep(
      sweep_type = input$sweep_type,
      base_url = base_url,
      prompt = input$prompt,
      batch_list = batch_list,
      ubatch_list = ubatch_list,
      tokens_list = tokens_list,
      concurrency_list = conc_list,
      warmup = input$warmup_requests,
      cell_pause = input$cell_pause,
      timeout = input$request_timeout,
      continue_on_error = input$continue_on_error,
      progress_callback = function(completed, total) {
        rv$sweep_progress <- completed
      }
    )

    rv$current_results <- result
    rv$sweep_running <- FALSE

    # Save results
    save_sweep_results(result, input$results_dir, input$sweep_type)
  })

  output$sweep_progress_ui <- renderUI({
    if (rv$sweep_running) {
      pct <- if (rv$sweep_total > 0) rv$sweep_progress / rv$sweep_total * 100 else 0
      div(
        div(
          class = "progress mb-2",
          div(
            class = "progress-bar progress-bar-striped progress-bar-animated",
            role = "progressbar",
            style = paste0("width: ", pct, "%"),
            paste0(round(pct), "%")
          )
        ),
        span(paste(rv$sweep_progress, "/", rv$sweep_total, "configurations"))
      )
    } else if (!is.null(rv$current_results)) {
      span("Sweep complete!", class = "text-success")
    } else {
      span("Ready", class = "text-muted")
    }
  })

  output$sweep_results_table <- renderDT({
    req(rv$current_results)
    datatable(
      rv$current_results,
      options = list(
        pageLength = 25,
        order = list(list(5, 'desc'))  # Sort by throughput
      ),
      rownames = FALSE
    ) |>
      formatRound(columns = c("throughput_tps", "elapsed_s"), digits = 2)
  })

  output$sweep_throughput_plot <- renderPlot({
    req(rv$current_results)

    df <- rv$current_results

    if ("concurrency" %in% names(df)) {
      ggplot(df, aes(x = factor(concurrency), y = throughput_tps)) +
        geom_boxplot(fill = "#7c3aed", alpha = 0.7) +
        geom_jitter(width = 0.2, alpha = 0.5) +
        labs(
          title = "Throughput by Concurrency",
          x = "Concurrency",
          y = "Throughput (tokens/sec)"
        ) +
        theme_minimal() +
        theme(
          plot.background = element_rect(fill = "#1e1e2e", color = NA),
          panel.background = element_rect(fill = "#1e1e2e", color = NA),
          text = element_text(color = "white"),
          axis.text = element_text(color = "white"),
          panel.grid = element_line(color = "#333")
        )
    }
  })

  output$sweep_heatmap <- renderPlot({
    req(rv$current_results)

    df <- rv$current_results

    if (all(c("batch", "concurrency") %in% names(df))) {
      # Aggregate by batch and concurrency
      agg <- aggregate(throughput_tps ~ batch + concurrency, df, mean)

      ggplot(agg, aes(x = factor(batch), y = factor(concurrency), fill = throughput_tps)) +
        geom_tile() +
        geom_text(aes(label = round(throughput_tps, 1)), color = "white") +
        scale_fill_viridis_c(option = "plasma") +
        labs(
          title = "Throughput Heatmap",
          x = "Batch Size",
          y = "Concurrency",
          fill = "Throughput\n(tok/s)"
        ) +
        theme_minimal() +
        theme(
          plot.background = element_rect(fill = "#1e1e2e", color = NA),
          panel.background = element_rect(fill = "#1e1e2e", color = NA),
          text = element_text(color = "white"),
          axis.text = element_text(color = "white"),
          legend.background = element_rect(fill = "#1e1e2e"),
          legend.text = element_text(color = "white")
        )
    }
  })

  output$best_config <- renderText({
    req(rv$current_results)

    df <- rv$current_results
    best <- df[which.max(df$throughput_tps), ]

    paste(
      "Best Configuration:",
      paste(names(best), "=", best, collapse = ", "),
      sep = "\n"
    )
  })

  # ============================================
  # Results Management
  # ============================================

  observe({
    rv$result_files <- list_result_files(input$results_dir)
  }) |> bindEvent(input$refresh_results, TRUE)

  observe({
    updateSelectInput(session, "result_file", choices = rv$result_files)
  })

  result_data <- reactive({
    req(input$result_file)
    path <- file.path(input$results_dir, input$result_file)
    if (file.exists(path)) {
      read.csv(path)
    } else {
      NULL
    }
  })

  output$result_data_table <- renderDT({
    req(result_data())
    datatable(
      result_data(),
      options = list(pageLength = 25),
      rownames = FALSE
    )
  })

  output$result_throughput_plot <- renderPlot({
    req(result_data())
    df <- result_data()

    if ("throughput_tps" %in% names(df)) {
      df$config <- paste0("C", seq_len(nrow(df)))

      ggplot(df, aes(x = reorder(config, -throughput_tps), y = throughput_tps)) +
        geom_col(fill = "#7c3aed") +
        labs(
          title = "Throughput by Configuration",
          x = "Configuration",
          y = "Throughput (tokens/sec)"
        ) +
        theme_minimal() +
        theme(
          plot.background = element_rect(fill = "#1e1e2e", color = NA),
          panel.background = element_rect(fill = "#1e1e2e", color = NA),
          text = element_text(color = "white"),
          axis.text = element_text(color = "white"),
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid = element_line(color = "#333")
        )
    }
  })

  output$download_csv <- downloadHandler(
    filename = function() {
      input$result_file
    },
    content = function(file) {
      path <- file.path(input$results_dir, input$result_file)
      file.copy(path, file)
    }
  )

  # ============================================
  # Quick Start Actions
  # ============================================

  output$quick_status <- renderUI({
    # Show validation status
    model_ok <- !is.null(input$model_path) && input$model_path != "" && file.exists(input$model_path)
    server_ok <- !is.null(input$server_bin) && input$server_bin != "" && file.exists(input$server_bin)

    if (!model_ok || !server_ok) {
      missing <- c()
      if (!model_ok) missing <- c(missing, "model")
      if (!server_ok) missing <- c(missing, "server binary")
      div(
        class = "alert alert-warning mb-0",
        icon("exclamation-triangle"),
        paste("Missing:", paste(missing, collapse = ", "))
      )
    } else if (nrow(rv$servers) > 0) {
      div(
        class = "alert alert-success mb-0",
        icon("check-circle"),
        paste("Server running on port", rv$servers$port[1])
      )
    } else {
      div(
        class = "alert alert-info mb-0",
        icon("info-circle"),
        "Ready to start. Click above to begin."
      )
    }
  })

  # Progress bar for tests
  output$test_progress_bar <- renderUI({
    if (rv$test_running || rv$sweep_running) {
      elapsed <- if (!is.null(rv$progress_start_time)) {
        round(as.numeric(difftime(Sys.time(), rv$progress_start_time, units = "secs")), 1)
      } else {
        0
      }

      div(
        class = "mt-3",
        div(
          class = "d-flex justify-content-between mb-1",
          tags$small(class = "text-muted", rv$progress_status),
          tags$small(class = "text-muted", paste(elapsed, "s"))
        ),
        div(
          class = "progress",
          style = "height: 25px;",
          div(
            class = "progress-bar progress-bar-striped progress-bar-animated bg-success",
            role = "progressbar",
            style = paste0("width: ", rv$progress_percent, "%;"),
            `aria-valuenow` = rv$progress_percent,
            `aria-valuemin` = "0",
            `aria-valuemax` = "100",
            if (rv$progress_percent > 0) paste0(round(rv$progress_percent), "%") else ""
          )
        ),
        if (rv$throughput > 0) {
          div(
            class = "mt-2 text-center",
            tags$strong(class = "text-success", paste(round(rv$throughput, 2), "tok/s")),
            tags$span(class = "text-muted", paste(" |", rv$tokens, "tokens"))
          )
        }
      )
    } else if (rv$throughput > 0) {
      # Show last result
      div(
        class = "mt-3",
        div(
          class = "alert alert-success mb-0",
          div(
            class = "d-flex justify-content-between align-items-center",
            div(
              icon("check-circle"),
              tags$strong(" Last Result:")
            ),
            div(
              tags$span(class = "badge bg-success fs-6", paste(round(rv$throughput, 2), "tok/s")),
              tags$span(class = "badge bg-secondary ms-1", paste(rv$tokens, "tokens")),
              tags$span(class = "badge bg-info ms-1", paste(round(rv$elapsed, 2), "s"))
            )
          )
        )
      )
    }
  })

  # Auto-invalidate progress bar while running
  observe({
    if (rv$test_running || rv$sweep_running) {
      invalidateLater(500)  # Update every 500ms
    }
  })

  observeEvent(input$quick_start_server, {
    # Validate inputs with user feedback
    if (is.null(input$model_path) || input$model_path == "") {
      showNotification("Please select a model first!", type = "error", duration = 5)
      return()
    }
    if (is.null(input$server_bin) || input$server_bin == "") {
      showNotification("Please set the llama-server path first!", type = "error", duration = 5)
      return()
    }
    if (!file.exists(input$model_path)) {
      showNotification(paste("Model file not found:", input$model_path), type = "error", duration = 5)
      return()
    }
    if (!file.exists(input$server_bin)) {
      showNotification(paste("Server binary not found:", input$server_bin), type = "error", duration = 5)
      return()
    }

    # Start server if not running
    if (nrow(rv$servers) == 0) {
      rv$test_running <- TRUE
      rv$progress_status <- "Starting server..."
      rv$progress_percent <- 10
      rv$progress_start_time <- Sys.time()
      rv$server_log <- c(rv$server_log, paste(Sys.time(), "- Quick start: Starting server..."))

      result <- start_llama_server(
        server_bin = input$server_bin,
        model_path = input$model_path,
        host = input$server_host,
        port = input$base_port,
        ctx_size = input$ctx_size,
        parallel = input$parallel,
        extra_args = input$extra_args
      )

      if (result$success) {
        rv$servers <- rbind(rv$servers, data.frame(
          id = 1,
          port = input$base_port,
          status = "starting",
          pid = result$pid,
          stringsAsFactors = FALSE
        ))
        rv$progress_status <- "Loading model..."
        rv$progress_percent <- 30
        rv$server_log <- c(rv$server_log, paste(Sys.time(), "- Server started, waiting for ready..."))

        # Wait for server to be ready (with progress feedback)
        showNotification("Starting server and loading model...", type = "message", duration = NULL, id = "quick_start")

        # Wait up to 60 seconds for server
        ready <- wait_for_ready(
          paste0("http://", input$server_host, ":", input$base_port),
          timeout = 60
        )

        if (ready) {
          rv$servers$status[1] <- "ready"
          rv$progress_status <- "Running test..."
          rv$progress_percent <- 60
          removeNotification("quick_start")
          showNotification("Server ready! Running test...", type = "message", duration = 3)

          # Run a quick single test
          base_url <- paste0("http://", input$server_host, ":", input$base_port)
          test_result <- run_single_test(
            base_url = base_url,
            prompt = input$prompt,
            n_predict = input$n_predict,
            temperature = input$temperature,
            timeout = input$request_timeout
          )

          rv$progress_percent <- 100
          rv$progress_status <- "Complete!"
          rv$tokens <- test_result$tokens
          rv$throughput <- test_result$throughput
          rv$elapsed <- test_result$elapsed
          rv$errors <- test_result$errors

          showNotification(
            paste("Test complete!", round(test_result$throughput, 2), "tok/s"),
            type = "message",
            duration = 5
          )
        } else {
          removeNotification("quick_start")
          showNotification("Server failed to start in time", type = "error")
          rv$progress_status <- "Failed"
          rv$progress_percent <- 0
        }
      } else {
        showNotification(paste("Failed to start server:", result$error), type = "error")
        rv$progress_status <- "Failed"
        rv$progress_percent <- 0
      }
      rv$test_running <- FALSE
    } else {
      # Server already running, just run test
      rv$test_running <- TRUE
      rv$progress_status <- "Running test..."
      rv$progress_percent <- 50
      rv$progress_start_time <- Sys.time()

      base_url <- paste0("http://", input$server_host, ":", rv$servers$port[1])
      test_result <- run_single_test(
        base_url = base_url,
        prompt = input$prompt,
        n_predict = input$n_predict,
        temperature = input$temperature,
        timeout = input$request_timeout
      )

      rv$progress_percent <- 100
      rv$progress_status <- "Complete!"
      rv$tokens <- test_result$tokens
      rv$throughput <- test_result$throughput
      rv$elapsed <- test_result$elapsed
      rv$errors <- test_result$errors
      rv$test_running <- FALSE

      showNotification(
        paste("Test complete!", round(test_result$throughput, 2), "tok/s"),
        type = "message",
        duration = 5
      )
    }
  })

  observeEvent(input$quick_benchmark, {
    # Check if server is running
    if (nrow(rv$servers) == 0) {
      showNotification("No server running! Click 'Start Server & Run Test' first.", type = "warning", duration = 5)
      return()
    }

    rv$test_running <- TRUE
    rv$progress_status <- "Running benchmark (8 requests, 4 concurrent)..."
    rv$progress_percent <- 20
    rv$progress_start_time <- Sys.time()

    # Determine URL
    base_url <- paste0("http://", input$server_host, ":", rv$servers$port[1])

    rv$progress_percent <- 40

    # Run concurrent test
    test_result <- run_concurrent_test(
      base_url = base_url,
      prompt = input$prompt,
      n_predict = input$n_predict,
      temperature = input$temperature,
      concurrency = 4,
      num_requests = 8,
      timeout = input$request_timeout
    )

    rv$progress_percent <- 100
    rv$progress_status <- "Complete!"
    rv$tokens <- test_result$tokens
    rv$throughput <- test_result$throughput
    rv$elapsed <- test_result$elapsed
    rv$errors <- test_result$errors
    rv$test_running <- FALSE

    showNotification(
      paste("Benchmark complete!", round(test_result$throughput, 2), "tok/s"),
      type = if (test_result$errors == 0) "message" else "warning",
      duration = 5
    )
  })

  # ============================================
  # Analysis Tab
  # ============================================

  # Reactive value for analysis data
  analysis_data <- reactiveVal(NULL)

  # Update analysis file dropdown
  observe({
    files <- list_result_files(input$results_dir)
    updateSelectInput(session, "analysis_file", choices = files)
  })

  # Load data for analysis
  observeEvent(input$load_analysis, {
    req(input$analysis_file)
    path <- file.path(input$results_dir, input$analysis_file)
    if (file.exists(path)) {
      df <- read.csv(path)
      analysis_data(df)
      showNotification(paste("Loaded", nrow(df), "records"), type = "message")
    }
  })

  # Summary statistics
  output$analysis_best <- renderText({
    req(analysis_data())
    df <- analysis_data()
    metric <- input$analysis_metric
    if (metric %in% names(df)) {
      sprintf("%.2f", max(df[[metric]], na.rm = TRUE))
    } else {
      "N/A"
    }
  })

  output$analysis_avg <- renderText({
    req(analysis_data())
    df <- analysis_data()
    metric <- input$analysis_metric
    if (metric %in% names(df)) {
      sprintf("%.2f", mean(df[[metric]], na.rm = TRUE))
    } else {
      "N/A"
    }
  })

  output$analysis_sd <- renderText({
    req(analysis_data())
    df <- analysis_data()
    metric <- input$analysis_metric
    if (metric %in% names(df)) {
      sprintf("%.2f", sd(df[[metric]], na.rm = TRUE))
    } else {
      "N/A"
    }
  })

  output$analysis_count <- renderText({
    req(analysis_data())
    nrow(analysis_data())
  })

  # Histogram
  output$analysis_histogram <- renderPlot({
    req(analysis_data())
    df <- analysis_data()
    metric <- input$analysis_metric

    if (metric %in% names(df)) {
      ggplot(df, aes_string(x = metric)) +
        geom_histogram(fill = "#7c3aed", alpha = 0.8, bins = 20) +
        geom_vline(aes(xintercept = mean(df[[metric]], na.rm = TRUE)),
                   color = "#22c55e", linetype = "dashed", size = 1) +
        labs(
          title = paste("Distribution of", metric),
          x = metric,
          y = "Count"
        ) +
        theme_minimal() +
        theme(
          plot.background = element_rect(fill = "#1e1e2e", color = NA),
          panel.background = element_rect(fill = "#1e1e2e", color = NA),
          text = element_text(color = "white"),
          axis.text = element_text(color = "white"),
          panel.grid = element_line(color = "#333")
        )
    }
  })

  # Boxplot by group
  output$analysis_boxplot <- renderPlot({
    req(analysis_data(), length(input$group_vars) > 0)
    df <- analysis_data()
    metric <- input$analysis_metric
    group_var <- input$group_vars[1]

    if (metric %in% names(df) && group_var %in% names(df)) {
      ggplot(df, aes_string(x = paste0("factor(", group_var, ")"), y = metric)) +
        geom_boxplot(fill = "#7c3aed", alpha = 0.7) +
        geom_jitter(width = 0.2, alpha = 0.5, color = "#22c55e") +
        labs(
          title = paste(metric, "by", group_var),
          x = group_var,
          y = metric
        ) +
        theme_minimal() +
        theme(
          plot.background = element_rect(fill = "#1e1e2e", color = NA),
          panel.background = element_rect(fill = "#1e1e2e", color = NA),
          text = element_text(color = "white"),
          axis.text = element_text(color = "white"),
          panel.grid = element_line(color = "#333")
        )
    }
  })

  # Correlation matrix
  output$analysis_correlation <- renderPlot({
    req(analysis_data())
    df <- analysis_data()

    # Select numeric columns
    numeric_cols <- sapply(df, is.numeric)
    df_numeric <- df[, numeric_cols, drop = FALSE]

    if (ncol(df_numeric) >= 2) {
      cor_matrix <- cor(df_numeric, use = "complete.obs")

      # Convert to long format for ggplot
      cor_df <- as.data.frame(as.table(cor_matrix))
      names(cor_df) <- c("Var1", "Var2", "Correlation")

      ggplot(cor_df, aes(x = Var1, y = Var2, fill = Correlation)) +
        geom_tile() +
        geom_text(aes(label = round(Correlation, 2)), color = "white", size = 3) +
        scale_fill_gradient2(low = "#ef4444", mid = "#1e1e2e", high = "#22c55e",
                             midpoint = 0, limits = c(-1, 1)) +
        labs(title = "Correlation Matrix") +
        theme_minimal() +
        theme(
          plot.background = element_rect(fill = "#1e1e2e", color = NA),
          panel.background = element_rect(fill = "#1e1e2e", color = NA),
          text = element_text(color = "white"),
          axis.text = element_text(color = "white", angle = 45, hjust = 1),
          axis.title = element_blank(),
          legend.background = element_rect(fill = "#1e1e2e"),
          legend.text = element_text(color = "white")
        )
    }
  })

  # Trend analysis
  output$analysis_trend <- renderPlot({
    req(analysis_data(), length(input$group_vars) > 0)
    df <- analysis_data()
    metric <- input$analysis_metric
    group_var <- input$group_vars[1]

    if (metric %in% names(df) && group_var %in% names(df)) {
      # Aggregate by group
      agg <- aggregate(df[[metric]] ~ df[[group_var]], FUN = mean)
      names(agg) <- c(group_var, metric)

      ggplot(agg, aes_string(x = group_var, y = metric)) +
        geom_line(color = "#7c3aed", size = 1.5) +
        geom_point(color = "#22c55e", size = 4) +
        labs(
          title = paste("Trend:", metric, "by", group_var),
          x = group_var,
          y = metric
        ) +
        theme_minimal() +
        theme(
          plot.background = element_rect(fill = "#1e1e2e", color = NA),
          panel.background = element_rect(fill = "#1e1e2e", color = NA),
          text = element_text(color = "white"),
          axis.text = element_text(color = "white"),
          panel.grid = element_line(color = "#333")
        )
    }
  })

  # Top configurations table
  output$analysis_top_configs <- renderDT({
    req(analysis_data())
    df <- analysis_data()
    metric <- input$analysis_metric

    if (metric %in% names(df)) {
      # Sort by metric descending and take top 10
      df_sorted <- df[order(-df[[metric]]), ]
      top_10 <- head(df_sorted, 10)

      datatable(
        top_10,
        options = list(pageLength = 10, dom = 't'),
        rownames = FALSE
      ) |>
        formatRound(columns = intersect(c("throughput_tps", "elapsed_s"), names(top_10)), digits = 2)
    }
  })

  # Statistical summary
  output$analysis_summary <- renderText({
    req(analysis_data())
    df <- analysis_data()
    metric <- input$analysis_metric

    if (metric %in% names(df)) {
      values <- df[[metric]]
      values <- values[!is.na(values)]

      lines <- c(
        paste("Metric:", metric),
        paste(rep("=", 40), collapse = ""),
        "",
        paste("Count:    ", length(values)),
        paste("Mean:     ", sprintf("%.4f", mean(values))),
        paste("Median:   ", sprintf("%.4f", median(values))),
        paste("Std Dev:  ", sprintf("%.4f", sd(values))),
        paste("Min:      ", sprintf("%.4f", min(values))),
        paste("Max:      ", sprintf("%.4f", max(values))),
        paste("Range:    ", sprintf("%.4f", max(values) - min(values))),
        "",
        "Percentiles:",
        paste("  25%:    ", sprintf("%.4f", quantile(values, 0.25))),
        paste("  50%:    ", sprintf("%.4f", quantile(values, 0.50))),
        paste("  75%:    ", sprintf("%.4f", quantile(values, 0.75))),
        paste("  90%:    ", sprintf("%.4f", quantile(values, 0.90))),
        paste("  95%:    ", sprintf("%.4f", quantile(values, 0.95))),
        paste("  99%:    ", sprintf("%.4f", quantile(values, 0.99)))
      )

      paste(lines, collapse = "\n")
    } else {
      "Select a valid metric"
    }
  })

  # Best configuration details
  output$analysis_best_config <- renderText({
    req(analysis_data())
    df <- analysis_data()
    metric <- input$analysis_metric

    if (metric %in% names(df)) {
      best_idx <- which.max(df[[metric]])
      best_row <- df[best_idx, ]

      lines <- sapply(names(best_row), function(col) {
        paste(col, "=", best_row[[col]])
      })

      paste(lines, collapse = "\n")
    } else {
      "No data"
    }
  })

  # Recommendations
  output$analysis_recommendations <- renderUI({
    req(analysis_data())
    df <- analysis_data()

    recommendations <- list()

    # Analyze concurrency if present
    if ("concurrency" %in% names(df) && "throughput_tps" %in% names(df)) {
      agg <- aggregate(throughput_tps ~ concurrency, df, mean)
      best_conc <- agg$concurrency[which.max(agg$throughput_tps)]
      recommendations <- c(recommendations,
        paste("Optimal concurrency:", best_conc)
      )
    }

    # Analyze batch size if present
    if ("batch" %in% names(df) && "throughput_tps" %in% names(df)) {
      agg <- aggregate(throughput_tps ~ batch, df, mean)
      best_batch <- agg$batch[which.max(agg$throughput_tps)]
      recommendations <- c(recommendations,
        paste("Optimal batch size:", best_batch)
      )
    }

    # Check for errors
    if ("errors" %in% names(df)) {
      error_rate <- mean(df$errors > 0) * 100
      if (error_rate > 10) {
        recommendations <- c(recommendations,
          paste("Warning: High error rate (", round(error_rate, 1), "%). Consider increasing timeout or reducing concurrency.")
        )
      }
    }

    if (length(recommendations) == 0) {
      recommendations <- "Load data and run analysis to see recommendations."
    }

    tags$ul(
      lapply(recommendations, function(r) tags$li(r))
    )
  })

  # Download analysis report
  output$download_analysis <- downloadHandler(
    filename = function() {
      paste0("analysis_report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt")
    },
    content = function(file) {
      req(analysis_data())
      df <- analysis_data()
      metric <- input$analysis_metric

      lines <- c(
        "Llama Throughput Lab - Analysis Report",
        paste("Generated:", Sys.time()),
        paste(rep("=", 50), collapse = ""),
        "",
        paste("Data file:", input$analysis_file),
        paste("Records:", nrow(df)),
        paste("Metric analyzed:", metric),
        "",
        "Summary Statistics:",
        paste(rep("-", 30), collapse = ""),
        paste("Mean:", sprintf("%.4f", mean(df[[metric]], na.rm = TRUE))),
        paste("Median:", sprintf("%.4f", median(df[[metric]], na.rm = TRUE))),
        paste("Std Dev:", sprintf("%.4f", sd(df[[metric]], na.rm = TRUE))),
        paste("Min:", sprintf("%.4f", min(df[[metric]], na.rm = TRUE))),
        paste("Max:", sprintf("%.4f", max(df[[metric]], na.rm = TRUE))),
        "",
        "Best Configuration:",
        paste(rep("-", 30), collapse = "")
      )

      best_idx <- which.max(df[[metric]])
      best_row <- df[best_idx, ]
      for (col in names(best_row)) {
        lines <- c(lines, paste(" ", col, "=", best_row[[col]]))
      }

      writeLines(lines, file)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
