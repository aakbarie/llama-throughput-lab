# Utility functions for Llama Throughput Lab

#' Parse a comma/space-separated list of values
#' Supports "default" as a special value
#' @param x Character string with comma or space separated values
#' @return Vector of values (character for "default", numeric otherwise)
parse_list <- function(x) {
  if (is.null(x) || x == "") return(character())

  # Split by comma or whitespace
  values <- unlist(strsplit(x, "[,\\s]+"))
  values <- trimws(values)
  values <- values[values != ""]

  # Try to convert to numeric, keep "default" as string
  result <- sapply(values, function(v) {
    if (tolower(v) == "default") {
      return("default")
    }
    num <- suppressWarnings(as.numeric(v))
    if (!is.na(num)) return(num)
    return(v)
  }, USE.NAMES = FALSE)

  result
}

#' Get the project root directory (parent of shiny folder)
#' @return Path to project root
get_project_root <- function() {
  # Try to find app directory
app_dir <- tryCatch({
    # If running in Shiny, get the app directory
    if (exists("shiny::getShinyOption")) {
      app_path <- shiny::getShinyOption("appDir")
      if (!is.null(app_path)) {
        return(dirname(app_path))  # Parent of shiny/ folder
      }
    }
    NULL
  }, error = function(e) NULL)

  # Fallback: check if we're in the shiny directory
  cwd <- getwd()
  if (basename(cwd) == "shiny") {
    return(dirname(cwd))
  }

  # Check if shiny subfolder exists (we're in project root)
  if (dir.exists(file.path(cwd, "shiny"))) {
    return(cwd)
  }

  # Last resort: use current directory
cwd
}

#' Detect model path from common locations
#' @return Path to first .gguf file found, or NULL
detect_model_path <- function() {
  # Check environment variable first
  env_path <- Sys.getenv("LLAMA_MODEL_PATH", "")
  if (env_path != "" && file.exists(env_path)) {
    return(env_path)
  }

  # Get project root
  project_root <- get_project_root()

  # Check model directories from environment
  model_dirs_env <- Sys.getenv("LLAMA_MODEL_DIRS", "")
  if (model_dirs_env != "") {
    model_dirs <- unlist(strsplit(model_dirs_env, ":"))
  } else {
    # Build search list dynamically, checking existence
    model_dirs <- character()

    # Project-specific locations (highest priority)
    candidates <- c(
      file.path(project_root, "models"),
      "/Volumes/macStorage/my projects/llama-throughput-lab/models",
      "models",
      file.path(getwd(), "models"),
      path.expand("~/Downloads"),
      path.expand("~/.cache/lm-studio/models"),
      path.expand("~/models"),
      "/usr/local/share/models"
    )

    for (d in candidates) {
      if (dir.exists(d)) {
        # Normalize to avoid duplicates
        norm_d <- tryCatch(normalizePath(d), error = function(e) d)
        if (!norm_d %in% model_dirs) {
          model_dirs <- c(model_dirs, norm_d)
        }
      }
    }
  }

  for (dir in model_dirs) {
    if (dir.exists(dir)) {
      # Find .gguf files
      gguf_files <- list.files(dir, pattern = "\\.gguf$",
                                full.names = TRUE, recursive = TRUE)
      if (length(gguf_files) > 0) {
        # Return most recently modified
        info <- file.info(gguf_files)
        newest <- gguf_files[which.max(info$mtime)]
        return(newest)
      }
    }
  }

  NULL
}

#' List all available model files
#' @return Named vector of model paths (names are basenames)
list_available_models <- function() {
  project_root <- get_project_root()

  # Build list of directories to search
  model_dirs <- character()

  # Project models directory (highest priority)
  project_models <- file.path(project_root, "models")
  if (dir.exists(project_models)) {
    model_dirs <- c(model_dirs, project_models)
  }

  # Hardcoded project path (for when running from different locations)
  hardcoded_path <- "/Volumes/macStorage/my projects/llama-throughput-lab/models"
  if (dir.exists(hardcoded_path) && !hardcoded_path %in% model_dirs) {
    model_dirs <- c(model_dirs, hardcoded_path)
  }

  # Relative models directory
  if (dir.exists("models")) {
    model_dirs <- c(model_dirs, normalizePath("models"))
  }

  # CWD models
  cwd_models <- file.path(getwd(), "models")
  if (dir.exists(cwd_models) && !normalizePath(cwd_models) %in% model_dirs) {
    model_dirs <- c(model_dirs, cwd_models)
  }

  # User directories
  user_dirs <- c(
    path.expand("~/Downloads"),
    path.expand("~/.cache/lm-studio/models"),
    path.expand("~/models")
  )
  for (d in user_dirs) {
    if (dir.exists(d)) {
      model_dirs <- c(model_dirs, d)
    }
  }

  all_models <- character()

  for (dir in model_dirs) {
    if (dir.exists(dir)) {
      gguf_files <- list.files(dir, pattern = "\\.gguf$",
                                full.names = TRUE, recursive = TRUE)
      all_models <- c(all_models, gguf_files)
    }
  }

  # Remove duplicates and sort
  all_models <- unique(all_models)

  if (length(all_models) == 0) {
    return(character())
  }

  # Create named vector (basename -> full path)
  names(all_models) <- basename(all_models)
  all_models
}

#' Detect llama-server binary
#' @return Path to llama-server, or NULL
detect_server_binary <- function() {
  # Check environment variable first
  env_bin <- Sys.getenv("LLAMA_SERVER_BIN", "")
  if (env_bin != "" && file.exists(env_bin)) {
    return(env_bin)
  }

  # Check LLAMA_CPP_DIR
  cpp_dir <- Sys.getenv("LLAMA_CPP_DIR", "")
  if (cpp_dir != "") {
    candidates <- c(
      file.path(cpp_dir, "build", "bin", "llama-server"),
      file.path(cpp_dir, "llama-server"),
      file.path(cpp_dir, "server")
    )
    for (path in candidates) {
      if (file.exists(path)) return(path)
    }
  }

  # Search common locations
  search_paths <- c(
    "/usr/local/bin/llama-server",
    "/usr/bin/llama-server",
    path.expand("~/llama.cpp/build/bin/llama-server"),
    path.expand("~/llama.cpp/llama-server"),
    "../llama.cpp/build/bin/llama-server",
    "../../llama.cpp/build/bin/llama-server"
  )

  for (path in search_paths) {
    if (file.exists(path)) return(normalizePath(path))
  }

  # Try which command
  result <- tryCatch({
    path <- system2("which", "llama-server", stdout = TRUE, stderr = FALSE)
    if (length(path) > 0 && file.exists(path)) path else NULL
  }, error = function(e) NULL)

  result
}

#' Find an available port
#' @param start Starting port to check
#' @param host Host to check
#' @return Available port number
find_available_port <- function(start = 8080, host = "127.0.0.1") {
  for (port in start:(start + 100)) {
    # Try to connect to check if port is in use
    con <- tryCatch({
      socketConnection(host = host, port = port, open = "r",
                       blocking = FALSE, timeout = 1)
    }, error = function(e) NULL)

    if (is.null(con)) {
      return(port)  # Port is available
    } else {
      close(con)
    }
  }

  stop("Could not find available port")
}

#' Check if a port is responding to HTTP
#' @param host Host to check
#' @param port Port to check
#' @param path Path to request
#' @param timeout Timeout in seconds
#' @return TRUE if responding, FALSE otherwise
check_http <- function(host, port, path = "/health", timeout = 5) {
  url <- paste0("http://", host, ":", port, path)

  tryCatch({
    response <- httr::GET(url, httr::timeout(timeout))
    httr::status_code(response) == 200
  }, error = function(e) FALSE)
}

#' Wait for server to be ready
#' @param host Server host
#' @param port Server port
#' @param timeout Maximum wait time in seconds
#' @param interval Check interval in seconds
#' @return TRUE if server is ready, FALSE if timeout
wait_for_server <- function(host, port, timeout = 300, interval = 1) {
  start_time <- Sys.time()

  while (difftime(Sys.time(), start_time, units = "secs") < timeout) {
    if (check_http(host, port, "/health")) {
      # Also check if model is loaded
      if (check_http(host, port, "/v1/models")) {
        return(TRUE)
      }
    }
    Sys.sleep(interval)
  }

  FALSE
}

#' Format elapsed time nicely
#' @param seconds Number of seconds
#' @return Formatted string
format_elapsed <- function(seconds) {
  if (seconds < 60) {
    sprintf("%.1fs", seconds)
  } else if (seconds < 3600) {
    sprintf("%dm %ds", floor(seconds / 60), round(seconds %% 60))
  } else {
    sprintf("%dh %dm", floor(seconds / 3600), floor((seconds %% 3600) / 60))
  }
}

#' Generate timestamp for filenames
#' @return Timestamp string
timestamp_string <- function() {
  format(Sys.time(), "%Y%m%d_%H%M%S")
}

#' List result files in a directory
#' @param results_dir Directory to search
#' @return Vector of CSV filenames
list_result_files <- function(results_dir = "results") {
  if (!dir.exists(results_dir)) {
    return(character())
  }

  files <- list.files(results_dir, pattern = "\\.csv$",
                      recursive = TRUE, full.names = FALSE)

  # Sort by modification time (newest first)
  full_paths <- file.path(results_dir, files)
  info <- file.info(full_paths)
  files[order(info$mtime, decreasing = TRUE)]
}

#' Save sweep results to CSV
#' @param results Data frame of results
#' @param results_dir Base results directory
#' @param sweep_type Type of sweep for subdirectory
#' @return Path to saved file
save_sweep_results <- function(results, results_dir = "results", sweep_type = "sweep") {
  # Create directory structure
  subdir <- file.path(results_dir, sweep_type)
  if (!dir.exists(subdir)) {
    dir.create(subdir, recursive = TRUE)
  }

  # Generate filename
  filename <- paste0(sweep_type, "_", timestamp_string(), ".csv")
  filepath <- file.path(subdir, filename)

  # Write CSV
  write.csv(results, filepath, row.names = FALSE)

  message("Results saved to: ", filepath)
  filepath
}

#' Append a single result row to CSV (incremental writing)
#' @param result Single row data frame or list
#' @param filepath Path to CSV file
#' @param create_header Whether to create header row
append_result_row <- function(result, filepath, create_header = FALSE) {
  if (is.list(result) && !is.data.frame(result)) {
    result <- as.data.frame(result, stringsAsFactors = FALSE)
  }

  write.table(
    result,
    file = filepath,
    sep = ",",
    row.names = FALSE,
    col.names = create_header,
    append = !create_header
  )
}

#' Calculate throughput from tokens and elapsed time
#' @param tokens Total tokens generated
#' @param elapsed Elapsed time in seconds
#' @return Tokens per second
calc_throughput <- function(tokens, elapsed) {
  if (elapsed <= 0) return(0)
  tokens / elapsed
}

#' Retry an operation with exponential backoff
#' @param fn Function to execute
#' @param max_attempts Maximum attempts
#' @param base_sleep Base sleep time in seconds
#' @param retry_codes HTTP status codes that trigger retry
#' @return Result of function or error
retry_operation <- function(fn, max_attempts = 3, base_sleep = 2,
                            retry_codes = c(500, 502, 503, 504)) {
  attempt <- 1

  while (attempt <= max_attempts) {
    result <- tryCatch({
      list(success = TRUE, value = fn())
    }, error = function(e) {
      list(success = FALSE, error = e)
    })

    if (result$success) {
      return(result$value)
    }

    # Check if we should retry
    error_msg <- as.character(result$error)
    should_retry <- any(sapply(retry_codes, function(code) {
      grepl(as.character(code), error_msg)
    }))

    if (!should_retry || attempt == max_attempts) {
      stop(result$error)
    }

    # Exponential backoff
    sleep_time <- base_sleep * (2 ^ (attempt - 1))
    message("Retrying in ", sleep_time, "s (attempt ", attempt + 1, "/", max_attempts, ")")
    Sys.sleep(sleep_time)
    attempt <- attempt + 1
  }
}

#' Create a request body for the completion API
#' @param prompt Input prompt
#' @param n_predict Tokens to generate
#' @param temperature Sampling temperature
#' @return List suitable for JSON conversion
create_completion_body <- function(prompt, n_predict = 50, temperature = 0.7) {
  list(
    prompt = prompt,
    n_predict = n_predict,
    temperature = temperature,
    stream = FALSE
  )
}

#' Extract token count from API response
#' @param response Parsed JSON response
#' @return Number of tokens generated
extract_token_count <- function(response) {
  # Try different response formats
  timings <- response$timings
  timing_keys <- c("predicted_n", "tokens_predicted", "completion_tokens")

  if (!is.null(timings)) {
    for (key in timing_keys) {
      if (!is.null(timings[[key]])) {
        return(timings[[key]])
      }
    }
  }

  for (key in timing_keys) {
    if (!is.null(response[[key]])) {
      return(response[[key]])
    }
  }

  if (!is.null(response$usage$completion_tokens)) {
    return(response$usage$completion_tokens)
  }

  if (!is.null(response$content)) {
    # Estimate from content length
    return(length(unlist(strsplit(response$content, "\\s+"))))
  }

  0
}

#' Extract timing information from API response
#' @param response Parsed JSON response
#' @return Named list with timing info
extract_timing <- function(response) {
  if (!is.null(response$timings)) {
    tokens_per_second <- response$timings$predicted_per_second

    if (is.null(tokens_per_second) &&
        !is.null(response$timings$predicted_n) &&
        !is.null(response$timings$predicted_ms) &&
        response$timings$predicted_ms > 0) {
      tokens_per_second <- response$timings$predicted_n /
        (response$timings$predicted_ms / 1000)
    }

    return(list(
      prompt_eval_time = response$timings$prompt_eval_time_ms / 1000,
      generation_time = response$timings$predicted_time_ms / 1000,
      tokens_per_second = tokens_per_second
    ))
  }

  list(
    prompt_eval_time = NA,
    generation_time = NA,
    tokens_per_second = NA
  )
}
