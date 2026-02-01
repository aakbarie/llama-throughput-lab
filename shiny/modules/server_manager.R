# Server Management Module for Llama Throughput Lab
# Handles starting/stopping llama-server instances and nginx

#' Start a single llama-server instance
#' @param server_bin Path to llama-server binary
#' @param model_path Path to GGUF model file
#' @param host Host to bind to
#' @param port Port to listen on
#' @param ctx_size Context size
#' @param parallel Number of parallel slots
#' @param batch Batch size (NULL for default)
#' @param ubatch UBatch size (NULL for default)
#' @param extra_args Additional command line arguments
#' @param log_file Path to log file (NULL for temp file)
#' @return List with success, pid, log_file, error
start_llama_server <- function(server_bin,
                                model_path,
                                host = "127.0.0.1",
                                port = 8080,
                                ctx_size = 2048,
                                parallel = 1,
                                batch = NULL,
                                ubatch = NULL,
                                extra_args = "",
                                log_file = NULL) {
  # Validate inputs
  if (!file.exists(server_bin)) {
    return(list(success = FALSE, error = paste("Server binary not found:", server_bin)))
  }

  if (!file.exists(model_path)) {
    return(list(success = FALSE, error = paste("Model file not found:", model_path)))
  }

  # Build command
  cmd_args <- c(
    "-m", shQuote(model_path),
    "--host", host,
    "--port", as.character(port),
    "-c", as.character(ctx_size),
    "--parallel", as.character(parallel)
  )

  # Optional batch sizes

if (!is.null(batch) && batch != "default") {
    cmd_args <- c(cmd_args, "-b", as.character(batch))
  }

  if (!is.null(ubatch) && ubatch != "default") {
    cmd_args <- c(cmd_args, "-ub", as.character(ubatch))
  }

  # Extra args
  if (extra_args != "") {
    extra <- unlist(strsplit(extra_args, "\\s+"))
    cmd_args <- c(cmd_args, extra)
  }

  # Log file
  if (is.null(log_file)) {
    log_file <- tempfile(pattern = paste0("llama_server_", port, "_"),
                         fileext = ".log")
  }

  # Start process
  tryCatch({
    # Use system2 with wait=FALSE for background process
    # This returns the command string, we need processx for proper PID handling

    # Create a wrapper script to get PID
    cmd <- paste(c(shQuote(server_bin), cmd_args), collapse = " ")
    full_cmd <- paste(cmd, ">", shQuote(log_file), "2>&1 &")

    # Execute and capture PID
    pid_cmd <- paste("(", full_cmd, ") && echo $!")

    # Alternative: use processx if available
    if (requireNamespace("processx", quietly = TRUE)) {
      proc <- processx::process$new(
        command = server_bin,
        args = cmd_args,
        stdout = log_file,
        stderr = "2>&1",
        cleanup = FALSE
      )
      pid <- proc$get_pid()
    } else {
      # Fallback: use shell
      system(full_cmd, wait = FALSE)

      # Try to find the PID by searching for the process
      Sys.sleep(0.5)
      ps_result <- system2("pgrep", c("-f", shQuote(paste0("llama-server.*", port))),
                           stdout = TRUE, stderr = FALSE)
      pid <- if (length(ps_result) > 0) as.integer(ps_result[1]) else NA
    }

    list(
      success = TRUE,
      pid = pid,
      port = port,
      log_file = log_file,
      error = NULL
    )
  }, error = function(e) {
    list(
      success = FALSE,
      pid = NA,
      port = port,
      log_file = log_file,
      error = as.character(e)
    )
  })
}

#' Stop a process by PID
#' @param pid Process ID to stop
#' @param timeout Timeout in seconds before force kill
#' @return TRUE if stopped successfully
stop_process <- function(pid, timeout = 5) {
  if (is.na(pid)) return(TRUE)

  tryCatch({
    # Try graceful termination first
    tools::pskill(pid, signal = 15)  # SIGTERM

    # Wait for process to exit
    start_time <- Sys.time()
    while (difftime(Sys.time(), start_time, units = "secs") < timeout) {
      # Check if process still exists
      result <- system2("kill", c("-0", as.character(pid)),
                        stdout = FALSE, stderr = FALSE)
      if (result != 0) {
        return(TRUE)  # Process is gone
      }
      Sys.sleep(0.5)
    }

    # Force kill if still running
    tools::pskill(pid, signal = 9)  # SIGKILL
    Sys.sleep(0.5)

    TRUE
  }, error = function(e) {
    # Process might already be gone
    TRUE
  })
}

#' Generate nginx configuration for round-robin load balancing
#' @param ports Vector of backend server ports
#' @param nginx_port Nginx listen port
#' @param host Host address
#' @return nginx configuration as character string
generate_nginx_config <- function(ports, nginx_port = 8090, host = "127.0.0.1") {
  # Build upstream block
  upstream_servers <- paste(
    sapply(ports, function(p) paste0("        server ", host, ":", p, ";")),
    collapse = "\n"
  )

  # Configuration template
  config <- sprintf('
worker_processes 1;
error_log /tmp/nginx_llama_error.log;
pid /tmp/nginx_llama.pid;

events {
    worker_connections 1024;
}

http {
    access_log /tmp/nginx_llama_access.log;

    upstream llama_servers {
%s
    }

    server {
        listen %d;
        server_name localhost;

        location / {
            proxy_pass http://llama_servers;
            proxy_http_version 1.1;
            proxy_set_header Host $host;
            proxy_set_header X-Real-IP $remote_addr;
            proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
            proxy_connect_timeout 300s;
            proxy_send_timeout 300s;
            proxy_read_timeout 300s;
        }

        location /health {
            return 200 "OK";
            add_header Content-Type text/plain;
        }
    }
}
', upstream_servers, nginx_port)

  config
}

#' Start nginx with round-robin configuration
#' @param ports Vector of backend server ports
#' @param nginx_port Nginx listen port
#' @param host Host address
#' @param config_file Path to write config (NULL for temp file)
#' @return List with success, pid, config_file, error
start_nginx <- function(ports, nginx_port = 8090, host = "127.0.0.1",
                        config_file = NULL) {
  # Generate configuration
  config <- generate_nginx_config(ports, nginx_port, host)

  # Write config file
  if (is.null(config_file)) {
    config_file <- tempfile(pattern = "nginx_llama_", fileext = ".conf")
  }
  writeLines(config, config_file)

  # Find nginx binary
  nginx_bin <- Sys.which("nginx")
  if (nginx_bin == "") {
    # Try common locations
    candidates <- c("/usr/local/bin/nginx", "/usr/bin/nginx", "/opt/homebrew/bin/nginx")
    for (cand in candidates) {
      if (file.exists(cand)) {
        nginx_bin <- cand
        break
      }
    }
  }

  if (nginx_bin == "") {
    return(list(success = FALSE, error = "nginx not found"))
  }

  # Stop any existing nginx with our config
  system2(nginx_bin, c("-s", "stop", "-c", config_file),
          stdout = FALSE, stderr = FALSE)
  Sys.sleep(0.5)

  # Start nginx
  result <- system2(nginx_bin, c("-c", config_file),
                    stdout = TRUE, stderr = TRUE)

  # Get PID from pid file
  pid_file <- "/tmp/nginx_llama.pid"
  pid <- NA
  if (file.exists(pid_file)) {
    pid <- as.integer(readLines(pid_file, n = 1))
  }

  # Check if nginx is running
  Sys.sleep(0.5)
  running <- check_http(host, nginx_port, "/health", timeout = 2)

  if (running) {
    list(
      success = TRUE,
      pid = pid,
      port = nginx_port,
      config_file = config_file,
      error = NULL
    )
  } else {
    list(
      success = FALSE,
      pid = NA,
      port = nginx_port,
      config_file = config_file,
      error = paste("nginx failed to start:", paste(result, collapse = "\n"))
    )
  }
}

#' Stop nginx
#' @param config_file Path to nginx config file (optional)
#' @return TRUE if stopped
stop_nginx <- function(config_file = NULL) {
  nginx_bin <- Sys.which("nginx")
  if (nginx_bin == "") {
    candidates <- c("/usr/local/bin/nginx", "/usr/bin/nginx", "/opt/homebrew/bin/nginx")
    for (cand in candidates) {
      if (file.exists(cand)) {
        nginx_bin <- cand
        break
      }
    }
  }

  if (nginx_bin != "") {
    if (!is.null(config_file) && file.exists(config_file)) {
      system2(nginx_bin, c("-s", "stop", "-c", config_file),
              stdout = FALSE, stderr = FALSE)
    } else {
      # Try to stop using the PID file
      pid_file <- "/tmp/nginx_llama.pid"
      if (file.exists(pid_file)) {
        pid <- as.integer(readLines(pid_file, n = 1))
        stop_process(pid)
      }
    }
  }

  TRUE
}

#' Start multiple llama-server instances for round-robin
#' @param server_bin Path to llama-server binary
#' @param model_path Path to GGUF model file
#' @param instances Number of instances to start
#' @param base_port Starting port number
#' @param host Host to bind to
#' @param ctx_size Context size
#' @param parallel Number of parallel slots per instance
#' @param batch Batch size
#' @param ubatch UBatch size
#' @param extra_args Additional arguments
#' @param startup_delay Delay between starting instances (seconds)
#' @return List with servers (data frame) and errors
start_server_cluster <- function(server_bin,
                                  model_path,
                                  instances = 2,
                                  base_port = 8080,
                                  host = "127.0.0.1",
                                  ctx_size = 2048,
                                  parallel = 1,
                                  batch = NULL,
                                  ubatch = NULL,
                                  extra_args = "",
                                  startup_delay = 1) {
  servers <- data.frame(
    instance = integer(),
    port = integer(),
    pid = integer(),
    status = character(),
    log_file = character(),
    stringsAsFactors = FALSE
  )
  errors <- character()

  for (i in seq_len(instances)) {
    port <- base_port + i - 1

    result <- start_llama_server(
      server_bin = server_bin,
      model_path = model_path,
      host = host,
      port = port,
      ctx_size = ctx_size,
      parallel = parallel,
      batch = batch,
      ubatch = ubatch,
      extra_args = extra_args
    )

    if (result$success) {
      servers <- rbind(servers, data.frame(
        instance = i,
        port = port,
        pid = result$pid,
        status = "starting",
        log_file = result$log_file,
        stringsAsFactors = FALSE
      ))
    } else {
      errors <- c(errors, paste("Instance", i, "failed:", result$error))
    }

    if (i < instances) {
      Sys.sleep(startup_delay)
    }
  }

  list(servers = servers, errors = errors)
}

#' Stop all servers in a cluster
#' @param servers Data frame of server information
#' @return Number of servers stopped
stop_server_cluster <- function(servers) {
  stopped <- 0

  if (nrow(servers) > 0) {
    for (i in seq_len(nrow(servers))) {
      pid <- servers$pid[i]
      if (!is.na(pid)) {
        if (stop_process(pid)) {
          stopped <- stopped + 1
        }
      }
    }
  }

  stopped
}

#' Check health of all servers in cluster
#' @param servers Data frame of server information
#' @param host Server host
#' @return Updated servers data frame with status
check_cluster_health <- function(servers, host = "127.0.0.1") {
  if (nrow(servers) == 0) return(servers)

  for (i in seq_len(nrow(servers))) {
    port <- servers$port[i]
    healthy <- check_http(host, port, "/health", timeout = 5)
    servers$status[i] <- if (healthy) "healthy" else "unhealthy"
  }

  servers
}

#' Wait for all servers in cluster to be ready
#' @param servers Data frame of server information
#' @param host Server host
#' @param timeout Maximum wait time in seconds
#' @param interval Check interval in seconds
#' @return Updated servers data frame with status
wait_for_cluster <- function(servers, host = "127.0.0.1",
                             timeout = 300, interval = 2) {
  if (nrow(servers) == 0) return(servers)

  start_time <- Sys.time()

  while (difftime(Sys.time(), start_time, units = "secs") < timeout) {
    all_ready <- TRUE

    for (i in seq_len(nrow(servers))) {
      if (servers$status[i] != "ready") {
        port <- servers$port[i]
        healthy <- check_http(host, port, "/health", timeout = 5)

        if (healthy) {
          # Also check if model is loaded
          model_ready <- check_http(host, port, "/v1/models", timeout = 5)
          if (model_ready) {
            servers$status[i] <- "ready"
          } else {
            servers$status[i] <- "loading"
            all_ready <- FALSE
          }
        } else {
          servers$status[i] <- "starting"
          all_ready <- FALSE
        }
      }
    }

    if (all_ready) break

    Sys.sleep(interval)
  }

  # Mark any still not ready as timeout
  servers$status[servers$status %in% c("starting", "loading")] <- "timeout"

  servers
}

#' Get server logs
#' @param log_file Path to log file
#' @param n_lines Number of lines to return (from end)
#' @return Character vector of log lines
get_server_logs <- function(log_file, n_lines = 50) {
  if (!file.exists(log_file)) {
    return(character())
  }

  lines <- readLines(log_file, warn = FALSE)
  if (length(lines) > n_lines) {
    lines <- tail(lines, n_lines)
  }
  lines
}
