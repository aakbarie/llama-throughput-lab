# Sweep Runner Module for Llama Throughput Lab
# Handles threads, round-robin, and full parameter sweeps

#' Run a parameter sweep
#' @param sweep_type Type of sweep: "threads", "round_robin", or "full"
#' @param base_url Base URL of the server (or nginx)
#' @param prompt Input prompt
#' @param batch_list Vector of batch sizes
#' @param ubatch_list Vector of ubatch sizes
#' @param tokens_list Vector of max_tokens values
#' @param concurrency_list Vector of concurrency values
#' @param instances_list Vector of instance counts (full sweep only)
#' @param parallel_list Vector of parallel slot values (full sweep only)
#' @param threads_list Vector of thread values (threads sweep only)
#' @param threads_http_list Vector of http thread values (threads sweep only)
#' @param warmup Number of warmup requests per configuration
#' @param cell_pause Pause between configurations (seconds)
#' @param timeout Request timeout
#' @param continue_on_error Continue on failures
#' @param retry_attempts Max retries per request
#' @param retry_sleep Base retry sleep
#' @param progress_callback Function(completed, total) for progress updates
#' @param server_restart_fn Function to restart server with new params (for threads/full)
#' @return Data frame of results
run_sweep <- function(sweep_type,
                      base_url,
                      prompt,
                      model = NULL,
                      batch_list = c(512, 1024, 2048),
                      ubatch_list = c(256, 512),
                      tokens_list = c(50, 100, 200),
                      concurrency_list = c(1, 2, 4, 8),
                      instances_list = c(1, 2, 4),
                      parallel_list = c(1, 2, 4),
                      threads_list = c(4, 8, 16),
                      threads_http_list = c("default", 4, 8),
                      warmup = 2,
                      cell_pause = 1,
                      timeout = 120,
                      continue_on_error = TRUE,
                      retry_attempts = 3,
                      retry_sleep = 2,
                      progress_callback = NULL,
                      server_restart_fn = NULL) {
  switch(sweep_type,
    "threads" = run_threads_sweep(
      base_url = base_url,
      prompt = prompt,
      model = model,
      tokens = tokens_list[1],
      concurrency = concurrency_list[1],
      threads_list = threads_list,
      threads_http_list = threads_http_list,
      warmup = warmup,
      cell_pause = cell_pause,
      timeout = timeout,
      continue_on_error = continue_on_error,
      progress_callback = progress_callback,
      server_restart_fn = server_restart_fn
    ),

    "round_robin" = run_round_robin_sweep(
      base_url = base_url,
      prompt = prompt,
      model = model,
      batch_list = batch_list,
      ubatch_list = ubatch_list,
      tokens_list = tokens_list,
      concurrency_list = concurrency_list,
      warmup = warmup,
      cell_pause = cell_pause,
      timeout = timeout,
      continue_on_error = continue_on_error,
      retry_attempts = retry_attempts,
      retry_sleep = retry_sleep,
      progress_callback = progress_callback
    ),

    "full" = run_full_sweep(
      base_url = base_url,
      prompt = prompt,
      model = model,
      instances_list = instances_list,
      parallel_list = parallel_list,
      batch_list = batch_list,
      ubatch_list = ubatch_list,
      concurrency_list = concurrency_list,
      tokens = tokens_list[1],
      warmup = warmup,
      cell_pause = cell_pause,
      timeout = timeout,
      continue_on_error = continue_on_error,
      retry_attempts = retry_attempts,
      retry_sleep = retry_sleep,
      progress_callback = progress_callback,
      server_restart_fn = server_restart_fn
    ),

    # Default
    stop("Unknown sweep type: ", sweep_type)
  )
}

#' Run round-robin sweep (batch × ubatch × max_tokens × concurrency)
#' @return Data frame with columns: batch, ubatch, max_tokens, concurrency,
#'         throughput_tps, total_tokens, elapsed_s, errors
run_round_robin_sweep <- function(base_url,
                                   prompt,
                                   model = NULL,
                                   batch_list,
                                   ubatch_list,
                                   tokens_list,
                                   concurrency_list,
                                   warmup = 2,
                                   cell_pause = 1,
                                   timeout = 120,
                                   continue_on_error = TRUE,
                                   retry_attempts = 3,
                                   retry_sleep = 2,
                                   progress_callback = NULL) {
  # Calculate total configurations
  total <- length(batch_list) * length(ubatch_list) *
           length(tokens_list) * length(concurrency_list)
  completed <- 0

  # Initialize results
  results <- data.frame(
    batch = integer(),
    ubatch = integer(),
    max_tokens = integer(),
    concurrency = integer(),
    throughput_tps = numeric(),
    total_tokens = integer(),
    elapsed_s = numeric(),
    errors = integer(),
    stringsAsFactors = FALSE
  )

  best_throughput <- 0
  best_config <- NULL

  # Note: For round-robin sweep, we assume servers are already running

# with the desired batch/ubatch (need server restart for those changes)
  # This sweep primarily varies max_tokens and concurrency

  for (batch in batch_list) {
    for (ubatch in ubatch_list) {
      for (max_tokens in tokens_list) {
        for (concurrency in concurrency_list) {
          completed <- completed + 1

          if (!is.null(progress_callback)) {
            progress_callback(completed, total)
          }

          # Calculate num_requests
          multiplier <- 4
          num_requests <- concurrency * multiplier

          # Run test
          result <- tryCatch({
            run_test_config(
              base_url = base_url,
              prompt = prompt,
              n_predict = max_tokens,
              model = model,
              concurrency = concurrency,
              num_requests = num_requests,
              timeout = timeout,
              warmup = warmup,
              retry_attempts = retry_attempts,
              retry_sleep = retry_sleep
            )
          }, error = function(e) {
            if (continue_on_error) {
              list(tokens = 0, throughput = 0, elapsed = 0, errors = 1)
            } else {
              stop(e)
            }
          })

          # Add to results
          row <- data.frame(
            batch = as.integer(if (batch == "default") NA else batch),
            ubatch = as.integer(if (ubatch == "default") NA else ubatch),
            max_tokens = as.integer(max_tokens),
            concurrency = as.integer(concurrency),
            throughput_tps = result$throughput,
            total_tokens = result$tokens,
            elapsed_s = result$elapsed,
            errors = result$errors,
            stringsAsFactors = FALSE
          )
          results <- rbind(results, row)

          # Track best
          if (result$throughput > best_throughput) {
            best_throughput <- result$throughput
            best_config <- row
          }

          # Pause between cells
          if (cell_pause > 0 && completed < total) {
            Sys.sleep(cell_pause)
          }
        }
      }
    }
  }

  attr(results, "best_config") <- best_config
  attr(results, "best_throughput") <- best_throughput

  # Also capture total_tokens if missing
  if (!"total_tokens" %in% names(results)) {
    results$total_tokens <- results$throughput_tps * results$elapsed_s
  }

  results
}

#' Run threads sweep (threads × threads_http)
#' @return Data frame with columns: threads, threads_http, throughput_tps,
#'         total_tokens, elapsed_s, errors
run_threads_sweep <- function(base_url,
                               prompt,
                               model = NULL,
                               tokens = 50,
                               concurrency = 4,
                               threads_list,
                               threads_http_list,
                               warmup = 2,
                               cell_pause = 1,
                               timeout = 120,
                               continue_on_error = TRUE,
                               progress_callback = NULL,
                               server_restart_fn = NULL) {
  # Calculate total
  total <- length(threads_list) * length(threads_http_list)
  completed <- 0

  # Initialize results
  results <- data.frame(
    threads = integer(),
    threads_http = character(),
    throughput_tps = numeric(),
    total_tokens = integer(),
    elapsed_s = numeric(),
    errors = integer(),
    stringsAsFactors = FALSE
  )

  best_throughput <- 0
  best_config <- NULL

  for (threads in threads_list) {
    for (threads_http in threads_http_list) {
      completed <- completed + 1

      if (!is.null(progress_callback)) {
        progress_callback(completed, total)
      }

      # Restart server with new thread configuration
      if (!is.null(server_restart_fn)) {
        restart_result <- server_restart_fn(
          threads = threads,
          threads_http = threads_http
        )

        if (!restart_result$success) {
          if (continue_on_error) {
            results <- rbind(results, data.frame(
              threads = as.integer(threads),
              threads_http = as.character(threads_http),
              throughput_tps = 0,
              total_tokens = 0,
              elapsed_s = 0,
              errors = 1,
              stringsAsFactors = FALSE
            ))
            next
          } else {
            stop("Failed to restart server: ", restart_result$error)
          }
        }

        # Wait for server to be ready
        if (!wait_for_ready(base_url, timeout = 300)) {
          if (continue_on_error) {
            results <- rbind(results, data.frame(
              threads = as.integer(threads),
              threads_http = as.character(threads_http),
              throughput_tps = 0,
              total_tokens = 0,
              elapsed_s = 0,
              errors = 1,
              stringsAsFactors = FALSE
            ))
            next
          } else {
            stop("Server did not become ready after restart")
          }
        }
      }

      # Calculate num_requests
      multiplier <- 4
      num_requests <- concurrency * multiplier

      # Run test
      result <- tryCatch({
        run_test_config(
          base_url = base_url,
          prompt = prompt,
          n_predict = tokens,
          model = model,
          concurrency = concurrency,
          num_requests = num_requests,
          timeout = timeout,
          warmup = warmup
        )
      }, error = function(e) {
        if (continue_on_error) {
          list(tokens = 0, throughput = 0, elapsed = 0, errors = 1)
        } else {
          stop(e)
        }
      })

      # Add to results
      row <- data.frame(
        threads = as.integer(threads),
        threads_http = as.character(threads_http),
        throughput_tps = result$throughput,
        total_tokens = result$tokens,
        elapsed_s = result$elapsed,
        errors = result$errors,
        stringsAsFactors = FALSE
      )
      results <- rbind(results, row)

      # Track best
      if (result$throughput > best_throughput) {
        best_throughput <- result$throughput
        best_config <- row
      }

      # Pause between cells
      if (cell_pause > 0 && completed < total) {
        Sys.sleep(cell_pause)
      }
    }
  }

  attr(results, "best_config") <- best_config
  attr(results, "best_throughput") <- best_throughput

  results
}

#' Run full sweep (instances × parallel × batch × ubatch × concurrency)
#' @return Data frame with columns: instances, parallel, batch, ubatch,
#'         concurrency, throughput_tps, total_tokens, elapsed_s, errors
run_full_sweep <- function(base_url,
                            prompt,
                            model = NULL,
                            instances_list,
                            parallel_list,
                            batch_list,
                            ubatch_list,
                            concurrency_list,
                            tokens = 50,
                            warmup = 2,
                            cell_pause = 1,
                            timeout = 120,
                            continue_on_error = TRUE,
                            retry_attempts = 3,
                            retry_sleep = 2,
                            progress_callback = NULL,
                            server_restart_fn = NULL) {
  # Calculate total
  total <- length(instances_list) * length(parallel_list) *
           length(batch_list) * length(ubatch_list) * length(concurrency_list)
  completed <- 0

  # Initialize results
  results <- data.frame(
    instances = integer(),
    parallel = integer(),
    batch = integer(),
    ubatch = integer(),
    concurrency = integer(),
    throughput_tps = numeric(),
    total_tokens = integer(),
    elapsed_s = numeric(),
    errors = integer(),
    stringsAsFactors = FALSE
  )

  best_throughput <- 0
  best_config <- NULL

  for (instances in instances_list) {
    for (parallel in parallel_list) {
      for (batch in batch_list) {
        for (ubatch in ubatch_list) {
          # These parameters require server restart
          if (!is.null(server_restart_fn)) {
            restart_result <- server_restart_fn(
              instances = instances,
              parallel = parallel,
              batch = batch,
              ubatch = ubatch
            )

            if (!restart_result$success) {
              if (continue_on_error) {
                # Add failure rows for all concurrency values
                for (concurrency in concurrency_list) {
                  completed <- completed + 1
                  if (!is.null(progress_callback)) {
                    progress_callback(completed, total)
                  }

                  results <- rbind(results, data.frame(
                    instances = as.integer(instances),
                    parallel = as.integer(parallel),
                    batch = as.integer(if (batch == "default") NA else batch),
                    ubatch = as.integer(if (ubatch == "default") NA else ubatch),
                    concurrency = as.integer(concurrency),
                    throughput_tps = 0,
                    total_tokens = 0,
                    elapsed_s = 0,
                    errors = 1,
                    stringsAsFactors = FALSE
                  ))
                }
                next
              } else {
                stop("Failed to restart server: ", restart_result$error)
              }
            }

            # Wait for server to be ready
            if (!wait_for_ready(base_url, timeout = 300)) {
              if (continue_on_error) {
                for (concurrency in concurrency_list) {
                  completed <- completed + 1
                  if (!is.null(progress_callback)) {
                    progress_callback(completed, total)
                  }

                  results <- rbind(results, data.frame(
                    instances = as.integer(instances),
                    parallel = as.integer(parallel),
                    batch = as.integer(if (batch == "default") NA else batch),
                    ubatch = as.integer(if (ubatch == "default") NA else ubatch),
                    concurrency = as.integer(concurrency),
                    throughput_tps = 0,
                    total_tokens = 0,
                    elapsed_s = 0,
                    errors = 1,
                    stringsAsFactors = FALSE
                  ))
                }
                next
              } else {
                stop("Server did not become ready")
              }
            }
          }

          for (concurrency in concurrency_list) {
            completed <- completed + 1

            if (!is.null(progress_callback)) {
              progress_callback(completed, total)
            }

            # Calculate num_requests
            multiplier <- 4
            num_requests <- concurrency * multiplier

            # Run test
            result <- tryCatch({
              run_test_config(
                base_url = base_url,
                prompt = prompt,
                n_predict = tokens,
                model = model,
                concurrency = concurrency,
                num_requests = num_requests,
                timeout = timeout,
                warmup = warmup,
                retry_attempts = retry_attempts,
                retry_sleep = retry_sleep
              )
            }, error = function(e) {
              if (continue_on_error) {
                list(tokens = 0, throughput = 0, elapsed = 0, errors = 1)
              } else {
                stop(e)
              }
            })

            # Add to results
            row <- data.frame(
              instances = as.integer(instances),
              parallel = as.integer(parallel),
              batch = as.integer(if (batch == "default") NA else batch),
              ubatch = as.integer(if (ubatch == "default") NA else ubatch),
              concurrency = as.integer(concurrency),
              throughput_tps = result$throughput,
              total_tokens = result$tokens,
              elapsed_s = result$elapsed,
              errors = result$errors,
              stringsAsFactors = FALSE
            )
            results <- rbind(results, row)

            # Track best
            if (result$throughput > best_throughput) {
              best_throughput <- result$throughput
              best_config <- row
            }

            # Pause between cells
            if (cell_pause > 0 && completed < total) {
              Sys.sleep(cell_pause)
            }
          }
        }
      }
    }
  }

  attr(results, "best_config") <- best_config
  attr(results, "best_throughput") <- best_throughput

  results
}

#' Calculate sweep statistics
#' @param results Data frame from a sweep
#' @return List with statistics
calculate_sweep_stats <- function(results) {
  list(
    total_configs = nrow(results),
    successful = sum(results$errors == 0),
    failed = sum(results$errors > 0),
    min_throughput = min(results$throughput_tps[results$errors == 0], na.rm = TRUE),
    max_throughput = max(results$throughput_tps[results$errors == 0], na.rm = TRUE),
    mean_throughput = mean(results$throughput_tps[results$errors == 0], na.rm = TRUE),
    median_throughput = median(results$throughput_tps[results$errors == 0], na.rm = TRUE),
    total_tokens = sum(results$total_tokens),
    total_elapsed = sum(results$elapsed_s),
    best_config = attr(results, "best_config"),
    best_throughput = attr(results, "best_throughput")
  )
}

#' Format sweep results summary
#' @param results Data frame from a sweep
#' @return Character string with formatted summary
format_sweep_summary <- function(results) {
  stats <- calculate_sweep_stats(results)
  best <- stats$best_config

  lines <- c(
    "=== Sweep Summary ===",
    sprintf("Total configurations: %d", stats$total_configs),
    sprintf("Successful: %d, Failed: %d", stats$successful, stats$failed),
    "",
    "Throughput Statistics:",
    sprintf("  Min: %.2f tok/s", stats$min_throughput),
    sprintf("  Max: %.2f tok/s", stats$max_throughput),
    sprintf("  Mean: %.2f tok/s", stats$mean_throughput),
    sprintf("  Median: %.2f tok/s", stats$median_throughput),
    "",
    "Best Configuration:",
    sprintf("  Throughput: %.2f tok/s", stats$best_throughput)
  )

  if (!is.null(best)) {
    for (col in names(best)) {
      if (col != "throughput_tps") {
        lines <- c(lines, sprintf("  %s: %s", col, best[[col]]))
      }
    }
  }

  paste(lines, collapse = "\n")
}

#' Estimate sweep duration
#' @param sweep_type Type of sweep
#' @param total_configs Total number of configurations
#' @param warmup Warmup requests per config
#' @param cell_pause Pause between configs
#' @param estimated_request_time Estimated time per request
#' @return Estimated duration in seconds
estimate_sweep_duration <- function(sweep_type,
                                     total_configs,
                                     warmup = 2,
                                     cell_pause = 1,
                                     estimated_request_time = 5) {
  # Requests per config (4 × concurrency, but we'll use average)
  avg_requests_per_config <- 16

  time_per_config <- (warmup + avg_requests_per_config) * estimated_request_time + cell_pause

  # Add server restart time for sweeps that need it
  if (sweep_type %in% c("threads", "full")) {
    server_restart_time <- 30  # Estimated model load time
    time_per_config <- time_per_config + server_restart_time
  }

  total_configs * time_per_config
}
