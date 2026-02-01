# Test Runner Module for Llama Throughput Lab
# Handles single, concurrent, and round-robin test execution

#' Send a completion request to the server
#' @param base_url Base URL of the server
#' @param prompt Input prompt
#' @param n_predict Tokens to generate
#' @param temperature Sampling temperature
#' @param timeout Request timeout in seconds
#' @return List with success, tokens, elapsed, response, error
send_completion_request <- function(base_url,
                                     prompt,
                                     n_predict = 50,
                                     temperature = 0.7,
                                     timeout = 120) {
  url <- paste0(base_url, "/completion")

  body <- list(
    prompt = prompt,
    n_predict = n_predict,
    temperature = temperature,
    stream = FALSE
  )

  start_time <- Sys.time()

  tryCatch({
    response <- httr::POST(
      url,
      body = jsonlite::toJSON(body, auto_unbox = TRUE),
      httr::content_type_json(),
      httr::timeout(timeout)
    )

    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

    if (httr::status_code(response) == 200) {
      content <- httr::content(response, as = "parsed", type = "application/json")
      tokens <- extract_token_count(content)
      timing <- extract_timing(content)

      list(
        success = TRUE,
        tokens = tokens,
        elapsed = elapsed,
        tokens_per_second = timing$tokens_per_second,
        response = content,
        error = NULL
      )
    } else {
      list(
        success = FALSE,
        tokens = 0,
        elapsed = elapsed,
        tokens_per_second = 0,
        response = NULL,
        error = paste("HTTP", httr::status_code(response))
      )
    }
  }, error = function(e) {
    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    list(
      success = FALSE,
      tokens = 0,
      elapsed = elapsed,
      tokens_per_second = 0,
      response = NULL,
      error = as.character(e)
    )
  })
}

#' Send a completion request with retry logic
#' @param base_url Base URL of the server
#' @param prompt Input prompt
#' @param n_predict Tokens to generate
#' @param temperature Sampling temperature
#' @param timeout Request timeout in seconds
#' @param max_attempts Maximum retry attempts
#' @param base_sleep Base sleep time between retries
#' @return List with success, tokens, elapsed, response, error, attempts
send_completion_with_retry <- function(base_url,
                                        prompt,
                                        n_predict = 50,
                                        temperature = 0.7,
                                        timeout = 120,
                                        max_attempts = 3,
                                        base_sleep = 2) {
  retry_codes <- c("500", "502", "503", "504")
  total_elapsed <- 0

  for (attempt in seq_len(max_attempts)) {
    result <- send_completion_request(
      base_url = base_url,
      prompt = prompt,
      n_predict = n_predict,
      temperature = temperature,
      timeout = timeout
    )

    total_elapsed <- total_elapsed + result$elapsed

    if (result$success) {
      result$attempts <- attempt
      result$elapsed <- total_elapsed
      return(result)
    }

    # Check if error is retryable
    should_retry <- any(sapply(retry_codes, function(code) {
      grepl(code, result$error)
    }))

    if (!should_retry || attempt == max_attempts) {
      result$attempts <- attempt
      result$elapsed <- total_elapsed
      return(result)
    }

    # Exponential backoff
    sleep_time <- base_sleep * (2 ^ (attempt - 1))
    Sys.sleep(sleep_time)
  }
}

#' Run a single request test
#' @param base_url Base URL of the server
#' @param prompt Input prompt
#' @param n_predict Tokens to generate
#' @param temperature Sampling temperature
#' @param timeout Request timeout in seconds
#' @return List with tokens, throughput, elapsed, errors
run_single_test <- function(base_url,
                            prompt,
                            n_predict = 50,
                            temperature = 0.7,
                            timeout = 120) {
  result <- send_completion_request(
    base_url = base_url,
    prompt = prompt,
    n_predict = n_predict,
    temperature = temperature,
    timeout = timeout
  )

  throughput <- if (result$elapsed > 0) result$tokens / result$elapsed else 0

  list(
    tokens = result$tokens,
    throughput = throughput,
    elapsed = result$elapsed,
    errors = if (result$success) 0 else 1,
    details = result
  )
}

#' Run concurrent request test
#' @param base_url Base URL of the server
#' @param prompt Input prompt
#' @param n_predict Tokens to generate
#' @param temperature Sampling temperature
#' @param concurrency Number of concurrent requests
#' @param num_requests Total number of requests
#' @param timeout Request timeout in seconds
#' @param retry_attempts Max retry attempts per request
#' @param retry_sleep Base retry sleep time
#' @param progress_callback Optional callback function(completed, total)
#' @return List with tokens, throughput, elapsed, errors, per_request
run_concurrent_test <- function(base_url,
                                 prompt,
                                 n_predict = 50,
                                 temperature = 0.7,
                                 concurrency = 4,
                                 num_requests = 16,
                                 timeout = 120,
                                 retry_attempts = 3,
                                 retry_sleep = 2,
                                 progress_callback = NULL) {
  # Use parallel processing if available
  use_parallel <- requireNamespace("future", quietly = TRUE) &&
                  requireNamespace("future.apply", quietly = TRUE)

  start_time <- Sys.time()

  if (use_parallel) {
    # Set up parallel execution
    oplan <- future::plan(future::multisession, workers = concurrency)
    on.exit(future::plan(oplan), add = TRUE)

    results <- future.apply::future_lapply(seq_len(num_requests), function(i) {
      result <- send_completion_with_retry(
        base_url = base_url,
        prompt = prompt,
        n_predict = n_predict,
        temperature = temperature,
        timeout = timeout,
        max_attempts = retry_attempts,
        base_sleep = retry_sleep
      )

      if (!is.null(progress_callback)) {
        # Note: This won't work well in parallel, but we'll track at the end
      }

      result
    }, future.seed = TRUE)
  } else {
    # Sequential execution with batching to simulate concurrency
    results <- list()

    for (batch_start in seq(1, num_requests, by = concurrency)) {
      batch_end <- min(batch_start + concurrency - 1, num_requests)
      batch_size <- batch_end - batch_start + 1

      # Send batch of requests
      # For true concurrency without future, we'd need curl_multi or similar
      for (i in batch_start:batch_end) {
        results[[i]] <- send_completion_with_retry(
          base_url = base_url,
          prompt = prompt,
          n_predict = n_predict,
          temperature = temperature,
          timeout = timeout,
          max_attempts = retry_attempts,
          base_sleep = retry_sleep
        )

        if (!is.null(progress_callback)) {
          progress_callback(i, num_requests)
        }
      }
    }
  }

  total_elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

  # Aggregate results
  total_tokens <- sum(sapply(results, function(r) r$tokens))
  errors <- sum(sapply(results, function(r) if (r$success) 0 else 1))
  throughput <- if (total_elapsed > 0) total_tokens / total_elapsed else 0

  per_request <- data.frame(
    request = seq_len(num_requests),
    tokens = sapply(results, function(r) r$tokens),
    elapsed = sapply(results, function(r) r$elapsed),
    success = sapply(results, function(r) r$success),
    attempts = sapply(results, function(r) r$attempts %||% 1),
    stringsAsFactors = FALSE
  )

  list(
    tokens = total_tokens,
    throughput = throughput,
    elapsed = total_elapsed,
    errors = errors,
    num_requests = num_requests,
    concurrency = concurrency,
    per_request = per_request
  )
}

#' Run warmup requests
#' @param base_url Base URL of the server
#' @param prompt Input prompt
#' @param n_predict Tokens to generate
#' @param num_warmup Number of warmup requests
#' @param timeout Request timeout
#' @return Number of successful warmup requests
run_warmup <- function(base_url,
                       prompt,
                       n_predict = 50,
                       num_warmup = 2,
                       timeout = 120) {
  success_count <- 0

  for (i in seq_len(num_warmup)) {
    result <- send_completion_request(
      base_url = base_url,
      prompt = prompt,
      n_predict = n_predict,
      timeout = timeout
    )

    if (result$success) {
      success_count <- success_count + 1
    }
  }

  success_count
}

#' Run a test configuration and return standardized results
#' @param base_url Base URL of the server
#' @param prompt Input prompt
#' @param n_predict Tokens to generate
#' @param temperature Sampling temperature
#' @param concurrency Concurrency level
#' @param num_requests Total requests (calculated as concurrency * multiplier)
#' @param timeout Request timeout
#' @param warmup Number of warmup requests
#' @param retry_attempts Max retries
#' @param retry_sleep Retry base sleep
#' @return List with standardized test results
run_test_config <- function(base_url,
                            prompt,
                            n_predict = 50,
                            temperature = 0.7,
                            concurrency = 4,
                            num_requests = NULL,
                            timeout = 120,
                            warmup = 2,
                            retry_attempts = 3,
                            retry_sleep = 2) {
  # Calculate num_requests if not provided
  if (is.null(num_requests)) {
    multiplier <- 4  # Default multiplier
    num_requests <- concurrency * multiplier
  }

  # Run warmup
  if (warmup > 0) {
    run_warmup(base_url, prompt, n_predict, warmup, timeout)
  }

  # Run actual test
  result <- run_concurrent_test(
    base_url = base_url,
    prompt = prompt,
    n_predict = n_predict,
    temperature = temperature,
    concurrency = concurrency,
    num_requests = num_requests,
    timeout = timeout,
    retry_attempts = retry_attempts,
    retry_sleep = retry_sleep
  )

  result
}

#' Check if server is responding
#' @param base_url Base URL to check
#' @param timeout Timeout in seconds
#' @return TRUE if server is responding
check_server_ready <- function(base_url, timeout = 5) {
  url <- paste0(base_url, "/health")

  tryCatch({
    response <- httr::GET(url, httr::timeout(timeout))
    status <- httr::status_code(response)
    if (status == 200) {
      return(TRUE)
    }
    if (status == 404) {
      fallback_url <- paste0(base_url, "/v1/models")
      fallback_response <- httr::GET(fallback_url, httr::timeout(timeout))
      return(httr::status_code(fallback_response) == 200)
    }
    FALSE
  }, error = function(e) FALSE)
}

#' Check if model is loaded and ready
#' @param base_url Base URL to check
#' @param timeout Timeout in seconds
#' @return TRUE if model is ready
check_model_ready <- function(base_url, timeout = 10) {
  url <- paste0(base_url, "/v1/models")

  tryCatch({
    response <- httr::GET(url, httr::timeout(timeout))
    if (httr::status_code(response) == 200) {
      content <- httr::content(response, as = "parsed")
      # Check if data contains model info
      length(content$data) > 0
    } else {
      FALSE
    }
  }, error = function(e) FALSE)
}

#' Wait for server to be fully ready
#' @param base_url Base URL of server
#' @param timeout Maximum wait time in seconds
#' @param interval Check interval in seconds
#' @return TRUE if server is ready, FALSE if timeout
wait_for_ready <- function(base_url, timeout = 300, interval = 2) {
  start_time <- Sys.time()

  while (difftime(Sys.time(), start_time, units = "secs") < timeout) {
    # First check health
    if (check_server_ready(base_url, timeout = 5)) {
      # Then check model
      if (check_model_ready(base_url, timeout = 10)) {
        return(TRUE)
      }
    }
    Sys.sleep(interval)
  }

  FALSE
}

# Null coalescing operator
`%||%` <- function(a, b) if (is.null(a)) b else a
