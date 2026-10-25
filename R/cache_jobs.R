# ==============================================================================
# R/cache_jobs.R
# Multi-tier cache manager (Import, FX/Returns, Risk Basis/Matrix, Portfolio,
# Scenarios) with call counters and async background worker diagnostics
# ==============================================================================

create_cache_manager <- function(max_size_mb = 256) {
  store <- cachem::cache_mem(max_size = max_size_mb * 1024^2)
  counters <- new.env(parent = emptyenv())
  counters$import_calls <- 0L
  counters$fx_return_calls <- 0L
  counters$covariance_estimations <- 0L
  counters$portfolio_evaluations <- 0L
  counters$scenario_evaluations <- 0L
  counters$cache_hits <- 0L
  counters$cache_misses <- 0L

  make_key <- function(prefix, parts) {
    hash <- compute_object_hash(parts)
    paste0(tolower(prefix), "_", substr(hash, 1L, 32L))
  }

  get_or_compute <- function(tier, key_parts, compute_fn, counter_name = NULL) {
    k <- make_key(tier, key_parts)
    cached <- store$get(k, missing = NULL)
    if (!is.null(cached)) {
      counters$cache_hits <- counters$cache_hits + 1L
      return(list(value = cached, cache_hit = TRUE, cache_key = k))
    }
    counters$cache_misses <- counters$cache_misses + 1L
    if (!is.null(counter_name) && exists(counter_name, envir = counters, inherits = FALSE)) {
      assign(counter_name, get(counter_name, envir = counters, inherits = FALSE) + 1L, envir = counters)
    }
    val <- compute_fn()
    if (is.list(val) && isTRUE(val$valid)) {
      store$set(k, val)
    }
    list(value = val, cache_hit = FALSE, cache_key = k)
  }

  get_stats <- function() {
    list(
      import_calls = counters$import_calls,
      fx_return_calls = counters$fx_return_calls,
      covariance_estimations = counters$covariance_estimations,
      portfolio_evaluations = counters$portfolio_evaluations,
      scenario_evaluations = counters$scenario_evaluations,
      cache_hits = counters$cache_hits,
      cache_misses = counters$cache_misses,
      keys_count = length(store$keys())
    )
  }

  reset_all <- function() {
    store$reset()
  }

  list(
    store = store,
    counters = counters,
    make_key = make_key,
    get_or_compute = get_or_compute,
    get_stats = get_stats,
    reset_all = reset_all
  )
}

init_async_workers <- function(max_workers = 1L) {
  status <- list(
    async_supported = FALSE,
    backend = "sequential",
    workers = 1L,
    message = "Synchroner Fallback-Modus aktiv."
  )

  if (requireNamespace("future", quietly = TRUE) && requireNamespace("promises", quietly = TRUE)) {
    tryCatch({
      current_plan <- class(future::plan())[1L]
      if (identical(current_plan, "sequential") || identical(current_plan, "default")) {
        n_w <- max(1L, min(as.integer(max_workers), 2L))
        future::plan(future::multisession, workers = n_w)
      }
      status$async_supported <- TRUE
      status$backend <- class(future::plan())[1L]
      status$workers <- as.integer(future::nbrOfWorkers())
      status$message <- sprintf("Asynchrone Hintergrundjobs aktiv (%s, %d Worker).", status$backend, status$workers)
    }, error = function(e) {
      tryCatch(future::plan(future::sequential), error = function(...) NULL)
      status$async_supported <<- FALSE
      status$backend <<- "sequential"
      status$workers <<- 1L
      status$message <<- sprintf("Hintergrundprozesse in dieser Umgebung eingeschränkt (%s); synchroner Batchmodus aktiv.", conditionMessage(e))
    })
  }
  status
}
