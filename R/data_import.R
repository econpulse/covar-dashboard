# ==============================================================================
# R/data_import.R
# Excel wide workbook reader, month header parser, and long table normalizer
# Pure R module - no Shiny dependencies
# ==============================================================================

parse_month_headers <- function(headers) {
  headers_chr <- as.character(headers)
  n <- length(headers_chr)
  issues <- list()

  if (n == 0L) {
    issues[[length(issues) + 1L]] <- make_issue(
      code = "EMPTY_DATE_HEADERS",
      severity = "ERROR",
      message = "Keine Datumsspalten im Datenblatt gefunden.",
      scope = "IMPORT"
    )
    return(list(
      valid = FALSE,
      headers = character(0),
      dates = as.Date(character(0)),
      month_keys = character(0),
      month_index = integer(0),
      convention = "UNKNOWN",
      issues = issues
    ))
  }

  parsed_dates <- rep(as.Date(NA), n)
  conventions <- character(n)

  for (i in seq_len(n)) {
    h <- trimws(headers_chr[i])
    num_val <- suppressWarnings(as.numeric(h))
    if (!is.na(num_val) && grepl("^[0-9]+(\\.[0-9]+)?$", h) && num_val >= 10000 && num_val <= 90000) {
      # Excel serial date (1900 date system with Excel leap-year bug -> origin 1899-12-30)
      parsed_dates[i] <- as.Date(round(num_val), origin = "1899-12-30")
      conventions[i] <- "EXCEL_SERIAL"
    } else if (grepl("^[0-9]{2}\\.[0-9]{2}\\.[0-9]{4}$", h)) {
      parsed_dates[i] <- as.Date(h, format = "%d.%m.%Y")
      conventions[i] <- "DD.MM.YYYY"
    } else if (grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", h)) {
      parsed_dates[i] <- as.Date(h, format = "%Y-%m-%d")
      conventions[i] <- "YYYY-MM-DD"
    } else if (grepl("^[0-9]{4}/[0-9]{2}/[0-9]{2}$", h)) {
      parsed_dates[i] <- as.Date(h, format = "%Y/%m/%d")
      conventions[i] <- "YYYY/MM/DD"
    } else {
      conventions[i] <- "INVALID"
    }
  }

  bad_idx <- which(is.na(parsed_dates))
  if (length(bad_idx) > 0L) {
    issues[[length(issues) + 1L]] <- make_issue(
      code = "INVALID_DATE_HEADER",
      severity = "ERROR",
      message = sprintf(
        "Ungültige Datumsspalten-Header gefunden (%d Spalten): %s",
        length(bad_idx),
        paste(utils::head(headers_chr[bad_idx], 5L), collapse = ", ")
      ),
      scope = "IMPORT",
      details = list(invalid_headers = headers_chr[bad_idx], indices = bad_idx)
    )
  }

  month_keys <- ifelse(is.na(parsed_dates), NA_character_, format(parsed_dates, "%Y-%m"))
  years <- as.integer( ifelse(is.na(parsed_dates), NA_integer_, format(parsed_dates, "%Y")) )
  months <- as.integer( ifelse(is.na(parsed_dates), NA_integer_, format(parsed_dates, "%m")) )
  month_index <- years * 12L + months

  # Check duplicate months
  valid_keys <- month_keys[!is.na(month_keys)]
  dup_keys <- unique(valid_keys[duplicated(valid_keys)])
  if (length(dup_keys) > 0L) {
    issues[[length(issues) + 1L]] <- make_issue(
      code = "DUPLICATE_MONTH_HEADER",
      severity = "ERROR",
      message = sprintf(
        "Doppelte Monatsschlüssel in den Spaltenheadern gefunden: %s",
        paste(dup_keys, collapse = ", ")
      ),
      scope = "IMPORT",
      details = list(duplicate_months = dup_keys)
    )
  }

  # Check strict chronological order and month gaps in header grid
  if (length(bad_idx) == 0L && length(dup_keys) == 0L && n > 1L) {
    diffs <- diff(month_index)
    if (any(diffs <= 0L)) {
      issues[[length(issues) + 1L]] <- make_issue(
        code = "NON_MONOTONIC_DATE_HEADERS",
        severity = "ERROR",
        message = "Datumsspalten sind nicht streng chronologisch aufsteigend sortiert.",
        scope = "IMPORT"
      )
    } else if (any(diffs > 1L)) {
      gap_pos <- which(diffs > 1L)
      issues[[length(issues) + 1L]] <- make_issue(
        code = "HEADER_MONTH_GAP",
        severity = "WARNING",
        message = sprintf(
          "Zwischen %d Datumsspalten fehlen Kalendermonate im Spaltenraster (keine Überbrückung bei Returnberechnung).",
          length(gap_pos)
        ),
        scope = "IMPORT",
        details = list(
          gaps_after_month = month_keys[gap_pos],
          gaps_before_month = month_keys[gap_pos + 1L]
        )
      )
    }
  }

  dom_convention <- names(sort(table(conventions[conventions != "INVALID"]), decreasing = TRUE))[1L] %||% "UNKNOWN"
  has_errors <- any(vapply(issues, function(x) identical(x$severity, "ERROR"), logical(1)))

  list(
    valid = !has_errors,
    headers = headers_chr,
    dates = parsed_dates,
    month_keys = month_keys,
    month_index = month_index,
    convention = dom_convention,
    issues = issues
  )
}

normalize_series_table <- function(raw, parsed_headers = NULL) {
  if (!is.data.frame(raw)) {
    stop("raw input must be a data.frame or tibble", call. = FALSE)
  }
  col_names <- names(raw)
  if (!("Code" %in% col_names)) {
    return(list(
      valid = FALSE,
      data = data.table::data.table(),
      provenance = data.table::data.table(),
      issues = list(make_issue(
        code = "MISSING_CODE_COLUMN",
        severity = "ERROR",
        message = "Pflichtspalte 'Code' fehlt im Rohdatenblatt.",
        scope = "IMPORT"
      ))
    ))
  }

  # Optional 'Name' column is ignored for value import; authoritative names come from Metadata
  meta_cols <- intersect(c("Code", "Name"), col_names)
  date_cols <- setdiff(col_names, meta_cols)

  if (is.null(parsed_headers)) {
    parsed_headers <- parse_month_headers(date_cols)
  }
  issues <- parsed_headers$issues

  codes <- trimws(as.character(raw[["Code"]]))
  raw_names <- if ("Name" %in% col_names) as.character(raw[["Name"]]) else rep(NA_character_, nrow(raw))

  empty_code_idx <- which(is.na(codes) | !nzchar(codes))
  if (length(empty_code_idx) > 0L) {
    issues[[length(issues) + 1L]] <- make_issue(
      code = "EMPTY_SERIES_CODE",
      severity = "ERROR",
      message = sprintf("Leere Code-Einträge in Zeilen: %s", paste(empty_code_idx, collapse = ", ")),
      scope = "IMPORT"
    )
  }

  dup_codes <- unique(codes[!is.na(codes) & duplicated(codes)])
  if (length(dup_codes) > 0L) {
    issues[[length(issues) + 1L]] <- make_issue(
      code = "DUPLICATE_SERIES_CODE_IN_RAW",
      severity = "ERROR",
      message = sprintf("Doppelte Zeitreihen-Codes im Rohdatenblatt: %s", paste(dup_codes, collapse = ", ")),
      scope = "IMPORT",
      details = list(duplicate_codes = dup_codes)
    )
  }

  if (!parsed_headers$valid || length(empty_code_idx) > 0L || length(dup_codes) > 0L) {
    return(list(
      valid = FALSE,
      data = data.table::data.table(),
      provenance = data.table::data.table(),
      parsed_headers = parsed_headers,
      issues = issues
    ))
  }

  # Extract numeric matrix carefully without altering exact Code strings
  val_list <- lapply(date_cols, function(col_nm) {
    col_vec <- raw[[col_nm]]
    if (is.numeric(col_vec)) {
      as.numeric(col_vec)
    } else {
      suppressWarnings(as.numeric(as.character(col_vec)))
    }
  })
  val_mat <- do.call(cbind, val_list)
  n_rows <- nrow(raw)
  n_dates <- length(date_cols)

  long_dt <- data.table::data.table(
    date = rep(parsed_headers$dates, each = n_rows),
    month_key = rep(parsed_headers$month_keys, each = n_rows),
    month_index = rep(parsed_headers$month_index, each = n_rows),
    Code = rep(codes, times = n_dates),
    value = as.numeric(val_mat)
  )
  data.table::setorder(long_dt, Code, month_index)

  provenance_dt <- data.table::data.table(
    row_index = seq_len(n_rows),
    Code = codes,
    raw_name = raw_names
  )

  list(
    valid = TRUE,
    data = long_dt,
    provenance = provenance_dt,
    parsed_headers = parsed_headers,
    issues = issues
  )
}

read_input_workbook <- function(path, config = NULL) {
  issues <- list()
  if (!file.exists(path)) {
    issues[[1L]] <- make_issue(
      code = "WORKBOOK_NOT_FOUND",
      severity = "ERROR",
      message = sprintf("Eingabedatei nicht gefunden: %s", path),
      scope = "IMPORT"
    )
    return(list(valid = FALSE, issues = issues))
  }

  file_hash <- compute_file_hash(path)
  sheets <- tryCatch(
    readxl::excel_sheets(path),
    error = function(e) {
      issues[[length(issues) + 1L]] <<- make_issue(
        code = "WORKBOOK_READ_ERROR",
        severity = "ERROR",
        message = sprintf("Excel-Datei konnte nicht gelesen werden: %s", conditionMessage(e)),
        scope = "IMPORT"
      )
      character(0)
    }
  )
  if (length(sheets) == 0L) {
    return(list(valid = FALSE, file_hash = file_hash, issues = issues))
  }

  meta_sheet <- if ("Metadata" %in% sheets) "Metadata" else sheets[grep("meta", sheets, ignore.case = TRUE)[1L]]
  data_sheet <- if ("Asset_TR_Monthly" %in% sheets) {
    "Asset_TR_Monthly"
  } else {
    setdiff(sheets, meta_sheet)[1L]
  }

  if (is.na(meta_sheet) || is.na(data_sheet)) {
    issues[[length(issues) + 1L]] <- make_issue(
      code = "MISSING_WORKBOOK_SHEETS",
      severity = "ERROR",
      message = sprintf(
        "Erwartete Tabellenblätter ('Metadata' und 'Asset_TR_Monthly') nicht gefunden. Vorhanden: %s",
        paste(sheets, collapse = ", ")
      ),
      scope = "IMPORT"
    )
    return(list(valid = FALSE, file_hash = file_hash, issues = issues))
  }

  # Read without name repair so exact Code strings and date headers remain untouched
  meta_raw <- as.data.frame(
    readxl::read_excel(path, sheet = meta_sheet, .name_repair = "minimal"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  wide_raw <- as.data.frame(
    readxl::read_excel(path, sheet = data_sheet, .name_repair = "minimal"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  norm_res <- normalize_series_table(wide_raw)
  issues <- c(issues, norm_res$issues)
  has_errors <- any(vapply(issues, function(x) identical(x$severity, "ERROR"), logical(1)))

  list(
    valid = !has_errors && isTRUE(norm_res$valid),
    path = normalizePath(path, winslash = "/", mustWork = FALSE),
    file_hash = file_hash,
    sheets = list(metadata = meta_sheet, data = data_sheet, all = sheets),
    meta_raw = meta_raw,
    wide_raw = wide_raw,
    long_data = norm_res$data,
    provenance = norm_res$provenance,
    parsed_headers = norm_res$parsed_headers,
    issues = issues
  )
}
