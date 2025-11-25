#' Run a scrape in the background using callr
#'
#' This runs [unite_scrape()] in a separate R session, writing results to
#' `outfile` and logging progress events to a JSONL logfile.
#'
#' @param queries Character vector of species names.
#' @param outfile File path to write results. If it ends with `.csv`
#'   (case-insensitive) a CSV is written, otherwise an RDS file is written.
#' @param delay_seconds Delay between browser actions, passed to
#'   [scrape_by_species()].
#' @param countries Optional character vector of allowed country names;
#'   if supplied, results are filtered to `country %in% countries`.
#'
#' @return A list with components `process` (a `callr::r_bg` object),
#'   `outfile`, and `logfile`, with class `"unite_bg"`.
#' @export
unite_scrape_bg <- function(queries,
                            outfile       = tempfile(fileext = ".rds"),
                            delay_seconds = 0.5,
                            countries     = NULL) {
  logf <- tempfile(fileext = ".jsonl")

  fun <- function(queries, outfile, logf, delay_seconds, countries) {
    # child session: load package namespace
    library(uniteScrapeR)

    progress_append(logf, "start", list(
      n      = length(queries),
      method = "species"
    ))

    start_unite_session(headless = TRUE)
    on.exit(stop_unite_session(), add = TRUE)

    res <- scrape_by_species(
      species       = queries,
      delay_seconds = delay_seconds
    )

    res <- harmonise_columns(res)
    if (!is.null(countries)) {
      res <- dplyr::filter(res, .data$country %in% countries)
    }

    # write output
    if (grepl("\\.csv$", outfile, ignore.case = TRUE)) {
      readr::write_csv(res, outfile)
    } else {
      saveRDS(res, outfile)
    }

    progress_append(logf, "done", list(
      rows    = nrow(res),
      outfile = outfile
    ))

    invisible(TRUE)
  }

  p <- callr::r_bg(
    func = fun,
    args = list(
      queries       = queries,
      outfile       = outfile,
      logf          = logf,
      delay_seconds = delay_seconds,
      countries     = countries
    )
  )

  structure(
    list(
      process = p,
      outfile = outfile,
      logfile = logf
    ),
    class = "unite_bg"
  )
}


#' Tail the background log and print pretty status
#'
#' @param bg An object returned by [unite_scrape_bg()].
#' @param n Number of log lines to show from the end.
#'
#' @export
unite_bg_status <- function(bg, n = 20) {
  if (!inherits(bg, "unite_bg")) {
    rlang::abort("Pass the object returned by unite_scrape_bg().")
  }

  if (!file.exists(bg$logfile)) {
    cli::cli_alert_info("No log yet.")
    return(invisible(NULL))
  }

  lines <- readr::read_lines(bg$logfile)
  if (length(lines) == 0) {
    cli::cli_alert_info("Log is empty.")
    return(invisible(NULL))
  }

  tail(lines, n) |> cat(sep = "\n")
  invisible(lines)
}
