#' Run a scrape in the background using callr
#'
#' @inheritParams unite_scrape
#' @return A list with `process` (callr::r_bg object), `outfile`, and `logfile`.
#' @export
unite_scrape_bg <- function(queries, method = c("species","sh"), outfile = tempfile(fileext = ".rds"),
                            delay_seconds = 0.5, countries = NULL) {
  method <- rlang::arg_match(method)
  logf <- tempfile(fileext = ".jsonl")


  fun <- function(queries, method, outfile, logf, delay_seconds, countries) {
    # child session: load package namespace
    library(uniteScrapeR)
    progress_append(logf, "start", list(n = length(queries), method = method))
    start_unite_session(headless = TRUE)
    on.exit(stop_unite_session(), add = TRUE)


    res <- switch(method,
                  species = scrape_by_species(queries, delay_seconds = delay_seconds),
                  sh = scrape_by_sh(queries, delay_seconds = delay_seconds)
    )
    res <- harmonise_columns(res)
    if (!is.null(countries)) res <- dplyr::filter(res, .data$country %in% countries)


    # write output
    if (grepl("\\.csv$", outfile, ignore.case = TRUE)) {
      readr::write_csv(res, outfile)
    } else {
      saveRDS(res, outfile)
    }
    progress_append(logf, "done", list(rows = nrow(res), outfile = outfile))
    invisible(TRUE)
  }


  p <- callr::r_bg(fun, args = list(queries = queries, method = method, outfile = outfile, logf = logf,
                                    delay_seconds = delay_seconds, countries = countries))
  structure(list(process = p, outfile = outfile, logfile = logf), class = "unite_bg")
}


#' Tail the background log and print pretty status
#' @param bg An object returned by unite_scrape_bg().
#' @param n Number of lines to show from the end.
#' @export
unite_bg_status <- function(bg, n = 20) {
  if (!inherits(bg, "unite_bg")) rlang::abort("Pass the object returned by unite_scrape_bg().")
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
