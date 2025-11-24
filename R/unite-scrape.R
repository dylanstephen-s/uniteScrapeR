#' High-level wrapper
#'
#' @param queries Character vector of species names (method = "species") or SH codes (method = "sh").
#' @param method One of "species" or "sh".
#' @param headless Pass to start_unite_session().
#' @param background If TRUE, run in a background R session (writes result to `outfile`).
#' @param outfile File path to write results (csv or rds). Used for background mode.
#' @param delay_seconds Delay between actions.
#' @param countries Optional character vector whitelist (e.g., European countries). If provided, result is filtered to these.
#' @return tibble (foreground) or invisibly the background process handle.
#' @export
#'
# in R/unite-scrape.R
#' @importFrom rlang .data
unite_scrape <- function(queries,
                         method = c("species"),
                         ...) {
  method <- match.arg(method)

  if (method == "species") {
    out <- scrape_by_species(queries, ...)
  }

  # final clean
  out |>
    dplyr::select(-dplyr::any_of(c("sh", "source")))
}


#' Make columns consistent across methods and add metadata
#' @keywords internal
# Keep final columns minimal & consistent across methods
harmonise_columns <- function(df) {
  # desired output (note: no sample_id, no source)
  cols <- c("accession","sh","species","country","marker","sequence","method")
  for (nm in cols) if (!nm %in% names(df)) df[[nm]] <- NA_character_
  df <- df |>
    dplyr::select(dplyr::all_of(cols)) |>
    tibble::as_tibble()
  df$date_scraped <- as.Date(Sys.Date())
  df
}


#' @importFrom rlang .data
#' @importFrom utils tail
NULL
