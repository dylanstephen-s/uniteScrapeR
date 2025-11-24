#' Safe element getter with friendly error
#' @keywords internal

sel_safe <- function(selector, timeout = 10) {
  start <- Sys.time()
  repeat {
    ok <- tryCatch({
      el <- selenider::s(selector)
      TRUE
    }, error = function(e) FALSE)
    if (ok) return(selenider::s(selector))
    if (difftime(Sys.time(), start, units = "secs") > timeout) {
      rlang::abort(glue::glue("Selector not found after {timeout}s: {selector}"))
    }
    Sys.sleep(0.2)
  }
}


#' Clip helper between two markers in a string (if present)
#' @keywords internal
str_clip_between <- function(x, begin, end) {
  x <- sub(paste0(".*", begin), "", x)
  sub(paste0(end, ".*"), "", x)
}


#' DNA sanity filter (IUPAC, uppercase, no spaces)
#' @keywords internal
clean_dna <- function(x) {
  x <- gsub("\\s+", "", x)
  x <- toupper(x)
  ok <- grepl("^[TCGAMNWRYSKBDHV]+$", x)
  ifelse(ok, x, NA_character_)
}


#' Basic country normalisation
#' @keywords internal
norm_country <- function(x) {
  x <- dplyr::case_when(
    grepl("Unspecified", x, ignore.case = TRUE) ~ NA_character_,
    grepl("Russian Federation", x, ignore.case = TRUE) ~ "Russia",
    TRUE ~ x
  )
  trimws(x)
}


#' Write a JSONL progress line for background runs
#' @keywords internal
progress_append <- function(path, event, data = list()) {
  rec <- list(time = format(Sys.time(), tz = "UTC", usetz = TRUE), event = event, data = data)
  write(jsonlite::toJSON(rec, auto_unbox = TRUE), file = path, append = TRUE)
  write("\n", file = path, append = TRUE)
}


#' @importFrom rlang .data
#' @importFrom utils tail
NULL


