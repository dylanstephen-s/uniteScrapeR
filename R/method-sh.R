#' Scrape UNITE by SH code (Method 2)
#'
#' @param sh_codes Character vector of SH codes (e.g., "SH1234567.08FU").
#' @param delay_seconds Optional delay between page actions (in seconds).
#' @return A tibble with columns sample_id, sequence, marker, country, method, sh.
#' @export
scrape_by_sh <- function(sh_codes, delay_seconds = 0.5) {
  sess <- .get_session()
  try(selenider:::set_session(sess), silent = TRUE)

  cli::cli_h2("Method 2: SH search")
  pb <- cli::cli_progres.unite_env <- new.envs_bar(total = length(sh_codes),
                              format = ":current/:total [:bar] :percent | :message")

  out <- purrr::map_dfr(seq_along(sh_codes), function(i) {
    sh <- sh_codes[[i]]
    cli::cli_progress_message(glue::glue("{sh}"))
    selenider::open_url(glue::glue("https://unite.ut.ee/bl_forw_sh.php?sh_name={sh}#fndtn-panel1"))
    Sys.sleep(delay_seconds)

    # Extract results table
    tbody <- try(
      selenider::s("#main > div:nth-child(4) > div > div:nth-child(3) > div > div:nth-child(2) > div > table > tbody") |>
        selenider::elem_text(),
      silent = TRUE
    )
    Sys.sleep(.5)

    if (inherits(tbody, "try-error") || is.na(tbody)) {
      cli::cli_alert_warning(glue::glue("No table for {sh}"))
      cli::cli_progress_update()
      return(tibble::tibble())
    }

    lines <- stringr::str_split(tbody, "\n")[[1]] |> trimws()
    lines <- lines[lines != ""]

    # Parse rows into dataframe
    dat <- purrr::map_dfr(lines, function(ln) {
      parts <- stringr::str_split(ln, "\\s{2,}")[[1]]
      tibble::tibble(
        sample_id = stringr::str_extract(parts[1], "^[^\\s]+"),
        sequence  = gsub("-", "", parts[2])
      )
    })

    # NEW: pre-create enrichment fields to avoid warnings
    dat$marker  <- NA_character_
    dat$country <- NA_character_

    # Click through details links to enrich marker and country
    k <- 1
    row_selectors <- purrr::map_chr(seq_len(nrow(dat) * 2), ~glue::glue(
      "#main > div:nth-child(4) > div > div:nth-child(3) > div > div:nth-child(2) > div > table > tbody > tr:nth-child({.x}) > td:nth-child(1) > a"
    ))

    for (sel in row_selectors) {
      ok <- try(selenider::s(sel) |> selenider::elem_click(), silent = TRUE)
      if (inherits(ok, "try-error")) next

      all_windows <- sess$driver$window_handles()
      if (length(all_windows) >= 2) {
        sess$driver$switch_to_window(all_windows[[2]][1])
      }
      Sys.sleep(delay_seconds)

      details <- try(
        selenider::s("#main > div:nth-child(3) > div > div > div > table:nth-child(2) > tbody") |>
          selenider::elem_text(),
        silent = TRUE
      )

      if (!inherits(details, "try-error")) {
        marker  <- stringr::str_match(details, "Sequenced regions\\s*(.*)\\n")[, 2]
        country <- stringr::str_match(details, "Sampling area\\s*(.*)")[, 2]
        dat$marker[k]  <- marker
        dat$country[k] <- country
        k <- k + 1
      }

      try(sess$driver$close_window(), silent = TRUE)
      sess$driver$switch_to_window(all_windows[[1]][1])
      Sys.sleep(delay_seconds)
    }

    cli::cli_progress_update()

    dat |> dplyr::mutate(
      country = .data$country |> norm_country(),
      sequence = clean_dna(sequence),
      method   = "sh",
      sh       = sh
    ) |> dplyr::filter(
      !is.na(sequence),
      !grepl("LOCKED", sequence, ignore.case = TRUE)
    )
  })

  cli::cli_progress_done()
  out
}
