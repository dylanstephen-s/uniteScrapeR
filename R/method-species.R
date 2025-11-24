#' Scrape UNITE + INSD by species name (Method 1 – variants + pagination)
#'
#' For each species, and for each dataset (UNITE, INSD), this:
#'   1) Opens the Sequences tab with the chosen dataset.
#'   2) Selects all regions and the "include subtaxa" box.
#'   3) Types the species name.
#'   4) Iterates over dropdown variants:
#'        - attempt 1: Enter (first option)
#'        - attempt >1: ArrowDown^(attempt-1) then Enter
#'      It reads the selected label from
#'      `#taxon_name_repr_div_seq > span:nth-child(1)` and stops when it
#'      wraps around (same label seen again).
#'   5) For each variant that yields records, it paginates over *all* pages,
#'      clicks each details link, and scrapes marker + sequence + country.
#'
#' @param species Character vector of species names.
#' @param delay_seconds Delay between actions (seconds).
#' @param max_variants Safety cap on dropdown variants per species+dataset.
#' @param max_pages Safety cap on pages per variant.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{accession}{accession as shown in the main table}
#'     \item{species}{species name queried}
#'     \item{country}{country (details-page Sampling area if available, else table)}
#'     \item{marker}{Sequenced regions}
#'     \item{sequence}{nucleotide sequence}
#'     \item{method}{"species"}
#'     \item{database}{"UNITE" or "INSD"}
#'     \item{date_scraped}{Sys.Date()}
#'   }
#' @export
scrape_by_species <- function(species,
                              delay_seconds = 1,
                              max_variants = 20,
                              max_pages = 50) {

  # One session for the whole run
  session <- selenider::selenider_session("selenium", browser = "chrome")
  on.exit(try(session$close(), silent = TRUE))

  ENTER      <- "\uE007"
  ARROW_DOWN <- "\uE015"

  # --- helpers -------------------------------------------------------------

  # Move mouse somewhere "safe" so it doesn't hover in the dropdown
  park_mouse <- function() {
    try({
      selenider::s("body") |>
        selenider::elem_hover()
    }, silent = TRUE)
  }

  # Read the selected label in the chip area
  read_label <- function(timeout = 3, poll = 0.1) {
    t0 <- Sys.time()
    css <- "#taxon_name_repr_div_seq > span:nth-child(1)"
    repeat {
      lab <- try(selenider::s(css) |> selenider::elem_text(), silent = TRUE)
      if (!inherits(lab, "try-error")) {
        lab <- trimws(as.character(lab))
        if (nzchar(lab)) return(lab)
      }
      if (as.numeric(difftime(Sys.time(), t0, units = "secs")) > timeout) break
      Sys.sleep(poll)
    }
    NA_character_
  }

  # Clear chip (small "x" next to species)
  clear_species_chip <- function() {
    try({
      selenider::s("#taxon_name_repr_div_seq > span > a > i") |>
        selenider::elem_click()
      Sys.sleep(0.2)
    }, silent = TRUE)
  }

  # Select all regions
  select_regions <- function() {
    try(
      selenider::s("#region_seq_all") |>
        selenider::elem_click(),
      silent = TRUE
    )
  }

  # Ensure "include subtaxa" is ticked
  select_subtaxa <- function() {
    try({
      cb <- selenider::s("#parent_taxon_seq")
      chk <- try(cb |> selenider::elem_attr("checked"), silent = TRUE)
      if (inherits(chk, "try-error") || is.na(chk) ||
          (!identical(chk, TRUE) && !identical(chk, "true"))) {
        cb |> selenider::elem_click()
      }
    }, silent = TRUE)
  }

  # For a given species + dataset, select attempt-th dropdown option and return label
  select_variant <- function(sp, dataset, attempt, delay_seconds) {
    base_url <- paste0(
      "https://unite.ut.ee/search.php?",
      "qresult_seq=yes",
      "&dataset=", dataset,
      "&taxon_name_seq=&taxon_id_seq=&taxon_name_repr_seq=&accno_seq=&",
      "it_seq=&it_id_seq=&it_repr_seq=&country_seq=&country_id_seq=&country_repr_seq=&",
      "insd_organism=&source_seq=&ecm_lineage_seq=&region_seq=1#fndtn-panel2"
    )

    selenider::open_url(base_url)
    Sys.sleep(delay_seconds)

    # 1) all regions, 2) subtaxa
    select_regions()
    select_subtaxa()

    # mouse out of the way
    park_mouse()

    clear_species_chip()

    # type species
    search <- selenider::s("#taxon_name_seq")
    search |>
      selenider::elem_clear_value() |>
      selenider::elem_send_keys(sp)

    Sys.sleep(0.7)

    # attempt 1: Enter (first option)
    # attempt >1: ArrowDown^(attempt-1) then Enter
    if (attempt > 1L) {
      for (j in seq_len(attempt - 1L)) {
        search |> selenider::elem_send_keys(ARROW_DOWN)
        Sys.sleep(0.08)
      }
    }
    search |> selenider::elem_send_keys(ENTER)
    Sys.sleep(0.35)

    lab <- read_label(timeout = 2, poll = 0.1)
    if (!nzchar(lab) || is.na(lab)) {
      search |> selenider::elem_send_keys(ENTER)
      Sys.sleep(0.35)
      lab <- read_label(timeout = 1.3, poll = 0.1)
    }
    lab
  }

  paginate_current_results <- function(sp, dataset,
                                       delay_seconds,
                                       max_pages) {
    all_pages <- list()
    page_counter <- 1L
    seen_first_accessions <- character(0)

    repeat {
      # --- read table on current page ----------------------------------------
      species_info <- tryCatch(
        {
          selenider::s("#panel2_results > div > div > table > tbody") |>
            selenider::elem_text()
        },
        error = function(e) NA_character_
      )

      if (is.na(species_info)) {
        message(sprintf("Couldn't read results table in %s for: %s (page %d)",
                        toupper(dataset), sp, page_counter))
        break
      }

      beginning <- "Accession number UNITE taxon name Country Source"
      end       <- "TWEETS"

      clipped_text <- sub(paste0(".*", beginning), "", species_info)
      clipped_text <- sub(paste0(end, ".*"), "", clipped_text)

      rows <- unlist(strsplit(clipped_text, "\n"))
      rows <- rows[rows != ""]
      rows <- trimws(rows)
      n_records <- length(rows)

      if (!n_records) {
        message(sprintf("No visible rows in %s for: %s (page %d)",
                        toupper(dataset), sp, page_counter))
        break
      }

      rows_clean  <- sub("^\\d+\\.\\s*", "", rows)
      split_rows  <- strsplit(rows_clean, "\\s+")
      accession_numbers <- vapply(split_rows, function(x) x[1], character(1))
      taxon_names       <- rep(sp, length(accession_numbers))

      # --- NEW: break if we see the same page again --------------------------
      first_acc <- accession_numbers[1]
      if (first_acc %in% seen_first_accessions) {
        message(sprintf(
          "First accession %s already seen for %s (%s) – stopping pagination.",
          first_acc, sp, toupper(dataset)
        ))
        break
      }
      seen_first_accessions <- c(seen_first_accessions, first_acc)

      countries_sources <- vapply(
        split_rows,
        function(x) paste(x[4:length(x)], collapse = " "),
        character(1)
      )
      cs_split  <- stringr::str_split_fixed(countries_sources, "Fungus:", 2)
      countries <- trimws(cs_split[, 1])

      df_page <- tibble::tibble(
        accession = accession_numbers,
        species   = taxon_names,
        country   = countries,
        marker    = NA_character_,
        sequence  = NA_character_
      )

      Sys.sleep(delay_seconds)

      # --- click each row on THIS page and scrape details --------------------
      for (i in seq_len(n_records)) {
        if (i == 1) {
          selector <- "#panel2_results > div > div > table > tbody > tr > td:nth-child(2) > a"
        } else {
          selector <- paste0(
            "#panel2_results > div > div > table > tbody > tr:nth-child(",
            i,
            ") > td:nth-child(2) > a"
          )
        }

        record_temp <- selenider::s(selector)
        click_ok <- try(record_temp |> selenider::elem_click(), silent = TRUE)
        if (inherits(click_ok, "try-error")) {
          message(sprintf("  [%s p.%d] %s: could not click row %d/%d",
                          toupper(dataset), page_counter, sp, i, n_records))
          next
        }

        all_windows <- session$driver$window_handles()
        if (length(all_windows) < 2L) {
          message(sprintf("Could not find details window for %s (%s) row %d (page %d)",
                          sp, toupper(dataset), i, page_counter))
          next
        }

        session$driver$switch_to_window(all_windows[[2]][1])
        Sys.sleep(delay_seconds)

        text <- selenider::s(
          "#main > div:nth-child(3) > div > div > div > table:nth-child(2) > tbody"
        ) |>
          selenider::elem_text()

        matched <- stringr::str_match(text, "Sequenced regions\\s*(.*)\\n")
        sequenced_regions <- matched[, 2]

        seq_line <- stringr::str_match(text, "Sequence \\s*(.*)\\n")[, 2]
        seq_line <- stringr::str_match(seq_line, "\\)\\s*(.*)")[, 2]

        det_country <- stringr::str_match(text, "Sampling area\\s*(.*)")[, 2]
        if (!is.na(det_country)) {
          det_country <- trimws(strsplit(det_country, ",")[[1]][1])
        }

        df_page$marker[i]   <- sequenced_regions
        df_page$sequence[i] <- seq_line
        if (!is.na(det_country) && nzchar(det_country)) {
          df_page$country[i] <- det_country
        }

        msg_country <- if (is.na(df_page$country[i]) || !nzchar(df_page$country[i])) {
          "Unknown country"
        } else {
          df_page$country[i]
        }
        msg_len <- tryCatch(nchar(df_page$sequence[i]), error = function(e) NA_integer_)

        message(sprintf(
          "  [%s p.%d] %s: record %d/%d – %s (%s, %s bp)",
          toupper(dataset), page_counter, sp, i, n_records,
          ifelse(is.na(sequenced_regions), "no marker", sequenced_regions),
          msg_country,
          ifelse(is.na(msg_len), "?", msg_len)
        ))

        Sys.sleep(delay_seconds)

        session$driver$close_window()
        session$driver$switch_to_window(all_windows[[1]][1])
        Sys.sleep(delay_seconds)
      } # end rows loop

      all_pages[[length(all_pages) + 1L]] <- df_page

      # --- pagination: only if this page is "full" (50 rows) -----------------
      if (n_records < 50L || page_counter >= max_pages) {
        break
      }

      advanced <- FALSE

      # Next numbered page in this block
      next_num_css <- glue::glue(
        "#panel2_results > div > div > div > div:nth-child(2) > div > ul > li:nth-child({page_counter + 1}) > a"
      )
      next_num_el <- try(selenider::s(next_num_css), silent = TRUE)
      if (!inherits(next_num_el, "try-error")) {
        click_res <- try(next_num_el |> selenider::elem_click(), silent = TRUE)
        if (!inherits(click_res, "try-error")) {
          Sys.sleep(delay_seconds)
          page_counter <- page_counter + 1L
          advanced <- TRUE
        }
      }

      # If that didn't work, try the "arrow" (next block of 10 pages)
      if (!advanced) {
        arrow_css <- "#panel2_results > div > div > div > div:nth-child(2) > div > ul > li.arrow > a"
        arrow_el <- try(selenider::s(arrow_css), silent = TRUE)
        if (!inherits(arrow_el, "try-error")) {
          click_res <- try(arrow_el |> selenider::elem_click(), silent = TRUE)
          if (!inherits(click_res, "try-error")) {
            Sys.sleep(delay_seconds)
            page_counter <- page_counter + 1L
            advanced <- TRUE
          }
        }
      }

      if (!advanced) {
        # We expected another page (n_records == 50) but couldn't move.
        break
      }
    } # repeat over pages

    if (!length(all_pages)) {
      return(tibble::tibble())
    }

    dplyr::bind_rows(all_pages)
  }



  # scrape a single species for one dataset
  scrape_one_dataset <- function(sp, dataset = c("unite", "insd")) {
    dataset <- match.arg(dataset)
    seen_labels <- character(0)
    all_variants <- list()

    for (attempt in seq_len(max_variants)) {
      lab <- select_variant(sp, dataset, attempt, delay_seconds)
      if (!nzchar(lab) || is.na(lab)) {
        message(sprintf("No selection label for %s (%s, attempt %d).",
                        sp, toupper(dataset), attempt))
        if (attempt == 1L) break else next
      }
      if (lab %in% seen_labels) {
        # wrapped around
        break
      }
      seen_labels <- c(seen_labels, lab)
      message(sprintf("  [%s] %s – variant %d: '%s'",
                      toupper(dataset), sp, attempt, lab))

      # GO
      go_button <- selenider::s(
        "#seq_search_full > div > div > div:nth-child(4) > div:nth-child(5) > input"
      )
      go_button |> selenider::elem_click()
      Sys.sleep(delay_seconds)

      # no records?
      panel_txt <- try(
        selenider::s("#panel2_results") |> selenider::elem_text(),
        silent = TRUE
      )
      if (!inherits(panel_txt, "try-error") &&
          grepl("No records found", panel_txt, ignore.case = TRUE)) {
        message(sprintf("    No records for %s (%s, variant '%s').",
                        sp, toupper(dataset), lab))
        next
      }

      df_var <- paginate_current_results(sp, dataset, delay_seconds, max_pages)
      if (nrow(df_var) > 0L) {
        all_variants[[length(all_variants) + 1L]] <- df_var
      }
    }

    if (!length(all_variants)) return(tibble::tibble())

    dplyr::bind_rows(all_variants) |>
      dplyr::mutate(
        method       = "species",
        database     = toupper(dataset),
        date_scraped = Sys.Date()
      )
  }

  # ---- outer loop over species + both datasets ----------------------------

  all_res <- dplyr::bind_rows(
    lapply(species, function(sp) {
      dplyr::bind_rows(
        scrape_one_dataset(sp, "unite"),
        scrape_one_dataset(sp, "insd")
      )
    })
  )

  tibble::as_tibble(all_res)
}

# For dplyr tidy-eval
#' @importFrom rlang .data
NULL
