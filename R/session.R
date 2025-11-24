# R/session.R

.unite_env <- new.env(parent = emptyenv())

#' Start Selenium/Chrome (VISIBLE, global session like your script)
#' @export
unite_start <- function() {
  cli::cli_inform("Starting selenider session (Selenium, visible Chrome)...")

  # IMPORTANT: make it global and persistent
  sess <- selenider::selenider_session(
    "selenium",
    browser = "chrome",
    .env    = .GlobalEnv,  # register in global env so open_url()/s() see it
    local   = FALSE        # don't auto-close at function exit
  )

  assign(".unite_session", sess, envir = .unite_env)  # optional: keep a handle
  invisible(sess)
}

#' Stop the active session
#' @export
unite_stop <- function() {
  if (exists(".unite_session", envir = .unite_env)) {
    sess <- get(".unite_session", envir = .unite_env)
    try(sess$close(), silent = TRUE)   # or selenider::close_session()
    rm(".unite_session", envir = .unite_env)
    cli::cli_alert_success("Selenider session closed.")
  } else {
    # fall back to closing the globally-registered one if present
    try(selenider::close_session(), silent = TRUE)
    cli::cli_alert_info("No session to close.")
  }
}

#' INTERNAL: get the session (if you still need a handle)
#' @keywords internal
.get_session <- function() {
  if (!exists(".unite_session", envir = .unite_env)) {
    rlang::abort("No active session. Call unite_start() first.")
  }
  get(".unite_session", envir = .unite_env)
}

# Reconnect if the Selenium session was killed
# Use the same visible Chrome setup you already have in unite_start()
ensure_session <- function() {
  # no internal handle? start
  if (!exists(".unite_session", envir = .unite_env)) {
    unite_start()
    return(invisible(TRUE))
  }
  # ping the session (about:blank is cheap & harmless)
  ok <- tryCatch({
    selenider::open_url("about:blank")
    TRUE
  }, error = function(e) {
    msg <- conditionMessage(e)
    if (grepl("Invalid session id|session deleted|disconnected", msg, ignore.case = TRUE)) {
      FALSE
    } else {
      # Any other transient error: treat as dead and recreate
      FALSE
    }
  })

  if (!isTRUE(ok)) {
    cli::cli_alert_warning("Browser session went stale. Reconnecting...")
    unite_stop()
    unite_start()
  }
  invisible(TRUE)
}

# Convenience wrapper if you like
open_url_safe <- function(url) {
  ensure_session()
  selenider::open_url(url)
}

