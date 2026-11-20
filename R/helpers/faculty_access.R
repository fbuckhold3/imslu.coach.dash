# ==============================================================================
# FACULTY ACCESS (per-coach login)
# R/helpers/faculty_access.R
# ==============================================================================
#
# Looks up a coach's personal access code in the FAC (faculty) database and
# works out:
#   - who they are (fac_name)
#   - which RDM coach label they correspond to (RDM `coach` dropdown labels
#     and `second_rev` text values are last names only, e.g. "Kunnath")
#   - whether they get the leadership view (any coach's residents)
#
# Leadership = FAC `fac_med_ed` checkbox code 3 (APD, IM Program) or
# code 8 (Core IM PD). Rosters are maintained in REDCap, not in code.
#
# FAC is re-read at most every FAC_ACCESS_MAX_AGE_SEC seconds, so a code
# added or revoked in REDCap takes effect within a few minutes without a
# redeploy. If FAC can't be reached, the last good copy is reused.
#
# Codes are never logged or returned to the UI.
# ==============================================================================

FAC_ACCESS_MAX_AGE_SEC <- 300

FAC_LEADER_CODES <- c("3", "8")  # fac_med_ed: 3 = APD (IM), 8 = Core IM PD

.fac_access_cache <- new.env(parent = emptyenv())

#' Load Active Faculty Access Records
#'
#' @param redcap_url REDCap API URL
#' @param fac_token FAC database token
#' @param force Re-read even if the cached copy is fresh
#' @return Data frame (record_id, fac_name, fac_l_name, fac_access, is_leader)
#'   for non-archived faculty, or NULL if FAC has never been reachable.
load_faculty_access <- function(redcap_url = REDCAP_CONFIG$url,
                                fac_token  = REDCAP_CONFIG$fac_token,
                                force      = FALSE) {

  cached <- .fac_access_cache$data
  fresh <- !is.null(cached) &&
    as.numeric(difftime(Sys.time(), .fac_access_cache$time, units = "secs")) < FAC_ACCESS_MAX_AGE_SEC
  if (!force && fresh) return(cached)

  if (!nzchar(fac_token)) {
    message("[FAC] FAC_TOKEN not set; per-coach login unavailable")
    return(cached)
  }

  result <- tryCatch(
    REDCapR::redcap_read(
      redcap_uri   = redcap_url,
      token        = fac_token,
      fields       = c("record_id", "fac_name", "fac_l_name", "fac_access",
                       "archived", "fac_med_ed"),
      raw_or_label = "raw",
      # Read everything as text so codes keep any leading zeros
      col_types    = readr::cols(.default = readr::col_character()),
      verbose      = FALSE
    ),
    error = function(e) {
      message("[FAC] Read failed: ", e$message)
      NULL
    }
  )

  if (is.null(result) || !isTRUE(result$success)) {
    message("[FAC] Using ", if (is.null(cached)) "no" else "cached", " faculty access data")
    return(cached)
  }

  d <- result$data
  if ("redcap_repeat_instrument" %in% names(d)) {
    d <- d[is.na(d$redcap_repeat_instrument), , drop = FALSE]
  }

  # Only archived == "1" hides someone; blank counts as active
  # (same rule as gmed::load_faculty_roster)
  d <- d[is.na(d$archived) | d$archived != "1", , drop = FALSE]

  flag <- function(code) {
    col <- paste0("fac_med_ed___", code)
    if (col %in% names(d)) !is.na(d[[col]]) & d[[col]] == "1" else rep(FALSE, nrow(d))
  }
  is_leader <- Reduce(`|`, lapply(FAC_LEADER_CODES, flag), rep(FALSE, nrow(d)))

  out <- data.frame(
    record_id  = d$record_id,
    fac_name   = trimws(d$fac_name),
    fac_l_name = trimws(d$fac_l_name),
    fac_access = trimws(d$fac_access),
    is_leader  = is_leader,
    stringsAsFactors = FALSE
  )

  .fac_access_cache$data <- out
  .fac_access_cache$time <- Sys.time()
  message("[FAC] Loaded ", nrow(out), " active faculty (", sum(out$is_leader), " leadership)")
  out
}

#' Last-name keys for a faculty row
#'
#' Uses both fac_l_name and the last word of fac_name, because fac_l_name
#' is sometimes misspelled (fac_name is the display name people maintain).
.fac_name_keys <- function(fac_name, fac_l_name) {
  keys <- c(fac_l_name, sub("^.*\\s", "", fac_name))
  keys <- tolower(trimws(keys))
  unique(keys[!is.na(keys) & nzchar(keys)])
}

#' Match an Access Code to a Coach
#'
#' @param code Code typed at login
#' @param fac Output of load_faculty_access()
#' @param coach_labels Character vector of RDM coach labels in use
#'   (translated `coach` values and `second_rev` values)
#' @return list(status, record_id, fac_name, coach_name, is_leader).
#'   status is "ok", "invalid" (no match) or "duplicate" (code shared by
#'   more than one active faculty record, refused). coach_name is NA when
#'   the person has no single matching RDM coach label.
match_access_code <- function(code, fac, coach_labels) {
  code <- trimws(as.character(code))
  if (is.null(fac) || length(code) != 1 || is.na(code) || !nzchar(code)) {
    return(list(status = "invalid"))
  }

  hit <- fac[!is.na(fac$fac_access) & fac$fac_access == code, , drop = FALSE]

  if (nrow(hit) == 0) return(list(status = "invalid"))

  if (nrow(hit) > 1) {
    message("[FAC] Access code shared by ", nrow(hit),
            " active faculty records (", paste(hit$record_id, collapse = ", "),
            "); login refused")
    return(list(status = "duplicate"))
  }

  coach_labels <- unique(trimws(as.character(coach_labels)))
  coach_labels <- coach_labels[!is.na(coach_labels) & nzchar(coach_labels)]

  keys  <- .fac_name_keys(hit$fac_name, hit$fac_l_name)
  label <- coach_labels[tolower(coach_labels) %in% keys]

  coach_name <- NA_character_
  if (length(label) == 1) {
    # The label must point to exactly one active faculty member, otherwise
    # two people sharing a last name would see each other's residents.
    owners <- vapply(seq_len(nrow(fac)), function(i) {
      tolower(label) %in% .fac_name_keys(fac$fac_name[i], fac$fac_l_name[i])
    }, logical(1))
    if (sum(owners) == 1) {
      coach_name <- label
    } else {
      message("[FAC] Coach label '", label, "' matches ", sum(owners),
              " active faculty; not linking ", hit$fac_name)
    }
  } else if (length(label) > 1) {
    message("[FAC] ", hit$fac_name, " matches several coach labels: ",
            paste(label, collapse = ", "))
  }

  list(
    status     = "ok",
    record_id  = hit$record_id,
    fac_name   = hit$fac_name,
    coach_name = coach_name,
    is_leader  = isTRUE(hit$is_leader)
  )
}
