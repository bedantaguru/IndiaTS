
.current_fixed_date <- as.Date("2026-01-01")

extract_month_only <- function(x, warn = TRUE) {

  n <- length(x)
  out <- rep(NA_integer_, n)

  # -------------------------------
  # 1. Month name patterns
  #    Priority over digits
  # -------------------------------

  month_map <- c(
    jan = 1L, january = 1L,
    feb = 2L, february = 2L,
    mar = 3L, march = 3L,
    apr = 4L, april = 4L,
    may = 5L,
    jun = 6L, june = 6L,
    jul = 7L, july = 7L,
    aug = 8L, august = 8L,
    sep = 9L, sept = 9L, september = 9L,
    oct = 10L, october = 10L,
    nov = 11L, november = 11L,
    dec = 12L, december = 12L
  )

  pat_month_text <- stringr::regex(
    paste0("\\b(", paste(names(month_map), collapse = "|"), ")\\b"),
    ignore_case = TRUE
  )

  m_text <- stringr::str_match(x, pat_month_text)
  has_text <- !is.na(m_text[, 1])

  out[has_text] <- unname(month_map[tolower(m_text[has_text, 2])])

  # -------------------------------
  # 2. Numeric month detection
  #    Only if NO text month found
  # -------------------------------

  needs_digit <- is.na(out)

  if (any(needs_digit)) {

    digit_matches <- stringr::str_extract_all(
      x[needs_digit],
      "\\b(0[1-9]|1[0-2])\\b"
    )

    lens <- vapply(digit_matches, length, integer(1))

    ambig  <- lens > 1
    single <- lens == 1

    # warn on ambiguity
    if (any(ambig) && warn) {
      warning(
        "Ambiguous numeric month detected (set to NA) at positions: ",
        paste(which(needs_digit)[ambig], collapse = ", ")
      )
    }

    # assign only unambiguous single matches
    out[needs_digit][single] <- as.integer(unlist(digit_matches[single]))
  }

  out
}

extract_month <- function(x, warn = TRUE, fy_over_two_century = TRUE) {

  n <- length(x)

  # --------------------------------------------------
  # Step 1: extract components
  # --------------------------------------------------

  mnts  <- extract_month_only(x, warn = warn)               # integer month (1–12)
  yrs_4 <- stringr::str_extract(x, "\\b\\d{4}\\b")          # YYYY
  yrs_2 <- stringr::str_extract(x, "\\b\\d{2}\\b")          # YY
  fys   <- extract_fy(x, fy_over_two_century = fy_over_two_century)  # "YYYY-YY"

  # initialise output year
  yr <- rep(NA_integer_, n)

  # current year context (for 2-digit resolution)
  current_year <- as.integer(format(.current_fixed_date, "%Y"))
  current_cc   <- current_year %/% 100
  current_yy   <- current_year %% 100

  # --------------------------------------------------
  # Step 2: determine year ONLY where month exists
  # --------------------------------------------------

  has_month <- !is.na(mnts)

  # --------------------------------------------------
  # Case A: FY available (authoritative)
  # --------------------------------------------------

  has_fy <- has_month & !is.na(fys)

  if (any(has_fy)) {

    fy_start <- as.integer(substr(fys[has_fy], 1, 4))
    fy_end   <- fy_start + 1

    # Jan–Mar → end year, else → start year
    yr[has_fy] <- ifelse(mnts[has_fy] <= 3, fy_end, fy_start)
  }

  # --------------------------------------------------
  # Case B: No FY, but 4-digit year available
  # --------------------------------------------------

  case_b <- has_month & is.na(yr) & !is.na(yrs_4)

  if (any(case_b)) {
    yr[case_b] <- as.integer(yrs_4[case_b])
  }

  # --------------------------------------------------
  # Case C: No FY, no YYYY, but YY available
  #         (resolve via century logic)
  # --------------------------------------------------

  case_c <- has_month & is.na(yr) & is.na(yrs_4) & !is.na(yrs_2)

  if (any(case_c)) {

    yy <- as.integer(yrs_2[case_c])

    cc <- if (fy_over_two_century) {
      ifelse(
        yy <= current_yy + 18,
        current_cc,
        current_cc - 1
      )
    } else {
      rep(current_cc, length(yy))
    }

    yr[case_c] <- cc * 100 + yy
  }

  # --------------------------------------------------
  # Step 3: return result
  # --------------------------------------------------

  # return Like Jan:YYYY format only if both month and year are non NA (for those cases)
  out <- rep(NA_character_, n)
  valid <- !is.na(mnts) & !is.na(yr)
  out[valid] <- paste0(
    format(as.Date(sprintf("2000-%02d-01", mnts[valid])), "%b"),":",
    yr[valid]
  )
  out

}

# fiscal and calendar month are same as per convention, so we can reuse the same functions for both
fiscal_month_for_date <- function(date, with_year = TRUE){

  if(with_year){
    format(date,"%b:%Y")
  }else{
    format(date,"%b")
  }

}

fiscal_month_for_txt <- function(txt, with_year = TRUE) {
  if(with_year){
    extract_month(txt)
  } else {
    extract_month_only(txt)
  }
}


extract_fy_halfyear_only <- function(x) {
  # regex patterns (case-insensitive)

  # H1: April-September
  pat_h1 <- stringr::regex(
    paste0(
      "\\b(",
      "h1|1h|half\\s*1|first\\s*half|",
      "apr(?:il)?\\s*(?:-|–|to|_|\\s)\\s*sep(?:t(?:ember)?)?",
      ")\\b"
    ),
    ignore_case = TRUE
  )

  # H2: October-March
  pat_h2 <- stringr::regex(
    paste0(
      "\\b(",
      "h2|2h|half\\s*2|second\\s*half|",
      "oct(?:ober)?\\s*(?:-|–|to|_|\\s)\\s*mar(?:ch)?",
      ")\\b"
    ),
    ignore_case = TRUE
  )

  # extract first matching half-year
  out <- rep(NA_character_, length(x))
  out[stringr::str_detect(x, pat_h1) & is.na(out)] <- "h1"
  out[stringr::str_detect(x, pat_h2) & is.na(out)] <- "h2"

  out
}


extract_calendar_quarter_only <- function(x) {

  # regex patterns (case-insensitive)
  # Calendar quarters: Q1=Jan-Mar, Q2=Apr-Jun, Q3=Jul-Sep, Q4=Oct-Dec

  pat_q1 <- stringr::regex(
    "\\b(q1|jan(?:uary)?\\s*(?:-|–|to|_|\\s)\\s*mar(?:ch)?)\\b",
    ignore_case = TRUE
  )
  pat_q2 <- stringr::regex(
    "\\b(q2|apr(?:il)?\\s*(?:-|–|to|_|\\s)\\s*jun(?:e)?)\\b",
    ignore_case = TRUE
  )
  pat_q3 <- stringr::regex(
    "\\b(q3|jul(?:y)?\\s*(?:-|–|to|_|\\s)\\s*sep(?:t(?:ember)?)?)\\b",
    ignore_case = TRUE
  )
  pat_q4 <- stringr::regex(
    "\\b(q4|oct(?:ober)?\\s*(?:-|–|to|_|\\s)\\s*dec(?:ember)?)\\b",
    ignore_case = TRUE
  )

  # extract first matching quarter
  out <- rep(NA_character_, length(x))

  out[stringr::str_detect(x, pat_q1) & is.na(out)] <- "q1"
  out[stringr::str_detect(x, pat_q2) & is.na(out)] <- "q2"
  out[stringr::str_detect(x, pat_q3) & is.na(out)] <- "q3"
  out[stringr::str_detect(x, pat_q4) & is.na(out)] <- "q4"

  out
}

extract_fy_quarter_only <- function(x) {

  # regex patterns (case-insensitive)
  pat_q1 <- stringr::regex(
    "\\b(q1|apr(?:il)?\\s*(?:-|–|to|_|\\s)\\s*jun(?:e)?)\\b",
    ignore_case = TRUE
  )
  pat_q2 <- stringr::regex(
    "\\b(q2|jul(?:y)?\\s*(?:-|–|to|_|\\s)\\s*sep(?:t(?:ember)?)?)\\b",
    ignore_case = TRUE
  )
  pat_q3 <- stringr::regex(
    "\\b(q3|oct(?:ober)?\\s*(?:-|–|to|_|\\s)\\s*dec(?:ember)?)\\b",
    ignore_case = TRUE
  )
  pat_q4 <- stringr::regex(
    "\\b(q4|jan(?:uary)?\\s*(?:-|–|to|_|\\s)\\s*mar(?:ch)?)\\b",
    ignore_case = TRUE
  )

  # extract first matching quarter
  out <- rep(NA_character_, length(x))

  out[stringr::str_detect(x, pat_q1) & is.na(out)] <- "q1"
  out[stringr::str_detect(x, pat_q2) & is.na(out)] <- "q2"
  out[stringr::str_detect(x, pat_q3) & is.na(out)] <- "q3"
  out[stringr::str_detect(x, pat_q4) & is.na(out)] <- "q4"

  out
}

extract_fy <- function(x, fy_over_two_century = TRUE) {
  n <- length(x)
  out <- rep(NA_character_, n)

  # --------------------------------------------------
  # 1. Explicit range: YYYY-YY or YYYY–YY
  #    Valid only if YY == (YYYY + 1) %% 100
  # --------------------------------------------------
  m_range <- stringr::str_match(
    x,
    stringr::regex("(\\d{4})\\s*(?:-|–)\\s*(\\d{2})", ignore_case = TRUE)
  )
  has_range <- !is.na(m_range[, 1])
  if (any(has_range)) {
    y1 <- as.integer(m_range[has_range, 2])   # YYYY
    y2 <- as.integer(m_range[has_range, 3])   # YY
    ok <- y2 == (y1 + 1) %% 100
    idx <- which(has_range)[ok]
    out[idx] <- paste0(
      y1[ok],
      "-",
      sprintf("%02d", y2[ok])
    )
  }

  # --------------------------------------------------
  # 2. Two-digit year range: XX-XX or XX–XX
  #    Valid only if second year == (first year + 1) %% 100
  # --------------------------------------------------
  m_2digit <- stringr::str_match(
    x,
    stringr::regex("^\\s*(\\d{2})\\s*(?:-|–)\\s*(\\d{2})\\s*$", ignore_case = TRUE)
  )
  has_2digit <- is.na(out) & !is.na(m_2digit[, 1])
  if (any(has_2digit)) {
    yy1 <- as.integer(m_2digit[has_2digit, 2])  # First YY
    yy2 <- as.integer(m_2digit[has_2digit, 3])  # Second YY

    # Validate that it's consecutive years
    ok <- yy2 == (yy1 + 1) %% 100

    if (any(ok)) {
      current_year <- as.integer(format(.current_fixed_date, "%Y"))
      current_cc   <- current_year %/% 100
      current_yy   <- current_year %% 100

      yy1_valid <- yy1[ok]

      # Century inference using same logic as FYxx
      cc <- if (fy_over_two_century) {
        # If yy1 is <= current_yy + 18, assume current century, else previous
        ifelse(yy1_valid <= current_yy + 18, current_cc, current_cc - 1)
      } else {
        rep(current_cc, length(yy1_valid))
      }

      start_year <- cc * 100 + yy1_valid

      idx <- which(has_2digit)[ok]
      out[idx] <- paste0(
        start_year,
        "-",
        sprintf("%02d", (start_year + 1) %% 100)
      )
    }
  }

  # --------------------------------------------------
  # 3. FY shorthand: FYxx or FYxxxx
  # --------------------------------------------------
  m_fy <- stringr::str_match(
    x,
    stringr::regex(
      "fy[^a-zA-Z0-9]*(\\d{4}|\\d{2})",
      ignore_case = TRUE
    )
  )
  has_fy <- is.na(out) & !is.na(m_fy[, 1])
  if (any(has_fy)) {
    yr_str <- m_fy[has_fy, 2]
    yr     <- as.integer(yr_str)
    is_4d  <- nchar(yr_str) == 4
    current_year <- as.integer(format(.current_fixed_date, "%Y"))
    current_cc   <- current_year %/% 100
    current_yy   <- current_year %% 100
    start_year <- integer(sum(has_fy))
    # --------------------------------------------
    # FYXXXX handling (no century adjustment)
    # FY2026 -> 2025-26
    # --------------------------------------------
    if (any(is_4d)) {
      start_year[is_4d] <- yr[is_4d] - 1
    }
    # --------------------------------------------
    # FYxx handling (century inference)
    # --------------------------------------------
    if (any(!is_4d)) {
      yy <- yr[!is_4d]
      cc <- if (fy_over_two_century) {
        ifelse(yy <= current_yy + 18, current_cc, current_cc - 1)
      } else {
        rep(current_cc, length(yy))
      }
      start_year[!is_4d] <- cc * 100 + yy - 1
    }
    idx <- which(has_fy)
    out[idx] <- paste0(
      start_year,
      "-",
      sprintf("%02d", (start_year + 1) %% 100)
    )
  }

  out
}

extract_calendar_year <- function(x, fy_over_two_century = TRUE) {

  n <- length(x)
  out <- rep(NA_integer_, n)

  # --------------------------------------------------
  # Extract both 4-digit and 2-digit years
  # --------------------------------------------------

  yrs_4_all <- stringr::str_extract_all(x, "\\b\\d{4}\\b")
  lens_4 <- vapply(yrs_4_all, length, integer(1))

  yrs_2_all <- stringr::str_extract_all(x, "\\b\\d{2}\\b")
  lens_2 <- vapply(yrs_2_all, length, integer(1))

  # --------------------------------------------------
  # 1. Use YYYY only if:
  #    - Exactly ONE 4-digit year found
  #    - AND no 2-digit years present
  # --------------------------------------------------

  has_single_4_only <- (lens_4 == 1) & (lens_2 == 0)

  if (any(has_single_4_only)) {
    out[has_single_4_only] <- as.integer(unlist(yrs_4_all[has_single_4_only]))
  }

  # --------------------------------------------------
  # 2. Use YY only if:
  #    - Exactly ONE 2-digit year found
  #    - AND no 4-digit years present
  # --------------------------------------------------

  has_single_2_only <- (lens_2 == 1) & (lens_4 == 0)

  if (any(has_single_2_only)) {

    yy <- as.integer(unlist(yrs_2_all[has_single_2_only]))

    # Century inference logic
    current_year <- as.integer(format(.current_fixed_date, "%Y"))
    current_cc   <- current_year %/% 100
    current_yy   <- current_year %% 100

    cc <- if (fy_over_two_century) {
      ifelse(yy <= current_yy + 18, current_cc, current_cc - 1)
    } else {
      rep(current_cc, length(yy))
    }

    out[has_single_2_only] <- cc * 100 + yy
  }

  # All other cases (multiple years, mixed 4d+2d, etc.) remain NA

  out
}

extract_calendar_quarter <- function(x) {
  # extract components
  q  <- extract_calendar_quarter_only(x)
  yr <- extract_calendar_year(x)

  # initialise output
  out <- rep(NA_character_, length(x))

  # only when BOTH quarter and year are present
  both <- !is.na(q) & !is.na(yr)
  out[both] <- paste0(toupper(q[both]), ":", yr[both])

  out
}


extract_fy_quarter <- function(x) {

  # extract components
  q  <- extract_fy_quarter_only(x)
  fy <- extract_fy(x)

  # initialise output
  out <- rep(NA_character_, length(x))

  # only when BOTH quarter and FY are present
  both <- !is.na(q) & !is.na(fy)
  out[both] <- paste0(toupper(q[both]), ":", fy[both])

  out
}

extract_fy_halfyear <- function(x){
  # extract components
  hy <- extract_fy_halfyear_only(x)
  fy <- extract_fy(x)

  # initialise output
  out <- rep(NA_character_, length(x))

  # only when BOTH half-year and FY are present
  both <- !is.na(hy) & !is.na(fy)
  out[both] <- paste0(toupper(hy[both]), ":", fy[both])

  out
}

fiscal_halfyear_for_date <- function(date, with_year = TRUE) {

  mon <- lubridate::month(date)

  hy <- dplyr::case_when(
    mon %in% 4:9   ~ "H1",
    mon %in% c(10,11,12,1,2,3) ~ "H2"
  )

  if (with_year) {
    res <- paste0(hy, ":", fiscal_year(date))
  } else {
    res <- hy
  }

  res <- ifelse(is.na(date), NA_character_, res)
  res
}

fiscal_halfyear_for_txt <- function(txt, with_year = TRUE) {
  if(with_year){
    extract_fy_halfyear(txt)
  } else {
    extract_fy_halfyear_only(txt)
  }
}

fiscal_halfyear_to_date <- function(x, anchor = c("mid", "first", "last")) {
  anchor <- match.arg(anchor)
  out <- rep(as.Date(NA), length(x))

  # strict pattern: H1/H2:YYYY-YY
  pat <- "^(H[12]):(\\d{4}-\\d{2})$"
  m <- stringr::str_match(x, pat)
  valid <- !is.na(m[, 1])

  if (!any(valid)) return(out)

  hy   <- m[valid, 2]
  fy   <- m[valid, 3]
  idx <- which(valid)

  # extract base year from FY
  base_year <- as.integer(substr(fy, 1, 4))

  # compute start and end dates for each half-year
  for (i in seq_along(idx)) {
    if (hy[i] == "H1") {
      # H1: April to September of base year
      start_date <- as.Date(sprintf("%04d-04-01", base_year[i]))
      end_date   <- as.Date(sprintf("%04d-09-30", base_year[i]))
    } else {
      # H2: October of base year to March of next year
      start_date <- as.Date(sprintf("%04d-10-01", base_year[i]))
      end_date   <- as.Date(sprintf("%04d-03-31", base_year[i] + 1L))
    }

    out[idx[i]] <- switch(
      anchor,
      first = start_date,
      last  = end_date,
      mid   = start_date + as.integer((end_date - start_date) / 2)
    )
  }

  out
}

calendar_quarter_to_date <- function(x, anchor = c("mid", "first", "last")) {

  anchor <- match.arg(anchor)

  out <- rep(as.Date(NA), length(x))

  # strict pattern: Q1:YYYY
  pat <- "^Q([1-4]):(\\d{4})$"

  m <- stringr::str_match(x, pat)
  valid <- !is.na(m[, 1])
  if (!any(valid)) return(out)

  q    <- as.integer(m[valid, 2])
  year <- as.integer(m[valid, 3])

  idx <- which(valid)

  # start of calendar quarter
  start_month <- c(1L, 4L, 7L, 10L)[q]
  start_date  <- as.Date(sprintf("%04d-%02d-01", year, start_month))

  # compute end of quarter
  next_month <- c(4L, 7L, 10L, 1L)[q]
  next_year  <- ifelse(q == 4L, year + 1L, year)

  end_date <- as.Date(sprintf("%04d-%02d-01", next_year, next_month)) - 1

  out[idx] <- switch(
    anchor,
    first = start_date,
    last  = end_date,
    mid   = start_date + as.integer((end_date - start_date) / 2)
  )

  out
}

fiscal_quarter_to_date <- function(x, anchor = c("mid", "first", "last")) {

  anchor <- match.arg(anchor)
  out <- rep(as.Date(NA), length(x))

  pat <- "^Q([1-4]):(\\d{4})-(\\d{2})$"
  m <- stringr::str_match(x, pat)

  valid <- !is.na(m[, 1])
  if (!any(valid)) return(out)

  q        <- as.integer(m[valid, 2])
  fy_start <- as.integer(m[valid, 3])
  fy_end   <- as.integer(m[valid, 4])

  ok  <- fy_end == (fy_start + 1) %% 100
  idx <- which(valid)[ok]
  if (!length(idx)) return(out)

  q        <- q[ok]
  fy_start <- fy_start[ok]

  ## ---- START DATE ----
  start_month <- c(4L, 7L, 10L, 1L)[q]
  start_year  <- ifelse(q == 4L, fy_start + 1L, fy_start)

  start_date <- as.Date(sprintf(
    "%04d-%02d-01",
    start_year, start_month
  ))

  ## ---- END DATE (FIXED) ----
  next_month <- c(7L, 10L, 1L, 4L)[q]
  next_year  <- ifelse(q %in% c(1L, 2L),
                       fy_start,
                       fy_start + 1L)

  end_date <- as.Date(sprintf(
    "%04d-%02d-01",
    next_year, next_month
  )) - 1L

  out[idx] <- switch(
    anchor,
    first = start_date,
    last  = end_date,
    mid   = start_date + as.integer((end_date - start_date) / 2)
  )

  out
}

fiscal_month_to_date <- function(x, anchor = c("mid", "first", "last")) {

  anchor <- match.arg(anchor)

  out <- rep(as.Date(NA), length(x))

  # strict pattern: Mon:YYYY
  # month must be valid 3-letter English abbreviation
  pat <- "^(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec):(\\d{4})$"

  m <- stringr::str_match(x, pat)
  valid <- !is.na(m[, 1])
  if (!any(valid)) return(out)

  mon_txt <- m[valid, 2]
  year    <- as.integer(m[valid, 3])

  # map month abbreviations to month number
  mon_num <- match(mon_txt,
                   c("Jan","Feb","Mar","Apr","May","Jun",
                     "Jul","Aug","Sep","Oct","Nov","Dec"))

  ok <- !is.na(mon_num)
  idx <- which(valid)[ok]
  if (!length(idx)) return(out)

  mon_num <- mon_num[ok]
  year    <- year[ok]

  # first day of month
  start_date <- as.Date(sprintf("%04d-%02d-01", year, mon_num))

  # last day of month (vector-safe)
  next_mon  <- ifelse(mon_num == 12L, 1L, mon_num + 1L)
  next_year <- ifelse(mon_num == 12L, year + 1L, year)

  end_date <- as.Date(sprintf("%04d-%02d-01", next_year, next_mon)) - 1

  out[idx] <- switch(
    anchor,
    first = start_date,
    last  = end_date,
    mid   = start_date + as.integer((end_date - start_date) / 2)
  )

  out
}

fiscal_year_to_date <- function(x, anchor = c("mid", "first", "last")) {

  anchor <- match.arg(anchor)

  out <- rep(as.Date(NA), length(x))

  # strict pattern: YYYY-YY (any century)
  pat <- "^(\\d{4})-(\\d{2})$"

  m <- stringr::str_match(x, pat)
  valid <- !is.na(m[, 1])
  if (!any(valid)) return(out)

  fy_start <- as.integer(m[valid, 2])
  fy_end   <- as.integer(m[valid, 3])

  # validate FY logic: YY == (YYYY + 1) %% 100
  ok  <- fy_end == (fy_start + 1) %% 100
  idx <- which(valid)[ok]
  if (!length(idx)) return(out)

  fy_start <- fy_start[ok]

  # fiscal year runs from Apr 1 to Mar 31
  start_date <- as.Date(sprintf("%04d-04-01", fy_start))
  end_date   <- as.Date(sprintf("%04d-03-31", fy_start + 1L))

  out[idx] <- switch(
    anchor,
    first = start_date,
    last  = end_date,
    mid   = start_date + as.integer((end_date - start_date) / 2)
  )

  out
}


fiscal_quarter_for_txt <- function(txt, with_year = TRUE){
  if(with_year){
    extract_fy_quarter(txt)
  } else {
    extract_fy_quarter_only(txt)
  }
}

fiscal_quarter_for_date <- function(date, with_year = TRUE) {

  mon <- lubridate::month(date)

  qtr <- dplyr::case_when(
    mon %in% 4:6   ~ "Q1",
    mon %in% 7:9   ~ "Q2",
    mon %in% 10:12 ~ "Q3",
    mon %in% 1:3   ~ "Q4"
  )

  if (with_year) {
    res <- paste0(qtr, ":", fiscal_year(date))
  } else {
    res <- qtr
  }

  res <- ifelse(is.na(date), NA_character_, res)
  res
}


calendar_quarter_for_txt <- function(txt, with_year = TRUE) {
  if (with_year) {
    extract_calendar_quarter(txt)
  } else {
    extract_calendar_quarter_only(txt)
  }
}

calendar_quarter_for_date <- function(date, with_year = TRUE) {

  qtr <- paste0("Q", lubridate::quarter(date))

  if (with_year) {
    paste0(qtr, ":", lubridate::year(date))
  } else {
    qtr
  }
}



fiscal_year_for_date <- function(date) {
  yr  <- lubridate::year(date)
  mon <- lubridate::month(date)

  start_yr <- ifelse(mon <= 3, yr - 1, yr)
  end_yr   <- ifelse(mon <= 3, yr,     yr + 1)

  paste0(start_yr, "-", substr(end_yr, 3, 4))
}



previous_period_for_fiscal_period <- function(
    fp,
    lag_len = c(
      "month" = 30,
      "quarter" = 90,
      "halfyear" = 182,
      "year" = 365
    )
) {

  fp_date <- as.Date.fiscal_period(fp, anchor = "mid")

  fqs <- frequency.fiscal_period(fp)

  days_to_subtract <- dplyr::case_when(
    fqs == "month" ~ as.numeric(lag_len["month"]),
    fqs == "quarter" ~ as.numeric(lag_len["quarter"]),
    fqs == "halfyear" ~ as.numeric(lag_len["halfyear"]),
    fqs == "year" ~ as.numeric(lag_len["year"]),
    TRUE ~ NA_real_
  )

  this_dates <- fp_date - days_to_subtract

  out <- dplyr::case_when(
    fqs == "month" ~ fiscal_month_for_date(this_dates, with_year = TRUE),
    fqs == "quarter" ~ fiscal_quarter_for_date(this_dates, with_year = TRUE),
    fqs == "halfyear" ~ fiscal_halfyear_for_date(this_dates, with_year = TRUE),
    fqs == "year" ~ fiscal_year_for_date(this_dates),
    TRUE ~ NA
  )

  class(out) <- class(fp)  # preserve class of input

  out
}

previous_period_for_calendar_period <- function(cp, lag_len = c("month" = 30, "quarter" = 90, "year" = 365)) {

  cp_date <- as.Date.calendar_period(cp, anchor = "mid")

  fqs <- frequency.calendar_period(cp)

  days_to_subtract <- dplyr::case_when(
    fqs == "month" ~ as.numeric(lag_len["month"]),
    fqs == "quarter" ~ as.numeric(lag_len["quarter"]),
    fqs == "year" ~ as.numeric(lag_len["year"]),
    TRUE ~ NA_real_
  )

  this_dates <- cp_date - days_to_subtract

  out <- dplyr::case_when(
    fqs == "month" ~ fiscal_month_for_date(this_dates, with_year = TRUE),
    fqs == "quarter" ~ calendar_quarter_for_date(this_dates, with_year = TRUE),
    fqs == "year" ~ as.character(lubridate::year(this_dates)),
    TRUE ~ NA
  )

  class(out) <- class(cp)  # preserve class of input

  out

}





as_fiscal_period_for_txt <- function(x, with_year = TRUE) {

  n <- length(x)
  out <- rep(NA_character_, n)

  # 1. Month (finest granularity)
  month_result <- fiscal_month_for_txt(x, with_year = with_year)
  out <- ifelse(is.na(out) & !is.na(month_result), month_result, out)

  # 2. Quarter
  still_na <- is.na(out)
  if (any(still_na)) {
    quarter_result <- fiscal_quarter_for_txt(x[still_na], with_year = with_year)
    out[still_na] <- ifelse(!is.na(quarter_result), quarter_result, out[still_na])
  }

  # 3. Half-year
  still_na <- is.na(out)
  if (any(still_na)) {
    halfyear_result <- fiscal_halfyear_for_txt(x[still_na], with_year = with_year)
    out[still_na] <- ifelse(!is.na(halfyear_result), halfyear_result, out[still_na])
  }

  # 4. Year (coarsest)
  still_na <- is.na(out)
  if (any(still_na) && with_year) {
    year_result <- extract_fy(x[still_na])
    out[still_na] <- ifelse(!is.na(year_result), year_result, out[still_na])
  }

  class(out) <- fiscal_period_class
  out
}


as_fiscal_period_for_date <- function(x, with_year = TRUE) {

  fq <- frequency.Date(x)

  if(is.na(fq)){
    stop("Unable to determine frequency of input dates. Please ensure they are regular.", call. = FALSE)
  }

  if(fq == "mixed"){
    stop("Input dates have mixed frequencies. Please ensure they are regular.", call. = FALSE)
  }

  if(!(fq %in% c("month", "quarter", "halfyear", "year"))){
    stop("Unsupported frequency detected: ", fq, ". Supported frequencies are: month, quarter, halfyear, year.", call. = FALSE)
  }

  # Proceed for other cases (month, quarter, halfyear, year)
  if(fq %in% c("month", "quarter", "halfyear", "year")){
    out <- dplyr::case_when(
      fq == "month" ~ fiscal_month_for_date(x, with_year = with_year),
      fq == "quarter" ~ fiscal_quarter_for_date(x, with_year = with_year),
      fq == "halfyear" ~ fiscal_halfyear_for_date(x, with_year = with_year),
      fq == "year" ~ fiscal_year_for_date(x),
      TRUE ~ NA_character_
    )
    return(out)
  }
  return(NULL)
}

