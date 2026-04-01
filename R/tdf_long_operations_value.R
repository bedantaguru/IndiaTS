# =============================================================================
# Utility helpers (numerical safety)
# =============================================================================

safe_divide <- function(num, den) {
  ifelse(is.na(den) | den == 0, NA_real_, num / den)
}

safe_log_ratio <- function(num, den) {
  ratio <- safe_divide(num, den)
  ifelse(!is.na(ratio) & ratio > 0, log(ratio), NA_real_)
}

safe_pct <- function(x) {
  s <- sum(x, na.rm = TRUE)
  ifelse(s == 0, NA_real_, x / s * 100)
}

# =============================================================================
# compute_metrics
# =============================================================================
# Computes derived time-series metrics such as growth, momentum,
# contributions, and shares from value.level.
#
# Only requested metrics are returned. Intermediate values are computed
# internally but not exposed unless explicitly requested.
# =============================================================================

compute_metrics <- function(
    dat,
    level_prev_prd               = FALSE,
    level_prev_year              = FALSE,
    growth_rate                  = FALSE,
    momentum                     = FALSE,
    ln_growth_rate               = FALSE,
    ln_momentum                  = FALSE,
    base_effect                  = FALSE,
    ln_base_effect               = FALSE,
    growth_rate_prev_prd         = FALSE,
    ln_growth_rate_prev_prd      = FALSE,
    delta_growth_rate            = FALSE,
    delta_ln_growth_rate         = FALSE,
    level_parent                 = FALSE,
    share_pct                    = FALSE,
    share_prev_prd               = FALSE,
    share_prev_year              = FALSE,
    contribution_growth_rate     = FALSE,
    contribution_momentum        = FALSE,
    contribution_growth_rate_pct = FALSE,
    contribution_momentum_pct    = FALSE,
    annual_contribution          = FALSE,
    .compute_all                 = FALSE
) {

  # Enable all metrics if global flag is set
  if (.compute_all) {
    level_prev_prd               <- TRUE
    level_prev_year              <- TRUE
    growth_rate                  <- TRUE
    momentum                     <- TRUE
    ln_growth_rate               <- TRUE
    ln_momentum                  <- TRUE
    base_effect                  <- TRUE
    ln_base_effect               <- TRUE
    growth_rate_prev_prd         <- TRUE
    ln_growth_rate_prev_prd      <- TRUE
    delta_growth_rate            <- TRUE
    delta_ln_growth_rate         <- TRUE
    level_parent                 <- TRUE
    share_pct                    <- TRUE
    share_prev_prd               <- TRUE
    share_prev_year              <- TRUE
    contribution_growth_rate     <- TRUE
    contribution_momentum        <- TRUE
    contribution_growth_rate_pct <- TRUE
    contribution_momentum_pct    <- TRUE
    annual_contribution          <- TRUE
  }

  # Identify requested outputs
  requested_cols <- c(
    if (level_prev_prd)               "value.level_prev_prd",
    if (level_prev_year)              "value.level_prev_year",
    if (growth_rate)                  "value.growth_rate",
    if (momentum)                     "value.momentum",
    if (ln_growth_rate)               "value.ln_growth_rate",
    if (ln_momentum)                  "value.ln_momentum",
    if (base_effect)                  "value.base_effect",
    if (ln_base_effect)               "value.ln_base_effect",
    if (growth_rate_prev_prd)         "value.growth_rate_prev_prd",
    if (ln_growth_rate_prev_prd)      "value.ln_growth_rate_prev_prd",
    if (delta_growth_rate)            "value.delta_growth_rate",
    if (delta_ln_growth_rate)         "value.delta_ln_growth_rate",
    if (level_parent)                 "value.level_parent",
    if (share_pct)                    "value.share_pct",
    if (share_prev_prd)               "value.share_prev_prd",
    if (share_prev_year)              "value.share_prev_year",
    if (contribution_growth_rate)     "value.contribution_growth_rate",
    if (contribution_momentum)        "value.contribution_momentum",
    if (contribution_growth_rate_pct) "value.contribution_growth_rate_pct",
    if (contribution_momentum_pct)    "value.contribution_momentum_pct",
    if (annual_contribution)          "value.annual_contribution"
  )

  if (length(requested_cols) == 0) {
    message("compute_metrics: no output requested. Returning data unchanged.")
    return(dat)
  }

  if (!"value.level" %in% colnames(dat)) {
    stop("compute_metrics: value.level column missing.", call. = FALSE)
  }

  # Drop requested columns if they already exist to prevent rename collisions.
  # Also clean up any stray dot-prefixed temporary columns from previous runs.
  dat <- dat %>% select(
    -dplyr::any_of(requested_cols),
    -dplyr::starts_with(".time_"),
    -dplyr::starts_with(".value.")
  )

  id_cols <- c("meta.release_tag", "meta.price_basis", "meta.name", "meta.disaggregation_group")
  join_keys_same <- setNames(id_cols, id_cols)

  has_parent <- all(c("meta.parent", "meta.parent_disaggregation_group") %in% colnames(dat))

  # Drop parent-dependent metrics if parent info is missing
  parent_metrics <- c(
    "value.level_parent",
    "value.share_pct",
    "value.share_prev_prd",
    "value.share_prev_year",
    "value.contribution_growth_rate",
    "value.contribution_momentum",
    "value.contribution_growth_rate_pct",
    "value.contribution_momentum_pct"
  )

  parent_dependent <- intersect(requested_cols, parent_metrics)

  if (length(parent_dependent) > 0 && !has_parent) {
    warning(
      "compute_metrics: meta.parent / meta.parent_disaggregation_group not found. ",
      "The following requested columns will be dropped: ", paste(parent_dependent, collapse = ", "),
      call. = FALSE
    )
    requested_cols <- setdiff(requested_cols, parent_dependent)
    level_parent <- share_pct <- share_prev_prd <- share_prev_year <- FALSE
    contribution_growth_rate <- contribution_momentum <- FALSE
    contribution_growth_rate_pct <- contribution_momentum_pct <- FALSE
  }

  # Determine dependencies
  need_level_prev_prd       <- level_prev_prd || momentum || ln_momentum || contribution_momentum || contribution_momentum_pct || base_effect || ln_base_effect
  need_level_prev_year      <- level_prev_year || growth_rate || ln_growth_rate || contribution_growth_rate || contribution_growth_rate_pct || delta_growth_rate || delta_ln_growth_rate

  need_growth_rate          <- growth_rate || growth_rate_prev_prd || delta_growth_rate || contribution_growth_rate || contribution_growth_rate_pct
  need_ln_growth_rate       <- ln_growth_rate || ln_growth_rate_prev_prd || delta_ln_growth_rate
  need_momentum             <- momentum || contribution_momentum || contribution_momentum_pct
  need_ln_momentum          <- ln_momentum || base_effect || ln_base_effect

  need_share_pct            <- share_pct || share_prev_prd || share_prev_year || contribution_growth_rate || contribution_momentum || contribution_growth_rate_pct || contribution_momentum_pct
  need_share_prev_prd       <- share_prev_prd || contribution_momentum || contribution_momentum_pct
  need_share_prev_year      <- share_prev_year || contribution_growth_rate || contribution_growth_rate_pct

  need_growth_rate_prev_prd <- growth_rate_prev_prd || delta_growth_rate
  need_ln_growth_prev_prd   <- ln_growth_rate_prev_prd || delta_ln_growth_rate

  # ---- Time references ----
  dat <- dat %>%
    mutate(
      .time_prev_period = previous_period(time),
      .time_prev_year   = previous_year(time)
    )

  # ---- Previous values ----
  if (need_level_prev_prd || need_level_prev_year) {
    lookup <- dat %>% select(time, dplyr::all_of(id_cols), value.level)

    if (need_level_prev_prd) {
      dat <- dat %>% left_join(lookup %>% rename(.value.level_prev_prd = value.level), by = c(".time_prev_period" = "time", join_keys_same))
    }
    if (need_level_prev_year) {
      dat <- dat %>% left_join(lookup %>% rename(.value.level_prev_year = value.level), by = c(".time_prev_year" = "time", join_keys_same))
    }
  }

  # ---- Growth and momentum ----
  if (need_growth_rate || need_momentum || need_ln_growth_rate || need_ln_momentum) {
    dat <- dat %>%
      mutate(
        .value.growth_rate    = if (need_growth_rate)    (safe_divide(value.level, .value.level_prev_year) - 1) * 100 else NA_real_,
        .value.momentum       = if (need_momentum)       (safe_divide(value.level, .value.level_prev_prd) - 1) * 100  else NA_real_,
        .value.ln_growth_rate = if (need_ln_growth_rate) safe_log_ratio(value.level, .value.level_prev_year) * 100    else NA_real_,
        .value.ln_momentum    = if (need_ln_momentum)    safe_log_ratio(value.level, .value.level_prev_prd) * 100     else NA_real_
      )
  }

  # ---- Base effects ----
  if (base_effect || ln_base_effect) {
    dat <- dat %>%
      left_join(
        dat %>%
          select(time, dplyr::all_of(id_cols), .value.ln_momentum) %>%
          mutate(
            .value.base_effect    = -.value.ln_momentum,
            .value.ln_base_effect = -.value.ln_momentum
          ) %>%
          select(-.value.ln_momentum),
        by = c(".time_prev_year" = "time", join_keys_same)
      )
  }

  # ---- Delta growth ----
  if (need_growth_rate_prev_prd || need_ln_growth_prev_prd) {
    dat <- dat %>%
      left_join(
        dat %>%
          select(time, dplyr::all_of(id_cols),
                 .value.growth_rate_prev_prd    = if(need_growth_rate_prev_prd) ".value.growth_rate" else NULL,
                 .value.ln_growth_rate_prev_prd = if(need_ln_growth_prev_prd) ".value.ln_growth_rate" else NULL),
        by = c(".time_prev_period" = "time", join_keys_same)
      ) %>%
      mutate(
        .value.delta_growth_rate    = if (delta_growth_rate)    .value.growth_rate - .value.growth_rate_prev_prd else NA_real_,
        .value.delta_ln_growth_rate = if (delta_ln_growth_rate) .value.ln_growth_rate - .value.ln_growth_rate_prev_prd else NA_real_
      )
  }

  # ---- Shares and contributions ----
  if (need_share_pct) {

    dat_parent <- dat %>%
      filter(meta.name == meta.parent,
             meta.disaggregation_group == meta.parent_disaggregation_group) %>%
      select(time, meta.release_tag, meta.price_basis,
             meta.parent, meta.parent_disaggregation_group,
             .value.level_parent = value.level)

    dat <- dat %>%
      left_join(dat_parent,
                by = c("time", "meta.release_tag", "meta.price_basis",
                       "meta.parent", "meta.parent_disaggregation_group")) %>%
      mutate(.value.share_pct = safe_divide(value.level, .value.level_parent) * 100)

    if (need_share_prev_prd) {
      dat <- dat %>% left_join(dat %>% select(time, dplyr::all_of(id_cols), .value.share_prev_prd = .value.share_pct),
                               by = c(".time_prev_period" = "time", join_keys_same))
    }

    if (need_share_prev_year) {
      dat <- dat %>% left_join(dat %>% select(time, dplyr::all_of(id_cols), .value.share_prev_year = .value.share_pct),
                               by = c(".time_prev_year" = "time", join_keys_same))
    }

    if (contribution_growth_rate || contribution_momentum || contribution_growth_rate_pct || contribution_momentum_pct) {
      dat <- dat %>%
        mutate(
          .value.contribution_growth_rate = if (contribution_growth_rate || contribution_growth_rate_pct) .value.share_prev_year * .value.growth_rate / 100 else NA_real_,
          .value.contribution_momentum    = if (contribution_momentum || contribution_momentum_pct) .value.share_prev_prd  * .value.momentum    / 100 else NA_real_
        )
    }

    if (contribution_growth_rate_pct || contribution_momentum_pct) {
      dat <- dat %>%
        group_by(time, meta.release_tag, meta.price_basis,
                 meta.disaggregation_group, meta.parent, meta.parent_disaggregation_group) %>%
        mutate(
          .value.contribution_growth_rate_pct = if (contribution_growth_rate_pct) safe_pct(.value.contribution_growth_rate) else NA_real_,
          .value.contribution_momentum_pct    = if (contribution_momentum_pct) safe_pct(.value.contribution_momentum) else NA_real_
        ) %>%
        ungroup()
    }
  }

  # ---- Annual contribution ----
  if (annual_contribution) {
    dat <- dat %>%
      mutate(.time_year = fiscal_year(time)) %>%
      group_by(.time_year, meta.release_tag, meta.price_basis, meta.name, meta.disaggregation_group) %>%
      mutate(.value.annual_contribution = safe_divide(value.level, sum(value.level, na.rm = TRUE)) * 100) %>%
      ungroup() %>%
      select(-.time_year)
  }

  # ---- Rename outputs ----
  rename_map <- setNames(paste0(".", requested_cols), requested_cols)
  rename_map <- rename_map[paste0(".", requested_cols) %in% colnames(dat)]

  dat <- dat %>% rename(dplyr::all_of(rename_map))

  # ---- Final cleanup ----
  dat %>% select(-dplyr::starts_with(".time_"), -dplyr::starts_with(".value."))
}

# =============================================================================
# compute_deflator_metrics
# =============================================================================
# Computes deflator and deflator inflation from real and nominal series.
# Returns NULL if matching pairs are not found.
# =============================================================================

compute_deflator_metrics <- function(
    dat,
    deflator           = FALSE,
    deflator_inflation = FALSE,
    .compute_all       = FALSE
) {

  if (.compute_all) {
    deflator <- deflator_inflation <- TRUE
  }

  requested_cols <- c(
    if (deflator)           "value.deflator",
    if (deflator_inflation) "value.deflator_inflation"
  )

  if (length(requested_cols) == 0) return(dat)

  # Drop existing requested columns to prevent collisions
  dat <- dat %>% select(
    -dplyr::any_of(requested_cols),
    -dplyr::starts_with(".time_"),
    -dplyr::starts_with(".value.")
  )

  dat_prep <- dat %>%
    mutate(
      meta.price_basis = tolower(meta.price_basis),
      .is_real         = str_detect(meta.price_basis, "real"),
      .is_nominal      = str_detect(meta.price_basis, "nominal"),
      meta.price_basis = str_trim(str_remove(meta.price_basis, "real|nominal"))
    )

  valid <- any(dat_prep$.is_real, na.rm = TRUE) && any(dat_prep$.is_nominal, na.rm = TRUE)

  if (!valid) return(NULL)

  join_cols <- c("time", "meta.name", "meta.disaggregation_group",
                 "meta.release_tag", "meta.price_basis")

  dat_defl <- dat_prep %>%
    filter(.is_real) %>%
    inner_join(
      dat_prep %>% filter(.is_nominal),
      by = join_cols,
      suffix = c("_real", "_nominal")
    ) %>%
    mutate(.value.deflator = safe_divide(value.level_nominal, value.level_real) * 100)

  if (nrow(dat_defl) == 0) return(NULL)

  if (deflator_inflation) {
    id_cols <- c("meta.release_tag", "meta.price_basis", "meta.name", "meta.disaggregation_group")
    join_keys_same <- setNames(id_cols, id_cols)

    dat_defl <- dat_defl %>%
      mutate(.time_prev_year = previous_year(time)) %>%
      left_join(
        dat_defl %>% select(time, dplyr::all_of(id_cols), .value.deflator_prev_year = .value.deflator),
        by = c(".time_prev_year" = "time", join_keys_same)
      ) %>%
      mutate(
        .value.deflator_inflation =
          (safe_divide(.value.deflator, .value.deflator_prev_year) - 1) * 100
      ) %>%
      select(-.time_prev_year, -.value.deflator_prev_year)
  }

  rename_map <- setNames(paste0(".", requested_cols), requested_cols)
  rename_map <- rename_map[paste0(".", requested_cols) %in% colnames(dat_defl)]

  dat_defl %>%
    rename(dplyr::all_of(rename_map)) %>%
    select(dplyr::all_of(c(join_cols, intersect(requested_cols, colnames(.)))))
}

# =============================================================================
# compute_standard_metrics
# =============================================================================
# Orchestrates full metric computation including deflator metrics.
# =============================================================================

compute_standard_metrics <- function(tdl) {

  if (!"value.level" %in% colnames(tdl$data)) {
    stop("value.level column not found.", call. = FALSE)
  }

  if (!all(c("meta.parent", "meta.parent_disaggregation_group") %in% colnames(tdl$data))) {
    message(
      "Note: meta.parent column not found in data. ",
      "Auto attaching parent disaggregation layer (highest level in hierarchy map) as meta.parent. ",
      "If you want to suppress this message, please use attach_parent explicitly."
    )
    tdl <- attach_parent(tdl)
  }

  dat_orig <- tdl$data

  tdl$data <- compute_metrics(dat_orig, .compute_all = TRUE)

  dat_defl <- compute_deflator_metrics(dat_orig, .compute_all = TRUE)

  if (is.null(dat_defl)) return(list(main = tdl))

  list(main = tdl, deflator = dat_defl)
}
