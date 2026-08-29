
# =============================================================================
# METRIC FRAMEWORK
# =============================================================================
# Design:
#   - Public API uses EXPLICIT boolean arguments for IDE autocomplete,
#     transparent Roxygen documentation, and clean tracebacks.
#   - Each metric is declared ONCE with metadata in `metric_registry`.
#   - Repetitive metric math is abstracted into helper factories.
#   - The dependency graph resolves order using a clean DFS `for` loop.
#   - Computations are applied sequentially using a simple `for` loop.
#   - Computationally expensive metric categories (e.g., "Seasonal") are
#     excluded from `.compute_all` via `.avoid_during_compute_all`.
#   - Load-time consistency checks guarantee signatures map to the registry.
# =============================================================================

# ---- small numeric helpers -------------------------------------------------

safe_divide <- function(num, den) {
  den_safe <- dplyr::if_else(is.na(den) | den == 0, NA_real_, as.numeric(den))
  num / den_safe
}

safe_log_ratio <- function(num, den) {
  ratio <- safe_divide(num, den)
  ratio_safe <- dplyr::if_else(is.na(ratio) | ratio <= 0, NA_real_, as.numeric(ratio))
  log(ratio_safe)
}

safe_pct <- function(x) {
  s <- sum(x, na.rm = TRUE)
  if (is.na(s) || s == 0) rep(NA_real_, length(x)) else (x / s) * 100
}

# ---- registry entry constructor --------------------------------------------

new_metric <- function(label = "", description = "", category = "", output = "",
                       depends = character(0), requires = NULL, compute_fn) {
  stopifnot(is.function(compute_fn))
  list(
    label = label,
    description = description,
    category = category,
    output = output,
    depends = depends,
    requires = requires,
    compute_fn = compute_fn
  )
}

# ---- metric compute factories ----------------------------------------------

make_prev_n_metric <- function(out_col, n_lag) {
  function(dat, ctx) {
    lookup <- dat %>% dplyr::select(time, dplyr::all_of(ctx$id_cols), value.level)
    names(lookup)[names(lookup) == "value.level"] <- out_col
    dat$.time_prev_n_period <- dat$time - n_lag
    ctx_by <- ctx$join_prev_period
    names(ctx_by)[which(ctx_by=="time")] <- ".time_prev_n_period"
    dat %>% dplyr::left_join(lookup, by = ctx_by)
  }
}

# A specific example is following: (but this is used for more generic usage)
# make_prev_level_metric <- function(out_col, ctx_join_name) {
#   make_prev_n_metric(out_col = out_col, ctx_join_name = ctx_join_name, n_lag = 1)
# }


make_prev_level_metric <- function(out_col, ctx_join_name) {
  function(dat, ctx) {
    lookup <- dat %>% dplyr::select(time, dplyr::all_of(ctx$id_cols), value.level)
    names(lookup)[names(lookup) == "value.level"] <- out_col
    dat %>% dplyr::left_join(lookup, by = ctx[[ctx_join_name]])
  }
}

make_rate_metric <- function(out_col, prev_col, is_log = FALSE) {
  function(dat, ctx) {
    if (is_log) {
      dat[[out_col]] <- safe_log_ratio(dat$value.level, dat[[prev_col]]) * 100
    } else {
      dat[[out_col]] <- (safe_divide(dat$value.level, dat[[prev_col]]) - 1) * 100
    }
    dat
  }
}

make_prev_rate_metric <- function(out_col, in_col) {
  function(dat, ctx) {
    lookup <- dat %>% dplyr::select(time, dplyr::all_of(ctx$id_cols), dplyr::all_of(in_col))
    names(lookup)[names(lookup) == in_col] <- out_col
    dat %>% dplyr::left_join(lookup, by = ctx$join_prev_period)
  }
}

make_delta_metric <- function(out_col, curr_col, prev_col) {
  function(dat, ctx) {
    dat[[out_col]] <- dat[[curr_col]] - dat[[prev_col]]
    dat
  }
}

make_base_effect_metric <- function(out_col, in_col) {
  function(dat, ctx) {
    lookup <- dat %>% dplyr::select(time, dplyr::all_of(ctx$id_cols), dplyr::all_of(in_col))
    names(lookup)[names(lookup) == in_col] <- out_col
    lookup[[out_col]] <- -lookup[[out_col]]
    dat %>% dplyr::left_join(lookup, by = ctx$join_prev_year)
  }
}

make_share_prev_metric <- function(out_col, ctx_join_name) {
  function(dat, ctx) {
    lookup <- dat %>% dplyr::select(time, dplyr::all_of(ctx$id_cols), .value.share_pct)
    names(lookup)[names(lookup) == ".value.share_pct"] <- out_col
    dat %>% dplyr::left_join(lookup, by = ctx[[ctx_join_name]])
  }
}

make_contribution_metric <- function(out_col, share_col, present_metric_col) {
  function(dat, ctx) {
    dat[[out_col]] <- (dat[[share_col]] * dat[[present_metric_col]]) / 100
    dat
  }
}

# =============================================================================
# THE REGISTRY  -- Single source of truth for standard metrics
# =============================================================================

metric_registry <- list(

  # Placeholder for option passing
  level_lag_n = new_metric(
    label = "Previous n-Period Level (Lag)", category = "Level", output = ".value.level_lag_n",
    compute_fn = make_prev_n_metric(".value.level_lag_n", n_lag = 0)
  ),

  level_lead_n = new_metric(
    label = "Next n-Period Level (Lead)", category = "Level", output = ".value.level_lead_n",
    compute_fn = make_prev_n_metric(".value.level_lead_n", n_lag = 0)
  ),


  level_prev_prd = new_metric(
    label = "Previous Period Level", category = "Level", output = ".value.level_prev_prd",
    compute_fn = make_prev_level_metric(".value.level_prev_prd", "join_prev_period")
  ),

  level_prev_year = new_metric(
    label = "Previous Year Level", category = "Level", output = ".value.level_prev_year",
    compute_fn = make_prev_level_metric(".value.level_prev_year", "join_prev_year")
  ),

  growth_rate = new_metric(
    label = "Growth Rate (YoY)", category = "Growth", output = ".value.growth_rate", depends = "level_prev_year",
    compute_fn = make_rate_metric(".value.growth_rate", ".value.level_prev_year", is_log = FALSE)
  ),

  momentum = new_metric(
    label = "Momentum (PoP)", category = "Growth", output = ".value.momentum", depends = "level_prev_prd",
    compute_fn = make_rate_metric(".value.momentum", ".value.level_prev_prd", is_log = FALSE)
  ),

  ln_growth_rate = new_metric(
    label = "Log Growth Rate (YoY)", category = "Growth", output = ".value.ln_growth_rate", depends = "level_prev_year",
    compute_fn = make_rate_metric(".value.ln_growth_rate", ".value.level_prev_year", is_log = TRUE)
  ),

  ln_momentum = new_metric(
    label = "Log Momentum (PoP)", category = "Growth", output = ".value.ln_momentum", depends = "level_prev_prd",
    compute_fn = make_rate_metric(".value.ln_momentum", ".value.level_prev_prd", is_log = TRUE)
  ),

  base_effect = new_metric(
    label = "Base Effect", category = "Decomposition", output = ".value.base_effect", depends = "ln_momentum",
    compute_fn = make_base_effect_metric(".value.base_effect", ".value.ln_momentum")
  ),

  ln_base_effect = new_metric(
    label = "Log Base Effect", category = "Decomposition", output = ".value.ln_base_effect", depends = "ln_momentum",
    compute_fn = make_base_effect_metric(".value.ln_base_effect", ".value.ln_momentum")
  ),

  growth_rate_prev_prd = new_metric(
    label = "Previous Period Growth", category = "Growth", output = ".value.growth_rate_prev_prd", depends = "growth_rate",
    compute_fn = make_prev_rate_metric(".value.growth_rate_prev_prd", ".value.growth_rate")
  ),

  ln_growth_rate_prev_prd = new_metric(
    label = "Previous Period Log Growth", category = "Growth", output = ".value.ln_growth_rate_prev_prd", depends = "ln_growth_rate",
    compute_fn = make_prev_rate_metric(".value.ln_growth_rate_prev_prd", ".value.ln_growth_rate")
  ),

  delta_growth_rate = new_metric(
    label = "Delta Growth Rate", category = "Change", output = ".value.delta_growth_rate", depends = c("growth_rate", "growth_rate_prev_prd"),
    compute_fn = make_delta_metric(".value.delta_growth_rate", ".value.growth_rate", ".value.growth_rate_prev_prd")
  ),

  delta_ln_growth_rate = new_metric(
    label = "Delta Log Growth", category = "Change", output = ".value.delta_ln_growth_rate", depends = c("ln_growth_rate", "ln_growth_rate_prev_prd"),
    compute_fn = make_delta_metric(".value.delta_ln_growth_rate", ".value.ln_growth_rate", ".value.ln_growth_rate_prev_prd")
  ),

  level_parent = new_metric(
    label = "Parent Level", category = "Structure", output = ".value.level_parent", requires = "parent",
    compute_fn = function(dat, ctx) {
      dat_parent <- dat %>%
        dplyr::filter(
          meta.name == meta.parent,
          meta.disaggregation_group == meta.parent_disaggregation_group
        ) %>%
        dplyr::select(
          time, meta.release_tag, meta.price_basis,
          meta.parent, meta.parent_disaggregation_group,
          .value.level_parent = value.level
        )
      dat %>% dplyr::left_join(
        dat_parent,
        by = c(
          "time", "meta.release_tag", "meta.price_basis",
          "meta.parent", "meta.parent_disaggregation_group"
        )
      )
    }
  ),

  share_pct = new_metric(
    label = "Share (%)", category = "Structure", output = ".value.share_pct", depends = "level_parent", requires = "parent",
    compute_fn = function(dat, ctx) {
      dat$.value.share_pct <- safe_divide(dat$.tmp_wtd * dat$value.level, dat$.value.level_parent) * 100
      dat
    }
  ),

  share_prev_prd = new_metric(
    label = "Previous Period Share", category = "Structure", output = ".value.share_prev_prd", depends = "share_pct", requires = "parent",
    compute_fn = make_share_prev_metric(".value.share_prev_prd", "join_prev_period")
  ),

  share_prev_year = new_metric(
    label = "Previous Year Share", category = "Structure", output = ".value.share_prev_year", depends = "share_pct", requires = "parent",
    compute_fn = make_share_prev_metric(".value.share_prev_year", "join_prev_year")
  ),

  contribution_growth_rate = new_metric(
    label = "Growth Contribution", category = "Contribution", output = ".value.contribution_growth_rate", depends = c("share_prev_year", "growth_rate"), requires = "parent",
    compute_fn = make_contribution_metric(".value.contribution_growth_rate", ".value.share_prev_year", ".value.growth_rate")
  ),

  contribution_momentum = new_metric(
    label = "Momentum Contribution", category = "Contribution", output = ".value.contribution_momentum", depends = c("share_prev_prd", "momentum"), requires = "parent",
    compute_fn = make_contribution_metric(".value.contribution_momentum", ".value.share_prev_prd", ".value.momentum")
  ),

  contribution_growth_rate_pct = new_metric(
    label = "Relative Growth Contribution (%)", category = "Contribution", output = ".value.contribution_growth_rate_pct", depends = "contribution_growth_rate", requires = "parent",
    compute_fn = function(dat, ctx) {
      group_cols <- c("time", ctx$id_cols, "meta.parent", "meta.parent_disaggregation_group")
      dat %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
        dplyr::mutate(.value.contribution_growth_rate_pct = safe_pct(.value.contribution_growth_rate)) %>%
        dplyr::ungroup()
    }
  ),

  contribution_momentum_pct = new_metric(
    label = "Relative Momentum Contribution (%)", category = "Contribution", output = ".value.contribution_momentum_pct", depends = "contribution_momentum", requires = "parent",
    compute_fn = function(dat, ctx) {
      group_cols <- c("time", ctx$id_cols, "meta.parent", "meta.parent_disaggregation_group")
      dat %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
        dplyr::mutate(.value.contribution_momentum_pct = safe_pct(.value.contribution_momentum)) %>%
        dplyr::ungroup()
    }
  ),

  annual_contribution = new_metric(
    label = "Annual Contribution", category = "Structure", output = ".value.annual_contribution",
    compute_fn = function(dat, ctx) {
      group_cols <- c(".time_year", ctx$id_cols)
      dat %>%
        dplyr::mutate(.time_year = fiscal_year(time)) %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
        dplyr::mutate(.value.annual_contribution = safe_divide(value.level, sum(value.level, na.rm = TRUE)) * 100) %>%
        dplyr::ungroup() %>%
        dplyr::select(-.time_year)
    }
  ),

  SA = new_metric(
    label = "Seasonal Adjustment", category = "Seasonal", output = ".value.SA",
    compute_fn = function(dat, ctx) {
      fq_this <- frequency(dat$time, singular = TRUE)
      if (!fq_this %in% c("quarter", "month")) {
        warning(sprintf(
          "Seasonal adjustment-related measures are supported only for quarterly or monthly data (received frequency: '%s'). They will be omitted.",
          fq_this
        ), call. = FALSE)
        dat$.value.SA <- NA_real_
        return(dat)
      }
      dat_td_lst <- split(
        dat,
        list(dat$meta.release_tag, dat$meta.price_basis, dat$meta.name, dat$meta.disaggregation_group),
        drop = TRUE
      )
      for_a_node <- function(nd) {
        ndt <- as_tdf(nd)
        nd_sa <- tryCatch(
          suppressMessages(suppressWarnings(auto_seas(ndt))),
          error = function(e) NULL
        )
        if (is.null(nd_sa) || NROW(nd_sa) != NROW(nd)) {
          nd$.value.SA <- NA_real_
        } else {
          colnames(nd_sa) <- c("time", ".value.SA")
          nd <- dplyr::left_join(nd, nd_sa, by = "time")
        }
        nd
      }
      dat_re <- purrr::map(dat_td_lst, for_a_node, .progress = interactive()) |>
        purrr::list_rbind()
      if (!is_valid_derived_tdf_long(dat, dat_re)) {
        stop("Unexpected Error Found!", call. = FALSE)
      }
      dat_re
    }
  ),

  seasonally_adjusted = new_metric(
    label = "Seasonally Adjusted Level", category = "Seasonal", output = ".value.seasonally_adjusted", depends = "SA",
    compute_fn = function(dat, ctx) {
      dat$.value.seasonally_adjusted <- dat$.value.SA
      dat
    }
  ),

  # Non-recursive: joins .value.SA against itself at the previous period directly.
  SAAR = new_metric(
    label = "SAAR", category = "Seasonal", output = ".value.SAAR", depends = "SA",
    compute_fn = function(dat, ctx) {
      fq_this <- frequency(dat$time, singular = TRUE)
      if (!fq_this %in% c("quarter", "month")) {
        dat$.value.SAAR <- NA_real_
        return(dat)
      }

      lookup <- dat %>%
        dplyr::select(time, dplyr::all_of(ctx$id_cols), .value.SA) %>%
        dplyr::rename(.value.SA_prev = .value.SA)

      dat <- dat %>% dplyr::left_join(lookup, by = ctx$join_prev_period)

      fct <- if (fq_this == "quarter") 4 else 12
      dat$.value.SAAR <- ((dat$.value.SA / dat$.value.SA_prev)^fct - 1) * 100

      dat %>% dplyr::select(-.value.SA_prev)
    }
  )
)

# =============================================================================
# DEPENDENCY RESOLUTION (Clean DFS pure-functional approach)
# =============================================================================

resolve_metric_order <- function(requested, registry) {
  unknown <- setdiff(requested, names(registry))
  if (length(unknown) > 0) {
    stop("Unknown metric(s): ", paste(unknown, collapse = ", "), call. = FALSE)
  }

  initial_state <- list(
    order = character(0),
    visiting = character(0)
  )

  visit <- function(m, state) {
    if (m %in% state$order) return(state)
    if (m %in% state$visiting) stop("Circular dependency detected at metric: ", m, call. = FALSE)

    state$visiting <- c(state$visiting, m)

    for (dep in registry[[m]]$depends) {
      state <- visit(dep, state)
    }

    state$visiting <- setdiff(state$visiting, m)
    state$order <- c(state$order, m)
    return(state)
  }

  current_state <- initial_state
  for (req in requested) {
    current_state <- visit(req, current_state)
  }

  current_state$order
}

# =============================================================================
# ENGINE  -- executes compute_fn(), renames, and cleans up
# =============================================================================

compute_metrics_engine <- function(dat, requested, registry) {

  if (length(requested) == 0) {
    message("compute_metric: no output requested. Returning data unchanged.")
    return(dat)
  }
  if (!"value.level" %in% colnames(dat)) {
    stop("compute_metric: value.level column missing.", call. = FALSE)
  }

  requested_cols <- paste0("value.", requested)

  dat <- dat %>% dplyr::select(
    -dplyr::any_of(requested_cols),
    -dplyr::starts_with(".tmp_"),
    -dplyr::starts_with(".time_"),
    -dplyr::starts_with(".value.")
  )

  # Check for wtd is exists
  wchk <- hmap(dat, check_weight = TRUE)
  if(isTRUE(wchk)){

    wtd <- hmap(dat, weight = TRUE)

    idx_type_check <- wtd |>
      dplyr::group_by(meta.parent, meta.parent_disaggregation_group) |>
      dplyr::summarise(sw = sum(wtd), .groups = "drop") |>
      dplyr::pull(sw)

    # This will be 0 for index types (where wtd sums close to 1)
    idx_type_check <- max(abs(round(idx_type_check - 1,1)))

    dat <- dat |>
      left_join(
        # get wtd
        wtd,
        by = c("meta.name", "meta.disaggregation_group","meta.parent", "meta.parent_disaggregation_group")
      )
    colnames(dat)[colnames(dat)=="wtd"] <- ".tmp_wtd"

    if(anyNA(dat[[".tmp_wtd"]])){
      if(idx_type_check){
        message(
          "Some weights are unavailable for certain meta.name–parent combinations ",
          "and have been set to 1. This may not be appropriate for index-type data. ",
          "Please check the results carefully."
        )
      }
      dat[[".tmp_wtd"]][is.na(dat[[".tmp_wtd"]])] <- 1
    }

  } else {
    # Set wtd to 1 for calculation symmetry
    dat[[".tmp_wtd"]] <- 1
  }

  id_cols <- c("meta.release_tag", "meta.price_basis", "meta.name", "meta.disaggregation_group")
  missing_id_cols <- setdiff(id_cols, colnames(dat))
  if (length(missing_id_cols) > 0) {
    dat[missing_id_cols] <- "dummy"
    if (dat$meta.release_tag[1] == "dummy" && length(unique(dat$meta.release_tag)) == 1) {
      dat$meta.release_tag <- "#main"
    }
    chk <- dat |>
      dplyr::group_by(dplyr::across(dplyr::all_of(unique(c("time", id_cols))))) |>
      cols_causing_group_variation()
    if (length(chk) > 0) {
      stop("Failed to create dummy ID columns. Check if required `meta.*` columns were accidentally dropped.", call. = FALSE)
    }
  }

  has_parent <- all(c("meta.parent", "meta.parent_disaggregation_group") %in% colnames(dat))
  join_keys_same <- setNames(id_cols, id_cols)

  ctx <- list(
    id_cols = id_cols,
    join_keys_same = join_keys_same,
    join_prev_period = c(setNames("time", ".time_prev_period"), join_keys_same),
    join_prev_year = c(setNames("time", ".time_prev_year"), join_keys_same),
    has_parent = has_parent,
    missing_id_cols = missing_id_cols
  )

  if (!ctx$has_parent) {
    parent_needed <- requested[vapply(requested, function(m) identical(registry[[m]]$requires, "parent"), logical(1))]
    if (length(parent_needed) > 0) {
      warning(
        "compute_metric: meta.parent / meta.parent_disaggregation_group not found. ",
        "The following requested metrics will be dropped: ", paste(parent_needed, collapse = ", "),
        call. = FALSE
      )
      requested <- setdiff(requested, parent_needed)
      requested_cols <- paste0("value.", requested)
    }
  }

  if (length(requested) == 0) return(dat)

  order <- resolve_metric_order(requested, registry)

  dat <- dat %>% dplyr::mutate(
    .time_prev_period = previous_period(time),
    .time_prev_year   = previous_year(time)
  )

  # Sequential execution
  for (m in order) {
    dat <- registry[[m]]$compute_fn(dat, ctx)
  }

  rename_map <- setNames(paste0(".", requested_cols), requested_cols)
  rename_map <- rename_map[paste0(".", requested_cols) %in% colnames(dat)]
  dat <- dat %>% dplyr::rename(dplyr::all_of(rename_map))

  dat %>% dplyr::select(
    -dplyr::starts_with(".tmp_"),
    -dplyr::starts_with(".time_"),
    -dplyr::starts_with(".value."),
    -dplyr::any_of(ctx$missing_id_cols)
  )
}

# =============================================================================
# EXPLICIT PUBLIC SIGNATURES
# =============================================================================

#' Compute Derived Time-Series Metrics
#'
#' @description
#' A unified generic entry point for computing derived time-series metrics such as
#' growth rates, momentum, base effects, structural shares, growth contributions,
#' seasonal measures, and deflator measures. Supports \code{data.frame},
#' \code{tdf_long}, and \code{tdf} inputs through S3 dispatch.
#'
#' @details
#' The calculation engine is driven by a declarative metric registry and a
#' dependency graph. When a metric has dependencies, the required prerequisite
#' metrics are automatically resolved and computed in the correct order. For
#' example, requesting \code{contribution_growth_rate} automatically computes
#' its required \code{share_prev_year} and \code{growth_rate} metrics first.
#'
#' The public function signature explicitly exposes the available metric
#' arguments to support IDE autocomplete. Computation is then dispatched to
#' the appropriate S3 method according to the class of \code{dat}.
#'
#' \strong{Important limitations:}
#' \itemize{
#'   \item \strong{Standard vs. deflator metrics:} Standard metrics and deflator
#'   metrics cannot be computed in the same call. They must be requested in
#'   separate calls.
#'   \item \strong{Wide \code{tdf} objects:} A maximum of one metric may be
#'   computed per call for a wide \code{tdf} object. The \code{.compute_all}
#'   option, deflator calculations, and custom functions returning multiple
#'   metrics are not supported for wide \code{tdf} objects.
#' }
#'
#' @param dat A \code{data.frame}, \code{tdf_long}, or \code{tdf} object.
#' Must contain at least \code{time} and \code{value.level} columns.
#' Standard data frames are checked for compatibility with \code{tdf_long}
#' or \code{tdf} and converted internally when possible.
#'
#' @param level_lag_n Logical. Compute Previous n-Period Level (Lag).
#' The lag value is specified in \code{.options}.
#' @param level_lead_n Logical. Compute Next n-Period Level (Lead).
#' The lead value is specified in \code{.options}.
#' @param level_prev_prd Logical. Compute Previous Period Level.
#' @param level_prev_year Logical. Compute Previous Year Level.
#' @param growth_rate Logical. Compute Growth Rate (YoY).
#' @param momentum Logical. Compute Momentum (PoP).
#' @param ln_growth_rate Logical. Compute Log Growth Rate (YoY).
#' @param ln_momentum Logical. Compute Log Momentum (PoP).
#' @param base_effect Logical. Compute Base Effect.
#' @param ln_base_effect Logical. Compute Log Base Effect.
#' @param growth_rate_prev_prd Logical. Compute Previous Period Growth.
#' @param ln_growth_rate_prev_prd Logical. Compute Previous Period Log Growth.
#' @param delta_growth_rate Logical. Compute Delta Growth Rate.
#' @param delta_ln_growth_rate Logical. Compute Delta Log Growth.
#' @param level_parent Logical. Compute Parent Level. Requires
#' \code{meta.parent} and \code{meta.parent_disaggregation_group}.
#' @param share_pct Logical. Compute Share (\%). Requires
#' \code{meta.parent} and \code{meta.parent_disaggregation_group}.
#' @param share_prev_prd Logical. Compute Previous Period Share. Requires
#' \code{meta.parent} and \code{meta.parent_disaggregation_group}.
#' @param share_prev_year Logical. Compute Previous Year Share. Requires
#' \code{meta.parent} and \code{meta.parent_disaggregation_group}.
#' @param contribution_growth_rate Logical. Compute Growth Contribution.
#' Requires \code{meta.parent} and \code{meta.parent_disaggregation_group}.
#' @param contribution_momentum Logical. Compute Momentum Contribution.
#' Requires \code{meta.parent} and \code{meta.parent_disaggregation_group}.
#' @param contribution_growth_rate_pct Logical. Compute Relative Growth
#' Contribution (\%). Requires \code{meta.parent} and
#' \code{meta.parent_disaggregation_group}.
#' @param contribution_momentum_pct Logical. Compute Relative Momentum
#' Contribution (\%). Requires \code{meta.parent} and
#' \code{meta.parent_disaggregation_group}.
#' @param annual_contribution Logical. Compute Annual Contribution.
#' @param SA Logical. Compute Seasonal Adjustment. Supported for quarterly
#' and monthly data.
#' @param seasonally_adjusted Logical. Compute Seasonally Adjusted Level.
#' Supported for quarterly and monthly data.
#' @param SAAR Logical. Compute SAAR (Seasonally Adjusted Annual Rate).
#' Supported for quarterly and monthly data.
#'
#' @param deflator Logical. Compute the Implicit Price Deflator. Requires
#' matching real and nominal series in \code{meta.price_basis}.
#' @param deflator_inflation Logical. Compute Deflator Inflation. Requires
#' matching real and nominal series in \code{meta.price_basis}.
#'
#' @param .compute_all Logical. If \code{TRUE}, automatically computes all
#' available metrics in the selected metric family. Standard metrics are
#' computed when no deflator metric is requested; deflator metrics are
#' computed when \code{deflator} or \code{deflator_inflation} is requested.
#' Default is \code{FALSE}.
#' @param .avoid_during_compute_all Character vector. Categories of metrics
#' to exclude when \code{.compute_all = TRUE}. Defaults to \code{"Seasonal"}.
#' @param .custom_function A function or a named list of functions defining
#' custom metrics. By default, each function is applied to the
#' \code{value.level} vector. When \code{.options$.custom_function = "data"},
#' the function receives the complete data passed to the metric engine.
#' For named lists, each name is used as the suffix of the generated
#' \code{value.*} column.
#' @param .options A named list of options controlling dynamic metrics and
#' custom functions. \code{level_lag_n} and \code{level_lead_n} specify the
#' lag and lead lengths, respectively. \code{.custom_function} specifies how
#' \code{.custom_function} is evaluated and must be either \code{"on_level"}
#' or \code{"data"}. Default is
#' \code{list(level_lag_n = 1, level_lead_n = 1,
#' .custom_function = c("on_level", "data"))}.
#' @param .omit_na Logical. Used only for wide \code{tdf} output. If
#' \code{TRUE}, omits \code{NA} values after computing the metric. If
#' \code{NULL}, the default, the behaviour is determined automatically from
#' the original data.
#' @param ... Additional arguments passed to the dispatched S3 method.
#'
#' @return Depending on the input class:
#' \itemize{
#'   \item For \code{tdf_long}: The input data with newly appended
#'   \code{.value.*} columns for the requested metrics.
#'   \item For \code{data.frame}: The input data with computed metric
#'   columns appended and its original class and attributes restored.
#'   \item For \code{tdf}: A \code{tdf} object containing the calculated
#'   metric in wide format. Wide \code{tdf} input supports at most one metric
#'   per call.
#' }
#'
#' @export
#' @rdname compute_metric
compute_metric <- function(
    dat,
    level_lag_n = FALSE,
    level_lead_n = FALSE,
    level_prev_prd = FALSE,
    level_prev_year = FALSE,
    growth_rate = FALSE,
    momentum = FALSE,
    ln_growth_rate = FALSE,
    ln_momentum = FALSE,
    base_effect = FALSE,
    ln_base_effect = FALSE,
    growth_rate_prev_prd = FALSE,
    ln_growth_rate_prev_prd = FALSE,
    delta_growth_rate = FALSE,
    delta_ln_growth_rate = FALSE,
    level_parent = FALSE,
    share_pct = FALSE,
    share_prev_prd = FALSE,
    share_prev_year = FALSE,
    contribution_growth_rate = FALSE,
    contribution_momentum = FALSE,
    contribution_growth_rate_pct = FALSE,
    contribution_momentum_pct = FALSE,
    annual_contribution = FALSE,
    SA = FALSE,
    seasonally_adjusted = FALSE,
    SAAR = FALSE,
    deflator = FALSE,
    deflator_inflation = FALSE,
    .compute_all = FALSE,
    .avoid_during_compute_all = "Seasonal",
    # Custom function has to be either a simple function or a named list of functions
    .custom_function = NULL,
    .options = list(
      level_lag_n = 1,
      level_lead_n = 1,
      .custom_function = c("on_level","data")
    ),
    ...) UseMethod("compute_metric")

#' @export
#' @rdname compute_metric
compute_metric.tdf_long <- function(
    dat,
    level_lag_n = FALSE,
    level_lead_n = FALSE,
    level_prev_prd = FALSE,
    level_prev_year = FALSE,
    growth_rate = FALSE,
    momentum = FALSE,
    ln_growth_rate = FALSE,
    ln_momentum = FALSE,
    base_effect = FALSE,
    ln_base_effect = FALSE,
    growth_rate_prev_prd = FALSE,
    ln_growth_rate_prev_prd = FALSE,
    delta_growth_rate = FALSE,
    delta_ln_growth_rate = FALSE,
    level_parent = FALSE,
    share_pct = FALSE,
    share_prev_prd = FALSE,
    share_prev_year = FALSE,
    contribution_growth_rate = FALSE,
    contribution_momentum = FALSE,
    contribution_growth_rate_pct = FALSE,
    contribution_momentum_pct = FALSE,
    annual_contribution = FALSE,
    SA = FALSE,
    seasonally_adjusted = FALSE,
    SAAR = FALSE,
    deflator = FALSE,
    deflator_inflation = FALSE,
    .compute_all = FALSE,
    .avoid_during_compute_all = "Seasonal",
    # Custom function has to be either a simple function or a named list of functions
    .custom_function = NULL,
    .options = list(
      level_lag_n = 1,
      level_lead_n = 1,
      .custom_function = c("on_level","data")
    )
) {

  # This needs to run before any further variable definition
  all_args <- as.list(environment())

  has_deflator <- isTRUE(deflator) || isTRUE(deflator_inflation)

  ignore_args <- c("dat", ".compute_all", ".avoid_during_compute_all", "deflator", "deflator_inflation", ".custom_function",".options")
  standard_args <- all_args[setdiff(names(all_args), ignore_args)]

  has_standard <- any(unlist(standard_args) == TRUE) || (isTRUE(.compute_all) && !has_deflator)

  ## custom function

  .options$.custom_function <- match.arg(.options$.custom_function, c("on_level","data"))

  f_present <- FALSE

  cfs <- .custom_function

  if(is.list(cfs)){

    cfs <- cfs[!is.na(names(cfs)) & names(cfs) != ""]

    if(length(names(cfs))>0){

      if(all(vapply(cfs, is.function, logical(1)))){
        f_present <- TRUE
      }

    }
  }

  if(is.function(cfs)) {
    f_present <- TRUE
    cfs <- list(custom_metric = .custom_function)
  }


  if (has_standard && has_deflator) {
    stop("Cannot compute standard metrics and deflator metrics simultaneously. Please calculate them in separate calls.", call. = FALSE)
  }

  if (has_deflator) {
    return(compute_deflator_metrics(
      dat,
      deflator = deflator,
      deflator_inflation = deflator_inflation,
      .compute_all = .compute_all
    ))
  }

  if (isTRUE(.compute_all)) {
    requested <- names(standard_args)

    # Exclude metrics belonging to specified categories
    if (!is.null(.avoid_during_compute_all) && length(.avoid_during_compute_all) > 0 && !all(.avoid_during_compute_all == "")) {
      metrics_to_drop <- names(metric_registry)[
        vapply(metric_registry, function(m) m$category %in% .avoid_during_compute_all, logical(1))
      ]
      requested <- setdiff(requested, metrics_to_drop)
    }
  } else {
    requested <- names(standard_args)[unlist(standard_args) == TRUE]
  }

  metric_registry_this <- metric_registry

  # Dynamic (option linked measures) - need to update them
  metric_registry_this$level_lag_n$compute_fn <- make_prev_n_metric(
    ".value.level_lag_n", n_lag = as.integer(.options$level_lag_n))

  metric_registry_this$level_lead_n$compute_fn <- make_prev_n_metric(
    # Note the -1 sign here
    ".value.level_lead_n", n_lag = -as.integer(.options$level_lead_n))


  if(f_present){

    common_names <- intersect(requested, names(cfs))

    if(length(common_names)>0){
      warning(
        paste0(
          "Custom metric names overlap with existing metrics: ",
          paste(common_names, collapse = ", "),
          ". Results may be misleading."
        ),
        call. = FALSE
      )
    }

    requested <- unique(c( requested, names(cfs)))

    new_metric_registry <- cfs |> purrr::map2(
      names(cfs), function(nd, nn){
        new_metric(
          label = paste0("Custom Function", nn),
          category = "Custom",
          output = paste0(".value.", nn),
          compute_fn = function(dat, ctx){

            dat <- dat |>
              dplyr::group_by() |>
              dplyr::arrange(.data$time)

            if(.options$.custom_function == "data"){
              dat <- dat |>
                dplyr::mutate(.value.custom_fn_tmp = nd( .data))
            } else {
              dat <- dat |>
                dplyr::mutate(.value.custom_fn_tmp = nd( .data$value.level))
            }

            dat <- dat |>
              dplyr::ungroup()

            names(dat)[names(dat) == ".value.custom_fn_tmp"] <- paste0(".value.", nn)
            dat
          })
      })

    metric_registry_this <- c(metric_registry_this, new_metric_registry)
  }

  compute_metrics_engine(dat, requested, metric_registry_this)
}


#' @export
#' @rdname compute_metric
compute_metric.tdf <- function(
    dat,
    level_lag_n = FALSE,
    level_lead_n = FALSE,
    level_prev_prd = FALSE,
    level_prev_year = FALSE,
    growth_rate = FALSE,
    momentum = FALSE,
    ln_growth_rate = FALSE,
    ln_momentum = FALSE,
    base_effect = FALSE,
    ln_base_effect = FALSE,
    growth_rate_prev_prd = FALSE,
    ln_growth_rate_prev_prd = FALSE,
    delta_growth_rate = FALSE,
    delta_ln_growth_rate = FALSE,
    level_parent = FALSE,
    share_pct = FALSE,
    share_prev_prd = FALSE,
    share_prev_year = FALSE,
    contribution_growth_rate = FALSE,
    contribution_momentum = FALSE,
    contribution_growth_rate_pct = FALSE,
    contribution_momentum_pct = FALSE,
    annual_contribution = FALSE,
    SA = FALSE,
    seasonally_adjusted = FALSE,
    SAAR = FALSE,
    .custom_function = NULL,
    .options = list(
      level_lag_n = 1,
      level_lead_n = 1,
      .custom_function = c("on_level","data")
    ),
    .omit_na = NULL
) {

  # This needs to run before any further variable definition
  all_args <- as.list(environment())

  if(!is.logical(.omit_na)){
    dchk <- stats::na.omit(dat)
    # If main data does not have NA then omit NA otherwise not
    .omit_na <- NROW(dchk)==NROW(dat)
  }

  f_mode <- is.function(.custom_function)

  metric_registry_this <- metric_registry

  # Dynamic (option linked measures) - need to update them
  metric_registry_this$level_lag_n$compute_fn <- make_prev_n_metric(
    ".value.level_lag_n", n_lag = as.integer(.options$level_lag_n))

  metric_registry_this$level_lead_n$compute_fn <- make_prev_n_metric(
    # Note the -1 sign here
    ".value.level_lead_n", n_lag = -as.integer(.options$level_lead_n))


  if(f_mode){

    requested <- "custom_fn"

    .options$.custom_function <- match.arg(.options$.custom_function, c("on_level","data"))

    metric_registry_this$custom_fn <- new_metric(
      label = "Custom Function", category = "Custom", output = ".value.custom_fn",
      compute_fn = function(dat, ctx){
        dat <- dat |>
          dplyr::group_by() |>
          dplyr::arrange(.data$time)

        if(.options$.custom_function == "data"){
          dat <- dat |>
            dplyr::mutate(.value.custom_fn = .custom_function( .data))
        } else {
          dat <- dat |>
            dplyr::mutate(.value.custom_fn = .custom_function( .data$value.level))
        }

        dat <- dat |>
          dplyr::ungroup()

        dat
      })

  } else {

    metric_args <- all_args[setdiff(names(all_args), c("dat",".custom_function",".options",".omit_na"))]
    requested <- names(metric_args)[unlist(metric_args) == TRUE]

    if (length(requested) > 1) {
      stop("Conflicting arguments: You can set a maximum of ONE of the metric arguments to TRUE for a wide .tdf.", call. = FALSE)
    }

    if (length(requested) == 0) return(dat)

  }

  dl <- as_tdf_long(dat)
  dl2 <- compute_metrics_engine(dl, requested, metric_registry_this)
  d2 <- as_tdf(dl2, value_name = requested[1])
  if(.omit_na) {
    stats::na.omit(d2)
  } else {
    d2
  }
}


#' @export
compute_metric.data.frame <- function(dat, ...) {

  out_dat <- NULL

  chk_tdfl <- tryCatch(
    {
      suppressWarnings(
        tdf_long_check_structure(dat, hmap = data.frame())
      )
      TRUE
    }, error = function(e) FALSE
  )

  if (!chk_tdfl) {

    chk_tdf <- tryCatch(
      {
        d0 <- as_tdf_for_ts_and_df(dat)
        if (NCOL(d0) < NCOL(dat)) {
          data.frame()
        } else {
          d0
        }
      },
      error = function(e) data.frame(),
      warning = function(w) data.frame()
    )

    if (NROW(chk_tdf) == 0 || (NCOL(chk_tdf) < NCOL(dat))) {
      stop("Input data.frame cannot be cast to 'tdf' or 'tdf_long' format. Please verify your data structure.", call. = FALSE)
    } else {
      message("Data implicitly & internally converted to 'tdf' format. To suppress this message, explicitly cast your data.frame prior to computing metrics.")
      out_dat <- compute_metric.tdf(chk_tdf, ...)
    }

  } else {
    # Note this conversion is not required this_dat <- as_tdf_long.data.frame(dat) as compute_metric.tdf_long does not strictly needs tdf-long
    out_dat <- compute_metric.tdf_long(dat, ...)
  }

  if (NROW(out_dat) == 0 || is.null(out_dat)) {
    stop("Can't Compute Metric: Please explicitly convert the input data.frame to either 'tdf' or 'tdf_long' format before proceeding.", call. = FALSE)
  }

  # Reset class and attributes
  ao <- attributes(out_dat)
  ai <- attributes(dat)

  ao <- ao[names(ai)]
  ao$class <- ai$class
  attributes(out_dat) <- ao
  class(out_dat) <- class(dat)

  out_dat
}


# =============================================================================
# SIGNATURE / REGISTRY CONSISTENCY CHECK
# =============================================================================
# Ensures that manual arguments in the exported functions completely map
# to the declarative `metric_registry`. Fails fast on load if drifted.

stopifnot(
  "compute_metric.tdf_long args out of sync with metric_registry" =
    setequal(
      setdiff(names(formals(compute_metric.tdf_long)), c("dat", ".compute_all", ".avoid_during_compute_all", "deflator", "deflator_inflation",".custom_function",".options")),
      names(metric_registry)
    ),
  "compute_metric args out of sync with tdf_long and tdf method args" =
    setequal(
      setdiff(names(formals(compute_metric)), c(names(formals(compute_metric.tdf)), names(formals(compute_metric.tdf_long)))),
      "..."
    ),
  "compute_metric.tdf args out of sync with metric_registry" =
    setequal(setdiff(names(formals(compute_metric.tdf)), c("dat", ".custom_function",".options",".omit_na")), names(metric_registry))
)

# =============================================================================
# DEFLATOR PATH
# =============================================================================

deflator_registry <- list(
  deflator = new_metric(
    label = "Implicit Price Deflator", output = ".value.deflator",
    compute_fn = function(dat, ctx) {
      dat$.value.deflator <- safe_divide(dat$value.level_nominal, dat$value.level_real) * 100
      dat
    }
  ),
  deflator_inflation = new_metric(
    label = "Deflator Inflation", output = ".value.deflator_inflation", depends = "deflator",
    compute_fn = function(dat, ctx) {
      dat <- dat %>% dplyr::mutate(.time_prev_year = previous_year(time))

      lookup <- dat %>%
        dplyr::select(time, dplyr::all_of(ctx$id_cols), .value.deflator) %>%
        dplyr::rename(.value.deflator_prev_year = .value.deflator)

      dat %>%
        dplyr::left_join(lookup, by = ctx$join_prev_year) %>%
        dplyr::mutate(.value.deflator_inflation = (safe_divide(.value.deflator, .value.deflator_prev_year) - 1) * 100) %>%
        dplyr::select(-.time_prev_year, -.value.deflator_prev_year)
    }
  )
)

compute_deflator_metrics <- function(dat, deflator = FALSE, deflator_inflation = FALSE,
                                     .compute_all = FALSE, .blank_for_real_nominal_missing = FALSE) {

  flags <- list(deflator = deflator, deflator_inflation = deflator_inflation)

  if (isTRUE(.compute_all)) {
    requested <- names(flags)
  } else {
    requested <- names(flags)[unlist(flags) == TRUE]
  }

  if (length(requested) == 0) return(dat)

  id_cols <- c("meta.release_tag", "meta.price_basis", "meta.name", "meta.disaggregation_group")
  missing_id_cols <- setdiff(id_cols, colnames(dat))
  if (length(missing_id_cols) > 0) {
    dat[missing_id_cols] <- "dummy"
    if (dat$meta.release_tag[1] == "dummy" && length(unique(dat$meta.release_tag)) == 1) {
      dat$meta.release_tag <- "#main"
    }
  }

  dat_prep <- dat %>%
    dplyr::mutate(
      meta.price_basis = tolower(meta.price_basis),
      .is_real    = stringr::str_detect(meta.price_basis, "real"),
      .is_nominal = stringr::str_detect(meta.price_basis, "nominal"),
      meta.price_basis = stringr::str_trim(stringr::str_remove(meta.price_basis, "real|nominal"))
    )

  valid <- any(dat_prep$.is_real, na.rm = TRUE) && any(dat_prep$.is_nominal, na.rm = TRUE)
  if (!valid) {
    if (.blank_for_real_nominal_missing) return(NULL)
    stop("Not possible to calculate deflator and related measures! (real nominal both needed)", call. = FALSE)
  }

  join_cols <- c("time", "meta.name", "meta.disaggregation_group", "meta.release_tag", "meta.price_basis")
  dat_defl <- dat_prep %>%
    dplyr::filter(.is_real) %>%
    dplyr::inner_join(dat_prep %>% dplyr::filter(.is_nominal), by = join_cols, suffix = c("_real", "_nominal"))

  if (nrow(dat_defl) == 0) return(NULL)

  join_keys_same <- setNames(id_cols, id_cols)
  ctx <- list(
    id_cols = id_cols,
    join_keys_same = join_keys_same,
    join_prev_year = c(setNames("time", ".time_prev_year"), join_keys_same)
  )

  order <- resolve_metric_order(requested, deflator_registry)

  for (m in order) {
    dat_defl <- deflator_registry[[m]]$compute_fn(dat_defl, ctx)
  }

  requested_cols <- paste0("value.", requested)
  rename_map <- setNames(paste0(".", requested_cols), requested_cols)
  rename_map <- rename_map[paste0(".", requested_cols) %in% colnames(dat_defl)]
  dat_defl <- dat_defl %>% dplyr::rename(dplyr::all_of(rename_map))

  dat_defl %>% dplyr::select(dplyr::all_of(c(join_cols, intersect(requested_cols, colnames(dat_defl)))))
}

# =============================================================================
# compute_standard_metrics (Wrapper)
# =============================================================================
# Orchestrates full metric computation for a `tdl` object. Auto-resolves
# missing parent hierarchies and routes data through both the standard
# and deflator execution engines, supporting categorical metric avoidance.
# =============================================================================

compute_standard_metrics <- function(tdl, .avoid_during_compute_all = "Seasonal") {

  if (!"value.level" %in% colnames(tdl$data)) {
    stop("value.level column not found.", call. = FALSE)
  }

  # Auto-attach parent hierarchy if missing BEFORE passing to the framework
  if (!all(c("meta.parent", "meta.parent_disaggregation_group") %in% colnames(tdl$data))) {
    message(
      "Note: meta.parent column not found in data. ",
      "Auto attaching parent disaggregation layer (highest level in hierarchy map) as meta.parent. ",
      "If you want to suppress this message, please use attach_parent explicitly."
    )
    tdl <- attach_parent(tdl)
  }

  dat_orig <- tdl$data

  # 1. Compute all standard metrics using the new framework's explicit method
  tdl$data <- compute_metric.tdf_long(
    dat_orig,
    .compute_all = TRUE,
    .avoid_during_compute_all = .avoid_during_compute_all
  )

  # 2. Compute deflator metrics using the new standalone deflator function
  dat_defl <- compute_deflator_metrics(
    dat_orig,
    .compute_all = TRUE,
    .blank_for_real_nominal_missing = TRUE
  )

  if (is.null(dat_defl)) return(list(main = tdl))

  list(main = tdl, deflator = dat_defl)
}

