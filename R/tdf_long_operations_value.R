
calculate_standard_measures <- function(tdl){
  # Calculate:

  # Growth Rate, Momentum, Contribution to Growth Rate, Contribution to
  # Momentum, YoY Growth Rate, QoQ Growth Rate, etc. for all possible
  # combinations of meta.disaggregation_group and meta.name based on the
  # hierarchy map.

  # It requires a parent.


  if(!"value.level" %in% colnames(tdl$data)){
    stop("value.level column not found in data. Please ensure that the data has value.level column for calculations.", call. = FALSE)
  }

  if(!"meta.parent" %in% colnames(tdl$data) || !"meta.parent_disaggregation_group" %in% colnames(tdl$data)){
    message(
      "Note: meta.parent column not found in data. ",
      "Auto attaching parent disaggregation layer (highest level in hierarchy map) as meta.parent. ",
      "If you want to suppress this message, please use attach_parent explicitly."
    )

    tdl <- attach_parent(tdl)
  }

  dat_orig <- tdl$data

  dat <- dat_orig %>%
    mutate(time_prev_period = previous_period(time),
           time_prev_year = previous_year(time))

  # Attach last year and last period levels
  dat <- dat %>%
    left_join(
      dat %>%
        select(time, meta.release_tag, meta.price_basis, meta.name, meta.disaggregation_group,
               value.level_prev_prd = value.level),
      by = c("time_prev_period" = "time",
             "meta.release_tag", "meta.price_basis", "meta.name", "meta.disaggregation_group")
    ) %>%
    left_join(
      dat %>%
        select(time, meta.release_tag, meta.price_basis, meta.name, meta.disaggregation_group,
               value.level_prev_year = value.level),
      by = c("time_prev_year" = "time",
             "meta.release_tag", "meta.price_basis", "meta.name", "meta.disaggregation_group")
    )

  # Calculate Growth Rate, Momentum
  dat <- dat %>%
    mutate(
      value.growth_rate = (value.level/value.level_prev_year - 1)*100,
      value.momentum = (value.level/value.level_prev_prd - 1)*100,
      value.ln_growth_rate = log(value.level/value.level_prev_year)*100,
      value.ln_momentum = log(value.level/value.level_prev_prd)*100
    )

  # Base Effect
  dat <- dat %>%
    left_join(
      dat %>%
        mutate(
          value.base_effect = -value.ln_momentum,
          # This should be - ln_base_effect (but using this for convenience)
          value.ln_base_effect = -value.ln_momentum
        ) %>%
        select(time, meta.release_tag, meta.price_basis, meta.name, meta.disaggregation_group,
               value.base_effect,
               value.ln_base_effect),
      by = c("time_prev_year" = "time",
             "meta.release_tag", "meta.price_basis", "meta.name", "meta.disaggregation_group")
    )

  # Previous Period in Growth Rates
  dat <- dat %>%
    left_join(
      dat %>%
        select(time, meta.release_tag, meta.price_basis, meta.name, meta.disaggregation_group,
               value.growth_rate_prev_prd = value.growth_rate,
               value.ln_growth_rate_prev_prd = value.ln_growth_rate),
      by = c("time_prev_period" = "time",
             "meta.release_tag", "meta.price_basis", "meta.name", "meta.disaggregation_group")
    )

  # Delta of Growth Rate
  dat <- dat %>%
    mutate(
      value.delta_growth_rate = value.growth_rate - value.growth_rate_prev_prd,
      value.delta_ln_growth_rate = value.ln_growth_rate - value.ln_growth_rate_prev_prd
    )

  # Share in parent (for this meta.disaggregation_group and meta.name)
  dat <- dat %>%
    group_by(time, meta.release_tag, meta.price_basis,
             meta.disaggregation_group,
             meta.parent, meta.parent_disaggregation_group) %>%
    mutate(
      value.share_pct = value.level / sum(value.level, na.rm = TRUE)*100
    ) %>%
    ungroup()

  # Attach last year and last period shares
  dat <- dat %>%
    left_join(
      dat %>%
        select(time, meta.release_tag, meta.price_basis, meta.name, meta.disaggregation_group,
               value.share_prev_prd = value.share_pct),
      by = c("time_prev_period" = "time",
             "meta.release_tag", "meta.price_basis", "meta.name", "meta.disaggregation_group")
    ) %>%
    left_join(
      dat %>%
        select(time, meta.release_tag, meta.price_basis, meta.name, meta.disaggregation_group,
               value.share_prev_year = value.share_pct),
      by = c("time_prev_year" = "time",
             "meta.release_tag", "meta.price_basis", "meta.name", "meta.disaggregation_group")
    )

  # Now Contribution in growth rate
  dat <- dat %>%
    ungroup() %>%
    mutate(
      value.contribution_growth_rate = value.share_prev_year * value.growth_rate / 100,
      value.contribution_momentum = value.share_prev_prd * value.momentum / 100
    ) %>%
    group_by(time, meta.release_tag, meta.price_basis,
             meta.disaggregation_group,
             meta.parent, meta.parent_disaggregation_group) %>%
    mutate(
      value.contribution_growth_rate_pct = value.contribution_growth_rate / sum(value.contribution_growth_rate, na.rm = TRUE)*100,
      value.contribution_momentum_pct = value.contribution_momentum / sum(value.contribution_momentum, na.rm = TRUE)*100
    )

  # Temporal Side Measure (Contribution in Annual)

   dat <- dat %>%
     mutate(time_year = fiscal_year(time))  %>%
     group_by(time_year, meta.release_tag, meta.price_basis,
              meta.name, meta.disaggregation_group) %>%
     mutate(
       value.annual_contribution = value.level / sum(value.level, na.rm = TRUE)*100
     ) %>%
     ungroup() %>%
     select(-time_year)


  # Remove intermediate time_prev_period and time_prev_year columns
  dat <- dat %>% select(-time_prev_period, -time_prev_year)


  # Deflator Calculation
  # (Special Calculation and this also adds rows - at the end)


  allowed_cols <- c("time", "meta.name", "meta.disaggregation_group", "meta.release_tag", "meta.price_basis", "value.level",
                    "meta.release_order", "meta.release_date")

  this_allowed_cols <- colnames(dat_orig) %>% intersect(allowed_cols)


  dat_defl <- dat_orig %>%
    select(all_of(this_allowed_cols)) %>%
    mutate(
      meta.price_basis = meta.price_basis %>% tolower()
    ) %>%
    mutate(
      meta.is_real = meta.price_basis %>% str_detect("real"),
      meta.is_nominal = meta.price_basis %>% str_detect("nominal")
    ) %>%
    mutate(
      # remove real or nominal from string part and make str_trim
      meta.price_basis = meta.price_basis %>% str_remove("real|nominal") %>% str_trim()
    )

  # Check if deflator can be calculated
  chk <- FALSE
  dat_defl_calc <- NULL

  if(all(!is.na(dat_defl$meta.is_real)) && all(!is.na(dat_defl$meta.is_nominal))){
    if(all(!dat_defl$meta.is_real==dat_defl$meta.is_nominal)) {
      nr <- dat_defl %>% filter(meta.is_real) %>% NROW()
      nn <- dat_defl %>% filter(meta.is_nominal) %>% NROW()
      if(nr > 1 && nn > 1){
        chk <- TRUE
      }
    }
  }


  if(chk){
    dat_defl_real <- dat_defl %>% filter(meta.is_real) %>% select(-meta.is_real, -meta.is_nominal)
    dat_defl_nominal <- dat_defl %>% filter(meta.is_nominal) %>% select(-meta.is_real, -meta.is_nominal)

    dat_defl_calc <- dat_defl_real %>%
      inner_join(
        dat_defl_nominal,
        by = c("time", "meta.name", "meta.disaggregation_group", "meta.release_tag", "meta.price_basis",
               intersect(c("meta.release_order", "meta.release_date"), colnames(dat_defl_real))),
        suffix = c("_real", "_nominal")
      ) %>%
      mutate(
        value.deflator = value.level_nominal / value.level_real*100
      )

    dat_defl_calc <- dat_defl_calc %>%
      select(-value.level_real, -value.level_nominal)

    # Deflator Inflation
    dat_defl_calc <- dat_defl_calc %>%
      mutate(time_prev_year = previous_year(time)) %>%
      left_join(
        dat_defl_calc %>%
          select(time, meta.release_tag, meta.price_basis, meta.name, meta.disaggregation_group,
                 value.deflator_prev_year = value.deflator),
        by = c("time_prev_year" = "time",
               "meta.release_tag", "meta.price_basis", "meta.name", "meta.disaggregation_group")
      ) %>%
      mutate(
        value.deflator_inflation = (value.deflator/value.deflator_prev_year - 1)*100
      )

    dat_defl_calc <- dat_defl_calc %>% select(-time_prev_year)

  } else {
    message("Deflator calculation skipped as real and nominal price basis could not be identified in data.")
  }

  tdl$data <- dat

  if(is.null(dat_defl_calc)){
    list(main = tdl)
  }

  list(main = tdl, deflator = dat_defl_calc)
}
