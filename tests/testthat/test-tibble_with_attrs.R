

test_that("attributes survive every verb", {
  d   <- tibble_with_attrs(iris, hmap = mtcars)
  lkp <- tibble::tibble(Species = unique(d$Species), extra = 1:3)

  checks <- list(
    # row ops
    filter        = dplyr::filter(d, Sepal.Length > 5),
    arrange       = dplyr::arrange(d, Sepal.Width),
    slice         = dplyr::slice(d, 1:5),
    slice_head    = dplyr::slice_head(d, n = 5),
    slice_tail    = dplyr::slice_tail(d, n = 5),
    slice_sample  = dplyr::slice_sample(d, n = 5),
    slice_min     = dplyr::slice_min(d, Sepal.Length, n = 5),
    slice_max     = dplyr::slice_max(d, Sepal.Length, n = 5),
    distinct      = dplyr::distinct(d, Species),

    # column ops
    select        = dplyr::select(d, Species),
    rename        = dplyr::rename(d, SL = Sepal.Length),
    rename_with   = dplyr::rename_with(d, toupper),
    relocate      = dplyr::relocate(d, Petal.Length),
    mutate        = dplyr::mutate(d, z = Sepal.Length * 2),
    transmute     = dplyr::transmute(d, z = Sepal.Length),

    # summary
    summarise     = dplyr::summarise(dplyr::group_by(d, Species), n = dplyr::n()),
    reframe       = dplyr::reframe(d, n = dplyr::n()),
    count         = dplyr::count(d, Species),
    tally         = dplyr::tally(dplyr::group_by(d, Species)),
    add_count     = dplyr::add_count(d, Species),

    # grouping
    group_by      = dplyr::group_by(d, Species),
    ungroup       = dplyr::ungroup(dplyr::group_by(d, Species)),
    rowwise       = dplyr::rowwise(d),
    nest_by       = dplyr::nest_by(d, Species),

    # binds
    bind_rows     = dplyr::bind_rows(d, d),
    bind_cols     = dplyr::bind_cols(d, tibble::tibble(z = seq_len(nrow(d)))),

    # joins
    left_join     = dplyr::left_join(d,  lkp, by = "Species"),
    right_join    = dplyr::right_join(d, lkp, by = "Species"),
    inner_join    = dplyr::inner_join(d, lkp, by = "Species"),
    full_join     = dplyr::full_join(d,  lkp, by = "Species"),
    semi_join     = dplyr::semi_join(d,  lkp, by = "Species"),
    anti_join     = dplyr::anti_join(d,  lkp, by = "Species"),

    # subsetting
    bracket_rows  = d[1:5, ],
    bracket_cols  = d[, 1:2],
    bracket_both  = d[1:5, 1:2]
  )

  for (nm in names(checks)) {
    expect_identical(attr(checks[[nm]], "hmap"), mtcars, info = nm)
    expect_s3_class(checks[[nm]], "tibble_with_attrs")
  }
})


test_that("grouped verbs preserve attrs AND produce correct results", {
  d  <- tibble_with_attrs(iris, hmap = mtcars)
  gd <- dplyr::group_by(d, Species)

  # plain tibble baseline — same ops without tibble_with_attrs
  ref <- dplyr::group_by(tibble::as_tibble(iris), Species)

  grouped_checks <- list(
    grp_mutate     = dplyr::mutate(gd, z = mean(Petal.Length)),
    grp_filter     = dplyr::filter(gd, Sepal.Length > mean(Sepal.Length)),
    grp_arrange    = dplyr::arrange(gd, Sepal.Width),
    grp_select     = dplyr::select(gd, Species, Sepal.Length),
    grp_summarise  = dplyr::summarise(gd, n = dplyr::n()),
    grp_slice      = dplyr::slice(gd, 1:2),
    grp_distinct   = dplyr::distinct(gd, Sepal.Length, .keep_all = TRUE),
    grp_rename     = dplyr::rename(gd, SL = Sepal.Length),
    grp_relocate   = dplyr::relocate(gd, Petal.Length),
    grp_count      = dplyr::count(gd, Petal.Width > 1),
    grp_tally      = dplyr::tally(gd),
    grp_reframe    = dplyr::reframe(gd, n = dplyr::n()),
    grp_ungroup    = dplyr::ungroup(gd)
  )

  baselines <- list(
    grp_mutate     = dplyr::mutate(ref, z = mean(Petal.Length)),
    grp_filter     = dplyr::filter(ref, Sepal.Length > mean(Sepal.Length)),
    grp_arrange    = dplyr::arrange(ref, Sepal.Width),
    grp_select     = dplyr::select(ref, Species, Sepal.Length),
    grp_summarise  = dplyr::summarise(ref, n = dplyr::n()),
    grp_slice      = dplyr::slice(ref, 1:2),
    grp_distinct   = dplyr::distinct(ref, Sepal.Length, .keep_all = TRUE),
    grp_rename     = dplyr::rename(ref, SL = Sepal.Length),
    grp_relocate   = dplyr::relocate(ref, Petal.Length),
    grp_count      = dplyr::count(ref, Petal.Width > 1),
    grp_tally      = dplyr::tally(ref),
    grp_reframe    = dplyr::reframe(ref, n = dplyr::n()),
    grp_ungroup    = dplyr::ungroup(ref)
  )

  for (nm in names(grouped_checks)) {
    res <- grouped_checks[[nm]]

    # 1. sticky attrs preserved
    expect_identical(attr(res, "hmap"), mtcars, info = nm)
    expect_s3_class(res, "tibble_with_attrs")

    # 2. data content matches plain-tibble baseline
    res_plain <- res
    class(res_plain) <- setdiff(class(res_plain), "tibble_with_attrs")
    for (a in names(sticky_attrs(res))) attr(res_plain, a) <- NULL
    expect_equal(res_plain, baselines[[nm]], info = paste0(nm, " (data)"))
  }
})




test_that("custom superclass on top of tibble_with_attrs survives all ops", {
  d <- tibble_with_attrs(iris, hmap = mtcars)
  class(d) <- c("top_test_twa", class(d))
  # class should be: top_test_twa, tibble_with_attrs, tbl_df, tbl, data.frame

  lkp <- tibble::tibble(Species = unique(d$Species), extra = 1:3)
  gd  <- dplyr::group_by(d, Species)
  ref_plain <- tibble::as_tibble(iris)
  ref_grp   <- dplyr::group_by(ref_plain, Species)

  checks <- list(
    # ungrouped verbs
    filter        = dplyr::filter(d, Sepal.Length > 5),
    arrange       = dplyr::arrange(d, Sepal.Width),
    slice         = dplyr::slice(d, 1:5),
    distinct      = dplyr::distinct(d, Species),
    select        = dplyr::select(d, Species),
    rename        = dplyr::rename(d, SL = Sepal.Length),
    rename_with   = dplyr::rename_with(d, toupper),
    relocate      = dplyr::relocate(d, Petal.Length),
    mutate        = dplyr::mutate(d, z = Sepal.Length * 2),
    transmute     = dplyr::transmute(d, z = Sepal.Length),
    summarise     = dplyr::summarise(gd, n = dplyr::n()),
    reframe       = dplyr::reframe(d, n = dplyr::n()),
    count         = dplyr::count(d, Species),
    tally         = dplyr::tally(gd),
    add_count     = dplyr::add_count(d, Species),
    group_by      = dplyr::group_by(d, Species),
    ungroup       = dplyr::ungroup(gd),
    left_join     = dplyr::left_join(d, lkp, by = "Species"),
    bracket       = d[1:5, ],

    # grouped verbs
    grp_mutate    = dplyr::mutate(gd, z = mean(Petal.Length)),
    grp_filter    = dplyr::filter(gd, Sepal.Length > mean(Sepal.Length)),
    grp_arrange   = dplyr::arrange(gd, Sepal.Width),
    grp_select    = dplyr::select(gd, Species, Sepal.Length),
    grp_summarise = dplyr::summarise(gd, n = dplyr::n()),
    grp_slice     = dplyr::slice(gd, 1:2),
    grp_rename    = dplyr::rename(gd, SL = Sepal.Length),
    grp_count     = dplyr::count(gd, Petal.Width > 1),
    grp_tally     = dplyr::tally(gd),
    grp_reframe   = dplyr::reframe(gd, n = dplyr::n()),
    grp_ungroup   = dplyr::ungroup(gd)
  )

  baselines <- list(
    filter        = dplyr::filter(ref_plain, Sepal.Length > 5),
    arrange       = dplyr::arrange(ref_plain, Sepal.Width),
    slice         = dplyr::slice(ref_plain, 1:5),
    distinct      = dplyr::distinct(ref_plain, Species),
    select        = dplyr::select(ref_plain, Species),
    rename        = dplyr::rename(ref_plain, SL = Sepal.Length),
    rename_with   = dplyr::rename_with(ref_plain, toupper),
    relocate      = dplyr::relocate(ref_plain, Petal.Length),
    mutate        = dplyr::mutate(ref_plain, z = Sepal.Length * 2),
    transmute     = dplyr::transmute(ref_plain, z = Sepal.Length),
    summarise     = dplyr::summarise(ref_grp, n = dplyr::n()),
    reframe       = dplyr::reframe(ref_plain, n = dplyr::n()),
    count         = dplyr::count(ref_plain, Species),
    tally         = dplyr::tally(ref_grp),
    add_count     = dplyr::add_count(ref_plain, Species),
    group_by      = dplyr::group_by(ref_plain, Species),
    ungroup       = dplyr::ungroup(ref_grp),
    left_join     = dplyr::left_join(ref_plain, lkp, by = "Species"),
    bracket       = ref_plain[1:5, ],

    grp_mutate    = dplyr::mutate(ref_grp, z = mean(Petal.Length)),
    grp_filter    = dplyr::filter(ref_grp, Sepal.Length > mean(Sepal.Length)),
    grp_arrange   = dplyr::arrange(ref_grp, Sepal.Width),
    grp_select    = dplyr::select(ref_grp, Species, Sepal.Length),
    grp_summarise = dplyr::summarise(ref_grp, n = dplyr::n()),
    grp_slice     = dplyr::slice(ref_grp, 1:2),
    grp_rename    = dplyr::rename(ref_grp, SL = Sepal.Length),
    grp_count     = dplyr::count(ref_grp, Petal.Width > 1),
    grp_tally     = dplyr::tally(ref_grp),
    grp_reframe   = dplyr::reframe(ref_grp, n = dplyr::n()),
    grp_ungroup   = dplyr::ungroup(ref_grp)
  )

  for (nm in names(checks)) {
    res <- checks[[nm]]

    # 1. superclass preserved
    expect_true("top_test_twa" %in% class(res),
                info = paste0(nm, " (top_test_twa class)"))

    # 2. tibble_with_attrs preserved
    expect_s3_class(res, "tibble_with_attrs")

    # 3. superclass comes before tibble_with_attrs
    cls <- class(res)
    expect_true(
      which(cls == "top_test_twa") < which(cls == "tibble_with_attrs"),
      info = paste0(nm, " (class order)")
    )

    # 4. sticky attr preserved
    expect_identical(attr(res, "hmap"), mtcars,
                     info = paste0(nm, " (hmap attr)"))

    # 5. data content matches plain-tibble baseline
    res_plain <- res
    class(res_plain) <- setdiff(class(res_plain), c("top_test_twa", "tibble_with_attrs"))
    for (a in names(sticky_attrs(res))) attr(res_plain, a) <- NULL
    expect_equal(res_plain, baselines[[nm]],
                 info = paste0(nm, " (data)"))
  }
})
