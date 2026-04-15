

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

