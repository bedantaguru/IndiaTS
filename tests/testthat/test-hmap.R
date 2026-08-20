
test_that("hmap works", {
  g <- readRDS(test_path("testdata", "gva_tdl_main_real.rds"))

  hm <- hmap(g)

  expect_equal(
    sort(colnames(hm)),
    c("agri_flag_type", "early_industry", "economic_sector", "indicator",
      "industry", "macro_activity_group", "sub_industry", "sub_industry_legacy"
    ))

  expect_gt(NROW(hm),20)
  expect_gt(NCOL(hm),5)

  expect_equal(hm,attr(g, "hmap"))

  gcomp <- hmap(g, compact = TRUE)

  expect_equal(hmap(gcomp) |> NROW(), 8)
  expect_equal(hmap(gcomp) |> NCOL(), 2)

  g <- test_path("testdata", "gva_annual_tdl_main_real.rds") |> readRDS()

  expect_equal(hmap(g) |> unlist() |> unname() |> unique() |> sort(),
               hmap(g, compact = TRUE) |> hmap() |> unlist() |> unname() |> unique() |> sort())

  new_hmap <- tibble::tibble(
    sub_industry = c("Crops", "Livestock", "Forestry and Logging",
                     "Fishing and Aquaculture"),
    agri_regroup_1 = c("Crops", "Allied Activities",
                       "Allied Activities", "Allied Activities")
  )

  g2 <- hmap(g, add = new_hmap)

  # retain most granular data
  g2 <- g2 |> dplyr::filter(meta.disaggregation_group == "sub_industry")

  g2 <- aggregate(g2, type = "component")

  g2 <- attach_parent(g2, "agri_flag_type")

  g2 <- g2 |> compute_metric(contribution_growth_rate = TRUE)

  g2.1 <- g2 |> dplyr::filter(
    meta.disaggregation_group %in% c("agri_regroup_1","macro_activity_group"),
    meta.name!="#NR", meta.price_basis == "real"
  )

  g2.1 <- g2.1 |> as_tdf(value_name = "value.contribution_growth_rate")

  g2.1 <- g2.1 |> dplyr::select(time, `Allied Activities`, Crops, `Agriculture and Allied`)

  chk <- (g2.1$Crops+g2.1$`Allied Activities`)/g2.1$`Agriculture and Allied`
  chk <- chk[!is.na(chk)]

  expect_equal(c(mean(chk), min(chk), max(chk)), c(1,1,1))

  # formula spec
  g2 <- hmap(g, add = `Allied Activities` ~ Livestock + `Forestry and Logging` + `Fishing and Aquaculture`)

  h2 <- hmap(g2)

  expect_true(NCOL(h2) == NCOL(hm)+1 )

  g3 <- hmap(
    g,
    add = list(agri_regroup_2 = `Allied Activities This` ~ Livestock + `Forestry and Logging` + `Fishing and Aquaculture`))

  h3 <- hmap(g3)
  expect_true(NCOL(h2) == NCOL(h3))

  expect_equal(
    h2$user_defined_disaggregation_group |> sort() |> setdiff("Allied Activities"),
    h3$agri_regroup_2 |> sort() |> setdiff("Allied Activities This")
  )

  # retain most granular data
  g3 <- g3 |> dplyr::filter(meta.disaggregation_group == "sub_industry")
  g3 <- aggregate(g3, type = "component")
  g3 <- attach_parent(g3, "agri_flag_type")
  g3 <- g3 |> compute_metric(contribution_growth_rate = TRUE)

  g2.2 <- g3 |> dplyr::filter(
    meta.disaggregation_group %in% c("agri_regroup_2","macro_activity_group"),
    meta.price_basis == "real"
  )

  g2.2 <- g2.2 |> as_tdf(value_name = "value.contribution_growth_rate")

  g2.2 <- g2.2 |> dplyr::select(time, `Allied Activities This`, Crops, `Agriculture and Allied`)

  chk <- (g2.2$Crops+g2.2$`Allied Activities This`)/g2.2$`Agriculture and Allied`
  chk <- chk[!is.na(chk)]

  expect_equal(c(mean(chk), min(chk), max(chk)), c(1,1,1))

  g2.1 <- sort(g2.1)
  g2.2 <- sort(g2.2)


  expect_equal(g2.1$`Allied Activities`, g2.2$`Allied Activities This`)



  # Internal function testing

  expect_error(
    hmap_create_hmap_from_formula(
      c(`Allied Activities` ~ Livestock + `Forestry and Logging` + `Fishing and Aquaculture`,
        `Trade and Hotels` ~ `Hotels & Restaurants` + `Trade & Repair Services`,
        `Agri and Service` ~ `Agriculture and Allied` + `Services`),
      hm
    ),
    "combines multiple disaggregation layers: sub_industry_legacy, macro_activity_group"
  )

  x <- hmap_create_hmap_from_formula(
    list(
      Agri_revisited = c(`Allied Activities` ~ Livestock + `Forestry and Logging` + `Fishing and Aquaculture`,
                         `Trade and Hotels` ~ `Hotels & Restaurants` + `Trade & Repair Services`),
      ASI = `Agri and Service` ~ `Agriculture and Allied` + `Services`),
    hm
  )

  expect_equal(
    x[[1]] |> dplyr::filter(Agri_revisited %in% c("Allied Activities","Trade and Hotels")) |> pull(sub_industry_legacy) |> sort(),
    c("Fishing and Aquaculture", "Forestry and Logging", "Hotels & Restaurants",
      "Livestock", "Trade & Repair Services")
  )

  expect_equal(
    sort(paste(x[[2]]$ASI, x[[2]]$macro_activity_group)),
    c("Agri and Service Agriculture and Allied", "Agri and Service Services",
      "Industry Industry")
  )


  g_int <- hmap_add_on_tdf_long_from_formula(
    g,
    list(
      Agri_revisited = c(`Allied Activities` ~ Livestock + `Forestry and Logging` + `Fishing and Aquaculture`,
                         `Trade and Hotels` ~ `Hotels & Restaurants` + `Trade & Repair Services`),
      ASI = `Agri and Service` ~ `Agriculture and Allied` + `Services`)
  )


  expect_true(
    all(c("ASI","Agri_revisited") %in% colnames(hmap(g_int)))
  )

})

test_that("hmap args works", {

  expect_all_equal(hmap_normalize_form_in(A~B), hmap_normalize_form_in(c(A~B)))

  expect_all_equal(hmap_normalize_form_in(list(n1 = A~B)), hmap_normalize_form_in(list(n1 = c(A~B))))

  expect_equal(
    hmap_normalize_form_in(list(nt = c(A~B+C, D~E+F))),
    list(nt = c(A~B+C, D~E+F))
  )

  expect_error(hmap_normalize_form_in(list(nt = c(A~B+C, D~E+F), A~F)))
  expect_error(hmap_normalize_form_in(list(nt = c(A~B+C, D~E+F), c(A~F, E~R))))

  expect_equal(
    hmap_normalize_form_in(A~B),
    list(user_defined_disaggregation_group = list(A ~ B))
  )

})


test_that("hmap prune resync and sync works", {

  g <- test_path("testdata", "gva_annual_tdl_main_real.rds") |> readRDS()

  g2 <- g |> aggregate(type = "component")

  # Normally aggregate component does not affect values
  expect_equal(
    mean(g$value.level)/sd(g$value.level),
    mean(g2$value.level)/sd(g2$value.level)
  )

  g3 <- g |> hmap(prune_data = TRUE)

  expect_equal(
    unique(g3$meta.disaggregation_group) |> sort(),
    c("early_industry", "sub_industry", "sub_industry_legacy")
  )

  g4 <- hmap(g3, resync_data = TRUE)

  expect_equal(NROW(g4), NROW(g2))

  expect_failure(
    expect_equal(
      mean(g$value.level)/sd(g$value.level),
      mean(g4$value.level)/sd(g4$value.level)
    )
  )

  chk_val <- (mean(g$value.level)/sd(g$value.level))/(mean(g4$value.level)/sd(g4$value.level))

  # Not Too much variation
  expect_true(
    chk_val< 1.1 & chk_val > 0.9
  )

  chk_val <- (mean(g$value.level))/(mean(g4$value.level))

  # Not Too much variation
  expect_true(
    chk_val< 1.1 & chk_val > 0.9
  )


})
