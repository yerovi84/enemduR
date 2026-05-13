test_that("hsize is built from household identifier", {
  data <- tibble::tibble(
    id_hogar = c("a", "a", "b", "b", "b"),
    value = 1:5
  )

  out <- enemdu_build_hsize(data)

  expect_true("hsize" %in% names(out))
  expect_equal(out$hsize, c(2L, 2L, 3L, 3L, 3L))
})

test_that("household scale adjustment creates adjusted variables", {
  data <- tibble::tibble(
    id_hogar = c("a", "a", "b", "b", "b"),
    hsize = c(2L, 2L, 3L, 3L, 3L),
    pobre_hogar = c(1, 1, 0, 0, 0)
  )

  out <- enemdu_apply_household_scale_adjustment(
    data,
    vars = "pobre_hogar",
    hsize = "hsize"
  )

  expect_true("pobre_hogar_hscale" %in% names(out))
  expect_equal(out$pobre_hogar_hscale, c(0.5, 0.5, 0, 0, 0))
})

test_that("household scale adjustment fails for non numeric variables", {
  data <- tibble::tibble(
    id_hogar = c("a", "a"),
    hsize = c(2L, 2L),
    flag = c("yes", "yes")
  )

  expect_error(
    enemdu_apply_household_scale_adjustment(
      data,
      vars = "flag",
      hsize = "hsize"
    ),
    class = "enemdu_error_invalid_numeric_var"
  )
})
