test_that("`path_clean()` works", {
  expect_equal(
    path_clean("../docs"),
    path.expand("../docs"),
    ignore_attr = TRUE
  )
})
