testthat::test_that("get_features maps each bit to its own feature", {
  testthat::expect_null(get_features(NULL))
  testthat::expect_equal(get_features(0L), "none")
  testthat::expect_equal(get_features(1L), "heart rate monitor")
  testthat::expect_equal(get_features(32L), "no raw data")
  testthat::expect_equal(get_features(17L), "heart rate monitor,epoch data")
  testthat::expect_no_warning(get_features(63L))
})
