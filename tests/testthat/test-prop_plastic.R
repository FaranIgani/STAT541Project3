test_that("plastic type proportions are correct", {

  dat <- load_data()

  result <- calc_plastic_type_props(dat)

  expect_s3_class(result, "data.frame")

  expect_equal(result$Other[result$Year == 2019],
               0.604,
               tolerance = 0.001)

  expect_equal(result$PET[result$Year == 2019],
               0.2257,
               tolerance = 0.001)

  expect_equal(result$PP[result$Year == 2020],
               0.178,
               tolerance = 0.001)
})
