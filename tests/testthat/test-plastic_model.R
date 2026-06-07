test_that("plastic_model_table works", {

  model <- fit_plastic_lmer(test_data)

  result <- plastic_model_table(model)

  expect_s3_class(result, "gt_tbl")
})


test_that("fit_plastic_lmer returns an lmer model", {

  model <- fit_plastic_lmer(test_data)

  expect_s4_class(model, "lmerMod")

})
