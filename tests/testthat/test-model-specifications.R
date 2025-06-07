test_that("found model specification for hmm_rp", {
  model_specs <- get_all_model_specs()
  hmm_rp_spec <- model_specs$hmm_rp$class
  expect_equal(hmm_rp_spec, 'hmm_rp')
})

test_that("found model specification for hmm_rp", {
  model_specs <- get_all_model_specs()
  hmm_rp_spec <- model_specs$hmm_rp$class
  expect_equal(hmm_rp_spec, 'hmm_rp')
})
