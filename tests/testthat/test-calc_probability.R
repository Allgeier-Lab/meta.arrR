probs <- calc_probability(metasyst = metasyst)

probs_0 <- calc_probability(metasyst = metasyst, diag_value = 0.0)

probs_lower <- calc_probability(metasyst = metasyst, full = FALSE)

test_that("calc_probability returns matrix", {

  expect_true(object = inherits(x = probs, what = "matrix"))

  expect_true(object = nrow(probs) == n)

  expect_true(object = ncol(probs) == n)

  expect_false(object = anyNA(probs_0))

})

test_that("calc_probability uses diagonal values", {

  expect_true(object = all(is.na(diag(probs))))

  expect_true(object = all(diag(probs_0) == 0))

})

test_that("calc_probability returns only lower", {

  expect_true(all(is.na(probs_lower[upper.tri(probs_lower)])))

})

test_that("calc_probability returns errer", {

  expect_error(object = calc_probability(metasyst = matrix(data = 1:4, ncol = 2)),
               regexp = "Please provide 'meta_syst' object.")

})

