# simulate nutrient input
nutr_input <- meta.arrR::sim_nutr_input(n = n, max_i = max_i, input_mn = 1, freq_mn = 3,
                                     variability = 0.5, verbose = FALSE)

# extract values only
values <- vapply(nutr_input$values, FUN = length, FUN.VALUE = numeric(1))

test_that("simulate_nutr_input returns meta_rn", {

  expect_is(object = nutr_input, class = "nutr_input")

})

test_that("simulate_nutr_input returns input for each n", {

  expect_length(object = nutr_input$values, n = n)

})

test_that("simulate_nutr_input returns value for timestep", {

  expect_equal(object = unname(values), expected = rep(x = max_i, times = n))

})
