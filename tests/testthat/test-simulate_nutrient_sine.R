# extract values only
values <- vapply(nutrients_input$values, FUN = nrow, FUN.VALUE = numeric(1))

test_that("simulate_nutr_input returns meta_rn", {

  expect_is(object = nutrients_input, class = "nutr_input")

})

test_that("simulate_nutr_input returns input for each n", {

  expect_length(object = nutrients_input$values, n = n)

})

test_that("simulate_nutr_input returns value for timestep", {

  expect_equal(object = unname(values), expected = rep(x = max_i, times = n))

})
