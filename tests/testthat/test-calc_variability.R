var_input <- calc_variability(nutrients_input)

var_output <- calc_variability(result_behav)

test_that("calc_variability returns correct objects", {

  expect_s3_class(object = var_input, class = "data.frame")

  expect_true(object = inherits(x = var_output, what = "list"))

})

test_that("calc_variability contains all part and mesuares", {

  expect_equal(object = var_input$measure, expected = c("alpha", "beta",
                                                        "gamma", "synchrony"))

  expect_true(object = all(var_output$biomass$part %in%
                             c("ag_biomass", "bg_biomass", "ttl_biomass")))

  expect_equal(object = var_output$biomass$measure,
               expected = rep(x = c("alpha", "beta", "gamma", "synchrony"), times = 3))

  expect_true(object = all(var_output$production$part %in%
                             c("ag_production", "bg_production", "ttl_production")))

  expect_equal(object = var_output$production$measure,
               expected = rep(x = c("alpha", "beta", "gamma", "synchrony"), times = 3))

})
