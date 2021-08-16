# set number of metaecosystems
n <- 3

# set max_i and save_each
max_i <- 10

nutr <- simulate_nutr_input_noise(n = n, max_i = max_i, freq_mn = 3,
                                  input_max = 1, variability = 0.5)

nutr_gg <- plot(nutr)

test_that("plot.nutr_input returns ggplot", {

  expect_is(object = nutr_gg, class = "ggplot")

})
