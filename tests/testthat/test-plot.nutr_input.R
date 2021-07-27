# set number of metaecosystems
n <- 3

# set max_i and save_each
max_i <- 10

save_each <- 2

nutr <- simulate_nutr_input(n = n, max_i = max_i, freq_mn = 3, freq_sd = 0.1,
                            input_max = 0.1, input_sd = 0.5)

nutr_gg <- plot(nutr)

test_that("plot.nutr_input returns ggplot", {

  expect_is(object = nutr_gg, class = "ggplot")

})
