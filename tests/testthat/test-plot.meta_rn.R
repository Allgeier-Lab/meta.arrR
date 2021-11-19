# create plot
result_gg <- plot(result_rand)
result_gg_sum <- plot(result_rand, summarize = TRUE)

# create plot
result_gg_fish <- plot(result_rand, what = "fishpop")
result_gg_sum_fish <- plot(result_rand, summarize = TRUE, what = "fishpop")

test_that("plot.meta_rn returns ggplot for seafloor", {

  expect_s3_class(object = result_gg, class = "ggplot")
  expect_s3_class(object = result_gg_sum, class = "ggplot")

})

test_that("plot.meta_rn returns ggplot for fishpop", {

  expect_s3_class(object = result_gg_fish, class = "ggplot")
  expect_s3_class(object = result_gg_sum_fish, class = "ggplot")

})
