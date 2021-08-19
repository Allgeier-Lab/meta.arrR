# create plot
result_gg <- plot(result_rand)

test_that("plot.meta_rn returns ggplot", {

  expect_s3_class(object = result_gg, class = "ggplot")

})
