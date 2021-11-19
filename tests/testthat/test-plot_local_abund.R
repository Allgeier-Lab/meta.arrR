# create plot
result_gg <- plot_local_abund(result_rand)

test_that("plot_local_abund returns ggplot", {

  expect_s3_class(object = result_gg, class = "ggplot")

})

