# create plot
nutr_gg <- plot(nutrients_input)

test_that("plot.nutr_input returns ggplot", {

  expect_s3_class(object = nutr_gg, class = "ggplot")

})
