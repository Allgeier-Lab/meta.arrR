# create plot
meta_gg <- plot(metasyst)

test_that("plot.meta_syst returns ggplot", {

  expect_s3_class(object = meta_gg, class = "ggplot")

})
