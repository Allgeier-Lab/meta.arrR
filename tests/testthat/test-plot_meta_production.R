# create plot
prod_gg <- plot_meta_production(result = result_behav)

test_that("plot_meta_production returns ggplot", {

  expect_s3_class(object = prod_gg, class = "ggplot")

})
