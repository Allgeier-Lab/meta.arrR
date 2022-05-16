# create vector with names
present_names <- c("n", "seafloor", "fishpop", "seafloor_xy", "fishpop_attr",
                   "starting_values", "parameters", "reef", "dimensions", "extent",
                   "grain")

test_that("setup_meta returns meta_syst", {

  expect_is(object = metasyst, class = "meta_syst")

  expect_is(object = metasyst_nr, class = "meta_syst")

})

test_that("setup_meta contains all information", {

  expect_equal(object = names(metasyst), expected = present_names)

  expect_equal(object = names(metasyst_nr), expected = present_names)

})

test_that("setup_meta returns list with seafloor and fishpop", {

  expect_length(object = metasyst$seafloor, n = n)

  expect_length(object = metasyst$fishpop, n = n)


  expect_length(object = metasyst_nr$seafloor, n = n)

  expect_length(object = metasyst_nr$fishpop, n = n)

})

test_that("setup_meta returns correct information", {

  expect_equal(object = metasyst$n, expected = n)

  expect_equal(object = unname(metasyst$extent),
               expected = metasyst$dimensions / 2 * c(-1, 1, -1, 1))

  expect_equal(object = metasyst$grain, expected = grain)

  expect_equal(object = unname(metasyst$dimensions), expected = dimensions)


  expect_equal(object = metasyst_nr$n, expected = n)

  expect_equal(object = unname(metasyst_nr$extent),
               expected = metasyst_nr$dimensions / 2 * c(-1, 1, -1, 1))

  expect_equal(object = metasyst_nr$grain, expected = grain)

  expect_equal(object = metasyst_nr$dimensions, expected = dimensions)

})
