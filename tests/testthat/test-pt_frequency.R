test_that("function works properly", {
  expect_equal(pt_frequency(count = c(1, 2, 1, 2, 5),
                            time  = c(1, 1, 0.5, 0.5, 1)),
               expected = c(1, 2, 2, 4, 5)
               )
})

test_that("zero count", {
  expect_equal(pt_frequency(count = c(0, 0, 0, 0, 0),
                            time  = c(1, 2, 3, 4, 5)),
               expected = c(antilog(log10(1/1) - log10(2)),
                            antilog(log10(1/2) - log10(2)),
                            antilog(log10(1/3) - log10(2)),
                            antilog(log10(1/4) - log10(2)),
                            antilog(log10(1/5) - log10(2)))
               )
})
