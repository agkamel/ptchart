test_that("it works correctly", {
  expect_equal(recode_freq_when_zero(0, 1), 0.5)
  expect_equal(recode_freq_when_zero(0.25, 1), 0.5)
  expect_equal(recode_freq_when_zero(0.25, 0.5), 1)
})



