test_that("input validation works", {
  expect_error(comp("0.5", "0.5"), regexp = "'recalculated'")
  expect_error(comp(0.5, 0.5), regexp = "'reported'")
  expect_error(comp(0.5, "0.5", "0.1"), regexp = "'margin'")
})

test_that("correct rounding margins are defined", {
  expect_equal(comp(0.5, "0.5", 0)$margin, c(lower = 0.45, upper = 0.55))
  expect_equal(comp(0.5, "0.50", 0)$margin, c(lower = 0.495, upper = 0.505))
})

test_that("correct full margins are defined", {
  expect_equal(comp(7.4, "7.4", 0.01)$margin, c(lower = 7.2765, upper = 7.5245))
})

test_that("correct results are calculated", {
  expect_true(comp(0.52, "0.5")$correct)
  expect_false(comp(0.7, "0.5")$correct)
})
