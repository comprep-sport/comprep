test_that("input validation works", {
  expect_error(pcomp("0.5", "0.5"), regexp = "'recalculated'")
  expect_error(pcomp(0.5, 0.5), regexp = "'reported'")
  expect_error(pcomp(0.5, "0.5", "0.1"), regexp = "'margin'")
  expect_error(pcomp(0.5, "0.5", 0.01, "0.1"), regexp = "'siglevel'")
})

test_that("exact reproduction is detected", {
  expect_identical(pcomp(0.9, "0.9")$presult, "exact")
  expect_identical(pcomp(0.021, "0.02")$presult, "exact")
})

test_that("compatible reproduction is detected", {
  expect_identical(pcomp(0.9, ">0.5")$presult, "compatible")
  expect_identical(pcomp(0.003, "<0.01")$presult, "compatible")
  # special case when inequality matches level of significance
  expect_identical(pcomp(0.9, ">0.05")$presult, "compatible")
  expect_identical(pcomp(0.003, "<0.05")$presult, "compatible")
})

test_that("incorrect reproduction is detected", {
  # out of margin (without decision error)
  expect_identical(pcomp(0.04, "0.02")$presult, "incorrect")
  expect_false(pcomp(0.04, "0.02")$dec_error)
  # out of margin (with decision error)
  expect_identical(pcomp(0.12, "0.02")$presult, "incorrect")
  expect_true(pcomp(0.12, "0.02")$dec_error)
  # decision error (but in margin)
  expect_identical(pcomp(0.052, "0.048")$presult, "incorrect")
  expect_true(pcomp(0.052, "0.048")$dec_error)
})
