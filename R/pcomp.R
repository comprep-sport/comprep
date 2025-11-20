#' Compare a recalculated p-value to a reported value p-value
#'
#' Assesses the numerical correctness of a reproduced p-value by calculating
#' whether it it is exactly reported, falls within the margin of error of the
#' reported value, and falls on the same side of the level of significance.
#'
#' A p-value reproduction is labeled as 'exact', when the reported p-value was
#' given as an exact value, the recalculated value falls within its margin of
#' error, and is on the same side of the level of significance (i.e. no decision
#' error). A p-value is labeled as 'compatible' if it was reported as an
#' inequality that (considering the error margin for its bound) matches the
#' reproduced value, and both fall on the same side of the level of
#' significance. A p-value reproduction is labeled as 'incorrect' otherwise.
#'
#' For details on the calculation of the error margin, see [comp()].
#'
#' @param reported A character with the reported result. (Note that this is a
#'   character and not a number. This is important to define the error margin
#'   created by rounding. E.g., a reported value of 2.70 has a different error
#'   margin than a reported value of 2.7).
#' @param siglevel Level of significance, to check for decision errors.
#' @inheritParams comp
#'
#' @returns A list with different elements: `margin` return the error margin as
#'   a vector. `recalculated` repeats the recalculated value. `correct` is a
#'   logical, whether the recalculated value is within the margin. `presult` is
#'   a character, either 'exact', 'compatible', or 'incorrect'. `dec_error` is a
#'   logical, whether a decision error is present (reported and recalculated
#'   p-values fall on different sides of the level of significance).
#' @examples
#' pcomp(0.51, "0.5")
#' pcomp(0.02, "<0.05")
#' pcomp(0.055, "<0.05")
#' @export

pcomp <- function(recalculated, reported, margin = 0.01, siglevel = 0.05) {
  # input validation
  if (!is.numeric(recalculated)) stop("'recalculated' needs to be numeric.")
  if (!is.character(reported)) stop("'reported' needs to be a character.")
  if (!is.numeric(margin)) stop("'margin' needs to be numeric.")
  if (!is.numeric(siglevel)) stop("'siglevel' needs to be numeric.")

  # reported number without inequality
  num_string <- gsub("[<>]", "", reported)
  # as number
  num <- as.numeric(num_string)

  # calculate margins using general agreement function
  res <- comp(recalculated, num_string, margin)

  # check if reported p-value features an inequality
  exact <- FALSE
  if (grepl(">", reported)) {
    res$margin[["upper"]] <- 1
  } else if (grepl("<", reported)) {
    res$margin[["lower"]] <- 0
  } else {
    exact <- TRUE
  }

  # recalculate correctness decision
  res$correct = (recalculated >= res$margin[["lower"]] && recalculated <= res$margin[["upper"]])

  # check for agreement of significance
  if ((recalculated > siglevel && num > siglevel) | (recalculated <= siglevel && num <= siglevel)) {
    agree <- TRUE
  } else if (res$margin[["upper"]] == 1 & siglevel == num & recalculated >= siglevel) {
    agree <- TRUE
  } else {
    agree <- FALSE
  }

  # determine final results
  res$dec_error <- FALSE
  if (exact & agree & res$correct) {
    res$presult <- "exact"
  } else if (!exact & agree & res$correct) {
    res$presult <- "compatible"
  } else {
    res$presult <- "incorrect"
    if (!agree) res$dec_error <- TRUE
  }
  res
}
