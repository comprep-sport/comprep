#' Compare a recalculated to a reported value
#'
#' Assesses the numerical correctness of a reproduced result by calculating
#' whether it falls into a defined margin of error around the reported number
#'
#' The procedure for calculating the margin of error is the following: (1) Add
#' the the maximum possible margin of rounding, given the number of decimals,
#' to the value; (2) then add an additional error margin (e.g., 1% around) this
#' bounds.
#'
#' @param recalculated A numeric value with the recalculated result.
#' @param reported A character with the reported result. (Note that this is a
#'   character and not a number. This is important to define the error margin
#'   created by rounding. E.g., a reported value of 2.70 has a different error
#'   margin than a reported value of 2.7)
#' @param margin Additional margin of error added to the margin bounds. Defaults
#'   to 1%.
#'
#' @returns A list with different elements: `margin` return the error margin as
#'   a vector. `recalculated` repeats the recalculated value. `correct` is a
#'   logical, whether the recalculated is in the margin.
#' @examples
#' comp(0.51, "0.5")
#' @export

comp <- function(recalculated, reported, margin = 0.01) {
  # input validation
  if (!is.numeric(recalculated)) stop("'recalculated' needs to be numeric.")
  if (!is.character(reported)) stop("'reported' needs to be a character.")
  if (!is.numeric(margin)) stop("'margin' need to be numeric.")

  # convert to number
  num <- as.numeric(reported)

  # Find number of decimal places (including trailing zeros)
  split_num <- strsplit(reported, ".", fixed = TRUE)[[1]]
  decimal_places <- ifelse(length(split_num) == 2, nchar(split_num[2]), 0)

  # Calculate the rounding unit
  unit <- 10^(-decimal_places)

  # Compute the margin (half of the unit)
  half_unit <- unit / 2

  # Return the lower and upper bound
  bounds <- c(lower = num - half_unit, upper = num + half_unit)

  # add additional error margin
  bounds[[1]] <- bounds[[1]] * (1 - margin)
  bounds[[2]] <- bounds[[2]] * (1 + margin)

  list(
    recalculated = recalculated,
    margin = bounds,
    correct = (recalculated >= bounds[["lower"]] && recalculated <= bounds[["upper"]])
  )
}
