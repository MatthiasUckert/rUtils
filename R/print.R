#' Get Size of an R Object
#'
#' @param .obj An R object
#' @return The size of the object (length, nrow, or nlevels)
#' @noRd
get_size <- function(.obj) {
  tryCatch(
    {
      if (is.data.frame(.obj) || is.matrix(.obj)) nrow(.obj)
      else if (is.factor(.obj)) nlevels(.obj)
      else length(.obj)
    },
    error = function(e) {
      warning("Unable to determine size for object of class ", class(.obj))
      return(NA)
    }
  )
}

#' Format Numbers with Commas
#'
#' This function takes an R object and returns a comma-formatted string.
#' For numeric values, it adds commas for thousands. For other objects,
#' it determines the size and formats that with commas.
#'
#' @param .obj An R object to be formatted.
#' @param .prec Integer. The number of decimal places for numeric values. Default is 0.
#'
#' @return A character string with comma-formatted number.
#'
#' @examples
#' pcomma(1234567)  # Returns "1,234,567"
#' pcomma(1234567.89, .prec = 2)  # Returns "1,234,567.89"
#' pcomma(1:1000)  # Returns "1,000"
#' pcomma(data.frame(a = 1:1000, b = 2:1001))  # Returns "1,000"
#' pcomma(list(a = 1, b = 2, c = 3))  # Returns "3"
#'
#' @import scales
#' @export
pcomma <- function(.obj, .prec = 0L) {
  if (is.numeric(.obj) || is.integer(.obj)) {
    if (length(.obj) == 1) {
      return(scales::comma(.obj, accuracy = 10^(-.prec)))
    }
  }

  size <- get_size(.obj)
  scales::comma(size, accuracy = 1)
}

#' Format Percentage
#'
#' This function takes two R objects and returns a percentage string.
#' If both inputs are single numeric values, it calculates the percentage directly.
#' Otherwise, it divides the size of .obj1 by the size of .obj2 and formats the result as a percentage.
#'
#' @param .obj1 An R object (numerator).
#' @param .obj2 An R object (denominator).
#' @param .prec Integer. The number of decimal places for the percentage. Default is 0.
#'
#' @return A character string with percentage.
#'
#' @examples
#' ppercent(50, 100)  # Returns "50%"
#' ppercent(1:75, 1:100)  # Returns "75%"
#' ppercent(data.frame(a = 1:3), data.frame(b = 1:4))  # Returns "75%"
#' ppercent(33.333, 100, .prec = 2)  # Returns "33.33%"
#'
#' @import scales
#' @export
ppercent <- function(.obj1, .obj2, .prec = 0L) {
  if (is.numeric(.obj1) && is.numeric(.obj2) && length(.obj1) == 1 && length(.obj2) == 1) {
    if (.obj2 == 0) {
      warning("Invalid input or division by zero")
      return(NA)
    }
    percentage <- .obj1 / .obj2
  } else {
    size1 <- get_size(.obj1)
    size2 <- get_size(.obj2)

    if (is.na(size1) || is.na(size2) || size2 == 0) {
      warning("Invalid input or division by zero")
      return(NA)
    }

    percentage <- size1 / size2
  }

  scales::percent(percentage, accuracy = 10^(-.prec))
}
