

## Fix for R CMD check
if (getRversion() >= "2.15.1") {
  utils::globalVariables(
    c(
      ########
      # for dplyr operations : non-standard evaluation (NSE)
      ".data"

    )
  )

}

