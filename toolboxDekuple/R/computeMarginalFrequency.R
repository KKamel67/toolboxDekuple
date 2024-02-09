#' @title Compute marginal frequencies
#'
#' @description Compute the sum by row and column of the numeric columns
#'
#' @param df A data frame
#'
#' @importFrom assertthat assert_that
#' @export
#'
compute_marginal_frequency <- function(df) {

  assert_that(inherits(df, "data.frame"), msg = "Not a data frame")

  numeric_cols <- sapply(df, is.numeric)
  if (length(numeric_cols[numeric_cols]) > 0) {
    df$Total <- rowSums(df[,numeric_cols])
    col_sum <- colSums(df[,numeric_cols])
    col_sum <- c(col_sum, sum(col_sum))
    names(col_sum)[length(col_sum)] <- "Total"
    df <- rbind(df, data.frame(sexe='Total', t(col_sum)))
  }

  return(df)
}
