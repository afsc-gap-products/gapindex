#' Helper function to format sql queries
#' 
#' @param stitch_what string of values to stitch
#' @return a single string that encapsulates the values of `stitch_what` in 
#'         a parenthesis. 
#' @export
#' 
stitch_entries <- function(stitch_what = "") {
  return( paste0("(", paste0(stitch_what, collapse=", "), ")")  )
}
