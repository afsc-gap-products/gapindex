#' Helper function to format sql queries
#' 
#' @param stitch_what string of values to stitch
#' @return a single string that encapsulates the values of `stitch_what` in 
#'         a parenthesis. 
#' @export
#' 
stitch_entries <- function(stitch_what = "") {
  
  if (class(stitch_what) == "character") {
    return( paste0("(", paste0(sQuote(x = stitch_what, q = F), collapse=", "), ")")  )
  } else
    return( paste0("(", paste0(stitch_what, collapse=", "), ")")  )
}
