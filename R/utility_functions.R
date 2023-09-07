#https://jsta.rbind.io/blog/automated-roxygen-documentation-of-r-package-data/
tabular <- function(df, select = names(df), ...) {
  stopifnot(is.data.frame(x = df))
  
  df <- subset(x = df, select = select)
  
  align <- function(x) if (is.numeric(x = x)) "r" else "l"
  col_align <- vapply(X = df, FUN = align, character(1))
  
  cols <- lapply(X = df, FUN = format, ...)
  contents <- do.call("paste",
                      c(cols, list(sep = " \\tab ", 
                                   collapse = "\\cr\n  ")))
  col_names <- paste0("\\bold{",
                      do.call("paste",
                              c(names(df), list(sep = "} \\tab \\bold{", 
                                                collapse = "\\cr\n  "))),
                      "} \\cr")
  
  paste("\\tabular{", paste(col_align, collapse = ""), "}{\n",
        col_names,
        "\n",
        contents, "\n}\n", sep = "")
}

#' @importFrom utils read.csv
get_table_metadata <- function(path, ...) {
  dt <- read.csv(file = path, stringsAsFactors = FALSE)
  paste0(readLines(con = textConnection(tabular(df = dt, ...))))
}

