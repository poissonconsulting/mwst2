check_rows <- function (x) {
  if(nrow(x) == 0) stop("x must contain at least one row of data")
  TRUE
}

check_columns <- function (x, colnames) {
  stopifnot(is.data.frame(x))
  stopifnot(is.character(colnames))

  colnames <- unique(colnames)

  bol <- colnames %in% colnames(x)
  if(!all(bol))
    stop("x must contain ", plural("column", sum(!bol) > 1, " "), punctuate_strings(colnames[!bol], "and"))
  TRUE
}

check_class_columns <- function (x, columns) {
  check_columns(x, names(columns))

  for(colname in names(columns)) {
    if(!class(x[[colname]]) %in% columns[[colname]])
      stop("column ", colname, " must be class ", punctuate_strings(columns[[colname]], "or"))
  }
  TRUE
}

plural <- function (x, s = FALSE, end = "") {
  paste0(x, ifelse(s, "s", ""), end)
}

punctuate_strings <- function (x, qualifier = "or") {
  if(length(x) == 1)
    return (x)
  n <- length(x)
  paste(paste(x[-n], collapse = ", "), qualifier, x[n])
}
