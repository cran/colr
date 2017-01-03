#' a function to change column names
#'
#' @param x A dataframe, list or matrix with column names
#' @param pattern A string
#' @param replacement A string
#' @param dim A character either "c" for selection of columns or "r" for selection of rows, if x is a list this
#' parameter has no meaning
#' @param gl A boolean, if TRUE (the default) all occurences in the input strings (row or column names) are replaced,
#' if FALSE only the first occurence in every string is replaced.
#'
#' @return A dataframe, list or matrix where the columns or rows are renamed based on the 'Regex' substitution
#' defined in the pattern and the replacement. By default column are renamed, unless the dim parameter specifies
#' "r" for rows.
#' The default is to use gsub (replacing all occurences of pattern in each column name), to replace only the first
#' occurence set gl to FALSE.
#'
#' For lists the function acts on the highest level of the list and this is irrespective of the gl parameter.
#'
#' @seealso \link[base]{regex}\{base\}
#'
#' @examples
#' head(csub(iris, "\\.", "-")) # will change all dots in column names in "-"
#' head(csub(iris, "[pP]etal", "Beetle"))
#'
#' # a more complex example showing the power of 'regex'
#'
#' head(csub(data.frame(WorldPhones),"^([NM](?:\\w{2})?\\.)Amer", "\\1America"))
#'
#'
#' # this example will also work on other types (the call data.frame is not necessary)
#'
#' @export
csub <- function(x,pattern,replacement,dim = c("c", "r"),gl=TRUE){
  # gl <- match.arg(gl)
  if (!(is.logical(gl))) {stop("gl must be one off TRUE or FALSE", call. = TRUE)}
  dim <- match.arg(dim)
  if (gl == T) { myf <- gsub } else {myf <- sub}
  if (is.list(x)){
    if (is.data.frame(x) && dim == "r") {
      row.names(x) <- myf(pattern, replacement, row.names(x), perl = TRUE)
    }
    else
    {
      names(x) <- myf(pattern, replacement, names(x), perl = TRUE)
    }
  } else {
    if (is.null(dim(x))) {
      names(x) <- myf(pattern, replacement, names(x), perl = TRUE)
    }
    else {
      if (dim == "c")
      {
        colnames(x) <- myf(pattern, replacement, colnames(x), perl = TRUE)
      } else {
        row.names(x) <- myf(pattern, replacement, row.names(x), perl = TRUE)
      }
    }
  }
  x
}
