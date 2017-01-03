# The 'colr' package does only one thing but it does it very well
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

#' a function to select columns by 'regex'
#'
#' @description Select columns (or rows) by 'perl' regular expression. See \link[base]{regex}\{base\} for
#' 'regex' documenation. 'regex' is a very powerful grammar to match strings.
#'
#' @seealso \link[base]{regex}\{base\}
#'
#' @param x A dataframe, list a matrix with column names or a named numeric.
#' @param pattern A search string
#' @param dim A character either "c" for selection of columns or "r" for selection of rows, if x is a list this
#' parameter has no meaning
#' @return  A Dataframe, list or matrix where the column- or row names match the pattern. By default the
#' selecion is by column, unless the dim parameter specifies "r" for rows. If f x is not a dataframe and if
#'  a single column or row is selected 'R' will flatten the results to a vector.
#'
#' @examples
#'  head(cgrep(iris, "^.+$")) # matches all columns that have non-empty column names and thus
#'  #drops all columns with empty names
#'
#'  head(cgrep(iris, "^Petal\\.")) # matches all columns that have  names starting with the
#'  #string "Petal."
#'  head(cgrep(iris, "\\.")) # columns with names that contain a dot
#'  head(cgrep(iris, "\\.[Ww]idth$")) # columns with names ending in the string ".width" or ".Width".
#'  head(cgrep(iris, "^[SP]e.al")) # columns that have names starting with either capital
#'  #'S' or 'P' then an 'e' followed by any character and then 'al'
#'
#' @export
cgrep <- function(x, pattern, dim = c("c", "r")){
  dim <- match.arg(dim)
  if (is.list(x)) {
    if (is.data.frame(x)) {
      if (dim == "c")
      {
        x[grep(pattern, names(x), perl=TRUE)]
      }
      else
      {
        x[grep(pattern, row.names(x), perl=TRUE),]
      }
    }
    else {
      x[grep(pattern, names(x), perl = TRUE)]
    }
  }
  else {
    if (dim == "c"){
      select <- grep(pattern, colnames(x), perl=TRUE)
      if (length(select) == 0){
        select <- grep(pattern, names(x), perl = TRUE)
        if (length(select) == 0){
          NULL
        }
        else {
          (x)[select]
        }
      }
      else {
        (x)[,select]
      }
    }
    else
      {select <- grep(pattern, rownames(x), perl=TRUE)
      if (length(select) == 0){
        select <- grep(pattern, names(x), perl = TRUE)
        if (length(select) == 0){
          NULL
        }
        else {
          (x)[select]
        }
      }
      else {
        (x)[select,]
      }
    }
  }
}

