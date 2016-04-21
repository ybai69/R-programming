#' Print coefficents in lad
#'
#' (Description paragraph)
#' @param x (description of x; one line per parameter: Arguments section)
#' @param ... further arguments passed to or from other methods
#' @return the named coeffecients vector
#' @details (Details section)
#' @export
#' @examples
#' # (Examples section)
#' 
#'

print.lad<-function(x,...){
  print(x$coefficients)
}
