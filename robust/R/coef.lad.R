#' coefficients of lad()
#'
#' (Description paragraph)
#' @param object (description of x; one line per parameter: Arguments section)
#' @param ... further arguments passed to or from other methods
#' @return coefficient
#' @details (Details section)
#' @export
#' @examples
#' # (Examples section)
#' 
#'

coef.lad<-function(object,...){
  return(object$coefficients)  
}