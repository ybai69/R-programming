#' Predict use object in lad
#'
#' (Description paragraph)
#' @param object a regression
#' @param new.x a vector
#' @param ... further arguments passed to or from other methods
#' @return fitted value
#' @details (Details section)
#' @export
#' @examples
#' # (Examples section)
#' 
#'

predict.lad<-function(object, new.x, ...){
  coef<-object$coefficients
  return(coef[1]+coef[2]*new.x) 
}