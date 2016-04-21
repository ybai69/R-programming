#' Least absolute deviations regression
#'
#' a function of Least absolute deviations regression
#' @param x a vector
#' @param y a vector
#' @return a list of coefficients,fitted.values and residuals
#' @details (Details section)
#' @export
#' @examples
#' lad1<-lad(area$land,area$farm)
#' print(lad1)
#' plot(area$land,area$farm)
#' abline(lm(area$farm~area$land))
#' abline(lad1,col="red")
#' legend("topleft",legend=c("lm","lad"),col=c("black","red"),lty=c(1,1))
#' predict(lad1,quantile(area$land,c(0,1/4,1/2,3/4,1)))
#' x<-quantile(area$land,c(0,1/4,1/2,3/4,1))
#' y<-predict(lad1,quantile(area$land,c(0,1/4,1/2,3/4,1)))
#' points(x,y,col="green",pch=19)
#'

lad<-function(x,y){
  SAD<-function(beta){
    beta0<-beta[1]
    beta1<-beta[2]
    return(sum(abs(y-beta0-beta1*x)))
  }
  optimization<-optim(par=lm(y~x)$coefficients,fn=SAD,method = "Nelder-Mead")
  coefficients<-optimization$par
  fitted.values<-coefficients[1]+coefficients[2]*x
  residuals<-y-fitted.values
  result<-list(coefficients=coefficients,fitted.values=fitted.values,residuals=residuals)
  class(result)<-"lad"
  return(result)
}


  