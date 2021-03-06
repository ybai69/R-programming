% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lad.R
\name{lad}
\alias{lad}
\title{Least absolute deviations regression}
\usage{
lad(x, y)
}
\arguments{
\item{x}{a vector}

\item{y}{a vector}
}
\value{
a list of coefficients,fitted.values and residuals
}
\description{
a function of Least absolute deviations regression
}
\details{
(Details section)
}
\examples{
lad1<-lad(area$land,area$farm)
print(lad1)
plot(area$land,area$farm)
abline(lm(area$farm~area$land))
abline(lad1,col="red")
legend("topleft",legend=c("lm","lad"),col=c("black","red"),lty=c(1,1))
predict(lad1,quantile(area$land,c(0,1/4,1/2,3/4,1)))
x<-quantile(area$land,c(0,1/4,1/2,3/4,1))
y<-predict(lad1,quantile(area$land,c(0,1/4,1/2,3/4,1)))
points(x,y,col="green",pch=19)

}

