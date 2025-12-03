#' @title Bootstrap function
#'
#' @param iter Number of iterations
#' @param x Sample data
#' @param fun function to use
#' @param alpha confidence level
#' @param ... other arguments
#'
#' @returns A fancy bootstrap histogram
#' @importFrom graphics abline segments text
#' @importFrom stats qt quantile sd
#' @export
#'
#' @examples
#' \dontrun{
#' sam=rnorm(20,mean=10,sd=2)
#' myboot(10000,x=sam,fun=function(x) mean(x)/median(x),alpha=0.05,xlab="mean(x)/median(x)",col="Blue")
#' }
myboot<-function(iter=10000,x,fun="mean",alpha=0.05,...){  #Notice where the ... is repeated in the code
  n=length(x)   #sample size

  y=sample(x,n*iter,replace=TRUE)
  rs.mat=matrix(y,nr=n,nc=iter,byrow=TRUE)
  xstat=apply(rs.mat,2,fun) # xstat is a vector and will have iter values in it

  # bootstrap
  ci=quantile(xstat,c(alpha/2,1-alpha/2))# Nice way to form a confidence interval

  # A histogram follows
  # The object para will contain the parameters used to make the histogram
  para=hist(xstat,freq=FALSE,las=1, breaks = 8, col = rainbow(9, alpha = 0.7),
            main=paste("Histogram of Bootstrap sample statistics","\n","alpha=",alpha," iter=",iter,sep=""),
            ...)

  #mat will be a matrix that contains the data, this is done so that I can use apply()
  mat=matrix(x,nr=length(x),nc=1,byrow=TRUE)

  #pte is the point estimate
  #This uses whatever fun is
  pte=apply(mat,2,fun)
  abline(v=pte,lwd=3,col="Black")# Vertical line

  segments(ci[1],0,ci[2],0,lwd=4, col="black")      #Make the segment for the ci
  text(ci[1],0.1,paste("(",round(ci[1],2),sep=""),col="Red",cex=3)
  text(ci[2],0.1,paste(round(ci[2],2),")",sep=""),col="Red",cex=3)

  # theoretical
  s = sd(x)
  se = s / sqrt(n)
  t = qt(1-alpha/2, df = n-1)
  cit = pte + c(-1,1) * t * se

  # segments(cit[1],0.15,cit[2],0.15,lwd=4, col="blue")      #Make the segment for the ci
  text(cit[1],0.2,paste("(",round(cit[1],2),sep=""),col="blue",cex=3)
  text(cit[2],0.2,paste(round(cit[2],2),")",sep=""),col="blue",cex=3)


  # plot the point estimate 1/2 way up the density
  text(pte,max(para$density)/2,round(pte,2),cex=3)

  return(list(fun=fun,x=x,t=t,ci=ci, cit=cit))# Some output to use if necessary
}
