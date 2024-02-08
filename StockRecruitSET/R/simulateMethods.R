##' Simulate recruitment
##' @param shape can be "hockey", "Ricker", or "Beverton-Holt"
##' @param pars vector of parameters for the specified shape (see details)
##' @param g is only used for the hyperbolic continuous hockey stick model (see details)
##' @param var used to specify variance on the natural scale
##' @param cor temporal correlation in residuals
##' @param varlog used to specify variance on the log scale
##' @param tail controls bonanzas (i.e. extra good recruitment events) (see details)
##' @details
##' \itemize{
##' \item "hockey" shape must have pars=c(a,b) where (a,b) is the inflection point
##' \item "contHockey" shape must have pars=c(beta, S*) following the notation of Mesnil & Rochet 2010 (eqn 4)
##' \item "Ricker shape" must have pars=c(a,b) where expected recruitment is a*S*exp(-b*S)
##' \item "BevertonHolt" shape must have pars=c(a,b) where expected recruitment is a*S/(1+b*S)
##' \item tail is a named list containing a probability ("prob") of observing a bonanza year and a multiplier ("mult") to indicate how a bonanza compares to the expected mean. e.g. tail=list(prob=0.2, mult=1.5)
##' }
##' @export
##' @importFrom MASS mvrnorm
simR = function(S, shape="contHockey", pars=c(10, 100), g=1, var=0, cor=0, varlog=NULL, tail=NULL)
{
	a = pars[1]
	b = pars[2]
	n = length(S)

	#Calculate expected recruitment without temporal autocorrelation
	if(shape=="hockey") {
		m = ifelse(S>a, b, S*b/a)
	} else {
			if(shape=="contHockey") {
				m=a*(S + sqrt(b*b + (g^2)/4) - sqrt((S-b)^2 + (g^2)/4))
			} else {
					if(shape=="Ricker") {
						m = a*S*exp(-b*S)
					} else {
							if(shape=="BevertonHolt") {
								m = a*S/(1+b*S)
	}}}}
	if(!is.null(tail)) { #possibility of bonanzas
		prob=tail$prob
		mult=tail$mult
		b=rbinom(n, size=1, prob=prob)
		m=ifelse(b==0, m, m*mult) #if b==1, bonanza and  m=m*mult
	}
	if(is.null(varlog)) { #variance is specified on the natural scale
	  mu = log(m/sqrt(1+var/(m^2)))
	  sig = sqrt(log(1+var/(m^2)))
	  #Sigma=matrix(sig, n, n, byrow=TRUE)*sig*cor^abs(row(Sigma)-col(Sigma))
	  #rec=exp(mvrnorm(n, mu, Sigma))
	  cormat = cor^abs(matrix(1: n, n, n, byrow=TRUE)-matrix(1: n, n, n))
	  z = mvrnorm(1, rep(0, n), cormat)
	} else {
	 sig=varlog
	 mu=log(m)-sig*sig/2
	 z=rnorm(n=n, mean=rep(0, n), sd=1)
	}
	x = mu + z * sig
	R = exp(x)

	return(R)
}

##' Simulate SSB
##' @param Sdist the distribution of SSB, can be "unif" or "lognormal"
##' @details
##' \itemize{
##' \item unif uses pars=(minS, maxS)
##' \item lognormal uses pars=(mean, sd)
##' }
##' @export
simS =  function(n=20, Sdist="unif", pars=c(10, 400))
{
  if(Sdist=="unif") S = runif(n, pars[1], pars[2])
  if(Sdist=="lognormal") S= exp(rnorm(n, mean=pars[1], sd=pars[2]))

  return(S)
}
