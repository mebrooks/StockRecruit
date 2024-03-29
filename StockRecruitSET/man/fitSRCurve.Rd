% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/estimationMethods.R
\name{fitSRCurve}
\alias{fitSRCurve}
\title{Fit a stock recruitment curve}
\usage{
fitSRCurve(S, R, shape = "contHockey", g = 0.1, start = NULL, weights = NULL)
}
\arguments{
\item{S}{vector of spawning stock biomasses}

\item{R}{vector of recruitment values}

\item{shape}{can be "contHockey", "Ricker", or "BevertonHolt"}

\item{g}{is the smoothing parameter gamma in the bent hyperbola hockey-stick stock-recruitment model}

\item{start}{(optional) list of parameters}

\item{weights}{(optional) weights of each data point to reflect uncertainty}
}
\description{
Fit a stock recruitment curve
}
\details{
\itemize{
\item "contHockey" shape has pars=c(beta, delta, log_sd) following the bent hyperbola hockey-stick stock-recruitment model (Mesnil & Rochet 2010)
\item "Ricker" shape has pars=c(log_a, log_b, log_sd) where expected recruitment is a*S*exp(-b*S)
\item "BevertonHolt" shape has pars=c(log_a, log_b, log_sd) where expected recruitment is a*S/(1+b*S)
}
}
