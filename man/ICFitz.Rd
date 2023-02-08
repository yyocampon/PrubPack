\name{IC.Fitz}
\alias{IC.Fitz}
\title{Multinomial Confidence Interval Using the Fitzpatrick and Scott's Method.}
\usage{
IC.Fitz()
}
\description{
Computes a confidence interval for multinomial parameters using the F distribution. Also called the Fitzpatrick and Scott's Method.
}
\arguments{
  \item{datos}{a vector of the counts.}
  \item{level}{the confidence level of the interval.}
}
\references{Correa-Morales, J. C. (2022) Analysis of CATegoriacal Data Via GSK and Simulation Using R: Lecture Notes. Escuela de Estadistica. Facultad de Ciencias. Universidad Nacional de Colombia. Sede Medellin.}
\examples{
IC.Fitz(c(474,246,11,59))

$LI
[1] 0.56513381 0.27652621 0.00000000 0.03981735

$LS
[1] 0.63486619 0.34625860 0.04879025 0.10954974

$Volumen
[1] 1.654385e-05
}
