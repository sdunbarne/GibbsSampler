gibbs<-function (n, rho) 
{
        mat <- matrix(ncol = 2, nrow = n)
        x <- 0
        y <- 0
        mat[1, ] <- c(x, y)
        for (i in 2:n) {
                x <- rnorm(1, rho * y, sqrt(1 - rho^2))
                y <- rnorm(1, rho * x, sqrt(1 - rho^2))
                mat[i, ] <- c(x, y)
        }
        mat
}

f  <- function(x) { return( (1/sqrt(2 * pi)) * exp(-x^2/2) )}

N  <-  1000
rho  <-  0.8

bvngs <- gibbs(N, rho)
bvn <- bvngs[(N/2 + 1):N, ]

par(mfrow=c(4,2))
hist(bvn[,1], freq=FALSE, 40)
curve(f, -4, 4, add=TRUE, col="red")
hist(bvn[,2], freq=FALSE, 40)
curve(f, -4, 4, add=TRUE, col="red")
qqnorm(bvn[ , 1], main="Q-Q Plot of bvn[, 1]")
qqnorm(bvn[ , 2], main="Q-Q Plot of bvn[ ,2]")
plot(bvn, col=1:500)
plot(bvn, type="l")
plot(ts(bvn[ , 1]))
plot(ts(bvn[ , 2]))
par(mfrow=c(1,1))


## NAME: bivariatenormalsampler.R
## USAGE: within R, at interactive prompt
##        source("bivariatenormalsampler.R")
## REQUIRED ARGUMENTS: none
## OPTIONS: none
## DESCRIPTION: Demonstrate Gibbs sampler finding marginals from 
##              bivariate normal distribution/
## DIAGNOSTICS: none
## CONFIGURATION AND  ENVIRONMENT: Base R
## DEPENDENCIES: none
## INCOMPATIBILITIES: none known
## PROVENANCE: 
## BUGS AND LIMITATIONS: none known
## FEATURES AND POTENTIAL IMPROVEMENTS: Better graphics arrangements

## AUTHOR:  Steve Dunbar
## VERSION: Version 1.0 Fri Feb  5 08:12:51 AM CST 2021
## KEYWORDS: Gibbs sampler, bivariate normal, marginal distribution

