## ----echo = FALSE, message = FALSE--------------------------------------------
require(jagsUI)
knitr::opts_chunk$set(
  comment = "#",
    error = FALSE,
     tidy = FALSE,
    cache = FALSE,
 collapse = TRUE
)

## -----------------------------------------------------------------------------
library(jagsUI)

## -----------------------------------------------------------------------------
data(longley)
head(longley)

## -----------------------------------------------------------------------------
jags_data <- list(
  gnp = longley$GNP,
  employed = longley$Employed,
  n = nrow(longley)
)

## -----------------------------------------------------------------------------
# Create a temporary file
modfile <- tempfile()

#Write model to file
writeLines("
model{

  # Likelihood
  for (i in 1:n){ 
    # Model data
    employed[i] ~ dnorm(mu[i], tau)
    # Calculate linear predictor
    mu[i] <- alpha + beta*gnp[i]
  }
    
  # Priors
  alpha ~ dnorm(0, 0.00001)
  beta ~ dnorm(0, 0.00001)
  sigma ~ dunif(0,1000)
  tau <- pow(sigma,-2)

}
", con=modfile)

## -----------------------------------------------------------------------------
inits <- function(){  
  list(alpha=rnorm(1,0,1),
       beta=rnorm(1,0,1),
       sigma=runif(1,0,3)
  )  
}

## -----------------------------------------------------------------------------
params <- c('alpha','beta','sigma')

## -----------------------------------------------------------------------------
out <- jags(data = jags_data,
            inits = inits,
            parameters.to.save = params,
            model.file = modfile,
            n.chains = 3,
            n.adapt = 100,
            n.iter = 1000,
            n.burnin = 500,
            n.thin = 2)

## ----eval=FALSE---------------------------------------------------------------
#  out <- jags(data = jags_data,
#              inits = inits,
#              parameters.to.save = params,
#              model.file = modfile,
#              n.chains = 3,
#              n.adapt = 100,
#              n.iter = 1000,
#              n.burnin = 500,
#              n.thin = 2,
#              parallel = TRUE)

## -----------------------------------------------------------------------------
out

## -----------------------------------------------------------------------------
names(out)

## -----------------------------------------------------------------------------
traceplot(out)

## -----------------------------------------------------------------------------
densityplot(out)

## -----------------------------------------------------------------------------
plot(out)

## -----------------------------------------------------------------------------
post_alpha <- out$sims.list$alpha
hist(post_alpha, xlab="Value", main = "alpha posterior")

## -----------------------------------------------------------------------------
# Now save mu also
params <- c(params, "mu")
out2 <- update(out, n.iter=300, parameters.to.save = params)

## -----------------------------------------------------------------------------
out2

## -----------------------------------------------------------------------------
whiskerplot(out2, 'mu')

