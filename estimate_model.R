extimate <- function(matdata)
{ library(MASS)
  z_mat <- matdata[,c(1,3)]## column of ones and z column to z_mat
  const_vec <- ginv(t(z_mat) %*% z_mat) %*% t(z_mat) %*% matdata[,2] # inverse of term in first bracket
  intercept <- const_vec[1] # the intercept
  Z <- cov(matdata[,2],matdata[,4])/cov(matdata[,3],matdata[,4])
  data.frame(intercept,Z)
  
} 

data_gen <- function()
{
  n <- 100 # l e n g t h o f the s e r i e s
  v <- rnorm ( n , mean=0, sd =0.27)
  eps <-  rnorm ( n , mean=0, sd =0.05)
  u <- -0.5 * v + eps
  w <- rnorm ( n , mean=0, sd=1)
  z <- 0.2 * w + v
  y <- 1 + 0.5 * z + eps 
  data_to_1 <- matrix(c(rep(1,n),y,z,w), nrow = 100, ncol = 4)## returns a matrix
  data_to_2 <- data.frame(y,z,w) 
  list(data_to_1,data_to_2)
}

## source script and assign data_gen to any variable e-g vals
## run extimate <- extimate(may[[1]])
##  extimate ## this allows viewing of contents of extimate
## run ivreg(formula = y ~ z | w, data = may[[2]]) to view 
## ivreg parameters and compare

