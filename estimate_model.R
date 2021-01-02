extimate <- function(y_val,z_mat,w_mat)
{ library(MASS)
  const_vec <- ginv(t(z_mat[,1]) %*% z_mat[,1]) %*% t(z_mat[,1]) %*% y_val # inverse of term in first bracket
  const_vec <- const_vec[1] # the intercept
  B1 <- ginv(t(w_mat) %*% z_mat[,2]) %*% t(w_mat) %*% y_val # b1
  matrix(c(const_vec[1],B1), nrow = 2, ncol = 1)
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
  data_to <- data.frame(y,z,w)
  data_to
}


