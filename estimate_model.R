extimate <- function(y_val,z_mat,w_mat)
{ library(MASS)
  const_vec <- ginv(t(z_mat[,1]) %*% z_mat[,1]) %*% t(z_mat[,1]) %*% y_val # inverse of term in first bracket
  const_vec <- const_vec[1] # the intercept
  B1 <- ginv(t(w_mat) %*% z_mat[,2]) %*% t(w_mat) %*% y_val # b1
  matrix(c(const_vec[1],B1), nrow = 2, ncol = 1)
  } 