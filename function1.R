pollutantMean <- function(directory, pollutant, id = 1:332)
  { 
  char_vector <- id
  setwd(directory)
  
  char_vector_data <- vector('list', length = length(id) ) 
  means_2 <- vector('list', length = length(id))
  means_3 <- vector('numeric',length = length(id))
  sums <- vector('numeric',length = length(id))
  
  
  
for (i in seq_along(id) ) { 
  
  if ( (id[i] <= 9) && (id[i] > 0) )
  {
    char_vector[i] <- paste('00',id[i],'.csv', sep = '')

  }
  else if (id[i] <= 99 && id[i] >= 10)
  {
    char_vector[i] <- paste('0',id[i],'.csv', sep = '')
  } 
  else if (id[i] >= 100 && id[i] <= 332)
  {
    char_vector[i] <- paste(id[i],'.csv', sep = '')
  }
  else 
  {
    paste('out of range, the file',id[i], 'doesnt exist')
  }
}
  
#####  reading values

 char_vector_data <- lapply(char_vector, read.csv)
 

 
 setwd("C:/Rstudio")
  
  #means_1#$pollutant
  # ( char_vector_data[[1]])[pollutant]
 for ( i in seq_along(id)) 
   {
means_2[i] <- (char_vector_data[[i]])[pollutant]   
  
   
 }
 
 for (i in seq_along(id)) 
   {
      means_3[i] <- sum(means_2[[i]], na.rm = TRUE) # sums of colums
      sums[i] <- length(na.exclude((means_2[[i]])))
      
 }

#mean(means_3) 
 mean_return <- sum(means_3)/sum(sums)

}



