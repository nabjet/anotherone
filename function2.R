complete_files <- function(directory, id = 1:332)
{ char_vector <- c(1: length(id))
  setwd(directory)
  char_vector_data <- vector('list', length = length(id) ) 
  means_2 <- vector('list', length = length(id))# sulfate values , each i is an id
  means_3 <- vector('list', length = length(id)) #nitrate values , each i is an id
  #t_sulf <- vector('list', length = length(id))# logical telling whether positive for not(NA) sulfate
  t_nitr <- vector('numeric', length = length(id))# logical telling whether positive for not(NA) nitrate
  tot_com <- vector('list', length = length(id))# holds results for sulf && nitr true only if the values are not NA for both cases 
  tot_com_log <- vector('numeric', length = length(id))# should contain the total number of TRUEs for #tot_com
 
  
  ## converting id's to format 'id.csv'
  
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
  
 


 char_vector_data <- lapply(char_vector, read.csv)

for ( i in seq_along(id)) 
{
  means_2[i] <- (char_vector_data[[i]])['sulfate']  ##sulfate values
  means_3[i] <- (char_vector_data[[i]])['sulfate']  ##nitrate values 
  t_sulf <- !(is.na(means_2[[i]])) & !(is.na(means_3[[i]])) #sulfate logical and nitrate logical()
  t_nitr[i] <- length(t_sulf[t_sulf == TRUE]) ## number of complete cases
  
}


  
  setwd('c:/Rstudio')
  y <-  data.frame(id,t_nitr)
  y
  
  

}



