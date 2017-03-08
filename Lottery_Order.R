#Code to solve the following problem from MindYourDecisions' puzzle: https://www.youtube.com/watch?v=emzh-2i_Fzk

# k = balls selected
# n = ball count

probability_of_ordered_lottery_numbers <- function (k = 5, n = 60, iterations = 100) {
  
  i = 1
  all_numbers_drawn <- c()
  tix_asc <- c()
  tix_desc <- c()
  tix_unord <- c()
  
  while ( i <= iterations ){
    possible_numbers <- seq(from = 1, to = n, by = 1)
    j = 1
    while ( j <= k ){
    
      rand_ball_seq <- ceiling(runif(1, min = 1, max = length(possible_numbers)))
      all_numbers_drawn[((i*k)-k)+j] <- possible_numbers[rand_ball_seq]
      possible_numbers <- possible_numbers[-rand_ball_seq]
      j = j +1
    }
    
  #decide if the series are in ascending, descending and no order
   row_begin <- ((i*k)-k)+1
   row_end <- i*k
   this_iteration_numbers <- all_numbers_drawn[row_begin:row_end]
   this_iteration_numbers_desc <- this_iteration_numbers[order(this_iteration_numbers, decreasing = TRUE)]
   this_iteration_numbers_asc <- this_iteration_numbers[order(this_iteration_numbers, decreasing = FALSE)]
   
   asc_match <- this_iteration_numbers == this_iteration_numbers_asc
   desc_match <- this_iteration_numbers == this_iteration_numbers_desc
     
   tix_asc[i] <- if(length(asc_match[asc_match==TRUE])==k) {TRUE} else {FALSE} 
   tix_desc[i] <- if(length(desc_match[desc_match==TRUE])==k) {TRUE} else {FALSE} 
   tix_unord[i] <- if(tix_asc[i] == FALSE && tix_desc[i] == FALSE) {TRUE} else {FALSE}
   
   i = i + 1 
  }

  
  #merge matrix of my numbers with vectors of analysis
  my_tix_matrix <- matrix(all_numbers_drawn,iterations,k, byrow = TRUE)
  final_matrix <- cbind(my_tix_matrix,tix_asc,tix_desc,tix_unord)
  ##print(final_matrix)
  pct_asc <- mean(tix_asc)
  pct_desc <- mean(tix_desc)
  pct_unord <- mean(tix_unord)
  
  #display the results
  print(paste("Probability of ascending is: ", pct_asc))
  print(paste("Probability of descending is: ", pct_desc))
  print(paste("Probability of neither is: ", pct_unord))
  print(paste("2/k! is: ", round(2/factorial(k),4)))
  print(paste("Probability of ascending plus descending is: ", pct_asc + pct_desc))
  
}