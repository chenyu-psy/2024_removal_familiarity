

labelCondition <- function(.data) {
  
  results <- .data %>% 
    mutate(
      condition = factor(
        condition,
        levels = c("keeping","encoding","removal","updating"),
        labels = c("Keeping", "Encoding", "Removal", "Updating")
      ))
  
  
  return (results)
} 
