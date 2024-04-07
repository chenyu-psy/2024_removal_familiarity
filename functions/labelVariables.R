

labelCondition <- function(.data) {
  
  results <- .data %>% 
    mutate(
      condition = factor(
        condition,
        levels = c("cn","kr"),
        labels = c("Familiar (Chinese)", "Unfamiliar (Korean)")
      ))
  
  
  return (results)
} 
