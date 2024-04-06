
labelResponse <- function(.data) {
  
  results <- .data %>% 
    mutate(response = factor(
      response,
      levels = c("CorrectOld","CorrectNew","OutSame","NPL","TransOld","TransNew","OutOther","NoRes"),
      labels = c(
        "Original items (Correct)", "New items (Correct)", "Outdated items (Original)",
        "NPL",
        "Original items (Other)", "New items (Other)", "Outdated items (Other)",
        "No response")
    ))
  
  return (results)
}
