

labelCondition <- function(.data) {
  
  results <- .data
  
  if ("condition" %in% names(.data)) {
    results <- results %>% 
      mutate(
        condition = factor(
          condition,
          levels = c(
            "cn","kr",
            "Familiar (Chinese)", "Unfamiliar (Korean)"),
          labels = c(
            "Familiar (Chinese)", "Unfamiliar (Korean)",
            "Familiar (Chinese)", "Unfamiliar (Korean)")
        ))
  }
  
  if ("itemType" %in% names(.data)) {
    results <- results %>% 
      mutate(
        itemType = factor(
          itemType,
          levels = c("original","new"),
          labels = c("Original items", "New items")
        )
      )
  }
  
  if ("recallPos" %in% names(.data)) {
    results <- results %>% 
      mutate(
        recallPos = factor(
          recallPos,
          levels = c("original","other"),
          labels = c("Original position", "Other positions")
        )
      )
  }
  
  return (results)
}
